//! Private byte protocol. Every received SCM_RIGHTS descriptor is owned immediately;
//! malformed messages and partial copies roll back by dropping those owners.
use crate::native;
use rustix::net::{
    self, AddressFamily, RecvAncillaryBuffer, RecvAncillaryMessage, RecvFlags, ReturnFlags,
    SendAncillaryBuffer, SendAncillaryMessage, SendFlags, Shutdown, SocketAddrUnix, SocketType,
};
use std::{
    io::{self, IoSlice, IoSliceMut},
    mem::MaybeUninit,
    os::fd::{AsFd, BorrowedFd, OwnedFd},
};
fn invalid() -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        "invalid session descriptor message",
    )
}
fn socket() -> io::Result<OwnedFd> {
    let fd = net::socket(AddressFamily::UNIX, SocketType::STREAM, None)?;
    #[cfg(target_os = "macos")]
    net::sockopt::set_socket_nosigpipe(&fd, true)?;
    native::duplicate_private(fd.as_fd())
}
pub fn listen(path: &[u8]) -> io::Result<OwnedFd> {
    let fd = socket()?;
    net::bind(&fd, &SocketAddrUnix::new(path)?)?;
    net::listen(&fd, 16)?;
    Ok(fd)
}
pub fn accept(listener: BorrowedFd<'_>) -> io::Result<OwnedFd> {
    let fd = net::accept(listener)?;
    native::duplicate_private(fd.as_fd())
}
pub fn connect(path: &[u8]) -> io::Result<OwnedFd> {
    let fd = socket()?;
    net::connect(&fd, &SocketAddrUnix::new(path)?)?;
    Ok(fd)
}
pub fn send_fds(socket: BorrowedFd<'_>, fds: &[BorrowedFd<'_>]) -> io::Result<()> {
    if fds.len() > 4 {
        return Err(invalid());
    }
    let mut space = [MaybeUninit::uninit(); rustix::cmsg_space!(ScmRights(4))];
    let mut ancillary = SendAncillaryBuffer::new(&mut space);
    if !fds.is_empty() && !ancillary.push(SendAncillaryMessage::ScmRights(fds)) {
        return Err(invalid());
    }
    let sent = net::sendmsg(socket, &[IoSlice::new(b"M")], &mut ancillary, send_flags())?;
    if sent != 1 {
        return Err(invalid());
    }
    Ok(())
}
pub fn receive_fds(socket: BorrowedFd<'_>) -> io::Result<Vec<OwnedFd>> {
    receive_fds_interruptible(socket, &mut || Ok(()))
}
fn receive_fds_interruptible(
    socket: BorrowedFd<'_>,
    check: &mut impl FnMut() -> io::Result<()>,
) -> io::Result<Vec<OwnedFd>> {
    // Receive the kernel's entire maximum message, then enforce our four-fd
    // protocol limit. Darwin leaves cmsg_len larger than the returned control
    // buffer on MSG_CTRUNC, which rustix 1.1.5 cannot safely drain. XNU accepts
    // one SCM_RIGHTS message, at most UIPC_MAX_CMSG_FD=512; Linux allows253.
    // https://github.com/apple-oss-distributions/xnu/blob/main/bsd/kern/uipc_usrreq.c
    let mut space = [MaybeUninit::uninit(); rustix::cmsg_space!(ScmRights(512))];
    let mut ancillary = RecvAncillaryBuffer::new(&mut space);
    let mut marker = [0];
    let received = loop {
        match net::recvmsg(
            socket,
            &mut [IoSliceMut::new(&mut marker)],
            &mut ancillary,
            RecvFlags::empty(),
        ) {
            Ok(value) => break value,
            Err(rustix::io::Errno::INTR) => check()?,
            Err(e) => return Err(e.into()),
        }
    };
    let mut raw = Vec::new();
    let mut unexpected = false;
    for message in ancillary.drain() {
        match message {
            RecvAncillaryMessage::ScmRights(fds) => raw.extend(fds),
            _ => unexpected = true,
        }
    }
    if received.bytes != 1
        || marker != *b"M"
        || received
            .flags
            .intersects(ReturnFlags::CTRUNC | ReturnFlags::TRUNC)
        || raw.len() > 4
        || unexpected
    {
        return Err(invalid());
    }
    raw.iter()
        .map(|fd| native::duplicate_private(fd.as_fd()))
        .collect()
}
pub fn read_all(fd: BorrowedFd<'_>) -> io::Result<Vec<u8>> {
    read_all_interruptible(fd, &mut || {
        if native::pending_signal().is_some() {
            Err(io::ErrorKind::Interrupted.into())
        } else {
            Ok(())
        }
    })
}
fn read_all_interruptible(
    fd: BorrowedFd<'_>,
    check: &mut impl FnMut() -> io::Result<()>,
) -> io::Result<Vec<u8>> {
    let mut bytes = Vec::new();
    let mut chunk = [0u8; 65536];
    loop {
        match rustix::io::read(fd, &mut chunk) {
            Ok(0) => return Ok(bytes),
            Ok(n) => bytes.extend_from_slice(&chunk[..n]),
            Err(rustix::io::Errno::INTR) => check()?,
            Err(e) => return Err(e.into()),
        }
    }
}
pub fn request(path: &[u8], fds: &[BorrowedFd<'_>], bytes: &[u8]) -> io::Result<Vec<u8>> {
    let socket = connect(path)?;
    send_fds(socket.as_fd(), fds)?;
    write(socket.as_fd(), bytes)?;
    net::shutdown(&socket, Shutdown::Write)?;
    read_all(socket.as_fd())
}
pub fn request_lease(path: &[u8], bytes: &[u8]) -> io::Result<(OwnedFd, Vec<u8>)> {
    let socket = connect(path)?;
    send_fds(socket.as_fd(), &[])?;
    write(socket.as_fd(), bytes)?;
    net::shutdown(&socket, Shutdown::Write)?;
    let mut fds = receive_fds(socket.as_fd())?;
    let response = read_all(socket.as_fd())?;
    if fds.len() != 1 {
        return Err(invalid());
    }
    Ok((fds.remove(0), response))
}
pub fn receive(socket: BorrowedFd<'_>) -> io::Result<(Vec<OwnedFd>, Vec<u8>)> {
    let fds = receive_fds(socket)?;
    let bytes = read_all(socket)?;
    Ok((fds, bytes))
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn ancillary_transfer_retains_shared_offset() {
        let (left, right) = std::os::unix::net::UnixStream::pair().unwrap();
        let mut file = tempfile::tempfile().unwrap();
        use std::io::{Seek, Write};
        file.write_all(b"abc").unwrap();
        file.rewind().unwrap();
        send_fds(left.as_fd(), &[file.as_fd()]).unwrap();
        let received = receive_fds(right.as_fd()).unwrap();
        let mut byte = [0];
        rustix::io::read(&received[0], &mut byte).unwrap();
        assert_eq!(&byte, b"a");
        rustix::io::read(&file, &mut byte).unwrap();
        assert_eq!(&byte, b"b");
    }
    #[test]
    fn wrong_marker_is_rejected() {
        let (left, right) = std::os::unix::net::UnixStream::pair().unwrap();
        native::write_all(left.as_fd(), b"?").unwrap();
        assert!(receive_fds(right.as_fd()).is_err());
    }
}

fn send_flags() -> SendFlags {
    #[cfg(target_os = "linux")]
    {
        SendFlags::NOSIGNAL
    }
    #[cfg(target_os = "macos")]
    {
        SendFlags::empty()
    }
}
pub fn write(socket: BorrowedFd<'_>, mut bytes: &[u8]) -> io::Result<()> {
    while !bytes.is_empty() {
        let n = match net::send(socket, bytes, send_flags()) {
            Ok(n) => n,
            Err(rustix::io::Errno::INTR) if native::pending_signal().is_none() => continue,
            Err(e) => return Err(e.into()),
        };
        if n == 0 {
            return Err(io::ErrorKind::WriteZero.into());
        }
        bytes = &bytes[n..];
    }
    Ok(())
}

pub fn receive_interruptible(
    socket: BorrowedFd<'_>,
    mut check: impl FnMut() -> io::Result<()>,
) -> io::Result<(Vec<OwnedFd>, Vec<u8>)> {
    let fds = receive_fds_interruptible(socket, &mut check)?;
    let bytes = read_all_interruptible(socket, &mut check)?;
    Ok((fds, bytes))
}

/// Absolute deadline across the complete unauthenticated message, including a
/// peer that sends a slow trickle of bytes. Read timeouts alone reset per call.
pub fn receive_until(
    socket: BorrowedFd<'_>,
    deadline: std::time::Instant,
) -> io::Result<(Vec<OwnedFd>, Vec<u8>)> {
    use rustix::{
        event::{PollFd, PollFlags, Timespec, poll},
        fs::{OFlags, fcntl_getfl, fcntl_setfl},
    };
    let original = fcntl_getfl(socket)?;
    fcntl_setfl(socket, original | OFlags::NONBLOCK)?;
    let remaining = || {
        deadline
            .checked_duration_since(std::time::Instant::now())
            .ok_or_else(|| io::Error::from(io::ErrorKind::TimedOut))
    };
    let wait = || -> io::Result<()> {
        loop {
            let time = remaining()?;
            let time = Timespec {
                tv_sec: time.as_secs() as _,
                tv_nsec: time.subsec_nanos() as _,
            };
            match poll(
                &mut [PollFd::from_borrowed_fd(socket, PollFlags::IN)],
                Some(&time),
            ) {
                Ok(0) => return Err(io::ErrorKind::TimedOut.into()),
                Ok(_) => return Ok(()),
                Err(rustix::io::Errno::INTR) => continue,
                Err(e) => return Err(e.into()),
            }
        }
    };
    let result = (|| {
        let fds = loop {
            wait()?;
            match receive_fds_interruptible(socket, &mut || remaining().map(|_| ())) {
                Err(e) if e.kind() == io::ErrorKind::WouldBlock => continue,
                result => break result?,
            }
        };
        let mut bytes = Vec::new();
        let mut chunk = [0; 65536];
        loop {
            wait()?;
            match rustix::io::read(socket, &mut chunk) {
                Ok(0) => break,
                Ok(n) => bytes.extend_from_slice(&chunk[..n]),
                Err(rustix::io::Errno::INTR | rustix::io::Errno::AGAIN) => continue,
                Err(e) => return Err(e.into()),
            }
        }
        Ok((fds, bytes))
    })();
    fcntl_setfl(socket, original)?;
    result
}

#[cfg(test)]
mod failure_tests {
    use super::*;
    #[test]
    fn excess_rights_close_all_received_aliases() {
        let (left, right) = std::os::unix::net::UnixStream::pair().unwrap();
        let pairs = (0..8)
            .map(|_| {
                let (reader, writer) = native::private_pipe().unwrap();
                rustix::fs::fcntl_setfl(&reader, rustix::fs::OFlags::NONBLOCK).unwrap();
                (reader, writer)
            })
            .collect::<Vec<_>>();
        let mut space = [MaybeUninit::uninit(); rustix::cmsg_space!(ScmRights(8))];
        let mut control = SendAncillaryBuffer::new(&mut space);
        let writers = pairs
            .iter()
            .map(|(_, writer)| writer.as_fd())
            .collect::<Vec<_>>();
        assert!(control.push(SendAncillaryMessage::ScmRights(&writers)));
        net::sendmsg(&left, &[IoSlice::new(b"M")], &mut control, send_flags()).unwrap();
        assert!(receive_fds(right.as_fd()).is_err());
        drop(writers);
        let readers = pairs
            .into_iter()
            .map(|(reader, writer)| {
                drop(writer);
                reader
            })
            .collect::<Vec<_>>();
        for reader in readers {
            assert_eq!(
                rustix::io::read(reader, &mut [0]).unwrap(),
                0,
                "an ancillary descriptor leaked"
            );
        }
    }
    #[test]
    fn trickled_payload_does_not_extend_absolute_deadline() {
        let (left, right) = std::os::unix::net::UnixStream::pair().unwrap();
        send_fds(left.as_fd(), &[]).unwrap();
        let start = std::time::Instant::now();
        let sender = std::thread::spawn(move || {
            for _ in 0..40 {
                if native::write_all(left.as_fd(), b"x").is_err() {
                    break;
                }
                std::thread::sleep(std::time::Duration::from_millis(5));
            }
        });
        assert_eq!(
            receive_until(right.as_fd(), start + std::time::Duration::from_millis(60))
                .unwrap_err()
                .kind(),
            io::ErrorKind::TimedOut
        );
        // Keep the peer alive until sender finishes so default SIGPIPE cannot affect this unit-test process.
        assert!(start.elapsed() < std::time::Duration::from_millis(180));
        sender.join().unwrap();
        drop(right);
    }
}
