//! One bounded nonblocking transport step per ready peer. Semantic requests stay
//! serialized in the session owner; a partial receive or reply never owns it.
use super::*;
use rustix::{
    event::PollFlags,
    fs::{OFlags, fcntl_getfl, fcntl_setfl},
};

enum State {
    Marker,
    Body {
        fds: Vec<OwnedFd>,
        bytes: Vec<u8>,
    },
    Reply {
        bytes: Vec<u8>,
        offset: usize,
        fds: Option<Vec<OwnedFd>>,
    },
    Done,
}
pub struct Peer {
    socket: OwnedFd,
    state: State,
}
impl Peer {
    pub fn new(socket: OwnedFd) -> io::Result<Self> {
        fcntl_setfl(&socket, fcntl_getfl(&socket)? | OFlags::NONBLOCK)?;
        Ok(Self {
            socket,
            state: State::Marker,
        })
    }
    pub fn fd(&self) -> BorrowedFd<'_> {
        self.socket.as_fd()
    }
    pub fn events(&self) -> PollFlags {
        match self.state {
            State::Reply { .. } => PollFlags::OUT,
            _ => PollFlags::IN,
        }
    }
    pub fn done(&self) -> bool {
        matches!(self.state, State::Done)
    }
    pub fn reject(&mut self, bytes: Vec<u8>) {
        if matches!(self.state, State::Reply { .. }) {
            self.state = State::Done;
        } else {
            self.reply(bytes);
        }
    }
    pub fn close(&mut self) {
        self.state = State::Done;
    }
    pub fn reply(&mut self, bytes: Vec<u8>) {
        self.state = State::Reply {
            bytes,
            offset: 0,
            fds: None,
        };
    }
    pub fn reply_with_fds(&mut self, bytes: Vec<u8>, fds: Vec<OwnedFd>) {
        self.state = State::Reply {
            bytes,
            offset: 0,
            fds: Some(fds),
        };
    }
    pub fn advance(&mut self) -> io::Result<Option<(Vec<OwnedFd>, Vec<u8>)>> {
        match &mut self.state {
            State::Marker => match receive_fds(self.socket.as_fd()) {
                Ok(fds) => {
                    self.state = State::Body {
                        fds,
                        bytes: Vec::new(),
                    }
                }
                Err(e)
                    if matches!(
                        e.kind(),
                        io::ErrorKind::WouldBlock | io::ErrorKind::Interrupted
                    ) => {}
                Err(e) => return Err(e),
            },
            State::Body { bytes, .. } => {
                let mut chunk = [0; 65536];
                match rustix::io::read(&self.socket, &mut chunk) {
                    Ok(0) => {
                        let State::Body { fds, bytes } =
                            std::mem::replace(&mut self.state, State::Done)
                        else {
                            unreachable!()
                        };
                        return Ok(Some((fds, bytes)));
                    }
                    Ok(n) => bytes.extend_from_slice(&chunk[..n]),
                    Err(rustix::io::Errno::INTR | rustix::io::Errno::AGAIN) => {}
                    Err(e) => return Err(e.into()),
                }
            }
            State::Reply { bytes, offset, fds } => {
                if let Some(descriptors) = fds {
                    match send_fds(
                        self.socket.as_fd(),
                        &descriptors.iter().map(AsFd::as_fd).collect::<Vec<_>>(),
                    ) {
                        Ok(()) => *fds = None,
                        Err(error)
                            if matches!(
                                error.kind(),
                                io::ErrorKind::WouldBlock | io::ErrorKind::Interrupted
                            ) => {}
                        Err(error) => return Err(error),
                    }
                    return Ok(None);
                }
                let end = (*offset + 65536).min(bytes.len());
                match net::send(&self.socket, &bytes[*offset..end], send_flags()) {
                    Ok(0) if *offset < bytes.len() => return Err(io::ErrorKind::WriteZero.into()),
                    Ok(n) => {
                        *offset += n;
                        if *offset == bytes.len() {
                            self.state = State::Done;
                        }
                    }
                    Err(rustix::io::Errno::INTR | rustix::io::Errno::AGAIN) => {}
                    Err(e) => return Err(e.into()),
                }
            }
            State::Done => {}
        }
        Ok(None)
    }
}
