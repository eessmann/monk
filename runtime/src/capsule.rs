//! Transferable bootstrap workspace, owned by a detached guardian after prepare.
use crate::{
    native::{self, Streams},
    protocol, transport,
    types::SourceFd,
};
use rustix::event::{PollFd, PollFlags, Timespec, poll};
use std::{
    io::{self, Read, Write},
    os::{
        fd::{AsFd, AsRawFd, BorrowedFd, OwnedFd},
        unix::{ffi::OsStrExt, fs::PermissionsExt},
    },
    path::{Path, PathBuf},
    time::{Duration, Instant},
};
pub fn invalid(message: &str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, message)
}
pub fn path(bytes: &[u8]) -> &Path {
    Path::new(std::ffi::OsStr::from_bytes(bytes))
}
pub fn bytes(path: &Path) -> Vec<u8> {
    path.as_os_str().as_bytes().to_vec()
}
pub fn runtime() -> io::Result<Vec<u8>> {
    Ok(bytes(&std::env::current_exe()?))
}
pub fn workspace(prefix: &str) -> io::Result<tempfile::TempDir> {
    tempfile::Builder::new()
        .prefix(prefix)
        .permissions(std::fs::Permissions::from_mode(0o700))
        .tempdir_in("/tmp")
}
pub fn file(directory: &Path, value: &[u8]) -> io::Result<Vec<u8>> {
    let mut file = tempfile::NamedTempFile::new_in(directory)?;
    file.write_all(value)?;
    file.flush()?;
    let (_, path) = file.keep().map_err(|e| e.error)?;
    Ok(bytes(&path))
}
pub fn token() -> io::Result<Vec<u8>> {
    let mut source = std::fs::File::open("/dev/urandom")?;
    let mut value = [0; 32];
    source.read_exact(&mut value)?;
    Ok(value
        .into_iter()
        .flat_map(|b| {
            [
                b"0123456789abcdef"[(b >> 4) as usize],
                b"0123456789abcdef"[(b & 15) as usize],
            ]
        })
        .collect())
}
pub fn prepare(script: &[u8]) -> io::Result<Vec<u8>> {
    let directory = workspace("monk-capsule-")?;
    let script_path = file(directory.path(), script)?;
    let token = token()?;
    let listener = transport::listen(&bytes(&directory.path().join("lease")))?;
    let mut streams = Streams::new();
    let number = listener.as_raw_fd();
    streams.insert(SourceFd::new(number)?, listener);
    native::spawn(
        &streams,
        None,
        &native::environment(),
        &runtime()?,
        &[
            b"--abi".to_vec(),
            b"2".to_vec(),
            crate::abi2::opcode::cli::SESSION_GUARDIAN.to_vec(),
            bytes(directory.path()),
            script_path,
            token.clone(),
            number.to_string().into_bytes(),
        ],
        false,
    )?;
    let directory = directory.keep();
    Ok(protocol::encode(&[
        b"ok".to_vec(),
        b"0".to_vec(),
        bytes(&directory),
        token,
    ]))
}
/// A lease cannot outlive its descriptor. The guardian observes EOF even on SIGKILL.
pub struct CapsuleLease {
    pub script: Vec<u8>,
    _lease: OwnedFd,
}
pub fn acquire(directory: &[u8], token: &[u8]) -> io::Result<CapsuleLease> {
    let endpoint = path(directory).join("lease");
    let (lease, response) =
        transport::request_lease(&bytes(&endpoint), &protocol::encode(&[token.to_vec()]))?;
    match protocol::decode(&response)
        .map_err(|_| invalid("invalid capsule reply"))?
        .as_slice()
    {
        [ok, script] if ok == b"ok" => Ok(CapsuleLease {
            script: script.clone(),
            _lease: lease,
        }),
        _ => Err(invalid("capsule takeover rejected")),
    }
}
struct Cleanup(PathBuf);
impl Drop for Cleanup {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}
pub(crate) fn guardian_descriptor(args: &[Vec<u8>]) -> io::Result<i32> {
    let number = match args {
        [flag, _, number] if flag == b"--workspace" => number,
        [_, _, _, number] => number,
        _ => return Err(invalid("invalid capsule guardian arguments")),
    };
    std::str::from_utf8(number)
        .ok()
        .and_then(|value| value.parse::<i32>().ok())
        .filter(|number| *number >= 3)
        .ok_or_else(|| invalid("invalid capsule guardian descriptor"))
}

pub fn guardian(args: &[Vec<u8>], descriptor: OwnedFd) -> io::Result<i32> {
    if let [flag, directory, _] = args
        && flag == b"--workspace"
    {
        return workspace_guardian(directory, descriptor);
    }
    let [directory, script, token, _] = args else {
        return Err(invalid("invalid capsule guardian arguments"));
    };
    let _cleanup = Cleanup(path(directory).to_owned());
    let listener = descriptor;
    nix::unistd::setsid()?;
    let (reader, writer) = native::private_pipe()?;
    let deadline = Instant::now() + Duration::from_secs(60);
    let Some(mut peer) = receive_takeover(listener.as_fd(), token, deadline)? else {
        return Ok(0);
    };
    peer.reply_with_fds(
        protocol::encode(&[b"ok".to_vec(), script.clone()]),
        vec![native::duplicate_private(writer.as_fd())?],
    );
    while !peer.done() {
        if !poll_before_deadline(
            &mut [PollFd::from_borrowed_fd(peer.fd(), peer.events())],
            deadline,
        )? {
            return Ok(0);
        }
        peer.advance()?;
    }
    drop(peer);
    drop(writer);
    let mut byte = [0];
    loop {
        match rustix::io::read(&reader, &mut byte) {
            Ok(_) => return Ok(0),
            Err(rustix::io::Errno::INTR) => {}
            Err(e) => return Err(e.into()),
        }
    }
}

fn poll_before_deadline(watched: &mut [PollFd<'_>], deadline: Instant) -> io::Result<bool> {
    loop {
        let Some(left) = deadline.checked_duration_since(Instant::now()) else {
            return Ok(false);
        };
        let timeout = Timespec {
            tv_sec: left.as_secs() as _,
            tv_nsec: left.subsec_nanos() as _,
        };
        match poll(watched, Some(&timeout)) {
            Ok(ready) => return Ok(ready != 0),
            Err(rustix::io::Errno::INTR) => {}
            Err(error) => return Err(error.into()),
        }
    }
}

fn receive_takeover(
    listener: BorrowedFd<'_>,
    token: &[u8],
    deadline: Instant,
) -> io::Result<Option<transport::Peer>> {
    use rustix::fs::{OFlags, fcntl_getfl, fcntl_setfl};
    fcntl_setfl(listener, fcntl_getfl(listener)? | OFlags::NONBLOCK)?;
    let mut peers: Vec<transport::Peer> = Vec::new();
    loop {
        peers.retain(|peer| !peer.done());
        let mut watched = vec![PollFd::from_borrowed_fd(listener, PollFlags::IN)];
        watched.extend(
            peers
                .iter()
                .map(|peer| PollFd::from_borrowed_fd(peer.fd(), peer.events())),
        );
        if !poll_before_deadline(&mut watched, deadline)? {
            return Ok(None);
        }
        let ready = watched
            .iter()
            .map(|fd| !fd.revents().is_empty())
            .collect::<Vec<_>>();
        drop(watched);
        for (index, peer) in peers.iter_mut().enumerate() {
            if !ready[index + 1] {
                continue;
            }
            match peer.advance() {
                Ok(Some((fds, request))) => {
                    if fds.is_empty()
                        && protocol::borrowed(&request).is_ok_and(|frames| frames == [token])
                    {
                        // Dropping all other peers releases incomplete buffers/fds;
                        // only this authenticated connection receives the lease.
                        return Ok(Some(peers.swap_remove(index)));
                    }
                    peer.close();
                }
                Ok(None) => {}
                Err(_) => peer.close(),
            }
        }
        if ready[0] {
            match transport::accept(listener).and_then(transport::Peer::new) {
                Ok(peer) => peers.push(peer),
                Err(error)
                    if matches!(
                        error.kind(),
                        io::ErrorKind::WouldBlock | io::ErrorKind::Interrupted
                    ) => {}
                Err(error) => return Err(error),
            }
        }
    }
}

/// One detached guardian owns the session root. Each generated job receives a
/// private alias of its liveness pipe. The root survives owner exit until the
/// final background runtime releases its lease, including release by SIGKILL.
pub struct WorkspaceOwner {
    directory: PathBuf,
    lease: Option<OwnedFd>,
    guardian: Option<native::RunningChild>,
}
impl WorkspaceOwner {
    pub fn new(prefix: &str) -> io::Result<Self> {
        let directory = workspace(prefix)?;
        let (reader, writer) = native::private_pipe()?;
        let number = reader.as_raw_fd();
        let mut streams = Streams::new();
        streams.insert(SourceFd::new(number)?, reader);
        let guardian = native::spawn(
            &streams,
            None,
            &native::environment(),
            &runtime()?,
            &[
                b"--abi".to_vec(),
                b"2".to_vec(),
                crate::abi2::opcode::cli::SESSION_GUARDIAN.to_vec(),
                b"--workspace".to_vec(),
                bytes(directory.path()),
                number.to_string().into_bytes(),
            ],
            false,
        )?;
        Ok(Self {
            directory: directory.keep(),
            lease: Some(writer),
            guardian: Some(guardian),
        })
    }
    pub fn path(&self) -> &Path {
        &self.directory
    }
    pub fn job(&self, prefix: &str) -> io::Result<JobWorkspace> {
        let directory = tempfile::Builder::new()
            .prefix(prefix)
            .permissions(std::fs::Permissions::from_mode(0o700))
            .tempdir_in(self.path())?;
        let lease = native::duplicate_private(self.lease.as_ref().unwrap().as_fd())?;
        Ok(JobWorkspace { directory, lease })
    }
    /// Call only when every owned job has completed. Live background jobs keep
    /// the guardian alive through their inherited leases after this owner drops.
    pub fn finish(mut self) -> io::Result<()> {
        self.lease.take();
        if let Some(mut guardian) = self.guardian.take() {
            loop {
                match guardian.wait() {
                    Err(error) if error.kind() == io::ErrorKind::Interrupted => continue,
                    result => {
                        result?;
                        break;
                    }
                }
            }
        }
        Ok(())
    }
}

/// Prepared job workspace. Any failure before launch rolls its files back.
/// Successful launch consumes it and transfers cleanup to a workspace lease.
pub struct JobWorkspace {
    directory: tempfile::TempDir,
    lease: OwnedFd,
}
impl JobWorkspace {
    pub fn path(&self) -> &Path {
        self.directory.path()
    }
    pub fn spawn(
        self,
        streams: &Streams,
        cwd: native::BorrowedDirectory<'_>,
        env: &native::Environment,
        args: &[Vec<u8>],
        asynchronous: bool,
    ) -> io::Result<(native::RunningChild, WorkspaceLease)> {
        let mut streams = streams
            .iter()
            .map(|(n, fd)| Ok((*n, native::duplicate_private(fd.as_fd())?)))
            .collect::<io::Result<Streams>>()?;
        let number = streams.keys().map(|n| n.get()).max().unwrap_or(9).max(9) + 1;
        streams.insert(
            SourceFd::new(number)?,
            native::duplicate_private(self.lease.as_fd())?,
        );
        let mut env = env.clone();
        env.retain(|(name, _)| name != b"MONK_WORKSPACE_LEASE");
        env.push((
            b"MONK_WORKSPACE_LEASE".to_vec(),
            number.to_string().into_bytes(),
        ));
        let child = native::spawn(&streams, Some(cwd), &env, &runtime()?, args, asynchronous)?;
        Ok((
            child,
            WorkspaceLease {
                directory: self.directory.keep(),
                _lease: self.lease,
            },
        ))
    }
}
/// A launched child's payload remains owned until completion; dropping this
/// lease leaves its files to the root guardian if the session exits first.
pub struct WorkspaceLease {
    directory: PathBuf,
    _lease: OwnedFd,
}
impl WorkspaceLease {
    pub fn finish(self) -> io::Result<()> {
        match std::fs::remove_dir_all(&self.directory) {
            Err(error) if error.kind() != io::ErrorKind::NotFound => Err(error),
            _ => Ok(()),
        }
    }
}
pub(crate) fn inherited_workspace_number() -> io::Result<Option<i32>> {
    let Some(number) = std::env::var_os("MONK_WORKSPACE_LEASE") else {
        return Ok(None);
    };
    let number = number
        .to_str()
        .and_then(|n| n.parse::<i32>().ok())
        .filter(|n| *n >= 3)
        .ok_or_else(|| invalid("invalid workspace lease"))?;
    Ok(Some(number))
}
fn workspace_guardian(directory: &[u8], reader: OwnedFd) -> io::Result<i32> {
    let _cleanup = Cleanup(path(directory).to_owned());
    nix::unistd::setsid()?;
    let mut chunk = [0; 64];
    loop {
        match rustix::io::read(&reader, &mut chunk) {
            Ok(0) => return Ok(0),
            Ok(_) | Err(rustix::io::Errno::INTR) => {}
            Err(e) => return Err(e.into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn trickled_takeover_does_not_extend_the_global_startup_deadline() {
        let directory = workspace("monk-takeover-deadline-").unwrap();
        let endpoint = bytes(&directory.path().join("lease"));
        let listener = transport::listen(&endpoint).unwrap();
        let client = transport::connect(&endpoint).unwrap();
        transport::send_fds(client.as_fd(), &[]).unwrap();
        let start = Instant::now();
        let sender = std::thread::spawn(move || {
            for _ in 0..40 {
                if transport::write(client.as_fd(), b"x").is_err() {
                    break;
                }
                std::thread::sleep(Duration::from_millis(5));
            }
        });
        let selected = receive_takeover(
            listener.as_fd(),
            b"token",
            start + Duration::from_millis(60),
        )
        .unwrap();
        assert!(selected.is_none());
        assert!(start.elapsed() < Duration::from_millis(180));
        sender.join().unwrap();
    }
}
