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
        fd::{AsFd, AsRawFd, OwnedFd},
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
pub fn guardian(args: &[Vec<u8>]) -> io::Result<i32> {
    let [directory, script, token, number] = args else {
        return Err(invalid("invalid capsule guardian arguments"));
    };
    let number = std::str::from_utf8(number)
        .ok()
        .and_then(|v| v.parse::<i32>().ok())
        .filter(|n| *n >= 3)
        .ok_or_else(|| invalid("invalid capsule guardian descriptor"))?;
    let _cleanup = Cleanup(path(directory).to_owned());
    // This guardian CLI invocation inherits the listener directly from its
    // launcher; no Rust owner or previous adoption exists in this process.
    let listener = native::adopt_inherited(number)?;
    nix::unistd::setsid()?;
    let (reader, writer) = native::private_pipe()?;
    let deadline = Instant::now() + Duration::from_secs(60);
    loop {
        let Some(left) = deadline.checked_duration_since(Instant::now()) else {
            return Ok(0);
        };
        let timeout = Timespec {
            tv_sec: left.as_secs() as _,
            tv_nsec: left.subsec_nanos() as _,
        };
        if poll(&mut [PollFd::new(&listener, PollFlags::IN)], Some(&timeout))? == 0 {
            return Ok(0);
        }
        let connection = transport::accept(listener.as_fd())?;
        // A stalled unauthenticated peer must not defeat the takeover timeout.
        let Ok((fds, request)) = transport::receive_until(connection.as_fd(), deadline) else {
            continue;
        };
        if !fds.is_empty() || protocol::decode(&request) != Ok(vec![token.clone()]) {
            continue;
        }
        transport::send_fds(connection.as_fd(), &[writer.as_fd()])?;
        transport::write(
            connection.as_fd(),
            &protocol::encode(&[b"ok".to_vec(), script.clone()]),
        )?;
        drop(connection);
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
}
