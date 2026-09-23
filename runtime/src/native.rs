//! The only native process boundary. All allocations precede fork; child execution
//! is a leaf of direct async-signal-safe libc calls ending in execve or _exit.
use crate::types::SourceFd;
use std::{
    collections::BTreeMap,
    ffi::CString,
    io,
    mem::MaybeUninit,
    os::fd::{AsFd, AsRawFd, BorrowedFd, FromRawFd, OwnedFd},
    ptr,
    sync::{
        OnceLock,
        atomic::{AtomicI32, Ordering},
    },
};
pub type Streams = BTreeMap<SourceFd, OwnedFd>;
pub type Environment = Vec<(Vec<u8>, Vec<u8>)>;
const SIGNALS: [i32; 9] = [
    libc::SIGPIPE,
    libc::SIGINT,
    libc::SIGQUIT,
    libc::SIGTERM,
    libc::SIGHUP,
    libc::SIGCHLD,
    libc::SIGTSTP,
    libc::SIGTTIN,
    libc::SIGTTOU,
];
fn cvt(result: i32) -> io::Result<i32> {
    if result < 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(result)
    }
}
fn positive(result: i32) -> io::Result<()> {
    if result == 0 {
        Ok(())
    } else {
        Err(io::Error::from_raw_os_error(result))
    }
}
pub fn native_error_message(error: &io::Error) -> Vec<u8> {
    let code = error.raw_os_error().unwrap_or(libc::EBADF);
    // POSIX strerror strings are copied before any subsequent libc call.
    unsafe { std::ffi::CStr::from_ptr(libc::strerror(code)) }
        .to_bytes()
        .to_vec()
}
mod descriptors;
mod lifecycle;
mod process;
mod resources;
mod signals;
pub use descriptors::*;
pub use lifecycle::*;
pub use process::*;
pub(crate) use resources::Workspace;
pub use signals::*;
#[cfg(test)]
mod tests;
