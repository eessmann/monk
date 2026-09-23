//! Bounded execution with source-aware diagnostics; never an ENOEXEC interpreter.
use crate::native;
use std::{
    io,
    os::{fd::AsFd, unix::ffi::OsStrExt},
};

pub fn execution_failure(
    cwd: Option<native::BorrowedDirectory<'_>>,
    origin: &[u8],
    line: &[u8],
    command: &[u8],
    failure: &io::Error,
) -> (i32, Vec<u8>) {
    let error = failure.raw_os_error().unwrap_or(libc::EBADF);
    let directory = error == libc::EACCES && {
        let path = std::ffi::OsStr::from_bytes(command);
        let stat = match cwd {
            Some(fd) => rustix::fs::statat(fd, path, rustix::fs::AtFlags::empty()),
            None => rustix::fs::stat(path),
        };
        stat.is_ok_and(|status| {
            rustix::fs::FileType::from_raw_mode(status.st_mode) == rustix::fs::FileType::Directory
        })
    };
    let message = if directory {
        b"Is a directory".to_vec()
    } else if error == libc::ENOEXEC {
        b"cannot execute binary file: Exec format error".to_vec()
    } else if !command.contains(&b'/') && matches!(error, libc::ENOENT | libc::ENOTDIR) {
        b"command not found".to_vec()
    } else {
        native::native_error_message(failure)
    };
    let code = if error == libc::ENOENT || (!command.contains(&b'/') && error == libc::ENOTDIR) {
        127
    } else {
        126
    };
    (
        code,
        [
            origin, b": line ", line, b": ", command, b": ", &message, b"\n",
        ]
        .concat(),
    )
}
pub fn dispatch(args: &[Vec<u8>]) -> io::Result<i32> {
    let [origin, line, command, rest @ ..] = args else {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "exec-site needs origin, line and executable",
        ));
    };
    let environment = native::environment()
        .into_iter()
        .filter(|(key, _)| key != b"MONK_LAUNCH_ORIGINAL" && key != b"MONK_LAUNCH_WRAPPER")
        .collect::<Vec<_>>();
    let failure = native::exec_process(&environment, command, rest);
    let (code, diagnostic) = execution_failure(None, origin, line, command, &failure);
    if native::initial_descriptor_open(2)
        && let Ok(fd) = native::inherited_fd(2)
    {
        let _ = native::write_all(fd.as_fd(), &diagnostic);
    }
    Ok(code)
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn command_not_found_and_format_errors() {
        assert_eq!(
            execution_failure(
                None,
                b"source",
                b"4",
                b"absent",
                &io::Error::from_raw_os_error(libc::ENOENT)
            ),
            (127, b"source: line 4: absent: command not found\n".to_vec())
        );
        assert_eq!(
            execution_failure(
                None,
                b"source",
                b"4",
                b"./bad",
                &io::Error::from_raw_os_error(libc::ENOEXEC)
            ),
            (
                126,
                b"source: line 4: ./bad: cannot execute binary file: Exec format error\n".to_vec()
            )
        );
    }
}
