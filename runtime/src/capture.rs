//! Shared command-substitution byte collector. One buffer owns the captured
//! value; NUL removal and final newline trimming never copy the completed body.
use std::{io, os::fd::BorrowedFd};
pub(crate) fn drain(
    fd: BorrowedFd<'_>,
    mut check: impl FnMut() -> io::Result<()>,
    mut warn: impl FnMut() -> io::Result<()>,
) -> io::Result<Vec<u8>> {
    let mut value = Vec::new();
    let mut chunk = [0; 65536];
    let mut warned = false;
    loop {
        check()?;
        let n = match rustix::io::read(fd, &mut chunk) {
            Ok(n) => n,
            Err(rustix::io::Errno::INTR) => {
                check()?;
                continue;
            }
            Err(e) => return Err(e.into()),
        };
        if n == 0 {
            break;
        }
        let bytes = &chunk[..n];
        if !warned && bytes.contains(&0) {
            warn()?;
            warned = true;
        }
        value.extend(bytes.iter().copied().filter(|b| *b != 0));
    }
    while value.last() == Some(&b'\n') {
        value.pop();
    }
    Ok(value)
}
#[cfg(test)]
mod tests {
    use super::*;
    use std::os::fd::AsFd;
    #[test]
    fn buffered_input_checks_cancellation_between_chunks() {
        use std::io::{Seek, Write};
        let mut file = tempfile::tempfile().unwrap();
        file.write_all(&vec![b'x'; 131072]).unwrap();
        file.rewind().unwrap();
        let mut checks = 0;
        let result = drain(
            file.as_fd(),
            || {
                checks += 1;
                if checks > 1 {
                    Err(io::ErrorKind::Interrupted.into())
                } else {
                    Ok(())
                }
            },
            || Ok(()),
        );
        assert_eq!(
            result.err().map(|error| error.kind()),
            Some(io::ErrorKind::Interrupted)
        );
    }
    #[test]
    fn raw_bytes_null_warning_and_trailing_newlines_share_one_collector() {
        let (reader, writer) = crate::native::private_pipe().unwrap();
        crate::native::write_all(writer.as_fd(), b"\xff\0a\n\0b\n\n").unwrap();
        drop(writer);
        let mut warnings = 0;
        let value = drain(
            reader.as_fd(),
            || Ok(()),
            || {
                warnings += 1;
                Ok(())
            },
        )
        .unwrap();
        assert_eq!(value, b"\xffa\nb");
        assert_eq!(warnings, 1);
    }
}
