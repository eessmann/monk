//! Framed transport for materialized Fish children and supervised child callback.
use crate::native::Workspace;
use crate::{
    native,
    types::{DescriptorMask, SourceFd},
};
use std::{
    io,
    os::fd::{AsFd, BorrowedFd},
};

pub struct Request {
    pub warning: Vec<u8>,
    pub mask: DescriptorMask,
    pub script: Vec<u8>,
    pub level: Vec<u8>,
    pub state: Vec<u8>,
}
pub type SessionCallback = fn(bool, Request) -> io::Result<(i32, Vec<u8>)>;
pub fn decode(bytes: &[u8]) -> io::Result<Request> {
    let bad = || io::Error::new(io::ErrorKind::InvalidInput, "invalid child transport");
    let mut rest = bytes;
    let mut frame = || -> io::Result<Vec<u8>> {
        let end = rest.iter().position(|b| *b == 0).ok_or_else(bad)?;
        let value = rest[..end].to_vec();
        rest = &rest[end + 1..];
        Ok(value)
    };
    let warning = frame()?;
    let mask = frame()?;
    let script = frame()?;
    let level = frame()?;
    let mask = std::str::from_utf8(&mask)
        .ok()
        .and_then(|s| s.parse::<u8>().ok())
        .ok_or_else(bad)?;
    let mask = DescriptorMask::new(mask)?;
    let mut digits = level.as_slice();
    if digits.first() == Some(&b'-') || digits.first() == Some(&b'+') {
        digits = &digits[1..];
    }
    if digits.is_empty() || !digits.iter().all(u8::is_ascii_digit) {
        return Err(bad());
    }
    if !rest.is_empty() && rest.last() != Some(&0) {
        return Err(bad());
    }
    Ok(Request {
        warning,
        mask,
        script,
        level,
        state: rest.to_vec(),
    })
}
pub fn dispatch(capture: bool, supervised: Option<SessionCallback>) -> io::Result<i32> {
    let result = (|| {
        let _signals = native::SignalGuard::install()?;
        let input = native::inherited_fd(0)?;
        let bytes = read_all(input.as_fd())?;
        let request = decode(&bytes)?;
        if request.mask.contains(SourceFd::new(0)?) && !native::initial_descriptor_open(3) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "missing original stdin descriptor",
            ));
        }
        if let Some(callback) = supervised {
            callback(capture, request)
        } else {
            run(capture, request)
        }
    })();
    if let Some(signal) = native::pending_signal() {
        return Ok(128 + signal);
    }
    match result {
        Ok((code, output)) => {
            if capture {
                packet(b"ok", code, &output)?;
                Ok(0)
            } else {
                Ok(code)
            }
        }
        Err(_) => {
            if capture {
                match packet(b"error", 125, b"child-transport-failure") {
                    Ok(()) => Ok(0),
                    Err(_) => Ok(125),
                }
            } else {
                diagnostic(b"monk: child transport failed\n");
                Ok(125)
            }
        }
    }
}
fn run(capture: bool, request: Request) -> io::Result<(i32, Vec<u8>)> {
    let workspace = Workspace::new(b"monk-child-")?;
    let script = workspace.file(&restore_closed_streams(
        capture,
        request.mask,
        &request.script,
    ))?;
    let state = workspace.file(&request.state)?;
    let mut environment = native::environment();
    environment.retain(|(key, _)| key != b"SHLVL");
    environment.insert(0, (b"SHLVL".to_vec(), normalize_integer(&request.level)));
    let mut streams = native::Streams::new();
    for fd in 0..3 {
        let source = SourceFd::new(fd)?;
        if request.mask.contains(source) {
            let inherited = if fd == 0 { 3 } else { fd };
            streams.insert(source, native::inherited_fd(inherited)?);
        }
    }
    let pipe = if capture {
        let (reader, writer) = native::private_pipe()?;
        streams.insert(SourceFd::new(1)?, writer);
        Some(reader)
    } else {
        None
    };
    let mut child = native::spawn(
        &streams,
        None,
        &environment,
        b"fish",
        &[b"--no-config".to_vec(), script, state],
        false,
    )?;
    drop(streams);
    let output_result = if let Some(reader) = pipe {
        drain_capture(
            reader.as_fd(),
            request.mask.contains(SourceFd::new(2)?),
            &request.warning,
        )
    } else {
        Ok(Vec::new())
    };
    let output = match output_result {
        Ok(output) => output,
        Err(error) => {
            let _ = child.terminate();
            return Err(error);
        }
    };
    let code = child.complete()?.outcome().code();
    Ok((code, output))
}

fn normalize_integer(value: &[u8]) -> Vec<u8> {
    let negative = value.first() == Some(&b'-');
    let digits = value
        .strip_prefix(b"-")
        .or_else(|| value.strip_prefix(b"+"))
        .unwrap_or(value);
    let start = digits
        .iter()
        .position(|b| *b != b'0')
        .unwrap_or(digits.len());
    if start == digits.len() {
        return b"0".to_vec();
    }
    if negative {
        [b"-".as_slice(), &digits[start..]].concat()
    } else {
        digits[start..].to_vec()
    }
}
pub fn restore_closed_streams(capture: bool, mask: DescriptorMask, script: &[u8]) -> Vec<u8> {
    let mut result = [b"begin\n".as_slice(), script, b"\nend"].concat();
    for fd in 0..3 {
        if mask.get() & (1 << fd) == 0 && !(capture && fd == 1) {
            result.extend_from_slice(format!(" {fd}>&-").as_bytes());
        }
    }
    result.push(b'\n');
    result
}
pub fn read_all(fd: BorrowedFd<'_>) -> io::Result<Vec<u8>> {
    crate::transport::read_all(fd)
}
fn drain_capture(fd: BorrowedFd<'_>, stderr_open: bool, warning: &[u8]) -> io::Result<Vec<u8>> {
    crate::capture::drain(
        fd,
        || {
            if native::pending_signal().is_some() {
                Err(io::ErrorKind::Interrupted.into())
            } else {
                Ok(())
            }
        },
        || {
            if stderr_open {
                diagnostic(warning);
            }
            Ok(())
        },
    )
}
fn packet(tag: &[u8], code: i32, bytes: &[u8]) -> io::Result<()> {
    native::write_all(
        native::inherited_fd(1)?.as_fd(),
        &[tag, b"\0", code.to_string().as_bytes(), b"\0", bytes, b"\0"].concat(),
    )
}
fn diagnostic(bytes: &[u8]) {
    if let Ok(fd) = native::inherited_fd(2) {
        let _ = native::write_all(fd.as_fd(), bytes);
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn child_frames_keep_raw_bytes() {
        let request = decode(
            b"warn\0"
                .iter()
                .chain(b"5\0\xff\0-0005\0a\0")
                .copied()
                .collect::<Vec<_>>()
                .as_slice(),
        )
        .unwrap();
        assert_eq!(request.script, b"\xff");
        assert_eq!(request.state, b"a\0");
        assert_eq!(normalize_integer(&request.level), b"-5");
    }
    #[test]
    fn child_rejects_bad_mask_and_unterminated_state() {
        assert!(
            decode(
                b"\0"
                    .iter()
                    .chain(b"8\0script\x001\0")
                    .copied()
                    .collect::<Vec<_>>()
                    .as_slice()
            )
            .is_err()
        );
        assert!(decode(b"w\x007\x00s\x001\x00bad").is_err());
    }
    #[test]
    fn capture_restores_only_original_closed_streams() {
        assert_eq!(
            restore_closed_streams(true, DescriptorMask::new(0).unwrap(), b"printf x"),
            b"begin\nprintf x\nend 0>&- 2>&-\n"
        );
    }
}
