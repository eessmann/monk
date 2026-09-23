//! CLI dispatch for the byte-framed ABI. No source language evaluator lives here.
use crate::abi2::opcode::cli as opcode;
use crate::{abi2, capsule, child, directory, exec, launch, native, protocol, semantics, session};
use clap::Parser;
use std::{
    ffi::OsString,
    io,
    os::{fd::AsFd, unix::ffi::OsStringExt},
};
/// The ABI header is positional even though its literal words begin with `--`.
/// A parser-only end-of-options marker keeps every original argument opaque.
#[derive(Parser)]
#[command(disable_help_flag = true, disable_version_flag = true)]
struct ParsedCli {
    header: OsString,
    version: Option<OsString>,
    operation: Option<OsString>,
    arguments: Vec<OsString>,
}

enum Invocation {
    Describe,
    Abi {
        operation: Vec<u8>,
        arguments: Vec<Vec<u8>>,
    },
}
impl Invocation {
    fn parse(args: Vec<OsString>) -> Result<Self, Vec<u8>> {
        let invalid = || b"expected --abi 2 OPERATION or --describe".to_vec();
        let parsed = ParsedCli::try_parse_from(
            [OsString::from("monk-runtime"), OsString::from("--")]
                .into_iter()
                .chain(args),
        )
        .map_err(|_| invalid())?;
        match (parsed.header, parsed.version, parsed.operation) {
            (header, None, None) if header == "--describe" && parsed.arguments.is_empty() => {
                Ok(Self::Describe)
            }
            (header, Some(version), Some(operation)) if header == "--abi" && version == "2" => {
                Ok(Self::Abi {
                    operation: operation.into_vec(),
                    arguments: parsed
                        .arguments
                        .into_iter()
                        .map(OsString::into_vec)
                        .collect(),
                })
            }
            _ => Err(invalid()),
        }
    }
}

pub(crate) fn run() -> ! {
    let result = native::initialize_signals()
        .map_err(io_failure)
        .and_then(|()| dispatch(std::env::args_os().skip(1).collect()));
    let status = match result {
        Ok(status) => status,
        Err(message) => {
            diagnostic(&[b"monk-runtime: ".as_slice(), &message, b"\n"].concat());
            125
        }
    };
    std::process::exit(status);
}
fn io_failure(error: io::Error) -> Vec<u8> {
    if error.raw_os_error().is_some() {
        native::native_error_message(&error)
    } else {
        error.to_string().into_bytes()
    }
}
fn input() -> Result<Vec<u8>, Vec<u8>> {
    let fd = native::inherited_fd(0).map_err(io_failure)?;
    child::read_all(fd.as_fd()).map_err(io_failure)
}
fn output(bytes: &[u8]) -> Result<i32, Vec<u8>> {
    if !bytes.is_empty() {
        let fd = native::inherited_fd(1).map_err(io_failure)?;
        native::write_all(fd.as_fd(), bytes).map_err(io_failure)?;
    }
    Ok(0)
}
fn diagnostic(bytes: &[u8]) {
    if native::initial_descriptor_open(2)
        && let Ok(fd) = native::inherited_fd(2)
    {
        let _ = native::write_all(fd.as_fd(), bytes);
    }
}
fn dispatch(args: Vec<OsString>) -> Result<i32, Vec<u8>> {
    let description =
        abi2::description().ok_or_else(|| b"unsupported native OS or architecture".to_vec())?;
    let (operation, arguments) = match Invocation::parse(args)? {
        Invocation::Describe => return output(description.as_bytes()),
        Invocation::Abi {
            operation,
            arguments,
        } => (operation, arguments),
    };
    let rest = arguments.as_slice();
    let raw = match operation.as_slice() {
        opcode::SESSION_GUARDIAN => Some(capsule::guardian(rest)),
        opcode::LAUNCH => Some(launch::dispatch(rest)),
        opcode::EXEC_SITE => Some(exec::dispatch(rest)),
        opcode::SESSION_EXEC_ERROR => Some(session::exec_error(rest)),
        opcode::SESSION_WRITE => Some(session::writer(rest)),
        opcode::SESSION_RUN => Some(session::dispatch(rest)),
        opcode::SESSION_DIRECTORY_DIAGNOSTIC if rest.is_empty() => {
            Some(session::directory_diagnostic())
        }
        opcode::SESSION_CLIENT if rest.is_empty() => Some(session::client(false)),
        opcode::SESSION_CLIENT if rest == [b"--reply"] => Some(session::client(true)),
        opcode::CHILD_RUN if rest.is_empty() => Some(child::dispatch(false, None)),
        opcode::CHILD_CAPTURE if rest.is_empty() => Some(child::dispatch(true, None)),
        opcode::CHILD_RUN_SESSION if rest.is_empty() => {
            Some(child::dispatch(false, Some(session::child)))
        }
        opcode::CHILD_CAPTURE_SESSION if rest.is_empty() => {
            Some(child::dispatch(true, Some(session::child)))
        }
        _ => None,
    };
    if let Some(result) = raw {
        return result.map_err(io_failure);
    }
    if operation == opcode::RAISE_SIGNAL {
        if rest == [b"13"] {
            native::terminate_with(libc::SIGPIPE);
        }
        return Err(b"raise-signal needs signal 13".to_vec());
    }
    if !rest.is_empty() {
        return Err(b"expected --abi 2 OPERATION or --describe".to_vec());
    }
    match operation.as_slice() {
        opcode::SESSION_PREPARE => {
            let frames = protocol::decode(&input()?)?;
            let [script] = frames.as_slice() else {
                return Err(b"session-prepare expects one compiled Fish script".to_vec());
            };
            return output(&capsule::prepare(script).map_err(io_failure)?);
        }
        opcode::PIPE_PATHS => {
            return Ok(if native::probe_pipe_paths().unwrap_or(false) {
                0
            } else {
                125
            });
        }
        opcode::DESCRIPTOR_STATE => {
            return Ok((0..3)
                .filter(|fd| native::initial_descriptor_open(*fd))
                .fold(0, |mask, fd| mask | (1 << fd)));
        }
        opcode::DIRECTORY_PHYSICAL => return output(&directory::physical().map_err(io_failure)?),
        opcode::DIRECTORY_DIAGNOSTIC => {
            let (_, _, _, message) = directory::diagnostic(&input()?)?;
            diagnostic(&message);
            return Ok(0);
        }
        opcode::WRITE_BUILTIN => return write_builtin(),
        _ => {}
    }
    let frames = protocol::decode(&input()?)?;
    match (operation.as_slice(), frames.as_slice()) {
        (opcode::BYTES_PLATFORM, [darwin, linux]) => {
            let darwin = decode_hex(darwin)?;
            let linux = decode_hex(linux)?;
            return output(&protocol::encode(&[if cfg!(target_os = "macos") {
                darwin
            } else {
                linux
            }]));
        }
        (opcode::DIRECTORY_PATH_BOUND, [cwd, operand]) => {
            return Ok(if directory::path_bound(cwd, operand) {
                0
            } else {
                125
            });
        }
        (opcode::DIRECTORY_INITIAL_OLDPWD, [path]) => {
            return Ok(if directory::oldpwd_valid(path) { 0 } else { 1 });
        }
        (opcode::DIRECTORY_VALIDATE, _) => {
            return Ok(if directory::validate(&frames) { 0 } else { 125 });
        }
        (opcode::DIRECTORY_STACK, _) => return output(&directory::stack(&frames)?),
        _ => {}
    }
    let op = std::str::from_utf8(&operation)
        .map_err(|_| b"unknown operation or invalid frame count".to_vec())?;
    match semantics::dispatch(op, &frames)? {
        Some(semantics::Output::Bytes(bytes)) => output(&bytes),
        Some(semantics::Output::Frames(frames)) => output(&protocol::encode(&frames)),
        Some(semantics::Output::Status(status)) => Ok(status),
        None => Err(b"unknown operation or invalid frame count".to_vec()),
    }
}
fn write_builtin() -> Result<i32, Vec<u8>> {
    let frames = protocol::decode(&input()?)?;
    let [origin, line, name, args @ ..] = frames.as_slice() else {
        return Err(
            b"write-builtin needs origin, positive source line, builtin and arguments".to_vec(),
        );
    };
    if origin.is_empty()
        || line.is_empty()
        || line[0] == b'0'
        || !line.iter().all(u8::is_ascii_digit)
    {
        return Err(
            b"write-builtin needs origin, positive source line, builtin and arguments".to_vec(),
        );
    }
    let bytes = match name.as_slice() {
        opcode::ECHO => semantics::fields::echo_bytes(args),
        opcode::PRINTF => semantics::printf::printf_bytes(args)?,
        b"echo-bytes" => {
            let [value] = args else {
                return Err(b"echo-bytes needs one payload".to_vec());
            };
            value.clone()
        }
        _ => return Err(b"write-builtin needs an admitted builtin".to_vec()),
    };
    if bytes.is_empty() {
        return Ok(0);
    }
    let result = if native::initial_descriptor_open(1) {
        native::inherited_fd(1).and_then(|fd| native::write_all(fd.as_fd(), &bytes))
    } else {
        Err(io::Error::from_raw_os_error(libc::EBADF))
    };
    if let Err(error) = result {
        let name = if name == b"echo-bytes" {
            opcode::ECHO
        } else {
            name
        };
        diagnostic(
            &[
                origin.as_slice(),
                b": line ",
                line,
                b": ",
                name,
                b": write error: ",
                &native::native_error_message(&error),
                b"\n",
            ]
            .concat(),
        );
        return Ok(1);
    }
    Ok(0)
}
fn decode_hex(bytes: &[u8]) -> Result<Vec<u8>, Vec<u8>> {
    let digit = |b| match b {
        b'0'..=b'9' => Some(b - b'0'),
        b'a'..=b'f' => Some(b - b'a' + 10),
        b'A'..=b'F' => Some(b - b'A' + 10),
        _ => None,
    };
    if !bytes.len().is_multiple_of(2) {
        return Err(b"invalid platform byte hex".to_vec());
    }
    bytes
        .as_chunks::<2>()
        .0
        .iter()
        .map(|pair| {
            digit(pair[0])
                .zip(digit(pair[1]))
                .map(|(high, low)| high * 16 + low)
                .ok_or_else(|| b"invalid platform byte hex".to_vec())
        })
        .collect()
}
