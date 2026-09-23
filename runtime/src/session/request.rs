//! Borrowed ABI-2 decoding. Only fully validated operations reach the owner.
//! In particular, every pipeline stage is decoded before any pipe or child exists.
use super::table::OpenMode;
use super::*;
use crate::abi2::opcode::{body::Opcode as BodyOpcode, session::Opcode};
use crate::types::DescriptorMask;

pub(super) struct Request<'a> {
    pub inherited: Streams,
    pub operation: Operation<'a>,
}
pub(super) enum Operation<'a> {
    Release,
    Reset,
    Push,
    Pop(usize),
    Close(SourceFd),
    Data(SourceFd, &'a [u8]),
    Open {
        cwd: native::WorkingDirectory,
        origin: &'a [u8],
        line: &'a [u8],
        number: SourceFd,
        mode: OpenMode,
        path: &'a [u8],
    },
    Dup {
        origin: &'a [u8],
        line: &'a [u8],
        target: SourceFd,
        original: SourceFd,
        spelling: &'a [u8],
    },
    Endpoint {
        target: SourceFd,
        lease: SourceFd,
    },
    Read {
        origin: &'a [u8],
        line: &'a [u8],
        number: SourceFd,
        spelling: &'a [u8],
        config: crate::read::Config,
    },
    FinishSignal,
    Ping,
    Wait {
        origin: &'a [u8],
        line: &'a [u8],
        args: Vec<&'a [u8]>,
    },
    Launch {
        cwd: native::WorkingDirectory,
        environment: native::Environment,
        mode: LaunchMode<'a>,
        body: Body<'a>,
    },
}
pub(super) enum LaunchMode<'a> {
    Run,
    Spawn,
    Capture(&'a [u8]),
    Substitution(Direction),
}
pub(super) enum Direction {
    Input,
    Output,
}
pub(super) enum Body<'a> {
    Single(Stage<'a>),
    Pipeline {
        pipefail: bool,
        stages: Vec<Stage<'a>>,
    },
}
pub(super) enum Stage<'a> {
    External {
        command: &'a [u8],
        args: Vec<&'a [u8]>,
    },
    ExternalSite {
        origin: &'a [u8],
        line: &'a [u8],
        command: &'a [u8],
        args: Vec<&'a [u8]>,
    },
    Writer {
        origin: &'a [u8],
        line: &'a [u8],
        name: &'a [u8],
        descriptor: SourceFd,
        bytes: Bytes,
    },
    Region {
        script: &'a [u8],
        args: Vec<&'a [u8]>,
    },
    Snapshot {
        script: &'a [u8],
        level: &'a [u8],
        state: Vec<&'a [u8]>,
    },
}
fn count(bytes: &[u8]) -> io::Result<usize> {
    usize::try_from(integer(bytes)?).map_err(|_| invalid("invalid session count"))
}
impl<'a> Stage<'a> {
    fn decode(kind: BodyOpcode, args: &[&'a [u8]]) -> io::Result<Self> {
        Ok(match (kind, args) {
            (BodyOpcode::External, [command, args @ ..]) => Self::External {
                command,
                args: args.to_vec(),
            },
            (BodyOpcode::ExternalSite, [origin, line, command, args @ ..]) => Self::ExternalSite {
                origin,
                line,
                command,
                args: args.to_vec(),
            },
            (BodyOpcode::Builtin, [origin, line, name, args @ ..]) => {
                let args = args.iter().map(|v| v.to_vec()).collect::<Vec<_>>();
                let bytes = match *name {
                    b"echo" => crate::semantics::fields::echo_bytes(&args),
                    b"printf" => crate::semantics::printf::printf_bytes(&args)
                        .map_err(|_| invalid("unsupported printf"))?,
                    _ => return Err(invalid("unsupported session builtin")),
                };
                Self::Writer {
                    origin,
                    line,
                    name,
                    descriptor: source(1)?,
                    bytes,
                }
            }
            (BodyOpcode::DirectoryOutput, [origin, line, name, descriptor, value])
                if [b"pwd".as_slice(), b"cd", b"pushd", b"popd"].contains(name)
                    && (*descriptor == b"1" || *descriptor == b"2") =>
            {
                Self::Writer {
                    origin,
                    line,
                    name,
                    descriptor: source_bytes(descriptor)?,
                    bytes: value.to_vec(),
                }
            }
            (BodyOpcode::Body, [script, args @ ..]) => Self::Region {
                script,
                args: args.to_vec(),
            },
            (BodyOpcode::Snapshot, [script, level, state @ ..]) => Self::Snapshot {
                script,
                level,
                state: state.to_vec(),
            },
            _ => return Err(invalid("invalid session body")),
        })
    }
}
impl<'a> Body<'a> {
    fn decode(frames: &[&'a [u8]]) -> io::Result<Self> {
        let [kind, args @ ..] = frames else {
            return Err(invalid("empty session body"));
        };
        let kind = BodyOpcode::parse(kind).ok_or_else(|| invalid("invalid session body opcode"))?;
        if kind != BodyOpcode::Pipeline {
            return Stage::decode(kind, args).map(Self::Single);
        }
        let [policy, stages, rest @ ..] = args else {
            return Err(invalid("invalid pipeline"));
        };
        let mut rest = rest;
        if *policy != b"0" && *policy != b"1" {
            return Err(invalid("invalid pipefail"));
        }
        let number = count(stages)?;
        if number == 0 || number > rest.len() / 2 {
            return Err(invalid("invalid pipeline count"));
        }
        let mut stages = Vec::with_capacity(number);
        for _ in 0..number {
            let [kind, argc, tail @ ..] = rest else {
                return Err(invalid("invalid pipeline stage"));
            };
            let argc = count(argc)?;
            if argc > tail.len() {
                return Err(invalid("invalid pipeline arguments"));
            }
            stages.push(Stage::decode(
                BodyOpcode::parse(kind).ok_or_else(|| invalid("invalid pipeline opcode"))?,
                &tail[..argc],
            )?);
            rest = &tail[argc..];
        }
        if !rest.is_empty() {
            return Err(invalid("trailing pipeline frames"));
        }
        Ok(Self::Pipeline {
            pipefail: *policy == b"1",
            stages,
        })
    }
}
impl<'a> Request<'a> {
    pub fn decode(token: &[u8], fds: Vec<OwnedFd>, bytes: &'a [u8]) -> io::Result<Self> {
        let frames = protocol::borrowed(bytes).map_err(|_| invalid("invalid session frames"))?;
        let [authentication, op, mask, rest @ ..] = frames.as_slice() else {
            return Err(invalid("invalid session request"));
        };
        if *authentication != token {
            return Err(invalid("unauthenticated session request"));
        }
        let mask = u8::try_from(integer(mask)?)
            .ok()
            .and_then(|m| DescriptorMask::new(m).ok())
            .ok_or_else(|| invalid("invalid descriptor mask"))?;
        let op = Opcode::parse(op).ok_or_else(|| invalid("unknown session operation"))?;
        let needs_cwd = [
            Opcode::Run,
            Opcode::Spawn,
            Opcode::Capture,
            Opcode::Substitution,
            Opcode::FdOpen,
        ]
        .contains(&op);
        if fds.len() != mask.get().count_ones() as usize + usize::from(needs_cwd) {
            return Err(invalid("invalid session descriptor mask"));
        }
        let mut fds = fds.into_iter();
        let mut inherited = Streams::new();
        for n in 0..3 {
            let fd = source(n)?;
            if mask.contains(fd) {
                inherited.insert(fd, fds.next().unwrap());
            }
        }
        let mut cwd = fds
            .next()
            .map(native::WorkingDirectory::from_owned)
            .transpose()?;
        let operation = match (op, rest) {
            (Opcode::SubstitutionRelease, []) => Operation::Release,
            (Opcode::FdReset, []) => Operation::Reset,
            (Opcode::FdPush, []) => Operation::Push,
            (Opcode::FdPop, []) => Operation::Pop(1),
            (Opcode::FdPop, [n]) => Operation::Pop(count(n)?),
            (Opcode::FdClose, [n]) => Operation::Close(source_bytes(n)?),
            (Opcode::FdData, [n, value]) => Operation::Data(source_bytes(n)?, value),
            (Opcode::FdOpen, [_, origin, line, number, mode, path]) => Operation::Open {
                cwd: cwd.take().unwrap(),
                origin,
                line,
                number: source_bytes(number)?,
                mode: OpenMode::decode(mode)?,
                path,
            },
            (Opcode::FdDup, [origin, line, target, original]) => Operation::Dup {
                origin,
                line,
                target: source_bytes(target)?,
                original: source_bytes(original)?,
                spelling: original,
            },
            (Opcode::FdEndpoint, [_, _, target, lease]) => Operation::Endpoint {
                target: source_bytes(target)?,
                lease: source_bytes(lease)?,
            },
            (
                Opcode::Read,
                [
                    origin,
                    line,
                    number,
                    raw,
                    delimiter,
                    limit,
                    ifs,
                    mode,
                    names,
                ],
            ) => {
                let limit = integer(limit)?;
                let names = integer(names)?;
                let destination = match *mode {
                    b"reply" => crate::read::Destination::Reply,
                    b"scalar" if names > 0 => crate::read::Destination::Scalars(names as usize),
                    b"array" => crate::read::Destination::Array,
                    _ => return Err(invalid("invalid read destination")),
                };
                if (*raw != b"0" && *raw != b"1") || limit < -1 {
                    return Err(invalid("invalid read flags"));
                }
                Operation::Read {
                    origin,
                    line,
                    number: source_bytes(number)?,
                    spelling: number,
                    config: crate::read::Config {
                        raw: *raw == b"1",
                        delimiter: delimiter.first().copied().unwrap_or(0),
                        limit: if limit < 0 {
                            None
                        } else {
                            Some(limit as usize)
                        },
                        ifs: ifs.to_vec(),
                        destination,
                    },
                }
            }
            (Opcode::FinishSignal, [b"13"]) => Operation::FinishSignal,
            (Opcode::Ping, []) => Operation::Ping,
            (Opcode::Wait, [origin, line, args @ ..]) => Operation::Wait {
                origin,
                line,
                args: args.to_vec(),
            },
            (op, [_, n, payload @ ..])
                if [
                    Opcode::Run,
                    Opcode::Spawn,
                    Opcode::Capture,
                    Opcode::Substitution,
                ]
                .contains(&op) =>
            {
                let n = count(n)?;
                if n > payload.len() / 2 {
                    return Err(invalid("invalid environment count"));
                }
                let mut environment = Vec::with_capacity(n);
                for pair in payload[..n * 2].as_chunks::<2>().0 {
                    if pair[0].is_empty() || pair[0].contains(&b'=') {
                        return Err(invalid("invalid environment name"));
                    }
                    environment.push((pair[0].to_vec(), pair[1].to_vec()));
                }
                let body = &payload[n * 2..];
                let (mode, body) = match (op, body) {
                    (Opcode::Substitution, [direction, body @ ..]) => (
                        LaunchMode::Substitution(match *direction {
                            b"input" => Direction::Input,
                            b"output" => Direction::Output,
                            _ => return Err(invalid("invalid substitution direction")),
                        }),
                        Body::decode(body)?,
                    ),
                    (Opcode::Capture, [warning, kind, body @ ..])
                        if *kind == body_opcode::SNAPSHOT =>
                    {
                        (
                            LaunchMode::Capture(warning),
                            Body::Single(Stage::decode(BodyOpcode::Snapshot, body)?),
                        )
                    }
                    (Opcode::Spawn, body) => (LaunchMode::Spawn, Body::decode(body)?),
                    (Opcode::Run | Opcode::Capture, body) => (LaunchMode::Run, Body::decode(body)?),
                    _ => return Err(invalid("invalid session launch")),
                };
                Operation::Launch {
                    cwd: cwd.take().unwrap(),
                    environment,
                    mode,
                    body,
                }
            }
            _ => return Err(invalid("unknown session operation")),
        };
        Ok(Self {
            inherited,
            operation,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn borrowed_fields_keep_empty_and_non_utf8_values() {
        let bytes = b"token\0fd-data\x000\x003\0\xff\0";
        let request = Request::decode(b"token", vec![], bytes).unwrap();
        let Operation::Data(number, value) = request.operation else {
            panic!("data operation");
        };
        assert_eq!(number.get(), 3);
        assert_eq!(value, b"\xff");
        assert_eq!(value.as_ptr(), bytes[18..].as_ptr());
        let empty = Request::decode(b"token", vec![], b"token\0fd-data\x000\x003\0\0").unwrap();
        assert!(matches!(empty.operation, Operation::Data(_, b"")));
    }
    #[test]
    fn descriptor_count_cwd_and_mask_are_validated_before_dispatch() {
        assert!(Request::decode(b"token", vec![], b"token\0ping\x008\0").is_err());
        assert!(
            Request::decode(
                b"token",
                vec![],
                b"token\0run\x000\0\x000\0external\0/bin/true\0"
            )
            .is_err()
        );
        let file = tempfile::tempfile().unwrap();
        assert!(
            Request::decode(
                b"token",
                vec![file.into()],
                b"token\0run\x000\0\x000\0external\0/bin/true\0"
            )
            .is_err()
        );
        let directory = std::fs::File::open(".").unwrap();
        assert!(Request::decode(b"token", vec![directory.into()], b"token\0ping\x000\0").is_err());
    }
    #[test]
    fn source_descriptors_follow_mask_order_and_cwd_is_last() {
        use std::io::{Seek, Write};
        let mut input = tempfile::tempfile().unwrap();
        input.write_all(b"i").unwrap();
        input.rewind().unwrap();
        let mut error = tempfile::tempfile().unwrap();
        error.write_all(b"e").unwrap();
        error.rewind().unwrap();
        let cwd = std::fs::File::open(".").unwrap();
        let request = Request::decode(
            b"token",
            vec![input.into(), error.into(), cwd.into()],
            b"token\0run\x005\0\x000\0external\0/bin/true\0",
        )
        .unwrap();
        assert!(matches!(request.operation, Operation::Launch { .. }));
        assert!(!request.inherited.contains_key(&source(1).unwrap()));
        let mut byte = [0];
        rustix::io::read(&request.inherited[&source(0).unwrap()], &mut byte).unwrap();
        assert_eq!(&byte, b"i");
        rustix::io::read(&request.inherited[&source(2).unwrap()], &mut byte).unwrap();
        assert_eq!(&byte, b"e");
    }
    #[test]
    fn later_stage_payload_and_trailing_frames_are_rejected_as_one_body() {
        assert!(
            Body::decode(&[
                b"pipeline",
                b"0",
                b"2",
                b"external",
                b"1",
                b"/bin/true",
                b"builtin",
                b"3",
                b"src",
                b"1",
                b"bad"
            ])
            .is_err()
        );
        assert!(
            Body::decode(&[
                b"pipeline",
                b"0",
                b"1",
                b"external",
                b"1",
                b"/bin/true",
                b"extra"
            ])
            .is_err()
        );
        assert!(Body::decode(&[b"pipeline", b"0", b"1", b"external", b"2", b"/bin/true"]).is_err());
    }
}
