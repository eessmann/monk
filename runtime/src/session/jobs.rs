use super::*;
pub(super) struct Job {
    pub children: Vec<native::RunningChild>,
    pub pipefail: bool,
    pub substitution: bool,
    pub implicit: bool,
}
impl Job {
    pub fn pid(&self) -> i32 {
        self.children.last().expect("nonempty job").pid()
    }
    pub fn poll(&mut self) -> io::Result<Option<i32>> {
        let mut all = true;
        let mut codes = Vec::new();
        for child in &mut self.children {
            match child.try_wait()? {
                Some(status) => codes.push(status.code()),
                None => all = false,
            }
        }
        if !all {
            return Ok(None);
        }
        Ok(Some(if self.pipefail {
            codes.into_iter().rev().find(|c| *c != 0).unwrap_or(0)
        } else {
            *codes.last().unwrap_or(&0)
        }))
    }
    pub fn terminate(&mut self) {
        for child in &mut self.children {
            let _ = child.terminate();
        }
    }
}
pub(super) fn start(
    asynchronous: bool,
    diagnostic: Option<BorrowedFd<'_>>,
    streams: &Streams,
    cwd: native::BorrowedDirectory<'_>,
    env: &native::Environment,
    kind: &[u8],
    args: &[Bytes],
) -> io::Result<Job> {
    let mut job = Job {
        children: Vec::new(),
        pipefail: false,
        substitution: false,
        implicit: true,
    };
    if kind != body_opcode::PIPELINE {
        job.children.push(user(
            asynchronous,
            if asynchronous {
                streams.get(&source(2)?).map(AsFd::as_fd)
            } else {
                diagnostic
            },
            streams,
            cwd,
            env,
            kind,
            args,
        )?);
        return Ok(job);
    }
    let [policy, count, rest @ ..] = args else {
        return Err(invalid("invalid pipeline frames"));
    };
    if policy != b"0" && policy != b"1" {
        return Err(invalid("invalid pipeline status policy"));
    }
    job.pipefail = policy == b"1";
    let count = usize::try_from(integer(count)?).map_err(|_| invalid("invalid pipeline count"))?;
    if count == 0 {
        return Err(invalid("empty pipeline"));
    }
    let mut stages = Vec::new();
    let mut remaining = rest;
    for _ in 0..count {
        let [kind, argc, tail @ ..] = remaining else {
            return Err(invalid("invalid pipeline stage frames"));
        };
        if ![
            body_opcode::EXTERNAL,
            body_opcode::EXTERNAL_SITE,
            body_opcode::BODY,
            body_opcode::SNAPSHOT,
            body_opcode::BUILTIN,
            body_opcode::DIRECTORY_OUTPUT,
        ]
        .contains(&kind.as_slice())
        {
            return Err(invalid("invalid pipeline stage kind"));
        }
        let argc = usize::try_from(integer(argc)?)
            .map_err(|_| invalid("invalid pipeline stage arguments"))?;
        if argc > tail.len() {
            return Err(invalid("invalid pipeline stage arguments"));
        }
        stages.push((kind, &tail[..argc]));
        remaining = &tail[argc..];
    }
    if !remaining.is_empty() {
        return Err(invalid("invalid pipeline stage frames"));
    }
    let pipes = (0..count - 1)
        .map(|_| native::private_pipe())
        .collect::<io::Result<Vec<_>>>()?;
    let result = (|| {
        for (index, (kind, args)) in stages.iter().enumerate() {
            let mut stage = table::copy(streams)?;
            if index > 0 {
                stage.insert(
                    source(0)?,
                    native::duplicate_private(pipes[index - 1].0.as_fd())?,
                );
            }
            if index + 1 < count {
                stage.insert(
                    source(1)?,
                    native::duplicate_private(pipes[index].1.as_fd())?,
                );
            }
            job.children.push(user(
                asynchronous,
                stage.get(&source(2)?).map(AsFd::as_fd),
                &stage,
                cwd,
                env,
                kind,
                args,
            )?);
        }
        Ok(())
    })();
    if let Err(error) = result {
        job.terminate();
        return Err(error);
    }
    Ok(job)
}
fn user(
    asynchronous: bool,
    diagnostic: Option<BorrowedFd<'_>>,
    streams: &Streams,
    cwd: native::BorrowedDirectory<'_>,
    env: &native::Environment,
    kind: &[u8],
    args: &[Bytes],
) -> io::Result<native::RunningChild> {
    let clean = clean_environment(env.clone());
    match (kind, args) {
        (body_opcode::EXTERNAL, [command, args @ ..]) => {
            native::spawn(streams, Some(cwd), &clean, command, args, asynchronous)
                .or_else(|_| native::spawn_status(127))
        }
        (body_opcode::EXTERNAL_SITE, [origin, line, command, args @ ..]) => {
            match native::spawn(streams, Some(cwd), &clean, command, args, asynchronous) {
                Ok(child) => Ok(child),
                Err(error) => {
                    let (code, message) =
                        crate::exec::execution_failure(Some(cwd), origin, line, command, &error);
                    let directory = capsule::workspace("monk-exec-error-")?;
                    let path = capsule::file(directory.path(), &message)?;
                    let child = native::spawn(
                        streams,
                        Some(cwd),
                        &clean,
                        &capsule::runtime()?,
                        &[
                            b"--abi".to_vec(),
                            b"2".to_vec(),
                            b"session-exec-error".to_vec(),
                            capsule::bytes(directory.path()),
                            path,
                            decimal(code),
                        ],
                        asynchronous,
                    )?;
                    let _ = directory.keep();
                    Ok(child)
                }
            }
        }
        (body_opcode::BUILTIN, [origin, line, name, args @ ..]) => {
            let bytes = match name.as_slice() {
                b"echo" => crate::semantics::fields::echo_bytes(args),
                b"printf" => crate::semantics::printf::printf_bytes(args)
                    .map_err(|_| invalid("unsupported printf"))?,
                _ => return Err(invalid("unsupported session builtin")),
            };
            writer(
                asynchronous,
                diagnostic,
                streams,
                cwd,
                &clean,
                origin,
                line,
                name,
                &bytes,
            )
        }
        (body_opcode::DIRECTORY_OUTPUT, [origin, line, name, descriptor, value])
            if [b"pwd".as_slice(), b"cd", b"pushd", b"popd"].contains(&name.as_slice())
                && (descriptor == b"1" || descriptor == b"2") =>
        {
            let mut output = table::copy(streams)?;
            if descriptor == b"2" {
                output.remove(&source(1)?);
                if let Some(fd) = streams.get(&source(2)?).map(AsFd::as_fd).or(diagnostic) {
                    output.insert(source(1)?, native::duplicate_private(fd)?);
                }
            }
            writer(
                asynchronous,
                diagnostic,
                &output,
                cwd,
                &clean,
                origin,
                line,
                name,
                value,
            )
        }
        (body_opcode::BODY, [script, args @ ..]) => {
            region(asynchronous, streams, cwd, &clean, script, args, None, None)
        }
        (body_opcode::SNAPSHOT, [script, level, state @ ..]) => region(
            asynchronous,
            streams,
            cwd,
            &clean,
            script,
            &[],
            Some(level),
            Some(state),
        ),
        _ => Err(invalid("empty session command")),
    }
}
#[allow(clippy::too_many_arguments)]
fn writer(
    asynchronous: bool,
    diagnostic: Option<BorrowedFd<'_>>,
    streams: &Streams,
    cwd: native::BorrowedDirectory<'_>,
    env: &native::Environment,
    origin: &[u8],
    line: &[u8],
    name: &[u8],
    bytes: &[u8],
) -> io::Result<native::RunningChild> {
    let directory = capsule::workspace("monk-writer-")?;
    let path = capsule::file(directory.path(), bytes)?;
    let mut output = table::copy(streams)?;
    if !output.contains_key(&source(2)?)
        && let Some(fd) = diagnostic
    {
        output.insert(source(2)?, native::duplicate_private(fd)?);
    }
    let child = native::spawn(
        &output,
        Some(cwd),
        env,
        &capsule::runtime()?,
        &[
            b"--abi".to_vec(),
            b"2".to_vec(),
            b"session-write".to_vec(),
            capsule::bytes(directory.path()),
            path,
            origin.to_vec(),
            line.to_vec(),
            name.to_vec(),
        ],
        asynchronous,
    )?;
    let _ = directory.keep();
    Ok(child)
}
#[allow(clippy::too_many_arguments)]
fn region(
    asynchronous: bool,
    streams: &Streams,
    cwd: native::BorrowedDirectory<'_>,
    env: &native::Environment,
    script: &[u8],
    args: &[Bytes],
    level: Option<&Bytes>,
    state: Option<&[Bytes]>,
) -> io::Result<native::RunningChild> {
    let directory = capsule::workspace("monk-region-")?;
    let path = capsule::file(directory.path(), script)?;
    let args = match state {
        Some(values) => vec![capsule::file(directory.path(), &protocol::encode(values))?],
        None => args.to_vec(),
    };
    let manifest = streams
        .keys()
        .filter(|n| n.get() > 2)
        .map(|n| n.get().to_string())
        .collect::<Vec<_>>()
        .join(",")
        .into_bytes();
    let mut env = env.clone();
    env.insert(0, (b"MONK_SESSION_FDS".to_vec(), manifest));
    if let Some(level) = level {
        env.retain(|(k, _)| k != b"SHLVL");
        env.insert(0, (b"SHLVL".to_vec(), level.clone()));
    }
    let mut command = vec![
        b"--abi".to_vec(),
        b"2".to_vec(),
        b"session-run".to_vec(),
        b"--owned".to_vec(),
        capsule::bytes(directory.path()),
        path,
    ];
    command.extend(args);
    let child = native::spawn(
        streams,
        Some(cwd),
        &env,
        &capsule::runtime()?,
        &command,
        asynchronous,
    )?;
    let _ = directory.keep();
    Ok(child)
}
