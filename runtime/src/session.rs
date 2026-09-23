//! One process owns signal policy, all child identities, wait caches and source scopes.
use crate::abi2::opcode::{body as body_opcode, session as opcode};

mod jobs;
mod table;
use crate::{
    capsule,
    native::{self, Streams},
    protocol::{self, Bytes},
    transport,
    types::SourceFd,
};
use capsule::invalid;
use rustix::event::{PollFd, PollFlags, Timespec, poll};
use std::{
    collections::BTreeMap,
    io,
    os::{
        fd::{AsFd, BorrowedFd, OwnedFd},
        unix::ffi::OsStrExt,
    },
};
fn integer(value: &[u8]) -> io::Result<i64> {
    std::str::from_utf8(value)
        .ok()
        .and_then(|v| v.trim().parse().ok())
        .ok_or_else(|| invalid("invalid session integer"))
}
fn ensure_active(evaluator: &mut native::RunningChild) -> io::Result<()> {
    if native::pending_signal().is_some() || evaluator.try_wait()?.is_some() {
        return Err(io::ErrorKind::Interrupted.into());
    }
    Ok(())
}
fn decimal(number: impl ToString) -> Bytes {
    number.to_string().into_bytes()
}
fn source(number: i32) -> io::Result<SourceFd> {
    SourceFd::new(number)
}
fn source_bytes(number: &[u8]) -> io::Result<SourceFd> {
    source(i32::try_from(integer(number)?).map_err(|_| invalid("invalid source descriptor"))?)
}
fn reply(code: i32, pid: Option<i32>) -> Bytes {
    protocol::encode(&[
        b"ok".to_vec(),
        decimal(code),
        pid.map(decimal).unwrap_or_default(),
    ])
}
fn clean_environment(mut env: native::Environment) -> native::Environment {
    env.retain(|(name, _)| {
        ![
            b"MONK_SESSION_SOCKET".as_slice(),
            b"MONK_SESSION_TOKEN",
            b"MONK_SESSION_REPLY",
            b"MONK_SESSION_FDS",
            b"MONK_LAUNCH_ORIGINAL",
            b"MONK_LAUNCH_WRAPPER",
        ]
        .contains(&name.as_slice())
    });
    env
}
fn variable(name: &str) -> io::Result<Bytes> {
    std::env::var_os(name)
        .map(|s| s.as_bytes().to_vec())
        .ok_or_else(|| invalid("missing session variable"))
}
fn environment_frames(env: &native::Environment) -> Vec<Bytes> {
    env.iter()
        .flat_map(|(k, v)| [k.clone(), v.clone()])
        .collect()
}
fn write_stream(streams: &Streams, number: i32, bytes: &[u8]) -> io::Result<()> {
    if let Some(fd) = streams.get(&source(number)?) {
        native::write_all(fd.as_fd(), bytes)?;
    }
    Ok(())
}
struct Owner {
    table: table::Table,
    diagnostic: Option<OwnedFd>,
    endpoints: BTreeMap<SourceFd, crate::types::EndpointLease>,
    jobs: BTreeMap<i32, jobs::Job>,
    evaluator: native::RunningChild,
    finish_signal: Option<i32>,
}
impl Owner {
    fn diagnostics(&self, streams: &Streams) -> io::Result<Streams> {
        let mut result = table::copy(streams)?;
        if !result.contains_key(&source(2)?)
            && let Some(fd) = &self.diagnostic
        {
            result.insert(source(2)?, native::duplicate_private(fd.as_fd())?);
        }
        Ok(result)
    }
    fn interrupted(&mut self) -> io::Result<()> {
        ensure_active(&mut self.evaluator)
    }
    fn wait(&mut self, pid: i32) -> io::Result<i32> {
        loop {
            self.interrupted()?;
            let Some(job) = self.jobs.get_mut(&pid) else {
                return Ok(127);
            };
            if let Some(code) = job.poll()? {
                return Ok(code);
            }
            match poll(
                &mut [],
                Some(&Timespec {
                    tv_sec: 0,
                    tv_nsec: 1_000_000,
                }),
            ) {
                Ok(_) | Err(rustix::io::Errno::INTR) => {}
                Err(e) => return Err(e.into()),
            };
        }
    }
    fn supersede(&mut self) {
        for job in self.jobs.values_mut() {
            if job.substitution {
                job.implicit = false;
            }
        }
    }
    fn wait_arguments(
        &mut self,
        diagnostics: &Streams,
        origin: &[u8],
        line: &[u8],
        args: &[Bytes],
    ) -> io::Result<i32> {
        let prefix = [origin, b": line ", line, b": wait: "].concat();
        let diagnostic =
            |text: &[u8]| write_stream(diagnostics, 2, &[prefix.as_slice(), text].concat());
        if args.is_empty() {
            let pids = self
                .jobs
                .iter()
                .filter(|(_, job)| job.implicit)
                .map(|(pid, _)| *pid)
                .collect::<Vec<_>>();
            for pid in pids {
                self.wait(pid)?;
                self.jobs.remove(&pid);
            }
            return Ok(0);
        }
        if args[0].len() > 1 && args[0][0] == b'-' {
            diagnostic(
                &[
                    &args[0][..2],
                    b": invalid option\nwait: usage: wait [-fn] [-p var] [id ...]\n",
                ]
                .concat(),
            )?;
            return Ok(2);
        }
        let mut code = 0;
        for arg in args {
            let number = std::str::from_utf8(arg)
                .ok()
                .and_then(|s| s.parse::<i32>().ok())
                .filter(|v| *v >= 0 && !arg.is_empty() && arg.iter().all(u8::is_ascii_digit));
            code = if let Some(pid) = number {
                if !self.jobs.contains_key(&pid) {
                    diagnostic(format!("pid {pid} is not a child of this shell\n").as_bytes())?;
                }
                self.wait(pid)?
            } else {
                diagnostic(&[b"`", arg.as_slice(), b"': not a pid or valid job spec\n"].concat())?;
                1
            };
        }
        Ok(code)
    }
    fn descriptor_failure(
        &self,
        diagnostics: &Streams,
        origin: &[u8],
        line: &[u8],
        operand: &[u8],
        failure: io::Error,
    ) -> io::Result<Bytes> {
        write_stream(
            diagnostics,
            2,
            &[
                origin,
                b": line ",
                line,
                b": ",
                operand,
                b": ",
                &native::native_error_message(&failure),
                b"\n",
            ]
            .concat(),
        )?;
        Ok(reply(1, None))
    }
    fn request(&mut self, token: &[u8], fds: Vec<OwnedFd>, bytes: &[u8]) -> io::Result<Bytes> {
        let frames = protocol::decode(bytes).map_err(|_| invalid("invalid session frames"))?;
        let [authentication, operation, mask, rest @ ..] = frames.as_slice() else {
            return Err(invalid("invalid session request"));
        };
        if authentication != token {
            return Err(invalid("unauthenticated session request"));
        }
        let mask = u8::try_from(integer(mask)?)
            .ok()
            .and_then(|m| crate::types::DescriptorMask::new(m).ok())
            .ok_or_else(|| invalid("invalid descriptor mask"))?;
        let needs_cwd = [
            opcode::RUN,
            opcode::SPAWN,
            opcode::CAPTURE,
            opcode::SUBSTITUTION,
            opcode::FD_OPEN,
        ]
        .contains(&operation.as_slice());
        let count = mask.get().count_ones() as usize;
        if fds.len() != count + usize::from(needs_cwd) {
            return Err(invalid("invalid session descriptor mask"));
        }
        let mut fds = fds.into_iter();
        let mut inherited = Streams::new();
        for n in 0..3 {
            if mask.contains(source(n)?) {
                inherited.insert(source(n)?, fds.next().unwrap());
            }
        }
        let cwd = fds
            .next()
            .map(native::WorkingDirectory::from_owned)
            .transpose()?;
        let mut streams = self.table.merged(&inherited)?;
        let diagnostics = self.diagnostics(&streams)?;
        match (operation.as_slice(), rest) {
            (opcode::SUBSTITUTION_RELEASE, []) => {
                self.endpoints.clear();
                Ok(reply(0, None))
            }
            (opcode::FD_RESET, []) => {
                self.table.reset();
                Ok(reply(0, None))
            }
            (opcode::FD_PUSH, []) => {
                self.table.push()?;
                Ok(reply(0, None))
            }
            (opcode::FD_POP, []) => {
                self.table.pop(1)?;
                Ok(reply(0, None))
            }
            (opcode::FD_POP, [count]) => {
                self.table.pop(
                    usize::try_from(integer(count)?).map_err(|_| invalid("invalid scope pop"))?,
                )?;
                Ok(reply(0, None))
            }
            (opcode::FD_CLOSE, [number]) => {
                self.table.set(source_bytes(number)?, None);
                Ok(reply(0, None))
            }
            (opcode::FD_DATA, [number, value]) => {
                self.table.data(source_bytes(number)?, value)?;
                Ok(reply(0, None))
            }
            (opcode::FD_OPEN, [_, origin, line, number, mode, path]) => {
                let result = self.table.open(
                    source_bytes(number)?,
                    cwd.as_ref().unwrap().borrow(),
                    mode,
                    path,
                    || ensure_active(&mut self.evaluator),
                );
                match result {
                    Ok(()) => Ok(reply(0, None)),
                    Err(e) => self.descriptor_failure(&diagnostics, origin, line, path, e),
                }
            }
            (opcode::FD_DUP, [origin, line, target, original]) => {
                let result = (|| {
                    let number = source_bytes(target)?;
                    let original = streams
                        .get(&source_bytes(original)?)
                        .ok_or_else(|| io::Error::from_raw_os_error(libc::EBADF))?;
                    self.table
                        .set(number, Some(native::duplicate_private(original.as_fd())?));
                    Ok(())
                })();
                match result {
                    Ok(()) => Ok(reply(0, None)),
                    Err(e) => self.descriptor_failure(&diagnostics, origin, line, original, e),
                }
            }
            (opcode::FD_ENDPOINT, [origin, line, target, lease]) => {
                let result = (|| {
                    let target = source_bytes(target)?;
                    let lease = source_bytes(lease)?;
                    let Some(endpoint) = self.endpoints.remove(&lease) else {
                        return Ok(false);
                    };
                    self.table.set(target, Some(endpoint.transfer()));
                    Ok(true)
                })();
                match result {
                    Ok(true) => Ok(reply(0, None)),
                    Ok(false) => Ok(reply(125, None)),
                    Err(e) => self.descriptor_failure(&diagnostics, origin, line, lease, e),
                }
            }
            (
                opcode::READ,
                [
                    origin,
                    line,
                    number,
                    raw,
                    delimiter,
                    count,
                    ifs,
                    mode,
                    names,
                ],
            ) => {
                let count = integer(count)?;
                let names = integer(names)?;
                let destination = match mode.as_slice() {
                    b"reply" => crate::read::Destination::Reply,
                    b"scalar" if names > 0 => crate::read::Destination::Scalars(names as usize),
                    b"array" => crate::read::Destination::Array,
                    _ => return Err(invalid("invalid read destination")),
                };
                if (raw != b"0" && raw != b"1") || count < -1 {
                    return Err(invalid("invalid read flags"));
                }
                let config = crate::read::Config {
                    raw: raw == b"1",
                    delimiter: delimiter.first().copied().unwrap_or(0),
                    limit: if count < 0 {
                        None
                    } else {
                        Some(count as usize)
                    },
                    ifs: ifs.clone(),
                    destination,
                };
                if let Some(fd) = streams.get(&source_bytes(number)?) {
                    match crate::read::descriptor_interruptible(&config, fd.as_fd(), || {
                        self.interrupted()
                    }) {
                        Ok((code, values)) => {
                            let mut result = vec![b"ok".to_vec(), decimal(code), b"1".to_vec()];
                            result.extend(values);
                            Ok(protocol::encode(&result))
                        }
                        Err(error) => {
                            write_stream(
                                &diagnostics,
                                2,
                                &[
                                    origin.as_slice(),
                                    b": line ",
                                    line,
                                    b": read: ",
                                    number,
                                    b": read error: ",
                                    &native::native_error_message(&error),
                                    b"\n",
                                ]
                                .concat(),
                            )?;
                            Ok(protocol::encode(&[
                                b"ok".to_vec(),
                                b"1".to_vec(),
                                b"0".to_vec(),
                            ]))
                        }
                    }
                } else {
                    write_stream(
                        &diagnostics,
                        2,
                        &[
                            origin.as_slice(),
                            b": line ",
                            line,
                            b": read: ",
                            number,
                            b": invalid file descriptor: Bad file descriptor\n",
                        ]
                        .concat(),
                    )?;
                    Ok(protocol::encode(&[
                        b"ok".to_vec(),
                        b"1".to_vec(),
                        b"0".to_vec(),
                    ]))
                }
            }
            (opcode::FINISH_SIGNAL, [signal]) if signal == b"13" => {
                self.finish_signal = Some(13);
                Ok(reply(0, None))
            }
            (opcode::PING, []) => Ok(reply(0, Some(std::process::id() as i32))),
            (opcode::WAIT, [origin, line, args @ ..]) => {
                let code = self.wait_arguments(&diagnostics, origin, line, args)?;
                Ok(reply(code, None))
            }
            (mode, [_, count, payload @ ..])
                if [
                    opcode::RUN,
                    opcode::SPAWN,
                    opcode::CAPTURE,
                    opcode::SUBSTITUTION,
                ]
                .contains(&mode) =>
            {
                let count = usize::try_from(integer(count)?)
                    .map_err(|_| invalid("invalid environment count"))?;
                if count > payload.len() / 2 {
                    return Err(invalid("invalid environment count"));
                }
                let mut env = Vec::new();
                for pair in payload[..count * 2].as_chunks::<2>().0 {
                    if pair[0].is_empty() || pair[0].contains(&b'=') {
                        return Err(invalid("invalid environment name"));
                    }
                    env.push((pair[0].clone(), pair[1].clone()));
                }
                let body = &payload[count * 2..];
                let cwd = cwd.as_ref().unwrap().borrow();
                match body {
                    [direction, kind, operands @ ..]
                        if mode == opcode::SUBSTITUTION
                            && (direction == b"input" || direction == b"output") =>
                    {
                        let (reader, writer) = native::private_pipe()?;
                        let (endpoint, producer, target) = if direction == b"input" {
                            (reader, writer, 1)
                        } else {
                            (writer, reader, 0)
                        };
                        streams.insert(source(target)?, producer);
                        let mut job = jobs::start(
                            true,
                            self.diagnostic.as_ref().map(AsFd::as_fd),
                            &streams,
                            cwd,
                            &env,
                            kind,
                            operands,
                        )?;
                        let number = self
                            .endpoints
                            .keys()
                            .map(|fd| fd.get())
                            .max()
                            .unwrap_or(255)
                            .max(255)
                            + 1;
                        let lease = source(number)?;
                        self.endpoints
                            .insert(lease, crate::types::EndpointLease::new(endpoint));
                        job.substitution = true;
                        let pid = job.pid();
                        self.supersede();
                        self.jobs.insert(pid, job);
                        Ok(protocol::encode(&[
                            b"ok".to_vec(),
                            b"0".to_vec(),
                            decimal(pid),
                            format!("/dev/fd/{number}").into_bytes(),
                            decimal(number),
                        ]))
                    }
                    [warning, kind, operands @ ..]
                        if mode == opcode::CAPTURE && kind == body_opcode::SNAPSHOT =>
                    {
                        self.capture(&streams, cwd, &env, warning, operands)
                    }
                    [kind, operands @ ..]
                        if [
                            body_opcode::EXTERNAL,
                            body_opcode::EXTERNAL_SITE,
                            body_opcode::BODY,
                            body_opcode::SNAPSHOT,
                            body_opcode::BUILTIN,
                            body_opcode::DIRECTORY_OUTPUT,
                            body_opcode::PIPELINE,
                        ]
                        .contains(&kind.as_slice()) =>
                    {
                        for (n, fd) in &self.endpoints {
                            streams.insert(*n, native::duplicate_private(fd.as_fd())?);
                        }
                        let asynchronous = mode == opcode::SPAWN;
                        if asynchronous {
                            let null = rustix::fs::open(
                                "/dev/null",
                                rustix::fs::OFlags::RDONLY | rustix::fs::OFlags::CLOEXEC,
                                rustix::fs::Mode::empty(),
                            )?;
                            streams.insert(source(0)?, native::duplicate_private(null.as_fd())?);
                        }
                        let result = jobs::start(
                            asynchronous,
                            self.diagnostic.as_ref().map(AsFd::as_fd),
                            &streams,
                            cwd,
                            &env,
                            kind,
                            operands,
                        );
                        self.endpoints.clear();
                        let job = result?;
                        let pid = job.pid();
                        if asynchronous {
                            self.supersede();
                        }
                        self.jobs.insert(pid, job);
                        drop(streams);
                        if asynchronous {
                            Ok(reply(0, Some(pid)))
                        } else {
                            let status = self.wait(pid);
                            if status.is_err()
                                && let Some(job) = self.jobs.get_mut(&pid)
                            {
                                job.terminate();
                            }
                            self.jobs.remove(&pid);
                            Ok(reply(status?, Some(pid)))
                        }
                    }
                    _ => Err(invalid("unknown compiled session body kind")),
                }
            }
            _ => Err(invalid("unknown session operation")),
        }
    }
    fn capture(
        &mut self,
        streams: &Streams,
        cwd: native::BorrowedDirectory<'_>,
        env: &native::Environment,
        warning: &[u8],
        operands: &[Bytes],
    ) -> io::Result<Bytes> {
        let (reader, writer) = native::private_pipe()?;
        let mut output = table::copy(streams)?;
        output.insert(source(1)?, writer);
        let job = jobs::start(
            false,
            self.diagnostic.as_ref().map(AsFd::as_fd),
            &output,
            cwd,
            env,
            body_opcode::SNAPSHOT,
            operands,
        )?;
        let pid = job.pid();
        self.jobs.insert(pid, job);
        drop(output);
        let result = (|| {
            let mut value = Vec::new();
            let mut chunk = [0; 65536];
            let mut warned = false;
            loop {
                let n = match rustix::io::read(&reader, &mut chunk) {
                    Ok(n) => n,
                    Err(rustix::io::Errno::INTR) => {
                        self.interrupted()?;
                        continue;
                    }
                    Err(e) => return Err(e.into()),
                };
                if n == 0 {
                    break;
                }
                let bytes = &chunk[..n];
                if bytes.contains(&0) && !warned {
                    write_stream(&self.diagnostics(streams)?, 2, warning)?;
                    warned = true;
                }
                value.extend(bytes.iter().copied().filter(|b| *b != 0));
            }
            while value.last() == Some(&10) {
                value.pop();
            }
            let code = self.wait(pid)?;
            Ok(protocol::encode(&[b"ok".to_vec(), decimal(code), value]))
        })();
        if result.is_err()
            && let Some(job) = self.jobs.get_mut(&pid)
        {
            job.terminate();
        }
        self.jobs.remove(&pid);
        result
    }
}

pub fn dispatch(args: &[Bytes]) -> io::Result<i32> {
    // Adopt the entire inherited set before capsule/diagnostic/socket acquisition.
    let table = table::Table::new()?;
    let (code, signal) = match args {
        [flag, directory, token, rest @ ..] if flag == b"--capsule" => {
            let lease = capsule::acquire(directory, token)?;
            run(table, &lease.script, rest)?
        }
        [flag, directory, script, rest @ ..] if flag == b"--owned" => {
            let result = run(table, script, rest);
            let _ = std::fs::remove_dir_all(capsule::path(directory));
            result?
        }
        [script, rest @ ..] => run(table, script, rest)?,
        _ => return Err(invalid("session-run needs a generated Fish script")),
    };
    if let Some(signal) = signal {
        native::terminate_with(signal);
    }
    Ok(code)
}
fn run(table: table::Table, script: &[u8], arguments: &[Bytes]) -> io::Result<(i32, Option<i32>)> {
    let diagnostic = if native::initial_descriptor_open(2) {
        Some(native::inherited_fd(2)?)
    } else {
        None
    };
    let directory = capsule::workspace("monk-session-")?;
    let socket_path = capsule::bytes(&directory.path().join("control"));
    let listener = transport::listen(&socket_path)?;
    let token = capsule::token()?;
    let response_path = capsule::file(directory.path(), b"")?;
    let closed = (0..3)
        .filter(|n| !native::initial_descriptor_open(*n))
        .collect::<Vec<_>>();
    let script = if closed.is_empty() {
        script.to_vec()
    } else {
        let body = std::fs::read(capsule::path(script))?;
        let closures = closed
            .iter()
            .map(|fd| format!(" {fd}>&-"))
            .collect::<String>();
        capsule::file(
            directory.path(),
            &[
                b"begin\n",
                body.as_slice(),
                b"\nend",
                closures.as_bytes(),
                b"\n",
            ]
            .concat(),
        )?
    };
    let mut env = clean_environment(native::environment());
    env.extend([
        (b"MONK_SESSION_SOCKET".to_vec(), socket_path),
        (b"MONK_SESSION_TOKEN".to_vec(), token.clone()),
        (b"MONK_SESSION_REPLY".to_vec(), response_path),
    ]);
    let mut argv = vec![b"--no-config".to_vec(), script];
    argv.extend_from_slice(arguments);
    let _signals = native::SignalGuard::install()?;
    let evaluator = native::spawn(
        &native::initial_streams()?,
        None,
        &env,
        b"fish",
        &argv,
        false,
    )?;
    let mut owner = Owner {
        table,
        diagnostic,
        endpoints: BTreeMap::new(),
        jobs: BTreeMap::new(),
        evaluator,
        finish_signal: None,
    };
    let _watch = native::EvaluatorWatch::new(owner.evaluator.pid())?;
    let result = (|| loop {
        if let Some(signal) = native::pending_signal() {
            break Ok((128 + signal, Some(signal)));
        }
        if let Some(status) = owner.evaluator.try_wait()? {
            break Ok((status.code(), owner.finish_signal));
        }
        for job in owner.jobs.values_mut() {
            job.poll()?;
        }
        let ready = match poll(
            &mut [PollFd::new(&listener, PollFlags::IN)],
            Some(&Timespec {
                tv_sec: 0,
                tv_nsec: 1_000_000,
            }),
        ) {
            Ok(n) => n,
            Err(rustix::io::Errno::INTR) => continue,
            Err(e) => break Err(e.into()),
        };
        if ready == 0 {
            continue;
        }
        let connection = match transport::accept(listener.as_fd()) {
            Ok(c) => c,
            Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
            Err(e) => break Err(e),
        };
        let response =
            match transport::receive_interruptible(connection.as_fd(), || owner.interrupted()) {
                Ok((fds, bytes)) => owner
                    .request(&token, fds, &bytes)
                    .unwrap_or_else(|_| reply(125, None)),
                Err(_) => reply(125, None),
            };
        let _ = transport::write(connection.as_fd(), &response);
    })();
    owner.endpoints.clear();
    if owner.evaluator.try_wait()?.is_none() {
        let _ = owner.evaluator.terminate();
    }
    result
}
fn publish(private_reply: bool, value: &[u8]) -> io::Result<()> {
    if private_reply {
        std::fs::write(capsule::path(&variable("MONK_SESSION_REPLY")?), value)
    } else {
        native::write_all(native::inherited_fd(1)?.as_fd(), value)
    }
}
pub fn client(private_reply: bool) -> io::Result<i32> {
    let action = (|| {
        if private_reply {
            publish(true, b"")?;
        }
        let socket = variable("MONK_SESSION_SOCKET")?;
        let token = variable("MONK_SESSION_TOKEN")?;
        let input = transport::read_all(native::inherited_fd(0)?.as_fd())?;
        let frames = protocol::decode(&input).map_err(|_| invalid("invalid session request"))?;
        let [operation, mask, operands @ ..] = frames.as_slice() else {
            return Err(invalid("invalid session request"));
        };
        let mask_value = u8::try_from(integer(mask)?)
            .ok()
            .and_then(|n| crate::types::DescriptorMask::new(n).ok())
            .ok_or_else(|| invalid("invalid descriptor mask"))?;
        let mut descriptors = Vec::new();
        for n in 0..3 {
            if mask_value.contains(source(n)?) {
                if !native::initial_descriptor_open(n + 3) {
                    return Err(invalid("missing user stream"));
                }
                descriptors.push(native::inherited_fd(n + 3)?);
            }
        }
        let mut payload = vec![token, operation.clone(), mask.clone()];
        if [opcode::RUN, opcode::SPAWN, opcode::SUBSTITUTION].contains(&operation.as_slice()) {
            let env = clean_environment(native::environment());
            payload.extend([Vec::new(), decimal(env.len())]);
            payload.extend(environment_frames(&env));
        } else if operation == opcode::FD_OPEN {
            payload.push(Vec::new());
        }
        payload.extend_from_slice(operands);
        if [
            opcode::RUN,
            opcode::SPAWN,
            opcode::SUBSTITUTION,
            opcode::FD_OPEN,
        ]
        .contains(&operation.as_slice())
        {
            descriptors.push(native::open_working_directory()?.into_owned());
        }
        transport::request(
            &socket,
            &descriptors.iter().map(AsFd::as_fd).collect::<Vec<_>>(),
            &protocol::encode(&payload),
        )
    })();
    publish(private_reply, &action.unwrap_or_else(|_| reply(125, None)))?;
    Ok(0)
}
pub fn child(capture: bool, request: crate::child::Request) -> io::Result<(i32, Bytes)> {
    let socket = variable("MONK_SESSION_SOCKET")?;
    let token = variable("MONK_SESSION_TOKEN")?;
    let env = clean_environment(native::environment());
    let values = protocol::decode(&request.state).map_err(|_| invalid("invalid child state"))?;
    let mut frames = vec![
        token,
        if capture {
            opcode::CAPTURE.to_vec()
        } else {
            opcode::RUN.to_vec()
        },
        decimal(request.mask.get()),
        Vec::new(),
        decimal(env.len()),
    ];
    frames.extend(environment_frames(&env));
    if capture {
        frames.push(request.warning);
    }
    frames.extend([
        body_opcode::SNAPSHOT.to_vec(),
        request.script,
        request.level,
    ]);
    frames.extend(values);
    let mut descriptors = Vec::new();
    for n in 0..3 {
        if request.mask.contains(source(n)?) {
            descriptors.push(native::inherited_fd(if n == 0 { 3 } else { n })?);
        }
    }
    descriptors.push(native::open_working_directory()?.into_owned());
    let response = transport::request(
        &socket,
        &descriptors.iter().map(AsFd::as_fd).collect::<Vec<_>>(),
        &protocol::encode(&frames),
    )?;
    match protocol::decode(&response)
        .map_err(|_| invalid("invalid child response"))?
        .as_slice()
    {
        [ok, status, value] if ok == b"ok" => Ok((
            integer(status)? as i32,
            if capture { value.clone() } else { Vec::new() },
        )),
        _ => Err(invalid("invalid child session reply")),
    }
}
pub fn directory_diagnostic() -> io::Result<i32> {
    let input = transport::read_all(native::inherited_fd(0)?.as_fd())?;
    let (origin, line, name, message) = crate::directory::diagnostic(&input)
        .map_err(|_| invalid("invalid directory diagnostic"))?;
    if message.is_empty() {
        return Ok(0);
    }
    let socket = variable("MONK_SESSION_SOCKET")?;
    let token = variable("MONK_SESSION_TOKEN")?;
    let env = clean_environment(native::environment());
    let error_open = native::initial_descriptor_open(2);
    let mut frames = vec![
        token,
        opcode::RUN.to_vec(),
        if error_open {
            b"4".to_vec()
        } else {
            b"0".to_vec()
        },
        Vec::new(),
        decimal(env.len()),
    ];
    frames.extend(environment_frames(&env));
    frames.extend([
        body_opcode::DIRECTORY_OUTPUT.to_vec(),
        origin,
        line,
        name,
        b"2".to_vec(),
        message,
    ]);
    let mut descriptors = Vec::new();
    if error_open {
        descriptors.push(native::inherited_fd(2)?);
    }
    descriptors.push(native::open_working_directory()?.into_owned());
    let response = transport::request(
        &socket,
        &descriptors.iter().map(AsFd::as_fd).collect::<Vec<_>>(),
        &protocol::encode(&frames),
    )?;
    match protocol::decode(&response)
        .map_err(|_| invalid("invalid directory session reply"))?
        .as_slice()
    {
        [ok, status, _] if ok == b"ok" => Ok(integer(status)? as i32),
        _ => Err(invalid("invalid directory session response")),
    }
}
pub fn writer(args: &[Bytes]) -> io::Result<i32> {
    let [directory, path, origin, line, name] = args else {
        return Err(invalid("invalid native writer arguments"));
    };
    let result = (|| {
        let file = std::fs::File::open(capsule::path(path))?;
        std::fs::remove_file(capsule::path(path))?;
        std::fs::remove_dir(capsule::path(directory))?;
        let mut chunk = [0; 65536];
        loop {
            let n = rustix::io::read(&file, &mut chunk)?;
            if n == 0 {
                break;
            }
            if !native::initial_descriptor_open(1) {
                return Err(io::Error::from_raw_os_error(libc::EBADF));
            }
            native::write_all(native::inherited_fd(1)?.as_fd(), &chunk[..n])?;
        }
        Ok(())
    })();
    match result {
        Ok(()) => Ok(0),
        Err(error) => {
            if native::initial_descriptor_open(2) {
                let _ = native::write_all(
                    native::inherited_fd(2)?.as_fd(),
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
            }
            Ok(1)
        }
    }
}
pub fn exec_error(args: &[Bytes]) -> io::Result<i32> {
    let [directory, path, code] = args else {
        return Err(invalid("invalid failed executable diagnostic arguments"));
    };
    let code = integer(code)? as i32;
    let file = std::fs::File::open(capsule::path(path))?;
    std::fs::remove_file(capsule::path(path))?;
    std::fs::remove_dir(capsule::path(directory))?;
    if native::initial_descriptor_open(2)
        && let Ok(value) = transport::read_all(file.as_fd())
    {
        let _ = native::write_all(native::inherited_fd(2)?.as_fd(), &value);
    }
    Ok(code)
}
