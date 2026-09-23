//! One process owns signal policy, all child identities, wait caches and source scopes.
use crate::abi2::opcode::{body as body_opcode, session as opcode};

mod jobs;
mod request;
use request::{Body, Direction, LaunchMode, Operation, Request};
mod client;
mod table;
use crate::{
    capsule,
    native::{self, Streams},
    protocol::{self, Bytes},
    transport,
    types::SourceFd,
};
use capsule::invalid;
pub use client::{child, client, directory_diagnostic, exec_error, writer};
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
            b"MONK_WORKSPACE_LEASE".as_slice(),
            b"MONK_SESSION_SOCKET",
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
    workspaces: capsule::WorkspaceOwner,
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
        args: &[&[u8]],
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
                diagnostic(&[b"`", *arg, b"': not a pid or valid job spec\n"].concat())?;
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
        let request = Request::decode(token, fds, bytes)?;
        let mut streams = self.table.merged(&request.inherited)?;
        let diagnostics = self.diagnostics(&streams)?;
        match request.operation {
            Operation::Release => {
                self.endpoints.clear();
                Ok(reply(0, None))
            }
            Operation::Reset => {
                self.table.reset();
                Ok(reply(0, None))
            }
            Operation::Push => {
                self.table.push()?;
                Ok(reply(0, None))
            }
            Operation::Pop(count) => {
                self.table.pop(count)?;
                Ok(reply(0, None))
            }
            Operation::Close(number) => {
                self.table.set(number, None);
                Ok(reply(0, None))
            }
            Operation::Data(number, value) => {
                self.table.data(number, value)?;
                Ok(reply(0, None))
            }
            Operation::Open {
                cwd,
                origin,
                line,
                number,
                mode,
                path,
            } => {
                match self.table.open(number, cwd.borrow(), mode, path, || {
                    ensure_active(&mut self.evaluator)
                }) {
                    Ok(()) => Ok(reply(0, None)),
                    Err(e) => self.descriptor_failure(&diagnostics, origin, line, path, e),
                }
            }
            Operation::Dup {
                origin,
                line,
                target,
                original,
                spelling,
            } => {
                let result = streams
                    .get(&original)
                    .ok_or_else(|| io::Error::from_raw_os_error(libc::EBADF))
                    .and_then(|fd| native::duplicate_private(fd.as_fd()));
                match result {
                    Ok(fd) => {
                        self.table.set(target, Some(fd));
                        Ok(reply(0, None))
                    }
                    Err(e) => self.descriptor_failure(&diagnostics, origin, line, spelling, e),
                }
            }
            Operation::Endpoint { target, lease } => {
                if let Some(endpoint) = self.endpoints.remove(&lease) {
                    self.table.set(target, Some(endpoint.transfer()));
                    Ok(reply(0, None))
                } else {
                    Ok(reply(125, None))
                }
            }
            Operation::Read {
                origin,
                line,
                number,
                spelling,
                config,
            } => {
                let result = streams.get(&number).map(|fd| {
                    crate::read::descriptor_interruptible(&config, fd.as_fd(), || {
                        self.interrupted()
                    })
                });
                match result {
                    Some(Ok((code, values))) => {
                        let mut result = vec![b"ok".to_vec(), decimal(code), b"1".to_vec()];
                        result.extend(values);
                        Ok(protocol::encode(&result))
                    }
                    failure => {
                        let message = match failure {
                            Some(Err(error)) => [
                                b": read error: ".as_slice(),
                                &native::native_error_message(&error),
                                b"\n",
                            ]
                            .concat(),
                            _ => b": invalid file descriptor: Bad file descriptor\n".to_vec(),
                        };
                        write_stream(
                            &diagnostics,
                            2,
                            &[origin, b": line ", line, b": read: ", spelling, &message].concat(),
                        )?;
                        Ok(protocol::encode_borrowed(&[b"ok", b"1", b"0"]))
                    }
                }
            }
            Operation::FinishSignal => {
                self.finish_signal = Some(13);
                Ok(reply(0, None))
            }
            Operation::Ping => Ok(reply(0, Some(std::process::id() as i32))),
            Operation::Wait { origin, line, args } => {
                let code = self.wait_arguments(&diagnostics, origin, line, &args)?;
                Ok(reply(code, None))
            }
            Operation::Launch {
                cwd,
                environment,
                mode,
                body,
            } => match mode {
                LaunchMode::Substitution(direction) => {
                    let (reader, writer) = native::private_pipe()?;
                    let (endpoint, producer, target) = match direction {
                        Direction::Input => (reader, writer, 1),
                        Direction::Output => (writer, reader, 0),
                    };
                    streams.insert(source(target)?, producer);
                    let mut job = jobs::start(
                        true,
                        self.diagnostic.as_ref().map(AsFd::as_fd),
                        &streams,
                        cwd.borrow(),
                        &environment,
                        body,
                        &self.workspaces,
                    )?;
                    let number = self
                        .endpoints
                        .keys()
                        .map(|fd| fd.get())
                        .max()
                        .unwrap_or(255)
                        .max(255)
                        + 1;
                    self.endpoints
                        .insert(source(number)?, crate::types::EndpointLease::new(endpoint));
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
                LaunchMode::Capture(warning) => {
                    self.capture(&streams, cwd.borrow(), &environment, warning, body)
                }
                mode => {
                    for (n, fd) in &self.endpoints {
                        streams.insert(*n, native::duplicate_private(fd.as_fd())?);
                    }
                    let asynchronous = matches!(mode, LaunchMode::Spawn);
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
                        cwd.borrow(),
                        &environment,
                        body,
                        &self.workspaces,
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
            },
        }
    }
    fn capture(
        &mut self,
        streams: &Streams,
        cwd: native::BorrowedDirectory<'_>,
        env: &native::Environment,
        warning: &[u8],
        body: Body<'_>,
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
            body,
            &self.workspaces,
        )?;
        let pid = job.pid();
        self.jobs.insert(pid, job);
        drop(output);
        let diagnostics = self.diagnostics(streams)?;
        let result = (|| {
            let value = crate::capture::drain(
                reader.as_fd(),
                || self.interrupted(),
                || write_stream(&diagnostics, 2, warning),
            )?;
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

pub(crate) fn inherited_numbers() -> io::Result<Vec<i32>> {
    table::inherited_numbers()
}

pub fn dispatch(args: &[Bytes], inherited: Streams) -> io::Result<i32> {
    let table = table::Table::from_inherited(inherited);
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
    let directory = capsule::WorkspaceOwner::new("monk-session-")?;
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
        workspaces: directory,
        diagnostic,
        endpoints: BTreeMap::new(),
        jobs: BTreeMap::new(),
        evaluator,
        finish_signal: None,
    };
    let _watch = native::EvaluatorWatch::new(owner.evaluator.pid())?;
    rustix::fs::fcntl_setfl(
        &listener,
        rustix::fs::fcntl_getfl(&listener)? | rustix::fs::OFlags::NONBLOCK,
    )?;
    let mut peers: Vec<transport::Peer> = Vec::new();
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
        peers.retain(|peer| !peer.done());
        let mut watched = vec![PollFd::new(&listener, PollFlags::IN)];
        watched.extend(
            peers
                .iter()
                .map(|peer| PollFd::from_borrowed_fd(peer.fd(), peer.events())),
        );
        match poll(
            &mut watched,
            Some(&Timespec {
                tv_sec: 0,
                tv_nsec: 1_000_000,
            }),
        ) {
            Ok(_) => {}
            Err(rustix::io::Errno::INTR) => continue,
            Err(e) => break Err(e.into()),
        }
        let ready = watched
            .iter()
            .map(|fd| !fd.revents().is_empty())
            .collect::<Vec<_>>();
        drop(watched);
        for (peer, ready) in peers.iter_mut().zip(&ready[1..]) {
            if !ready {
                continue;
            }
            match peer.advance() {
                Ok(Some((fds, bytes))) => peer.reply(
                    owner
                        .request(&token, fds, &bytes)
                        .unwrap_or_else(|_| reply(125, None)),
                ),
                Ok(None) => {}
                Err(_) => peer.reject(reply(125, None)),
            }
        }
        if ready[0] {
            match transport::accept(listener.as_fd()).and_then(transport::Peer::new) {
                Ok(peer) => peers.push(peer),
                Err(e)
                    if matches!(
                        e.kind(),
                        io::ErrorKind::WouldBlock | io::ErrorKind::Interrupted
                    ) => {}
                Err(e) => break Err(e),
            }
        }
    })();
    owner.endpoints.clear();
    if owner.evaluator.try_wait()?.is_none() {
        let _ = owner.evaluator.terminate();
    }
    let background = owner.jobs.values_mut().try_fold(false, |live, job| {
        job.poll().map(|status| live || status.is_none())
    })?;
    let workspaces = owner.workspaces;
    drop(owner.jobs);
    if !background {
        workspaces.finish()?;
    }
    result
}
