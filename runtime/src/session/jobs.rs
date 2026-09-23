use super::request::Stage;
use super::*;
pub(super) struct Job {
    pub children: Vec<native::RunningChild>,
    pub pipefail: bool,
    pub substitution: bool,
    pub implicit: bool,
    completion: Option<i32>,
    workspaces: Vec<capsule::WorkspaceLease>,
}
impl Job {
    pub fn pid(&self) -> i32 {
        self.children.last().expect("nonempty job").pid()
    }
    pub fn poll(&mut self) -> io::Result<Option<i32>> {
        if self.completion.is_some() {
            return Ok(self.completion);
        }
        let mut all = true;
        let mut code = 0;
        for child in &mut self.children {
            match child.try_wait()? {
                Some(status) => {
                    if !self.pipefail || status.code() != 0 {
                        code = status.code();
                    }
                }
                None => all = false,
            }
        }
        if all {
            self.completion = Some(code);
            for workspace in self.workspaces.drain(..) {
                workspace.finish()?;
            }
        }
        Ok(self.completion)
    }
    pub fn terminate(&mut self) {
        for child in &mut self.children {
            let _ = child.terminate();
        }
        for workspace in self.workspaces.drain(..) {
            let _ = workspace.finish();
        }
    }
}
pub(super) fn start(
    asynchronous: bool,
    diagnostic: Option<BorrowedFd<'_>>,
    streams: &Streams,
    cwd: native::BorrowedDirectory<'_>,
    env: &native::Environment,
    body: Body<'_>,
    guardian: &capsule::WorkspaceOwner,
) -> io::Result<Job> {
    let (pipeline, pipefail, stages) = match body {
        Body::Single(stage) => (false, false, vec![stage]),
        Body::Pipeline { pipefail, stages } => (true, pipefail, stages),
    };
    let mut job = Job {
        children: Vec::with_capacity(stages.len()),
        pipefail,
        substitution: false,
        implicit: true,
        completion: None,
        workspaces: Vec::new(),
    };
    let count = stages.len();
    let pipes = (0..count - 1)
        .map(|_| native::private_pipe())
        .collect::<io::Result<Vec<_>>>()?;
    let result = (|| {
        for (index, body) in stages.into_iter().enumerate() {
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
                // Even a one-stage pipeline owns its diagnostic descriptor;
                // only a synchronous single body uses the owner fallback.
                if !pipeline && !asynchronous {
                    diagnostic
                } else {
                    stage.get(&source(2)?).map(AsFd::as_fd)
                },
                &stage,
                cwd,
                env,
                body,
                &mut job.workspaces,
                guardian,
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
#[allow(clippy::too_many_arguments)]
fn user(
    asynchronous: bool,
    diagnostic: Option<BorrowedFd<'_>>,
    streams: &Streams,
    cwd: native::BorrowedDirectory<'_>,
    env: &native::Environment,
    body: Stage<'_>,
    workspaces: &mut Vec<capsule::WorkspaceLease>,
    guardian: &capsule::WorkspaceOwner,
) -> io::Result<native::RunningChild> {
    let clean = clean_environment(env.clone());
    match body {
        Stage::External { command, args } => native::spawn(
            streams,
            Some(cwd),
            &clean,
            command,
            &owned(&args),
            asynchronous,
        )
        .or_else(|_| native::spawn_status(127)),
        Stage::ExternalSite {
            origin,
            line,
            command,
            args,
        } => {
            match native::spawn(
                streams,
                Some(cwd),
                &clean,
                command,
                &owned(&args),
                asynchronous,
            ) {
                Ok(child) => Ok(child),
                Err(error) => {
                    let (code, message) =
                        crate::exec::execution_failure(Some(cwd), origin, line, command, &error);
                    let directory = guardian.job("monk-exec-error-")?;
                    let path = capsule::file(directory.path(), &message)?;
                    let directory_path = capsule::bytes(directory.path());
                    let (child, directory) = directory.spawn(
                        streams,
                        cwd,
                        &clean,
                        &[
                            b"--abi".to_vec(),
                            b"2".to_vec(),
                            b"session-exec-error".to_vec(),
                            directory_path,
                            path,
                            decimal(code),
                        ],
                        asynchronous,
                    )?;
                    workspaces.push(directory);
                    Ok(child)
                }
            }
        }
        Stage::Writer {
            origin,
            line,
            name,
            descriptor,
            bytes,
        } => {
            let mut output = table::copy(streams)?;
            if descriptor.get() == 2 {
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
                &bytes,
                workspaces,
                guardian,
            )
        }
        Stage::Region { script, args } => region(
            asynchronous,
            streams,
            cwd,
            &clean,
            script,
            &owned(&args),
            None,
            None,
            workspaces,
            guardian,
        ),
        Stage::Snapshot {
            script,
            level,
            state,
        } => region(
            asynchronous,
            streams,
            cwd,
            &clean,
            script,
            &[],
            Some(level),
            Some(&state),
            workspaces,
            guardian,
        ),
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
    workspaces: &mut Vec<capsule::WorkspaceLease>,
    guardian: &capsule::WorkspaceOwner,
) -> io::Result<native::RunningChild> {
    let directory = guardian.job("monk-writer-")?;
    let path = capsule::file(directory.path(), bytes)?;
    let mut output = table::copy(streams)?;
    if !output.contains_key(&source(2)?)
        && let Some(fd) = diagnostic
    {
        output.insert(source(2)?, native::duplicate_private(fd)?);
    }
    let directory_path = capsule::bytes(directory.path());
    let (child, directory) = directory.spawn(
        &output,
        cwd,
        env,
        &[
            b"--abi".to_vec(),
            b"2".to_vec(),
            b"session-write".to_vec(),
            directory_path,
            path,
            origin.to_vec(),
            line.to_vec(),
            name.to_vec(),
        ],
        asynchronous,
    )?;
    workspaces.push(directory);
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
    level: Option<&[u8]>,
    state: Option<&[&[u8]]>,
    workspaces: &mut Vec<capsule::WorkspaceLease>,
    guardian: &capsule::WorkspaceOwner,
) -> io::Result<native::RunningChild> {
    let directory = guardian.job("monk-region-")?;
    let path = capsule::file(directory.path(), script)?;
    let args = match state {
        Some(values) => vec![capsule::file(
            directory.path(),
            &protocol::encode_borrowed(values),
        )?],
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
        env.insert(0, (b"SHLVL".to_vec(), level.to_vec()));
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
    let (child, directory) = directory.spawn(streams, cwd, &env, &command, asynchronous)?;
    workspaces.push(directory);
    Ok(child)
}

fn owned(frames: &[&[u8]]) -> Vec<Bytes> {
    frames.iter().map(|frame| frame.to_vec()).collect()
}
