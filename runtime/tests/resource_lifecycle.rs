//! Native lifecycle regressions run in isolated processes; no process-global
//! signal policy or descriptor limit is changed in the Cargo test process.
#![cfg(not(miri))]
use monk_runtime::{protocol, transport};
use std::{
    fs::{self, File},
    io::{self, Read, Write},
    os::{
        fd::{AsFd, AsRawFd, BorrowedFd},
        unix::{
            ffi::OsStrExt,
            net::UnixStream,
            process::{CommandExt, ExitStatusExt},
        },
    },
    path::{Path, PathBuf},
    process::{Child, Command, ExitStatus, Stdio},
    thread,
    time::{Duration, Instant},
};
const BOUND: Duration = Duration::from_secs(3);
const PROBE: &str = "MONK_RESOURCE_LIFECYCLE_PROBE";
fn runtime() -> String {
    std::env::var("MONK_LIFECYCLE_RUNTIME")
        .unwrap_or_else(|_| env!("CARGO_BIN_EXE_monk-runtime").to_owned())
}
fn wait_until(mut check: impl FnMut() -> bool, label: &str) {
    let end = Instant::now() + BOUND;
    while !check() {
        assert!(Instant::now() < end, "timed out: {label}");
        thread::sleep(Duration::from_millis(5));
    }
}
struct Process(Child);
impl Process {
    fn wait(&mut self) -> ExitStatus {
        let mut result = None;
        wait_until(
            || {
                result = self.0.try_wait().unwrap();
                result.is_some()
            },
            "test subprocess exit",
        );
        result.unwrap()
    }
}
impl Drop for Process {
    fn drop(&mut self) {
        // Every test child starts its own process group. Remove its disposable
        // descendants even if the assertion or owner cleanup failed.
        unsafe {
            libc::kill(-(self.0.id() as i32), libc::SIGKILL);
        }
        let _ = self.0.wait();
    }
}
fn isolated_probe(name: &str) {
    let mut child = Process(
        Command::new(std::env::current_exe().unwrap())
            .args(["--exact", name, "--nocapture", "--test-threads=1"])
            .env(PROBE, name)
            .process_group(0)
            .spawn()
            .unwrap(),
    );
    assert!(child.wait().success(), "isolated probe {name} failed");
}
fn resolve_fish() -> PathBuf {
    let selected = std::env::var_os("MONK_REFERENCE_FISH").unwrap_or_else(|| "fish".into());
    let path = PathBuf::from(&selected);
    if path.is_absolute() {
        return path;
    }
    std::env::split_paths(&std::env::var_os("PATH").expect("PATH for Fish prerequisite"))
        .map(|dir| dir.join(&selected))
        .find(|path| path.is_file())
        .expect("Fish is required: use devenv or MONK_REFERENCE_FISH")
}
fn fish_quote(path: &Path) -> String {
    format!(
        "'{}'",
        path.to_str()
            .unwrap()
            .replace('\\', "\\\\")
            .replace('\'', "\\'")
    )
}
struct Session {
    process: Process,
    directory: tempfile::TempDir,
    socket: PathBuf,
    token: Vec<u8>,
    evaluator: i32,
    path: std::ffi::OsString,
}
impl Session {
    fn start() -> Self {
        let directory = tempfile::tempdir().unwrap();
        let providers = directory.path().join("providers");
        fs::create_dir(&providers).unwrap();
        std::os::unix::fs::symlink(resolve_fish(), providers.join("fish")).unwrap();
        let mut paths = vec![providers];
        paths.extend(std::env::split_paths(&std::env::var_os("PATH").unwrap()));
        let path = std::env::join_paths(paths).unwrap();
        let info = directory.path().join("ready");
        let script = directory.path().join("owner.fish");
        fs::write(&script, format!("printf '%s\\n' $MONK_SESSION_SOCKET $MONK_SESSION_TOKEN $fish_pid > {}\nwhile true; /bin/sleep 1; end\n", fish_quote(&info))).unwrap();
        let process = Process(
            Command::new(runtime())
                .args(["--abi", "2", "session-run"])
                .arg(script)
                .env("PATH", &path)
                .env_remove("MONK_SESSION_FDS")
                .stdin(Stdio::null())
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .process_group(0)
                .spawn()
                .unwrap(),
        );
        let mut fields = Vec::new();
        wait_until(
            || {
                fields = fs::read_to_string(&info)
                    .unwrap_or_default()
                    .lines()
                    .map(str::to_owned)
                    .collect();
                fields.len() == 3
            },
            "Fish evaluator readiness",
        );
        let session = Self {
            process,
            directory,
            socket: PathBuf::from(&fields[0]),
            token: fields[1].as_bytes().to_vec(),
            evaluator: fields[2].parse().unwrap(),
            path,
        };
        let mut ping = session.request(&[], &[b"ping", b"0"]);
        let mut response = Vec::new();
        ping.read_to_end(&mut response).unwrap();
        let values = protocol::decode(&response).unwrap();
        assert_eq!(values[0], b"ok");
        assert_eq!(values[1], b"0");
        assert_eq!(
            values[2],
            session.process.0.id().to_string().as_bytes(),
            "positive session-owner identity control"
        );
        session
    }
    fn connect(&self) -> UnixStream {
        let stream = UnixStream::connect(&self.socket).unwrap();
        stream.set_read_timeout(Some(BOUND)).unwrap();
        stream.set_write_timeout(Some(BOUND)).unwrap();
        stream
    }
    fn request(&self, fds: &[BorrowedFd<'_>], fields: &[&[u8]]) -> UnixStream {
        let mut stream = self.connect();
        transport::send_fds(stream.as_fd(), fds).unwrap();
        let mut values = vec![self.token.clone()];
        values.extend(fields.iter().map(|value| value.to_vec()));
        stream.write_all(&protocol::encode(&values)).unwrap();
        stream.shutdown(std::net::Shutdown::Write).unwrap();
        stream
    }
    fn finish(&mut self, kill_evaluator: bool) {
        let (pid, signal) = if kill_evaluator {
            (self.evaluator, libc::SIGKILL)
        } else {
            (self.process.0.id() as i32, libc::SIGTERM)
        };
        assert_eq!(unsafe { libc::kill(pid, signal) }, 0);
        let status = self.process.wait();
        if kill_evaluator {
            assert_eq!(status.code(), Some(137));
        } else {
            assert_eq!(status.signal(), Some(libc::SIGTERM));
        }
        assert!(
            !self.socket.parent().unwrap().exists(),
            "owner workspace must be released"
        );
    }
}
fn assert_blocked(stream: &mut UnixStream) {
    stream
        .set_read_timeout(Some(Duration::from_millis(70)))
        .unwrap();
    let error = stream
        .read(&mut [0])
        .expect_err("request must actually block before cancellation");
    assert!(
        matches!(
            error.kind(),
            io::ErrorKind::WouldBlock | io::ErrorKind::TimedOut
        ),
        "unexpected blocking probe error: {error}"
    );
}
#[test]
fn owner_cancels_fifo_open_and_pipe_read() {
    for kill_evaluator in [false, true] {
        let mut session = Session::start();
        let fifo = session.directory.path().join("fifo");
        nix::unistd::mkfifo(
            &fifo,
            nix::sys::stat::Mode::S_IRUSR | nix::sys::stat::Mode::S_IWUSR,
        )
        .unwrap();
        let cwd = File::open(session.directory.path()).unwrap();
        let mut request = session.request(
            &[cwd.as_fd()],
            &[
                b"fd-open",
                b"0",
                b"",
                b"source",
                b"1",
                b"3",
                b"read",
                fifo.as_os_str().as_bytes(),
            ],
        );
        assert_blocked(&mut request);
        session.finish(kill_evaluator);
        let mut session = Session::start();
        let (reader, _writer) = rustix::pipe::pipe().unwrap();
        let mut request = session.request(
            &[reader.as_fd()],
            &[
                b"read", b"1", b"source", b"1", b"0", b"1", b"\n", b"-1", b"", b"scalar", b"1",
            ],
        );
        assert_blocked(&mut request);
        session.finish(kill_evaluator);
    }
}
#[test]
fn owner_cancels_capture_and_incomplete_requests() {
    for kill_evaluator in [false, true] {
        let mut session = Session::start();
        let marker = session.directory.path().join("capture-started");
        let script = format!(
            "printf started > {}\nwhile true; /bin/sleep 1; end\n",
            fish_quote(&marker)
        );
        let cwd = File::open(session.directory.path()).unwrap();
        let mut request = session.request(
            &[cwd.as_fd()],
            &[
                b"capture",
                b"0",
                b"",
                b"1",
                b"PATH",
                session.path.as_bytes(),
                b"warning",
                b"snapshot",
                script.as_bytes(),
                b"0",
            ],
        );
        wait_until(
            || fs::read(&marker).ok().as_deref() == Some(b"started"),
            "captured evaluator actually started",
        );
        assert_blocked(&mut request);
        session.finish(kill_evaluator);
        for send_marker in [false, true] {
            let mut session = Session::start();
            let mut request = session.connect();
            if send_marker {
                request.write_all(b"Mpartial-authentication").unwrap();
            }
            assert_blocked(&mut request);
            session.finish(kill_evaluator);
        }
    }
}
#[test]
fn inherited_manifest_rejects_closed_slots_and_accepts_duplicate_names() {
    let scratch = tempfile::tempdir().unwrap();
    let marker = scratch.path().join("evaluated");
    let script = scratch.path().join("check.fish");
    fs::write(
        &script,
        format!("printf evaluated > {}\nexit 23\n", fish_quote(&marker)),
    )
    .unwrap();
    let providers = scratch.path().join("providers");
    fs::create_dir(&providers).unwrap();
    std::os::unix::fs::symlink(resolve_fish(), providers.join("fish")).unwrap();
    let paths = std::env::join_paths(
        std::iter::once(providers).chain(std::env::split_paths(&std::env::var_os("PATH").unwrap())),
    )
    .unwrap();
    let source = File::open("/dev/null").unwrap();
    let inherited = rustix::io::fcntl_dupfd_cloexec(source.as_fd(), 64).unwrap();
    let raw = inherited.as_raw_fd();
    for (manifest, expected) in [("10", 125), ("3,10", 125), ("3,3", 23)] {
        let _ = fs::remove_file(&marker);
        let mut command = Command::new(runtime());
        command
            .args(["--abi", "2", "session-run"])
            .arg(&script)
            .env("MONK_SESSION_FDS", manifest)
            .env("PATH", &paths)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .process_group(0);
        // Only direct syscalls after fork. The source capability is above every
        // slot manipulated here, and remains owned by the parent until exec.
        unsafe {
            command.pre_exec(move || {
                if libc::dup2(raw, 3) < 0 {
                    return Err(io::Error::last_os_error());
                }
                libc::close(10);
                Ok(())
            });
        }
        let mut child = Process(command.spawn().unwrap());
        assert_eq!(child.wait().code(), Some(expected), "manifest={manifest}");
        assert_eq!(
            marker.exists(),
            expected == 23,
            "invalid manifest must fail before evaluator/private allocations"
        );
    }
}
#[test]
fn received_descriptors_roll_back_after_partial_duplication_failure() {
    let name = "received_descriptors_roll_back_after_partial_duplication_failure";
    if std::env::var(PROBE).ok().as_deref() != Some(name) {
        isolated_probe(name);
        return;
    }
    let (left, right) = UnixStream::pair().unwrap();
    let pairs = (0..2)
        .map(|_| rustix::pipe::pipe().unwrap())
        .collect::<Vec<_>>();
    let writers = pairs
        .iter()
        .map(|(_, writer)| writer.as_fd())
        .collect::<Vec<_>>();
    transport::send_fds(left.as_fd(), &writers).unwrap();
    drop(writers);
    let mut filled: Vec<File> = Vec::new();
    loop {
        let file = File::open("/dev/null").unwrap();
        let number = file.as_raw_fd();
        filled.push(file);
        if number >= 63 {
            assert_eq!(number, 63);
            break;
        }
    }
    let mut original = unsafe { std::mem::zeroed::<libc::rlimit>() };
    unsafe {
        assert_eq!(libc::getrlimit(libc::RLIMIT_NOFILE, &mut original), 0);
    }
    let limited = libc::rlimit {
        rlim_cur: 64,
        rlim_max: original.rlim_max,
    };
    unsafe {
        assert_eq!(libc::setrlimit(libc::RLIMIT_NOFILE, &limited), 0);
    }
    let holes = (0..3)
        .map(|_| {
            let file = filled.pop().unwrap();
            let fd = file.as_raw_fd();
            drop(file);
            fd
        })
        .collect::<Vec<_>>();
    let result = transport::receive_fds(right.as_fd());
    let all_closed = holes
        .iter()
        .all(|fd| unsafe { libc::fcntl(*fd, libc::F_GETFD) } < 0);
    unsafe {
        assert_eq!(libc::setrlimit(libc::RLIMIT_NOFILE, &original), 0);
    }
    assert_eq!(result.unwrap_err().raw_os_error(), Some(libc::EMFILE));
    assert!(
        all_closed,
        "received aliases and the first successful duplicate must all roll back"
    );
    for (reader, writer) in pairs {
        drop(writer);
        assert_eq!(
            rustix::io::read(&reader, &mut [0]).unwrap(),
            0,
            "received writer leaked"
        );
    }
}

#[test]
fn incomplete_peers_do_not_delay_authenticated_requests() {
    let mut session = Session::start();
    let _without_marker = session.connect();
    let mut incomplete = session.connect();
    transport::send_fds(incomplete.as_fd(), &[]).unwrap();
    incomplete.write_all(b"unfinished").unwrap();
    let mut ping = session.request(&[], &[b"ping", b"0"]);
    ping.set_read_timeout(Some(Duration::from_millis(300)))
        .unwrap();
    let mut response = Vec::new();
    ping.read_to_end(&mut response)
        .expect("incomplete peers monopolized owner");
    assert_eq!(protocol::decode(&response).unwrap()[1], b"0");
    session.finish(false);
}

#[test]
fn invalid_later_pipeline_stage_cannot_launch_an_earlier_stage() {
    let mut session = Session::start();
    let marker = session.directory.path().join("launched");
    let cwd = File::open(session.directory.path()).unwrap();
    let command = format!("touch '{}'; sleep 0.1", marker.display());
    let mut frames: Vec<&[u8]> = vec![
        b"run",
        b"0",
        b"",
        b"0",
        b"pipeline",
        b"0",
        b"34",
        b"external",
        b"3",
        b"/bin/sh",
        b"-c",
        command.as_bytes(),
    ];
    for _ in 0..32 {
        frames.extend([b"external".as_slice(), b"1", b"/bin/true"]);
    }
    frames.extend([b"builtin".as_slice(), b"3", b"source", b"1", b"unsupported"]);
    let mut response = session.request(&[cwd.as_fd()], &frames);
    let mut bytes = Vec::new();
    response.read_to_end(&mut bytes).unwrap();
    assert_eq!(protocol::decode(&bytes).unwrap()[1], b"125");
    thread::sleep(Duration::from_millis(150));
    assert!(
        !marker.exists(),
        "unvalidated pipeline already caused effects"
    );
    session.finish(false);
}

#[test]
fn job_workspace_survives_owner_exit_and_is_reclaimed_after_job_kill() {
    let mut session = Session::start();
    let ready = session.directory.path().join("job-ready");
    let cwd = File::open(session.directory.path()).unwrap();
    let script = format!(
        "printf '%s\\n' (status filename) > {}; while true; /bin/sleep 1; end",
        fish_quote(&ready)
    );
    let null = File::options()
        .read(true)
        .write(true)
        .open("/dev/null")
        .unwrap();
    let mut response = session.request(
        &[null.as_fd(), null.as_fd(), null.as_fd(), cwd.as_fd()],
        &[
            b"spawn",
            b"7",
            b"",
            b"1",
            b"PATH",
            session.path.as_bytes(),
            b"body",
            script.as_bytes(),
        ],
    );
    let mut bytes = Vec::new();
    response.read_to_end(&mut bytes).unwrap();
    let frames = protocol::decode(&bytes).unwrap();
    assert_eq!(frames[1], b"0");
    let pid = std::str::from_utf8(&frames[2])
        .unwrap()
        .parse::<i32>()
        .unwrap();
    wait_until(
        || {
            ready.exists()
                && fs::read_to_string(&ready)
                    .unwrap_or_default()
                    .contains("monk-region-")
        },
        "background body readiness",
    );
    let path = PathBuf::from(fs::read_to_string(&ready).unwrap().trim());
    let workspace = path.parent().unwrap().to_owned();
    assert_eq!(
        unsafe { libc::kill(session.process.0.id() as i32, libc::SIGTERM) },
        0
    );
    assert_eq!(session.process.wait().signal(), Some(libc::SIGTERM));
    assert!(
        path.exists(),
        "owner exit removed a live background workspace"
    );
    assert_eq!(unsafe { libc::kill(pid, libc::SIGKILL) }, 0);
    wait_until(|| !workspace.exists(), "guardian cleanup after job SIGKILL");
}

#[test]
fn owner_workspace_is_reclaimed_after_sigkill() {
    let mut session = Session::start();
    let workspace = session.socket.parent().unwrap().to_owned();
    assert_eq!(
        unsafe { libc::kill(session.process.0.id() as i32, libc::SIGKILL) },
        0
    );
    assert_eq!(session.process.wait().signal(), Some(libc::SIGKILL));
    wait_until(
        || !workspace.exists(),
        "guardian cleanup after owner SIGKILL",
    );
}

#[test]
fn unread_large_reply_does_not_delay_other_clients() {
    let mut session = Session::start();
    let cwd = File::open(session.directory.path()).unwrap();
    let mut large = session.request(
        &[cwd.as_fd()],
        &[
            b"capture",
            b"0",
            b"",
            b"1",
            b"PATH",
            session.path.as_bytes(),
            b"",
            b"snapshot",
            b"string repeat --no-newline -n 4000000 x",
            b"1",
        ],
    );
    large
        .read_exact(&mut [0])
        .expect("large reply reached transmission");
    let mut ping = session.request(&[], &[b"ping", b"0"]);
    ping.set_read_timeout(Some(Duration::from_millis(300)))
        .unwrap();
    let mut response = Vec::new();
    ping.read_to_end(&mut response)
        .expect("unread reply monopolized owner");
    assert_eq!(protocol::decode(&response).unwrap()[1], b"0");
    drop(large);
    session.finish(false);
}

#[test]
fn completed_background_jobs_remain_explicitly_waitable_with_cached_status() {
    let mut session = Session::start();
    let cwd = File::open(session.directory.path()).unwrap();
    let mut request = session.request(
        &[cwd.as_fd()],
        &[
            b"spawn",
            b"0",
            b"",
            b"0",
            b"external",
            b"/bin/sh",
            b"-c",
            b"exit 7",
        ],
    );
    let mut response = Vec::new();
    request.read_to_end(&mut response).unwrap();
    let fields = protocol::decode(&response).unwrap();
    assert_eq!(fields[1], b"0");
    for _ in 0..2 {
        let mut request = session.request(&[], &[b"wait", b"0", b"src", b"1", &fields[2]]);
        let mut response = Vec::new();
        request.read_to_end(&mut response).unwrap();
        assert_eq!(protocol::decode(&response).unwrap()[1], b"7");
    }
    session.finish(false);
}

#[test]
fn continuously_ready_capture_remains_cancellable() {
    let scratch = tempfile::tempdir().unwrap();
    let ready = scratch.path().join("capture-ready");
    let script = format!("printf ready > {}; exec /usr/bin/yes x", fish_quote(&ready));
    let mut child = Process(
        Command::new(runtime())
            .args(["--abi", "2", "child-capture"])
            .stdin(Stdio::piped())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .process_group(0)
            .spawn()
            .unwrap(),
    );
    child
        .0
        .stdin
        .take()
        .unwrap()
        .write_all(&protocol::encode(&[
            b"warning".to_vec(),
            b"6".to_vec(),
            script.into_bytes(),
            b"1".to_vec(),
        ]))
        .unwrap();
    wait_until(|| ready.exists(), "capture producer readiness");
    // Let the native producer keep the pipe readable while the consumer filters
    // bytes. Cancellation must not rely on a subsequent read returning EINTR.
    thread::sleep(Duration::from_millis(50));
    assert_eq!(unsafe { libc::kill(child.0.id() as i32, libc::SIGTERM) }, 0);
    assert_eq!(child.wait().code(), Some(128 + libc::SIGTERM));
}

#[test]
fn workspace_lease_and_user_manifest_are_validated_before_adoption() {
    let scratch = tempfile::tempdir().unwrap();
    let marker = scratch.path().join("evaluated");
    let script = scratch.path().join("check.fish");
    fs::write(
        &script,
        format!("printf evaluated > {}; exit 23", fish_quote(&marker)),
    )
    .unwrap();
    let source = File::open("/dev/null").unwrap();
    let inherited = rustix::io::fcntl_dupfd_cloexec(source.as_fd(), 64).unwrap();
    let raw = inherited.as_raw_fd();
    for (manifest, lease_open, expected) in [
        ("11", true, 125),
        ("10", true, 125),
        ("3", false, 125),
        ("3", true, 23),
        ("3,3", true, 23),
    ] {
        let _ = fs::remove_file(&marker);
        let mut command = Command::new(runtime());
        command
            .args(["--abi", "2", "session-run"])
            .arg(&script)
            .env("MONK_SESSION_FDS", manifest)
            .env("MONK_WORKSPACE_LEASE", "10")
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .process_group(0);
        unsafe {
            command.pre_exec(move || {
                if libc::dup2(raw, 3) < 0 {
                    return Err(io::Error::last_os_error());
                }
                libc::close(10);
                libc::close(11);
                if lease_open && libc::dup2(raw, 10) < 0 {
                    return Err(io::Error::last_os_error());
                }
                Ok(())
            });
        }
        let mut child = Process(command.spawn().unwrap());
        assert_eq!(
            child.wait().code(),
            Some(expected),
            "manifest={manifest}, lease_open={lease_open}"
        );
        assert_eq!(
            marker.exists(),
            expected == 23,
            "invalid private/user inheritance reached evaluator"
        );
    }
}

#[test]
fn guardian_cannot_adopt_a_relocated_private_lease() {
    let scratch = tempfile::tempdir().unwrap();
    let root = scratch.path().join("owned");
    fs::create_dir(&root).unwrap();
    let marker = root.join("untouched");
    fs::write(&marker, "must remain").unwrap();
    let source = File::open("/dev/null").unwrap();
    let inherited = rustix::io::fcntl_dupfd_cloexec(source.as_fd(), 64).unwrap();
    let raw = inherited.as_raw_fd();
    let mut command = Command::new(runtime());
    command
        .args(["--abi", "2", "session-guardian", "--workspace"])
        .arg(&root)
        .arg("11")
        .env("MONK_WORKSPACE_LEASE", "10")
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .process_group(0);
    unsafe {
        command.pre_exec(move || {
            libc::close(11);
            if libc::dup2(raw, 10) < 0 {
                return Err(io::Error::last_os_error());
            }
            Ok(())
        });
    }
    let mut child = Process(command.spawn().unwrap());
    assert_eq!(child.wait().code(), Some(125));
    assert!(
        marker.exists(),
        "invalid guardian descriptor reached workspace cleanup"
    );
}

#[test]
fn capsule_takeover_bypasses_an_incomplete_first_peer() {
    let mut prepare = Process(
        Command::new(runtime())
            .args(["--abi", "2", "session-prepare"])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .process_group(0)
            .spawn()
            .unwrap(),
    );
    prepare
        .0
        .stdin
        .take()
        .unwrap()
        .write_all(b"exit 0\n\0")
        .unwrap();
    assert!(prepare.wait().success());
    let mut reply = Vec::new();
    prepare
        .0
        .stdout
        .take()
        .unwrap()
        .read_to_end(&mut reply)
        .unwrap();
    let fields = protocol::decode(&reply).unwrap();
    let root = PathBuf::from(std::ffi::OsStr::from_bytes(&fields[2]));
    let stalled = UnixStream::connect(root.join("lease")).unwrap();
    thread::sleep(Duration::from_millis(25));
    let mut ready = UnixStream::connect(root.join("lease")).unwrap();
    ready
        .set_read_timeout(Some(Duration::from_millis(300)))
        .unwrap();
    transport::send_fds(ready.as_fd(), &[]).unwrap();
    ready
        .write_all(&protocol::encode(&[fields[3].clone()]))
        .unwrap();
    ready.shutdown(std::net::Shutdown::Write).unwrap();
    let result = (|| -> io::Result<Vec<u8>> {
        let leases = transport::receive_fds(ready.as_fd())?;
        let mut response = Vec::new();
        ready.read_to_end(&mut response)?;
        assert_eq!(leases.len(), 1);
        Ok(response)
    })();
    // Close both peers even on the original timeout failure; the selected
    // takeover and its transferred lease must not survive this test.
    drop(stalled);
    drop(ready);
    wait_until(
        || !root.exists(),
        "capsule cleanup after takeover peers close",
    );
    let response = result.expect("incomplete first peer blocked authenticated takeover");
    assert_eq!(protocol::decode(&response).unwrap()[0], b"ok");
}
