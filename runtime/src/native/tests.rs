use super::*;
#[test]
fn explicit_path_candidates_and_empty_components() {
    let env = vec![(b"PATH".to_vec(), b":/bin:".to_vec())];
    assert_eq!(
        candidate_paths(&env, b"tool"),
        vec![b"tool".to_vec(), b"/bin/tool".to_vec(), b"tool".to_vec()]
    );
    assert_eq!(candidate_paths(&env, b"./tool"), vec![b"./tool".to_vec()]);
}
#[test]
fn duplicate_is_owned_private_and_cloexec() {
    let file = std::fs::File::open("/dev/null").unwrap();
    let dup = duplicate_private(file.as_fd()).unwrap();
    assert!(dup.as_raw_fd() >= 10);
    assert_ne!(
        unsafe { libc::fcntl(dup.as_raw_fd(), libc::F_GETFD) } & libc::FD_CLOEXEC,
        0
    );
}
#[test]
fn process_exit_and_missing_executable() {
    let mut child = spawn(
        &Streams::new(),
        None,
        &[],
        b"/bin/sh",
        &[b"-c".to_vec(), b"exit 37".to_vec()],
        false,
    )
    .unwrap();
    assert_eq!(child.wait().unwrap(), ProcessOutcome::Exited(37));
    assert_eq!(
        spawn(
            &Streams::new(),
            None,
            &[],
            b"/monk/does-not-exist",
            &[],
            false
        )
        .unwrap_err()
        .raw_os_error(),
        Some(libc::ENOENT)
    );
}
#[test]
fn no_enoexec_shell_fallback() {
    use std::os::unix::fs::PermissionsExt;
    let file = std::env::temp_dir().join(format!("monk-native-no-fallback-{}", std::process::id()));
    std::fs::write(&file, b"exit 0\n").unwrap();
    std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o700)).unwrap();
    use std::os::unix::ffi::OsStrExt;
    let result = spawn(
        &Streams::new(),
        None,
        &[],
        file.as_os_str().as_bytes(),
        &[],
        false,
    );
    std::fs::remove_file(file).unwrap();
    assert_eq!(result.unwrap_err().raw_os_error(), Some(libc::ENOEXEC));
}
#[test]
fn cwd_identity_survives_rename() {
    let base = std::env::temp_dir().join(format!("monk-native-cwd-{}", std::process::id()));
    let renamed = base.with_extension("renamed");
    std::fs::create_dir(&base).unwrap();
    std::fs::write(base.join("marker"), b"identity").unwrap();
    let directory = std::fs::File::open(&base).unwrap();
    std::fs::rename(&base, &renamed).unwrap();
    let (read, write) = private_pipe().unwrap();
    let mut streams = Streams::new();
    streams.insert(SourceFd::new(1).unwrap(), write);
    let mut child = spawn(
        &streams,
        Some(BorrowedDirectory::from_borrowed(directory.as_fd()).unwrap()),
        &[],
        b"/bin/cat",
        &[b"marker".to_vec()],
        false,
    )
    .unwrap();
    drop(streams);
    assert_eq!(crate::child::read_all(read.as_fd()).unwrap(), b"identity");
    assert_eq!(child.wait().unwrap().code(), 0);
    std::fs::remove_dir_all(renamed).unwrap();
}
#[test]
fn async_spawn_keeps_int_quit_ignored() {
    let (read, write) = private_pipe().unwrap();
    let mut streams = Streams::new();
    streams.insert(SourceFd::new(1).unwrap(), write);
    let mut child = spawn(
        &streams,
        None,
        &[],
        b"/bin/sh",
        &[
            b"-c".to_vec(),
            b"kill -INT $$; kill -QUIT $$; printf survived".to_vec(),
        ],
        true,
    )
    .unwrap();
    drop(streams);
    assert_eq!(crate::child::read_all(read.as_fd()).unwrap(), b"survived");
    assert_eq!(child.wait().unwrap().code(), 0);
}
#[test]
fn wait_preserves_signal_identity() {
    let mut child = spawn(
        &Streams::new(),
        None,
        &[],
        b"/bin/sh",
        &[b"-c".to_vec(), b"kill -TERM $$".to_vec()],
        false,
    )
    .unwrap();
    assert_eq!(
        child.wait().unwrap(),
        ProcessOutcome::Signaled(libc::SIGTERM)
    );
}
#[test]
fn path_access_error_precedes_absence() {
    use std::os::unix::ffi::OsStrExt;
    let dir = std::env::temp_dir().join(format!("monk-native-path-{}", std::process::id()));
    std::fs::create_dir(&dir).unwrap();
    std::fs::write(dir.join("denied"), b"no execution").unwrap();
    let env = vec![(
        b"PATH".to_vec(),
        [dir.as_os_str().as_bytes(), b":/nonexistent"].concat(),
    )];
    let error = spawn(&Streams::new(), None, &env, b"denied", &[], false).unwrap_err();
    std::fs::remove_dir_all(dir).unwrap();
    assert_eq!(error.raw_os_error(), Some(libc::EACCES));
}
/// Executed only in a new process, before the test harness can repair stdio.
#[test]
#[ignore]
fn pre_main_probe() {
    assert!(initial_descriptor_open(0));
    assert!(!initial_descriptor_open(1));
    assert!(!initial_descriptor_open(2));
    assert!(initial_descriptor_open(3));
    assert!(!initial_descriptor_open(4));
    assert!(initial_descriptor_open(5));
    assert!(initial_signal_ignored(libc::SIGINT));
    assert!(!initial_signal_ignored(libc::SIGQUIT));
    for fd in [1, 2] {
        let flags = unsafe { libc::fcntl(fd, libc::F_GETFD) };
        assert!(flags >= 0);
        assert_ne!(flags & libc::FD_CLOEXEC, 0);
    }
    let before = unsafe { libc::fcntl(1, libc::F_GETFD) };
    let error = exec_process(&[], b"/monk/does-not-exist", &[]);
    assert_eq!(error.raw_os_error(), Some(libc::ENOENT));
    assert_eq!(unsafe { libc::fcntl(1, libc::F_GETFD) }, before);
    let mut disposition = std::mem::MaybeUninit::<libc::sigaction>::uninit();
    assert_eq!(
        unsafe { libc::sigaction(libc::SIGINT, std::ptr::null(), disposition.as_mut_ptr()) },
        0
    );
    assert_eq!(
        unsafe { disposition.assume_init() }.sa_sigaction,
        libc::SIG_IGN
    );
}
#[test]
fn pre_main_snapshot_reservations_and_failed_exec_rollback() {
    use std::os::unix::process::CommandExt;
    let mut command = std::process::Command::new(std::env::current_exe().unwrap());
    command.args(["--exact", "native::tests::pre_main_probe", "--ignored"]);
    // This closure runs after fork: only direct, async-signal-safe libc calls.
    unsafe {
        command.pre_exec(|| {
            libc::dup2(0, 3);
            libc::dup2(0, 5);
            libc::close(1);
            libc::close(2);
            libc::close(4);
            libc::signal(libc::SIGINT, libc::SIG_IGN);
            libc::signal(libc::SIGQUIT, libc::SIG_DFL);
            Ok(())
        });
    }
    assert!(command.status().unwrap().success());
}
#[test]
fn native_pipe_paths_alias_the_same_pipe() {
    assert!(probe_pipe_paths().unwrap());
}
#[test]
#[ignore]
fn owner_heartbeat_probe() {
    let mut blocked = unsafe { std::mem::zeroed::<libc::sigset_t>() };
    unsafe {
        libc::sigemptyset(&mut blocked);
        libc::sigaddset(&mut blocked, libc::SIGCHLD);
        assert_eq!(
            libc::pthread_sigmask(libc::SIG_BLOCK, &blocked, std::ptr::null_mut()),
            0
        );
    }
    {
        let _signals = SignalGuard::install().unwrap();
        let mut evaluator = spawn(
            &Streams::new(),
            None,
            &[],
            b"/bin/sh",
            &[b"-c".to_vec(), b"sleep 0.1".to_vec()],
            false,
        )
        .unwrap();
        let _watch = EvaluatorWatch::new(evaluator.pid()).unwrap();
        let (_reader, writer) = private_pipe().unwrap();
        let start = std::time::Instant::now();
        assert_eq!(
            write_all(writer.as_fd(), &vec![b'x'; 1 << 20])
                .unwrap_err()
                .kind(),
            std::io::ErrorKind::Interrupted
        );
        assert!(start.elapsed() < std::time::Duration::from_secs(2));
        evaluator.wait().unwrap();
        assert!(owner_cancelled());
        // Evaluator's actual SIGCHLD has already been delivered and reaped.
        // The subsequent empty read still wakes through the periodic heartbeat.
        let (empty, _held_writer) = private_pipe().unwrap();
        let mut byte = [0u8; 1];
        assert_eq!(
            rustix::io::read(&empty, &mut byte).unwrap_err(),
            rustix::io::Errno::INTR
        );
    }
    let mut mask = unsafe { std::mem::zeroed::<libc::sigset_t>() };
    unsafe {
        assert_eq!(
            libc::pthread_sigmask(libc::SIG_SETMASK, std::ptr::null(), &mut mask),
            0
        );
        assert_eq!(libc::sigismember(&mask, libc::SIGCHLD), 1);
    }
}
#[test]
fn owner_heartbeat_cancels_blocked_write_and_preexisting_read_race() {
    let status = std::process::Command::new(std::env::current_exe().unwrap())
        .args([
            "--exact",
            "native::tests::owner_heartbeat_probe",
            "--ignored",
        ])
        .status()
        .unwrap();
    assert!(status.success());
}
#[test]
fn raw_wait_status_preserves_realtime_signal_numbers() {
    // These raw kernel statuses are Linux real-time terminations, even when
    // this pure decoder regression runs on Darwin. No nix Signal enum narrows them.
    for signal in [34, 64] {
        assert_eq!(
            decode_wait_status(signal),
            Some(ProcessOutcome::Signaled(signal))
        );
    }
}
#[cfg(target_os = "linux")]
#[test]
fn linux_child_realtime_signal_is_preserved() {
    let signal = libc::SIGRTMIN();
    let mut child = spawn(
        &Streams::new(),
        None,
        &[],
        b"/bin/sh",
        &[b"-c".to_vec(), format!("kill -{signal} $$").into_bytes()],
        false,
    )
    .unwrap();
    assert_eq!(child.wait().unwrap(), ProcessOutcome::Signaled(signal));
}

#[test]
fn directory_capabilities_reject_non_directory_descriptors() {
    let (reader, _writer) = private_pipe().unwrap();
    assert_eq!(
        WorkingDirectory::from_owned(reader)
            .unwrap_err()
            .raw_os_error(),
        Some(libc::ENOTDIR)
    );
    let file = std::fs::File::open("/dev/null").unwrap();
    assert_eq!(
        BorrowedDirectory::from_borrowed(file.as_fd())
            .unwrap_err()
            .raw_os_error(),
        Some(libc::ENOTDIR)
    );
    let cwd = open_working_directory().unwrap();
    assert!(rustix::fs::fstat(cwd.borrow()).is_ok());
}

#[test]
fn heartbeat_wakes_after_child_signal_precedes_blocking_read() {
    use std::os::unix::process::CommandExt;
    use std::{
        thread,
        time::{Duration, Instant},
    };
    let name = "native::tests::heartbeat_wakes_after_child_signal_precedes_blocking_read";
    const PROBE: &str = "MONK_RESOURCE_LIFECYCLE_PROBE";
    if std::env::var(PROBE).ok().as_deref() != Some(name) {
        let mut command = std::process::Command::new(std::env::current_exe().unwrap());
        command
            .args(["--exact", name, "--nocapture", "--test-threads=1"])
            .env(PROBE, name)
            .process_group(0);
        let mut child = command.spawn().unwrap();
        let deadline = Instant::now() + Duration::from_secs(3);
        loop {
            if let Some(status) = child.try_wait().unwrap() {
                assert!(status.success(), "isolated heartbeat probe failed");
                break;
            }
            if Instant::now() >= deadline {
                unsafe {
                    libc::kill(-(child.id() as i32), libc::SIGKILL);
                }
                let _ = child.wait();
                panic!("isolated heartbeat probe timed out");
            }
            thread::sleep(Duration::from_millis(5));
        }
        return;
    }
    let mut old = unsafe { std::mem::zeroed::<libc::sigset_t>() };
    let mut blocked = unsafe { std::mem::zeroed::<libc::sigset_t>() };
    unsafe {
        libc::sigemptyset(&mut blocked);
        libc::sigaddset(&mut blocked, libc::SIGCHLD);
        assert_eq!(
            libc::pthread_sigmask(libc::SIG_BLOCK, &blocked, &mut old),
            0
        );
    }
    {
        let _guard = super::SignalGuard::install().unwrap();
        let mut child = super::spawn_status(29).unwrap();
        let _watch = super::EvaluatorWatch::new(child.pid()).unwrap();
        let deadline = Instant::now() + Duration::from_secs(3);
        while !super::owner_cancelled() {
            assert!(Instant::now() < deadline, "child must exit before read");
            thread::sleep(Duration::from_millis(5));
        }
        thread::sleep(Duration::from_millis(30));
        let (reader, _writer) = rustix::pipe::pipe().unwrap();
        let config = crate::read::Config {
            raw: true,
            delimiter: b'\n',
            limit: None,
            ifs: vec![],
            destination: crate::read::Destination::Reply,
        };
        let mut callbacks = 0;
        let error = crate::read::descriptor_interruptible(&config, reader.as_fd(), || {
            callbacks += 1;
            if super::owner_cancelled() {
                Err(io::ErrorKind::Interrupted.into())
            } else {
                Ok(())
            }
        })
        .unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::Interrupted);
        assert!(callbacks > 0);
        assert_eq!(
            child.wait().unwrap().code(),
            29,
            "watch must not reap the child"
        );
        assert!(
            super::owner_cancelled(),
            "already-reaped evaluator remains cancelled"
        );
    }
    let mut current = unsafe { std::mem::zeroed::<libc::sigset_t>() };
    unsafe {
        assert_eq!(
            libc::pthread_sigmask(libc::SIG_SETMASK, std::ptr::null(), &mut current),
            0
        );
        assert_eq!(libc::sigismember(&current, libc::SIGCHLD), 1);
        assert_eq!(
            libc::pthread_sigmask(libc::SIG_SETMASK, &old, std::ptr::null_mut()),
            0
        );
    }
}
