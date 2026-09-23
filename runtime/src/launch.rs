//! Native launcher preserving descriptors before Fish repairs standard streams.
use crate::native::{self, Workspace};
use std::{
    io,
    os::{
        fd::AsFd,
        unix::ffi::{OsStrExt, OsStringExt},
    },
};
pub fn dispatch(args: &[Vec<u8>]) -> io::Result<i32> {
    let result = launch(args);
    if let Some(signal) = native::pending_signal() {
        native::terminate_with(signal);
    }
    match result {
        Ok(outcome) => match outcome {
            native::ProcessOutcome::Exited(code) => Ok(code),
            native::ProcessOutcome::Signaled(signal) => native::terminate_with(signal),
        },
        Err(error) if args.is_empty() => Err(error),
        Err(_) => {
            if let Some(signal) = native::pending_signal() {
                native::terminate_with(signal);
            }
            if native::initial_descriptor_open(2)
                && let Ok(fd) = native::inherited_fd(2)
            {
                let _ = native::write_all(fd.as_fd(), b"monk-runtime: native launch failed\n");
            }
            Ok(125)
        }
    }
}
fn launch(args: &[Vec<u8>]) -> io::Result<native::ProcessOutcome> {
    let [file, arguments @ ..] = args else {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "launch needs a compiled Fish file",
        ));
    };
    let mut environment = native::environment();
    if environment
        .iter()
        .any(|(k, _)| k == b"MONK_LAUNCH_ORIGINAL" || k == b"MONK_LAUNCH_WRAPPER")
    {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "reserved launcher markers already present",
        ));
    }
    let original = std::path::absolute(std::path::PathBuf::from(std::ffi::OsString::from_vec(
        file.clone(),
    )))?;
    let original_bytes = original.as_os_str().as_bytes().to_vec();
    let streams = native::initial_streams()?;
    let closed = (0..3)
        .filter(|fd| !native::initial_descriptor_open(*fd))
        .collect::<Vec<_>>();
    let mut workspace = None;
    let script = if closed.is_empty() {
        original_bytes.clone()
    } else {
        let owner = Workspace::new(b"monk-launch-")?;
        let body = std::fs::read(&original)?;
        let mut wrapper = [b"begin\n".as_slice(), &body, b"\nend"].concat();
        for fd in closed {
            wrapper.extend_from_slice(format!(" {fd}>&-").as_bytes());
        }
        wrapper.push(b'\n');
        let path = owner.file(&wrapper)?;
        environment.push((b"MONK_LAUNCH_ORIGINAL".to_vec(), original_bytes));
        environment.push((b"MONK_LAUNCH_WRAPPER".to_vec(), path.clone()));
        workspace = Some(owner);
        path
    };
    let guard = native::SignalGuard::install()?;
    let mut argv = vec![b"--no-config".to_vec(), script];
    argv.extend_from_slice(arguments);
    let child = native::spawn(&streams, None, &environment, b"fish", &argv, false)?;
    let result = child.complete().map(|completed| completed.outcome());
    drop(guard);
    drop(workspace);
    result
}
