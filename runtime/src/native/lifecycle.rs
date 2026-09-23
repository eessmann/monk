use super::*;
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcessOutcome {
    Exited(i32),
    Signaled(i32),
}
impl ProcessOutcome {
    pub fn code(self) -> i32 {
        match self {
            Self::Exited(c) => c,
            Self::Signaled(s) => 128 + s,
        }
    }
}
pub(super) fn decode_wait_status(status: i32) -> Option<ProcessOutcome> {
    if libc::WIFEXITED(status) {
        Some(ProcessOutcome::Exited(libc::WEXITSTATUS(status)))
    } else if libc::WIFSIGNALED(status) {
        Some(ProcessOutcome::Signaled(libc::WTERMSIG(status)))
    } else {
        None
    }
}
#[derive(Debug)]
pub struct RunningChild {
    pub(super) pid: libc::pid_t,
    pub(super) outcome: Option<ProcessOutcome>,
}
impl RunningChild {
    pub fn pid(&self) -> i32 {
        self.pid
    }
    fn collect(&mut self, blocking: bool) -> io::Result<Option<ProcessOutcome>> {
        use rustix::process::{Pid, WaitOptions, waitpid};
        if self.outcome.is_some() {
            return Ok(self.outcome);
        }
        let pid =
            Pid::from_raw(self.pid).ok_or_else(|| io::Error::from_raw_os_error(libc::ECHILD))?;
        loop {
            match waitpid(
                Some(pid),
                if blocking {
                    WaitOptions::empty()
                } else {
                    WaitOptions::NOHANG
                },
            ) {
                Ok(None) => return Ok(None),
                Ok(Some((_, status))) => {
                    if let Some(outcome) = decode_wait_status(status.as_raw()) {
                        self.outcome = Some(outcome);
                        return Ok(self.outcome);
                    }
                }
                Err(rustix::io::Errno::INTR) if !owner_cancelled() => continue,
                Err(error) => return Err(error.into()),
            }
        }
    }
    pub fn wait(&mut self) -> io::Result<ProcessOutcome> {
        self.collect(true)?
            .ok_or_else(|| io::Error::from_raw_os_error(libc::ECHILD))
    }
    pub fn try_wait(&mut self) -> io::Result<Option<ProcessOutcome>> {
        self.collect(false)
    }
    pub fn terminate(&mut self) -> io::Result<ProcessOutcome> {
        // Never signal a PID after a wait has reported ECHILD; it may already
        // have been reaped elsewhere and recycled. Observation precedes kill.
        loop {
            match self.try_wait() {
                Ok(Some(outcome)) => return Ok(outcome),
                Ok(None) => break,
                Err(error) if error.kind() == io::ErrorKind::Interrupted => continue,
                Err(error) => return Err(error),
            }
        }
        let _ = nix::sys::signal::kill(
            nix::unistd::Pid::from_raw(self.pid),
            nix::sys::signal::Signal::SIGKILL,
        );
        loop {
            match self.wait() {
                Err(error) if error.kind() == io::ErrorKind::Interrupted => continue,
                result => return result,
            }
        }
    }
}
pub fn spawn_status(status: i32) -> io::Result<RunningChild> {
    let pid = unsafe { libc::fork() };
    if pid == 0 {
        unsafe {
            libc::_exit(status);
        }
    }
    cvt(pid)?;
    Ok(RunningChild { pid, outcome: None })
}
/// Final observation of a reaped child. This state has no wait or launch method.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CompletedChild {
    pid: i32,
    outcome: ProcessOutcome,
}
impl CompletedChild {
    pub fn pid(self) -> i32 {
        self.pid
    }
    pub fn outcome(self) -> ProcessOutcome {
        self.outcome
    }
}
impl RunningChild {
    pub fn complete(mut self) -> io::Result<CompletedChild> {
        match self.wait() {
            Ok(outcome) => Ok(CompletedChild {
                pid: self.pid,
                outcome,
            }),
            Err(error) => {
                let _ = self.terminate();
                Err(error)
            }
        }
    }
}
