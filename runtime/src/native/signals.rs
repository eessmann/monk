use super::*;
static PENDING: AtomicI32 = AtomicI32::new(0);
extern "C" fn signal_notice(signal: i32) {
    if signal == libc::SIGCHLD {
        return;
    }
    let _ = PENDING.compare_exchange(0, signal, Ordering::Relaxed, Ordering::Relaxed);
}
pub fn pending_signal() -> Option<i32> {
    match PENDING.load(Ordering::Relaxed) {
        0 => None,
        s => Some(s),
    }
}
pub struct SignalGuard {
    saved: Vec<(i32, libc::sigaction)>,
    stop: std::sync::Arc<std::sync::atomic::AtomicBool>,
    heartbeat: Option<std::thread::JoinHandle<()>>,
    previous_mask: Option<libc::sigset_t>,
}
impl SignalGuard {
    pub fn install() -> io::Result<Self> {
        let mut result = Self {
            saved: Vec::new(),
            stop: std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)),
            heartbeat: None,
            previous_mask: None,
        };
        for signal in [
            libc::SIGHUP,
            libc::SIGINT,
            libc::SIGQUIT,
            libc::SIGTERM,
            libc::SIGPIPE,
            libc::SIGCHLD,
        ] {
            if initial_signal_ignored(signal) {
                continue;
            }
            let mut action = unsafe { std::mem::zeroed::<libc::sigaction>() };
            let mut previous = MaybeUninit::uninit();
            action.sa_sigaction = signal_notice as *const () as usize;
            unsafe {
                libc::sigemptyset(&mut action.sa_mask);
            }
            cvt(unsafe { libc::sigaction(signal, &action, previous.as_mut_ptr()) })?;
            result
                .saved
                .push((signal, unsafe { previous.assume_init() }));
        }
        // The heartbeat must reach the owner even if its caller blocked CHLD.
        // Preserve every other inherited mask bit and restore the original mask.
        let mut chld = unsafe { std::mem::zeroed::<libc::sigset_t>() };
        let mut previous_mask = MaybeUninit::uninit();
        unsafe {
            libc::sigemptyset(&mut chld);
            libc::sigaddset(&mut chld, libc::SIGCHLD);
        }
        positive(unsafe {
            libc::pthread_sigmask(libc::SIG_UNBLOCK, &chld, previous_mask.as_mut_ptr())
        })?;
        result.previous_mask = Some(unsafe { previous_mask.assume_init() });
        // A SIGCHLD edge can arrive just before a blocking syscall. Periodic
        // owner-thread notification closes that lost-wakeup window. This thread
        // owns no descriptors and never waits for/reaps a process.
        let owner = unsafe { libc::pthread_self() } as usize;
        let stop = std::sync::Arc::clone(&result.stop);
        result.heartbeat = Some(
            std::thread::Builder::new()
                .name("monk-owner-wakeup".into())
                .spawn(move || {
                    while !stop.load(Ordering::Acquire) {
                        std::thread::park_timeout(std::time::Duration::from_millis(10));
                        if stop.load(Ordering::Acquire) {
                            break;
                        }
                        unsafe {
                            libc::pthread_kill(owner as libc::pthread_t, libc::SIGCHLD);
                        }
                    }
                })?,
        );
        Ok(result)
    }
}
impl Drop for SignalGuard {
    fn drop(&mut self) {
        self.stop.store(true, Ordering::Release);
        if let Some(thread) = self.heartbeat.take() {
            thread.thread().unpark();
            let _ = thread.join();
        }
        for (signal, action) in &self.saved {
            unsafe {
                libc::sigaction(*signal, action, ptr::null_mut());
            }
        }
        if let Some(mask) = self.previous_mask.take() {
            unsafe {
                libc::pthread_sigmask(libc::SIG_SETMASK, &mask, ptr::null_mut());
            }
        }
    }
}
static EVALUATOR: AtomicI32 = AtomicI32::new(0);
/// A non-reaping lifecycle observation. RunningChild remains the sole reaper.
pub struct EvaluatorWatch {
    pid: i32,
}
impl EvaluatorWatch {
    pub fn new(pid: i32) -> io::Result<Self> {
        if pid <= 0 {
            return Err(io::Error::from_raw_os_error(libc::EINVAL));
        }
        EVALUATOR
            .compare_exchange(0, pid, Ordering::AcqRel, Ordering::Acquire)
            .map_err(|_| {
                io::Error::new(
                    io::ErrorKind::AlreadyExists,
                    "evaluator watch already installed",
                )
            })?;
        Ok(Self { pid })
    }
}
impl Drop for EvaluatorWatch {
    fn drop(&mut self) {
        let _ = EVALUATOR.compare_exchange(self.pid, 0, Ordering::AcqRel, Ordering::Acquire);
    }
}
pub fn owner_cancelled() -> bool {
    if pending_signal().is_some() {
        return true;
    }
    let pid = EVALUATOR.load(Ordering::Acquire);
    if pid == 0 {
        return false;
    }
    use rustix::process::{Pid, WaitId, WaitIdOptions, waitid};
    let Some(pid) = Pid::from_raw(pid) else {
        return true;
    };
    match waitid(
        WaitId::Pid(pid),
        WaitIdOptions::EXITED | WaitIdOptions::NOHANG | WaitIdOptions::NOWAIT,
    ) {
        Ok(Some(status)) => status.exited() || status.killed() || status.dumped(),
        Err(rustix::io::Errno::CHILD) => true,
        _ => false,
    }
}

pub fn terminate_with(signal: i32) -> ! {
    unsafe {
        let mut action = std::mem::zeroed::<libc::sigaction>();
        action.sa_sigaction = libc::SIG_DFL;
        libc::sigemptyset(&mut action.sa_mask);
        libc::sigaction(signal, &action, ptr::null_mut());
        let mut mask = std::mem::zeroed::<libc::sigset_t>();
        libc::sigemptyset(&mut mask);
        libc::sigaddset(&mut mask, signal);
        libc::sigprocmask(libc::SIG_UNBLOCK, &mask, ptr::null_mut());
        libc::kill(libc::getpid(), signal);
        libc::_exit(128 + signal);
    }
}
pub fn initialize_signals() -> io::Result<()> {
    for signal in [libc::SIGINT, libc::SIGQUIT, libc::SIGPIPE] {
        let mut action = unsafe { std::mem::zeroed::<libc::sigaction>() };
        action.sa_sigaction = if initial_signal_ignored(signal) {
            libc::SIG_IGN
        } else {
            libc::SIG_DFL
        };
        unsafe {
            libc::sigemptyset(&mut action.sa_mask);
        }
        cvt(unsafe { libc::sigaction(signal, &action, ptr::null_mut()) })?;
    }
    Ok(())
}
