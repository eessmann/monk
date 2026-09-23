//! The only native process boundary. All allocations precede fork; child execution
//! is a leaf of direct async-signal-safe libc calls ending in execve or _exit.
use crate::types::SourceFd;
use std::{
    collections::BTreeMap,
    ffi::CString,
    io,
    mem::MaybeUninit,
    os::fd::{AsFd, AsRawFd, BorrowedFd, FromRawFd, OwnedFd},
    ptr,
    sync::{
        OnceLock,
        atomic::{AtomicI32, Ordering},
    },
};
pub type Streams = BTreeMap<SourceFd, OwnedFd>;
pub type Environment = Vec<(Vec<u8>, Vec<u8>)>;
static INITIAL_MASK: AtomicI32 = AtomicI32::new(0);
static INITIAL_IGNORED: AtomicI32 = AtomicI32::new(0);
static RESERVATIONS: OnceLock<[Option<OwnedFd>; 3]> = OnceLock::new();
static PENDING: AtomicI32 = AtomicI32::new(0);
const SIGNALS: [i32; 9] = [
    libc::SIGPIPE,
    libc::SIGINT,
    libc::SIGQUIT,
    libc::SIGTERM,
    libc::SIGHUP,
    libc::SIGCHLD,
    libc::SIGTSTP,
    libc::SIGTTIN,
    libc::SIGTTOU,
];
extern "C" fn snapshot() {
    let mut mask = 0;
    for fd in 0..6 {
        if unsafe { libc::fcntl(fd, libc::F_GETFD) } >= 0 {
            mask |= 1 << fd;
        }
    }
    INITIAL_MASK.store(mask, Ordering::Relaxed);
    let mut ignored = 0;
    for (signal, bit) in [(libc::SIGINT, 1), (libc::SIGQUIT, 2)] {
        let mut action = MaybeUninit::<libc::sigaction>::uninit();
        if unsafe { libc::sigaction(signal, ptr::null(), action.as_mut_ptr()) } == 0
            && unsafe { action.assume_init() }.sa_sigaction == libc::SIG_IGN
        {
            ignored |= bit;
        }
    }
    INITIAL_IGNORED.store(ignored, Ordering::Relaxed);
    let reservations = std::array::from_fn(|index| {
        let fd = index as i32;
        if mask & (1 << fd) != 0 {
            return None;
        }
        let raw = unsafe { libc::open(c"/dev/null".as_ptr(), libc::O_RDWR | libc::O_CLOEXEC) };
        if raw < 0 {
            return None;
        }
        let owned = unsafe { OwnedFd::from_raw_fd(raw) };
        if raw == fd {
            Some(owned)
        } else {
            if unsafe { libc::dup2(raw, fd) } < 0 {
                return None;
            }
            unsafe {
                libc::fcntl(fd, libc::F_SETFD, libc::FD_CLOEXEC);
            }
            Some(unsafe { OwnedFd::from_raw_fd(fd) })
        }
    });
    let _ = RESERVATIONS.set(reservations);
}
#[used]
#[cfg_attr(target_os = "macos", unsafe(link_section = "__DATA,__mod_init_func"))]
#[cfg_attr(target_os = "linux", unsafe(link_section = ".init_array"))]
static INITIALIZER: extern "C" fn() = snapshot;
pub fn initial_descriptor_open(fd: i32) -> bool {
    (0..6).contains(&fd) && INITIAL_MASK.load(Ordering::Relaxed) & (1 << fd) != 0
}
pub fn initial_signal_ignored(signal: i32) -> bool {
    let bit = if signal == libc::SIGINT {
        1
    } else if signal == libc::SIGQUIT {
        2
    } else {
        0
    };
    INITIAL_IGNORED.load(Ordering::Relaxed) & bit != 0
}
pub fn initial_streams() -> io::Result<Streams> {
    let mut result = Streams::new();
    for fd in 0..3 {
        if initial_descriptor_open(fd) {
            result.insert(
                SourceFd::new(fd)?,
                duplicate_private(unsafe { BorrowedFd::borrow_raw(fd) })?,
            );
        }
    }
    Ok(result)
}
pub fn duplicate_private(fd: BorrowedFd<'_>) -> io::Result<OwnedFd> {
    duplicate_private_above(fd, 10)
}
pub fn duplicate_private_above(fd: BorrowedFd<'_>, minimum: i32) -> io::Result<OwnedFd> {
    loop {
        match rustix::io::fcntl_dupfd_cloexec(fd, minimum) {
            Ok(owned) => return Ok(owned),
            Err(rustix::io::Errno::INTR) => continue,
            Err(error) => return Err(error.into()),
        }
    }
}
pub fn private_pipe() -> io::Result<(OwnedFd, OwnedFd)> {
    private_pipe_above(10)
}
fn private_pipe_above(minimum: i32) -> io::Result<(OwnedFd, OwnedFd)> {
    let (read, write) = rustix::pipe::pipe()?;
    let read = duplicate_private_above(read.as_fd(), minimum)?;
    let write = duplicate_private_above(write.as_fd(), minimum)?;
    Ok((read, write))
}
/// Owned capability verified to designate a directory, independent of its path.
#[derive(Debug)]
pub struct WorkingDirectory(OwnedFd);
/// Borrowed directory capability; generic descriptors require validation first.
#[derive(Clone, Copy, Debug)]
pub struct BorrowedDirectory<'fd>(BorrowedFd<'fd>);
fn check_directory(fd: BorrowedFd<'_>) -> io::Result<()> {
    if rustix::fs::FileType::from_raw_mode(rustix::fs::fstat(fd)?.st_mode)
        != rustix::fs::FileType::Directory
    {
        return Err(io::Error::from_raw_os_error(libc::ENOTDIR));
    }
    Ok(())
}
impl WorkingDirectory {
    pub fn from_owned(fd: OwnedFd) -> io::Result<Self> {
        check_directory(fd.as_fd())?;
        Ok(Self(fd))
    }
    pub fn borrow(&self) -> BorrowedDirectory<'_> {
        BorrowedDirectory(self.0.as_fd())
    }
    pub fn into_owned(self) -> OwnedFd {
        self.0
    }
}
impl AsFd for WorkingDirectory {
    fn as_fd(&self) -> BorrowedFd<'_> {
        self.0.as_fd()
    }
}
impl<'fd> BorrowedDirectory<'fd> {
    pub fn from_borrowed(fd: BorrowedFd<'fd>) -> io::Result<Self> {
        check_directory(fd)?;
        Ok(Self(fd))
    }
}
impl AsFd for BorrowedDirectory<'_> {
    fn as_fd(&self) -> BorrowedFd<'_> {
        self.0
    }
}
pub fn open_working_directory() -> io::Result<WorkingDirectory> {
    #[cfg(target_os = "linux")]
    let flags = rustix::fs::OFlags::PATH | rustix::fs::OFlags::DIRECTORY;
    #[cfg(target_os = "macos")]
    let flags = rustix::fs::OFlags::from_bits_retain(libc::O_SEARCH as _);
    let owned = rustix::fs::open(
        ".",
        flags | rustix::fs::OFlags::CLOEXEC,
        rustix::fs::Mode::empty(),
    )?;
    duplicate_private(owned.as_fd()).and_then(WorkingDirectory::from_owned)
}

fn cvt(result: i32) -> io::Result<i32> {
    if result < 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(result)
    }
}
fn positive(result: i32) -> io::Result<()> {
    if result == 0 {
        Ok(())
    } else {
        Err(io::Error::from_raw_os_error(result))
    }
}
pub fn native_error_message(error: &io::Error) -> Vec<u8> {
    let code = error.raw_os_error().unwrap_or(libc::EBADF);
    // POSIX strerror strings are copied before any subsequent libc call.
    unsafe { std::ffi::CStr::from_ptr(libc::strerror(code)) }
        .to_bytes()
        .to_vec()
}
fn mark_cloexec(fd: i32) -> io::Result<()> {
    // An inventory slot may be absent (and the Linux fallback deliberately scans
    // absent slots). Raw fcntl is required here: constructing BorrowedFd before
    // proving that the slot is open would violate its safety contract.
    let flags = loop {
        let result = unsafe { libc::fcntl(fd, libc::F_GETFD) };
        if result >= 0 {
            break result;
        }
        let error = io::Error::last_os_error();
        if error.raw_os_error() == Some(libc::EBADF) {
            return Ok(());
        }
        if error.kind() != io::ErrorKind::Interrupted {
            return Err(error);
        }
    };
    loop {
        if unsafe { libc::fcntl(fd, libc::F_SETFD, flags | libc::FD_CLOEXEC) } >= 0 {
            return Ok(());
        }
        let error = io::Error::last_os_error();
        if error.raw_os_error() == Some(libc::EBADF) {
            return Ok(());
        }
        if error.kind() != io::ErrorKind::Interrupted {
            return Err(error);
        }
    }
}

pub fn private_close_on_exec() -> io::Result<()> {
    #[cfg(target_os = "macos")]
    {
        unsafe {
            *libc::__error() = 0;
        }
        let mut needed = unsafe {
            libc::proc_pidinfo(libc::getpid(), libc::PROC_PIDLISTFDS, 0, ptr::null_mut(), 0)
        };
        if needed < 0 || (needed == 0 && unsafe { *libc::__error() } != 0) {
            return Err(io::Error::last_os_error());
        }
        if needed == 0 {
            return Ok(());
        }
        loop {
            let count = needed as usize / std::mem::size_of::<libc::proc_fdinfo>() + 32;
            let mut fds: Vec<MaybeUninit<libc::proc_fdinfo>> = Vec::with_capacity(count);
            fds.resize_with(count, MaybeUninit::uninit);
            let capacity = std::mem::size_of_val(fds.as_slice()) as i32;
            unsafe {
                *libc::__error() = 0;
            }
            let used = unsafe {
                libc::proc_pidinfo(
                    libc::getpid(),
                    libc::PROC_PIDLISTFDS,
                    0,
                    fds.as_mut_ptr().cast(),
                    capacity,
                )
            };
            if used < 0 || (used == 0 && unsafe { *libc::__error() } != 0) {
                return Err(io::Error::last_os_error());
            }
            if used >= capacity {
                needed = used;
                continue;
            }
            for info in fds
                .iter()
                .take(used as usize / std::mem::size_of::<libc::proc_fdinfo>())
            {
                let fd = unsafe { info.assume_init_ref() }.proc_fd;
                if fd >= 3 {
                    mark_cloexec(fd)?;
                }
            }
            return Ok(());
        }
    }
    #[cfg(target_os = "linux")]
    {
        if unsafe {
            libc::syscall(
                libc::SYS_close_range,
                3u32,
                u32::MAX,
                libc::CLOSE_RANGE_CLOEXEC,
            )
        } == 0
        {
            return Ok(());
        }
        let e = io::Error::last_os_error();
        if !matches!(
            e.raw_os_error(),
            Some(libc::ENOSYS | libc::EINVAL | libc::EPERM)
        ) {
            return Err(e);
        }
        let mut limits = MaybeUninit::<libc::rlimit>::uninit();
        cvt(unsafe { libc::getrlimit(libc::RLIMIT_NOFILE, limits.as_mut_ptr()) })?;
        let hard = unsafe { limits.assume_init() }.rlim_max;
        let maximum = if hard == libc::RLIM_INFINITY {
            let configured = unsafe { libc::sysconf(libc::_SC_OPEN_MAX) };
            if configured < 0 {
                return Err(io::Error::last_os_error());
            }
            configured as libc::rlim_t
        } else {
            hard
        }
        .min(i32::MAX as libc::rlim_t);
        for fd in 3..maximum as i32 {
            mark_cloexec(fd)?;
        }
        Ok(())
    }
}
pub fn write_all(fd: BorrowedFd<'_>, mut bytes: &[u8]) -> io::Result<()> {
    while !bytes.is_empty() {
        if owner_cancelled() {
            return Err(io::ErrorKind::Interrupted.into());
        }
        match rustix::io::write(fd, bytes) {
            Ok(0) => return Err(io::ErrorKind::WriteZero.into()),
            Ok(n) => bytes = &bytes[n..],
            Err(rustix::io::Errno::INTR) if !owner_cancelled() => continue,
            Err(e) => return Err(e.into()),
        }
    }
    Ok(())
}

pub fn environment() -> Environment {
    use std::os::unix::ffi::OsStringExt;
    std::env::vars_os()
        .map(|(k, v)| (k.into_vec(), v.into_vec()))
        .collect()
}
pub fn candidate_paths(environment: &[(Vec<u8>, Vec<u8>)], command: &[u8]) -> Vec<Vec<u8>> {
    if command.contains(&b'/') {
        return vec![command.to_vec()];
    }
    let path = environment
        .iter()
        .find(|(k, _)| k == b"PATH")
        .map(|(_, v)| v.as_slice())
        .unwrap_or(b"/bin:/usr/bin");
    path.split(|b| *b == b':')
        .map(|dir| {
            if dir.is_empty() {
                command.to_vec()
            } else {
                [dir, b"/", command].concat()
            }
        })
        .collect()
}
struct Prepared {
    _argv: Vec<CString>,
    argv: Vec<*mut libc::c_char>,
    _env: Vec<CString>,
    env: Vec<*mut libc::c_char>,
}
impl Prepared {
    fn new(
        environment: &[(Vec<u8>, Vec<u8>)],
        command: &[u8],
        args: &[Vec<u8>],
    ) -> io::Result<Self> {
        let invalid = || io::Error::from_raw_os_error(libc::EINVAL);
        let av = std::iter::once(command)
            .chain(args.iter().map(Vec::as_slice))
            .map(|v| CString::new(v).map_err(|_| invalid()))
            .collect::<io::Result<Vec<_>>>()?;
        let ev = environment
            .iter()
            .map(|(k, v)| {
                CString::new([k.as_slice(), b"=", v.as_slice()].concat()).map_err(|_| invalid())
            })
            .collect::<io::Result<Vec<_>>>()?;
        let mut argv = av.iter().map(|s| s.as_ptr().cast_mut()).collect::<Vec<_>>();
        argv.push(ptr::null_mut());
        let mut env = ev.iter().map(|s| s.as_ptr().cast_mut()).collect::<Vec<_>>();
        env.push(ptr::null_mut());
        Ok(Self {
            _argv: av,
            argv,
            _env: ev,
            env,
        })
    }
}
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
fn decode_wait_status(status: i32) -> Option<ProcessOutcome> {
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
    pid: libc::pid_t,
    outcome: Option<ProcessOutcome>,
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
struct Actions(libc::posix_spawn_file_actions_t);
impl Actions {
    fn new() -> io::Result<Self> {
        let mut value = MaybeUninit::uninit();
        positive(unsafe { libc::posix_spawn_file_actions_init(value.as_mut_ptr()) })?;
        Ok(Self(unsafe { value.assume_init() }))
    }
}
impl Drop for Actions {
    fn drop(&mut self) {
        unsafe {
            libc::posix_spawn_file_actions_destroy(&mut self.0);
        }
    }
}
struct Attributes(libc::posix_spawnattr_t);
impl Attributes {
    fn new() -> io::Result<Self> {
        let mut value = MaybeUninit::uninit();
        positive(unsafe { libc::posix_spawnattr_init(value.as_mut_ptr()) })?;
        Ok(Self(unsafe { value.assume_init() }))
    }
}
impl Drop for Attributes {
    fn drop(&mut self) {
        unsafe {
            libc::posix_spawnattr_destroy(&mut self.0);
        }
    }
}
// libc does not expose this Darwin 10.15+ extension. Use its opaque action type.
#[cfg(target_os = "macos")]
unsafe extern "C" {
    fn posix_spawn_file_actions_addfchdir_np(
        actions: *mut libc::posix_spawn_file_actions_t,
        fd: libc::c_int,
    ) -> libc::c_int;
}
fn add_cwd(actions: &mut Actions, fd: i32) -> io::Result<()> {
    #[cfg(target_os = "macos")]
    let result = unsafe { posix_spawn_file_actions_addfchdir_np(&mut actions.0, fd) };
    #[cfg(target_os = "linux")]
    let result = unsafe { libc::posix_spawn_file_actions_addfchdir_np(&mut actions.0, fd) };
    positive(result)
}
/// A fully owned launch plan. Launch consumes its strings, cwd and descriptor
/// capabilities, so no caller can accidentally launch the same plan twice.
pub struct PreparedLaunch {
    _owned: Vec<(SourceFd, OwnedFd)>,
    moves: Vec<(i32, i32)>,
    cwd: Option<WorkingDirectory>,
    prepared: Prepared,
    paths: Vec<CString>,
    closed: i32,
    search: bool,
    asynchronous: bool,
}
impl PreparedLaunch {
    pub fn prepare(
        streams: &Streams,
        cwd: Option<BorrowedDirectory<'_>>,
        environment: &[(Vec<u8>, Vec<u8>)],
        command: &[u8],
        args: &[Vec<u8>],
        asynchronous: bool,
    ) -> io::Result<Self> {
        let minimum = streams
            .keys()
            .map(|fd| fd.get() + 1)
            .max()
            .unwrap_or(10)
            .max(10);
        let owned = streams
            .iter()
            .map(|(target, fd)| {
                duplicate_private_above(fd.as_fd(), minimum).map(|source| (*target, source))
            })
            .collect::<io::Result<Vec<_>>>()?;
        let moves = owned
            .iter()
            .map(|(target, fd)| (target.get(), fd.as_raw_fd()))
            .collect();
        let cwd = cwd
            .map(|directory| duplicate_private(directory.as_fd()).map(WorkingDirectory))
            .transpose()?;
        let closed = (0..3)
            .filter(|fd| !streams.contains_key(&SourceFd::new(*fd).expect("stdio")))
            .fold(0, |mask, fd| mask | (1 << fd));
        let prepared = Prepared::new(environment, command, args)?;
        let paths = candidate_paths(environment, command)
            .into_iter()
            .map(|path| CString::new(path).map_err(|_| io::Error::from_raw_os_error(libc::EINVAL)))
            .collect::<io::Result<Vec<_>>>()?;
        Ok(Self {
            _owned: owned,
            moves,
            cwd,
            prepared,
            paths,
            closed,
            search: !command.contains(&b'/'),
            asynchronous,
        })
    }
    pub fn launch(self) -> io::Result<RunningChild> {
        private_close_on_exec()?;
        let mut previous = libc::ENOENT;
        for path in &self.paths {
            let result = if self.asynchronous || INITIAL_IGNORED.load(Ordering::Relaxed) != 0 {
                spawn_ignored(
                    path,
                    &self.prepared,
                    self.cwd
                        .as_ref()
                        .map(|directory| directory.as_fd().as_raw_fd()),
                    &self.moves,
                    self.closed,
                    self.asynchronous,
                )
            } else {
                spawn_posix(
                    path,
                    &self.prepared,
                    self.cwd
                        .as_ref()
                        .map(|directory| directory.as_fd().as_raw_fd()),
                    &self.moves,
                    self.closed,
                )
            };
            match result {
                Ok(pid) => return Ok(RunningChild { pid, outcome: None }),
                Err(error) => {
                    let code = error.raw_os_error().unwrap_or(libc::EIO);
                    if self.search && matches!(code, libc::ENOENT | libc::ENOTDIR | libc::EACCES) {
                        if code == libc::EACCES {
                            previous = code;
                        }
                    } else {
                        return Err(error);
                    }
                }
            }
        }
        Err(io::Error::from_raw_os_error(previous))
    }
}
pub fn spawn(
    streams: &Streams,
    cwd: Option<BorrowedDirectory<'_>>,
    environment: &[(Vec<u8>, Vec<u8>)],
    command: &[u8],
    args: &[Vec<u8>],
    asynchronous: bool,
) -> io::Result<RunningChild> {
    PreparedLaunch::prepare(streams, cwd, environment, command, args, asynchronous)?.launch()
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

fn spawn_posix(
    path: &CString,
    prepared: &Prepared,
    cwd: Option<i32>,
    moves: &[(i32, i32)],
    closed: i32,
) -> io::Result<libc::pid_t> {
    let mut actions = Actions::new()?;
    let mut attributes = Attributes::new()?;
    if let Some(fd) = cwd {
        add_cwd(&mut actions, fd)?;
    }
    for fd in 0..3 {
        if closed & (1 << fd) != 0 && unsafe { libc::fcntl(fd, libc::F_GETFD) } >= 0 {
            positive(unsafe { libc::posix_spawn_file_actions_addclose(&mut actions.0, fd) })?;
        }
    }
    for &(target, source) in moves {
        positive(unsafe {
            libc::posix_spawn_file_actions_adddup2(&mut actions.0, source, target)
        })?;
    }
    let mut defaults = unsafe { std::mem::zeroed::<libc::sigset_t>() };
    let mut mask = unsafe { std::mem::zeroed::<libc::sigset_t>() };
    unsafe {
        libc::sigemptyset(&mut defaults);
        libc::sigemptyset(&mut mask);
    }
    for signal in SIGNALS {
        unsafe {
            libc::sigaddset(&mut defaults, signal);
        }
    }
    positive(unsafe { libc::posix_spawnattr_setsigdefault(&mut attributes.0, &defaults) })?;
    positive(unsafe { libc::posix_spawnattr_setsigmask(&mut attributes.0, &mask) })?;
    positive(unsafe {
        libc::posix_spawnattr_setflags(
            &mut attributes.0,
            (libc::POSIX_SPAWN_SETSIGDEF | libc::POSIX_SPAWN_SETSIGMASK) as _,
        )
    })?;
    let mut pid = 0;
    positive(unsafe {
        libc::posix_spawn(
            &mut pid,
            path.as_ptr(),
            &actions.0,
            &attributes.0,
            prepared.argv.as_ptr(),
            prepared.env.as_ptr(),
        )
    })?;
    Ok(pid)
}
// Prebuilt signal actions are copied in the parent, then applied without constructing
// Rust objects after fork. No post-fork branch returns to Rust ownership/destruction.
fn child_actions(asynchronous: bool) -> [libc::sigaction; 9] {
    SIGNALS.map(|signal| {
        let mut action = unsafe { std::mem::zeroed::<libc::sigaction>() };
        unsafe {
            libc::sigemptyset(&mut action.sa_mask);
        }
        action.sa_sigaction = if initial_signal_ignored(signal)
            || (asynchronous && (signal == libc::SIGINT || signal == libc::SIGQUIT))
        {
            libc::SIG_IGN
        } else {
            libc::SIG_DFL
        };
        action
    })
}
#[cfg(target_os = "macos")]
unsafe fn errno() -> i32 {
    unsafe { *libc::__error() }
}
#[cfg(target_os = "linux")]
unsafe fn errno() -> i32 {
    unsafe { *libc::__errno_location() }
}
fn spawn_ignored(
    path: &CString,
    prepared: &Prepared,
    cwd: Option<i32>,
    moves: &[(i32, i32)],
    closed: i32,
    asynchronous: bool,
) -> io::Result<libc::pid_t> {
    let minimum = moves
        .iter()
        .flat_map(|(a, b)| [*a, *b])
        .chain(cwd)
        .max()
        .unwrap_or(9)
        .max(9)
        + 1;
    let (reader, writer) = private_pipe_above(minimum)?;
    let read_fd = reader.as_raw_fd();
    let write_fd = writer.as_raw_fd();
    let actions = child_actions(asynchronous);
    let mut blocked = unsafe { std::mem::zeroed::<libc::sigset_t>() };
    let mut prior = unsafe { std::mem::zeroed::<libc::sigset_t>() };
    let mut empty = unsafe { std::mem::zeroed::<libc::sigset_t>() };
    unsafe {
        libc::sigfillset(&mut blocked);
        libc::sigemptyset(&mut empty);
    }
    positive(unsafe { libc::pthread_sigmask(libc::SIG_SETMASK, &blocked, &mut prior) })?;
    let pid = unsafe { libc::fork() };
    let fork_error = if pid < 0 {
        Some(io::Error::last_os_error())
    } else {
        None
    };
    if pid == 0 {
        unsafe {
            libc::close(read_fd);
            let mut error = 0;
            if let Some(fd) = cwd
                && libc::fchdir(fd) < 0
            {
                error = errno();
            }
            for fd in 0..3 {
                if closed & (1 << fd) != 0 {
                    libc::close(fd);
                }
            }
            for &(target, source) in moves {
                if error == 0 && libc::dup2(source, target) < 0 {
                    error = errno();
                }
            }
            for (signal, action) in SIGNALS.iter().zip(actions.iter()) {
                if error == 0 && libc::sigaction(*signal, action, ptr::null_mut()) < 0 {
                    error = errno();
                }
            }
            if error == 0 && libc::sigprocmask(libc::SIG_SETMASK, &empty, ptr::null_mut()) < 0 {
                error = errno();
            }
            if error == 0 {
                libc::execve(
                    path.as_ptr(),
                    prepared.argv.as_ptr().cast(),
                    prepared.env.as_ptr().cast(),
                );
                error = errno();
            }
            let mut bytes = (&error as *const i32).cast::<u8>();
            let mut remaining = std::mem::size_of::<i32>();
            while remaining > 0 {
                let written = libc::write(write_fd, bytes.cast(), remaining);
                if written < 0 && errno() == libc::EINTR {
                    continue;
                }
                if written <= 0 {
                    break;
                }
                bytes = bytes.add(written as usize);
                remaining -= written as usize;
            }
            libc::_exit(127);
        }
    }
    unsafe {
        libc::pthread_sigmask(libc::SIG_SETMASK, &prior, ptr::null_mut());
    }
    if let Some(error) = fork_error {
        return Err(error);
    }
    drop(writer);
    let mut error = 0i32;
    let mut received = 0usize;
    while received < std::mem::size_of::<i32>() {
        let n = unsafe {
            libc::read(
                read_fd,
                (&mut error as *mut i32).cast::<u8>().add(received).cast(),
                std::mem::size_of::<i32>() - received,
            )
        };
        if n < 0 {
            let e = io::Error::last_os_error();
            if e.kind() == io::ErrorKind::Interrupted && !owner_cancelled() {
                continue;
            }
            unsafe {
                libc::kill(pid, libc::SIGKILL);
            }
            error = e.raw_os_error().unwrap_or(libc::EIO);
            received = std::mem::size_of::<i32>();
            break;
        }
        if n == 0 {
            break;
        }
        received += n as usize;
    }
    if received > 0 {
        if received != std::mem::size_of::<i32>() {
            error = libc::EIO;
        }
        loop {
            if unsafe { libc::waitpid(pid, ptr::null_mut(), 0) } >= 0
                || io::Error::last_os_error().kind() != io::ErrorKind::Interrupted
            {
                break;
            }
        }
        return Err(io::Error::from_raw_os_error(error));
    }
    Ok(pid)
}
pub fn exec_process(
    environment: &[(Vec<u8>, Vec<u8>)],
    command: &[u8],
    args: &[Vec<u8>],
) -> io::Error {
    match exec_inner(environment, command, args) {
        Ok(()) => io::Error::from_raw_os_error(libc::EIO),
        Err(e) => e,
    }
}
fn exec_inner(
    environment: &[(Vec<u8>, Vec<u8>)],
    command: &[u8],
    args: &[Vec<u8>],
) -> io::Result<()> {
    private_close_on_exec()?;
    let prepared = Prepared::new(environment, command, args)?;
    let mut previous = libc::ENOENT;
    for path in candidate_paths(environment, command) {
        let path = CString::new(path).map_err(|_| io::Error::from_raw_os_error(libc::EINVAL))?;
        let e = exec_candidate(&path, &prepared)?;
        if !command.contains(&b'/') && matches!(e, libc::ENOENT | libc::ENOTDIR | libc::EACCES) {
            if e == libc::EACCES {
                previous = e;
            }
        } else {
            return Err(io::Error::from_raw_os_error(e));
        }
    }
    Err(io::Error::from_raw_os_error(previous))
}
fn exec_candidate(path: &CString, prepared: &Prepared) -> io::Result<i32> {
    let mut saved = unsafe { std::mem::zeroed::<[libc::sigaction; 9]>() };
    let mut mask = unsafe { std::mem::zeroed::<libc::sigset_t>() };
    for (signal, action) in SIGNALS.iter().zip(saved.iter_mut()) {
        cvt(unsafe { libc::sigaction(*signal, ptr::null(), action) })?;
    }
    cvt(unsafe { libc::sigprocmask(libc::SIG_SETMASK, ptr::null(), &mut mask) })?;
    let mut protected = Vec::new();
    for fd in 0..3 {
        if !initial_descriptor_open(fd) {
            let flags = unsafe { libc::fcntl(fd, libc::F_GETFD) };
            if flags >= 0 {
                protected.push((
                    fd,
                    flags,
                    duplicate_private(unsafe { BorrowedFd::borrow_raw(fd) })?,
                ));
            }
        }
    }
    let actions = child_actions(false);
    let mut empty = unsafe { std::mem::zeroed::<libc::sigset_t>() };
    unsafe {
        libc::sigemptyset(&mut empty);
    }
    for fd in 0..3 {
        if !initial_descriptor_open(fd) {
            unsafe {
                libc::close(fd);
            }
        }
    }
    let mut failure = 0;
    for (signal, action) in SIGNALS.iter().zip(actions.iter()) {
        if unsafe { libc::sigaction(*signal, action, ptr::null_mut()) } < 0 {
            failure = unsafe { errno() };
            break;
        }
    }
    if failure == 0 && unsafe { libc::sigprocmask(libc::SIG_SETMASK, &empty, ptr::null_mut()) } < 0
    {
        failure = unsafe { errno() };
    }
    if failure == 0 {
        unsafe {
            libc::execve(
                path.as_ptr(),
                prepared.argv.as_ptr().cast(),
                prepared.env.as_ptr().cast(),
            );
            failure = errno();
        }
    }
    for (fd, flags, saved) in protected {
        loop {
            if unsafe { libc::dup2(saved.as_raw_fd(), fd) } >= 0
                || io::Error::last_os_error().kind() != io::ErrorKind::Interrupted
            {
                break;
            }
        }
        unsafe {
            libc::fcntl(fd, libc::F_SETFD, flags);
        }
    }
    for (signal, action) in SIGNALS.iter().zip(saved.iter()) {
        unsafe {
            libc::sigaction(*signal, action, ptr::null_mut());
        }
    }
    unsafe {
        libc::sigprocmask(libc::SIG_SETMASK, &mask, ptr::null_mut());
    }
    Ok(failure)
}
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
#[cfg(test)]
#[path = "native/tests.rs"]
mod tests;

/// Duplicate a borrowed inherited descriptor without taking ownership of it.
pub fn inherited_fd(fd: i32) -> io::Result<OwnedFd> {
    cvt(unsafe { libc::fcntl(fd, libc::F_GETFD) })?;
    duplicate_private(unsafe { BorrowedFd::borrow_raw(fd) })
}
/// Take a manifest/transport descriptor exactly once, moving it above reserved stdio.
/// Internal ownership precondition: `fd` is exclusively inherited from the
/// launching process, is not owned by any Rust value, and has not previously
/// been adopted. The CLI guardian is the sole caller; no general raw-fd API is
/// exported. Validation alone does not establish this ownership precondition.
pub(crate) fn adopt_inherited(fd: i32) -> io::Result<OwnedFd> {
    if fd < 3 {
        return Err(io::Error::from_raw_os_error(libc::EBADF));
    }
    cvt(unsafe { libc::fcntl(fd, libc::F_GETFD) })?;
    let inherited = unsafe { OwnedFd::from_raw_fd(fd) };
    duplicate_private(inherited.as_fd())
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
/// Probe only the real pipe alias facility used by process substitutions.
/// Nonblocking opens/reads bound the check without requiring a worker thread.
pub fn probe_pipe_paths() -> io::Result<bool> {
    use rustix::fs::{FileType, Mode, OFlags};
    let (reader, writer) = rustix::pipe::pipe()?;
    let read_alias = rustix::fs::open(
        format!("/dev/fd/{}", reader.as_raw_fd()),
        OFlags::RDONLY | OFlags::NONBLOCK | OFlags::CLOEXEC,
        Mode::empty(),
    )?;
    let write_alias = rustix::fs::open(
        format!("/dev/fd/{}", writer.as_raw_fd()),
        OFlags::WRONLY | OFlags::NONBLOCK | OFlags::CLOEXEC,
        Mode::empty(),
    )?;
    if FileType::from_raw_mode(rustix::fs::fstat(&read_alias)?.st_mode) != FileType::Fifo {
        return Ok(false);
    }
    rustix::io::write(&write_alias, b"M")?;
    let mut byte = [0u8; 1];
    if rustix::io::read(&read_alias, &mut byte)? != 1 || byte != *b"M" {
        return Ok(false);
    }
    drop(writer);
    drop(write_alias);
    Ok(rustix::io::read(&read_alias, &mut byte)? == 0)
}
#[path = "native/resources.rs"]
mod resources;
pub(crate) use resources::Workspace;

/// Adopt the startup manifest before the process opens any other non-standard
/// descriptors. Only the session bootstrap calls this private native boundary.
/// The entire set is checked before duplication; every duplicate is above all
/// originals, and originals remain owned until every duplicate is acquired.
pub(crate) fn adopt_initial_inherited(numbers: &[i32]) -> io::Result<Streams> {
    let unique = numbers
        .iter()
        .copied()
        .collect::<std::collections::BTreeSet<_>>();
    for &fd in &unique {
        if !(3..=1048575).contains(&fd) {
            return Err(io::Error::from_raw_os_error(libc::EBADF));
        }
        cvt(unsafe { libc::fcntl(fd, libc::F_GETFD) })?;
    }
    let minimum = unique.last().copied().unwrap_or(9).max(9) + 1;
    // The caller runs at bootstrap, before any Rust-owned fd can occupy one of
    // these inherited slots; the validated unique set establishes exclusivity.
    let originals = unique
        .into_iter()
        .map(|fd| (fd, unsafe { OwnedFd::from_raw_fd(fd) }))
        .collect::<Vec<_>>();
    let mut streams = Streams::new();
    for (fd, owned) in &originals {
        streams.insert(
            SourceFd::new(*fd)?,
            duplicate_private_above(owned.as_fd(), minimum)?,
        );
    }
    Ok(streams)
}
