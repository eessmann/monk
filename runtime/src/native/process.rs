use super::*;
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
            .map(|directory| {
                duplicate_private(directory.as_fd()).and_then(WorkingDirectory::from_owned)
            })
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
            let result = if self.asynchronous
                || (initial_signal_ignored(libc::SIGINT) || initial_signal_ignored(libc::SIGQUIT))
            {
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
