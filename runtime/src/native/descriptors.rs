use super::*;
static INITIAL_MASK: AtomicI32 = AtomicI32::new(0);
static INITIAL_IGNORED: AtomicI32 = AtomicI32::new(0);
static RESERVATIONS: OnceLock<[Option<OwnedFd>; 3]> = OnceLock::new();
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
pub(super) fn private_pipe_above(minimum: i32) -> io::Result<(OwnedFd, OwnedFd)> {
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
        .filter(|(name, _)| name != "MONK_WORKSPACE_LEASE")
        .map(|(k, v)| (k.into_vec(), v.into_vec()))
        .collect()
}
pub fn inherited_fd(fd: i32) -> io::Result<OwnedFd> {
    cvt(unsafe { libc::fcntl(fd, libc::F_GETFD) })?;
    duplicate_private(unsafe { BorrowedFd::borrow_raw(fd) })
}
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
        if fd < 3 {
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
