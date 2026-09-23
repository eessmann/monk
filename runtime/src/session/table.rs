//! Source scopes own aliases of OS descriptions. Copying a scope shares offsets.
use crate::{
    native::{self, Streams},
    types::SourceFd,
};
use rustix::fs::{self, Mode, OFlags};
use std::{
    collections::BTreeMap,
    io::{self, Write},
    os::fd::{AsFd, OwnedFd},
};
type Bindings = BTreeMap<SourceFd, Option<OwnedFd>>;
pub(super) struct Table {
    stack: Vec<Bindings>,
}
pub(super) fn copy(streams: &Streams) -> io::Result<Streams> {
    streams
        .iter()
        .map(|(n, fd)| Ok((*n, native::duplicate_private(fd.as_fd())?)))
        .collect()
}
impl Table {
    pub fn new() -> io::Result<Self> {
        let mut numbers = Vec::new();
        if let Some(value) = std::env::var_os("MONK_SESSION_FDS") {
            use std::os::unix::ffi::OsStrExt;
            for number in value
                .as_bytes()
                .split(|b| *b == b',')
                .filter(|v| !v.is_empty())
            {
                let n = super::integer(number)?;
                if !(3..=1048575).contains(&n) {
                    return Err(super::invalid("invalid inherited descriptor table"));
                }
                numbers.push(n as i32);
            }
        }
        let initial = native::adopt_initial_inherited(&numbers)?
            .into_iter()
            .map(|(n, fd)| (n, Some(fd)))
            .collect();
        Ok(Self {
            stack: vec![initial],
        })
    }
    pub fn push(&mut self) -> io::Result<()> {
        let next = self
            .stack
            .last()
            .unwrap()
            .iter()
            .map(|(n, fd)| {
                Ok((
                    *n,
                    fd.as_ref()
                        .map(|f| native::duplicate_private(f.as_fd()))
                        .transpose()?,
                ))
            })
            .collect::<io::Result<_>>()?;
        self.stack.push(next);
        Ok(())
    }
    pub fn pop(&mut self, count: usize) -> io::Result<()> {
        if count >= self.stack.len() {
            return Err(super::invalid("invalid descriptor scope pop"));
        }
        self.stack.truncate(self.stack.len() - count);
        Ok(())
    }
    pub fn reset(&mut self) {
        self.stack.truncate(1);
    }
    pub fn merged(&self, inherited: &Streams) -> io::Result<Streams> {
        let bindings = self.stack.last().unwrap();
        let mut result = Streams::new();
        for (n, fd) in inherited {
            if !bindings.contains_key(n) {
                result.insert(*n, native::duplicate_private(fd.as_fd())?);
            }
        }
        for (n, fd) in bindings {
            if let Some(fd) = fd {
                result.insert(*n, native::duplicate_private(fd.as_fd())?);
            }
        }
        Ok(result)
    }
    pub fn set(&mut self, number: SourceFd, value: Option<OwnedFd>) {
        self.stack.last_mut().unwrap().insert(number, value);
    }
    pub fn data(&mut self, number: SourceFd, value: &[u8]) -> io::Result<()> {
        let mut file = tempfile::NamedTempFile::new()?;
        file.write_all(value)?;
        file.flush()?;
        let reader = fs::open(file.path(), OFlags::RDONLY | OFlags::CLOEXEC, Mode::empty())?;
        file.close()?;
        self.set(number, Some(native::duplicate_private(reader.as_fd())?));
        Ok(())
    }
    pub fn open(
        &mut self,
        number: SourceFd,
        cwd: native::BorrowedDirectory<'_>,
        mode: &[u8],
        path: &[u8],
        mut check: impl FnMut() -> io::Result<()>,
    ) -> io::Result<()> {
        let flags = match mode {
            b"read" => OFlags::RDONLY,
            b"write" => OFlags::WRONLY | OFlags::CREATE | OFlags::TRUNC,
            b"append" => OFlags::WRONLY | OFlags::CREATE | OFlags::APPEND,
            b"read-write" => OFlags::RDWR | OFlags::CREATE,
            _ => return Err(super::invalid("invalid descriptor open mode")),
        };
        let fd = loop {
            match fs::openat(
                cwd,
                path,
                flags | OFlags::CLOEXEC,
                Mode::from_raw_mode(0o666),
            ) {
                Err(rustix::io::Errno::INTR) => check()?,
                result => break result?,
            }
        };
        self.set(number, Some(native::duplicate_private(fd.as_fd())?));
        Ok(())
    }
}
