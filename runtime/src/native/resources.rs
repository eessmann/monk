//! Private temporary files with RAII cleanup and raw native path bytes.
use std::{
    io,
    os::{
        fd::AsFd,
        unix::{ffi::OsStrExt, fs::PermissionsExt},
    },
};
pub(crate) struct Workspace {
    directory: tempfile::TempDir,
}
impl Workspace {
    pub(crate) fn new(prefix: &[u8]) -> io::Result<Self> {
        let prefix = std::str::from_utf8(prefix).map_err(|_| io::ErrorKind::InvalidInput)?;
        Ok(Self {
            directory: tempfile::Builder::new()
                .prefix(prefix)
                .permissions(std::fs::Permissions::from_mode(0o700))
                .tempdir()?,
        })
    }
    pub(crate) fn file(&self, bytes: &[u8]) -> io::Result<Vec<u8>> {
        let file = tempfile::Builder::new()
            .prefix("transport-")
            .permissions(std::fs::Permissions::from_mode(0o600))
            .tempfile_in(self.directory.path())?;
        super::write_all(file.as_file().as_fd(), bytes)?;
        let (_file, path) = file.keep().map_err(|error| error.error)?;
        Ok(path.as_os_str().as_bytes().to_vec())
    }
}
