//! Validated source-language descriptor numbers. OS descriptors remain RAII capabilities.
use std::io;
type DescriptorNumber = std::pat::pattern_type!(i32 is 0..=2147483646);
/// A validated Bash descriptor number, distinct from an owned OS descriptor.
/// ```compile_fail
/// use monk_runtime::capabilities::SourceFd;
/// let bad = SourceFd(-1);
/// ```
#[derive(Clone, Copy)]
pub struct SourceFd(DescriptorNumber);
impl SourceFd {
    pub fn new(number: i32) -> io::Result<Self> {
        // SAFETY: the match proves the pattern refinement before the representation-preserving conversion.
        match number {
            value @ 0..=2147483646 => Ok(Self(unsafe {
                std::mem::transmute::<i32, DescriptorNumber>(value)
            })),
            _ => Err(io::Error::from_raw_os_error(libc::EBADF)),
        }
    }
    pub fn get(self) -> i32 {
        unsafe { std::mem::transmute::<DescriptorNumber, i32>(self.0) }
    }
}
type MaskBits = std::pat::pattern_type!(u8 is 0..=7);
#[derive(Clone, Copy)]
pub struct DescriptorMask(MaskBits);
impl DescriptorMask {
    pub fn new(bits: u8) -> io::Result<Self> {
        // SAFETY: only validated mask values enter the private pattern type.
        match bits {
            value @ 0..=7 => Ok(Self(unsafe { std::mem::transmute::<u8, MaskBits>(value) })),
            _ => Err(io::Error::from_raw_os_error(libc::EINVAL)),
        }
    }
    pub fn get(self) -> u8 {
        unsafe { std::mem::transmute::<MaskBits, u8>(self.0) }
    }
    pub fn contains(self, descriptor: SourceFd) -> bool {
        descriptor.get() < 3 && self.get() & (1 << descriptor.get()) != 0
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn source_descriptor_bounds() {
        assert!(SourceFd::new(-1).is_err());
        assert!(SourceFd::new(2147483647).is_err());
        assert_eq!(SourceFd::new(2147483646).unwrap().get(), 2147483646);
    }
    #[test]
    fn mask_bounds() {
        assert!(DescriptorMask::new(8).is_err());
        assert!(
            DescriptorMask::new(7)
                .unwrap()
                .contains(SourceFd::new(2).unwrap())
        );
        assert!(
            !DescriptorMask::new(7)
                .unwrap()
                .contains(SourceFd::new(3).unwrap())
        );
    }
}

impl std::fmt::Debug for SourceFd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.get().fmt(f)
    }
}
impl PartialEq for SourceFd {
    fn eq(&self, other: &Self) -> bool {
        self.get() == other.get()
    }
}
impl Eq for SourceFd {}
impl Ord for SourceFd {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.get().cmp(&other.get())
    }
}
impl PartialOrd for SourceFd {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl std::fmt::Debug for DescriptorMask {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.get().fmt(f)
    }
}
impl PartialEq for DescriptorMask {
    fn eq(&self, other: &Self) -> bool {
        self.get() == other.get()
    }
}
impl Eq for DescriptorMask {}

/// A producer endpoint remains owned until exactly one semantic-table handoff.
#[derive(Debug)]
pub struct EndpointLease(std::os::fd::OwnedFd);
impl EndpointLease {
    pub fn new(descriptor: std::os::fd::OwnedFd) -> Self {
        Self(descriptor)
    }
    pub fn transfer(self) -> std::os::fd::OwnedFd {
        self.0
    }
}
impl std::os::fd::AsFd for EndpointLease {
    fn as_fd(&self) -> std::os::fd::BorrowedFd<'_> {
        self.0.as_fd()
    }
}
