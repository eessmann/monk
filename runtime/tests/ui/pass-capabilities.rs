
use monk_runtime::capabilities::{SourceFd,DescriptorMask};
use std::os::fd::{AsFd,AsRawFd,OwnedFd};
pub fn cwd_lifecycle(fd:OwnedFd)->std::io::Result<OwnedFd>{let cwd=monk_runtime::capabilities::WorkingDirectory::from_owned(fd)?;let _=monk_runtime::capabilities::PreparedLaunch::prepare(&monk_runtime::capabilities::Streams::new(),Some(cwd.borrow()),&[],b"/bin/true",&[],false)?;Ok(cwd.into_owned())}
pub fn lifecycle(plan:monk_runtime::capabilities::PreparedLaunch)->std::io::Result<monk_runtime::capabilities::ProcessOutcome>{let running=plan.launch()?;let completed=running.complete()?;Ok(completed.outcome())}
pub fn transfer(fd:OwnedFd)->OwnedFd{let lease=monk_runtime::capabilities::EndpointLease::new(fd);let _=lease.as_fd();lease.transfer()}
pub fn good(fd:OwnedFd){let number=SourceFd::new(2).unwrap();let mask=DescriptorMask::new(7).unwrap();assert!(mask.contains(number));{let borrowed=fd.as_fd();let _=borrowed.as_raw_fd();}let transferred=fd;drop(transferred);}

fn main() {}
