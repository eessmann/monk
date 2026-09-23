// Expected E0308: raw descriptor cannot stand in for cwd capability
use monk_runtime::capabilities::{PreparedLaunch,Streams}; use std::os::fd::BorrowedFd; pub fn bad(fd:BorrowedFd<'_>){let _=PreparedLaunch::prepare(&Streams::new(),Some(fd),&[],b"/bin/true",&[],false);}
fn main() {}
