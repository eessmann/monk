// Expected E0505: borrow cannot outlive owner
use std::os::fd::{AsFd,AsRawFd,OwnedFd}; pub fn bad(fd:OwnedFd){let borrowed=fd.as_fd();drop(fd);let _=borrowed.as_raw_fd();}
fn main() {}
