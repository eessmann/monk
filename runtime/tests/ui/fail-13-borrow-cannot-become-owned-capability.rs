// Expected E0308: borrow cannot become owned capability
use std::os::fd::{BorrowedFd,OwnedFd}; pub fn bad(fd:BorrowedFd<'_>)->OwnedFd{fd}
fn main() {}
