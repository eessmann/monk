// Expected E0382: cannot reuse transferred ownership
use std::os::fd::OwnedFd; pub fn bad(fd:OwnedFd){let transferred=fd;drop(fd);drop(transferred);}
fn main() {}
