// Expected E0423: cannot forge source descriptor
use monk_runtime::capabilities::SourceFd; pub fn bad(){let _=SourceFd(0);}
fn main() {}
