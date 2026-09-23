// Expected E0423: cannot forge descriptor mask
use monk_runtime::capabilities::DescriptorMask; pub fn bad(){let _=DescriptorMask(0);}
fn main() {}
