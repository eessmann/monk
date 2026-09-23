// Expected E0382: running child is consumed by completion
use monk_runtime::capabilities::RunningChild; pub fn bad(child:RunningChild){let _=child.complete();let _=child.complete();}
fn main() {}
