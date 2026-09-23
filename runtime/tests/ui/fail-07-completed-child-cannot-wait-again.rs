// Expected E0599: completed child cannot wait again
use monk_runtime::capabilities::CompletedChild; pub fn bad(mut child:CompletedChild){let _=child.wait();}
fn main() {}
