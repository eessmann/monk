// Expected E0505: directory borrow cannot outlive owned directory
use monk_runtime::capabilities::WorkingDirectory; pub fn bad(cwd:WorkingDirectory){let borrowed=cwd.borrow();drop(cwd);let _=borrowed;}
fn main() {}
