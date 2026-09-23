// Expected E0433: capability facade does not export signal authority
pub fn bad(){let _=monk_runtime::capabilities::SignalGuard::install();}
fn main() {}
