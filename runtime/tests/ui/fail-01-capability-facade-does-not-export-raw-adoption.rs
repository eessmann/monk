// Expected E0425: capability facade does not export raw adoption
pub fn bad(){let _=monk_runtime::capabilities::adopt_initial_inherited(&[3]);}
fn main() {}
