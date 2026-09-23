// Expected E0603: native module is inaccessible
pub fn bad(){let _=monk_runtime::native::adopt_initial_inherited(&[3]);}
fn main() {}
