//! Compile real downstream ownership failures alongside a successful control.
#![cfg(not(miri))]
#[test]
fn downstream_ownership_and_bounded_scalar_guarantees() {
    let cases = trybuild::TestCases::new();
    cases.pass("tests/ui/pass-*.rs");
    cases.compile_fail("tests/ui/fail-*.rs");
}
