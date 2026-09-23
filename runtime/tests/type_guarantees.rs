//! Compile real downstream consumers: rejection without a successful positive
//! control could otherwise be a missing-library/toolchain false positive.
#![cfg(not(miri))]
use std::process::Command;

#[test]
fn downstream_ownership_and_bounded_scalar_guarantees() {
    // Ask Cargo to select a coherent dependency graph. Selecting the newest
    // rmeta by timestamp can mix check/clippy/Miri metadata with a test build.
    let scratch = tempfile::tempdir().unwrap();
    let input = scratch.path().join("lib.rs");
    let manifest = scratch.path().join("Cargo.toml");
    let runtime = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    std::fs::write(&manifest,format!(
        "[package]\nname=\"monk-type-consumer\"\nversion=\"0.0.0\"\nedition=\"2024\"\n[lib]\npath=\"lib.rs\"\n[dependencies]\nmonk-runtime={{path={:?}}}\n",
        runtime.to_string_lossy()
    )).unwrap();
    let compile = |source: &str| {
        std::fs::write(&input, source).unwrap();
        Command::new(env!("CARGO"))
            .current_dir(scratch.path())
            .args(["check", "--offline", "--lib", "--quiet", "--manifest-path"])
            .arg(&manifest)
            .env("CARGO_TARGET_DIR", scratch.path().join("target"))
            .output()
            .expect("Cargo for downstream capability checks")
    };
    let positive = r#"
use monk_runtime::capabilities::{SourceFd,DescriptorMask};
use std::os::fd::{AsFd,AsRawFd,OwnedFd};
pub fn cwd_lifecycle(fd:OwnedFd)->std::io::Result<OwnedFd>{let cwd=monk_runtime::capabilities::WorkingDirectory::from_owned(fd)?;let _=monk_runtime::capabilities::PreparedLaunch::prepare(&monk_runtime::capabilities::Streams::new(),Some(cwd.borrow()),&[],b"/bin/true",&[],false)?;Ok(cwd.into_owned())}
pub fn lifecycle(plan:monk_runtime::capabilities::PreparedLaunch)->std::io::Result<monk_runtime::capabilities::ProcessOutcome>{let running=plan.launch()?;let completed=running.complete()?;Ok(completed.outcome())}
pub fn transfer(fd:OwnedFd)->OwnedFd{let lease=monk_runtime::capabilities::EndpointLease::new(fd);let _=lease.as_fd();lease.transfer()}
pub fn good(fd:OwnedFd){let number=SourceFd::new(2).unwrap();let mask=DescriptorMask::new(7).unwrap();assert!(mask.contains(number));{let borrowed=fd.as_fd();let _=borrowed.as_raw_fd();}let transferred=fd;drop(transferred);}
"#;
    let result = compile(positive);
    assert!(
        result.status.success(),
        "positive control did not compile: {}",
        String::from_utf8_lossy(&result.stderr)
    );
    let cases = [
        (
            "native module is inaccessible",
            "pub fn bad(){let _=monk_runtime::native::adopt_inherited(3);}",
            "E0603",
        ),
        (
            "capability facade does not export raw adoption",
            "pub fn bad(){let _=monk_runtime::capabilities::adopt_inherited(3);}",
            "E0425",
        ),
        (
            "capability facade does not export signal authority",
            "pub fn bad(){let _=monk_runtime::capabilities::SignalGuard::install();}",
            "E0433",
        ),
        (
            "raw descriptor cannot stand in for cwd capability",
            "use monk_runtime::capabilities::{PreparedLaunch,Streams}; use std::os::fd::BorrowedFd; pub fn bad(fd:BorrowedFd<'_>){let _=PreparedLaunch::prepare(&Streams::new(),Some(fd),&[],b\"/bin/true\",&[],false);}",
            "E0308",
        ),
        (
            "directory borrow cannot outlive owned directory",
            "use monk_runtime::capabilities::WorkingDirectory; pub fn bad(cwd:WorkingDirectory){let borrowed=cwd.borrow();drop(cwd);let _=borrowed;}",
            "E0505",
        ),
        (
            "prepared launch is consumed",
            "use monk_runtime::capabilities::PreparedLaunch; pub fn bad(plan:PreparedLaunch){let _=plan.launch();let _=plan.launch();}",
            "E0382",
        ),
        (
            "running child is consumed by completion",
            "use monk_runtime::capabilities::RunningChild; pub fn bad(child:RunningChild){let _=child.complete();let _=child.complete();}",
            "E0382",
        ),
        (
            "completed child cannot wait again",
            "use monk_runtime::capabilities::CompletedChild; pub fn bad(mut child:CompletedChild){let _=child.wait();}",
            "E0599",
        ),
        (
            "endpoint lease cannot be reused after transfer",
            "use monk_runtime::capabilities::EndpointLease; pub fn bad(lease:EndpointLease){let _=lease.transfer();let _=lease.transfer();}",
            "E0382",
        ),
        (
            "cannot forge source descriptor",
            "use monk_runtime::capabilities::SourceFd; pub fn bad(){let _=SourceFd(0);}",
            "E0423",
        ),
        (
            "cannot forge descriptor mask",
            "use monk_runtime::capabilities::DescriptorMask; pub fn bad(){let _=DescriptorMask(0);}",
            "E0423",
        ),
        (
            "cannot reuse transferred ownership",
            "use std::os::fd::OwnedFd; pub fn bad(fd:OwnedFd){let transferred=fd;drop(fd);drop(transferred);}",
            "E0382",
        ),
        (
            "borrow cannot outlive owner",
            "use std::os::fd::{AsFd,AsRawFd,OwnedFd}; pub fn bad(fd:OwnedFd){let borrowed=fd.as_fd();drop(fd);let _=borrowed.as_raw_fd();}",
            "E0505",
        ),
        (
            "borrow cannot become owned capability",
            "use std::os::fd::{BorrowedFd,OwnedFd}; pub fn bad(fd:BorrowedFd<'_>)->OwnedFd{fd}",
            "E0308",
        ),
    ];
    for (label, source, code) in cases {
        let result = compile(source);
        let diagnostic = String::from_utf8_lossy(&result.stderr);
        assert!(!result.status.success(), "{label} unexpectedly compiled");
        assert!(
            diagnostic.contains(code),
            "{label} failed for an unrelated reason: {diagnostic}"
        );
    }
}
