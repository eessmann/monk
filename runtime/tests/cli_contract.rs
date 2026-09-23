//! CLI parsing must preserve ABI bytes, argument order and diagnostic boundaries.
use std::ffi::OsStr;
use std::io::Write;
use std::os::unix::ffi::OsStrExt;
use std::process::{Command, Output, Stdio};

fn execute(runtime: &OsStr, args: &[Vec<u8>], input: &[u8]) -> Output {
    let mut child = Command::new(runtime)
        .args(args.iter().map(|value| OsStr::from_bytes(value)))
        .env("LC_ALL", "C")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    if let Err(error) = child.stdin.take().unwrap().write_all(input) {
        assert_eq!(error.kind(), std::io::ErrorKind::BrokenPipe);
    }
    child.wait_with_output().unwrap()
}
fn words(args: &[&[u8]]) -> Vec<Vec<u8>> {
    args.iter().map(|arg| arg.to_vec()).collect()
}
fn runtime() -> &'static OsStr {
    OsStr::new(env!("CARGO_BIN_EXE_monk-runtime"))
}
fn malformed_headers() -> Vec<Vec<Vec<u8>>> {
    vec![
        words(&[]),
        words(&[b"--help"]),
        words(&[b"--"]),
        words(&[b"--version"]),
        words(&[b"--describe", b"extra"]),
        words(&[b"--describe", b"--"]),
        words(&[b"--describe", b"--abi", b"2"]),
        words(&[b"--abi"]),
        words(&[b"--abi", b"2"]),
        words(&[b"--abi=2", b"echo"]),
        words(&[b"--abi", b"02", b"echo"]),
        words(&[b"--abi", b"3", b"echo"]),
        words(&[b"--abi", b"\xff", b"echo"]),
        words(&[b"echo", b"--abi", b"2"]),
        words(&[b"--", b"--abi", b"2", b"echo"]),
        words(&[b"\xff", b"2", b"echo"]),
    ]
}
#[test]
fn malformed_headers_preserve_abi_status_and_diagnostic() {
    for args in malformed_headers() {
        let actual = execute(runtime(), &args, b"");
        assert_eq!(actual.status.code(), Some(125), "{args:?}");
        assert!(actual.stdout.is_empty(), "{args:?}");
        assert_eq!(
            actual.stderr, b"monk-runtime: expected --abi 2 OPERATION or --describe\n",
            "{args:?}"
        );
    }
}
#[test]
fn execution_arguments_are_opaque_bytes_including_flag_spellings() {
    let operands = words(&[
        b"--",
        b"--help",
        b"--describe",
        b"--abi=2",
        b"",
        b"\xff\n",
        b"-x",
        b"--abi",
        b"2",
        b"two words",
    ]);
    let mut args = words(&[
        b"--abi",
        b"2",
        b"exec-site",
        b"origin",
        b"1",
        b"/usr/bin/printf",
        b"%s\\0",
    ]);
    args.extend(operands.clone());
    let actual = execute(runtime(), &args, b"");
    let expected: Vec<_> = operands
        .into_iter()
        .flat_map(|arg| arg.into_iter().chain([0]))
        .collect();
    assert!(actual.status.success());
    assert_eq!(actual.stdout, expected);
    assert!(actual.stderr.is_empty());
}
#[test]
#[ignore = "requires the frozen pre-migration executable"]
fn cli_frames_and_errors_match_frozen_haskell() {
    let baseline = std::env::var_os("MONK_BASELINE_RUNTIME")
        .expect("set MONK_BASELINE_RUNTIME to the frozen Haskell executable");
    let mut cases = malformed_headers();
    cases.push(words(&[b"--describe"]));
    for operation in [
        b"unknown".as_slice(),
        b"",
        b"\xff",
        b"--",
        b"--describe",
        b"-x",
        b"echo",
        b"argv",
        b"integer",
        b"session-client",
        b"launch",
        b"exec-site",
        b"raise-signal",
    ] {
        let mut args = words(&[b"--abi", b"2", operation]);
        cases.push(args.clone());
        args.push(b"--".to_vec());
        cases.push(args.clone());
        args.push(b"\xff".to_vec());
        cases.push(args);
    }
    for args in cases {
        for input in [b"".as_slice(), b"\0", b"\xff", b"a\0\0b\0"] {
            let expected = execute(&baseline, &args, input);
            let actual = execute(runtime(), &args, input);
            assert_eq!(
                (actual.status, actual.stdout, actual.stderr),
                (expected.status, expected.stdout, expected.stderr),
                "args={args:?}, input={input:?}"
            );
        }
    }
}
