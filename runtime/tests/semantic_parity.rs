//! Replay byte-level migration comparisons against a frozen pre-migration
//! runtime: MONK_BASELINE_RUNTIME=/path/to/monk-runtime cargo test
//! --test semantic_parity -- --ignored
use monk_runtime::semantics::{self, Output};
use std::io::Write;
use std::process::{Command, Stdio};
struct Random(u64);
impl Random {
    fn next(&mut self) -> u64 {
        self.0 = self
            .0
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        self.0
    }
    fn bytes(&mut self, alphabet: &[u8], max: usize) -> Vec<u8> {
        let count = self.next() as usize % max;
        (0..count)
            .map(|_| alphabet[self.next() as usize % alphabet.len()])
            .collect()
    }
}
fn compare(operation: &str, frames: &[Vec<u8>]) {
    let baseline = std::env::var_os("MONK_BASELINE_RUNTIME")
        .expect("set MONK_BASELINE_RUNTIME to the frozen Haskell executable");
    let mut child = Command::new(baseline)
        .args(["--abi", "2", operation])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    {
        let mut input = child.stdin.take().unwrap();
        for frame in frames {
            input.write_all(frame).unwrap();
            input.write_all(&[0]).unwrap();
        }
    }
    let oracle = child.wait_with_output().unwrap();
    let actual = semantics::dispatch(operation, frames);
    let (status, output, error) = match actual {
        Ok(Some(Output::Bytes(bytes))) => (0, bytes, vec![]),
        Ok(Some(Output::Frames(frames))) => (
            0,
            frames
                .into_iter()
                .flat_map(|mut frame| {
                    frame.push(0);
                    frame
                })
                .collect(),
            vec![],
        ),
        Ok(Some(Output::Status(status))) => (status, vec![], vec![]),
        Err(reason) => (
            125,
            vec![],
            [b"monk-runtime: ".as_slice(), &reason, b"\n"].concat(),
        ),
        Ok(None) => panic!("operation not handled"),
    };
    assert_eq!(
        (status, output, error),
        (oracle.status.code().unwrap(), oracle.stdout, oracle.stderr),
        "operation={operation} frames={frames:?}"
    );
}
#[test]
#[ignore = "requires the frozen pre-migration executable"]
fn randomized_byte_semantics_match_frozen_haskell() {
    let mut random = Random(9232026);
    for _ in 0..250 {
        let operations = [
            b"read".as_slice(),
            b"neg",
            b"invert",
            b"add",
            b"div",
            b"rem",
            b"pow",
            b"shl",
            b"shr",
            b"logical-and",
        ];
        let operation = operations[random.next() as usize % operations.len()];
        let mut frames = vec![
            operation.to_vec(),
            (random.next() as i64).to_string().into_bytes(),
        ];
        if !matches!(operation, b"read" | b"neg" | b"invert") {
            frames.push((random.next() as i64).to_string().into_bytes());
        }
        compare("integer", &frames);
        compare(
            "split",
            &[
                random.bytes(b" \t\n,;:\xff", 6),
                random.bytes(b" \t\n,;:abc\xff", 40),
            ],
        );
        let subject = random.bytes(b"abc-[]?*\\\xff", 8);
        let mut parts = vec![];
        let count = random.next() % 4 + 1;
        for _ in 0..count {
            parts.push(if random.next().is_multiple_of(2) {
                b"0".to_vec()
            } else {
                b"1".to_vec()
            });
            parts.push(random.bytes(b"abc-[]?!^:*\\\xff", 9));
        }
        let mut frames = vec![b"match".to_vec(), subject.clone()];
        frames.extend(parts.clone());
        compare("pattern", &frames);
        let trims = [
            b"trim-prefix-short".as_slice(),
            b"trim-prefix-long",
            b"trim-suffix-short",
            b"trim-suffix-long",
        ];
        frames[0] = trims[random.next() as usize % trims.len()].to_vec();
        compare("pattern-parts", &frames);
        compare(
            "echo",
            &[
                b"-e".to_vec(),
                random.bytes(b"\\c0xuUabcdefABCDEF0123789\xff", 50),
            ],
        );
    }
    for spelling in [
        b"0x10".as_slice(),
        b"0o10",
        b"0b10",
        b"\xa01\xa0",
        b"-\xa01",
        b"9223372036854775808",
        b"18446744073709551616",
        b"+1",
        b"1e0",
        b"0b102",
        b"\x851",
        b"- (1)",
    ] {
        compare("printf", &[b"%d".to_vec(), spelling.to_vec()]);
    }
}
