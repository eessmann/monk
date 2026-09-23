use super::*;
fn frames(values: &[&[u8]]) -> Vec<Vec<u8>> {
    values.iter().map(|x| x.to_vec()).collect()
}
#[test]
fn integer_wrapping_bases_and_batch() {
    assert_eq!(integer::parse_number(b"18446744073709551615"), Ok(-1));
    assert_eq!(integer::parse_number(b"64#_"), Ok(63));
    assert_eq!(integer::parse_number(b"37#A"), Ok(36));
    assert_eq!(
        integer::parse_number(b"09"),
        Err(b"invalid-number".to_vec())
    );
    assert_eq!(
        integer::integer_value(b"div", &frames(&[b"-9223372036854775808", b"-1"])),
        Ok(i64::MIN)
    );
    assert_eq!(
        integer::integer_value(b"shr", &frames(&[b"-4", b"65"])),
        Ok(-2)
    );
    assert_eq!(
        integer::integer_value(
            b"batch",
            &frames(&[b"push", b"2", b"push", b"3", b"mul", b"neg"])
        ),
        Ok(-6)
    );
    assert_eq!(
        integer::integer_value(b"batch", &frames(&[b"push", b"2", b"push", b"3", b"pow"])),
        Err(b"invalid-batch".to_vec())
    );
}
#[test]
fn integer_error_transport_is_operation_specific() {
    assert_eq!(
        dispatch("integer", &frames(&[b"div", b"1", b"0"])),
        Ok(Some(Output::Bytes(
            b"error\n-\ndivision-by-zero\n".to_vec()
        )))
    );
    assert_eq!(
        dispatch("integer", &frames(&[b"batch", b"push", b"09"])),
        Err(b"invalid-number".to_vec())
    );
}
#[test]
fn fields_preserve_only_nonwhite_delimiter_empties() {
    assert_eq!(
        fields::split_fields(b" ,", b"  ,a, ,b, "),
        frames(&[b"", b"a", b"", b"b"])
    );
    assert_eq!(
        fields::split_fields(b"", &[0xff, b' ', 0]),
        vec![vec![0xff, b' ', 0]]
    );
    assert_eq!(
        fields::argv_fields(&frames(&[b"p", b"s", b"1"])),
        Ok(frames(&[b"ps"]))
    );
    assert_eq!(
        fields::argv_fields(&frames(&[b"p", b"s", b"0", b"a", b"b"])),
        Ok(frames(&[b"pa", b"bs"]))
    );
}
#[test]
fn echo_escapes_are_raw_bytes_and_stop_is_lexical() {
    assert_eq!(
        fields::echo_bytes(&frames(&[b"-ne", br"\0101\xff\u0042\u03b1\cignored"])),
        b"A\xffB\\u03B1"
    );
    assert_eq!(fields::echo_bytes(&frames(&[b"-e", br"\\c"])), b"\\c\n");
    assert_eq!(fields::echo_bytes(&frames(&[b"-eE", br"\n"])), b"\\n\n");
}
#[test]
fn printf_repeats_and_preserves_bytes() {
    assert_eq!(
        printf::printf_bytes(&frames(&[b"%s:%d\\n", &[0xff], b"-2", b"x"])),
        Ok(b"\xff:-2\nx:0\n".to_vec())
    );
    assert_eq!(
        printf::printf_bytes(&frames(&[br"\777\xff%%"])),
        Ok(vec![255, 255, b'%'])
    );
    assert_eq!(
        printf::printf_bytes(&frames(&[b"%d", b"01"])),
        Err(b"printf decimal operand is not canonical".to_vec())
    );
    assert_eq!(
        printf::printf_bytes(&frames(&[b"%x"])),
        Err(b"unsupported printf conversion".to_vec())
    );
}
#[test]
fn pattern_classes_fragment_quoting_and_byte_matching() {
    assert!(pattern::matches(&[0xff], &[(true, b"?".to_vec())]));
    assert!(pattern::matches(
        b"A9_",
        &[(true, b"[[:upper:]][[:digit:]][[:word:]]".to_vec())]
    ));
    assert!(pattern::matches(
        b"*ab",
        &[(false, b"*".to_vec()), (true, b"a?".to_vec())]
    ));
    assert!(pattern::matches(
        b"-",
        &[
            (true, b"[a".to_vec()),
            (false, b"-".to_vec()),
            (true, b"z]".to_vec())
        ]
    ));
    assert!(!pattern::matches(
        b"m",
        &[
            (true, b"[a".to_vec()),
            (false, b"-".to_vec()),
            (true, b"z]".to_vec())
        ]
    ));
    assert!(!pattern::matches(
        b"x",
        &[(true, b"[!a-[:digit:]]".to_vec())]
    ));
    assert_eq!(
        pattern::trim_pattern_parts(true, false, b"abcabc", &[(true, b"a*c".to_vec())]),
        b"abc"
    );
    assert_eq!(
        pattern::trim_pattern_parts(false, true, b"abcabc", &[(true, b"a*c".to_vec())]),
        b""
    );
    assert_eq!(
        pattern::replace_literal(true, b"aaaa", b"aa", b"&\\"),
        b"&\\&\\"
    );
}
#[test]
fn expansion_preserves_quoted_empty_positions() {
    assert_eq!(
        expansion::expand_words(&frames(&[b" ", b"e", b"a ", b"q", b"", b"e", b" b"])),
        Ok(frames(&[b"a", b"", b"b"]))
    );
    assert_eq!(
        expansion::expand_words(&frames(&[b",", b"l", b"x,", b"e", b"a,b", b"q", b"*"])),
        Ok(frames(&[b"x,a", b"b*"]))
    );
    assert_eq!(
        expansion::expand_words(&frames(&[b" ", b"e", b""])),
        Ok(vec![])
    );
    assert_eq!(
        expansion::expand_words(&frames(&[b" ", b"q", b""])),
        Ok(frames(&[b""]))
    );
}
#[test]
fn printf_noncanonical_read_syntax_has_legacy_error() {
    for value in [
        b"0x10".as_slice(),
        b"0o10",
        b"0b10",
        b"\xa01\xa0",
        b"-\xa01",
        b"9223372036854775808",
    ] {
        assert_eq!(
            printf::printf_bytes(&frames(&[b"%d", value])),
            Err(b"printf decimal operand is not canonical".to_vec())
        );
    }
    for value in [b"+1".as_slice(), b"1e0", b"0b102", b"\x851", b"- (1)"] {
        assert_eq!(
            printf::printf_bytes(&frames(&[b"%d", value])),
            Err(b"printf decimal operand is not signed-64 data".to_vec())
        );
    }
}
#[test]
#[cfg(not(miri))]
fn independent_bash53_arithmetic_and_byte_oracles() {
    use std::io::Write;
    use std::process::{Command, Stdio};
    fn bash(script: &str, args: &[Vec<u8>]) -> std::process::Output {
        let mut child =
            Command::new(std::env::var_os("MONK_REFERENCE_BASH").unwrap_or_else(|| "bash".into()))
                .args(["--noprofile", "--norc", "-c", script])
                .env("LC_ALL", "C")
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .expect("Bash is required for runtime semantic conformance");
        {
            let mut input = child.stdin.take().unwrap();
            for arg in args {
                input.write_all(arg).unwrap();
                input.write_all(&[0]).unwrap();
            }
        }
        child.wait_with_output().unwrap()
    }
    for (op, a, b, expression) in [
        (
            b"add".as_slice(),
            b"9223372036854775807".as_slice(),
            b"1".as_slice(),
            "9223372036854775807 + 1",
        ),
        (
            b"div",
            b"-9223372036854775808",
            b"-1",
            "(-9223372036854775807 - 1) / -1",
        ),
        (b"shr", b"-4", b"65", "-4 >> 65"),
        (b"pow", b"3", b"45", "3 ** 45"),
    ] {
        let actual = integer::integer_value(op, &frames(&[a, b]))
            .unwrap()
            .to_string()
            .into_bytes();
        let oracle = bash(&format!("printf '%s' \"$(( {expression} ))\""), &[]);
        assert!(oracle.status.success());
        assert_eq!(actual, oracle.stdout);
        assert!(oracle.stderr.is_empty());
    }
    for args in [
        frames(&[b"-e", br"a\0b\xff\cignored"]),
        frames(&[b"-ne", br"\u03b1\Uffffffff"]),
        frames(&[b"-eE", br"a\nb"]),
    ] {
        let oracle = bash("mapfile -d '' -t args; echo \"${args[@]}\"", &args);
        assert!(oracle.status.success());
        assert_eq!(fields::echo_bytes(&args), oracle.stdout);
        assert!(oracle.stderr.is_empty());
    }
    for args in [
        frames(&[b"%s:%d\\n", b"\xff", b"-2", b"x"]),
        frames(&[br"\777\xff%%"]),
    ] {
        let oracle = bash("mapfile -d '' -t args; printf \"${args[@]}\"", &args);
        assert!(oracle.status.success());
        assert_eq!(printf::printf_bytes(&args).unwrap(), oracle.stdout);
        assert!(oracle.stderr.is_empty());
    }
    for args in [
        frames(&[b" ,", b"  ,a, ,b, "]),
        frames(&[b"", b"\xff space"]),
    ] {
        let oracle = bash(
            "mapfile -d '' -t args; IFS=${args[0]}; value=${args[1]}; set -- $value; if (( $# )); then printf '%s\\0' \"$@\"; fi",
            &args,
        );
        assert!(oracle.status.success());
        let actual: Vec<u8> = fields::split_fields(&args[0], &args[1])
            .into_iter()
            .flat_map(|mut value| {
                value.push(0);
                value
            })
            .collect();
        assert_eq!(actual, oracle.stdout);
        assert!(oracle.stderr.is_empty());
    }
}
