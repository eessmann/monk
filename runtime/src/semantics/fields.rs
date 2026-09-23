//! Byte-only IFS splitting, positional argument composition, and echo rendering.
pub fn split_fields(ifs: &[u8], value: &[u8]) -> Vec<Vec<u8>> {
    let white = |c: &u8| ifs.contains(c) && matches!(c, 32 | 9 | 10);
    let mut rest = value;
    while rest.first().is_some_and(white) {
        rest = &rest[1..];
    }
    let mut result = Vec::new();
    loop {
        let n = rest
            .iter()
            .position(|c| ifs.contains(c))
            .unwrap_or(rest.len());
        let field = &rest[..n];
        if n == rest.len() {
            if !field.is_empty() {
                result.push(field.to_vec());
            }
            break;
        }
        let c = rest[n];
        let mut after = &rest[n + 1..];
        while after.first().is_some_and(white) {
            after = &after[1..];
        }
        let nonwhite = !white(&c) || after.first().is_some_and(|c| ifs.contains(c));
        if white(&c) && nonwhite {
            after = &after[1..];
            while after.first().is_some_and(white) {
                after = &after[1..];
            }
        }
        if !field.is_empty() || nonwhite {
            result.push(field.to_vec());
        }
        rest = after;
    }
    result
}
pub fn argv_fields(frames: &[Vec<u8>]) -> Result<Vec<Vec<u8>>, Vec<u8>> {
    let [prefix, suffix, force, values @ ..] = frames else {
        return Err(b"invalid argv frames".to_vec());
    };
    if force != b"0" && force != b"1" {
        return Err(b"invalid argv frames".to_vec());
    }
    if values.is_empty() {
        return Ok(if force == b"1" {
            vec![[prefix.as_slice(), suffix].concat()]
        } else {
            vec![]
        });
    }
    let mut out = values.to_vec();
    out[0] = [prefix.as_slice(), &out[0]].concat();
    let last = out.len() - 1;
    out[last].extend(suffix);
    Ok(out)
}
fn digit(c: u8, base: u64) -> Option<u64> {
    let n = match c {
        b'0'..=b'9' => u64::from(c - b'0'),
        b'a'..=b'f' => u64::from(c - b'a' + 10),
        b'A'..=b'F' => u64::from(c - b'A' + 10),
        _ => return None,
    };
    (n < base).then_some(n)
}
pub fn echo_bytes(args: &[Vec<u8>]) -> Vec<u8> {
    let (mut newline, mut escapes, mut at) = (true, false, 0);
    while at < args.len() {
        let a = &args[at];
        if a.len() <= 1 || a[0] != b'-' || !a[1..].iter().all(|c| matches!(c, b'n' | b'e' | b'E')) {
            break;
        }
        for c in &a[1..] {
            match c {
                b'n' => newline = false,
                b'e' => escapes = true,
                _ => escapes = false,
            }
        }
        at += 1;
    }
    let body = args[at..].join(&b' ');
    if !escapes {
        let mut out = body;
        if newline {
            out.push(b'\n');
        }
        return out;
    }
    // Bash's newline suppression recognizes a lexical \\c, even if a prior
    // numeric escape consumes bytes differently during rendering.
    let mut scan = 0;
    let mut stopped = false;
    while scan < body.len() {
        if body[scan] == b'\\' && scan + 1 < body.len() {
            if body[scan + 1] == b'c' {
                stopped = true;
                break;
            }
            scan += 2;
        } else {
            scan += 1;
        }
    }
    let mut out = Vec::new();
    let mut i = 0;
    while i < body.len() {
        if body[i] != b'\\' || i + 1 == body.len() {
            out.push(body[i]);
            i += 1;
            continue;
        }
        let c = body[i + 1];
        i += 2;
        if c == b'c' {
            break;
        }
        if let Some(v) = match c {
            b'a' => Some(7),
            b'b' => Some(8),
            b'e' | b'E' => Some(27),
            b'f' => Some(12),
            b'n' => Some(10),
            b'r' => Some(13),
            b't' => Some(9),
            b'v' => Some(11),
            b'\\' => Some(92),
            _ => None,
        } {
            out.push(v);
            continue;
        }
        let (count, base) = match c {
            b'0' => (3, 8),
            b'x' => (2, 16),
            b'u' => (4, 16),
            b'U' => (8, 16),
            _ => (0, 16),
        };
        let start = i;
        let mut number = 0_u64;
        while i < body.len() && i - start < count {
            let Some(d) = digit(body[i], base) else {
                break;
            };
            number = number * base + d;
            i += 1;
        }
        if start == i && c != b'0' {
            out.extend([b'\\', c]);
        } else if matches!(c, b'u' | b'U') {
            if number < 128 {
                out.push(number as u8);
            } else if number <= 0x7fff_ffff || cfg!(target_os = "macos") {
                out.extend(
                    if number <= 65535 {
                        format!("\\u{number:04X}")
                    } else {
                        format!("\\U{number:08X}")
                    }
                    .as_bytes(),
                );
            }
        } else {
            out.push(number as u8);
        }
    }
    if newline && !stopped {
        out.push(b'\n');
    }
    out
}
