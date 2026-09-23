//! The normalized finite printf language, independent of locale and libc printf.
enum Piece {
    Bytes(Vec<u8>),
    String,
    Decimal,
}
fn digit(c: u8) -> Option<u16> {
    match c {
        b'0'..=b'9' => Some(u16::from(c - b'0')),
        b'a'..=b'f' => Some(u16::from(c - b'a' + 10)),
        b'A'..=b'F' => Some(u16::from(c - b'A' + 10)),
        _ => None,
    }
}
fn parse_format(format: &[u8]) -> Result<Vec<Piece>, Vec<u8>> {
    let mut pieces = Vec::new();
    let mut i = 0;
    while i < format.len() {
        let c = format[i];
        i += 1;
        if c == b'%' {
            let Some(&conversion) = format.get(i) else {
                return Err(b"unterminated printf conversion".to_vec());
            };
            i += 1;
            pieces.push(match conversion {
                b'%' => Piece::Bytes(vec![b'%']),
                b's' => Piece::String,
                b'd' => Piece::Decimal,
                _ => return Err(b"unsupported printf conversion".to_vec()),
            });
        } else if c == b'\\' {
            let Some(&escape) = format.get(i) else {
                return Err(b"unterminated printf escape".to_vec());
            };
            i += 1;
            if let Some(byte) = match escape {
                b'a' => Some(7),
                b'b' => Some(8),
                b'e' => Some(27),
                b'f' => Some(12),
                b'n' => Some(10),
                b'r' => Some(13),
                b't' => Some(9),
                b'v' => Some(11),
                b'\\' => Some(92),
                _ => None,
            } {
                pieces.push(Piece::Bytes(vec![byte]));
                continue;
            }
            let (base, count) = if (b'0'..=b'7').contains(&escape) {
                i -= 1;
                (8, 3)
            } else if escape == b'x' && format.get(i).is_some_and(|c| digit(*c).is_some()) {
                (16, 2)
            } else {
                return Err(b"unsupported printf escape".to_vec());
            };
            let start = i;
            let mut number = 0;
            while i < format.len() && i - start < count {
                let Some(n) = digit(format[i]).filter(|n| *n < base) else {
                    break;
                };
                number = number * base + n;
                i += 1;
            }
            pieces.push(Piece::Bytes(vec![number as u8]));
        } else {
            let start = i - 1;
            while i < format.len() && !matches!(format[i], b'%' | b'\\') {
                i += 1;
            }
            pieces.push(Piece::Bytes(format[start..i].to_vec()));
        }
    }
    Ok(pieces)
}
fn decimal(value: &[u8]) -> Result<Vec<u8>, Vec<u8>> {
    // ByteString.Char8's Read Int64 accepts Latin-1 NBSP as whitespace,
    // parentheses, and binary/octal/hex prefixes. Parse those spellings before
    // checking canonical decimal form to preserve the exact error distinction.
    fn trim(mut bytes: &[u8]) -> &[u8] {
        let space = |c: &u8| matches!(c, 9..=13 | 32 | 160);
        while bytes.first().is_some_and(space) {
            bytes = &bytes[1..];
        }
        while bytes.last().is_some_and(space) {
            bytes = &bytes[..bytes.len() - 1];
        }
        bytes
    }
    let mut parsed = trim(value);
    while parsed.starts_with(b"(") && parsed.ends_with(b")") {
        parsed = trim(&parsed[1..parsed.len() - 1]);
    }
    let negative = parsed.starts_with(b"-");
    if negative {
        parsed = trim(&parsed[1..]);
    }
    let (base, digits) = if parsed.starts_with(b"0x") || parsed.starts_with(b"0X") {
        (16, &parsed[2..])
    } else if parsed.starts_with(b"0o") || parsed.starts_with(b"0O") {
        (8, &parsed[2..])
    } else if parsed.starts_with(b"0b") || parsed.starts_with(b"0B") {
        (2, &parsed[2..])
    } else {
        (10, parsed)
    };
    if digits.is_empty() {
        return Err(b"printf decimal operand is not signed-64 data".to_vec());
    }
    let mut n = 0_i64;
    for &c in digits {
        let Some(d) = digit(c).filter(|d| *d < base) else {
            return Err(b"printf decimal operand is not signed-64 data".to_vec());
        };
        n = n.wrapping_mul(i64::from(base)).wrapping_add(i64::from(d));
    }
    if negative {
        n = n.wrapping_neg();
    }
    let canonical = n.to_string().into_bytes();
    if canonical != value {
        return Err(b"printf decimal operand is not canonical".to_vec());
    }
    Ok(canonical)
}
pub fn printf_bytes(args: &[Vec<u8>]) -> Result<Vec<u8>, Vec<u8>> {
    let mut args = args;
    while args.first().is_some_and(|a| a == b"--") {
        args = &args[1..];
    }
    let Some((format, values)) = args.split_first() else {
        return Err(b"printf needs an admitted format".to_vec());
    };
    let pieces = parse_format(format)?;
    let consumes = pieces.iter().any(|p| !matches!(p, Piece::Bytes(_)));
    let mut at = 0;
    let mut out = Vec::new();
    loop {
        for piece in &pieces {
            match piece {
                Piece::Bytes(bytes) => out.extend(bytes),
                Piece::String => {
                    if let Some(value) = values.get(at) {
                        out.extend(value);
                        at += 1;
                    }
                }
                Piece::Decimal => {
                    let value = values.get(at).map_or(b"0".as_slice(), |value| {
                        at += 1;
                        value.as_slice()
                    });
                    out.extend(decimal(value)?);
                }
            }
        }
        if !consumes || at == values.len() {
            break;
        }
    }
    Ok(out)
}
