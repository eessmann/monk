//! bash53-i64 arithmetic: all arithmetic is explicitly wrapping, including parsing.
// This bounded scalar remains private to arithmetic; neither wire frames nor
// FFI structures expose experimental pattern-type layout.
use crate::abi2::opcode::integer as opcode;

type ShiftBits = std::pat::pattern_type!(u32 is 0..=63);
struct ShiftCount(ShiftBits);
impl ShiftCount {
    fn masked(value: i64) -> Self {
        let bits = (value & 63) as u32;
        // SAFETY: masking proves the complete validity range 0..=63.
        Self(unsafe { std::mem::transmute::<u32, ShiftBits>(bits) })
    }
    fn get(self) -> u32 {
        // SAFETY: the pattern type has the u32 representation; all its values
        // are valid u32 values, and only masked() constructs this wrapper.
        unsafe { std::mem::transmute::<ShiftBits, u32>(self.0) }
    }
}

fn invalid_number<T>() -> Result<T, Vec<u8>> {
    Err(b"invalid-number".to_vec())
}

pub fn parse_number(input: &[u8]) -> Result<i64, Vec<u8>> {
    if input.is_empty() {
        return Ok(0);
    }
    let (negative, s) = match input[0] {
        b'-' => (true, &input[1..]),
        b'+' => (false, &input[1..]),
        _ => (false, input),
    };
    let (base, digits) = if let Some(at) = s.iter().position(|&c| c == b'#') {
        let prefix = &s[..at];
        if prefix.is_empty()
            || prefix.len() > 2
            || prefix[0] == b'0'
            || !prefix.iter().all(u8::is_ascii_digit)
        {
            return invalid_number();
        }
        (
            prefix
                .iter()
                .fold(0_u32, |n, c| n * 10 + u32::from(c - b'0')),
            &s[at + 1..],
        )
    } else if s.starts_with(b"0x") || s.starts_with(b"0X") {
        (16, &s[2..])
    } else if s.len() > 1 && s[0] == b'0' {
        (8, &s[1..])
    } else {
        (10, s)
    };
    if digits.is_empty() || !(2..=64).contains(&base) {
        return invalid_number();
    }
    let mut n = 0_i64;
    for &c in digits {
        let digit = match c {
            b'0'..=b'9' => u32::from(c - b'0'),
            b'a'..=b'z' => u32::from(c - b'a') + 10,
            b'A'..=b'Z' => u32::from(c - b'A') + if base <= 36 { 10 } else { 36 },
            b'@' => 62,
            b'_' => 63,
            _ => 65,
        };
        if digit >= base {
            return invalid_number();
        }
        n = n
            .wrapping_mul(i64::from(base))
            .wrapping_add(i64::from(digit));
    }
    Ok(if negative { n.wrapping_neg() } else { n })
}
fn unary(op: &[u8]) -> bool {
    matches!(
        op,
        opcode::READ | opcode::POS | opcode::NEG | opcode::NOT | opcode::INVERT
    )
}
fn binary(op: &[u8]) -> bool {
    matches!(
        op,
        opcode::ADD
            | opcode::SUB
            | opcode::MUL
            | opcode::DIV
            | opcode::REM
            | opcode::POW
            | opcode::SHL
            | opcode::SHR
            | opcode::LT
            | opcode::LE
            | opcode::GT
            | opcode::GE
            | opcode::EQ
            | opcode::NE
            | opcode::AND
            | opcode::XOR
            | opcode::OR
            | opcode::LOGICAL_AND
            | opcode::LOGICAL_OR
    )
}
fn evaluate(op: &[u8], args: &[i64]) -> Result<i64, Vec<u8>> {
    Ok(match (op, args) {
        (opcode::READ | opcode::POS, [a]) => *a,
        (opcode::NEG, [a]) => a.wrapping_neg(),
        (opcode::NOT, [a]) => i64::from(*a == 0),
        (opcode::INVERT, [a]) => !a,
        (opcode::ADD, [a, b]) => a.wrapping_add(*b),
        (opcode::SUB, [a, b]) => a.wrapping_sub(*b),
        (opcode::MUL, [a, b]) => a.wrapping_mul(*b),
        (opcode::DIV | opcode::REM, [_, 0]) => return Err(b"division-by-zero".to_vec()),
        (opcode::DIV, [a, b]) => a.wrapping_div(*b),
        (opcode::REM, [a, b]) => a.wrapping_rem(*b),
        (opcode::POW, [a, b]) => {
            if *b < 0 {
                return Err(b"negative-exponent".to_vec());
            }
            let (mut base, mut exponent, mut acc) = (*a, *b, 1_i64);
            while exponent != 0 {
                if exponent & 1 != 0 {
                    acc = acc.wrapping_mul(base);
                }
                base = base.wrapping_mul(base);
                exponent /= 2;
            }
            acc
        }
        (opcode::SHL, [a, b]) => a.wrapping_shl(ShiftCount::masked(*b).get()),
        (opcode::SHR, [a, b]) => a.wrapping_shr(ShiftCount::masked(*b).get()),
        (opcode::LT, [a, b]) => i64::from(a < b),
        (opcode::LE, [a, b]) => i64::from(a <= b),
        (opcode::GT, [a, b]) => i64::from(a > b),
        (opcode::GE, [a, b]) => i64::from(a >= b),
        (opcode::EQ, [a, b]) => i64::from(a == b),
        (opcode::NE, [a, b]) => i64::from(a != b),
        (opcode::AND, [a, b]) => a & b,
        (opcode::XOR, [a, b]) => a ^ b,
        (opcode::OR, [a, b]) => a | b,
        (opcode::LOGICAL_AND, [a, b]) => i64::from(*a != 0 && *b != 0),
        (opcode::LOGICAL_OR, [a, b]) => i64::from(*a != 0 || *b != 0),
        _ => return Err(b"unknown-primitive".to_vec()),
    })
}
pub fn integer_value(op: &[u8], args: &[Vec<u8>]) -> Result<i64, Vec<u8>> {
    if op != opcode::BATCH {
        return evaluate(
            op,
            &args
                .iter()
                .map(|a| parse_number(a))
                .collect::<Result<Vec<_>, _>>()?,
        );
    }
    let mut stack = Vec::new();
    let mut at = 0;
    while at < args.len() {
        let op = args[at].as_slice();
        at += 1;
        if op == opcode::PUSH && at < args.len() {
            stack.push(parse_number(&args[at])?);
            at += 1;
        } else if matches!(op, opcode::POS | opcode::NEG | opcode::NOT | opcode::INVERT)
            && !stack.is_empty()
        {
            let index = stack.len() - 1;
            stack[index] = evaluate(op, &stack[index..])?;
        } else if matches!(
            op,
            opcode::ADD
                | opcode::SUB
                | opcode::MUL
                | opcode::SHL
                | opcode::SHR
                | opcode::LT
                | opcode::LE
                | opcode::GT
                | opcode::GE
                | opcode::EQ
                | opcode::NE
                | opcode::AND
                | opcode::XOR
                | opcode::OR
        ) && stack.len() >= 2
        {
            let index = stack.len() - 2;
            let result = evaluate(op, &stack[index..])?;
            stack.truncate(index);
            stack.push(result);
        } else {
            return Err(b"invalid-batch".to_vec());
        }
    }
    if stack.len() == 1 {
        Ok(stack[0])
    } else {
        Err(b"invalid-batch".to_vec())
    }
}
pub fn integer_operation(frames: &[Vec<u8>]) -> Result<Vec<u8>, Vec<u8>> {
    let Some((op, args)) = frames.split_first() else {
        return Err(b"invalid integer operation or arity".to_vec());
    };
    if op == opcode::BATCH {
        return Ok(format!("ok\n{}\n-\n", integer_value(op, args)?).into_bytes());
    }
    if !(unary(op) && args.len() == 1 || binary(op) && args.len() == 2) {
        return Err(b"invalid integer operation or arity".to_vec());
    }
    Ok(match integer_value(op, args) {
        Ok(value) => format!("ok\n{value}\n-\n").into_bytes(),
        Err(reason) => [b"error\n-\n".as_slice(), &reason, b"\n"].concat(),
    })
}

#[cfg(test)]
mod shift_tests {
    use super::*;
    #[test]
    fn masked_shift_count_covers_boundary_values() {
        for input in [i64::MIN, -65, -64, -1, 0, 1, 63, 64, 65, i64::MAX] {
            assert_eq!(ShiftCount::masked(input).get(), (input & 63) as u32);
        }
    }
}
