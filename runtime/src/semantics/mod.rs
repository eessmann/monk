use crate::abi2::opcode::{cli as cli_opcode, pattern as pattern_opcode};
pub mod expansion;
pub mod fields;
pub mod integer;
pub mod pattern;
pub mod printf;
#[derive(Debug, PartialEq, Eq)]
pub enum Output {
    Bytes(Vec<u8>),
    Frames(Vec<Vec<u8>>),
    Status(i32),
}
#[cfg(test)]
mod tests;

/// None leaves the operation for the syscall/session dispatcher. Malformed
/// frames for known operations fail without silently becoming another command.
pub fn dispatch(op: &str, frames: &[Vec<u8>]) -> Result<Option<Output>, Vec<u8>> {
    let result = match op.as_bytes() {
        cli_opcode::INTEGER => Output::Bytes(integer::integer_operation(frames)?),
        cli_opcode::SPLIT => {
            let [ifs, value] = frames else {
                return Err(b"unknown operation or invalid frame count".to_vec());
            };
            Output::Frames(fields::split_fields(ifs, value))
        }
        cli_opcode::ARGV => Output::Frames(fields::argv_fields(frames)?),
        cli_opcode::ECHO => Output::Bytes(fields::echo_bytes(frames)),
        cli_opcode::PRINTF => Output::Bytes(printf::printf_bytes(frames)?),
        cli_opcode::EXPANSION => Output::Frames(expansion::expand_words(frames)?),
        cli_opcode::GLOB => Output::Frames(pattern::glob_paths(&pattern::pattern_parts(frames)?)),
        cli_opcode::PATTERN | cli_opcode::PATTERN_PARTS => {
            let [operation, subject, rest @ ..] = frames else {
                return Err(b"unknown operation or invalid frame count".to_vec());
            };
            match operation.as_slice() {
                pattern_opcode::MATCH if op.as_bytes() == cli_opcode::PATTERN => Output::Status(
                    if pattern::matches(subject, &pattern::pattern_parts(rest)?) {
                        0
                    } else {
                        1
                    },
                ),
                pattern_opcode::TRIM_PREFIX_SHORT
                | pattern_opcode::TRIM_PREFIX_LONG
                | pattern_opcode::TRIM_SUFFIX_SHORT
                | pattern_opcode::TRIM_SUFFIX_LONG => {
                    let parts = if op.as_bytes() == cli_opcode::PATTERN_PARTS {
                        pattern::pattern_parts(rest)?
                    } else {
                        let [bytes] = rest else {
                            return Err(b"unknown operation or invalid frame count".to_vec());
                        };
                        vec![(true, bytes.clone())]
                    };
                    Output::Frames(vec![pattern::trim_pattern_parts(
                        operation.starts_with(b"trim-prefix"),
                        operation.ends_with(b"long"),
                        subject,
                        &parts,
                    )])
                }
                pattern_opcode::REPLACE_FIRST | pattern_opcode::REPLACE_ALL
                    if op.as_bytes() == cli_opcode::PATTERN =>
                {
                    let [needle, replacement] = rest else {
                        return Err(b"unknown operation or invalid frame count".to_vec());
                    };
                    Output::Frames(vec![pattern::replace_literal(
                        operation == pattern_opcode::REPLACE_ALL,
                        subject,
                        needle,
                        replacement,
                    )])
                }
                _ => return Err(b"unknown operation or invalid frame count".to_vec()),
            }
        }
        _ => return Ok(None),
    };
    Ok(Some(result))
}
