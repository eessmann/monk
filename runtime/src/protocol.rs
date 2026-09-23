//! ABI-2 raw byte framing. Shell values cannot contain NUL; no UTF-8 decoding occurs.
pub type Bytes = Vec<u8>;
pub type Frames = Vec<Bytes>;
pub fn decode(bytes: &[u8]) -> Result<Frames, Bytes> {
    if bytes.is_empty() {
        return Ok(Vec::new());
    }
    if bytes.last() != Some(&0) {
        return Err(b"unterminated frame".to_vec());
    }
    Ok(bytes[..bytes.len() - 1]
        .split(|b| *b == 0)
        .map(<[u8]>::to_vec)
        .collect())
}
pub fn encode(frames: &[Bytes]) -> Bytes {
    let mut bytes = Vec::new();
    for frame in frames {
        bytes.extend_from_slice(frame);
        bytes.push(0);
    }
    bytes
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn empty_stream_is_not_an_empty_value() {
        assert_eq!(decode(b""), Ok(vec![]));
        assert_eq!(decode(b"\0"), Ok(vec![vec![]]));
    }
    #[test]
    fn arbitrary_bytes_and_empty_fields_round_trip() {
        let v = vec![vec![], vec![255, 10, 1], vec![]];
        assert_eq!(decode(&encode(&v)), Ok(v));
    }
    #[test]
    fn truncated_frame_is_rejected() {
        assert!(decode(b"a\0b").is_err());
    }
}
