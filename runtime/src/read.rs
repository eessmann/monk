//! Byte-at-a-time reads preserve the shared open-file-description offset.
use std::io;
use std::os::fd::BorrowedFd;
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Destination {
    Reply,
    Scalars(usize),
    Array,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Config {
    pub raw: bool,
    pub delimiter: u8,
    pub limit: Option<usize>,
    pub ifs: Vec<u8>,
    pub destination: Destination,
}
#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use std::os::fd::AsFd;
    fn config(destination: Destination) -> Config {
        Config {
            raw: false,
            delimiter: b'\n',
            limit: None,
            ifs: b" ,".to_vec(),
            destination,
        }
    }
    #[test]
    fn read_preserves_escaped_delimiters_and_shared_offset() {
        let mut file = tempfile::tempfile().unwrap();
        file.write_all(b" a\\,b, c\nnext\n").unwrap();
        use std::io::{Seek, SeekFrom};
        file.seek(SeekFrom::Start(0)).unwrap();
        assert_eq!(
            descriptor(&config(Destination::Array), file.as_fd()).unwrap(),
            (0, vec![b"a,b".to_vec(), b"c".to_vec()])
        );
        assert_eq!(
            descriptor(&config(Destination::Reply), file.as_fd()).unwrap(),
            (0, vec![b"next".to_vec()])
        );
    }
    #[test]
    fn read_limit_null_and_continuation_count() {
        let mut file = tempfile::tempfile().unwrap();
        file.write_all(b"a\0\\\nbc\n").unwrap();
        use std::io::{Seek, SeekFrom};
        file.seek(SeekFrom::Start(0)).unwrap();
        let mut cfg = config(Destination::Reply);
        cfg.limit = Some(2);
        assert_eq!(
            descriptor(&cfg, file.as_fd()).unwrap(),
            (0, vec![b"ab".to_vec()])
        );
        cfg.limit = None;
        assert_eq!(
            descriptor(&cfg, file.as_fd()).unwrap(),
            (0, vec![b"c".to_vec()])
        );
        assert_eq!(descriptor(&cfg, file.as_fd()).unwrap(), (1, vec![vec![]]));
    }
    #[test]
    fn read_final_scalar_preserves_internal_separators() {
        let cfg = config(Destination::Scalars(2));
        let data: Vec<_> = b" a, b,,c, ".iter().map(|&c| (c, false)).collect();
        assert_eq!(fields(&cfg, &data), vec![b"a".to_vec(), b"b,,c,".to_vec()]);
    }
}

#[cfg(test)]
fn descriptor(config: &Config, descriptor: BorrowedFd<'_>) -> io::Result<(i32, Vec<Vec<u8>>)> {
    descriptor_interruptible(config, descriptor, || Ok(()))
}
/// Consult the owner after EINTR without dropping bytes or escaped-byte state.
pub fn descriptor_interruptible(
    config: &Config,
    descriptor: BorrowedFd<'_>,
    check: impl FnMut() -> io::Result<()>,
) -> io::Result<(i32, Vec<Vec<u8>>)> {
    gather(
        config,
        || {
            let mut byte = [0];
            rustix::io::read(descriptor, &mut byte)
                .map(|count| if count == 0 { None } else { Some(byte[0]) })
                .map_err(io::Error::from)
        },
        check,
    )
}
fn gather(
    config: &Config,
    mut read: impl FnMut() -> io::Result<Option<u8>>,
    mut check: impl FnMut() -> io::Result<()>,
) -> io::Result<(i32, Vec<Vec<u8>>)> {
    let mut escaped = false;
    let mut bytes = Vec::new();
    let status = loop {
        if config.limit.is_some_and(|limit| bytes.len() >= limit) {
            break 0;
        }
        let byte = match read() {
            Ok(Some(byte)) => byte,
            Ok(None) => break 1,
            Err(error) if error.kind() == io::ErrorKind::Interrupted => {
                check()?;
                continue;
            }
            Err(error) => return Err(error),
        };
        if byte == 0 && config.delimiter != 0 {
            continue;
        }
        if escaped && byte == b'\n' {
            escaped = false;
            continue;
        }
        if escaped {
            escaped = false;
            bytes.push((byte, true));
        } else if byte == config.delimiter {
            break 0;
        } else if byte == b'\\' && !config.raw {
            escaped = true;
        } else {
            bytes.push((byte, false));
        }
    };
    Ok((status, fields(config, &bytes)))
}
type ReadByte = (u8, bool);
fn separator(config: &Config, item: &ReadByte) -> bool {
    !item.1 && config.ifs.contains(&item.0)
}
fn whitespace(config: &Config, item: &ReadByte) -> bool {
    separator(config, item) && matches!(item.0, 32 | 9 | 10)
}
fn trim_start<'a>(config: &Config, bytes: &'a [ReadByte]) -> &'a [ReadByte] {
    let len = bytes.iter().take_while(|x| whitespace(config, x)).count();
    &bytes[len..]
}
fn next_field<'a>(
    config: &Config,
    input: &'a [ReadByte],
) -> Option<(&'a [ReadByte], &'a [ReadByte])> {
    let remaining = trim_start(config, input);
    if remaining.is_empty() {
        return None;
    }
    let n = remaining
        .iter()
        .position(|x| separator(config, x))
        .unwrap_or(remaining.len());
    let first = &remaining[..n];
    let rest = &remaining[n..];
    let after = if let Some((item, following)) = rest.split_first() {
        if whitespace(config, item) {
            let suffix = trim_start(config, following);
            if suffix.first().is_some_and(|x| separator(config, x)) {
                trim_start(config, &suffix[1..])
            } else {
                suffix
            }
        } else {
            trim_start(config, following)
        }
    } else {
        rest
    };
    Some((first, after))
}
fn fields(config: &Config, bytes: &[ReadByte]) -> Vec<Vec<u8>> {
    let value = |bytes: &[ReadByte]| bytes.iter().map(|x| x.0).collect::<Vec<u8>>();
    match config.destination {
        Destination::Reply => vec![value(bytes)],
        Destination::Array => {
            let mut out = Vec::new();
            let mut rest = bytes;
            while let Some((first, after)) = next_field(config, rest) {
                out.push(value(first));
                rest = after;
            }
            out
        }
        Destination::Scalars(count) => {
            let mut out = Vec::with_capacity(count);
            let mut rest = bytes;
            for i in 0..count {
                if let Some((first, after)) = next_field(config, rest) {
                    if i + 1 == count && next_field(config, after).is_some() {
                        let start = trim_start(config, rest);
                        let end = start.len()
                            - start
                                .iter()
                                .rev()
                                .take_while(|x| whitespace(config, x))
                                .count();
                        out.push(value(&start[..end]));
                    } else {
                        out.push(value(first));
                    }
                    rest = after;
                } else {
                    out.resize(count, Vec::new());
                    break;
                }
            }
            out
        }
    }
}

#[cfg(test)]
mod interrupt_tests {
    use super::*;
    #[test]
    fn interruption_callback_keeps_partial_record_and_escape_state() {
        let cfg = Config {
            raw: false,
            delimiter: b'\n',
            limit: None,
            ifs: b" ".to_vec(),
            destination: Destination::Reply,
        };
        let mut events = vec![
            Ok(Some(b'a')),
            Ok(Some(b'\\')),
            Err(io::Error::from(io::ErrorKind::Interrupted)),
            Ok(Some(b' ')),
            Ok(Some(b'b')),
            Ok(Some(b'\n')),
        ]
        .into_iter();
        let mut interrupts = 0;
        let result = gather(
            &cfg,
            || events.next().unwrap(),
            || {
                interrupts += 1;
                Ok(())
            },
        )
        .unwrap();
        assert_eq!(result, (0, vec![b"a b".to_vec()]));
        assert_eq!(interrupts, 1);
    }
    #[test]
    fn terminating_interruption_reaches_owner() {
        let cfg = Config {
            raw: true,
            delimiter: b'\n',
            limit: None,
            ifs: vec![],
            destination: Destination::Reply,
        };
        let result = gather(
            &cfg,
            || Err(io::Error::from(io::ErrorKind::Interrupted)),
            || Err(io::Error::from(io::ErrorKind::Interrupted)),
        );
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::Interrupted);
    }
}
