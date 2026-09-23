//! Directory diagnostics and bounded lexical metadata; no chdir side effects.
use std::ffi::OsStr;
use std::io;
use std::os::unix::ffi::{OsStrExt, OsStringExt};
const MAX_PATH: usize = if cfg!(target_os = "macos") {
    1023
} else {
    4095
};
pub fn stack(frames: &[Vec<u8>]) -> Result<Vec<u8>, Vec<u8>> {
    let [cwd, home, rest @ ..] = frames else {
        return Err(b"directory stack needs cwd and HOME frames".to_vec());
    };
    let mut out = Vec::new();
    for (i, path) in std::iter::once(cwd).chain(rest).enumerate() {
        if i > 0 {
            out.push(b' ');
        }
        if !home.is_empty()
            && (path == home || (path.starts_with(home) && path.get(home.len()) == Some(&b'/')))
        {
            out.push(b'~');
            out.extend(&path[home.len()..]);
        } else {
            out.extend(path);
        }
    }
    out.push(b'\n');
    Ok(out)
}
pub fn physical() -> io::Result<Vec<u8>> {
    let mut bytes = std::env::current_dir()?.into_os_string().into_vec();
    bytes.push(b'\n');
    Ok(bytes)
}
pub fn oldpwd_valid(path: &[u8]) -> bool {
    rustix::fs::stat(OsStr::from_bytes(path)).is_ok_and(|s| {
        rustix::fs::FileType::from_raw_mode(s.st_mode) == rustix::fs::FileType::Directory
    })
}
pub fn validate(frames: &[Vec<u8>]) -> bool {
    let Some((pwd, _)) = frames.split_first() else {
        return false;
    };
    if !frames.iter().all(|path| {
        !path.is_empty()
            && path[0] == b'/'
            && path.len() <= MAX_PATH
            && path
                .split(|&c| c == b'/')
                .all(|part| part.len() <= 255 && part != b"." && part != b"..")
    }) {
        return false;
    }
    match (
        rustix::fs::stat(OsStr::from_bytes(pwd)),
        rustix::fs::stat("."),
    ) {
        (Ok(logical), Ok(actual)) => {
            logical.st_dev == actual.st_dev && logical.st_ino == actual.st_ino
        }
        _ => false,
    }
}
pub fn path_bound(cwd: &[u8], operand: &[u8]) -> bool {
    let path = if operand.starts_with(b"/") {
        operand.to_vec()
    } else {
        [cwd, b"/", operand].concat()
    };
    let prefix = if path.starts_with(b"//") && !path.starts_with(b"///") {
        2
    } else {
        1
    };
    let mut parts = Vec::new();
    for component in path.split(|&c| c == b'/') {
        match component {
            b"" | b"." => {}
            b".." => {
                parts.pop();
            }
            _ => parts.push(component),
        }
    }
    let length =
        parts.iter().map(|x| x.len()).sum::<usize>() + parts.len().saturating_sub(1) + prefix;
    length <= MAX_PATH
}
fn diagnostic_path(path: &[u8]) -> Vec<u8> {
    if path.iter().all(|&c| (32..127).contains(&c)) {
        return path.to_vec();
    }
    let mut out = b"$'".to_vec();
    for &c in path {
        match c {
            7 => out.extend(b"\\a"),
            8 => out.extend(b"\\b"),
            9 => out.extend(b"\\t"),
            10 => out.extend(b"\\n"),
            11 => out.extend(b"\\v"),
            12 => out.extend(b"\\f"),
            13 => out.extend(b"\\r"),
            27 => out.extend(b"\\E"),
            39 => out.extend(b"\\'"),
            92 => out.extend(b"\\\\"),
            0..=31 | 127..=255 => out.extend(format!("\\{c:03o}").as_bytes()),
            _ => out.push(c),
        }
    }
    out.push(b'\'');
    out
}
pub type Diagnostic = (Vec<u8>, Vec<u8>, Vec<u8>, Vec<u8>);
pub fn diagnostic(input: &[u8]) -> Result<Diagnostic, Vec<u8>> {
    let mut fields = Vec::new();
    let mut rest = input;
    for _ in 0..4 {
        let Some(at) = rest.iter().position(|&c| c == 0) else {
            return Err(b"invalid directory diagnostic metadata".to_vec());
        };
        fields.push(&rest[..at]);
        rest = &rest[at + 1..];
    }
    let (origin, line, operation, operand) = (fields[0], fields[1], fields[2], fields[3]);
    if !matches!(operation, b"cd" | b"pushd" | b"popd") {
        return Err(b"invalid directory diagnostic metadata".to_vec());
    }
    let mut message = Vec::new();
    if !rest.is_empty() {
        let cases: &[(&[u8], &[u8], &[u8])] = &[
            (
                b"cd: The directory '",
                b"' does not exist\n",
                b"No such file or directory",
            ),
            (b"cd: '", b"' is not a directory\n", b"Not a directory"),
            (b"cd: Permission denied: '", b"'\n", b"Permission denied"),
            (
                b"cd: Too many levels of symbolic links: '",
                b"'\n",
                b"Too many levels of symbolic links",
            ),
            (
                b"cd: File name too long\ncd: Unknown error trying to locate directory '",
                b"'\n",
                b"File name too long",
            ),
        ];
        let reason = cases
            .iter()
            .find_map(|(prefix, suffix, reason)| {
                rest.starts_with(&[prefix, operand, suffix].concat())
                    .then_some(*reason)
            })
            .ok_or_else(|| b"unrecognized Fish 4.6 C-locale cd diagnostic".to_vec())?;
        message = [
            origin,
            b": line ",
            line,
            b": ",
            operation,
            b": ",
            &diagnostic_path(operand),
            b": ",
            reason,
            b"\n",
        ]
        .concat();
    }
    Ok((origin.to_vec(), line.to_vec(), operation.to_vec(), message))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn stack_abbreviates_home_only_at_component_boundary() {
        let input = [
            b"/home/me".to_vec(),
            b"/home/me".to_vec(),
            b"/home/me/a".to_vec(),
            b"/home/mean".to_vec(),
        ];
        assert_eq!(stack(&input).unwrap(), b"~ ~/a /home/mean\n");
    }
    #[test]
    fn diagnostic_quotes_non_ascii_bytes() {
        let input =
            b"script\x0012\x00cd\x00bad\xff\x00cd: The directory 'bad\xff' does not exist\nextra";
        assert_eq!(
            diagnostic(input).unwrap().3,
            b"script: line 12: cd: $'bad\\377': No such file or directory\n"
        );
    }
    #[test]
    fn lexical_bounds_normalize_without_touching_filesystem() {
        let huge = vec![b'x'; 5000];
        let operand = [huge.as_slice(), b"/../short"].concat();
        assert!(path_bound(b"/tmp", &operand));
        assert!(!path_bound(b"/tmp", &huge));
    }
}
