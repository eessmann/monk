//! C-locale byte patterns and pathname expansion. Protected fragments are
//! escaped before tokenization so brackets may span fragment boundaries.
use std::ffi::OsStr;
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
pub type Parts = Vec<(bool, Vec<u8>)>;
pub fn pattern_parts(frames: &[Vec<u8>]) -> Result<Parts, Vec<u8>> {
    if !frames.len().is_multiple_of(2) {
        return Err(b"invalid pattern frames".to_vec());
    }
    frames
        .as_chunks::<2>()
        .0
        .iter()
        .map(|pair| match pair[0].as_slice() {
            b"0" => Ok((false, pair[1].clone())),
            b"1" => Ok((true, pair[1].clone())),
            _ => Err(b"invalid pattern frames".to_vec()),
        })
        .collect()
}
#[derive(Clone, PartialEq, Eq)]
enum Token {
    Literal(u8),
    Star,
    AnyByte,
    Class(bool, Box<[bool; 256]>),
}
enum Member {
    Byte(bool, u8),
    Set(Box<[bool; 256]>, bool),
}
fn class_bytes(name: &[u8]) -> Box<[bool; 256]> {
    let mut bytes = Box::new([false; 256]);
    for n in 0_u16..256 {
        let c = n as u8;
        let alpha = c.is_ascii_alphabetic();
        let digit = c.is_ascii_digit();
        bytes[usize::from(c)] = match name {
            b"alnum" => alpha || digit,
            b"alpha" => alpha,
            b"ascii" => c < 128,
            b"blank" => matches!(c, 9 | 32),
            b"cntrl" => c < 32 || c == 127,
            b"digit" => digit,
            b"graph" => (33..=126).contains(&c),
            b"lower" => c.is_ascii_lowercase(),
            b"print" => (32..=126).contains(&c),
            b"punct" => (33..=126).contains(&c) && !(alpha || digit),
            b"space" => matches!(c, 9 | 10 | 11 | 12 | 13 | 32),
            b"upper" => c.is_ascii_uppercase(),
            b"word" => alpha || digit || c == 95,
            b"xdigit" => c.is_ascii_hexdigit(),
            _ => false,
        };
    }
    bytes
}
fn bracket_class(input: &[(bool, u8)]) -> Option<(Token, usize)> {
    let negative = matches!(input.first(), Some((true, b'!' | b'^')));
    let mut i = usize::from(negative);
    let mut members = Vec::new();
    let mut closed = false;
    while i < input.len() {
        if !members.is_empty() && input[i] == (true, b']') {
            i += 1;
            closed = true;
            break;
        }
        if input[i] == (true, b'[')
            && input
                .get(i + 1)
                .is_some_and(|&(active, c)| active && matches!(c, b':' | b'.' | b'='))
        {
            let marker = input[i + 1].1;
            let mut end = i + 2;
            while end + 1 < input.len()
                && !(input[end] == (true, marker) && input[end + 1] == (true, b']'))
            {
                end += 1;
            }
            if end + 1 < input.len() {
                let name: Vec<u8> = input[i + 2..end].iter().map(|x| x.1).collect();
                let (set, invalid) = if marker == b':' {
                    (class_bytes(&name), false)
                } else {
                    let mut set = Box::new([false; 256]);
                    if name.len() == 1 {
                        set[usize::from(name[0])] = true;
                    }
                    (set, name.len() != 1)
                };
                members.push(Member::Set(set, invalid));
                i = end + 2;
                continue;
            }
            members.push(Member::Byte(false, b'['));
            i += 1;
            continue;
        }
        members.push(Member::Byte(input[i].0, input[i].1));
        i += 1;
    }
    if !closed {
        return None;
    }
    let invalid = members.windows(3).any(|w| {
        matches!(
            w,
            [Member::Byte(..), Member::Byte(true, b'-'), Member::Set(..)]
        )
    }) || members.iter().any(|m| matches!(m, Member::Set(_, true)));
    let mut bytes = Box::new([false; 256]);
    let mut at = 0;
    while at < members.len() {
        match &members[at..] {
            [
                Member::Byte(_, low),
                Member::Byte(true, b'-'),
                Member::Byte(_, high),
                ..,
            ] => {
                for c in *low..=*high {
                    bytes[usize::from(c)] = true;
                }
                at += 3;
            }
            [Member::Byte(_, c), ..] => {
                bytes[usize::from(*c)] = true;
                at += 1;
            }
            [Member::Set(set, _), ..] => {
                for c in 0..256 {
                    bytes[c] |= set[c];
                }
                at += 1;
            }
            [] => break,
        }
    }
    Some((
        if invalid {
            Token::Class(false, Box::new([false; 256]))
        } else {
            Token::Class(negative, bytes)
        },
        i,
    ))
}
fn tokenize(parts: &[(bool, Vec<u8>)], pathname: bool) -> Vec<Token> {
    let mut encoded = Vec::new();
    for (active, bytes) in parts {
        for &c in bytes {
            if !active && b"!^-.:=*[]?\\%#(|)".contains(&c) {
                encoded.push(b'\\');
            }
            encoded.push(c);
        }
    }
    let mut unescaped = Vec::new();
    let mut at = 0;
    while at < encoded.len() {
        let c = encoded[at];
        if c == b'\\' && at + 1 < encoded.len() {
            unescaped.push((false, encoded[at + 1]));
            at += 2;
        } else {
            unescaped.push((true, c));
            at += 1;
        }
    }
    let mut out = Vec::new();
    at = 0;
    while at < unescaped.len() {
        let (active, c) = unescaped[at];
        at += 1;
        match (active, c) {
            (true, b'*') => out.push(Token::Star),
            (true, b'?') => out.push(Token::AnyByte),
            (true, b'[') => {
                if let Some((token, used)) = bracket_class(&unescaped[at..])
                    && (!pathname || !unescaped[at..at + used].iter().any(|x| x.1 == b'/'))
                {
                    out.push(token);
                    at += used;
                    continue;
                }
                out.push(Token::Literal(b'['));
            }
            _ => out.push(Token::Literal(c)),
        }
    }
    out
}
fn closure(tokens: &[Token], states: &mut [bool]) {
    for i in 0..tokens.len() {
        if states[i] && tokens[i] == Token::Star {
            states[i + 1] = true;
        }
    }
}
struct Matcher<'a> {
    tokens: &'a [Token],
    states: Vec<bool>,
    next: Vec<bool>,
}
impl<'a> Matcher<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            states: vec![false; tokens.len() + 1],
            next: vec![false; tokens.len() + 1],
        }
    }
    fn matches(&mut self, subject: &[u8]) -> bool {
        let tokens = self.tokens;
        let states = &mut self.states;
        let next = &mut self.next;
        states.fill(false);
        states[0] = true;
        closure(tokens, states);
        for &c in subject {
            next.fill(false);
            for i in 0..tokens.len() {
                if !states[i] {
                    continue;
                }
                match &tokens[i] {
                    Token::Star => next[i] = true,
                    Token::AnyByte => next[i + 1] = true,
                    Token::Literal(b) => next[i + 1] |= *b == c,
                    Token::Class(negative, members) => {
                        next[i + 1] |= members[usize::from(c)] != *negative
                    }
                }
            }
            closure(tokens, next);
            std::mem::swap(states, next);
        }
        states[tokens.len()]
    }
}
pub fn matches(subject: &[u8], parts: &[(bool, Vec<u8>)]) -> bool {
    Matcher::new(&tokenize(parts, false)).matches(subject)
}
fn all_literal(tokens: &[Token]) -> bool {
    tokens.iter().all(|t| matches!(t, Token::Literal(_)))
}
fn path(bytes: &[u8]) -> &Path {
    Path::new(OsStr::from_bytes(bytes))
}
fn is_directory(bytes: &[u8]) -> bool {
    std::fs::metadata(path(bytes)).is_ok_and(|m| m.is_dir())
}
fn walk(parent: Vec<u8>, components: &[Vec<Token>], found: &mut Vec<Vec<u8>>) {
    let Some((component, rest)) = components.split_first() else {
        if std::fs::symlink_metadata(path(&parent)).is_ok() {
            found.push(parent);
        }
        return;
    };
    if component.is_empty() {
        if rest.is_empty() {
            if is_directory(if parent.is_empty() { b"." } else { &parent }) {
                found.push(parent);
            }
        } else {
            let mut parent = parent;
            parent.push(b'/');
            walk(parent, rest, found);
        }
        return;
    }
    if all_literal(component) {
        let mut joined = parent;
        joined.extend(component.iter().filter_map(|t| {
            if let Token::Literal(c) = t {
                Some(*c)
            } else {
                None
            }
        }));
        descend(joined, rest, found);
        return;
    }
    let Ok(entries) = std::fs::read_dir(path(if parent.is_empty() { b"." } else { &parent }))
    else {
        return;
    };
    let mut matcher = Matcher::new(component);
    for entry in entries.flatten() {
        let name = entry.file_name();
        let bytes = name.as_bytes();
        if bytes.is_empty()
            || bytes == b"."
            || bytes == b".."
            || (bytes[0] == b'.' && component.first() != Some(&Token::Literal(b'.')))
            || !matcher.matches(bytes)
        {
            continue;
        }
        let joined = [parent.as_slice(), bytes].concat();
        if rest.is_empty() || is_directory(&joined) {
            descend(joined, rest, found);
        }
    }
}
fn descend(mut path: Vec<u8>, rest: &[Vec<Token>], found: &mut Vec<Vec<u8>>) {
    if !rest.is_empty() {
        path.push(b'/');
    }
    walk(path, rest, found);
}
pub fn glob_paths(parts: &[(bool, Vec<u8>)]) -> Vec<Vec<u8>> {
    let spelling: Vec<u8> = parts
        .iter()
        .flat_map(|(_, bytes)| bytes.iter().copied())
        .collect();
    let tokens = tokenize(parts, true);
    if all_literal(&tokens) {
        return vec![spelling];
    }
    let raw: Vec<Vec<Token>> = tokens
        .split(|t| *t == Token::Literal(b'/'))
        .map(<[Token]>::to_vec)
        .collect();
    let mut expanded = false;
    let mut components = Vec::new();
    for (i, part) in raw.iter().enumerate() {
        if expanded && part.is_empty() && i + 1 < raw.len() {
            continue;
        }
        expanded |= !all_literal(part);
        components.push(part.clone());
    }
    let mut found = Vec::new();
    walk(Vec::new(), &components, &mut found);
    if found.is_empty() {
        vec![spelling]
    } else {
        found.sort();
        found
    }
}
pub fn trim_pattern_parts(
    prefix: bool,
    longest: bool,
    subject: &[u8],
    parts: &[(bool, Vec<u8>)],
) -> Vec<u8> {
    let tokens = tokenize(parts, false);
    let mut matcher = Matcher::new(&tokens);
    for i in 0..=subject.len() {
        let n = if longest { subject.len() - i } else { i };
        let candidate = if prefix {
            &subject[..n]
        } else {
            &subject[subject.len() - n..]
        };
        if matcher.matches(candidate) {
            return if prefix {
                subject[n..].to_vec()
            } else {
                subject[..subject.len() - n].to_vec()
            };
        }
    }
    subject.to_vec()
}
pub fn replace_literal(
    all_matches: bool,
    subject: &[u8],
    needle: &[u8],
    replacement: &[u8],
) -> Vec<u8> {
    if needle.is_empty() {
        return subject.to_vec();
    }
    let mut rest = subject;
    let mut out = Vec::new();
    while let Some(at) = rest.windows(needle.len()).position(|s| s == needle) {
        out.extend(&rest[..at]);
        out.extend(replacement);
        rest = &rest[at + needle.len()..];
        if !all_matches {
            break;
        }
    }
    out.extend(rest);
    out
}
