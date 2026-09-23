//! Compose scalar fragments before splitting and globbing. Quoted empties
//! remain positions, not a property applied to the entire source word.
use super::pattern::{Parts, glob_paths};
enum Fragment {
    QuotedEmpty,
    Byte {
        split: bool,
        active: bool,
        value: u8,
    },
}
fn white(fragment: &Fragment, ifs: &[u8]) -> bool {
    matches!(fragment,Fragment::Byte{split:true,value,..} if ifs.contains(value)&&matches!(value,32|9|10))
}
fn delimiter(fragment: &Fragment, ifs: &[u8]) -> bool {
    matches!(fragment,Fragment::Byte{split:true,value,..} if ifs.contains(value))
}
fn append(parts: &mut Parts, active: bool, value: u8) {
    if let Some((last, bytes)) = parts.last_mut()
        && *last == active
    {
        bytes.push(value);
        return;
    }
    parts.push((active, vec![value]));
}
pub fn expand_words(frames: &[Vec<u8>]) -> Result<Vec<Vec<u8>>, Vec<u8>> {
    let Some((ifs, frames)) = frames.split_first() else {
        return Err(b"expansion requires IFS and fragment frames".to_vec());
    };
    if frames.len() % 2 != 0 {
        return Err(b"invalid expansion mode or fragment count".to_vec());
    }
    let mut fragments = Vec::new();
    for pair in frames.as_chunks::<2>().0 {
        let mode = pair[0].as_slice();
        if !matches!(mode, b"q" | b"l" | b"e") {
            return Err(b"invalid expansion mode or fragment count".to_vec());
        }
        if mode == b"q" && pair[1].is_empty() {
            fragments.push(Fragment::QuotedEmpty);
        } else {
            fragments.extend(pair[1].iter().map(|&value| Fragment::Byte {
                split: mode == b"e",
                active: mode != b"q",
                value,
            }));
        }
    }
    let mut out = Vec::new();
    let mut current = Parts::new();
    let mut forced = false;
    let mut at = 0;
    while at < fragments.len() {
        match &fragments[at] {
            Fragment::QuotedEmpty => {
                forced = true;
                at += 1;
            }
            Fragment::Byte { active, value, .. } => {
                if delimiter(&fragments[at], ifs) {
                    while at < fragments.len() && white(&fragments[at], ifs) {
                        at += 1;
                    }
                    let nonwhite = at < fragments.len() && delimiter(&fragments[at], ifs);
                    if nonwhite {
                        at += 1;
                        while at < fragments.len() && white(&fragments[at], ifs) {
                            at += 1;
                        }
                    }
                    if nonwhite || forced || !current.is_empty() {
                        out.extend(glob_paths(&current));
                    }
                    current.clear();
                    forced = false;
                } else {
                    append(&mut current, *active, *value);
                    at += 1;
                }
            }
        }
    }
    if forced || !current.is_empty() {
        out.extend(glob_paths(&current));
    }
    Ok(out)
}
