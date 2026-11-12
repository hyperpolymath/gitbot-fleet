// SPDX-License-Identifier: MPL-2.0
//! The pinned pons commit, compiled in from `canon/pin.toml`.

use serde::Deserialize;

const PIN_TOML: &str = include_str!("../canon/pin.toml");

#[derive(Debug, Deserialize)]
struct PinFile {
    source: Source,
}

/// The `[source]` table of `canon/pin.toml`.
#[derive(Debug, Clone, Deserialize)]
pub struct Source {
    pub repo: String,
    pub commit: String,
    pub pinned: String,
}

/// The pin, parsed from the compiled-in `canon/pin.toml`.
///
/// Panics only if the file shipped in this crate is malformed, which the
/// `pin_is_a_full_commit` test rejects before release.
pub fn pinned() -> Source {
    toml::from_str::<PinFile>(PIN_TOML)
        .expect("canon/pin.toml is well-formed")
        .source
}

/// True for a full 40-hex lowercase commit.
pub fn is_full_commit(s: &str) -> bool {
    s.len() == 40 && s.bytes().all(|b| matches!(b, b'0'..=b'9' | b'a'..=b'f'))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pin_is_a_full_commit() {
        let p = pinned();
        assert!(
            is_full_commit(&p.commit),
            "pin must be 40 hex: {}",
            p.commit
        );
        assert_eq!(p.repo, "hyperpolymath/pons-asinorum");
    }
}
