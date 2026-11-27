// SPDX-License-Identifier: MPL-2.0
//! The pinned engine commits, compiled in from `canon/pin.toml`.

use serde::Deserialize;

const PIN_TOML: &str = include_str!("../canon/pin.toml");

/// Both pins.
#[derive(Debug, Clone, Deserialize)]
pub struct Pins {
    pub ingest: Source,
    pub reconcile: Source,
}

/// One `[table]` of `canon/pin.toml`.
#[derive(Debug, Clone, Deserialize)]
pub struct Source {
    pub repo: String,
    pub commit: String,
    pub pinned: String,
}

/// The pins, parsed from the compiled-in `canon/pin.toml`. A malformed file
/// is reported as an error by the caller, never a panic.
pub fn pinned() -> Result<Pins, toml::de::Error> {
    toml::from_str(PIN_TOML)
}

/// True for a full 40-hex lowercase commit.
pub fn is_full_commit(s: &str) -> bool {
    s.len() == 40 && s.bytes().all(|b| matches!(b, b'0'..=b'9' | b'a'..=b'f'))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pins_are_full_commits() -> Result<(), toml::de::Error> {
        let p = pinned()?;
        assert!(is_full_commit(&p.ingest.commit));
        assert!(is_full_commit(&p.reconcile.commit));
        assert_eq!(p.ingest.repo, "hyperpolymath/docudactyl");
        assert_eq!(p.reconcile.repo, "hyperpolymath/recon-silly-ation");
        Ok(())
    }
}
