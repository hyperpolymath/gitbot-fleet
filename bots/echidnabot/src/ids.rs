// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! # Identifier minting — the only place echidnabot mints ids
//!
//! Two kinds of identifier, and nothing else in the crate mints one:
//!
//! * [`new_record_id`] — a **UUIDv7** (RFC 9562 §5.7) for *events and
//!   records*: jobs, repositories, proof-result records, fleet findings.
//!   Time-ordered, minted from the `uuid` crate's library API as
//!   `standards/docs/UUID-V7-ESTATE-STANDARD.adoc` requires (no hand-set
//!   version bits).
//! * [`content_id`] — a **UUIDv8** (RFC 9562 §5.8, Appendix B.2 style) for
//!   *content*: proof goals and `echidna.prove.result/1` objects. It is
//!   SHA-256 over the RFC 8785 (JCS) canonical bytes of the value, truncated
//!   to the first 16 bytes, with the version nibble set to `8` and the
//!   variant bits set to `10`. The same JSON content always yields the same
//!   id, whatever key order or whitespace it arrived in.
//!
//! The v7/v8 split is the owner's delegated policy of 2026-10-05: v7 for
//! things that *happened*, v8 for things that *are* (content addressing).
//! The same split, with the same construction, is implemented in
//! `hyperpolymath/echidna` (`src/rust/ids.rs`) and
//! `hyperpolymath/proof-burrower` (`burrower-core/src/ids.rs`), so a content
//! id computed by any of the three agrees. See
//! `docs/ECHIDNA-INTEGRATION.adoc` § Identifiers.
//!
//! IDs that arrive from outside (a forge's repository id, an ECHIDNA job id)
//! keep whatever format they came with; this module only mints our own.

use sha2::{Digest, Sha256};
use uuid::Uuid;

/// Mint a fresh time-ordered record id (UUIDv7).
///
/// Use for events and records: jobs, repositories, result records, fleet
/// findings. Within one process successive ids sort in minting order (the
/// `uuid` crate keeps a monotonic counter inside the millisecond).
pub fn new_record_id() -> Uuid {
    Uuid::now_v7()
}

/// Content id of a JSON value: UUIDv8 over SHA-256 of its JCS bytes.
///
/// Use for proof goals and prove results. Fails only if the value cannot be
/// canonicalised (RFC 8785 rejects non-finite numbers, which
/// `serde_json::Value` cannot hold anyway, so this is defensive).
pub fn content_id(value: &serde_json::Value) -> Result<Uuid, serde_json::Error> {
    let canonical = serde_json_canonicalizer::to_vec(value)?;
    let digest = Sha256::digest(&canonical);
    let mut first16 = [0u8; 16];
    first16.copy_from_slice(&digest[..16]);
    // `new_v8` overwrites the version nibble with 8 and the variant bits
    // with RFC 9562's `10`; the other 122 bits are the hash prefix.
    Ok(Uuid::new_v8(first16))
}

/// Content id of a proof goal's text (a JSON string).
///
/// Hashes the exact text as a JSON string, without trimming or normalisation.
/// Returns the nil UUID if canonicalisation fails instead of propagating an error.
pub fn goal_content_id(goal_text: &str) -> Uuid {
    content_id(&serde_json::Value::String(goal_text.to_string())).unwrap_or_else(|_| Uuid::nil())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// Same JCS content gives the same v8 id regardless of key order and spacing.
    #[test]
    fn same_canonical_content_same_v8_id() {
        let a: serde_json::Value = serde_json::from_str(r#"{"b":1,"a":[true,null]}"#).unwrap();
        let b: serde_json::Value =
            serde_json::from_str("{ \"a\" : [ true , null ] , \"b\" : 1 }").unwrap();
        assert_eq!(content_id(&a).unwrap(), content_id(&b).unwrap());
        assert_ne!(
            content_id(&a).unwrap(),
            content_id(&json!({"a":[true,null],"b":2})).unwrap()
        );
    }

    /// The v8 id is exactly the SHA-256 prefix with version/variant bits applied.
    #[test]
    fn v8_bits_and_hash_prefix() {
        let id = content_id(&json!("lemma foo: x = y")).unwrap();
        assert_eq!(id.get_version_num(), 8);
        assert_eq!(id.get_variant(), uuid::Variant::RFC4122);
        let bytes = id.as_bytes();
        assert_eq!(bytes[6] >> 4, 0x8);
        assert_eq!(bytes[8] >> 6, 0b10);
        let digest = Sha256::digest(br#""lemma foo: x = y""#);
        for (i, byte) in bytes.iter().enumerate() {
            let mask = match i {
                6 => 0x0f,
                8 => 0x3f,
                _ => 0xff,
            };
            assert_eq!(byte & mask, digest[i] & mask, "byte {i}");
        }
        assert_eq!(goal_content_id("lemma foo: x = y"), id);
    }

    /// v7 record ids carry version 7 and sort in minting order.
    #[test]
    fn v7_record_ids_are_versioned_and_monotonic() {
        let ids: Vec<Uuid> = (0..1000).map(|_| new_record_id()).collect();
        for w in ids.windows(2) {
            assert!(w[0] < w[1], "{} !< {}", w[0], w[1]);
        }
        assert_eq!(ids[0].get_version_num(), 7);
        assert_eq!(ids[0].get_variant(), uuid::Variant::RFC4122);
    }
}
