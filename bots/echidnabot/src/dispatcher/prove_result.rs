// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! Consumer side of the shared `echidna.prove.result/1` contract.
//!
//! ECHIDNA's `echidna prove ... --output json` prints exactly one
//! JCS-canonical (RFC 8785) I-JSON (RFC 7493) object:
//!
//! ```json
//! {"duration_ms":12,"echidna_version":"2.3.0","goal":"t","message":"...",
//!  "prover":"Lean","schema":"echidna.prove.result/1","status":"verified",
//!  "trust":{"axioms":[],"confidence":null}}
//! ```
//!
//! echidnabot accepts that object wherever it consumes a result: a REST
//! `/api/verify` body that carries `"schema":"echidna.prove.result/1"` is read
//! as this shape, and anything else falls back to the legacy REST shape.
//!
//! The `trust` object is ECHIDNA's own judgement (a *receipt*, in the
//! vocabulary of `hyperpolymath/epistemic-types`). echidnabot transports it
//! instead of re-deriving a score; see [`super::TrustSource`].
//!
//! Validation here is deliberately strict about the parts a consumer depends
//! on (schema tag, status vocabulary, the I-JSON integer bound on
//! `duration_ms`, finite `confidence`) and liberal about key order, so a
//! non-canonical but otherwise valid object is still accepted.

use serde::{Deserialize, Serialize};

use super::ProofStatus;
use crate::error::{Error, Result};

/// The schema tag every `echidna.prove.result/1` object carries.
pub const PROVE_RESULT_SCHEMA: &str = "echidna.prove.result/1";

/// Largest integer I-JSON (RFC 7493 §2.2) guarantees to round-trip: 2^53 − 1.
pub const IJSON_MAX_SAFE_INTEGER: u64 = (1 << 53) - 1;

/// Status vocabulary of `echidna.prove.result/1`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ProveStatus {
    /// The prover accepted the proof.
    Verified,
    /// The prover rejected the proof.
    Failed,
    /// ECHIDNA or the backend failed before producing a verdict.
    Error,
    /// The backend ran out of time.
    Timeout,
    /// No verdict either way.
    Unknown,
}

impl From<ProveStatus> for ProofStatus {
    /// Map the contract's status onto echidnabot's own status enum.
    fn from(status: ProveStatus) -> Self {
        match status {
            ProveStatus::Verified => ProofStatus::Verified,
            ProveStatus::Failed => ProofStatus::Failed,
            ProveStatus::Error => ProofStatus::Error,
            ProveStatus::Timeout => ProofStatus::Timeout,
            ProveStatus::Unknown => ProofStatus::Unknown,
        }
    }
}

/// ECHIDNA's trust judgement for one result.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ProveTrust {
    /// ECHIDNA's confidence, or `null` when no receipt-backed computation
    /// produced one.
    pub confidence: Option<f64>,
    /// Axioms / holes ECHIDNA found in the proof.
    pub axioms: Vec<String>,
}

/// One `echidna.prove.result/1` object.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ProveResult {
    /// Always [`PROVE_RESULT_SCHEMA`].
    pub schema: String,
    /// Verdict.
    pub status: ProveStatus,
    /// ECHIDNA's prover identifier.
    pub prover: String,
    /// The goal or theorem name.
    pub goal: String,
    /// Wall-clock duration; bounded by [`IJSON_MAX_SAFE_INTEGER`].
    pub duration_ms: u64,
    /// Human-readable message.
    pub message: String,
    /// ECHIDNA's trust judgement.
    pub trust: ProveTrust,
    /// Version of the ECHIDNA that produced the object.
    pub echidna_version: String,
}

impl ProveResult {
    /// Parse and validate one `echidna.prove.result/1` object from text.
    ///
    /// Accepts surrounding whitespace and non-canonical key order. Invalid
    /// JSON returns [`Error::Json`]; validation errors from [`Self::from_value`]
    /// are propagated.
    pub fn parse(text: &str) -> Result<Self> {
        let value: serde_json::Value = serde_json::from_str(text.trim())?;
        Self::from_value(value)
    }

    /// Validate an already-parsed JSON value as `echidna.prove.result/1`.
    ///
    /// # Errors
    /// Returns [`Error::Echidna`] for a missing or different schema tag, a
    /// supplied duration outside the integer range `0..=2^53-1` milliseconds,
    /// or non-finite confidence. Returns [`Error::Json`] for deserialisation
    /// failures, including missing required fields, unknown top-level fields
    /// and unknown statuses. Unknown fields within `trust` are ignored.
    pub fn from_value(value: serde_json::Value) -> Result<Self> {
        if !Self::is_prove_result(&value) {
            return Err(Error::Echidna(format!(
                "not an {PROVE_RESULT_SCHEMA} object (schema tag missing or different)"
            )));
        }
        if let Some(d) = value.get("duration_ms") {
            match d.as_u64() {
                Some(n) if n <= IJSON_MAX_SAFE_INTEGER => {}
                _ => {
                    return Err(Error::Echidna(format!(
                        "duration_ms must be a non-negative integer <= 2^53-1 (I-JSON), got {d}"
                    )))
                }
            }
        }
        let parsed: ProveResult = serde_json::from_value(value)?;
        if let Some(c) = parsed.trust.confidence {
            if !c.is_finite() {
                return Err(Error::Echidna(
                    "trust.confidence must be a finite number or null".to_string(),
                ));
            }
        }
        Ok(parsed)
    }

    /// Whether a JSON value carries the `echidna.prove.result/1` schema tag.
    pub fn is_prove_result(value: &serde_json::Value) -> bool {
        value.get("schema").and_then(|s| s.as_str()) == Some(PROVE_RESULT_SCHEMA)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const CANONICAL: &str = r#"{"duration_ms":12,"echidna_version":"2.3.0","goal":"t","message":"ok","prover":"Lean","schema":"echidna.prove.result/1","status":"verified","trust":{"axioms":[],"confidence":null}}"#;

    #[test]
    fn parses_canonical_object() {
        let r = ProveResult::parse(CANONICAL).unwrap();
        assert_eq!(r.status, ProveStatus::Verified);
        assert_eq!(ProofStatus::from(r.status), ProofStatus::Verified);
        assert_eq!(r.trust.confidence, None);
        assert_eq!(r.duration_ms, 12);
    }

    #[test]
    fn accepts_non_canonical_key_order() {
        let text = r#"{"schema":"echidna.prove.result/1","status":"timeout","prover":"Z3","goal":"g","duration_ms":5,"message":"","trust":{"confidence":0.5,"axioms":["classical"]},"echidna_version":"2.4.0"}"#;
        let r = ProveResult::parse(text).unwrap();
        assert_eq!(ProofStatus::from(r.status), ProofStatus::Timeout);
        assert_eq!(r.trust.axioms, vec!["classical".to_string()]);
    }

    #[test]
    fn rejects_wrong_schema_tag() {
        let text = CANONICAL.replace("echidna.prove.result/1", "echidna.prove.result/2");
        assert!(ProveResult::parse(&text).is_err());
    }

    #[test]
    fn rejects_unknown_status() {
        let text = CANONICAL.replace("\"verified\"", "\"proved\"");
        assert!(ProveResult::parse(&text).is_err());
    }

    #[test]
    fn rejects_duration_outside_ijson_range() {
        let text = CANONICAL.replace("\"duration_ms\":12", "\"duration_ms\":9007199254740992");
        assert!(ProveResult::parse(&text).is_err());
        let text = CANONICAL.replace("\"duration_ms\":12", "\"duration_ms\":9007199254740991");
        assert!(ProveResult::parse(&text).is_ok());
    }

    #[test]
    fn rejects_unknown_fields() {
        let text = CANONICAL.replace("\"goal\":\"t\"", "\"goal\":\"t\",\"extra\":1");
        assert!(ProveResult::parse(&text).is_err());
    }
}
