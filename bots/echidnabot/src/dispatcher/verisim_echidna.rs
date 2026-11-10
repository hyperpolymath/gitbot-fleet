// SPDX-License-Identifier: MPL-2.0
//! Proposed ECHIDNA-specific VeriSimDB/VCL boundary, NOT a live transport.
//!
//! No implementation of the gateway or production wiring exists here. See
//! `docs/ESTATE-ROUTING-AUDIT.adoc`. This is not a generic database/prover API.

use async_trait::async_trait;
use serde_json::Value;

use super::{ProveResult, ProverKind};
use crate::error::Result;

/// Input to the proposed ECHIDNA route; deliberately not a VCL wire format.
#[derive(Debug)]
pub struct EchidnaProveRequest<'a> {
    pub prover: &'a ProverKind,
    pub content: &'a str,
}

/// Backend-specific integration point awaiting a real gateway contract.
///
/// Implementations must submit through VeriSimDB's authorised VCL route and
/// extract exactly one ECHIDNA receipt, not a query-success flag or row count.
/// They must propagate routing failures without falling back to direct HTTP.
/// This trait documents obligations; it does not prove routing or soundness.
#[async_trait]
pub trait VeriSimEchidnaGateway: Send + Sync {
    /// Submit a proof request and return the extracted ECHIDNA receipt as JSON.
    ///
    /// # Errors
    /// Implementations must propagate routing failures to the caller.
    async fn prove_receipt(&self, request: EchidnaProveRequest<'_>) -> Result<Value>;
}

/// Validates only the ECHIDNA receipt, without local trust enrichment.
///
/// This adapter has no ECHIDNA endpoint, direct client, or legacy fallback.
/// Receipt validation is structural, not evidence that the claim is true or
/// that the supplied gateway actually used VCL.
pub struct VeriSimEchidnaAdapter<G> {
    gateway: G,
}

impl<G: VeriSimEchidnaGateway> VeriSimEchidnaAdapter<G> {
    /// Create an adapter using the supplied gateway.
    pub fn new(gateway: G) -> Self {
        Self { gateway }
    }

    /// Submit a request and return a structurally validated ECHIDNA receipt.
    ///
    /// Recognised fields are returned without local trust enrichment; unknown
    /// fields within `trust` are ignored.
    ///
    /// # Errors
    /// Propagates gateway errors and validation errors from
    /// [`ProveResult::from_value`].
    pub async fn prove(&self, request: EchidnaProveRequest<'_>) -> Result<ProveResult> {
        ProveResult::from_value(self.gateway.prove_receipt(request).await?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::Error;

    struct Stub(Value);

    #[async_trait]
    impl VeriSimEchidnaGateway for Stub {
        async fn prove_receipt(&self, request: EchidnaProveRequest<'_>) -> Result<Value> {
            assert_eq!(request.prover.as_str(), "lean");
            assert_eq!(request.content, "goal");
            Ok(self.0.clone())
        }
    }

    fn receipt() -> Value {
        serde_json::json!({
            "schema": "echidna.prove.result/1", "status": "verified",
            "prover": "Lean", "goal": "goal", "duration_ms": 12,
            "message": "ok", "echidna_version": "2.3.0",
            "trust": {"axioms": ["sorryAx", "backend-specific"], "confidence": 0.2}
        })
    }

    async fn run(value: Value) -> Result<ProveResult> {
        VeriSimEchidnaAdapter::new(Stub(value))
            .prove(EchidnaProveRequest {
                prover: &ProverKind::new("lean"),
                content: "goal",
            })
            .await
    }

    #[tokio::test]
    async fn preserves_typed_receipt_without_enrichment() {
        let value = receipt();
        let result = run(value.clone()).await.unwrap();
        assert_eq!(serde_json::to_value(result).unwrap(), value);
    }

    #[tokio::test]
    async fn rejects_legacy_envelopes_versions_and_invalid_receipts() {
        assert!(run(serde_json::json!({"valid": true})).await.is_err());
        for (key, value) in [
            ("schema", serde_json::json!("echidna.prove.result/2")),
            ("status", serde_json::json!("success")),
            ("duration_ms", serde_json::json!(1_u64 << 53)),
        ] {
            let mut body = receipt();
            body[key] = value;
            assert!(run(body).await.is_err());
        }
    }

    struct Unavailable;

    #[async_trait]
    impl VeriSimEchidnaGateway for Unavailable {
        async fn prove_receipt(&self, _: EchidnaProveRequest<'_>) -> Result<Value> {
            Err(Error::Unsupported("ECHIDNA VCL route unavailable".into()))
        }
    }

    #[tokio::test]
    async fn propagates_gateway_failure_without_direct_fallback() {
        let result = VeriSimEchidnaAdapter::new(Unavailable)
            .prove(EchidnaProveRequest {
                prover: &ProverKind::new("lean"),
                content: "goal",
            })
            .await;
        assert!(matches!(result, Err(Error::Unsupported(_))));
    }
}
