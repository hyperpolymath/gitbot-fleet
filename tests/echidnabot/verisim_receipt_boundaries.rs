// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! The proposed gateway transports one receipt and never upgrades its evidence.
use async_trait::async_trait;
use echidnabot::dispatcher::verisim_echidna::{
    EchidnaProveRequest, VeriSimEchidnaAdapter, VeriSimEchidnaGateway,
};
use echidnabot::dispatcher::ProverKind;
use echidnabot::error::{Error, Result};
use serde_json::{json, Value};
use std::sync::{Arc, atomic::{AtomicUsize, Ordering}};

struct Gateway {
    response: Option<Value>,
    calls: Arc<AtomicUsize>,
}

#[async_trait]
impl VeriSimEchidnaGateway for Gateway {
    async fn prove_receipt(&self, request: EchidnaProveRequest<'_>) -> Result<Value> {
        self.calls.fetch_add(1, Ordering::SeqCst);
        assert_eq!(request.prover.as_str(), "coq");
        assert_eq!(request.content, "Lemma unicode_λ : True.\nProof. exact I. Qed.");
        self.response.clone().ok_or_else(|| Error::Unsupported("synthetic gateway failure".into()))
    }
}

fn receipt() -> Value {
    json!({"schema": "echidna.prove.result/1", "status": "verified",
        "prover": "Coq", "goal": "True", "duration_ms": 0, "message": "",
        "echidna_version": "2.3.0", "trust": {"axioms": [], "confidence": null}})
}

async fn submit(response: Option<Value>) -> Result<Value> {
    let calls = Arc::new(AtomicUsize::new(0));
    let adapter = VeriSimEchidnaAdapter::new(Gateway { response, calls: Arc::clone(&calls) });
    let result = adapter.prove(EchidnaProveRequest {
        prover: &ProverKind::new("coq"),
        content: "Lemma unicode_λ : True.\nProof. exact I. Qed.",
    }).await;
    assert_eq!(calls.load(Ordering::SeqCst), 1, "no retry or alternate gateway call");
    result.map(|r| serde_json::to_value(r).unwrap())
}

#[tokio::test]
async fn all_receipt_statuses_and_safe_integer_boundary_are_preserved() {
    for status in ["verified", "failed", "timeout", "error", "unknown"] {
        let mut value = receipt();
        value["status"] = json!(status);
        value["duration_ms"] = json!((1_u64 << 53) - 1);
        assert_eq!(submit(Some(value.clone())).await.unwrap(), value);
    }
}

#[tokio::test]
async fn optional_confidence_and_backend_axioms_are_not_enriched() {
    for confidence in [Value::Null, json!(0.0), json!(1.0)] {
        let mut value = receipt();
        value["trust"] = json!({"axioms": ["opaque-backend-axiom", "sorryAx"],
            "confidence": confidence});
        assert_eq!(submit(Some(value.clone())).await.unwrap(), value);
    }
}

#[tokio::test]
async fn missing_required_fields_are_rejected_at_the_adapter_boundary() {
    for field in ["schema", "status", "prover", "goal", "duration_ms", "message",
        "echidna_version", "trust"] {
        let mut value = receipt();
        value.as_object_mut().unwrap().remove(field);
        assert!(submit(Some(value)).await.is_err(), "missing {field}");
    }
}

#[tokio::test]
async fn query_success_wrappers_and_multiple_receipts_are_not_proof_receipts() {
    for value in [Value::Null, json!(true), json!([]), json!([receipt()]),
        json!([receipt(), receipt()]), json!({"success": true, "receipt": receipt()}),
        json!({"data": {"receipt": receipt()}})] {
        assert!(submit(Some(value)).await.is_err());
    }
}

#[tokio::test]
async fn malformed_numeric_fields_do_not_fall_back_to_legacy_success() {
    for (field, invalid) in [("duration_ms", json!(-1)), ("duration_ms", json!(0.5)),
        ("duration_ms", json!("12")), ("duration_ms", json!(1_u64 << 53))] {
        let mut value = receipt();
        value[field] = invalid;
        assert!(submit(Some(value)).await.is_err());
    }
}

#[tokio::test]
async fn gateway_error_retains_its_kind_and_message() {
    assert!(matches!(submit(None).await,
        Err(Error::Unsupported(message)) if message == "synthetic gateway failure"));
}

#[tokio::test]
async fn unknown_trust_metadata_is_ignored_without_changing_recognised_fields() {
    let expected = receipt();
    let mut value = expected.clone();
    value["trust"]["backend_metadata"] = json!({"verified": true});
    assert_eq!(submit(Some(value)).await.unwrap(), expected);
}

#[tokio::test]
async fn unknown_top_level_fields_are_rejected_instead_of_silently_discarded() {
    let mut value = receipt();
    value["verified"] = json!(true);
    assert!(submit(Some(value)).await.is_err());
}

#[tokio::test]
async fn malformed_trust_objects_are_not_upgraded_to_valid_receipts() {
    for trust in [Value::Null, json!([]), json!({"confidence": 0.5}),
        json!({"axioms": "sorryAx", "confidence": null}),
        json!({"axioms": [12], "confidence": null}),
        json!({"axioms": [], "confidence": "1.0"})] {
        let mut value = receipt();
        value["trust"] = trust;
        assert!(submit(Some(value)).await.is_err());
    }
}

#[tokio::test]
async fn gateway_confidence_and_axioms_survive_even_a_failed_verdict() {
    let mut value = receipt();
    value["status"] = json!("failed");
    value["trust"] = json!({"confidence": 0.25, "axioms": ["sorryAx", "backend-axiom"]});
    assert_eq!(submit(Some(value.clone())).await.unwrap(), value);
}
