// SPDX-License-Identifier: MPL-2.0
//! HTTP contract tests: the server here validates wire messages, not proofs.
use axum::{
    routing::{get, post},
    Json, Router,
};
use echidnabot::config::{EchidnaApiMode, EchidnaConfig};
use echidnabot::dispatcher::echidna_client::ProverStatus;
use echidnabot::dispatcher::{EchidnaClient, ProofStatus, ProverKind};
use serde_json::{json, Value};

#[test]
fn checked_in_negative_proofs_are_detected() {
    use echidnabot::trust::axiom_tracker::{AxiomFlag, AxiomTracker};
    for (slug, source, expected) in [
        (
            "coq",
            include_str!("../proofs/coq/admitted_stub.v"),
            AxiomFlag::Admitted,
        ),
        (
            "lean4",
            include_str!("../proofs/lean/sorry_stub.lean"),
            AxiomFlag::Sorry,
        ),
    ] {
        // Scan the declaration and body, excluding the explanatory header:
        // removing the actual hole must fail this control even if its name
        // remains in a comment.
        let declaration = source
            .lines()
            .skip_while(|line| !line.starts_with("Theorem ") && !line.starts_with("theorem "))
            .collect::<Vec<_>>()
            .join("\n");
        assert!(!declaration.is_empty());
        let report = AxiomTracker::scan(&ProverKind::new(slug), &declaration);
        assert!(report.has_unsound());
        assert!(report.flags.contains(&expected));
    }
    for (slug, source) in [
        ("coq", include_str!("../proofs/coq/trivial_ok.v")),
        ("lean4", include_str!("../proofs/lean/trivial_ok.lean")),
    ] {
        assert!(!AxiomTracker::scan(&ProverKind::new(slug), source).has_unsound());
    }
}

async fn endpoint(router: Router) -> (String, tokio::task::JoinHandle<()>) {
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let url = format!("http://{}", listener.local_addr().unwrap());
    let task = tokio::spawn(async move { axum::serve(listener, router).await.unwrap() });
    (url, task)
}

#[tokio::test]
async fn rest_uses_core_identifiers_and_preserves_rejection() {
    let router = Router::new()
        .route("/api/provers", get(|| async { Json(json!({"provers":[
            {"name":"Lean","tier":1,"complexity":3},
            {"name":"Isabelle","tier":1,"complexity":4},
            {"name":"HOLLight","tier":2,"complexity":3}
        ]})) }))
        .route("/api/verify", post(|Json(body): Json<Value>| async move {
            assert!(["Lean", "Isabelle", "HOLLight"].contains(&body["prover"].as_str().unwrap()));
            Json(json!({"valid":body["content"] == "accepted-fixture", "goals_remaining":0, "tactics_used":0}))
        }));
    let (url, server) = endpoint(router).await;
    let client = EchidnaClient::new(&EchidnaConfig {
        rest_endpoint: url,
        mode: EchidnaApiMode::Rest,
        ..Default::default()
    });
    for slug in ["lean", "isabelle", "hol-light"] {
        let prover = ProverKind::new(slug);
        assert_eq!(
            client.prover_status(&prover).await.unwrap(),
            ProverStatus::Available
        );
        assert_eq!(
            client
                .verify_proof(&prover, "accepted-fixture")
                .await
                .unwrap()
                .status,
            ProofStatus::Verified
        );
        assert_eq!(
            client
                .verify_proof(&prover, "rejected-fixture")
                .await
                .unwrap()
                .status,
            ProofStatus::Failed
        );
    }
    server.abort();
}

#[tokio::test]
async fn graphql_uses_slug_for_verification_suggestions_and_status() {
    let router = Router::new().route("/graphql", post(|Json(body): Json<Value>| async move {
        assert_eq!(body["variables"]["prover"], "isabelle");
        let query = body["query"].as_str().unwrap();
        let response = if query.contains("VerifyProof") {
            json!({"verifyProof":{"status":"FAILED","message":"contract fixture", "proverOutput":"", "durationMs":1, "artifacts":[]}})
        } else if query.contains("SuggestTactics") {
            json!({"suggestTactics":[]})
        } else {
            assert!(query.contains("ProverStatus"));
            json!({"proverStatus":{"available":true,"message":null}})
        };
        Json(json!({"data":response}))
    }));
    let (url, server) = endpoint(router).await;
    let client = EchidnaClient::new(&EchidnaConfig {
        endpoint: format!("{url}/graphql"),
        mode: EchidnaApiMode::Graphql,
        ..Default::default()
    });
    let prover = ProverKind::new("isabelle");
    assert_eq!(
        client
            .verify_proof(&prover, "fixture")
            .await
            .unwrap()
            .status,
        ProofStatus::Failed
    );
    assert!(client
        .suggest_tactics(&prover, "", "fixture")
        .await
        .unwrap()
        .is_empty());
    assert_eq!(
        client.prover_status(&prover).await.unwrap(),
        ProverStatus::Available
    );
    server.abort();
}
