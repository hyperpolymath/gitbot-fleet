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

// ---------------------------------------------------------------------------
// submitProofObligation — the wire contract hypatia sends (inbound).
// The query strings below are byte-for-byte what hypatia builds:
// FleetDispatcher.build_proof_obligation_mutation/4 (with and without a
// prover line) and LearningScheduler.submit_requeue/4 (with `inline: true`).
// ---------------------------------------------------------------------------

/// Schema over an in-memory store, for executing inbound GraphQL directly.
async fn obligation_schema() -> echidnabot::api::graphql::EchidnabotSchema {
    use echidnabot::api::graphql::GraphQLState;
    use echidnabot::scheduler::JobScheduler;
    use echidnabot::store::SqliteStore;
    use std::sync::Arc;

    let config = echidnabot::config::Config::default();
    echidnabot::api::create_schema(GraphQLState {
        store: Arc::new(SqliteStore::new("sqlite::memory:").await.unwrap()),
        scheduler: Arc::new(JobScheduler::new(2, 10)),
        echidna: Arc::new(EchidnaClient::new(&config.echidna)),
    })
}

/// Execute `query` and return the response as JSON (data + errors).
async fn run(schema: &echidnabot::api::graphql::EchidnabotSchema, query: &str) -> Value {
    serde_json::to_value(schema.execute(query).await).unwrap()
}

/// FleetDispatcher's mutation; `prover_line` is "" or "        prover: X,\n".
fn fleet_mutation(repo: &str, claim: &str, context: &str, prover_line: &str) -> String {
    format!(
        "mutation {{\n  submitProofObligation(input: {{\n    repo: \"{repo}\",\n    \
         claim: \"{claim}\",\n    context: \"{context}\",\n{prover_line}  }}) {{\n    \
         success\n    proofId\n  }}\n}}\n"
    )
}

const LEARNING_SCHEDULER_MUTATION: &str = r#"mutation {
  submitProofObligation(input: {
    repo: "hyperpolymath/requeue",
    claim: "requeue-of-attempt-42",
    context: "strategy-shift class=lemma",
    prover: LEAN,
    inline: true
  }) { success proofId }
}
"#;

/// Assert a response has no errors and `success: true`; return the proofId.
fn accepted(resp: &Value) -> String {
    assert!(
        resp.get("errors")
            .is_none_or(|e| e.as_array().unwrap().is_empty()),
        "{resp}"
    );
    let payload = &resp["data"]["submitProofObligation"];
    assert_eq!(payload["success"], json!(true), "{resp}");
    payload["proofId"].as_str().unwrap().to_string()
}

#[tokio::test]
async fn fleet_dispatcher_obligation_with_prover_is_accepted_with_v8_id() {
    let schema = obligation_schema().await;
    let q = fleet_mutation(
        "hyperpolymath/echidnabot",
        "forall x, x = x",
        "pattern P1",
        "        prover: COQ,\n",
    );
    let id = uuid::Uuid::parse_str(&accepted(&run(&schema, &q).await)).unwrap();
    assert_eq!(
        id.get_version_num(),
        8,
        "proofId must be a UUIDv8 content id"
    );
}

#[tokio::test]
async fn fleet_dispatcher_obligation_without_prover_is_accepted() {
    let schema = obligation_schema().await;
    let q = fleet_mutation(
        "hyperpolymath/echidnabot",
        "forall x, x = x",
        "pattern P1",
        "",
    );
    accepted(&run(&schema, &q).await);
}

#[tokio::test]
async fn learning_scheduler_inline_requeue_is_accepted() {
    let schema = obligation_schema().await;
    accepted(&run(&schema, LEARNING_SCHEDULER_MUTATION).await);
}

#[tokio::test]
async fn resubmitting_an_obligation_is_idempotent() {
    let schema = obligation_schema().await;
    let q =
        "mutation { submitProofObligation(input: { repo: \"a/b\", claim: \"c\", context: \"d\" }) \
         { success proofId status newlyRecorded repoRegistered } }";
    let first = run(&schema, q).await;
    let second = run(&schema, q).await;
    assert_eq!(accepted(&first), accepted(&second));
    let p1 = &first["data"]["submitProofObligation"];
    let p2 = &second["data"]["submitProofObligation"];
    assert_eq!(p1["status"], json!("PENDING"));
    assert_eq!(p1["newlyRecorded"], json!(true));
    assert_eq!(p2["newlyRecorded"], json!(false));
    assert_eq!(p1["repoRegistered"], json!(false));
}

#[tokio::test]
async fn invalid_obligations_are_graphql_errors_not_success_false() {
    let schema = obligation_schema().await;
    let big = "x".repeat(echidnabot::api::graphql::MAX_OBLIGATION_FIELD_BYTES + 1);
    for q in [
        // Unknown prover enum value: rejected at validation.
        fleet_mutation("a/b", "c", "d", "        prover: LEAN4,\n"),
        // Not an owner/name slug.
        fleet_mutation("no-slash", "c", "d", ""),
        fleet_mutation("a/b/c", "c", "d", ""),
        // Empty claim.
        fleet_mutation("a/b", "  ", "d", ""),
        // Over the size cap.
        fleet_mutation("a/b", &big, "d", ""),
    ] {
        let resp = run(&schema, &q).await;
        let errors = resp["errors"].as_array().cloned().unwrap_or_default();
        assert!(
            !errors.is_empty(),
            "expected a GraphQL error for {q:.80}: {resp}"
        );
    }
}
