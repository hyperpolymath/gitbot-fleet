// SPDX-License-Identifier: MPL-2.0
//! Live trace: echidnabot's ECHIDNA client against a real `echidna server`.
//!
//! Skipped unless `ECHIDNABOT_LIVE_ECHIDNA_URL` names a running server
//! (e.g. `http://127.0.0.1:8081`, the `echidna server` default). Each check
//! pairs a positive case with a planted negative control, so a client that
//! always answered "verified" would fail here.
//!
//! Run: `ECHIDNABOT_LIVE_ECHIDNA_URL=http://127.0.0.1:8081 cargo test --test live_echidna`

use echidna_core_spark::axiom_tracker::DangerLevel;
use echidnabot::config::{EchidnaApiMode, EchidnaConfig};
use echidnabot::dispatcher::echidna_client::{EchidnaClient, MIN_ECHIDNA_VERSION};
use echidnabot::dispatcher::{ProofStatus, ProverKind};

/// Client for the live server named by the environment, or `None` to skip.
fn live_client() -> Option<EchidnaClient> {
    let url = std::env::var("ECHIDNABOT_LIVE_ECHIDNA_URL").ok()?;
    let config = EchidnaConfig {
        endpoint: format!("{url}/"),
        rest_endpoint: url,
        mode: EchidnaApiMode::Rest,
        timeout_secs: 120,
    };
    Some(EchidnaClient::new(&config))
}

/// Whether `bin` resolves on `PATH`.
fn on_path(bin: &str) -> bool {
    std::env::var_os("PATH")
        .map(|p| std::env::split_paths(&p).any(|d| d.join(bin).is_file()))
        .unwrap_or(false)
}

/// The REST handshake reads a version >= the minimum and ECHIDNA's prover names.
#[tokio::test]
async fn live_handshake_reads_version_and_prover_list() {
    let Some(client) = live_client() else {
        eprintln!("skipped: ECHIDNABOT_LIVE_ECHIDNA_URL not set");
        return;
    };
    let hs = client
        .handshake()
        .await
        .expect("handshake against live echidna");
    let min = semver::Version::parse(MIN_ECHIDNA_VERSION).unwrap();
    assert!(hs.version >= min, "version {} below {}", hs.version, min);
    assert!(
        hs.provers.iter().any(|p| p == "Z3"),
        "Z3 missing: {:?}",
        hs.provers
    );
    // Name resolution must use the server's spelling, not the slug.
    assert_eq!(client.echidna_name(&ProverKind::new("z3")), "Z3");
    assert_eq!(client.echidna_name(&ProverKind::new("cvc5")), "CVC5");
}

/// REST verify: a true Z3 obligation verifies; a planted false one fails.
#[tokio::test]
async fn live_verify_z3_true_goal_and_planted_false_goal() {
    let Some(client) = live_client() else {
        eprintln!("skipped: ECHIDNABOT_LIVE_ECHIDNA_URL not set");
        return;
    };
    if !on_path("z3") {
        eprintln!("skipped: z3 not on PATH");
        return;
    }
    client.ensure_handshake().await.expect("handshake");
    let z3 = ProverKind::new("z3");

    // Negated obligation x+0=x is unsat -> discharged.
    let good = "(declare-const x Int)\n(assert (not (= (+ x 0) x)))\n(check-sat)\n";
    let r = client.verify_proof(&z3, good).await.expect("verify good");
    assert_eq!(r.status, ProofStatus::Verified, "{r:?}");

    // Planted control: negated x+1=x is sat -> must NOT verify.
    let bad = "(declare-const x Int)\n(assert (not (= (+ x 1) x)))\n(check-sat)\n";
    let r = client.verify_proof(&z3, bad).await.expect("verify bad");
    assert_eq!(r.status, ProofStatus::Failed, "{r:?}");
}

/// REST verify: a true Coq proof verifies, a false one does not, and an
/// `Admitted` proof is flagged by the axiom scan.
#[tokio::test]
async fn live_verify_coq_true_goal_and_planted_false_goal() {
    let Some(client) = live_client() else {
        eprintln!("skipped: ECHIDNABOT_LIVE_ECHIDNA_URL not set");
        return;
    };
    if !on_path("coqc") {
        eprintln!("skipped: coqc not on PATH");
        return;
    }
    client.ensure_handshake().await.expect("handshake");
    let coq = ProverKind::new("coq");

    let good = "Theorem t : True. Proof. exact I. Qed.";
    let r = client.verify_proof(&coq, good).await.expect("verify good");
    assert_eq!(r.status, ProofStatus::Verified, "{r:?}");

    let bad = "Theorem t : 1 = 2. Proof. reflexivity. Qed.";
    let r = client.verify_proof(&coq, bad).await.expect("verify bad");
    assert_ne!(r.status, ProofStatus::Verified, "{r:?}");

    // `Admitted` is reported PROVED by echidna's /api/verify (upstream gap);
    // echidnabot's source scan must still flag it so trust is capped.
    let admitted = "Lemma l : False. Admitted.";
    let r = client
        .verify_proof(&coq, admitted)
        .await
        .expect("verify admitted");
    let axioms = r.axioms.expect("axiom report");
    assert_ne!(
        axioms.worst_danger,
        DangerLevel::Safe,
        "Admitted not flagged: {axioms:?}"
    );
}

/// GraphQL verify against `echidna-graphql`: true goal verifies, false fails.
#[tokio::test]
async fn live_graphql_verify_z3_true_goal_and_planted_false_goal() {
    // GraphQL is the separate `echidna-graphql` binary (serves at `/`).
    let Ok(url) = std::env::var("ECHIDNABOT_LIVE_ECHIDNA_GRAPHQL_URL") else {
        eprintln!("skipped: ECHIDNABOT_LIVE_ECHIDNA_GRAPHQL_URL not set");
        return;
    };
    if !on_path("z3") {
        eprintln!("skipped: z3 not on PATH");
        return;
    }
    let client = EchidnaClient::new(&EchidnaConfig {
        endpoint: url,
        rest_endpoint: "http://127.0.0.1:9".to_string(),
        mode: EchidnaApiMode::Graphql,
        timeout_secs: 120,
    });
    let z3 = ProverKind::new("z3");
    let good = "(declare-const x Int)\n(assert (not (= (+ x 0) x)))\n(check-sat)\n";
    let r = client.verify_proof(&z3, good).await.expect("graphql good");
    assert_eq!(r.status, ProofStatus::Verified, "{r:?}");
    let bad = "(declare-const x Int)\n(assert (not (= (+ x 1) x)))\n(check-sat)\n";
    let r = client.verify_proof(&z3, bad).await.expect("graphql bad");
    assert_eq!(r.status, ProofStatus::Failed, "{r:?}");
}

/// The default config (auto mode) reaches whichever ECHIDNA binary is on 8081.
#[tokio::test]
async fn live_default_config_reaches_whichever_echidna_runs_on_8081() {
    // Set ECHIDNABOT_LIVE_DEFAULTS=1 with either `echidna server` or
    // `echidna-graphql` running on its default port.
    if std::env::var("ECHIDNABOT_LIVE_DEFAULTS").is_err() || !on_path("z3") {
        eprintln!("skipped: ECHIDNABOT_LIVE_DEFAULTS not set or z3 missing");
        return;
    }
    let client = EchidnaClient::new(&EchidnaConfig::default());
    let z3 = ProverKind::new("z3");
    let good = "(declare-const x Int)\n(assert (not (= (+ x 0) x)))\n(check-sat)\n";
    let r = client.verify_proof(&z3, good).await.expect("default good");
    assert_eq!(r.status, ProofStatus::Verified, "{r:?}");
    let bad = "(declare-const x Int)\n(assert (not (= (+ x 1) x)))\n(check-sat)\n";
    let r = client.verify_proof(&z3, bad).await.expect("default bad");
    assert_eq!(r.status, ProofStatus::Failed, "{r:?}");
}
