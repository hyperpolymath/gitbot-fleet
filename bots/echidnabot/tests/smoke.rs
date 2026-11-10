// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Smoke tests — fast sanity checks (<30 seconds total).
//!
//! Testing taxonomy category: Smoke.
//! Verifies: "does it start, does it respond, are the obvious paths alive?"
//! Runs before deeper tests in CI to fail-fast on build regressions.

use axum::{routing::get, Extension, Router};
use axum_test::TestServer;
use echidnabot::api::auth::{graphql_post_route, ApiCredentials, ApiGate, RepoScope};
use echidnabot::api::graphql::GraphQLState;
use echidnabot::api::rate_limit::WebhookRateLimiter;
use echidnabot::api::webhooks::AppState;
use echidnabot::api::{create_schema, webhook_router};
use echidnabot::config::Config;
use echidnabot::dispatcher::EchidnaClient;
use echidnabot::modes::ModeSelector;
use echidnabot::scheduler::JobScheduler;
use echidnabot::store::SqliteStore;
use std::sync::Arc;

// Test-only credentials (the `test-` prefix marks them for the secret scanner).
// The Rust rule of that scan has no rule for the prefix: the pragma marks the
// 2 values for it.
const ROUTE_TOKEN: &str = "test-route-credential-0123456789abcdef"; // scanner-allow: rust-secrets
const ADMIN_TOKEN: &str = "test-admin-credential-0123456789abcdef"; // scanner-allow: rust-secrets

async fn make_test_server() -> TestServer {
    let config = Arc::new(Config::default());
    let store = Arc::new(SqliteStore::new("sqlite::memory:").await.unwrap());
    let scheduler = Arc::new(JobScheduler::new(2, 10));
    let echidna = Arc::new(EchidnaClient::new(&config.echidna));

    let graphql_state = GraphQLState {
        store: store.clone(),
        scheduler: scheduler.clone(),
        echidna: echidna.clone(),
    };
    let schema = create_schema(graphql_state);

    let app_state = AppState {
        config: config.clone(),
        store,
        scheduler,
        rate_limiter: None,
        mode_selector: ModeSelector::default(),
    };

    let credentials = ApiCredentials::new(
        Some(ROUTE_TOKEN.to_string()),
        Some(ADMIN_TOKEN.to_string()),
        RepoScope::new(["hyperpolymath/*"]),
    )
    .unwrap();
    let gate = Arc::new(ApiGate::new(credentials, None));

    let app = Router::new()
        .route("/health", get(|| async { "OK" }))
        .route("/graphql", graphql_post_route(gate))
        .merge(webhook_router(app_state.clone()))
        .layer(Extension(schema))
        .with_state(app_state);

    TestServer::new(app)
}

#[tokio::test]
async fn smoke_health_endpoint_returns_200() {
    let server = make_test_server().await;
    let response = server.get("/health").await;
    response.assert_status_ok();
    response.assert_text("OK");
}

#[tokio::test]
async fn smoke_unknown_route_returns_404() {
    let server = make_test_server().await;
    let response = server.get("/nonexistent-route-xyz").await;
    response.assert_status_not_found();
}

#[tokio::test]
async fn smoke_github_webhook_route_exists() {
    let server = make_test_server().await;
    // No webhook secret configured → 200 (no verification) or non-panic
    let response = server
        .post("/webhooks/github")
        .bytes(b"{}".as_ref().into())
        .await;
    let status = response.status_code().as_u16();
    assert!(
        status != 404 && status < 500,
        "webhook route must exist and not panic, got {status}"
    );
}

#[tokio::test]
async fn smoke_gitlab_webhook_route_exists() {
    let server = make_test_server().await;
    let response = server
        .post("/webhooks/gitlab")
        .bytes(b"{}".as_ref().into())
        .await;
    let status = response.status_code().as_u16();
    assert!(
        status != 404 && status < 500,
        "gitlab webhook must exist, got {status}"
    );
}

#[tokio::test]
async fn smoke_bitbucket_webhook_route_exists() {
    let server = make_test_server().await;
    let response = server
        .post("/webhooks/bitbucket")
        .bytes(b"{}".as_ref().into())
        .await;
    let status = response.status_code().as_u16();
    assert!(
        status != 404 && status < 500,
        "bitbucket webhook must exist, got {status}"
    );
}

#[tokio::test]
async fn smoke_graphql_typename_query() {
    let server = make_test_server().await;
    let response = server
        .post("/graphql")
        .authorization_bearer(ADMIN_TOKEN)
        .json(&serde_json::json!({ "query": "{ __typename }" }))
        .await;
    response.assert_status_ok();
    let body: serde_json::Value = response.json();
    assert!(
        body.get("data").is_some(),
        "must return data field, got: {body}"
    );
    assert!(
        body.get("errors").is_none(),
        "must not return errors, got: {body}"
    );
}

#[tokio::test]
async fn smoke_graphql_repositories_empty() {
    let server = make_test_server().await;
    let response = server
        .post("/graphql")
        .authorization_bearer(ADMIN_TOKEN)
        .json(&serde_json::json!({ "query": "{ repositories { id name } }" }))
        .await;
    response.assert_status_ok();
    let body: serde_json::Value = response.json();
    assert!(
        body.get("errors").is_none(),
        "repositories on empty DB must not error, got: {body}"
    );
    assert_eq!(
        body["data"]["repositories"],
        serde_json::json!([]),
        "empty DB must return empty list"
    );
}

#[tokio::test]
async fn smoke_rate_limiting_returns_429_at_limit() {
    let config = Arc::new(Config::default());
    let store = Arc::new(SqliteStore::new("sqlite::memory:").await.unwrap());
    let scheduler = Arc::new(JobScheduler::new(2, 10));
    let echidna = Arc::new(EchidnaClient::new(&config.echidna));

    let graphql_state = GraphQLState {
        store: store.clone(),
        scheduler: scheduler.clone(),
        echidna,
    };
    let schema = create_schema(graphql_state);

    let app_state = AppState {
        config,
        store,
        scheduler,
        rate_limiter: Some(Arc::new(WebhookRateLimiter::new(2))),
        mode_selector: ModeSelector::default(),
    };

    let app = Router::new()
        .route("/health", get(|| async { "OK" }))
        .merge(webhook_router(app_state.clone()))
        .layer(Extension(schema))
        .with_state(app_state);

    // into_make_service_with_connect_info required for ConnectInfo extractor in middleware
    let server = TestServer::new(app.into_make_service_with_connect_info::<std::net::SocketAddr>());

    // First 2 must pass (limit=2, all from same loopback IP in axum-test)
    for i in 0..2 {
        let r = server
            .post("/webhooks/github")
            .bytes(b"{}".as_ref().into())
            .await;
        assert_ne!(
            r.status_code(),
            429,
            "request {i} should not yet be rate-limited"
        );
    }
    // Third must be blocked
    let r = server
        .post("/webhooks/github")
        .bytes(b"{}".as_ref().into())
        .await;
    assert_eq!(
        r.status_code(),
        429,
        "request 3 must be rate-limited (limit=2)"
    );
    assert!(
        r.headers().get("Retry-After").is_some(),
        "rate-limited response must include Retry-After header"
    );
}

#[tokio::test]
async fn smoke_graphql_without_credential_is_401() {
    let server = make_test_server().await;
    let response = server
        .post("/graphql")
        .json(&serde_json::json!({ "query": "{ __typename }" }))
        .await;
    response.assert_status(axum::http::StatusCode::UNAUTHORIZED);
    assert_eq!(
        response.header("www-authenticate").to_str().unwrap(),
        "Bearer"
    );
}

#[tokio::test]
async fn smoke_graphql_with_wrong_credential_is_401() {
    let server = make_test_server().await;
    let response = server
        .post("/graphql")
        .authorization_bearer("test-not-a-configured-credential-000000")
        .json(&serde_json::json!({ "query": "{ __typename }" }))
        .await;
    response.assert_status(axum::http::StatusCode::UNAUTHORIZED);
}

#[tokio::test]
async fn smoke_graphql_route_credential_cannot_rewrite_webhook_secret() {
    let server = make_test_server().await;
    let response = server
        .post("/graphql")
        .authorization_bearer(ROUTE_TOKEN)
        // The nil UUID is a well-formed ID that names no repository. It comes
        // from the type: the estate UUID gate refuses a literal that is not
        // version 8.
        .json(&serde_json::json!({
            "query": format!(
                "mutation {{ updateRepoSettings(repoId: \"{}\", \
                 settings: {{ webhookSecret: \"forged\" }}) {{ id }} }}",
                uuid::Uuid::nil()
            )
        }))
        .await;
    response.assert_status_ok();
    let body: serde_json::Value = response.json();
    assert_eq!(
        body["errors"][0]["extensions"]["code"],
        serde_json::json!("FORBIDDEN"),
        "{body}"
    );
}

#[tokio::test]
async fn smoke_graphql_route_credential_submits_in_scope() {
    let server = make_test_server().await;
    let response = server
        .post("/graphql")
        .authorization_bearer(ROUTE_TOKEN)
        .json(&serde_json::json!({
            "query": "mutation { submitProofObligation(input: { repo: \"hyperpolymath/hypatia\", \
                      claim: \"forall x, x = x\", context: \"smoke\" }) { success } }"
        }))
        .await;
    response.assert_status_ok();
    let body: serde_json::Value = response.json();
    assert_eq!(
        body["data"]["submitProofObligation"]["success"],
        serde_json::json!(true),
        "{body}"
    );
}

#[tokio::test]
async fn smoke_graphql_rate_limit_returns_429() {
    let config = Arc::new(Config::default());
    let store = Arc::new(SqliteStore::new("sqlite::memory:").await.unwrap());
    let scheduler = Arc::new(JobScheduler::new(2, 10));
    let echidna = Arc::new(EchidnaClient::new(&config.echidna));
    let schema = create_schema(GraphQLState {
        store: store.clone(),
        scheduler: scheduler.clone(),
        echidna,
    });
    let app_state = AppState {
        config,
        store,
        scheduler,
        rate_limiter: None,
        mode_selector: ModeSelector::default(),
    };
    let credentials =
        ApiCredentials::new(None, Some(ADMIN_TOKEN.to_string()), RepoScope::default()).unwrap();
    let gate = Arc::new(ApiGate::new(credentials, Some(2)));
    let app = Router::new()
        .route("/graphql", graphql_post_route(gate))
        .layer(Extension(schema))
        .with_state(app_state);
    let server = TestServer::new(app.into_make_service_with_connect_info::<std::net::SocketAddr>());

    for i in 0..2 {
        let r = server
            .post("/graphql")
            .authorization_bearer(ADMIN_TOKEN)
            .json(&serde_json::json!({ "query": "{ __typename }" }))
            .await;
        assert_eq!(r.status_code(), 200, "request {i} should pass");
    }
    // An authenticated caller over the limit is refused...
    let r = server
        .post("/graphql")
        .authorization_bearer(ADMIN_TOKEN)
        .json(&serde_json::json!({ "query": "{ __typename }" }))
        .await;
    r.assert_status(axum::http::StatusCode::TOO_MANY_REQUESTS);
    // ...and, limited before authentication, so is an unauthenticated flood.
    let r = server
        .post("/graphql")
        .json(&serde_json::json!({ "query": "{ __typename }" }))
        .await;
    r.assert_status(axum::http::StatusCode::TOO_MANY_REQUESTS);
}
