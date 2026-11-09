// SPDX-License-Identifier: MPL-2.0
//! Exercise the HTTP gate independently of GraphQL and database setup.
use axum::{
    extract::ConnectInfo, http::StatusCode, middleware, routing::post, Extension, Router,
};
use axum_test::TestServer;
use echidnabot::api::auth::{api_gate_middleware, ApiCredentials, ApiGate, Principal, RepoScope};
use std::{net::SocketAddr, sync::Arc};

const TOKEN: &str = "test-admin-credential-0123456789abcdef";

fn server(limit: Option<u32>, peer: Option<SocketAddr>) -> TestServer {
    let credentials = ApiCredentials::new(None, Some(TOKEN.into()), RepoScope::default()).unwrap();
    let mut app = Router::new()
        .route("/graphql", post(|Extension(principal): Extension<Principal>| async move {
            assert_eq!(principal, Principal::Admin);
            "resolver reached"
        }))
        .layer(middleware::from_fn_with_state(
            Arc::new(ApiGate::new(credentials, limit)), api_gate_middleware,
        ));
    if let Some(peer) = peer {
        app = app.layer(Extension(ConnectInfo(peer)));
    }
    TestServer::new(app)
}

#[tokio::test]
async fn invalid_credentials_consume_the_budget_before_authentication() {
    let server = server(Some(1), Some("192.0.2.1:1234".parse().unwrap()));
    let denied = server.post("/graphql").authorization_bearer("wrong").await;
    denied.assert_status(StatusCode::UNAUTHORIZED);
    assert_eq!(denied.header("www-authenticate"), "Bearer");
    let limited = server.post("/graphql").authorization_bearer(TOKEN).await;
    limited.assert_status(StatusCode::TOO_MANY_REQUESTS);
    assert_eq!(limited.header("retry-after"), "60");
}

#[tokio::test]
async fn missing_connect_info_skips_limiting_but_still_requires_authentication() {
    let server = server(Some(0), None);
    server.post("/graphql").await.assert_status(StatusCode::UNAUTHORIZED);
    for _ in 0..3 {
        server.post("/graphql").authorization_bearer(TOKEN).await.assert_text("resolver reached");
    }
}

#[tokio::test]
async fn disabled_limiter_allows_repeated_authenticated_requests() {
    let server = server(None, Some("[2001:db8::1]:1234".parse().unwrap()));
    for _ in 0..3 {
        server.post("/graphql").authorization_bearer(TOKEN).await.assert_text("resolver reached");
    }
}

#[tokio::test]
async fn zero_budget_with_a_peer_refuses_even_valid_credentials() {
    let server = server(Some(0), Some("192.0.2.1:1234".parse().unwrap()));
    server.post("/graphql").authorization_bearer(TOKEN).await
        .assert_status(StatusCode::TOO_MANY_REQUESTS);
}
