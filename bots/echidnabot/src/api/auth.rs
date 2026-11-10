// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! Authentication and authorization for the GraphQL API.
//!
//! Before this module the whole schema was served without authentication.
//! Any caller that could reach the port could rewrite a repository's webhook
//! secret (`updateRepoSettings`) and then forge webhooks, or write training
//! rows (`recordTacticOutcome`).
//!
//! The API now has two credentials, each read from its own environment
//! variable and never from a config file:
//!
//! * the *route* credential ([`ROUTE_TOKEN_ENV`]) may only call
//!   `submitProofObligation`, and only for repositories in
//!   `[api] route_repos`. It is the credential Hypatia holds;
//! * the *admin* credential ([`ADMIN_TOKEN_ENV`]) may call everything.
//!
//! Enforcement is in two layers. [`api_gate_middleware`] runs before the
//! schema: it rate-limits by peer address and answers 401 to a request with
//! no valid credential. Field guards ([`admin_only`], [`route_or_admin`],
//! [`check_repo_scope`]) run inside the schema and fail closed: a resolver
//! executed without a [`Principal`] in the request data is refused, so a
//! caller that executes the schema directly cannot bypass the gate.

use std::sync::Arc;

use async_graphql::{Context, ErrorExtensions};
use async_graphql_axum::{GraphQLRequest, GraphQLResponse};
use axum::{
    body::Body,
    extract::{ConnectInfo, State},
    http::{header, Request, StatusCode},
    middleware::{self, Next},
    response::{IntoResponse, Response},
    routing::{post, MethodRouter},
    Extension,
};
use std::net::SocketAddr;

use super::graphql::EchidnabotSchema;
use super::rate_limit::WebhookRateLimiter;
use crate::trust::solver_integrity::constant_time_eq;

// The 2 values below are names of environment variables, not credentials.
// The pragma says so to the Rust rule of the estate secret scan, which reads
// a `TOKEN` constant with a string literal as a possible secret.

/// Environment variable holding the route credential.
pub const ROUTE_TOKEN_ENV: &str = "ECHIDNABOT_API_ROUTE_TOKEN"; // scanner-allow: rust-secrets

/// Environment variable holding the administrative credential.
pub const ADMIN_TOKEN_ENV: &str = "ECHIDNABOT_API_ADMIN_TOKEN"; // scanner-allow: rust-secrets

/// Shortest credential accepted, in bytes. Generate one with
/// `openssl rand -hex 32` (64 characters).
pub const MIN_TOKEN_LEN: usize = 32;

/// Who a request is, once its credential has been checked.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Principal {
    /// The route credential: submit obligations for the repositories in scope.
    Route(RepoScope),
    /// The administrative credential: every query and mutation.
    Admin,
}

/// The repositories the route credential may submit obligations for.
///
/// Each pattern is either an exact `owner/name` slug or `owner/*`, which
/// matches every repository of that owner. There is no global wildcard: an
/// empty scope allows nothing.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct RepoScope {
    patterns: Vec<String>,
}

impl RepoScope {
    /// Build a scope from `owner/name` and `owner/*` patterns.
    pub fn new<I, S>(patterns: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        Self {
            patterns: patterns.into_iter().map(Into::into).collect(),
        }
    }

    /// Whether the scope lists no pattern at all.
    pub fn is_empty(&self) -> bool {
        self.patterns.is_empty()
    }

    /// Whether `repo` (an `owner/name` slug) is in scope.
    pub fn allows(&self, repo: &str) -> bool {
        let Some((owner, name)) = repo.split_once('/') else {
            return false;
        };
        if owner.is_empty() || name.is_empty() || name.contains('/') {
            return false;
        }
        self.patterns
            .iter()
            .any(|pattern| match pattern.split_once('/') {
                Some((p_owner, "*")) => !p_owner.is_empty() && p_owner == owner,
                Some(_) => pattern == repo,
                None => false,
            })
    }
}

/// A credential that cannot be used safely.
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum CredentialError {
    /// The named credential is shorter than [`MIN_TOKEN_LEN`] bytes.
    #[error("{0} is shorter than {MIN_TOKEN_LEN} bytes")]
    TooShort(&'static str),
    /// Both credentials are the same string, so roles cannot be told apart.
    #[error("{ROUTE_TOKEN_ENV} and {ADMIN_TOKEN_ENV} must differ")]
    NotDistinct,
}

/// The configured credentials and the route credential's scope.
#[derive(Clone, Default)]
pub struct ApiCredentials {
    route_token: Option<String>,
    admin_token: Option<String>,
    route_scope: RepoScope,
}

impl std::fmt::Debug for ApiCredentials {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ApiCredentials")
            .field("route_token", &crate::config::redacted(&self.route_token))
            .field("admin_token", &crate::config::redacted(&self.admin_token))
            .field("route_scope", &self.route_scope)
            .finish()
    }
}

impl ApiCredentials {
    /// Validate and hold the two credentials. Surrounding whitespace is
    /// removed, as it is from a presented token, so a value read from a file
    /// with a trailing newline still matches. An empty value counts as unset.
    ///
    /// # Errors
    /// [`CredentialError`] when a credential is too short or both are equal.
    pub fn new(
        route_token: Option<String>,
        admin_token: Option<String>,
        route_scope: RepoScope,
    ) -> Result<Self, CredentialError> {
        let normalise =
            |t: Option<String>| t.map(|t| t.trim().to_string()).filter(|t| !t.is_empty());
        let route_token = normalise(route_token);
        let admin_token = normalise(admin_token);
        if route_token
            .as_ref()
            .is_some_and(|t| t.len() < MIN_TOKEN_LEN)
        {
            return Err(CredentialError::TooShort(ROUTE_TOKEN_ENV));
        }
        if admin_token
            .as_ref()
            .is_some_and(|t| t.len() < MIN_TOKEN_LEN)
        {
            return Err(CredentialError::TooShort(ADMIN_TOKEN_ENV));
        }
        if let (Some(route), Some(admin)) = (&route_token, &admin_token) {
            if constant_time_eq(route.as_bytes(), admin.as_bytes()) {
                return Err(CredentialError::NotDistinct);
            }
        }
        Ok(Self {
            route_token,
            admin_token,
            route_scope,
        })
    }

    /// Read both credentials from [`ROUTE_TOKEN_ENV`] and [`ADMIN_TOKEN_ENV`].
    ///
    /// # Errors
    /// As for [`ApiCredentials::new`].
    pub fn from_env(route_scope: RepoScope) -> Result<Self, CredentialError> {
        Self::new(
            std::env::var(ROUTE_TOKEN_ENV).ok(),
            std::env::var(ADMIN_TOKEN_ENV).ok(),
            route_scope,
        )
    }

    /// Whether no credential is configured, so every API request is refused.
    pub fn is_empty(&self) -> bool {
        self.route_token.is_none() && self.admin_token.is_none()
    }

    /// Whether a route credential is configured.
    pub fn has_route_token(&self) -> bool {
        self.route_token.is_some()
    }

    /// The route credential's repository scope.
    pub fn route_scope(&self) -> &RepoScope {
        &self.route_scope
    }

    /// Map an `Authorization` header value to a principal.
    ///
    /// Accepts `Bearer <token>` with the scheme in any letter case. Both
    /// credentials are compared in constant time, so the response time does
    /// not reveal which one, if either, a guess was close to.
    pub fn authenticate(&self, authorization: Option<&str>) -> Option<Principal> {
        let value = authorization?.trim();
        let (scheme, token) = value.split_once(' ')?;
        if !scheme.eq_ignore_ascii_case("bearer") {
            return None;
        }
        let token = token.trim().as_bytes();
        if token.is_empty() {
            return None;
        }
        let is_admin = self
            .admin_token
            .as_ref()
            .is_some_and(|t| constant_time_eq(t.as_bytes(), token));
        let is_route = self
            .route_token
            .as_ref()
            .is_some_and(|t| constant_time_eq(t.as_bytes(), token));
        if is_admin {
            Some(Principal::Admin)
        } else if is_route {
            Some(Principal::Route(self.route_scope.clone()))
        } else {
            None
        }
    }
}

/// What the gate needs: the credentials and an optional per-IP limiter.
pub struct ApiGate {
    credentials: ApiCredentials,
    limiter: Option<WebhookRateLimiter>,
}

impl ApiGate {
    /// A gate over `credentials`, limiting each peer to `rate_limit_rpm`
    /// requests a minute when given.
    pub fn new(credentials: ApiCredentials, rate_limit_rpm: Option<u32>) -> Self {
        Self {
            credentials,
            limiter: rate_limit_rpm.map(WebhookRateLimiter::new),
        }
    }
}

/// Middleware in front of `POST /graphql`: rate limit, then authenticate.
///
/// A request over the limit gets 429. A request without a valid credential
/// gets 401 with `WWW-Authenticate: Bearer` and never reaches the schema. An
/// authenticated request carries its [`Principal`] onward in the request
/// extensions.
pub async fn api_gate_middleware(
    State(gate): State<Arc<ApiGate>>,
    mut request: Request<Body>,
    next: Next,
) -> Response {
    if let Some(limiter) = &gate.limiter {
        let peer_ip = request
            .extensions()
            .get::<ConnectInfo<SocketAddr>>()
            .map(|ci| ci.0.ip());
        if let Some(ip) = peer_ip {
            if !limiter.check_ip(ip) {
                tracing::warn!(%ip, limit = limiter.limit(), "GraphQL rate limit exceeded");
                return (
                    StatusCode::TOO_MANY_REQUESTS,
                    [(header::RETRY_AFTER, "60")],
                    "Rate limit exceeded",
                )
                    .into_response();
            }
        }
    }

    let authorization = request
        .headers()
        .get(header::AUTHORIZATION)
        .and_then(|v| v.to_str().ok());
    match gate.credentials.authenticate(authorization) {
        Some(principal) => {
            request.extensions_mut().insert(principal);
            next.run(request).await
        }
        None => (
            StatusCode::UNAUTHORIZED,
            [(header::WWW_AUTHENTICATE, "Bearer")],
            "A valid bearer credential is required",
        )
            .into_response(),
    }
}

/// `POST /graphql`: execute the request with the caller's principal attached.
pub async fn graphql_post(
    Extension(schema): Extension<EchidnabotSchema>,
    Extension(principal): Extension<Principal>,
    request: GraphQLRequest,
) -> GraphQLResponse {
    schema
        .execute(request.into_inner().data(principal))
        .await
        .into()
}

/// The guarded `POST /graphql` handler, ready to mount on any router.
///
/// The router must also provide `Extension<EchidnabotSchema>`.
pub fn graphql_post_route<S>(gate: Arc<ApiGate>) -> MethodRouter<S>
where
    S: Clone + Send + Sync + 'static,
{
    post(graphql_post).layer(middleware::from_fn_with_state(gate, api_gate_middleware))
}

fn unauthenticated() -> async_graphql::Error {
    async_graphql::Error::new("authentication required")
        .extend_with(|_, e| e.set("code", "UNAUTHENTICATED"))
}

fn forbidden(reason: &str) -> async_graphql::Error {
    async_graphql::Error::new(format!("forbidden: {reason}"))
        .extend_with(|_, e| e.set("code", "FORBIDDEN"))
}

/// Field guard: the administrative credential only.
pub fn admin_only(ctx: &Context<'_>) -> async_graphql::Result<()> {
    match ctx.data_opt::<Principal>() {
        Some(Principal::Admin) => Ok(()),
        Some(Principal::Route(_)) => {
            Err(forbidden("this field needs the administrative credential"))
        }
        None => Err(unauthenticated()),
    }
}

/// Field guard: either credential.
pub fn route_or_admin(ctx: &Context<'_>) -> async_graphql::Result<()> {
    match ctx.data_opt::<Principal>() {
        Some(_) => Ok(()),
        None => Err(unauthenticated()),
    }
}

/// Refuse `repo` unless the caller is the administrator or `repo` is in the
/// route credential's scope.
pub fn check_repo_scope(ctx: &Context<'_>, repo: &str) -> async_graphql::Result<()> {
    match ctx.data_opt::<Principal>() {
        Some(Principal::Admin) => Ok(()),
        Some(Principal::Route(scope)) if scope.allows(repo) => Ok(()),
        Some(Principal::Route(_)) => Err(forbidden(
            "repository is outside the route credential's scope",
        )),
        None => Err(unauthenticated()),
    }
}

/// Whether `host` binds only the loopback interface.
pub fn is_loopback_host(host: &str) -> bool {
    let trimmed = host.trim_start_matches('[').trim_end_matches(']');
    if trimmed.eq_ignore_ascii_case("localhost") {
        return true;
    }
    trimmed
        .parse::<std::net::IpAddr>()
        .is_ok_and(|ip| ip.is_loopback())
}

#[cfg(test)]
mod tests {
    use super::*;

    const ROUTE: &str = "test-route-credential-0123456789abcdef";
    const ADMIN: &str = "test-admin-credential-0123456789abcdef";

    fn creds(scope: &[&str]) -> ApiCredentials {
        ApiCredentials::new(
            Some(ROUTE.to_string()),
            Some(ADMIN.to_string()),
            RepoScope::new(scope.iter().copied()),
        )
        .unwrap()
    }

    #[test]
    fn scope_matches_exact_and_owner_wildcard_only() {
        let scope = RepoScope::new(["hyperpolymath/*", "other/one"]);
        assert!(scope.allows("hyperpolymath/hypatia"));
        assert!(scope.allows("other/one"));
        assert!(!scope.allows("other/two"));
        assert!(!scope.allows("hyperpolymathx/hypatia"));
        assert!(!scope.allows("hyperpolymath/"));
        assert!(!scope.allows("hyperpolymath/a/b"));
        assert!(!scope.allows("hyperpolymath"));
        assert!(!RepoScope::new(["*"]).allows("a/b"));
        assert!(!RepoScope::new(["*/*"]).allows("a/b"));
        assert!(!RepoScope::default().allows("a/b"));
    }

    #[test]
    fn stored_credentials_are_trimmed_like_presented_ones() {
        // A value read from a secrets file often ends in a newline.
        let c = ApiCredentials::new(
            Some(format!("  {ROUTE}\n")),
            Some(format!("{ADMIN}\n")),
            RepoScope::new(["a/*"]),
        )
        .unwrap();
        assert_eq!(
            c.authenticate(Some(&format!("Bearer {ROUTE}"))),
            Some(Principal::Route(RepoScope::new(["a/*"])))
        );
        assert_eq!(
            c.authenticate(Some(&format!("Bearer {ADMIN}"))),
            Some(Principal::Admin)
        );
        // Whitespace alone is unset, not a short credential.
        let blank = ApiCredentials::new(Some(" \n".into()), None, RepoScope::default()).unwrap();
        assert!(blank.is_empty());
    }

    #[test]
    fn authenticate_maps_each_credential_to_its_role() {
        let c = creds(&["hyperpolymath/*"]);
        assert_eq!(
            c.authenticate(Some(&format!("Bearer {ADMIN}"))),
            Some(Principal::Admin)
        );
        assert_eq!(
            c.authenticate(Some(&format!("bearer {ROUTE}"))),
            Some(Principal::Route(RepoScope::new(["hyperpolymath/*"])))
        );
    }

    #[test]
    fn authenticate_refuses_missing_wrong_and_malformed_headers() {
        let c = creds(&[]);
        assert_eq!(c.authenticate(None), None);
        assert_eq!(c.authenticate(Some("")), None);
        assert_eq!(c.authenticate(Some("Bearer")), None);
        assert_eq!(c.authenticate(Some("Bearer ")), None);
        assert_eq!(c.authenticate(Some(&format!("Basic {ADMIN}"))), None);
        assert_eq!(c.authenticate(Some(ADMIN)), None);
        assert_eq!(
            c.authenticate(Some("Bearer not-a-configured-credential-xxxx")),
            None
        );
    }

    #[test]
    fn no_credentials_authenticate_nobody() {
        let c = ApiCredentials::default();
        assert!(c.is_empty());
        assert_eq!(
            c.authenticate(Some("Bearer anything-at-all-anything-at-all")),
            None
        );
    }

    #[test]
    fn short_or_identical_credentials_are_refused() {
        assert_eq!(
            ApiCredentials::new(Some("short".into()), None, RepoScope::default()).unwrap_err(),
            CredentialError::TooShort(ROUTE_TOKEN_ENV)
        );
        assert_eq!(
            ApiCredentials::new(None, Some("short".into()), RepoScope::default()).unwrap_err(),
            CredentialError::TooShort(ADMIN_TOKEN_ENV)
        );
        assert_eq!(
            ApiCredentials::new(Some(ADMIN.into()), Some(ADMIN.into()), RepoScope::default())
                .unwrap_err(),
            CredentialError::NotDistinct
        );
        assert!(ApiCredentials::new(
            Some(String::new()),
            Some(String::new()),
            RepoScope::default()
        )
        .unwrap()
        .is_empty());
    }

    #[test]
    fn debug_never_prints_a_credential() {
        let rendered = format!("{:?}", creds(&[]));
        assert!(!rendered.contains(ROUTE));
        assert!(!rendered.contains(ADMIN));
        assert!(rendered.contains("<redacted>"));
    }

    #[test]
    fn loopback_hosts_are_recognised() {
        for host in [
            "127.0.0.1",
            "::1",
            "[::1]",
            "localhost",
            "LOCALHOST",
            "127.1.2.3",
        ] {
            assert!(is_loopback_host(host), "{host}");
        }
        for host in ["0.0.0.0", "::", "192.168.1.10", "example.org", ""] {
            assert!(!is_loopback_host(host), "{host}");
        }
    }
}
