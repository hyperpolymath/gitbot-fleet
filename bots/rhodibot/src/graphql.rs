// SPDX-License-Identifier: MPL-2.0

//! GitHub GraphQL client for the writes rhodibot performs.
//!
//! # Why GraphQL rather than REST
//!
//! Everything the bot *writes* is a check run or an issue, and both are
//! first-class GraphQL mutations. The advantages over the REST equivalents are
//! concrete rather than aesthetic:
//!
//! - **One request per repository, not three.** A check run needs a repository
//!   node ID, which REST cannot give at all (REST's `id` is a numeric database
//!   ID — a different identifier). With GraphQL the caller can batch the node ID
//!   lookup with whatever else it is fetching.
//! - **A query costs roughly a point regardless of how many repositories it
//!   aliases.** Measured against the live API while planning the estate census:
//!   eight repositories in one query, cost 1. That matters at estate scale.
//! - **Errors arrive as data.** GraphQL returns a structured `errors` array
//!   alongside partial `data`, so a failed mutation is a typed error rather
//!   than an HTTP status to be interpreted.
//!
//! What it cannot do, verified by introspection of the live schema rather than
//! assumed: it cannot read or cancel **workflow runs** (no such query field or
//! mutation exists — that stays REST), and it cannot create a GitHub App.
//!
//! # Security considerations
//!
//! - The token is held in memory, sent only in the `Authorization` header, and
//!   never included in an error message.
//! - Repository node IDs are cached, keyed by `owner/name`.
//! - All strings that reach an issue body come from the caller; callers are
//!   expected to have passed them through [`crate::sanitize`].

use std::collections::HashMap;
use std::sync::Mutex;

use anyhow::{Context, Result, anyhow};
use serde::Deserialize;

use crate::app_auth::USER_AGENT;

/// Outcome of a completed check run, as GitHub's `CheckConclusionState`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Conclusion {
    /// Everything passed.
    Success,
    /// Something failed and the reader should look.
    Failure,
    /// Nothing to report; does not block anything.
    Neutral,
}

impl Conclusion {
    /// The GraphQL enum value.
    pub fn as_graphql(self) -> &'static str {
        match self {
            Self::Success => "SUCCESS",
            Self::Failure => "FAILURE",
            Self::Neutral => "NEUTRAL",
        }
    }
}

/// Everything needed to open a check run.
#[derive(Debug, Clone)]
pub struct CheckRun {
    /// Repository node ID (`R_kgDO...`), not `owner/name`.
    pub repository_id: String,
    /// Check name, shown in the PR checks list.
    pub name: String,
    /// Commit the check applies to.
    pub head_sha: String,
    /// How the check ended.
    pub conclusion: Conclusion,
    /// Short headline, e.g. "3 of 17 required files missing".
    pub title: String,
    /// Markdown body, the substance of the report.
    pub summary: String,
}

/// An issue this client created.
#[derive(Debug, Clone, Deserialize)]
pub struct CreatedIssue {
    /// GraphQL node ID.
    pub id: String,
    /// Issue number, for a human-readable reference.
    pub number: i64,
    /// Browser URL.
    pub url: String,
}

/// Repository node IDs, cached for the process lifetime.
///
/// GraphQL requires a node ID to address a repository, and resolving one costs
/// a request, so the result is kept.
#[derive(Default)]
pub struct NodeIdCache {
    ids: Mutex<HashMap<String, String>>,
}

impl NodeIdCache {
    /// Look up a cached node ID.
    pub fn get(&self, owner: &str, repo: &str) -> Option<String> {
        let key = format!("{owner}/{repo}");
        self.ids.lock().ok()?.get(&key).cloned()
    }

    /// Record a node ID.
    pub fn insert(&self, owner: &str, repo: &str, id: &str) {
        if let Ok(mut ids) = self.ids.lock() {
            ids.insert(format!("{owner}/{repo}"), id.to_string());
        }
    }
}

#[derive(Deserialize)]
struct GraphQlEnvelope {
    data: Option<serde_json::Value>,
    errors: Option<Vec<GraphQlError>>,
}

#[derive(Deserialize)]
struct GraphQlError {
    message: String,
}

/// A minimal GitHub GraphQL client.
pub struct GraphQLClient {
    client: reqwest::Client,
    endpoint: String,
    token: String,
    node_ids: NodeIdCache,
}

impl GraphQLClient {
    /// Build a client for an endpoint and an installation token.
    ///
    /// `api_url` is the REST base (`https://api.github.com`); the GraphQL
    /// endpoint is derived from it, so GitHub Enterprise installations work
    /// without extra configuration.
    pub fn new(api_url: &str, token: impl Into<String>) -> Self {
        let base = api_url.trim_end_matches('/');
        Self {
            client: reqwest::Client::new(),
            endpoint: format!("{base}/graphql"),
            token: token.into(),
            node_ids: NodeIdCache::default(),
        }
    }

    /// Build a client against an explicit GraphQL endpoint.
    pub fn with_endpoint(endpoint: impl Into<String>, token: impl Into<String>) -> Self {
        Self {
            client: reqwest::Client::new(),
            endpoint: endpoint.into(),
            token: token.into(),
            node_ids: NodeIdCache::default(),
        }
    }

    /// The GraphQL endpoint in use.
    pub fn endpoint(&self) -> &str {
        &self.endpoint
    }

    /// Resolve a repository's node ID, from cache when possible.
    pub async fn repository_node_id(&self, owner: &str, repo: &str) -> Result<String> {
        if let Some(id) = self.node_ids.get(owner, repo) {
            return Ok(id);
        }

        const QUERY: &str = "query($owner: String!, $name: String!) { \
                             repository(owner: $owner, name: $name) { id } }";
        let data = self
            .execute(QUERY, serde_json::json!({ "owner": owner, "name": repo }))
            .await
            .with_context(|| format!("resolving the node id for {owner}/{repo}"))?;

        let id = data
            .pointer("/repository/id")
            .and_then(|value| value.as_str())
            .ok_or_else(|| anyhow!("repository {owner}/{repo} has no node id (does it exist?)"))?;

        self.node_ids.insert(owner, repo, id);
        Ok(id.to_string())
    }

    /// Open a completed check run.
    pub async fn create_check_run(&self, check: &CheckRun) -> Result<String> {
        const MUTATION: &str = "mutation($input: CreateCheckRunInput!) { \
                                createCheckRun(input: $input) { checkRun { id } } }";

        let input = serde_json::json!({
            "repositoryId": check.repository_id,
            "name": check.name,
            "headSha": check.head_sha,
            "status": "COMPLETED",
            "conclusion": check.conclusion.as_graphql(),
            "output": {
                "title": check.title,
                "summary": check.summary,
            },
        });

        let data = self
            .execute(MUTATION, serde_json::json!({ "input": input }))
            .await
            .with_context(|| format!("creating the check run {:?}", check.name))?;

        data.pointer("/createCheckRun/checkRun/id")
            .and_then(|value| value.as_str())
            .map(str::to_string)
            .ok_or_else(|| anyhow!("createCheckRun returned no check run id"))
    }

    /// Open an issue.
    pub async fn create_issue(
        &self,
        repository_id: &str,
        title: &str,
        body: &str,
    ) -> Result<CreatedIssue> {
        const MUTATION: &str = "mutation($input: CreateIssueInput!) { \
                                createIssue(input: $input) { issue { id number url } } }";

        let input = serde_json::json!({
            "repositoryId": repository_id,
            "title": title,
            "body": body,
        });

        let data = self
            .execute(MUTATION, serde_json::json!({ "input": input }))
            .await
            .context("creating an issue")?;

        let issue = data
            .pointer("/createIssue/issue")
            .ok_or_else(|| anyhow!("createIssue returned no issue"))?;
        serde_json::from_value(issue.clone()).context("parsing the created issue")
    }

    /// Post a query or mutation and unwrap the envelope.
    async fn execute(
        &self,
        query: &str,
        variables: serde_json::Value,
    ) -> Result<serde_json::Value> {
        let body = serde_json::json!({ "query": query, "variables": variables });

        let response = self
            .client
            .post(&self.endpoint)
            .bearer_auth(&self.token)
            .header("Accept", "application/json")
            .header("User-Agent", USER_AGENT)
            .json(&body)
            .send()
            .await
            .context("posting a GraphQL request")?;

        let status = response.status();
        if !status.is_success() {
            // Deliberately no response body: it can echo the request, and the
            // request carries the token.
            return Err(anyhow!("GraphQL request failed with HTTP {status}"));
        }

        let envelope: GraphQlEnvelope = response
            .json()
            .await
            .context("parsing the GraphQL envelope")?;

        if let Some(errors) = envelope.errors.as_ref().filter(|errors| !errors.is_empty()) {
            let joined = errors
                .iter()
                .map(|error| error.message.as_str())
                .collect::<Vec<_>>()
                .join("; ");
            return Err(anyhow!("GraphQL returned an error: {joined}"));
        }

        envelope
            .data
            .ok_or_else(|| anyhow!("GraphQL returned neither data nor errors"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn conclusion_maps_to_the_graphql_enum() {
        assert_eq!(Conclusion::Success.as_graphql(), "SUCCESS");
        assert_eq!(Conclusion::Failure.as_graphql(), "FAILURE");
        assert_eq!(Conclusion::Neutral.as_graphql(), "NEUTRAL");
    }

    #[test]
    fn derives_the_graphql_endpoint_from_the_rest_base() {
        let client = GraphQLClient::new("https://api.github.com", "token");
        assert_eq!(client.endpoint(), "https://api.github.com/graphql");

        // a trailing slash must not produce a doubled separator
        let client = GraphQLClient::new("https://api.github.com/", "token");
        assert_eq!(client.endpoint(), "https://api.github.com/graphql");
    }

    #[test]
    fn node_id_cache_round_trips() {
        let cache = NodeIdCache::default();
        assert_eq!(cache.get("hyperpolymath", "ubicity"), None);
        cache.insert("hyperpolymath", "ubicity", "R_kgDOABCDEF");
        assert_eq!(
            cache.get("hyperpolymath", "ubicity"),
            Some("R_kgDOABCDEF".to_string())
        );
        assert_eq!(cache.get("hyperpolymath", "elsewhere"), None);
    }

    #[test]
    fn created_issue_deserializes_from_the_mutation_shape() {
        let payload = serde_json::json!({
            "id": "I_kwDOABCDEF",
            "number": 42,
            "url": "https://github.com/hyperpolymath/ubicity/issues/42"
        });
        let issue: CreatedIssue =
            serde_json::from_value(payload).expect("matches the GraphQL response shape");
        assert_eq!(issue.number, 42);
        assert_eq!(issue.id, "I_kwDOABCDEF");
    }

    // ── HTTP flows, against a mock GraphQL endpoint ──────────────────────────

    use wiremock::matchers::{header, method, path};
    use wiremock::{Mock, MockServer, ResponseTemplate};

    fn client_for(server: &MockServer) -> GraphQLClient {
        GraphQLClient::with_endpoint(server.uri(), "ghs_test_token")
    }

    fn ok(payload: serde_json::Value) -> ResponseTemplate {
        ResponseTemplate::new(200).set_body_json(serde_json::json!({ "data": payload }))
    }

    #[tokio::test]
    async fn resolves_and_caches_a_repository_node_id() {
        let server = MockServer::start().await;
        Mock::given(method("POST"))
            .and(path("/"))
            .respond_with(ok(serde_json::json!({
                "repository": { "id": "R_kgDOABCDEF" }
            })))
            .expect(1) // cached on the second call
            .mount(&server)
            .await;

        let client = client_for(&server);
        assert_eq!(
            client
                .repository_node_id("hyperpolymath", "ubicity")
                .await
                .expect("node id"),
            "R_kgDOABCDEF"
        );
        assert_eq!(
            client
                .repository_node_id("hyperpolymath", "ubicity")
                .await
                .expect("cached node id"),
            "R_kgDOABCDEF"
        );
    }

    #[tokio::test]
    async fn creates_a_check_run_with_the_expected_input() {
        let server = MockServer::start().await;
        Mock::given(method("POST"))
            .and(path("/"))
            .and(header("authorization", "Bearer ghs_test_token"))
            .respond_with(ok(serde_json::json!({
                "createCheckRun": { "checkRun": { "id": "CR_kwDOABCDEF" } }
            })))
            .expect(1)
            .mount(&server)
            .await;

        let client = client_for(&server);
        let id = client
            .create_check_run(&CheckRun {
                repository_id: "R_kgDOABCDEF".to_string(),
                name: "RSR compliance".to_string(),
                head_sha: "051e02f0000000000000000000000000000000000".to_string(),
                conclusion: Conclusion::Failure,
                title: "3 of 17 required files missing".to_string(),
                summary: "- `www/.well-known/security.txt` missing".to_string(),
            })
            .await
            .expect("check run created");
        assert_eq!(id, "CR_kwDOABCDEF");

        // the mutation must carry the enum value, not the Rust variant name
        let requests = server.received_requests().await.expect("recorded");
        let body = String::from_utf8_lossy(&requests[0].body).to_string();
        assert!(body.contains("createCheckRun"), "mutation name: {body}");
        assert!(body.contains("CreateCheckRunInput"), "variables: {body}");
        assert!(body.contains("FAILURE"), "conclusion enum: {body}");
        assert!(body.contains("R_kgDOABCDEF"), "repository id: {body}");
    }

    #[tokio::test]
    async fn creates_an_issue_and_parses_the_response() {
        let server = MockServer::start().await;
        Mock::given(method("POST"))
            .and(path("/"))
            .respond_with(ok(serde_json::json!({
                "createIssue": { "issue": {
                    "id": "I_kwDOABCDEF",
                    "number": 42,
                    "url": "https://github.com/hyperpolymath/ubicity/issues/42"
                }}
            })))
            .mount(&server)
            .await;

        let client = client_for(&server);
        let issue = client
            .create_issue("R_kgDOABCDEF", "RSR: missing www/.well-known/", "Body")
            .await
            .expect("issue created");
        assert_eq!(issue.number, 42);
        assert_eq!(issue.id, "I_kwDOABCDEF");
        assert!(issue.url.ends_with("/issues/42"));
    }

    #[tokio::test]
    async fn graphql_errors_are_surfaced_rather_than_looking_like_success() {
        // GitHub answers 200 with an `errors` array and NO data. Treating that
        // as success is how a bot silently reports nothing.
        let server = MockServer::start().await;
        Mock::given(method("POST"))
            .and(path("/"))
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
                "data": serde_json::Value::Null,
                "errors": [
                    { "message": "Could not resolve to a Repository with the name 'gone'." },
                    { "message": "second error" }
                ]
            })))
            .mount(&server)
            .await;

        let client = client_for(&server);
        let error = client
            .repository_node_id("hyperpolymath", "gone")
            .await
            .expect_err("must fail");
        // The whole chain, not just the outermost context: anyhow's plain
        // Display shows the context this crate added, while `{:#}` carries the
        // cause that actually explains the failure.
        let chain = format!("{error:#}");
        assert!(chain.contains("Could not resolve"), "first error: {chain}");
        assert!(
            chain.contains("second error"),
            "all errors reported: {chain}"
        );
        assert!(
            chain.contains("resolving the node id"),
            "context is preserved: {chain}"
        );
    }

    #[tokio::test]
    async fn an_http_failure_names_the_status_and_leaks_nothing() {
        let server = MockServer::start().await;
        Mock::given(method("POST"))
            .and(path("/"))
            .respond_with(ResponseTemplate::new(401).set_body_string("Bad credentials"))
            .mount(&server)
            .await;

        let client = client_for(&server);
        let error = client
            .repository_node_id("hyperpolymath", "ubicity")
            .await
            .expect_err("must fail");
        let text = format!("{error:#}"); // include the context chain
        assert!(text.contains("401"), "names the status: {text}");
        assert!(
            !text.contains("ghs_test_token"),
            "a token must never reach an error message: {text}"
        );
    }
}
