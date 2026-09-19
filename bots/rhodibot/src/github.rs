// SPDX-License-Identifier: MPL-2.0

//! GitHub API client module
//!
//! # Authentication
//!
//! Credentials are resolved once, in [`GitHubClient::new`]:
//!
//! - a **GitHub App**, when `app_id` and a private key are both configured.
//!   Each request is then scoped to the repository it concerns: the app JWT is
//!   exchanged for an installation token for that repository (see
//!   [`crate::app_auth`]), because installation tokens do not carry across
//!   repositories.
//! - a **static token** from `GITHUB_TOKEN`, which is a single identity and
//!   does not expire.
//! - otherwise anonymous, which works on public repositories under a punitive
//!   rate limit.
//!
//! A half-configured App is *not* allowed to fall back to the anonymous path:
//! the client reports itself unready (see [`GitHubClient::readiness`]) and
//! refuses to send authenticated requests, so a misconfiguration surfaces at
//! start-up instead of as a permissions puzzle later.
//!
//! # Security considerations
//!
//! - Credentials are passed only to `bearer_auth()`. They are never logged,
//!   serialized, or included in error messages.
//! - File paths passed to content APIs are validated against path traversal.

use std::sync::Arc;

use anyhow::{Result, bail};
use reqwest::Client;
use serde::{Deserialize, Serialize};
use tracing::error;

use crate::app_auth::AppAuth;
use crate::config::Config;
use crate::sanitize;

/// How the client authenticates to the GitHub API.
enum Credentials {
    /// A `GITHUB_TOKEN`-style token: one identity, one lifetime, no exchange.
    /// `None` means unauthenticated, which is allowed but rate-limited hard.
    Static(Option<String>),
    /// A GitHub App. The app JWT is signed per call and exchanged for an
    /// installation token scoped to the repository being touched; [`AppAuth`]
    /// caches both the lookup and the token.
    App(Arc<AppAuth>),
    /// App credentials were configured but cannot be used. Requests fail
    /// instead of quietly falling back to anonymous access, which would
    /// surface later as a puzzling permissions problem somewhere else.
    Invalid(String),
}

/// GitHub API client
pub struct GitHubClient {
    client: Client,
    base_url: String,
    credentials: Credentials,
}

impl GitHubClient {
    /// Create a new GitHub client.
    ///
    /// Credentials come from configuration: a GitHub App when an app ID and a
    /// private key are both present, otherwise `GITHUB_TOKEN`, otherwise
    /// anonymous. Call [`Self::readiness`] at start-up so a half-configured App
    /// is rejected before it serves traffic.
    pub fn new(config: &Config) -> Self {
        Self {
            client: Client::new(),
            base_url: config.github_api_url.clone(),
            credentials: Self::resolve_credentials(config),
        }
    }

    /// Decide how to authenticate. Touches no network.
    fn resolve_credentials(config: &Config) -> Credentials {
        match (config.app_id, config.private_key.as_deref()) {
            (Some(app_id), Some(private_key)) => {
                match AppAuth::new(app_id, private_key, &config.github_api_url) {
                    Ok(app_auth) => Credentials::App(Arc::new(app_auth)),
                    Err(error) => Credentials::Invalid(format!(
                        "GitHub App credentials are unusable: {error:#}"
                    )),
                }
            }
            (Some(_), None) => Credentials::Invalid(
                "GITHUB_APP_ID is set but no private key was provided".to_string(),
            ),
            (None, Some(_)) => Credentials::Invalid(
                "a GitHub App private key was provided but GITHUB_APP_ID is missing".to_string(),
            ),
            (None, None) => Credentials::Static(std::env::var("GITHUB_TOKEN").ok()),
        }
    }

    /// Fail when this client cannot authenticate.
    ///
    /// Called at start-up: an unusable App configuration should stop the
    /// process, not every webhook that arrives afterwards.
    pub fn readiness(&self) -> Result<()> {
        match &self.credentials {
            Credentials::Invalid(reason) => bail!("{reason}"),
            _ => Ok(()),
        }
    }

    /// Describe the credential in use, for start-up logging. Carries no
    /// credential material.
    pub fn credential_mode(&self) -> &'static str {
        match &self.credentials {
            Credentials::Static(Some(_)) => "static token from GITHUB_TOKEN",
            Credentials::Static(None) => "anonymous (public repositories only)",
            Credentials::App(_) => "GitHub App installation tokens",
            Credentials::Invalid(_) => "misconfigured",
        }
    }

    /// Attach credentials to a request about one repository.
    ///
    /// Installation tokens are per repository, so the token is minted for the
    /// repository this request is about rather than reused across the estate.
    async fn authorize(
        &self,
        request: reqwest::RequestBuilder,
        owner: &str,
        repo: &str,
    ) -> Result<reqwest::RequestBuilder> {
        match &self.credentials {
            Credentials::Static(Some(token)) => Ok(request.bearer_auth(token)),
            Credentials::Static(None) => Ok(request),
            Credentials::App(app_auth) => {
                let installation = app_auth.installation_for_repository(owner, repo).await?;
                let token = app_auth.installation_token(installation).await?;
                Ok(request.bearer_auth(token))
            }
            Credentials::Invalid(reason) => {
                bail!("refusing to send an unauthenticated request: {reason}")
            }
        }
    }

    /// Get repository information
    pub async fn get_repository(&self, owner: &str, repo: &str) -> Result<Repository> {
        let url = format!("{}/repos/{}/{}", self.base_url, owner, repo);
        let request = self.authorize(self.client.get(&url), owner, repo).await?;

        let response = request
            .header("Accept", "application/vnd.github+json")
            .header("User-Agent", "rhodibot")
            .send()
            .await?;

        Ok(response.json().await?)
    }

    /// Get repository contents
    ///
    /// The `path` parameter is validated against path traversal before use.
    pub async fn get_contents(
        &self,
        owner: &str,
        repo: &str,
        path: &str,
    ) -> Result<Vec<ContentItem>> {
        sanitize::validate_file_path(path)?;
        let url = format!(
            "{}/repos/{}/{}/contents/{}",
            self.base_url, owner, repo, path
        );
        let request = self.authorize(self.client.get(&url), owner, repo).await?;

        let response = request
            .header("Accept", "application/vnd.github+json")
            .header("User-Agent", "rhodibot")
            .send()
            .await?;

        Ok(response.json().await?)
    }

    /// Every file path in a repository's default branch.
    ///
    /// One request, not one per path: the canon's file-presence criteria ask
    /// about a few dozen paths and a repository holds thousands of files, so
    /// asking path by path would spend the whole rate limit on one repository.
    ///
    /// A truncated response is an error, not a short answer. GitHub truncates
    /// the tree at 100,000 entries; a truncated list is missing files, and a
    /// missing file reads as an absent one, which would manufacture findings
    /// against a repository that has the file. Refusing is the honest option --
    /// there is no way to tell "absent" from "not fetched" in a partial tree.
    pub async fn tree_paths(&self, owner: &str, repo: &str) -> Result<Vec<String>> {
        let repository = self.get_repository(owner, repo).await?;
        self.tree_paths_at(owner, repo, &repository.default_branch)
            .await
    }

    /// Every file path at one ref.
    pub async fn tree_paths_at(
        &self,
        owner: &str,
        repo: &str,
        reference: &str,
    ) -> Result<Vec<String>> {
        sanitize::validate_file_path(reference)?;
        let url = format!(
            "{}/repos/{}/{}/git/trees/{}?recursive=1",
            self.base_url, owner, repo, reference
        );
        let request = self.authorize(self.client.get(&url), owner, repo).await?;

        let response = request
            .header("Accept", "application/vnd.github+json")
            .header("User-Agent", "rhodibot")
            .send()
            .await?;

        if !response.status().is_success() {
            bail!(
                "could not read the tree of {owner}/{repo} at {reference}: {}",
                response.status()
            );
        }

        let tree: TreeResponse = response.json().await?;
        if tree.truncated {
            bail!(
                "{owner}/{repo} at {reference} is too large for GitHub to list in one response, \
                 so the file list is incomplete. A partial list cannot tell an absent file from \
                 an unfetched one, and reporting the difference as a finding would be wrong. \
                 Check this repository with `--path` against a local clone instead."
            );
        }

        Ok(tree
            .tree
            .into_iter()
            .filter(|entry| entry.entry_type == "blob")
            .map(|entry| entry.path)
            .collect())
    }

    /// Read a file, treating "not found" as `None` rather than an error.
    ///
    /// The distinction matters for optional files: an absent
    /// `.machine_readable/rsr-profile.a2ml` means a repository declares no
    /// capabilities, which is an answer. Only a 404 is absence; anything else
    /// is a real failure and is reported as one.
    pub async fn get_file_content_if_present(
        &self,
        owner: &str,
        repo: &str,
        path: &str,
    ) -> Result<Option<String>> {
        sanitize::validate_file_path(path)?;
        let url = format!(
            "{}/repos/{}/{}/contents/{}",
            self.base_url, owner, repo, path
        );
        let request = self.authorize(self.client.get(&url), owner, repo).await?;

        let response = request
            .header("Accept", "application/vnd.github.raw+json")
            .header("User-Agent", "rhodibot")
            .send()
            .await?;

        if response.status() == reqwest::StatusCode::NOT_FOUND {
            return Ok(None);
        }
        if !response.status().is_success() {
            bail!(
                "could not read {path} from {owner}/{repo}: {}",
                response.status()
            );
        }

        Ok(Some(response.text().await?))
    }

    /// Check if a file exists
    ///
    /// The `path` parameter is validated against path traversal before use.
    pub async fn file_exists(&self, owner: &str, repo: &str, path: &str) -> bool {
        if sanitize::validate_file_path(path).is_err() {
            return false;
        }
        let url = format!(
            "{}/repos/{}/{}/contents/{}",
            self.base_url, owner, repo, path
        );
        let request = match self.authorize(self.client.head(&url), owner, repo).await {
            Ok(request) => request,
            Err(error) => {
                // This function reports a boolean by contract, but "cannot
                // authenticate" is not "the file is absent" -- reporting it as
                // absence would mass-report non-compliance. Say so loudly.
                error!("cannot authenticate a file-existence check: {error:#}");
                return false;
            }
        };

        request
            .header("User-Agent", "rhodibot")
            .send()
            .await
            .map(|r| r.status().is_success())
            .unwrap_or(false)
    }

    /// Get file content (decoded from base64)
    ///
    /// The `path` parameter is validated against path traversal before use.
    pub async fn get_file_content(&self, owner: &str, repo: &str, path: &str) -> Result<String> {
        sanitize::validate_file_path(path)?;
        let url = format!(
            "{}/repos/{}/{}/contents/{}",
            self.base_url, owner, repo, path
        );
        let request = self.authorize(self.client.get(&url), owner, repo).await?;

        let response = request
            .header("Accept", "application/vnd.github.raw+json")
            .header("User-Agent", "rhodibot")
            .send()
            .await?;

        if !response.status().is_success() {
            anyhow::bail!("Failed to get file content: {}", response.status());
        }

        Ok(response.text().await?)
    }

    /// Create an issue
    pub async fn create_issue(
        &self,
        owner: &str,
        repo: &str,
        title: &str,
        body: &str,
        labels: &[&str],
    ) -> Result<Issue> {
        let full = format!("{}/{}", owner, repo);
        gitbot_shared_context::registry_guard::check_github_write(
            &full,
            gitbot_shared_context::ExclusionAction::CreateIssue,
        )
        .map_err(|e| anyhow::anyhow!("{}", e))?;

        let url = format!("{}/repos/{}/{}/issues", self.base_url, owner, repo);
        let request = self.authorize(self.client.post(&url), owner, repo).await?;

        let payload = CreateIssue {
            title: title.to_string(),
            body: body.to_string(),
            labels: labels.iter().map(|s| s.to_string()).collect(),
        };

        let response = request
            .header("Accept", "application/vnd.github+json")
            .header("User-Agent", "rhodibot")
            .json(&payload)
            .send()
            .await?;

        Ok(response.json().await?)
    }

    /// Create a check run
    pub async fn create_check_run(
        &self,
        owner: &str,
        repo: &str,
        check_run: &CreateCheckRun,
    ) -> Result<CheckRun> {
        let full = format!("{}/{}", owner, repo);
        gitbot_shared_context::registry_guard::check_github_write(
            &full,
            gitbot_shared_context::ExclusionAction::CreateCheckRun,
        )
        .map_err(|e| anyhow::anyhow!("{}", e))?;

        let url = format!("{}/repos/{}/{}/check-runs", self.base_url, owner, repo);
        let request = self.authorize(self.client.post(&url), owner, repo).await?;

        let response = request
            .header("Accept", "application/vnd.github+json")
            .header("User-Agent", "rhodibot")
            .json(check_run)
            .send()
            .await?;

        Ok(response.json().await?)
    }
}

/// Repository information from the GitHub API.
///
/// Fields are deserialized from the API response schema and may not all be
/// used directly. They are retained for completeness and future use (e.g.,
/// language detection for CodeQL, topic-based policy selection).
#[derive(Debug, Deserialize)]
pub struct Repository {
    pub id: u64,
    pub name: String,
    pub full_name: String,
    pub description: Option<String>,
    pub default_branch: String,
    pub language: Option<String>,
    pub topics: Vec<String>,
    pub license: Option<License>,
}

#[derive(Debug, Deserialize)]
pub struct License {
    pub key: String,
    pub name: String,
    pub spdx_id: Option<String>,
}

/// Content item from the GitHub contents API (directory listings).
/// A tree listing, as the git trees API returns it.
#[derive(Debug, Deserialize)]
struct TreeResponse {
    tree: Vec<TreeEntry>,
    /// Absent from the payload when false.
    #[serde(default)]
    truncated: bool,
}

#[derive(Debug, Deserialize)]
struct TreeEntry {
    path: String,
    #[serde(rename = "type")]
    entry_type: String,
}

#[derive(Debug, Deserialize)]
pub struct ContentItem {
    pub name: String,
    pub path: String,
    #[serde(rename = "type")]
    pub item_type: String,
}

#[derive(Debug, Serialize)]
struct CreateIssue {
    title: String,
    body: String,
    labels: Vec<String>,
}

#[derive(Debug, Deserialize)]
pub struct Issue {
    pub id: u64,
    pub number: u64,
    pub title: String,
    pub html_url: String,
}

#[derive(Debug, Serialize)]
pub struct CreateCheckRun {
    pub name: String,
    pub head_sha: String,
    pub status: String,
    pub conclusion: Option<String>,
    pub output: Option<CheckRunOutput>,
}

#[derive(Debug, Serialize)]
pub struct CheckRunOutput {
    pub title: String,
    pub summary: String,
    pub text: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct CheckRun {
    pub id: u64,
    pub name: String,
    pub status: String,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::app_auth::tests::test_private_key_pem;
    use chrono::Duration;
    use wiremock::matchers::{header, method, path};
    use wiremock::{Mock, MockServer, ResponseTemplate};

    /// A config aimed at the mock server, carrying no credentials of its own.
    fn config_for(server: &MockServer) -> Config {
        Config {
            app_id: None,
            private_key: None,
            webhook_secret: None,
            github_api_url: server.uri(),
        }
    }

    /// Mount the two calls an App makes before it can act: finding the
    /// installation, then exchanging a JWT for a token.
    async fn mount_app_handshake(server: &MockServer, installation_id: u64, token: &str) {
        Mock::given(method("GET"))
            .and(path("/repos/acme/widgets/installation"))
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
                "id": installation_id
            })))
            .mount(server)
            .await;

        Mock::given(method("POST"))
            .and(path(format!(
                "/app/installations/{installation_id}/access_tokens"
            )))
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
                "token": token,
                "expires_at": (chrono::Utc::now() + Duration::hours(1)).to_rfc3339(),
            })))
            .mount(server)
            .await;
    }

    /// A file check that answers only to the expected token.
    async fn mount_file_check(server: &MockServer, expected_token: &str) {
        Mock::given(method("HEAD"))
            .and(path("/repos/acme/widgets/contents/README.adoc"))
            .and(header(
                "authorization",
                format!("Bearer {expected_token}").as_str(),
            ))
            .respond_with(ResponseTemplate::new(200))
            .mount(server)
            .await;
    }

    #[tokio::test]
    async fn app_credentials_are_exchanged_for_a_repository_scoped_token() {
        let server = MockServer::start().await;
        mount_app_handshake(&server, 42, "ghs_installation").await;
        // Answers only when the request carries the installation token, so a
        // failure here means the app JWT leaked through to the repository API.
        mount_file_check(&server, "ghs_installation").await;

        let mut config = config_for(&server);
        config.app_id = Some(1234);
        config.private_key = Some(test_private_key_pem());

        let client = GitHubClient::new(&config);
        assert!(client.readiness().is_ok());
        assert_eq!(client.credential_mode(), "GitHub App installation tokens");
        assert!(client.file_exists("acme", "widgets", "README.adoc").await);
    }

    #[tokio::test]
    async fn the_installation_lookup_and_token_are_reused_across_requests() {
        let server = MockServer::start().await;
        mount_app_handshake(&server, 42, "ghs_installation").await;
        mount_file_check(&server, "ghs_installation").await;

        let mut config = config_for(&server);
        config.app_id = Some(1234);
        config.private_key = Some(test_private_key_pem());
        let client = GitHubClient::new(&config);

        assert!(client.file_exists("acme", "widgets", "README.adoc").await);
        assert!(client.file_exists("acme", "widgets", "README.adoc").await);

        let requests = server.received_requests().await.expect("recorded");
        let lookups = requests
            .iter()
            .filter(|request| request.url.path() == "/repos/acme/widgets/installation")
            .count();
        let exchanges = requests
            .iter()
            .filter(|request| request.url.path().ends_with("/access_tokens"))
            .count();
        assert_eq!(
            lookups, 1,
            "the installation lookup is an API call; it must not repeat per request"
        );
        assert_eq!(exchanges, 1, "a valid installation token must be reused");
    }

    #[tokio::test]
    async fn a_half_configured_app_is_refused_rather_than_downgraded() {
        let server = MockServer::start().await;
        let mut config = config_for(&server);
        config.app_id = Some(1234); // deliberately no private key

        let client = GitHubClient::new(&config);
        assert_eq!(client.credential_mode(), "misconfigured");
        let error = client
            .readiness()
            .expect_err("an App without a key is unready");
        assert!(
            format!("{error:#}").contains("no private key"),
            "unexpected message: {error:#}"
        );

        assert!(!client.file_exists("acme", "widgets", "README.adoc").await);
        assert!(
            server
                .received_requests()
                .await
                .expect("recorded")
                .is_empty(),
            "an unauthenticated request must not be sent as a fallback"
        );
    }

    #[tokio::test]
    async fn an_unusable_private_key_is_reported_at_start_up() {
        let server = MockServer::start().await;
        let mut config = config_for(&server);
        config.app_id = Some(1234);
        config.private_key = Some("this is not a key".to_string());

        let client = GitHubClient::new(&config);
        let error = client.readiness().expect_err("an unusable key is unready");
        assert!(
            format!("{error:#}").contains("unusable"),
            "unexpected message: {error:#}"
        );
    }

    /// A repository whose default branch is `main`, as `/repos/{o}/{r}` reports.
    async fn mount_repository(server: &MockServer) {
        Mock::given(method("GET"))
            .and(path("/repos/acme/widgets"))
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
                "id": 1,
                "name": "widgets",
                "full_name": "acme/widgets",
                "description": null,
                "default_branch": "main",
                "language": null,
                "topics": [],
                "license": null
            })))
            .mount(server)
            .await;
    }

    #[tokio::test]
    async fn the_tree_listing_returns_files_and_not_directories() {
        let server = MockServer::start().await;
        mount_repository(&server).await;
        Mock::given(method("GET"))
            .and(path("/repos/acme/widgets/git/trees/main"))
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
                "truncated": false,
                "tree": [
                    { "path": "src", "type": "tree" },
                    { "path": "src/main.rs", "type": "blob" },
                    { "path": "README.adoc", "type": "blob" },
                    { "path": ".machine_readable/descriptiles/STATE.a2ml", "type": "blob" }
                ]
            })))
            .mount(&server)
            .await;

        let client = GitHubClient::new(&config_for(&server));
        let paths = client
            .tree_paths("acme", "widgets")
            .await
            .expect("the listing parses");

        assert_eq!(
            paths,
            vec![
                "src/main.rs",
                "README.adoc",
                ".machine_readable/descriptiles/STATE.a2ml"
            ],
            "directories are not files: counting `src` would not tell a check anything, and a \
             criterion naming a directory is satisfied by what is inside it"
        );
    }

    #[tokio::test]
    async fn a_truncated_tree_is_an_error_rather_than_a_short_list() {
        let server = MockServer::start().await;
        mount_repository(&server).await;
        Mock::given(method("GET"))
            .and(path("/repos/acme/widgets/git/trees/main"))
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
                "truncated": true,
                "tree": [{ "path": "README.adoc", "type": "blob" }]
            })))
            .mount(&server)
            .await;

        let client = GitHubClient::new(&config_for(&server));
        let error = client
            .tree_paths("acme", "widgets")
            .await
            .expect_err("a partial tree must not be reported as a complete one");

        let message = format!("{error:#}");
        assert!(
            message.contains("incomplete") && message.contains("--path"),
            "the refusal must say why it matters and what to do instead: {message}"
        );
    }

    #[tokio::test]
    async fn an_absent_optional_file_is_none_and_a_failure_is_an_error() {
        let server = MockServer::start().await;

        Mock::given(method("GET"))
            .and(path(
                "/repos/acme/widgets/contents/.machine_readable/rsr-profile.a2ml",
            ))
            .respond_with(ResponseTemplate::new(404).set_body_json(serde_json::json!({
                "message": "Not Found"
            })))
            .mount(&server)
            .await;

        Mock::given(method("GET"))
            .and(path(
                "/repos/acme/widgets/contents/.machine_readable/STATE.a2ml",
            ))
            .respond_with(ResponseTemplate::new(500).set_body_string("boom"))
            .mount(&server)
            .await;

        let client = GitHubClient::new(&config_for(&server));

        assert_eq!(
            client
                .get_file_content_if_present(
                    "acme",
                    "widgets",
                    ".machine_readable/rsr-profile.a2ml"
                )
                .await
                .expect("a 404 is an answer, not a failure"),
            None,
            "a repository with no profile declares no capabilities"
        );

        let error = client
            .get_file_content_if_present("acme", "widgets", ".machine_readable/STATE.a2ml")
            .await
            .expect_err("a server error must not read as \"the file is absent\"");
        assert!(format!("{error:#}").contains("500"), "{error:#}");
    }

    #[tokio::test]
    async fn a_static_token_is_still_used_for_repository_requests() {
        let server = MockServer::start().await;
        mount_file_check(&server, "ghs_static").await;

        // Set directly: the static path reads the environment, and mutating
        // process-wide env vars from a parallel test suite is a race.
        let mut client = GitHubClient::new(&config_for(&server));
        client.credentials = Credentials::Static(Some("ghs_static".to_string()));

        assert!(client.readiness().is_ok());
        assert_eq!(client.credential_mode(), "static token from GITHUB_TOKEN");
        assert!(client.file_exists("acme", "widgets", "README.adoc").await);
    }
}
