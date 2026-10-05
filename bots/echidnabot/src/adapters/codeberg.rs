// SPDX-License-Identifier: MPL-2.0
// Owner: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Codeberg / Forgejo / Gitea platform adapter (scaffold).
//!
//! # Status
//!
//! **SCAFFOLD ONLY** — issue #62, roadmap "LOW PRIORITY, gated on Codeberg
//! API stability". This module compiles, dispatches stubbed events, and
//! provides a minimal smoke-test seam. It is **not production-ready** —
//! several methods either return `Ok(())` no-ops or `Err(Unsupported)`
//! pending downstream design decisions.
//!
//! # Why one adapter for three platforms
//!
//! Codeberg runs Forgejo, which is a hard fork of Gitea. The REST API
//! surfaces are 95%+ shared at the URL/payload level (`/api/v1/...`),
//! diverging only on a handful of newer Forgejo-specific endpoints
//! (federation, quota, code-review extras) we do not touch here.
//!
//! The adapter therefore targets the **Gitea-compatible subset** of
//! Forgejo's API. It works against:
//!
//! - `https://codeberg.org` (Forgejo, Codeberg-hosted)
//! - Self-hosted Forgejo instances
//! - Self-hosted Gitea instances (degraded gracefully on Forgejo-only fields)
//!
//! # Webhook signature scheme
//!
//! Gitea / Forgejo / Codeberg emit `X-Gitea-Signature: <hex>` — raw
//! HMAC-SHA256 hex, **without** the `sha256=` prefix GitHub uses. The
//! verification helper lives in `crate::api::webhooks` and is shared with
//! GitHub's verifier (different header name, same HMAC).
//!
//! # API client choice
//!
//! No first-class Forgejo/Gitea Rust SDK exists with the maturity of
//! `octocrab`. The crates that do exist (`gitea-sdk`, `forgejo-api`) are
//! either pre-1.0 with breaking releases or unmaintained. This adapter
//! therefore uses `reqwest` + `serde_json` directly — the same pattern
//! the GitLab and Bitbucket adapters use. If a stable SDK lands, the
//! private methods can be migrated module-internally without changing
//! the `PlatformAdapter` trait surface.
//!
//! # TODOs
//!
//! - [ ] Full webhook payload decode (push / pull_request / issue_comment)
//! - [ ] Inline review comments via Gitea Reviews API (`POST .../reviews`
//!   with `comments[]` array; non-trivial because Gitea's review model
//!   differs from GitHub's per-comment model)
//! - [ ] Branch protection rule reads (for Regulator mode)
//! - [ ] App-token auth (currently personal access token only)
//! - [ ] Federation-aware repo IDs (Forgejo 7.x+ supports federated repos)
//! - [ ] Rate-limit-aware retry (Codeberg enforces stricter limits than GitHub)
//! - [ ] Smoke test against a real Codeberg instance (currently unit-only)

use async_trait::async_trait;

use super::{
    CheckConclusion, CheckRun, CheckRunId, CheckStatus, CommentId, IssueId, NewIssue,
    PlatformAdapter, PrId, RepoId, ReviewCommentLocation,
};
use crate::error::{Error, Result};

/// Default Codeberg host. Override via `CodebergAdapter::new(Some("..."))`
/// to point at a self-hosted Forgejo or Gitea instance.
const DEFAULT_BASE_URL: &str = "https://codeberg.org";

/// Codeberg / Forgejo / Gitea adapter.
///
/// Holds a `base_url` (e.g. `https://codeberg.org` or a self-hosted
/// Forgejo URL), an optional personal access token (read from the
/// `CODEBERG_TOKEN` env var by default), and a shared `reqwest` client.
pub struct CodebergAdapter {
    base_url: String,
    token: Option<String>,
    client: reqwest::Client,
}

impl CodebergAdapter {
    /// Construct a new adapter.
    ///
    /// `base_url` defaults to `https://codeberg.org`. Pass `Some("...")`
    /// to target a self-hosted Forgejo or Gitea instance. Trailing
    /// slashes are stripped to match GitLab/Bitbucket conventions.
    pub fn new(base_url: Option<&str>) -> Self {
        let base = base_url.unwrap_or(DEFAULT_BASE_URL);
        Self {
            base_url: base.trim_end_matches('/').to_string(),
            // TODO(#62): also honour `FORGEJO_TOKEN` / `GITEA_TOKEN`
            // when no Codeberg-specific token is set, so the adapter
            // works against the broader Forgejo/Gitea ecosystem
            // without bespoke env vars.
            token: std::env::var("CODEBERG_TOKEN").ok(),
            client: reqwest::Client::new(),
        }
    }

    /// HTTPS clone URL for the repo.
    fn repo_url(&self, repo: &RepoId) -> String {
        format!("{}/{}/{}.git", self.base_url, repo.owner, repo.name)
    }

    /// Gitea/Forgejo REST API base. The `/api/v1` prefix is stable
    /// across both forks; the Forgejo `/api/forgejo/v1` namespace is
    /// reserved for fork-only extensions we do not call.
    fn api_url(&self) -> String {
        format!("{}/api/v1", self.base_url)
    }

    /// `owner/name` repo path used by Gitea endpoints.
    fn repo_path(&self, repo: &RepoId) -> String {
        format!("{}/{}", repo.owner, repo.name)
    }
}

#[async_trait]
impl PlatformAdapter for CodebergAdapter {
    async fn clone_repo(&self, repo: &RepoId, commit: &str) -> Result<tempfile::TempDir> {
        let url = self.repo_url(repo);
        super::clone_revision(&url, commit).await
    }

    async fn create_check_run(&self, repo: &RepoId, check: CheckRun) -> Result<CheckRunId> {
        // Gitea/Forgejo expose commit statuses (NOT GitHub-style check
        // runs) at:
        //   POST /api/v1/repos/{owner}/{repo}/statuses/{sha}
        // Body: {state, target_url, description, context}
        // States: pending | success | error | failure | warning
        let token = self
            .token
            .as_ref()
            .ok_or_else(|| Error::Config("CODEBERG_TOKEN not set".to_string()))?;

        let url = format!(
            "{}/repos/{}/statuses/{}",
            self.api_url(),
            self.repo_path(repo),
            check.head_sha,
        );

        let (state, description) = match &check.status {
            CheckStatus::Completed {
                conclusion,
                summary,
            } => {
                let state = match conclusion {
                    CheckConclusion::Success => "success",
                    CheckConclusion::Failure => "failure",
                    CheckConclusion::Cancelled => "error",
                    CheckConclusion::Neutral | CheckConclusion::Skipped => "warning",
                    CheckConclusion::TimedOut | CheckConclusion::ActionRequired => "error",
                };
                (state, summary.clone())
            }
            CheckStatus::InProgress => ("pending", String::new()),
            CheckStatus::Queued => ("pending", String::new()),
        };

        let payload = serde_json::json!({
            "state": state,
            "context": check.name,
            "description": description,
            "target_url": check.details_url,
        });

        let response = self
            .client
            .post(&url)
            .header("Authorization", format!("token {}", token))
            .json(&payload)
            .send()
            .await
            .map_err(|e| Error::GitHub(format!("Codeberg statuses API: {}", e)))?;

        if !response.status().is_success() {
            return Err(Error::GitHub(format!(
                "Codeberg statuses API returned {}",
                response.status()
            )));
        }

        let data: serde_json::Value = response
            .json()
            .await
            .map_err(|e| Error::GitHub(format!("Codeberg statuses response: {}", e)))?;

        Ok(CheckRunId(
            data["id"]
                .as_u64()
                .map(|id| id.to_string())
                .unwrap_or_else(|| "0".to_string()),
        ))
    }

    async fn update_check_run(&self, _id: CheckRunId, _status: CheckStatus) -> Result<()> {
        // Gitea/Forgejo commit statuses are append-only (like GitLab
        // pipeline statuses and Bitbucket build statuses). To "update",
        // POST a new status with the same `context`; consumers display
        // the latest. The trait method is intentionally a no-op here —
        // callers should re-invoke `create_check_run` instead.
        Ok(())
    }

    async fn create_comment(&self, repo: &RepoId, pr: PrId, body: &str) -> Result<CommentId> {
        // Gitea/Forgejo issue comments (which work for both Issues and
        // PRs, since PRs are issues in this model) at:
        //   POST /api/v1/repos/{owner}/{repo}/issues/{index}/comments
        let token = self
            .token
            .as_ref()
            .ok_or_else(|| Error::Config("CODEBERG_TOKEN not set".to_string()))?;

        let url = format!(
            "{}/repos/{}/issues/{}/comments",
            self.api_url(),
            self.repo_path(repo),
            pr.0,
        );

        let payload = serde_json::json!({
            "body": body,
        });

        let response = self
            .client
            .post(&url)
            .header("Authorization", format!("token {}", token))
            .json(&payload)
            .send()
            .await
            .map_err(|e| Error::GitHub(format!("Codeberg comments API: {}", e)))?;

        if !response.status().is_success() {
            return Err(Error::GitHub(format!(
                "Codeberg comments API returned {}",
                response.status()
            )));
        }

        let data: serde_json::Value = response
            .json()
            .await
            .map_err(|e| Error::GitHub(format!("Codeberg comments response: {}", e)))?;

        Ok(CommentId(
            data["id"]
                .as_u64()
                .map(|id| id.to_string())
                .ok_or_else(|| Error::GitHub("Missing id in Codeberg response".to_string()))?,
        ))
    }

    async fn create_issue(&self, repo: &RepoId, issue: NewIssue) -> Result<IssueId> {
        // POST /api/v1/repos/{owner}/{repo}/issues
        // Labels in Gitea are *numeric IDs*, not strings — so the
        // `issue.labels` Vec<String> would need a name→id lookup round
        // trip. For the scaffold we pass labels as a hint in the body
        // and TODO the proper label resolution.
        let token = self
            .token
            .as_ref()
            .ok_or_else(|| Error::Config("CODEBERG_TOKEN not set".to_string()))?;

        let url = format!("{}/repos/{}/issues", self.api_url(), self.repo_path(repo),);

        // TODO(#62): resolve label names → numeric IDs via
        //   GET /api/v1/repos/{owner}/{repo}/labels
        // and pass them in the `labels` array. For now, smuggle the
        // requested labels into the body so the issue is still useful
        // to a human reader.
        let body_with_labels = if issue.labels.is_empty() {
            issue.body.clone()
        } else {
            format!(
                "{}\n\n---\n_Requested labels (not yet resolved):_ {}",
                issue.body,
                issue.labels.join(", "),
            )
        };

        let payload = serde_json::json!({
            "title": issue.title,
            "body": body_with_labels,
        });

        let response = self
            .client
            .post(&url)
            .header("Authorization", format!("token {}", token))
            .json(&payload)
            .send()
            .await
            .map_err(|e| Error::GitHub(format!("Codeberg issues API: {}", e)))?;

        if !response.status().is_success() {
            return Err(Error::GitHub(format!(
                "Codeberg issues API returned {}",
                response.status()
            )));
        }

        let data: serde_json::Value = response
            .json()
            .await
            .map_err(|e| Error::GitHub(format!("Codeberg issues response: {}", e)))?;

        // Gitea returns the issue `number` (per-repo, human-facing) —
        // mirror GitHub's IssueId convention by using that.
        Ok(IssueId(
            data["number"]
                .as_u64()
                .map(|id| id.to_string())
                .ok_or_else(|| {
                    Error::GitHub("Missing number in Codeberg issue response".to_string())
                })?,
        ))
    }

    async fn get_default_branch(&self, repo: &RepoId) -> Result<String> {
        // GET /api/v1/repos/{owner}/{repo}  ->  {... "default_branch": "main", ...}
        let url = format!("{}/repos/{}", self.api_url(), self.repo_path(repo),);

        let mut req = self.client.get(&url);
        if let Some(token) = self.token.as_ref() {
            req = req.header("Authorization", format!("token {}", token));
        }

        let response = req
            .send()
            .await
            .map_err(|e| Error::GitHub(format!("Codeberg repo API: {}", e)))?;

        if !response.status().is_success() {
            return Err(Error::GitHub(format!(
                "Codeberg repo API returned {}",
                response.status()
            )));
        }

        let data: serde_json::Value = response
            .json()
            .await
            .map_err(|e| Error::GitHub(format!("Codeberg repo response: {}", e)))?;

        Ok(data["default_branch"]
            .as_str()
            .unwrap_or("main")
            .to_string())
    }

    async fn get_file_contents(
        &self,
        repo: &RepoId,
        branch: Option<&str>,
        path: &str,
    ) -> Result<Option<String>> {
        // Gitea/Forgejo raw file endpoint:
        //   GET /api/v1/repos/{owner}/{repo}/raw/{filepath}?ref={branch}
        // Returns raw bytes (no base64 envelope), same shape as GitLab's
        // `/files/.../raw`. 404 → Ok(None) so the directive-resolver
        // cascade keeps working.
        let encoded_path = path
            .split('/')
            .map(|s| urlencoding::encode(s).into_owned())
            .collect::<Vec<_>>()
            .join("/");

        let mut url = format!(
            "{}/repos/{}/raw/{}",
            self.api_url(),
            self.repo_path(repo),
            encoded_path,
        );
        if let Some(r) = branch {
            url.push_str(&format!("?ref={}", urlencoding::encode(r)));
        }

        let mut req = self.client.get(&url);
        if let Some(token) = self.token.as_ref() {
            req = req.header("Authorization", format!("token {}", token));
        }

        let resp = req
            .send()
            .await
            .map_err(|e| Error::GitHub(format!("Codeberg raw API: {}", e)))?;

        let status = resp.status();
        if status == reqwest::StatusCode::NOT_FOUND {
            return Ok(None);
        }
        if !status.is_success() {
            return Err(Error::GitHub(format!(
                "Codeberg raw API returned {}",
                status
            )));
        }

        let body = resp
            .text()
            .await
            .map_err(|e| Error::GitHub(format!("Codeberg raw response: {}", e)))?;
        Ok(Some(body))
    }

    async fn create_review_comment(
        &self,
        repo: &RepoId,
        pr: PrId,
        body: &str,
        _location: ReviewCommentLocation,
    ) -> Result<CommentId> {
        // Gitea/Forgejo inline review comments need a Review wrapper:
        //   POST /api/v1/repos/{owner}/{repo}/pulls/{index}/reviews
        // with body containing a `comments` array of {path, body,
        // old_position, new_position, ...}. The model differs enough
        // from GitHub's per-comment endpoint that mapping
        // ReviewCommentLocation requires a separate design pass.
        //
        // TODO(#62): wire the Reviews API. For now fall back to a
        // general issue comment so Consultant mode still posts.
        tracing::debug!(
            "Codeberg create_review_comment: falling back to general comment (Reviews API not wired)"
        );
        self.create_comment(repo, pr, body).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::adapters::Platform;

    #[test]
    fn codeberg_adapter_default_base_url() {
        let adapter = CodebergAdapter::new(None);
        assert_eq!(adapter.base_url, "https://codeberg.org");
        assert_eq!(adapter.api_url(), "https://codeberg.org/api/v1");
    }

    #[test]
    fn codeberg_adapter_self_hosted_strips_trailing_slash() {
        let adapter = CodebergAdapter::new(Some("https://forgejo.example.org/"));
        assert_eq!(adapter.base_url, "https://forgejo.example.org");
        assert_eq!(adapter.api_url(), "https://forgejo.example.org/api/v1");
    }

    #[test]
    fn codeberg_adapter_builds_repo_path() {
        let adapter = CodebergAdapter::new(None);
        let repo = RepoId::new(Platform::Codeberg, "hyperpolymath", "echidnabot");
        assert_eq!(adapter.repo_path(&repo), "hyperpolymath/echidnabot");
        assert_eq!(
            adapter.repo_url(&repo),
            "https://codeberg.org/hyperpolymath/echidnabot.git"
        );
    }
}
