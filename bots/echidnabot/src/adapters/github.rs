// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! GitHub platform adapter using Octocrab

use async_trait::async_trait;

use super::{
    CheckConclusion, CheckRun, CheckRunId, CheckStatus, CommentId, IssueId, NewIssue,
    PlatformAdapter, PrId, RepoId, ReviewCommentLocation,
};
use crate::error::{Error, Result};

/// GitHub adapter using Octocrab
pub struct GitHubAdapter {
    client: octocrab::Octocrab,
    /// Raw HTTP client for APIs not covered by Octocrab (e.g. inline review comments).
    http: reqwest::Client,
    token: String,
}

impl GitHubAdapter {
    /// Create a new GitHub adapter with a token
    pub fn new(token: &str) -> Result<Self> {
        let client = octocrab::Octocrab::builder()
            .personal_token(token.to_string())
            .build()
            .map_err(|e| Error::GitHub(e.to_string()))?;

        let http = reqwest::Client::builder()
            .user_agent("echidnabot/0.1.0")
            .build()
            .map_err(|e| Error::GitHub(e.to_string()))?;

        Ok(Self {
            client,
            http,
            token: token.to_string(),
        })
    }

    /// Create adapter from environment variable
    pub fn from_env() -> Result<Self> {
        let token = std::env::var("GITHUB_TOKEN")
            .map_err(|_| Error::Config("GITHUB_TOKEN not set".to_string()))?;
        Self::new(&token)
    }
}

#[async_trait]
impl PlatformAdapter for GitHubAdapter {
    async fn clone_repo(&self, repo: &RepoId, commit: &str) -> Result<tempfile::TempDir> {
        let url = format!("https://github.com/{}/{}.git", repo.owner, repo.name);
        super::clone_revision(&url, commit).await
    }

    async fn create_check_run(&self, repo: &RepoId, check: CheckRun) -> Result<CheckRunId> {
        let checks = self.client.checks(&repo.owner, &repo.name);

        use octocrab::params::checks::{
            CheckRunConclusion as OctoConclusion, CheckRunStatus as OctoStatus,
        };

        let (status, conclusion) = match check.status {
            CheckStatus::Queued => (OctoStatus::Queued, None),
            CheckStatus::InProgress => (OctoStatus::InProgress, None),
            CheckStatus::Completed { conclusion, .. } => {
                let c = match conclusion {
                    CheckConclusion::Success => OctoConclusion::Success,
                    CheckConclusion::Failure => OctoConclusion::Failure,
                    CheckConclusion::Neutral => OctoConclusion::Neutral,
                    CheckConclusion::Cancelled => OctoConclusion::Cancelled,
                    CheckConclusion::Skipped => OctoConclusion::Skipped,
                    CheckConclusion::TimedOut => OctoConclusion::TimedOut,
                    CheckConclusion::ActionRequired => OctoConclusion::ActionRequired,
                };
                (OctoStatus::Completed, Some(c))
            }
        };

        // Build check run request
        let mut builder = checks.create_check_run(check.name, check.head_sha);

        builder = builder.status(status);

        if let Some(c) = conclusion {
            builder = builder.conclusion(c);
        }

        if let Some(url) = check.details_url {
            builder = builder.details_url(url);
        }

        let result = builder
            .send()
            .await
            .map_err(|e| Error::GitHub(e.to_string()))?;

        Ok(CheckRunId(result.id.to_string()))
    }

    async fn update_check_run(&self, id: CheckRunId, status: CheckStatus) -> Result<()> {
        // Note: Octocrab doesn't have direct update_check_run, would need raw API
        // For now, log and return Ok
        tracing::info!("Would update check run {} to {:?}", id.0, status);
        Ok(())
    }

    async fn create_comment(&self, repo: &RepoId, pr: PrId, body: &str) -> Result<CommentId> {
        let pr_num: u64 =
            pr.0.parse()
                .map_err(|_| Error::GitHub("Invalid PR ID".to_string()))?;

        let comment = self
            .client
            .issues(&repo.owner, &repo.name)
            .create_comment(pr_num, body)
            .await
            .map_err(|e| Error::GitHub(e.to_string()))?;

        Ok(CommentId(comment.id.to_string()))
    }

    async fn create_issue(&self, repo: &RepoId, issue: NewIssue) -> Result<IssueId> {
        let created = self
            .client
            .issues(&repo.owner, &repo.name)
            .create(&issue.title)
            .body(&issue.body)
            .labels(issue.labels)
            .send()
            .await
            .map_err(|e| Error::GitHub(e.to_string()))?;

        Ok(IssueId(created.number.to_string()))
    }

    async fn get_default_branch(&self, repo: &RepoId) -> Result<String> {
        let repo_info = self
            .client
            .repos(&repo.owner, &repo.name)
            .get()
            .await
            .map_err(|e| Error::GitHub(e.to_string()))?;

        Ok(repo_info
            .default_branch
            .unwrap_or_else(|| "main".to_string()))
    }

    async fn get_file_contents(
        &self,
        repo: &RepoId,
        branch: Option<&str>,
        path: &str,
    ) -> Result<Option<String>> {
        // Bind the `repos(...)` call to a local; `get_content()` borrows
        // from it, so without the binding the temp drops mid-expression.
        let repo_handle = self.client.repos(&repo.owner, &repo.name);
        let mut req = repo_handle.get_content().path(path);
        if let Some(r) = branch {
            req = req.r#ref(r);
        }
        match req.send().await {
            Ok(mut content) => {
                // For a file path the response has a single item; directories
                // return many. We only call with file paths.
                if let Some(item) = content.items.pop() {
                    Ok(item.decoded_content())
                } else {
                    Ok(None)
                }
            }
            Err(e) => {
                let msg = e.to_string();
                // Map missing-file (404 / NotFound) to Ok(None) so callers
                // can cascade through the directive resolver. Real
                // failures (auth, rate-limit, network) bubble up.
                if msg.contains("404") || msg.to_lowercase().contains("not found") {
                    Ok(None)
                } else {
                    Err(Error::GitHub(msg))
                }
            }
        }
    }

    async fn create_review_comment(
        &self,
        repo: &RepoId,
        pr: PrId,
        body: &str,
        location: ReviewCommentLocation,
    ) -> Result<CommentId> {
        let pr_num: u64 =
            pr.0.parse()
                .map_err(|_| Error::GitHub("Invalid PR ID".to_string()))?;

        // GitHub API: POST /repos/{owner}/{repo}/pulls/{pull_number}/comments
        // Requires commit_id, path, side, and line (or position for legacy diffs).
        let url = format!(
            "https://api.github.com/repos/{}/{}/pulls/{}/comments",
            repo.owner, repo.name, pr_num
        );

        let payload = serde_json::json!({
            "body": body,
            "commit_id": location.commit_sha,
            "path": location.path,
            "side": "RIGHT",
            "line": location.line,
        });

        let response = self
            .http
            .post(&url)
            .header("Authorization", format!("Bearer {}", self.token))
            .header("Accept", "application/vnd.github.v3+json")
            .json(&payload)
            .send()
            .await
            .map_err(|e| Error::GitHub(e.to_string()))?;

        if !response.status().is_success() {
            let status = response.status();
            let text = response.text().await.unwrap_or_default();
            // 422 = file not in diff; callers fall back to create_comment.
            return Err(Error::GitHub(format!(
                "Review comment rejected by GitHub ({}): {}",
                status, text
            )));
        }

        let data: serde_json::Value = response
            .json()
            .await
            .map_err(|e| Error::GitHub(e.to_string()))?;

        Ok(CommentId(
            data["id"]
                .as_u64()
                .map(|id| id.to_string())
                .ok_or_else(|| {
                    Error::GitHub("Missing id in review comment response".to_string())
                })?,
        ))
    }
}
