// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! Bitbucket platform adapter (minimal clone support)

use async_trait::async_trait;

use super::{
    CheckConclusion, CheckRun, CheckRunId, CheckStatus, CommentId, IssueId, NewIssue,
    PlatformAdapter, PrId, RepoId, ReviewCommentLocation,
};
use crate::error::{Error, Result};

/// Bitbucket adapter (clone-only implementation)
pub struct BitbucketAdapter {
    base_url: String,
    token: Option<String>,
    client: reqwest::Client,
}

impl BitbucketAdapter {
    pub fn new(base_url: Option<&str>) -> Self {
        let base = base_url.unwrap_or("https://bitbucket.org");
        Self {
            base_url: base.trim_end_matches('/').to_string(),
            token: std::env::var("BITBUCKET_TOKEN").ok(),
            client: reqwest::Client::new(),
        }
    }

    fn repo_url(&self, repo: &RepoId) -> String {
        format!("{}/{}/{}.git", self.base_url, repo.owner, repo.name)
    }

    fn api_url(&self) -> String {
        "https://api.bitbucket.org/2.0".to_string()
    }

    fn project_path(&self, repo: &RepoId) -> String {
        format!("{}/{}", repo.owner, repo.name)
    }
}

#[async_trait]
impl PlatformAdapter for BitbucketAdapter {
    async fn clone_repo(&self, repo: &RepoId, commit: &str) -> Result<tempfile::TempDir> {
        let url = self.repo_url(repo);
        super::clone_revision(&url, commit).await
    }

    async fn create_check_run(&self, repo: &RepoId, check: CheckRun) -> Result<CheckRunId> {
        let token = self
            .token
            .as_ref()
            .ok_or_else(|| Error::Config("BITBUCKET_TOKEN not set".to_string()))?;

        let project_path = self.project_path(repo);
        let url = format!(
            "{}/repositories/{}/commit/{}/statuses/build",
            self.api_url(),
            project_path,
            check.head_sha
        );

        let (state, description) = match &check.status {
            CheckStatus::Completed {
                conclusion,
                summary,
            } => {
                let state = match conclusion {
                    CheckConclusion::Success => "SUCCESSFUL",
                    CheckConclusion::Failure => "FAILED",
                    CheckConclusion::Cancelled => "STOPPED",
                    _ => "FAILED",
                };
                (state, summary.clone())
            }
            CheckStatus::InProgress => ("INPROGRESS", String::new()),
            CheckStatus::Queued => ("INPROGRESS", String::new()),
        };

        let payload = serde_json::json!({
            "state": state,
            "key": check.name,
            "description": description,
        });

        let response = self
            .client
            .post(&url)
            .bearer_auth(token)
            .json(&payload)
            .send()
            .await
            .map_err(|e| Error::GitHub(e.to_string()))?;

        let data: serde_json::Value = response
            .json()
            .await
            .map_err(|e| Error::GitHub(e.to_string()))?;

        Ok(CheckRunId(data["uuid"].as_str().unwrap_or("0").to_string()))
    }

    async fn update_check_run(&self, _id: CheckRunId, _status: CheckStatus) -> Result<()> {
        // Bitbucket doesn't support updating build statuses after creation
        // To update, you would need to POST again with the same key
        Ok(())
    }

    async fn create_comment(&self, repo: &RepoId, pr: PrId, body: &str) -> Result<CommentId> {
        let token = self
            .token
            .as_ref()
            .ok_or_else(|| Error::Config("BITBUCKET_TOKEN not set".to_string()))?;

        let project_path = self.project_path(repo);
        let url = format!(
            "{}/repositories/{}/pullrequests/{}/comments",
            self.api_url(),
            project_path,
            pr.0
        );

        let payload = serde_json::json!({
            "content": {
                "raw": body,
            },
        });

        let response = self
            .client
            .post(&url)
            .bearer_auth(token)
            .json(&payload)
            .send()
            .await
            .map_err(|e| Error::GitHub(e.to_string()))?;

        let data: serde_json::Value = response
            .json()
            .await
            .map_err(|e| Error::GitHub(e.to_string()))?;

        Ok(CommentId(
            data["id"]
                .as_u64()
                .map(|id| id.to_string())
                .ok_or_else(|| Error::GitHub("Missing id in response".to_string()))?,
        ))
    }

    async fn create_issue(&self, repo: &RepoId, issue: NewIssue) -> Result<IssueId> {
        let token = self
            .token
            .as_ref()
            .ok_or_else(|| Error::Config("BITBUCKET_TOKEN not set".to_string()))?;

        let project_path = self.project_path(repo);
        let url = format!("{}/repositories/{}/issues", self.api_url(), project_path);

        let payload = serde_json::json!({
            "title": issue.title,
            "content": {
                "raw": issue.body,
            },
            "kind": "bug",  // Bitbucket requires a kind field
        });

        let response = self
            .client
            .post(&url)
            .bearer_auth(token)
            .json(&payload)
            .send()
            .await
            .map_err(|e| Error::GitHub(e.to_string()))?;

        let data: serde_json::Value = response
            .json()
            .await
            .map_err(|e| Error::GitHub(e.to_string()))?;

        Ok(IssueId(
            data["id"]
                .as_u64()
                .map(|id| id.to_string())
                .ok_or_else(|| Error::GitHub("Missing id in response".to_string()))?,
        ))
    }

    async fn get_default_branch(&self, repo: &RepoId) -> Result<String> {
        let token = self
            .token
            .as_ref()
            .ok_or_else(|| Error::Config("BITBUCKET_TOKEN not set".to_string()))?;

        let project_path = self.project_path(repo);
        let url = format!("{}/repositories/{}", self.api_url(), project_path);

        let response = self
            .client
            .get(&url)
            .bearer_auth(token)
            .send()
            .await
            .map_err(|e| Error::GitHub(e.to_string()))?;

        let data: serde_json::Value = response
            .json()
            .await
            .map_err(|e| Error::GitHub(e.to_string()))?;

        Ok(data["mainbranch"]["name"]
            .as_str()
            .ok_or_else(|| Error::GitHub("Missing mainbranch.name in response".to_string()))?
            .to_string())
    }

    async fn get_file_contents(
        &self,
        repo: &RepoId,
        branch: Option<&str>,
        path: &str,
    ) -> Result<Option<String>> {
        // Bitbucket Source endpoint:
        //   GET /repositories/:owner/:slug/src/:branch/:path
        // Returns raw file bytes (no JSON wrapper, no base64).
        // When :branch is omitted, Bitbucket resolves to the
        // repository's default branch.
        // Token auth via Bearer when configured.
        let project = self.project_path(repo);
        let r#ref = branch.unwrap_or("HEAD");
        // The path is interpreted as relative to the ref; URL-encode
        // segments individually to preserve the slash separators.
        let encoded_path = path
            .split('/')
            .map(|s| urlencoding::encode(s).into_owned())
            .collect::<Vec<_>>()
            .join("/");
        let url = format!(
            "{}/repositories/{}/src/{}/{}",
            self.api_url(),
            project,
            urlencoding::encode(r#ref),
            encoded_path,
        );
        let mut req = self.client.get(&url);
        if let Some(token) = self.token.as_ref() {
            req = req.bearer_auth(token);
        }
        let resp = req
            .send()
            .await
            .map_err(|e| Error::GitHub(format!("Bitbucket source API: {}", e)))?;
        let status = resp.status();
        if status == reqwest::StatusCode::NOT_FOUND {
            return Ok(None);
        }
        if !status.is_success() {
            return Err(Error::GitHub(format!(
                "Bitbucket source API returned {}",
                status
            )));
        }
        let body = resp
            .text()
            .await
            .map_err(|e| Error::GitHub(format!("Bitbucket source response: {}", e)))?;
        Ok(Some(body))
    }

    async fn create_review_comment(
        &self,
        repo: &RepoId,
        pr: PrId,
        body: &str,
        _location: ReviewCommentLocation,
    ) -> Result<CommentId> {
        // Bitbucket inline comments (Diff Comments API) are not yet implemented.
        // Fall back to a general PR comment so Consultant mode always posts.
        tracing::debug!(
            "Bitbucket create_review_comment: falling back to general PR comment (Diff Comments API not wired)"
        );
        self.create_comment(repo, pr, body).await
    }
}
