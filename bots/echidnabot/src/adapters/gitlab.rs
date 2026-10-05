// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! GitLab platform adapter (minimal clone support)

use async_trait::async_trait;

use super::{
    CheckConclusion, CheckRun, CheckRunId, CheckStatus, CommentId, IssueId, NewIssue,
    PlatformAdapter, PrId, RepoId, ReviewCommentLocation,
};
use crate::error::{Error, Result};

/// GitLab adapter (clone-only implementation)
pub struct GitLabAdapter {
    base_url: String,
    token: Option<String>,
    client: reqwest::Client,
}

impl GitLabAdapter {
    pub fn new(base_url: Option<&str>) -> Self {
        let base = base_url.unwrap_or("https://gitlab.com");
        Self {
            base_url: base.trim_end_matches('/').to_string(),
            token: std::env::var("GITLAB_TOKEN").ok(),
            client: reqwest::Client::new(),
        }
    }

    fn repo_url(&self, repo: &RepoId) -> String {
        format!("{}/{}/{}.git", self.base_url, repo.owner, repo.name)
    }

    fn api_url(&self) -> String {
        format!("{}/api/v4", self.base_url)
    }

    fn project_path(&self, repo: &RepoId) -> String {
        format!("{}/{}", repo.owner, repo.name)
    }
}

#[async_trait]
impl PlatformAdapter for GitLabAdapter {
    async fn clone_repo(&self, repo: &RepoId, commit: &str) -> Result<tempfile::TempDir> {
        let url = self.repo_url(repo);
        super::clone_revision(&url, commit).await
    }

    async fn create_check_run(&self, repo: &RepoId, check: CheckRun) -> Result<CheckRunId> {
        let token = self
            .token
            .as_ref()
            .ok_or_else(|| Error::Config("GITLAB_TOKEN not set".to_string()))?;

        let project_path = self.project_path(repo);
        let encoded_project = urlencoding::encode(&project_path);
        let url = format!(
            "{}/projects/{}/statuses/{}",
            self.api_url(),
            encoded_project,
            check.head_sha
        );

        let (state, description) = match &check.status {
            CheckStatus::Completed {
                conclusion,
                summary,
            } => {
                let state = match conclusion {
                    CheckConclusion::Success => "success",
                    CheckConclusion::Failure => "failed",
                    CheckConclusion::Cancelled => "canceled",
                    _ => "failed",
                };
                (state, summary.clone())
            }
            CheckStatus::InProgress => ("running", String::new()),
            CheckStatus::Queued => ("pending", String::new()),
        };

        let payload = serde_json::json!({
            "state": state,
            "name": check.name,
            "description": description,
        });

        let response = self
            .client
            .post(&url)
            .header("PRIVATE-TOKEN", token)
            .json(&payload)
            .send()
            .await
            .map_err(|e| Error::GitHub(e.to_string()))?;

        let data: serde_json::Value = response
            .json()
            .await
            .map_err(|e| Error::GitHub(e.to_string()))?;

        Ok(CheckRunId(
            data["id"]
                .as_u64()
                .map(|id| id.to_string())
                .ok_or_else(|| Error::GitHub("Missing id in response".to_string()))?,
        ))
    }

    async fn update_check_run(&self, _id: CheckRunId, _status: CheckStatus) -> Result<()> {
        // GitLab doesn't support updating commit statuses after creation
        Ok(())
    }

    async fn create_comment(&self, repo: &RepoId, pr: PrId, body: &str) -> Result<CommentId> {
        let token = self
            .token
            .as_ref()
            .ok_or_else(|| Error::Config("GITLAB_TOKEN not set".to_string()))?;

        let project_path = self.project_path(repo);
        let encoded_project = urlencoding::encode(&project_path);
        let url = format!(
            "{}/projects/{}/merge_requests/{}/notes",
            self.api_url(),
            encoded_project,
            pr.0
        );

        let payload = serde_json::json!({
            "body": body,
        });

        let response = self
            .client
            .post(&url)
            .header("PRIVATE-TOKEN", token)
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
            .ok_or_else(|| Error::Config("GITLAB_TOKEN not set".to_string()))?;

        let project_path = self.project_path(repo);
        let encoded_project = urlencoding::encode(&project_path);
        let url = format!("{}/projects/{}/issues", self.api_url(), encoded_project);

        let payload = serde_json::json!({
            "title": issue.title,
            "description": issue.body,
            "labels": issue.labels.join(","),
        });

        let response = self
            .client
            .post(&url)
            .header("PRIVATE-TOKEN", token)
            .json(&payload)
            .send()
            .await
            .map_err(|e| Error::GitHub(e.to_string()))?;

        let data: serde_json::Value = response
            .json()
            .await
            .map_err(|e| Error::GitHub(e.to_string()))?;

        Ok(IssueId(
            data["iid"]
                .as_u64()
                .map(|id| id.to_string())
                .ok_or_else(|| Error::GitHub("Missing iid in response".to_string()))?,
        ))
    }

    async fn get_default_branch(&self, repo: &RepoId) -> Result<String> {
        let token = self
            .token
            .as_ref()
            .ok_or_else(|| Error::Config("GITLAB_TOKEN not set".to_string()))?;

        let project_path = self.project_path(repo);
        let encoded_project = urlencoding::encode(&project_path);
        let url = format!("{}/projects/{}", self.api_url(), encoded_project);

        let response = self
            .client
            .get(&url)
            .header("PRIVATE-TOKEN", token)
            .send()
            .await
            .map_err(|e| Error::GitHub(e.to_string()))?;

        let data: serde_json::Value = response
            .json()
            .await
            .map_err(|e| Error::GitHub(e.to_string()))?;

        Ok(data["default_branch"]
            .as_str()
            .ok_or_else(|| Error::GitHub("Missing default_branch in response".to_string()))?
            .to_string())
    }

    async fn get_file_contents(
        &self,
        repo: &RepoId,
        branch: Option<&str>,
        path: &str,
    ) -> Result<Option<String>> {
        // GitLab Repository Files API — raw variant skips the JSON
        // base64 envelope:
        //   GET /api/v4/projects/:id/repository/files/:filepath/raw?ref=:branch
        // The :id can be the URL-encoded `owner/name` path, skipping
        // a numeric project-id resolution round-trip. :filepath also
        // URL-encoded per the GitLab spec.
        // Token auth via PRIVATE-TOKEN header when configured.
        let project_path = self.project_path(repo);
        let project = urlencoding::encode(&project_path);
        let file_path = urlencoding::encode(path);
        let r#ref = branch.unwrap_or("HEAD");
        let url = format!(
            "{}/projects/{}/repository/files/{}/raw?ref={}",
            self.api_url(),
            project,
            file_path,
            urlencoding::encode(r#ref),
        );
        let mut req = self.client.get(&url);
        if let Some(token) = self.token.as_ref() {
            req = req.header("PRIVATE-TOKEN", token);
        }
        let resp = req
            .send()
            .await
            .map_err(|e| Error::GitHub(format!("GitLab files API: {}", e)))?;
        let status = resp.status();
        if status == reqwest::StatusCode::NOT_FOUND {
            return Ok(None);
        }
        if !status.is_success() {
            return Err(Error::GitHub(format!(
                "GitLab files API returned {}",
                status
            )));
        }
        let body = resp
            .text()
            .await
            .map_err(|e| Error::GitHub(format!("GitLab files response: {}", e)))?;
        Ok(Some(body))
    }

    async fn create_review_comment(
        &self,
        repo: &RepoId,
        pr: PrId,
        body: &str,
        _location: ReviewCommentLocation,
    ) -> Result<CommentId> {
        // GitLab inline review comments require the Discussions API which
        // is not yet implemented in this adapter. Fall back to a general
        // MR note so Consultant mode always posts something useful.
        tracing::debug!(
            "GitLab create_review_comment: falling back to general MR note (Discussions API not wired)"
        );
        self.create_comment(repo, pr, body).await
    }
}
