// SPDX-License-Identifier: PMPL-1.0-or-later

//! GitHub forge client implementation

use super::{
    CheckResponse, CheckStatus, CommentResponse, ForgeClient, ForgeType,
    IssueResponse,
};
use anyhow::Result;
use async_trait::async_trait;
use reqwest::Client;
use serde::{Deserialize, Serialize};

pub struct GitHubForgeClient {
    client: Client,
    token: String,
    base_url: String,
}

impl GitHubForgeClient {
    pub fn new(token: impl Into<String>) -> Self {
        Self {
            client: Client::new(),
            token: token.into(),
            base_url: "https://api.github.com".to_string(),
        }
    }
}

#[async_trait]
impl ForgeClient for GitHubForgeClient {
    async fn create_issue(
        &self,
        owner: &str,
        repo: &str,
        title: &str,
        body: &str,
        labels: &[&str],
    ) -> Result<IssueResponse> {
        #[derive(Serialize)]
        struct CreateIssue {
            title: String,
            body: String,
            labels: Vec<String>,
        }

        #[derive(Deserialize)]
        struct GitHubIssue {
            id: u64,
            number: u64,
            html_url: String,
        }

        let url = format!("{}/repos/{}/{}/issues", self.base_url, owner, repo);

        let payload = CreateIssue {
            title: title.to_string(),
            body: body.to_string(),
            labels: labels.iter().map(|s| s.to_string()).collect(),
        };

        let response = self
            .client
            .post(&url)
            .bearer_auth(&self.token)
            .header("Accept", "application/vnd.github+json")
            .header("User-Agent", "seambot")
            .json(&payload)
            .send()
            .await?;

        let issue: GitHubIssue = response.json().await?;

        Ok(IssueResponse {
            id: issue.id,
            number: issue.number,
            url: issue.html_url,
        })
    }

    async fn add_comment(
        &self,
        owner: &str,
        repo: &str,
        issue_number: u64,
        body: &str,
    ) -> Result<CommentResponse> {
        #[derive(Serialize)]
        struct CreateComment {
            body: String,
        }

        #[derive(Deserialize)]
        struct GitHubComment {
            id: u64,
            html_url: String,
        }

        let url = format!(
            "{}/repos/{}/{}/issues/{}/comments",
            self.base_url, owner, repo, issue_number
        );

        let payload = CreateComment {
            body: body.to_string(),
        };

        let response = self
            .client
            .post(&url)
            .bearer_auth(&self.token)
            .header("Accept", "application/vnd.github+json")
            .header("User-Agent", "seambot")
            .json(&payload)
            .send()
            .await?;

        let comment: GitHubComment = response.json().await?;

        Ok(CommentResponse {
            id: comment.id,
            url: comment.html_url,
        })
    }

    async fn update_check(
        &self,
        owner: &str,
        repo: &str,
        commit_sha: &str,
        check: CheckStatus,
    ) -> Result<CheckResponse> {
        #[derive(Serialize)]
        struct CreateCheckRun {
            name: String,
            head_sha: String,
            status: String,
            conclusion: String,
            output: Output,
        }

        #[derive(Serialize)]
        struct Output {
            title: String,
            summary: String,
        }

        #[derive(Deserialize)]
        struct GitHubCheckRun {
            id: u64,
            status: String,
        }

        let url = format!("{}/repos/{}/{}/check-runs", self.base_url, owner, repo);

        let payload = CreateCheckRun {
            name: check.name,
            head_sha: commit_sha.to_string(),
            status: "completed".to_string(),
            conclusion: check.conclusion.to_string(),
            output: Output {
                title: check.title,
                summary: check.summary,
            },
        };

        let response = self
            .client
            .post(&url)
            .bearer_auth(&self.token)
            .header("Accept", "application/vnd.github+json")
            .header("User-Agent", "seambot")
            .json(&payload)
            .send()
            .await?;

        let check_run: GitHubCheckRun = response.json().await?;

        Ok(CheckResponse {
            id: check_run.id,
            status: check_run.status,
        })
    }

    fn forge_type(&self) -> ForgeType {
        ForgeType::GitHub
    }
}
