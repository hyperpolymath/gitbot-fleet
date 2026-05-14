// SPDX-License-Identifier: PMPL-1.0-or-later

//! GitLab forge client implementation

use super::{
    CheckResponse, CheckStatus, CommentResponse, Conclusion, ForgeClient, ForgeType,
    IssueResponse,
};
use anyhow::Result;
use async_trait::async_trait;
use reqwest::Client;
use serde::{Deserialize, Serialize};

pub struct GitLabForgeClient {
    client: Client,
    token: String,
    base_url: String,
}

impl GitLabForgeClient {
    pub fn new(token: impl Into<String>) -> Self {
        Self {
            client: Client::new(),
            token: token.into(),
            base_url: "https://gitlab.com/api/v4".to_string(),
        }
    }
}

#[async_trait]
impl ForgeClient for GitLabForgeClient {
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
            description: String,
            labels: String,
        }

        #[derive(Deserialize)]
        struct GitLabIssue {
            id: u64,
            iid: u64,
            web_url: String,
        }

        // GitLab uses project ID or "owner/repo" format
        let project_id = format!("{}/{}", owner, repo);
        let encoded_project_id = urlencoding::encode(&project_id);

        let url = format!("{}/projects/{}/issues", self.base_url, encoded_project_id);

        let payload = CreateIssue {
            title: title.to_string(),
            description: body.to_string(),
            labels: labels.join(","),
        };

        let response = self
            .client
            .post(&url)
            .header("PRIVATE-TOKEN", &self.token)
            .header("User-Agent", "seambot")
            .json(&payload)
            .send()
            .await?;

        let issue: GitLabIssue = response.json().await?;

        Ok(IssueResponse {
            id: issue.id,
            number: issue.iid,
            url: issue.web_url,
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
        struct CreateNote {
            body: String,
        }

        #[derive(Deserialize)]
        struct GitLabNote {
            id: u64,
        }

        let project_id = format!("{}/{}", owner, repo);
        let encoded_project_id = urlencoding::encode(&project_id);

        let url = format!(
            "{}/projects/{}/issues/{}/notes",
            self.base_url, encoded_project_id, issue_number
        );

        let payload = CreateNote {
            body: body.to_string(),
        };

        let response = self
            .client
            .post(&url)
            .header("PRIVATE-TOKEN", &self.token)
            .header("User-Agent", "seambot")
            .json(&payload)
            .send()
            .await?;

        let note: GitLabNote = response.json().await?;

        Ok(CommentResponse {
            id: note.id,
            url: format!("{}/issues/{}#note_{}", project_id, issue_number, note.id),
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
        struct CreateCommitStatus {
            state: String,
            name: String,
            description: String,
        }

        #[derive(Deserialize)]
        struct GitLabCommitStatus {
            id: u64,
            status: String,
        }

        let project_id = format!("{}/{}", owner, repo);
        let encoded_project_id = urlencoding::encode(&project_id);

        let url = format!(
            "{}/projects/{}/statuses/{}",
            self.base_url, encoded_project_id, commit_sha
        );

        // Convert conclusion to GitLab state
        let state = match check.conclusion {
            Conclusion::Success => "success",
            Conclusion::Failure => "failed",
            Conclusion::Cancelled => "canceled",
            _ => "pending",
        };

        let payload = CreateCommitStatus {
            state: state.to_string(),
            name: check.name,
            description: format!("{}: {}", check.title, check.summary),
        };

        let response = self
            .client
            .post(&url)
            .header("PRIVATE-TOKEN", &self.token)
            .header("User-Agent", "seambot")
            .json(&payload)
            .send()
            .await?;

        let status: GitLabCommitStatus = response.json().await?;

        Ok(CheckResponse {
            id: status.id,
            status: status.status,
        })
    }

    fn forge_type(&self) -> ForgeType {
        ForgeType::GitLab
    }
}
