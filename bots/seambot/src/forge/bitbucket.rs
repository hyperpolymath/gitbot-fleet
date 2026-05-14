// SPDX-License-Identifier: PMPL-1.0-or-later

//! Bitbucket forge client implementation

use super::{
    CheckResponse, CheckStatus, CommentResponse, Conclusion, ForgeClient, ForgeType,
    IssueResponse,
};
use anyhow::Result;
use async_trait::async_trait;
use reqwest::Client;
use serde::{Deserialize, Serialize};

pub struct BitbucketForgeClient {
    client: Client,
    token: String,
    base_url: String,
}

impl BitbucketForgeClient {
    pub fn new(token: impl Into<String>) -> Self {
        Self {
            client: Client::new(),
            token: token.into(),
            base_url: "https://api.bitbucket.org/2.0".to_string(),
        }
    }
}

#[async_trait]
impl ForgeClient for BitbucketForgeClient {
    async fn create_issue(
        &self,
        owner: &str,
        repo: &str,
        title: &str,
        body: &str,
        _labels: &[&str],
    ) -> Result<IssueResponse> {
        #[derive(Serialize)]
        struct CreateIssue {
            title: String,
            content: Content,
            kind: String,
        }

        #[derive(Serialize)]
        struct Content {
            raw: String,
        }

        #[derive(Deserialize)]
        struct BitbucketIssue {
            id: u64,
            links: Links,
        }

        #[derive(Deserialize)]
        struct Links {
            html: Link,
        }

        #[derive(Deserialize)]
        struct Link {
            href: String,
        }

        let url = format!(
            "{}/repositories/{}/{}/issues",
            self.base_url, owner, repo
        );

        let payload = CreateIssue {
            title: title.to_string(),
            content: Content {
                raw: body.to_string(),
            },
            kind: "bug".to_string(), // Bitbucket requires a kind
        };

        let response = self
            .client
            .post(&url)
            .bearer_auth(&self.token)
            .header("User-Agent", "seambot")
            .json(&payload)
            .send()
            .await?;

        let issue: BitbucketIssue = response.json().await?;

        Ok(IssueResponse {
            id: issue.id,
            number: issue.id, // Bitbucket uses ID as number
            url: issue.links.html.href,
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
            content: Content,
        }

        #[derive(Serialize)]
        struct Content {
            raw: String,
        }

        #[derive(Deserialize)]
        struct BitbucketComment {
            id: u64,
            links: Links,
        }

        #[derive(Deserialize)]
        struct Links {
            html: Link,
        }

        #[derive(Deserialize)]
        struct Link {
            href: String,
        }

        let url = format!(
            "{}/repositories/{}/{}/issues/{}/comments",
            self.base_url, owner, repo, issue_number
        );

        let payload = CreateComment {
            content: Content {
                raw: body.to_string(),
            },
        };

        let response = self
            .client
            .post(&url)
            .bearer_auth(&self.token)
            .header("User-Agent", "seambot")
            .json(&payload)
            .send()
            .await?;

        let comment: BitbucketComment = response.json().await?;

        Ok(CommentResponse {
            id: comment.id,
            url: comment.links.html.href,
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
        struct CreateBuildStatus {
            state: String,
            key: String,
            name: String,
            description: String,
        }

        #[derive(Deserialize)]
        struct BitbucketBuildStatus {
            uuid: String,
            state: String,
        }

        let url = format!(
            "{}/repositories/{}/{}/commit/{}/statuses/build",
            self.base_url, owner, repo, commit_sha
        );

        // Convert conclusion to Bitbucket state
        let state = match check.conclusion {
            Conclusion::Success => "SUCCESSFUL",
            Conclusion::Failure => "FAILED",
            _ => "INPROGRESS",
        };

        let payload = CreateBuildStatus {
            state: state.to_string(),
            key: check.name.clone(),
            name: check.name,
            description: format!("{}: {}", check.title, check.summary),
        };

        let response = self
            .client
            .post(&url)
            .bearer_auth(&self.token)
            .header("User-Agent", "seambot")
            .json(&payload)
            .send()
            .await?;

        let status: BitbucketBuildStatus = response.json().await?;

        // Bitbucket uses UUID instead of numeric ID
        let id = status.uuid.trim_matches('{').trim_matches('}').len() as u64;

        Ok(CheckResponse {
            id,
            status: status.state,
        })
    }

    fn forge_type(&self) -> ForgeType {
        ForgeType::Bitbucket
    }
}
