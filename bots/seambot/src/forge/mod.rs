// SPDX-License-Identifier: PMPL-1.0-or-later

//! Multi-forge integration for seam hygiene reporting
//!
//! Supports GitHub, GitLab, and Bitbucket for creating issues,
//! comments, and status checks.

use anyhow::Result;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};

pub mod github;
pub mod gitlab;
pub mod bitbucket;

/// Unified forge client trait
#[async_trait]
pub trait ForgeClient: Send + Sync {
    /// Create an issue in the repository
    async fn create_issue(
        &self,
        owner: &str,
        repo: &str,
        title: &str,
        body: &str,
        labels: &[&str],
    ) -> Result<IssueResponse>;

    /// Add a comment to an existing issue
    async fn add_comment(
        &self,
        owner: &str,
        repo: &str,
        issue_number: u64,
        body: &str,
    ) -> Result<CommentResponse>;

    /// Update a status check on a commit
    async fn update_check(
        &self,
        owner: &str,
        repo: &str,
        commit_sha: &str,
        check: CheckStatus,
    ) -> Result<CheckResponse>;

    /// Get the forge type (GitHub, GitLab, etc.)
    fn forge_type(&self) -> ForgeType;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ForgeType {
    GitHub,
    GitLab,
    Bitbucket,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IssueResponse {
    pub id: u64,
    pub number: u64,
    pub url: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CommentResponse {
    pub id: u64,
    pub url: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CheckResponse {
    pub id: u64,
    pub status: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CheckStatus {
    pub name: String,
    pub conclusion: Conclusion,
    pub title: String,
    pub summary: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Conclusion {
    Success,
    Failure,
    Neutral,
    Cancelled,
    Skipped,
    TimedOut,
    ActionRequired,
}

impl std::fmt::Display for Conclusion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Conclusion::Success => write!(f, "success"),
            Conclusion::Failure => write!(f, "failure"),
            Conclusion::Neutral => write!(f, "neutral"),
            Conclusion::Cancelled => write!(f, "cancelled"),
            Conclusion::Skipped => write!(f, "skipped"),
            Conclusion::TimedOut => write!(f, "timed_out"),
            Conclusion::ActionRequired => write!(f, "action_required"),
        }
    }
}

impl std::fmt::Display for ForgeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ForgeType::GitHub => write!(f, "GitHub"),
            ForgeType::GitLab => write!(f, "GitLab"),
            ForgeType::Bitbucket => write!(f, "Bitbucket"),
        }
    }
}

/// Factory for creating forge clients
pub fn create_forge_client(forge_type: ForgeType, token: &str) -> Box<dyn ForgeClient> {
    let client: Box<dyn ForgeClient> = match forge_type {
        ForgeType::GitHub => Box::new(github::GitHubForgeClient::new(token)),
        ForgeType::GitLab => Box::new(gitlab::GitLabForgeClient::new(token)),
        ForgeType::Bitbucket => Box::new(bitbucket::BitbucketForgeClient::new(token)),
    };
    tracing::debug!("Created {} forge client", client.forge_type());
    client
}
