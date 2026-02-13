// SPDX-License-Identifier: PMPL-1.0-or-later

//! GitHub API client module
//!
//! # Security considerations
//!
//! - The GitHub token is read from `GITHUB_TOKEN` environment variable and
//!   passed only to `bearer_auth()`. It is never logged, serialized, or
//!   included in error messages.
//! - File paths passed to content APIs are validated against path traversal.

use anyhow::Result;
use reqwest::Client;
use serde::{Deserialize, Serialize};

use crate::config::Config;
use crate::sanitize;

/// GitHub API client
pub struct GitHubClient {
    client: Client,
    base_url: String,
    token: Option<String>,
}

impl GitHubClient {
    /// Create a new GitHub client
    pub fn new(config: &Config) -> Self {
        Self {
            client: Client::new(),
            base_url: config.github_api_url.clone(),
            token: std::env::var("GITHUB_TOKEN").ok(),
        }
    }

    /// Get repository information
    pub async fn get_repository(&self, owner: &str, repo: &str) -> Result<Repository> {
        let url = format!("{}/repos/{}/{}", self.base_url, owner, repo);
        let mut request = self.client.get(&url);

        if let Some(ref token) = self.token {
            request = request.bearer_auth(token);
        }

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
        let url = format!("{}/repos/{}/{}/contents/{}", self.base_url, owner, repo, path);
        let mut request = self.client.get(&url);

        if let Some(ref token) = self.token {
            request = request.bearer_auth(token);
        }

        let response = request
            .header("Accept", "application/vnd.github+json")
            .header("User-Agent", "rhodibot")
            .send()
            .await?;

        Ok(response.json().await?)
    }

    /// Check if a file exists
    ///
    /// The `path` parameter is validated against path traversal before use.
    pub async fn file_exists(&self, owner: &str, repo: &str, path: &str) -> bool {
        if sanitize::validate_file_path(path).is_err() {
            return false;
        }
        let url = format!("{}/repos/{}/{}/contents/{}", self.base_url, owner, repo, path);
        let mut request = self.client.head(&url);

        if let Some(ref token) = self.token {
            request = request.bearer_auth(token);
        }

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
        let url = format!("{}/repos/{}/{}/contents/{}", self.base_url, owner, repo, path);
        let mut request = self.client.get(&url);

        if let Some(ref token) = self.token {
            request = request.bearer_auth(token);
        }

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
        let url = format!("{}/repos/{}/{}/issues", self.base_url, owner, repo);
        let mut request = self.client.post(&url);

        if let Some(ref token) = self.token {
            request = request.bearer_auth(token);
        }

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
        let url = format!("{}/repos/{}/{}/check-runs", self.base_url, owner, repo);
        let mut request = self.client.post(&url);

        if let Some(ref token) = self.token {
            request = request.bearer_auth(token);
        }

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
