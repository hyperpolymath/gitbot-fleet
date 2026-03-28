// SPDX-License-Identifier: PMPL-1.0-or-later
//! GitHub API integration

use reqwest::header::{HeaderMap, HeaderValue, ACCEPT, AUTHORIZATION, USER_AGENT};
use secrecy::{ExposeSecret, SecretString};
use serde::{Deserialize, Serialize};
use tracing::{debug, info};

use crate::error::{Error, Result};

/// GitHub API client
pub struct GitHubClient {
    client: reqwest::Client,
    base_url: String,
    org: String,
}

/// Repository information from GitHub
#[derive(Debug, Deserialize)]
pub struct Repository {
    pub id: u64,
    pub name: String,
    pub full_name: String,
    pub private: bool,
    pub default_branch: String,
    pub clone_url: String,
    pub ssh_url: String,
    pub language: Option<String>,
    pub archived: bool,
    pub disabled: bool,
}

/// Pull request creation request
#[derive(Debug, Serialize)]
pub struct CreatePullRequest {
    pub title: String,
    pub head: String,
    pub base: String,
    pub body: Option<String>,
    pub draft: bool,
}

/// Pull request response
#[derive(Debug, Deserialize)]
pub struct PullRequest {
    pub id: u64,
    pub number: u64,
    pub html_url: String,
    pub state: String,
}

/// Check run creation request
#[derive(Debug, Serialize)]
pub struct CreateCheckRun {
    pub name: String,
    pub head_sha: String,
    pub status: String,
    pub conclusion: Option<String>,
    pub output: Option<CheckRunOutput>,
}

/// Check run output
#[derive(Debug, Serialize)]
pub struct CheckRunOutput {
    pub title: String,
    pub summary: String,
    pub text: Option<String>,
}

/// Issue creation request
#[derive(Debug, Serialize)]
pub struct CreateIssue {
    pub title: String,
    pub body: Option<String>,
    pub labels: Vec<String>,
}

impl GitHubClient {
    /// Create a new GitHub client with a token
    pub fn new(token: &SecretString, org: &str, base_url: Option<&str>) -> Result<Self> {
        let mut headers = HeaderMap::new();
        headers.insert(
            AUTHORIZATION,
            HeaderValue::from_str(&format!("Bearer {}", token.expose_secret()))
                .map_err(|e| Error::GitHub(e.to_string()))?,
        );
        headers.insert(
            ACCEPT,
            HeaderValue::from_static("application/vnd.github+json"),
        );
        headers.insert(
            USER_AGENT,
            HeaderValue::from_static("robot-repo-automaton/0.1.0"),
        );
        headers.insert(
            "X-GitHub-Api-Version",
            HeaderValue::from_static("2022-11-28"),
        );

        let client = reqwest::Client::builder()
            .default_headers(headers)
            .build()
            .map_err(|e| Error::GitHub(e.to_string()))?;

        Ok(GitHubClient {
            client,
            base_url: base_url
                .unwrap_or("https://api.github.com")
                .to_string(),
            org: org.to_string(),
        })
    }

    /// List all repositories in the organization
    pub async fn list_repos(&self) -> Result<Vec<Repository>> {
        let mut all_repos = Vec::new();
        let mut page = 1;

        loop {
            let url = format!(
                "{}/orgs/{}/repos?per_page=100&page={}",
                self.base_url, self.org, page
            );
            debug!("Fetching repos page {}", page);

            let response = self
                .client
                .get(&url)
                .send()
                .await?
                .error_for_status()
                .map_err(|e| Error::GitHub(e.to_string()))?;

            let repos: Vec<Repository> = response.json().await?;
            if repos.is_empty() {
                break;
            }

            all_repos.extend(repos);
            page += 1;
        }

        info!("Found {} repositories in {}", all_repos.len(), self.org);
        Ok(all_repos)
    }

    /// Get a specific repository
    pub async fn get_repo(&self, name: &str) -> Result<Repository> {
        let url = format!("{}/repos/{}/{}", self.base_url, self.org, name);

        let response = self
            .client
            .get(&url)
            .send()
            .await?
            .error_for_status()
            .map_err(|e| Error::GitHub(e.to_string()))?;

        let repo: Repository = response.json().await?;
        Ok(repo)
    }

    /// Create a pull request
    pub async fn create_pull_request(
        &self,
        repo: &str,
        pr: CreatePullRequest,
    ) -> Result<PullRequest> {
        let url = format!("{}/repos/{}/{}/pulls", self.base_url, self.org, repo);

        let response = self
            .client
            .post(&url)
            .json(&pr)
            .send()
            .await?
            .error_for_status()
            .map_err(|e| Error::GitHub(e.to_string()))?;

        let pull_request: PullRequest = response.json().await?;
        info!("Created PR #{} in {}", pull_request.number, repo);
        Ok(pull_request)
    }

    /// Enable auto-merge on a pull request (squash strategy).
    ///
    /// Uses the GitHub GraphQL API because the REST API does not support
    /// enabling auto-merge. The PR will merge automatically once all
    /// required status checks pass.
    pub async fn enable_auto_merge(&self, repo: &str, pr_number: u64) -> Result<()> {
        // First get the PR node_id via REST (needed for GraphQL mutation)
        let pr_url = format!(
            "{}/repos/{}/{}/pulls/{}",
            self.base_url, self.org, repo, pr_number
        );

        let response = self
            .client
            .get(&pr_url)
            .send()
            .await?
            .error_for_status()
            .map_err(|e| Error::GitHub(e.to_string()))?;

        #[derive(Deserialize)]
        struct PrNodeId {
            node_id: String,
        }

        let pr_info: PrNodeId = response.json().await?;

        // GraphQL mutation to enable auto-merge
        let graphql_url = "https://api.github.com/graphql";

        #[derive(Serialize)]
        struct GraphQLRequest {
            query: String,
        }

        let mutation = GraphQLRequest {
            query: format!(
                r#"mutation {{
                    enablePullRequestAutoMerge(input: {{
                        pullRequestId: "{}",
                        mergeMethod: SQUASH
                    }}) {{
                        pullRequest {{
                            autoMergeRequest {{
                                enabledAt
                            }}
                        }}
                    }}
                }}"#,
                pr_info.node_id
            ),
        };

        let result = self
            .client
            .post(graphql_url)
            .json(&mutation)
            .send()
            .await?;

        if result.status().is_success() {
            info!("Enabled auto-merge (squash) on PR #{} in {}", pr_number, repo);
        } else {
            let status = result.status();
            let body = result.text().await.unwrap_or_default();
            debug!("Auto-merge request returned {}: {}", status, body);
            info!(
                "Auto-merge not available for PR #{} in {} (repo may not have it enabled)",
                pr_number, repo
            );
        }

        Ok(())
    }

    /// Create a check run
    pub async fn create_check_run(
        &self,
        repo: &str,
        check: CreateCheckRun,
    ) -> Result<()> {
        let url = format!(
            "{}/repos/{}/{}/check-runs",
            self.base_url, self.org, repo
        );

        self.client
            .post(&url)
            .json(&check)
            .send()
            .await?
            .error_for_status()
            .map_err(|e| Error::GitHub(e.to_string()))?;

        info!("Created check run '{}' in {}", check.name, repo);
        Ok(())
    }

    /// Create an issue
    pub async fn create_issue(&self, repo: &str, issue: CreateIssue) -> Result<u64> {
        let url = format!("{}/repos/{}/{}/issues", self.base_url, self.org, repo);

        let response = self
            .client
            .post(&url)
            .json(&issue)
            .send()
            .await?
            .error_for_status()
            .map_err(|e| Error::GitHub(e.to_string()))?;

        #[derive(Deserialize)]
        struct IssueResponse {
            number: u64,
        }

        let created: IssueResponse = response.json().await?;
        info!("Created issue #{} in {}", created.number, repo);
        Ok(created.number)
    }

    /// Clone a repository to a local path
    pub async fn clone_repo(&self, repo: &Repository, target: &std::path::Path) -> Result<()> {
        use git2::Repository as GitRepo;

        if target.exists() {
            debug!("Repository already exists at {}", target.display());
            return Ok(());
        }

        info!("Cloning {} to {}", repo.full_name, target.display());

        // Use HTTPS clone URL
        GitRepo::clone(&repo.clone_url, target)?;

        Ok(())
    }

    /// Get the default branch for a repository
    pub async fn get_default_branch(&self, repo: &str) -> Result<String> {
        let repo_info = self.get_repo(repo).await?;
        Ok(repo_info.default_branch)
    }

    /// Create a new branch
    pub async fn create_branch(
        &self,
        repo: &str,
        branch_name: &str,
        from_sha: &str,
    ) -> Result<()> {
        let url = format!("{}/repos/{}/{}/git/refs", self.base_url, self.org, repo);

        #[derive(Serialize)]
        struct CreateRef {
            #[serde(rename = "ref")]
            ref_name: String,
            sha: String,
        }

        let body = CreateRef {
            ref_name: format!("refs/heads/{}", branch_name),
            sha: from_sha.to_string(),
        };

        self.client
            .post(&url)
            .json(&body)
            .send()
            .await?
            .error_for_status()
            .map_err(|e| Error::GitHub(e.to_string()))?;

        info!("Created branch {} in {}", branch_name, repo);
        Ok(())
    }

    /// Get the SHA of a branch
    pub async fn get_branch_sha(&self, repo: &str, branch: &str) -> Result<String> {
        let url = format!(
            "{}/repos/{}/{}/git/ref/heads/{}",
            self.base_url, self.org, repo, branch
        );

        let response = self
            .client
            .get(&url)
            .send()
            .await?
            .error_for_status()
            .map_err(|e| Error::GitHub(e.to_string()))?;

        #[derive(Deserialize)]
        struct RefResponse {
            object: RefObject,
        }
        #[derive(Deserialize)]
        struct RefObject {
            sha: String,
        }

        let ref_info: RefResponse = response.json().await?;
        Ok(ref_info.object.sha)
    }
}
