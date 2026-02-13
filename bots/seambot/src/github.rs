// SPDX-License-Identifier: PMPL-1.0-or-later

//! GitHub App integration for seambot
//!
//! Provides GitHub App authentication, Checks API integration,
//! and PR commenting capabilities.

use anyhow::{Context, Result};
use chrono::{Duration, Utc};
use jsonwebtoken::{encode, Algorithm, EncodingKey, Header};
use reqwest::Client;
use serde::{Deserialize, Serialize};
use tracing::{debug, info};

use crate::seam::{CheckResult, CheckStatus, Finding, Severity};

/// GitHub App configuration
#[derive(Debug, Clone)]
pub struct GitHubAppConfig {
    /// GitHub App ID
    pub app_id: u64,
    /// Path to the private key PEM file
    pub private_key_path: String,
    /// Installation ID for the repository
    pub installation_id: u64,
    /// GitHub API base URL (for GitHub Enterprise)
    pub api_base_url: String,
}

impl GitHubAppConfig {
    pub fn new(app_id: u64, private_key_path: String, installation_id: u64) -> Self {
        Self {
            app_id,
            private_key_path,
            installation_id,
            api_base_url: "https://api.github.com".to_string(),
        }
    }

    pub fn with_enterprise_url(mut self, url: String) -> Self {
        self.api_base_url = url;
        self
    }
}

/// JWT claims for GitHub App authentication
#[derive(Debug, Serialize)]
struct GitHubAppClaims {
    /// Issued at time
    iat: i64,
    /// Expiration time (max 10 minutes)
    exp: i64,
    /// Issuer (GitHub App ID)
    iss: String,
}

/// Installation access token response
#[derive(Debug, Deserialize)]
struct InstallationToken {
    token: String,
    expires_at: String,
}

/// GitHub Check Run status
#[allow(dead_code)]
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CheckRunStatus {
    Queued,
    InProgress,
    Completed,
}

/// GitHub Check Run conclusion
#[allow(dead_code)]
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CheckRunConclusion {
    ActionRequired,
    Cancelled,
    Failure,
    Neutral,
    Success,
    Skipped,
    TimedOut,
}

/// GitHub Check Run output
#[derive(Debug, Clone, Serialize)]
pub struct CheckRunOutput {
    pub title: String,
    pub summary: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub annotations: Vec<CheckRunAnnotation>,
}

/// GitHub Check Run annotation
#[derive(Debug, Clone, Serialize)]
pub struct CheckRunAnnotation {
    pub path: String,
    pub start_line: u32,
    pub end_line: u32,
    pub annotation_level: AnnotationLevel,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
}

/// Annotation severity level
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum AnnotationLevel {
    Notice,
    Warning,
    Failure,
}

/// Create Check Run request
#[derive(Debug, Serialize)]
struct CreateCheckRunRequest {
    name: String,
    head_sha: String,
    status: CheckRunStatus,
    #[serde(skip_serializing_if = "Option::is_none")]
    conclusion: Option<CheckRunConclusion>,
    #[serde(skip_serializing_if = "Option::is_none")]
    output: Option<CheckRunOutput>,
    #[serde(skip_serializing_if = "Option::is_none")]
    started_at: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    completed_at: Option<String>,
}

/// Update Check Run request
#[derive(Debug, Serialize)]
struct UpdateCheckRunRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    status: Option<CheckRunStatus>,
    #[serde(skip_serializing_if = "Option::is_none")]
    conclusion: Option<CheckRunConclusion>,
    #[serde(skip_serializing_if = "Option::is_none")]
    output: Option<CheckRunOutput>,
    #[serde(skip_serializing_if = "Option::is_none")]
    completed_at: Option<String>,
}

/// Check Run response
#[derive(Debug, Deserialize)]
pub struct CheckRunResponse {
    pub id: u64,
    pub name: String,
    #[allow(dead_code)]
    pub status: String,
    pub conclusion: Option<String>,
    pub html_url: String,
}

/// PR comment request
#[derive(Debug, Serialize)]
struct CreateCommentRequest {
    body: String,
}

/// PR comment response
#[derive(Debug, Deserialize)]
pub struct CommentResponse {
    pub id: u64,
    pub html_url: String,
}

/// GitHub App client
pub struct GitHubClient {
    config: GitHubAppConfig,
    http: Client,
    installation_token: Option<String>,
    token_expires_at: Option<chrono::DateTime<Utc>>,
}

impl GitHubClient {
    /// Create a new GitHub App client
    pub fn new(config: GitHubAppConfig) -> Result<Self> {
        let http = Client::builder()
            .user_agent("seambot/0.1.0")
            .build()
            .context("Failed to create HTTP client")?;

        Ok(Self {
            config,
            http,
            installation_token: None,
            token_expires_at: None,
        })
    }

    /// Generate a JWT for GitHub App authentication
    fn generate_jwt(&self) -> Result<String> {
        let private_key = std::fs::read_to_string(&self.config.private_key_path)
            .context("Failed to read private key file")?;

        let now = Utc::now();
        let claims = GitHubAppClaims {
            iat: (now - Duration::seconds(60)).timestamp(),
            exp: (now + Duration::minutes(9)).timestamp(),
            iss: self.config.app_id.to_string(),
        };

        let header = Header::new(Algorithm::RS256);
        let key = EncodingKey::from_rsa_pem(private_key.as_bytes())
            .context("Failed to parse private key")?;

        encode(&header, &claims, &key).context("Failed to encode JWT")
    }

    /// Get or refresh the installation access token
    async fn get_installation_token(&mut self) -> Result<String> {
        // Check if we have a valid cached token
        if let (Some(token), Some(expires_at)) =
            (&self.installation_token, &self.token_expires_at)
        {
            if Utc::now() < *expires_at - Duration::minutes(5) {
                return Ok(token.clone());
            }
        }

        // Generate a new token
        let jwt = self.generate_jwt()?;

        let url = format!(
            "{}/app/installations/{}/access_tokens",
            self.config.api_base_url, self.config.installation_id
        );

        debug!("Requesting installation token from {}", url);

        let response = self
            .http
            .post(&url)
            .header("Authorization", format!("Bearer {}", jwt))
            .header("Accept", "application/vnd.github+json")
            .header("X-GitHub-Api-Version", "2022-11-28")
            .send()
            .await
            .context("Failed to request installation token")?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            anyhow::bail!("Failed to get installation token: {} - {}", status, body);
        }

        let token_response: InstallationToken = response
            .json()
            .await
            .context("Failed to parse installation token response")?;

        let expires_at = chrono::DateTime::parse_from_rfc3339(&token_response.expires_at)
            .context("Failed to parse token expiration")?
            .with_timezone(&Utc);

        self.installation_token = Some(token_response.token.clone());
        self.token_expires_at = Some(expires_at);

        info!("Obtained installation token (expires at {})", expires_at);

        Ok(token_response.token)
    }

    /// Create a new check run
    pub async fn create_check_run(
        &mut self,
        owner: &str,
        repo: &str,
        head_sha: &str,
        name: &str,
    ) -> Result<CheckRunResponse> {
        let token = self.get_installation_token().await?;

        let url = format!(
            "{}/repos/{}/{}/check-runs",
            self.config.api_base_url, owner, repo
        );

        let request = CreateCheckRunRequest {
            name: name.to_string(),
            head_sha: head_sha.to_string(),
            status: CheckRunStatus::InProgress,
            conclusion: None,
            output: None,
            started_at: Some(Utc::now().to_rfc3339()),
            completed_at: None,
        };

        debug!("Creating check run: {} for {}", name, head_sha);

        let response = self
            .http
            .post(&url)
            .header("Authorization", format!("Bearer {}", token))
            .header("Accept", "application/vnd.github+json")
            .header("X-GitHub-Api-Version", "2022-11-28")
            .json(&request)
            .send()
            .await
            .context("Failed to create check run")?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            anyhow::bail!("Failed to create check run: {} - {}", status, body);
        }

        let check_run: CheckRunResponse = response
            .json()
            .await
            .context("Failed to parse check run response")?;

        info!("Created check run: {} (ID: {})", check_run.name, check_run.id);

        Ok(check_run)
    }

    /// Update a check run with results
    pub async fn update_check_run(
        &mut self,
        owner: &str,
        repo: &str,
        check_run_id: u64,
        result: &CheckResult,
    ) -> Result<CheckRunResponse> {
        let token = self.get_installation_token().await?;

        let url = format!(
            "{}/repos/{}/{}/check-runs/{}",
            self.config.api_base_url, owner, repo, check_run_id
        );

        let conclusion = match result.status {
            CheckStatus::Pass => CheckRunConclusion::Success,
            CheckStatus::Warn => CheckRunConclusion::Neutral,
            CheckStatus::Fail => CheckRunConclusion::Failure,
        };

        let output = self.build_check_output(result);

        let request = UpdateCheckRunRequest {
            status: Some(CheckRunStatus::Completed),
            conclusion: Some(conclusion),
            output: Some(output),
            completed_at: Some(Utc::now().to_rfc3339()),
        };

        debug!("Updating check run {} with conclusion", check_run_id);

        let response = self
            .http
            .patch(&url)
            .header("Authorization", format!("Bearer {}", token))
            .header("Accept", "application/vnd.github+json")
            .header("X-GitHub-Api-Version", "2022-11-28")
            .json(&request)
            .send()
            .await
            .context("Failed to update check run")?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            anyhow::bail!("Failed to update check run: {} - {}", status, body);
        }

        let check_run: CheckRunResponse = response
            .json()
            .await
            .context("Failed to parse check run response")?;

        info!(
            "Updated check run: {} with conclusion {:?}",
            check_run.name, check_run.conclusion
        );

        Ok(check_run)
    }

    /// Build check output from results
    fn build_check_output(&self, result: &CheckResult) -> CheckRunOutput {
        let title = match result.status {
            CheckStatus::Pass => "Seam hygiene check passed",
            CheckStatus::Warn => "Seam hygiene check passed with warnings",
            CheckStatus::Fail => "Seam hygiene check failed",
        };

        let summary = format!(
            "**Status:** {:?}\n\n\
             | Metric | Count |\n\
             |--------|-------|\n\
             | Seams Checked | {}/{} |\n\
             | Errors | {} |\n\
             | Warnings | {} |\n\
             | Info | {} |",
            result.status,
            result.summary.checked_seams,
            result.summary.total_seams,
            result.summary.errors,
            result.summary.warnings,
            result.summary.info
        );

        let text = if result.findings.is_empty() {
            None
        } else {
            Some(self.format_findings_markdown(&result.findings))
        };

        let annotations = result
            .findings
            .iter()
            .filter_map(|f| self.finding_to_annotation(f))
            .take(50) // GitHub limits to 50 annotations per request
            .collect();

        CheckRunOutput {
            title: title.to_string(),
            summary,
            text,
            annotations,
        }
    }

    /// Format findings as markdown text
    fn format_findings_markdown(&self, findings: &[Finding]) -> String {
        let mut text = String::new();

        // Group by severity
        let errors: Vec<_> = findings
            .iter()
            .filter(|f| matches!(f.severity, Severity::Critical | Severity::Error))
            .collect();

        let warnings: Vec<_> = findings
            .iter()
            .filter(|f| matches!(f.severity, Severity::Warning))
            .collect();

        let info: Vec<_> = findings
            .iter()
            .filter(|f| matches!(f.severity, Severity::Info))
            .collect();

        if !errors.is_empty() {
            text.push_str("## Errors\n\n");
            for finding in errors {
                text.push_str(&self.format_single_finding(finding));
            }
        }

        if !warnings.is_empty() {
            text.push_str("## Warnings\n\n");
            for finding in warnings {
                text.push_str(&self.format_single_finding(finding));
            }
        }

        if !info.is_empty() {
            text.push_str("## Info\n\n");
            for finding in info {
                text.push_str(&self.format_single_finding(finding));
            }
        }

        text
    }

    fn format_single_finding(&self, finding: &Finding) -> String {
        let mut text = format!("- **[{}]** {}\n", finding.check, finding.message);

        if let Some(ref loc) = finding.location {
            text.push_str(&format!("  - Location: `{}`\n", loc));
        }

        if let Some(ref seam_id) = finding.seam_id {
            text.push_str(&format!("  - Seam: `{}`\n", seam_id));
        }

        if let Some(ref suggestion) = finding.suggestion {
            text.push_str(&format!("  - *Suggestion: {}*\n", suggestion));
        }

        text.push('\n');
        text
    }

    /// Convert a finding to a GitHub annotation
    fn finding_to_annotation(&self, finding: &Finding) -> Option<CheckRunAnnotation> {
        let path = finding.location.as_ref()?;

        let annotation_level = match finding.severity {
            Severity::Critical | Severity::Error => AnnotationLevel::Failure,
            Severity::Warning => AnnotationLevel::Warning,
            Severity::Info => AnnotationLevel::Notice,
        };

        Some(CheckRunAnnotation {
            path: path.clone(),
            start_line: 1,
            end_line: 1,
            annotation_level,
            message: finding.message.clone(),
            title: Some(finding.check.clone()),
        })
    }

    /// Post a comment on a pull request
    pub async fn post_pr_comment(
        &mut self,
        owner: &str,
        repo: &str,
        pr_number: u64,
        result: &CheckResult,
    ) -> Result<CommentResponse> {
        let token = self.get_installation_token().await?;

        let url = format!(
            "{}/repos/{}/{}/issues/{}/comments",
            self.config.api_base_url, owner, repo, pr_number
        );

        let body = self.build_pr_comment(result);

        let request = CreateCommentRequest { body };

        debug!("Posting comment to PR #{}", pr_number);

        let response = self
            .http
            .post(&url)
            .header("Authorization", format!("Bearer {}", token))
            .header("Accept", "application/vnd.github+json")
            .header("X-GitHub-Api-Version", "2022-11-28")
            .json(&request)
            .send()
            .await
            .context("Failed to post PR comment")?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            anyhow::bail!("Failed to post PR comment: {} - {}", status, body);
        }

        let comment: CommentResponse = response
            .json()
            .await
            .context("Failed to parse comment response")?;

        info!("Posted comment {} to PR #{}", comment.id, pr_number);

        Ok(comment)
    }

    /// Build PR comment body from results
    fn build_pr_comment(&self, result: &CheckResult) -> String {
        let status_emoji = match result.status {
            CheckStatus::Pass => "✅",
            CheckStatus::Warn => "⚠️",
            CheckStatus::Fail => "❌",
        };

        let mut comment = format!(
            "## {} Seambot Check Results\n\n\
             | Metric | Value |\n\
             |--------|-------|\n\
             | Status | {:?} |\n\
             | Seams Checked | {}/{} |\n\
             | Errors | {} |\n\
             | Warnings | {} |\n\
             | Info | {} |\n\n",
            status_emoji,
            result.status,
            result.summary.checked_seams,
            result.summary.total_seams,
            result.summary.errors,
            result.summary.warnings,
            result.summary.info
        );

        if !result.findings.is_empty() {
            comment.push_str(&self.format_findings_markdown(&result.findings));
        }

        comment.push_str("\n---\n*Generated by [seambot](https://github.com/hyperpolymath/seambot)*");

        comment
    }
}

/// GitHub webhook event types
///
/// These types are provided for consumers who want to parse GitHub webhook payloads.
#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "action")]
#[serde(rename_all = "snake_case")]
pub enum WebhookEvent {
    /// Check suite requested
    CheckSuiteRequested {
        check_suite: CheckSuitePayload,
        repository: RepositoryPayload,
        installation: InstallationPayload,
    },
    /// Check suite rerequested
    CheckSuiteRerequested {
        check_suite: CheckSuitePayload,
        repository: RepositoryPayload,
        installation: InstallationPayload,
    },
    /// Pull request opened
    Opened {
        pull_request: PullRequestPayload,
        repository: RepositoryPayload,
        installation: InstallationPayload,
    },
    /// Pull request synchronized (new commits)
    Synchronize {
        pull_request: PullRequestPayload,
        repository: RepositoryPayload,
        installation: InstallationPayload,
    },
}

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
pub struct CheckSuitePayload {
    pub id: u64,
    pub head_sha: String,
    pub head_branch: Option<String>,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
pub struct PullRequestPayload {
    pub number: u64,
    pub head: PullRequestHead,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
pub struct PullRequestHead {
    pub sha: String,
    #[serde(rename = "ref")]
    pub ref_name: String,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
pub struct RepositoryPayload {
    pub id: u64,
    pub name: String,
    pub full_name: String,
    pub owner: RepositoryOwner,
    pub clone_url: String,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
pub struct RepositoryOwner {
    pub login: String,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
pub struct InstallationPayload {
    pub id: u64,
}

/// Verify webhook signature
pub fn verify_webhook_signature(payload: &[u8], signature: &str, secret: &str) -> bool {
    let expected = format!(
        "sha256={}",
        hex::encode(
            hmac_sha256::HMAC::mac(payload, secret.as_bytes())
        )
    );

    // Constant-time comparison
    signature.len() == expected.len()
        && signature
            .bytes()
            .zip(expected.bytes())
            .fold(0u8, |acc, (a, b)| acc | (a ^ b))
            == 0
}

/// Load GitHub App configuration from environment
#[allow(dead_code)]
pub fn load_config_from_env() -> Result<GitHubAppConfig> {
    let app_id: u64 = std::env::var("GITHUB_APP_ID")
        .context("GITHUB_APP_ID not set")?
        .parse()
        .context("Invalid GITHUB_APP_ID")?;

    let private_key_path = std::env::var("GITHUB_APP_PRIVATE_KEY_PATH")
        .context("GITHUB_APP_PRIVATE_KEY_PATH not set")?;

    let installation_id: u64 = std::env::var("GITHUB_APP_INSTALLATION_ID")
        .context("GITHUB_APP_INSTALLATION_ID not set")?
        .parse()
        .context("Invalid GITHUB_APP_INSTALLATION_ID")?;

    let mut config = GitHubAppConfig::new(app_id, private_key_path, installation_id);

    if let Ok(url) = std::env::var("GITHUB_API_URL") {
        config = config.with_enterprise_url(url);
    }

    Ok(config)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_webhook_signature_verification() {
        let payload = b"test payload";
        let secret = "test-secret";

        // Generate a valid signature
        let signature = format!(
            "sha256={}",
            hex::encode(hmac_sha256::HMAC::mac(payload, secret.as_bytes()))
        );

        assert!(verify_webhook_signature(payload, &signature, secret));
        assert!(!verify_webhook_signature(payload, "sha256=invalid", secret));
        assert!(!verify_webhook_signature(payload, &signature, "wrong-secret"));
    }

    #[test]
    fn test_config_creation() {
        let config = GitHubAppConfig::new(12345, "/path/to/key.pem".to_string(), 67890);

        assert_eq!(config.app_id, 12345);
        assert_eq!(config.installation_id, 67890);
        assert_eq!(config.api_base_url, "https://api.github.com");
    }

    #[test]
    fn test_enterprise_url() {
        let config = GitHubAppConfig::new(12345, "/path/to/key.pem".to_string(), 67890)
            .with_enterprise_url("https://github.example.com/api/v3".to_string());

        assert_eq!(config.api_base_url, "https://github.example.com/api/v3");
    }
}
