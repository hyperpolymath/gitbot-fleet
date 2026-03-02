// SPDX-License-Identifier: PMPL-1.0-or-later
//! Configuration for robot-repo-automaton

use secrecy::SecretString;
use serde::Deserialize;
use std::path::PathBuf;

use crate::error::{Error, Result};

/// Main configuration structure
#[derive(Debug, Deserialize)]
pub struct Config {
    /// Path to error catalog (ERROR-CATALOG.scm)
    pub catalog_path: PathBuf,

    /// GitHub configuration
    pub github: Option<GitHubConfig>,

    /// cicd-hyper-a registry configuration
    pub registry: Option<RegistryConfig>,

    /// Automation settings
    #[serde(default)]
    pub automation: AutomationConfig,

    /// Hook configuration
    #[serde(default)]
    pub hooks: HookConfig,
}

/// GitHub API configuration
#[derive(Debug, Deserialize)]
pub struct GitHubConfig {
    /// GitHub API token
    #[serde(skip)]
    pub token: Option<SecretString>,

    /// GitHub App ID (alternative to token)
    pub app_id: Option<u64>,

    /// GitHub App private key path
    pub app_key_path: Option<PathBuf>,

    /// Organization to operate on
    pub org: String,

    /// Base URL for GitHub API (for GitHub Enterprise)
    #[serde(default = "default_github_api_url")]
    pub api_url: String,
}

fn default_github_api_url() -> String {
    "https://api.github.com".to_string()
}

/// cicd-hyper-a registry configuration
#[derive(Debug, Deserialize)]
pub struct RegistryConfig {
    /// Registry URL
    pub url: String,

    /// Rulesets to apply globally
    #[serde(default)]
    pub global_rulesets: Vec<String>,

    /// Language-specific rulesets
    #[serde(default)]
    pub language_rulesets: std::collections::HashMap<String, Vec<String>>,
}

/// Automation behavior settings
#[derive(Debug, Deserialize)]
pub struct AutomationConfig {
    /// Confidence threshold for auto-applying fixes (0.0-1.0)
    #[serde(default = "default_auto_fix_threshold")]
    pub auto_fix_threshold: f64,

    /// Confidence threshold for creating PRs (0.0-1.0)
    #[serde(default = "default_pr_threshold")]
    pub pr_threshold: f64,

    /// Confidence threshold for alerting humans (0.0-1.0)
    #[serde(default = "default_alert_threshold")]
    pub alert_threshold: f64,

    /// Dry run mode - don't actually make changes
    #[serde(default)]
    pub dry_run: bool,

    /// Maximum concurrent operations
    #[serde(default = "default_max_concurrent")]
    pub max_concurrent: usize,
}

fn default_auto_fix_threshold() -> f64 {
    0.95
}
fn default_pr_threshold() -> f64 {
    0.80
}
fn default_alert_threshold() -> f64 {
    0.50
}
fn default_max_concurrent() -> usize {
    4
}

impl Default for AutomationConfig {
    fn default() -> Self {
        Self {
            auto_fix_threshold: default_auto_fix_threshold(),
            pr_threshold: default_pr_threshold(),
            alert_threshold: default_alert_threshold(),
            dry_run: false,
            max_concurrent: default_max_concurrent(),
        }
    }
}

/// Git hook configuration
#[derive(Debug, Deserialize, Default)]
pub struct HookConfig {
    /// Enable pre-commit hook injection
    #[serde(default)]
    pub pre_commit: bool,

    /// Enable pre-push hook injection
    #[serde(default)]
    pub pre_push: bool,

    /// Hook template directory
    pub template_dir: Option<PathBuf>,
}

impl Config {
    /// Load configuration from a TOML file
    pub fn from_file(path: &PathBuf) -> Result<Self> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| Error::Config(format!("Failed to read config file: {}", e)))?;
        let config: Config = toml::from_str(&content)?;
        Ok(config)
    }

    /// Load configuration with environment variable overrides
    pub fn from_file_with_env(path: &PathBuf) -> Result<Self> {
        let mut config = Self::from_file(path)?;

        // Override GitHub token from environment
        if let Some(ref mut gh) = config.github {
            if let Ok(token) = std::env::var("GITHUB_TOKEN") {
                gh.token = Some(SecretString::from(token));
            }
        }

        Ok(config)
    }
}
