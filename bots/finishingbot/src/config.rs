// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Configuration handling for finishing-bot

use crate::error::{FinishingError, Result};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use tracing::debug;

/// Main configuration structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    /// License validation settings
    #[serde(default)]
    pub licenses: LicenseConfig,

    /// Placeholder detection settings
    #[serde(default)]
    pub placeholders: PlaceholderConfig,

    /// Claim verification settings
    #[serde(default)]
    pub claims: ClaimConfig,

    /// Release artifact settings
    #[serde(default)]
    pub release: ReleaseConfig,

    /// Logging configuration
    #[serde(default)]
    pub log: LogConfig,

    /// Paths to exclude from analysis
    #[serde(default)]
    pub exclude: Vec<String>,

    /// Whether to automatically fix issues
    #[serde(default)]
    pub auto_fix: bool,

    /// Dry run mode (no changes)
    #[serde(default)]
    pub dry_run: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            licenses: LicenseConfig::default(),
            placeholders: PlaceholderConfig::default(),
            claims: ClaimConfig::default(),
            release: ReleaseConfig::default(),
            log: LogConfig::default(),
            exclude: vec![
                ".git".to_string(),
                "node_modules".to_string(),
                "target".to_string(),
                "vendor".to_string(),
                ".venv".to_string(),
            ],
            auto_fix: false,
            dry_run: false,
        }
    }
}

/// License validation configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LicenseConfig {
    /// Allowed licenses
    #[serde(default = "default_allowed_licenses")]
    pub allowed: Vec<String>,

    /// Strict mode - fail on unknown licenses
    #[serde(default)]
    pub strict: bool,

    /// Require SPDX headers in source files
    #[serde(default = "default_true")]
    pub require_spdx_headers: bool,

    /// File extensions that require SPDX headers
    #[serde(default = "default_source_extensions")]
    pub source_extensions: Vec<String>,
}

impl Default for LicenseConfig {
    fn default() -> Self {
        Self {
            allowed: default_allowed_licenses(),
            strict: false,
            require_spdx_headers: true,
            source_extensions: default_source_extensions(),
        }
    }
}

fn default_allowed_licenses() -> Vec<String> {
    vec![
        "PMPL-1.0-or-later".to_string(),
        "MIT".to_string(),
        "Apache-2.0".to_string(),
        "BSD-3-Clause".to_string(),
        "PMPL-1.0".to_string(),
    ]
}

fn default_source_extensions() -> Vec<String> {
    vec![
        "rs".to_string(),
        "js".to_string(),
        "ts".to_string(),
        "py".to_string(),
        "go".to_string(),
        "java".to_string(),
        "kt".to_string(),
        "swift".to_string(),
        "c".to_string(),
        "cpp".to_string(),
        "h".to_string(),
        "hpp".to_string(),
        "res".to_string(),
        "ml".to_string(),
        "hs".to_string(),
        "ex".to_string(),
        "exs".to_string(),
        "gleam".to_string(),
        "yml".to_string(),
        "yaml".to_string(),
    ]
}

fn default_true() -> bool {
    true
}

/// Placeholder detection configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlaceholderConfig {
    /// Patterns to detect
    #[serde(default = "default_placeholder_patterns")]
    pub patterns: Vec<String>,

    /// Action to take: "remove", "flag", or "comment"
    #[serde(default = "default_placeholder_action")]
    pub action: PlaceholderAction,

    /// Custom patterns (regex)
    #[serde(default)]
    pub custom_patterns: Vec<String>,

    /// Maximum allowed placeholders (0 = none allowed)
    #[serde(default)]
    pub max_allowed: usize,
}

impl Default for PlaceholderConfig {
    fn default() -> Self {
        Self {
            patterns: default_placeholder_patterns(),
            action: PlaceholderAction::Flag,
            custom_patterns: vec![],
            max_allowed: 0,
        }
    }
}

fn default_placeholder_patterns() -> Vec<String> {
    vec![
        "TODO".to_string(),
        "FIXME".to_string(),
        "XXX".to_string(),
        "HACK".to_string(),
        "BUG".to_string(),
        "UNDONE".to_string(),
    ]
}

fn default_placeholder_action() -> PlaceholderAction {
    PlaceholderAction::Flag
}

/// What to do with placeholders
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum PlaceholderAction {
    /// Remove the placeholder comment entirely
    Remove,
    /// Flag it in output but don't change
    Flag,
    /// Convert to a different comment format
    Comment,
}

/// Claim verification configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClaimConfig {
    /// Verify documentation claims
    #[serde(default = "default_true")]
    pub verify_docs: bool,

    /// Verify test coverage claims
    #[serde(default = "default_true")]
    pub verify_tests: bool,

    /// Verify README existence
    #[serde(default = "default_true")]
    pub require_readme: bool,

    /// Verify CHANGELOG existence for releases
    #[serde(default)]
    pub require_changelog: bool,

    /// Minimum test coverage percentage (0-100, 0 = disabled)
    #[serde(default)]
    pub min_test_coverage: u8,

    /// Verify that claimed dependencies exist
    #[serde(default)]
    pub verify_dependencies: bool,
}

impl Default for ClaimConfig {
    fn default() -> Self {
        Self {
            verify_docs: true,
            verify_tests: true,
            require_readme: true,
            require_changelog: false,
            min_test_coverage: 0,
            verify_dependencies: false,
        }
    }
}

/// Release artifact configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReleaseConfig {
    /// Require hash verification for artifacts
    #[serde(default)]
    pub require_hashes: bool,

    /// Hash algorithms to use/verify
    #[serde(default = "default_hash_algorithms")]
    pub hash_algorithms: Vec<String>,

    /// Require signed releases
    #[serde(default)]
    pub require_signatures: bool,

    /// Require reproducible builds
    #[serde(default)]
    pub require_reproducible: bool,
}

impl Default for ReleaseConfig {
    fn default() -> Self {
        Self {
            require_hashes: false,
            hash_algorithms: default_hash_algorithms(),
            require_signatures: false,
            require_reproducible: false,
        }
    }
}

fn default_hash_algorithms() -> Vec<String> {
    vec!["SHA256".to_string(), "SHA512".to_string()]
}

/// Logging configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogConfig {
    /// Log level (trace, debug, info, warn, error)
    #[serde(default = "default_log_level")]
    pub level: String,

    /// Log file path (optional)
    #[serde(default)]
    pub file: Option<PathBuf>,

    /// Log format (pretty, compact, json)
    #[serde(default = "default_log_format")]
    pub format: String,
}

impl Default for LogConfig {
    fn default() -> Self {
        Self {
            level: default_log_level(),
            file: None,
            format: default_log_format(),
        }
    }
}

fn default_log_level() -> String {
    "info".to_string()
}

fn default_log_format() -> String {
    "pretty".to_string()
}

/// Load configuration from a path
pub fn load_config(path: &Path) -> Result<Config> {
    if !path.exists() {
        debug!("Config file not found at {}, using defaults", path.display());
        return Ok(Config::default());
    }

    let content = std::fs::read_to_string(path)?;

    let config: Config = if path.extension().map(|e| e == "toml").unwrap_or(false) {
        toml::from_str(&content)?
    } else {
        serde_yaml::from_str(&content)?
    };

    debug!(?config, "Loaded configuration");
    Ok(config)
}

/// Get the default config path for a repository
pub fn default_config_path() -> PathBuf {
    PathBuf::from(".finishing-bot/config.yml")
}

/// Write default configuration to a file
pub fn write_default_config(path: &Path) -> Result<()> {
    let config = Config::default();

    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let content = if path.extension().map(|e| e == "toml").unwrap_or(false) {
        toml::to_string_pretty(&config).map_err(|e| FinishingError::Config(e.to_string()))?
    } else {
        serde_yaml::to_string(&config)?
    };

    std::fs::write(path, content)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = Config::default();
        assert!(!config.auto_fix);
        assert!(!config.dry_run);
        assert!(config.licenses.allowed.contains(&"PMPL-1.0-or-later".to_string()));
    }

    #[test]
    fn test_placeholder_action_serde() {
        let yaml = "action: remove";
        let config: PlaceholderConfig = serde_yaml::from_str(&format!(
            "patterns: [TODO]\n{}", yaml
        )).unwrap();
        assert_eq!(config.action, PlaceholderAction::Remove);
    }
}
