// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Error types for finishing-bot

use thiserror::Error;

/// Main error type for finishing-bot
#[derive(Error, Debug)]
pub enum FinishingError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Configuration error: {0}")]
    Config(String),

    #[error("Git error: {0}")]
    Git(#[from] git2::Error),

    #[error("YAML parse error: {0}")]
    Yaml(#[from] serde_yaml::Error),

    #[error("TOML parse error: {0}")]
    Toml(#[from] toml::de::Error),

    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),

    #[error("HTTP error: {0}")]
    Http(#[from] reqwest::Error),

    #[error("Validation failed: {0}")]
    Validation(String),

    #[error("License violation: {0}")]
    LicenseViolation(String),

    #[error("Placeholder found: {0}")]
    PlaceholderFound(String),

    #[error("Claim verification failed: {0}")]
    ClaimFailed(String),

    #[error("Glob pattern error: {0}")]
    GlobPattern(#[from] globset::Error),

    #[error("Regex error: {0}")]
    Regex(#[from] regex::Error),
}

pub type Result<T> = std::result::Result<T, FinishingError>;
