// SPDX-License-Identifier: PMPL-1.0-or-later

//! Configuration module for Rhodibot
//!
//! # Security considerations
//!
//! - Private keys are read from file or environment variable and stored
//!   in memory only. They are never logged or serialized.
//! - Webhook secrets are stored in memory and used only for HMAC-SHA256
//!   signature verification.
//! - The `Config` struct derives `Debug`, but the `Debug` impl is auto-derived
//!   and will print field contents. In production, avoid logging `Config`
//!   directly; use structured logging of individual non-sensitive fields.

use anyhow::Result;

/// Application configuration
#[derive(Debug, Clone)]
pub struct Config {
    /// GitHub App ID (used for JWT auth when running as a GitHub App).
    ///
    /// TODO: Implement GitHub App JWT authentication using app_id + private_key
    /// to generate installation tokens. Currently only GITHUB_TOKEN env var is used.
    pub app_id: Option<u64>,

    /// GitHub App private key PEM (used for JWT auth when running as a GitHub App).
    ///
    /// TODO: Implement GitHub App JWT authentication using app_id + private_key
    /// to generate installation tokens. Currently only GITHUB_TOKEN env var is used.
    pub private_key: Option<String>,

    /// Webhook secret for HMAC-SHA256 signature verification
    pub webhook_secret: Option<String>,

    /// GitHub API base URL (for GitHub Enterprise support)
    pub github_api_url: String,
}

impl Config {
    /// Create configuration from individual parameters.
    ///
    /// This is used by `main.rs` to bridge CLI arguments into the config.
    pub fn new(
        app_id: Option<u64>,
        private_key_path: Option<&str>,
        webhook_secret: Option<String>,
    ) -> Result<Self> {
        let private_key = if let Some(path) = private_key_path {
            Some(std::fs::read_to_string(path)?)
        } else {
            std::env::var("GITHUB_PRIVATE_KEY").ok()
        };

        Ok(Self {
            app_id,
            private_key,
            webhook_secret,
            github_api_url: std::env::var("GITHUB_API_URL")
                .unwrap_or_else(|_| "https://api.github.com".to_string()),
        })
    }
}
