// SPDX-License-Identifier: PMPL-1.0-or-later
//! Directives — per-repo configuration parser for `.machine_readable/bot_directives/panicbot.a2ml`.
//!
//! Each repository can customise panicbot's behaviour via a TOML-shaped A2ML
//! file. If no directive file exists, safe defaults are used:
//! - Allow: `assail`, `adjudicate`, `diagnostics` (static analysis only)
//! - Deny: all dynamic attack modes (`attack`, `assault`, `ambush`, etc.)
//! - Min severity: `low` (report everything)
//! - Timeout: 300 seconds
//!
//! ## Directive Format (A2ML / TOML-shaped)
//!
//! ```toml
//! schema_version = "1.0"
//! bot = "panicbot"
//! scope = "static-analysis"
//! allow = ["assail", "adjudicate", "diagnostics"]
//! deny = ["attack", "assault", "ambush", "amuck", "abduct", "axial"]
//!
//! [config]
//! min-severity = "low"
//! timeout-seconds = 300
//! ```
//!
//! The SCM form was retired 2026-04-17; no fallback is supported.

use crate::config::{AllowedCommands, MinSeverity, PanicbotConfig};
use anyhow::{Context, Result};
use serde::Deserialize;
use std::path::Path;
use std::time::Duration;

/// Path to the bot directive file within a repository.
const DIRECTIVE_PATH: &str = ".machine_readable/bot_directives/panicbot.a2ml";

/// Load panicbot configuration from a repository's directive file.
///
/// If the file doesn't exist, returns the default config (static analysis
/// only, all severities, 300s timeout). If the file exists but can't be
/// parsed, returns an error.
pub fn load_directives(repo_path: &Path) -> Result<PanicbotConfig> {
    let directive_file = repo_path.join(DIRECTIVE_PATH);

    if !directive_file.exists() {
        tracing::debug!(
            "No directive file at {}, using defaults",
            directive_file.display()
        );
        return Ok(PanicbotConfig::default());
    }

    let content = std::fs::read_to_string(&directive_file)
        .with_context(|| format!("Failed to read {}", directive_file.display()))?;

    parse_directive(&content)
        .with_context(|| format!("Failed to parse {}", directive_file.display()))
}

/// Raw A2ML shape.
#[derive(Debug, Default, Deserialize)]
struct DirectiveFile {
    #[serde(default)]
    allow: Option<Vec<String>>,
    #[serde(default)]
    config: Option<ConfigBlock>,
    #[serde(default, rename = "min-severity")]
    min_severity_top: Option<String>,
    #[serde(default, rename = "timeout-seconds")]
    timeout_seconds_top: Option<u64>,
}

#[derive(Debug, Default, Deserialize)]
struct ConfigBlock {
    #[serde(default, rename = "min-severity")]
    min_severity: Option<String>,
    #[serde(default, rename = "timeout-seconds")]
    timeout_seconds: Option<u64>,
}

/// Parse a panicbot directive from its A2ML/TOML content.
fn parse_directive(content: &str) -> Result<PanicbotConfig> {
    let mut config = PanicbotConfig::default();
    let file: DirectiveFile = toml::from_str(content).context("Failed to parse TOML")?;

    // Allow list
    if let Some(allowed) = file.allow {
        config.allowed_commands = AllowedCommands {
            assail: allowed.iter().any(|s| s == "assail"),
            adjudicate: allowed.iter().any(|s| s == "adjudicate"),
            diagnostics: allowed.iter().any(|s| s == "diagnostics"),
        };
    }

    // min-severity: prefer [config] block, fall back to top-level kebab key.
    let severity_str = file
        .config
        .as_ref()
        .and_then(|c| c.min_severity.clone())
        .or(file.min_severity_top);
    if let Some(s) = severity_str {
        if let Some(sev) = MinSeverity::from_str(&s) {
            config.min_severity = sev;
        } else {
            tracing::warn!("Unknown min-severity '{}', using default", s);
        }
    }

    // timeout-seconds: same precedence.
    let timeout_seconds = file
        .config
        .as_ref()
        .and_then(|c| c.timeout_seconds)
        .or(file.timeout_seconds_top);
    if let Some(secs) = timeout_seconds {
        config.timeout = Duration::from_secs(secs);
    }

    Ok(config)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_default_directive() {
        let content = r#"
schema_version = "1.0"
bot = "panicbot"
scope = "static-analysis"
allow = ["assail", "adjudicate", "diagnostics"]
deny = ["attack", "assault", "ambush", "amuck", "abduct", "axial"]

[config]
min-severity = "low"
timeout-seconds = 300
"#;

        let config = parse_directive(content).unwrap();
        assert!(config.allowed_commands.assail);
        assert!(config.allowed_commands.adjudicate);
        assert!(config.allowed_commands.diagnostics);
        assert_eq!(config.min_severity, MinSeverity::Low);
        assert_eq!(config.timeout, Duration::from_secs(300));
    }

    #[test]
    fn test_parse_restricted_directive() {
        let content = r#"
bot = "panicbot"
allow = ["assail"]

[config]
min-severity = "high"
timeout-seconds = 60
"#;

        let config = parse_directive(content).unwrap();
        assert!(config.allowed_commands.assail);
        assert!(!config.allowed_commands.adjudicate);
        assert!(!config.allowed_commands.diagnostics);
        assert_eq!(config.min_severity, MinSeverity::High);
        assert_eq!(config.timeout, Duration::from_secs(60));
    }

    #[test]
    fn test_parse_empty_content() {
        let config = parse_directive("").unwrap();
        // Should return defaults (TOML parses empty as empty struct)
        assert!(config.allowed_commands.assail);
        assert!(config.allowed_commands.adjudicate);
        assert!(config.allowed_commands.diagnostics);
        assert_eq!(config.min_severity, MinSeverity::Low);
    }

    #[test]
    fn test_parse_top_level_kebab_fallback() {
        // Some older files flatten min-severity to the top level.
        let content = r#"
bot = "panicbot"
allow = ["assail", "diagnostics"]
min-severity = "critical"
timeout-seconds = 120
"#;

        let config = parse_directive(content).unwrap();
        assert_eq!(config.min_severity, MinSeverity::Critical);
        assert_eq!(config.timeout, Duration::from_secs(120));
    }

    #[test]
    fn test_load_directives_missing_file() {
        // Non-existent path should return defaults
        let config = load_directives(Path::new("/tmp/nonexistent-repo-12345")).unwrap();
        assert!(config.allowed_commands.assail);
        assert_eq!(config.min_severity, MinSeverity::Low);
    }
}
