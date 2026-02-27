// SPDX-License-Identifier: PMPL-1.0-or-later
//! Directives — per-repo configuration parser for `.machine_readable/bot_directives/panicbot.scm`.
//!
//! Each repository can customise panicbot's behaviour via a Scheme-formatted
//! directive file. If no directive file exists, safe defaults are used:
//! - Allow: `assail`, `adjudicate`, `diagnostics` (static analysis only)
//! - Deny: all dynamic attack modes (`attack`, `assault`, `ambush`, etc.)
//! - Min severity: `low` (report everything)
//! - Timeout: 300 seconds
//!
//! ## Directive Format
//!
//! ```scheme
//! (bot-directive
//!   (bot "panicbot")
//!   (scope "static-analysis")
//!   (allow ("assail" "adjudicate" "diagnostics"))
//!   (deny ("attack" "assault" "ambush" "amuck" "abduct" "axial"))
//!   (config
//!     (min-severity "low")
//!     (timeout-seconds 300)))
//! ```

use crate::config::{AllowedCommands, MinSeverity, PanicbotConfig};
use anyhow::{Context, Result};
use std::path::Path;
use std::time::Duration;

/// Path to the bot directive file within a repository.
const DIRECTIVE_PATH: &str = ".machine_readable/bot_directives/panicbot.scm";

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

/// Parse a panicbot directive from its Scheme-formatted content.
///
/// This is a lightweight parser that extracts key-value pairs from the
/// S-expression format without requiring a full Scheme interpreter.
/// We look for specific patterns rather than doing a full parse.
fn parse_directive(content: &str) -> Result<PanicbotConfig> {
    let mut config = PanicbotConfig::default();

    // Extract allow list
    if let Some(allow_str) = extract_list(content, "allow") {
        let allowed: Vec<String> = parse_string_list(&allow_str);
        config.allowed_commands = AllowedCommands {
            assail: allowed.iter().any(|s| s == "assail"),
            adjudicate: allowed.iter().any(|s| s == "adjudicate"),
            diagnostics: allowed.iter().any(|s| s == "diagnostics"),
        };
    }

    // Extract min-severity from config block
    if let Some(severity_str) = extract_config_value(content, "min-severity") {
        if let Some(severity) = MinSeverity::from_str(&severity_str) {
            config.min_severity = severity;
        } else {
            tracing::warn!("Unknown min-severity '{}', using default", severity_str);
        }
    }

    // Extract timeout from config block
    if let Some(timeout_str) = extract_config_value(content, "timeout-seconds") {
        if let Ok(seconds) = timeout_str.parse::<u64>() {
            config.timeout = Duration::from_secs(seconds);
        } else {
            tracing::warn!("Invalid timeout-seconds '{}', using default", timeout_str);
        }
    }

    Ok(config)
}

/// Extract a parenthesised list value for a given key.
///
/// For input `(allow ("assail" "adjudicate"))`, returns `"assail" "adjudicate"`.
fn extract_list(content: &str, key: &str) -> Option<String> {
    let pattern = format!("({}", key);
    let start = content.find(&pattern)?;
    let rest = &content[start + pattern.len()..];

    // Find the opening paren of the list
    let list_start = rest.find('(')?;
    let list_content = &rest[list_start + 1..];

    // Find the matching closing paren
    let list_end = list_content.find(')')?;
    Some(list_content[..list_end].to_string())
}

/// Extract a config value for a given key from a `(config ...)` block.
///
/// For input `(config (min-severity "low"))`, with key "min-severity",
/// returns "low".
fn extract_config_value(content: &str, key: &str) -> Option<String> {
    let pattern = format!("({}", key);
    let start = content.find(&pattern)?;
    let rest = &content[start + pattern.len()..];

    // Find the value — could be a quoted string or a bare number
    let trimmed = rest.trim_start();

    if trimmed.starts_with('"') {
        // Quoted string value
        let after_quote = &trimmed[1..];
        let end_quote = after_quote.find('"')?;
        Some(after_quote[..end_quote].to_string())
    } else {
        // Bare value (number, identifier)
        let end = trimmed.find(|c: char| c == ')' || c.is_whitespace())?;
        Some(trimmed[..end].to_string())
    }
}

/// Parse a space-separated list of quoted strings.
///
/// Input: `"assail" "adjudicate" "diagnostics"`
/// Output: `["assail", "adjudicate", "diagnostics"]`
fn parse_string_list(input: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&c) = chars.peek() {
        if c == '"' {
            chars.next(); // consume opening quote
            let mut s = String::new();
            while let Some(&c2) = chars.peek() {
                if c2 == '"' {
                    chars.next(); // consume closing quote
                    break;
                }
                s.push(c2);
                chars.next();
            }
            if !s.is_empty() {
                result.push(s);
            }
        } else {
            chars.next(); // skip whitespace/other
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_default_directive() {
        let content = r#"
;; SPDX-License-Identifier: PMPL-1.0-or-later

(bot-directive
  (bot "panicbot")
  (scope "static-analysis")
  (allow ("assail" "adjudicate" "diagnostics"))
  (deny ("attack" "assault" "ambush" "amuck" "abduct" "axial"))
  (config
    (min-severity "low")
    (timeout-seconds 300)))
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
(bot-directive
  (bot "panicbot")
  (scope "static-analysis")
  (allow ("assail"))
  (config
    (min-severity "high")
    (timeout-seconds 60)))
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
        // Should return defaults
        assert!(config.allowed_commands.assail);
        assert!(config.allowed_commands.adjudicate);
        assert!(config.allowed_commands.diagnostics);
        assert_eq!(config.min_severity, MinSeverity::Low);
    }

    #[test]
    fn test_extract_list() {
        let content = r#"(allow ("assail" "adjudicate"))"#;
        let list = extract_list(content, "allow").unwrap();
        assert_eq!(list, r#""assail" "adjudicate""#);
    }

    #[test]
    fn test_extract_config_value_string() {
        let content = r#"(min-severity "high")"#;
        let val = extract_config_value(content, "min-severity").unwrap();
        assert_eq!(val, "high");
    }

    #[test]
    fn test_extract_config_value_number() {
        let content = r#"(timeout-seconds 600)"#;
        let val = extract_config_value(content, "timeout-seconds").unwrap();
        assert_eq!(val, "600");
    }

    #[test]
    fn test_parse_string_list() {
        let input = r#""assail" "adjudicate" "diagnostics""#;
        let result = parse_string_list(input);
        assert_eq!(result, vec!["assail", "adjudicate", "diagnostics"]);
    }

    #[test]
    fn test_parse_string_list_empty() {
        let result = parse_string_list("");
        assert!(result.is_empty());
    }

    #[test]
    fn test_load_directives_missing_file() {
        // Non-existent path should return defaults
        let config = load_directives(Path::new("/tmp/nonexistent-repo-12345")).unwrap();
        assert!(config.allowed_commands.assail);
        assert_eq!(config.min_severity, MinSeverity::Low);
    }
}
