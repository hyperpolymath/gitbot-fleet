// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Parser for `.machine_readable/bot_directives/*.a2ml` files.
//!
//! These files control what bots are allowed to do in a given repository.
//! The format is TOML-shaped A2ML; the SCM form was retired 2026-04-17.

use anyhow::{Context, Result};
use serde::Deserialize;
use std::path::Path;

/// A parsed bot directive
#[derive(Debug, Clone)]
pub struct BotDirective {
    /// Bot name this directive applies to
    pub bot: String,
    /// Whether this bot is allowed to run
    pub allow: bool,
    /// Scopes the bot is allowed to operate in
    pub scopes: Vec<String>,
    /// Scopes explicitly denied
    pub deny: Vec<String>,
    /// Freeform notes
    pub notes: Option<String>,
    /// Custom threshold overrides
    pub thresholds: Vec<(String, f64)>,
}

/// Raw A2ML shape (TOML deserialization target). Fields here mirror the
/// migration-script output at `.machine_readable/bot_directives/<bot>.a2ml`.
#[derive(Debug, Deserialize)]
struct DirectiveFile {
    #[serde(default)]
    bot: Option<String>,
    /// Either a single scope string or a list of scopes.
    #[serde(default)]
    scope: Option<ScopeField>,
    #[serde(default)]
    scopes: Option<Vec<String>>,
    #[serde(default)]
    allow: Option<AllowField>,
    #[serde(default)]
    deny: Option<Vec<String>>,
    #[serde(default)]
    notes: Option<String>,
    #[serde(default)]
    thresholds: Option<toml::value::Table>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum ScopeField {
    One(String),
    Many(Vec<String>),
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum AllowField {
    Bool(bool),
    Scopes(Vec<String>),
}

/// Check if a specific bot has a directive in the given repo.
///
/// Looks for `.machine_readable/bot_directives/{bot_name}.a2ml`. Returns
/// `None` if the file does not exist or fails to parse.
pub fn check_directive(repo_path: &Path, bot_name: &str) -> Option<BotDirective> {
    let path = repo_path
        .join(".machine_readable")
        .join("bot_directives")
        .join(format!("{}.a2ml", bot_name));

    if !path.exists() {
        return None;
    }

    match parse_directive(&path, bot_name) {
        Ok(d) => Some(d),
        Err(e) => {
            tracing::warn!("Failed to parse directive {}: {}", path.display(), e);
            None
        }
    }
}

/// Parse a bot directive A2ML file.
fn parse_directive(path: &Path, bot_name: &str) -> Result<BotDirective> {
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Failed to read directive: {}", path.display()))?;

    let file: DirectiveFile = toml::from_str(&content)
        .with_context(|| format!("Failed to parse A2ML: {}", path.display()))?;

    let mut scopes: Vec<String> = match file.scope {
        Some(ScopeField::One(s)) => vec![s],
        Some(ScopeField::Many(v)) => v,
        None => Vec::new(),
    };
    if let Some(mut extra) = file.scopes {
        scopes.append(&mut extra);
    }

    let allow = match file.allow {
        // Plain boolean: honour as-is.
        Some(AllowField::Bool(b)) => b,
        // List-of-scopes: treat as allow = true + union the list into scopes.
        Some(AllowField::Scopes(list)) => {
            scopes.extend(list);
            true
        }
        // No allow field → default to allowed (conservative parse).
        None => true,
    };

    let thresholds = file
        .thresholds
        .unwrap_or_default()
        .into_iter()
        .filter_map(|(k, v)| v.as_float().map(|f| (k, f)))
        .collect();

    Ok(BotDirective {
        bot: file.bot.unwrap_or_else(|| bot_name.to_string()),
        allow,
        scopes,
        deny: file.deny.unwrap_or_default(),
        notes: file.notes,
        thresholds,
    })
}

/// Check if the directive allows a specific scope
pub fn is_scope_allowed(directive: &BotDirective, scope: &str) -> bool {
    if !directive.allow {
        return false;
    }

    // If deny list contains this scope, it's denied
    if directive.deny.iter().any(|d| d == scope) {
        return false;
    }

    // If scopes list is empty, all scopes are allowed
    if directive.scopes.is_empty() {
        return true;
    }

    // Otherwise, scope must be in the allow list
    directive.scopes.iter().any(|s| s == scope)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn write_directive(dir: &Path, bot: &str, body: &str) {
        let d = dir.join(".machine_readable").join("bot_directives");
        fs::create_dir_all(&d).unwrap();
        fs::write(d.join(format!("{}.a2ml", bot)), body).unwrap();
    }

    #[test]
    fn test_default_directive() {
        let d = BotDirective {
            bot: "test".to_string(),
            allow: true,
            scopes: vec![],
            deny: vec![],
            notes: None,
            thresholds: vec![],
        };
        assert!(is_scope_allowed(&d, "anything"));
    }

    #[test]
    fn test_denied_scope() {
        let d = BotDirective {
            bot: "test".to_string(),
            allow: true,
            scopes: vec![],
            deny: vec!["security".to_string()],
            notes: None,
            thresholds: vec![],
        };
        assert!(!is_scope_allowed(&d, "security"));
        assert!(is_scope_allowed(&d, "eco"));
    }

    #[test]
    fn test_fully_denied() {
        let d = BotDirective {
            bot: "test".to_string(),
            allow: false,
            scopes: vec![],
            deny: vec![],
            notes: None,
            thresholds: vec![],
        };
        assert!(!is_scope_allowed(&d, "anything"));
    }

    #[test]
    fn test_parse_typical_bot_directive() {
        let dir = TempDir::new().unwrap();
        write_directive(
            dir.path(),
            "echidnabot",
            r#"
schema_version = "1.0"
directive_type = "bot-directive"
bot = "echidnabot"
scope = "formal verification and fuzzing"
allow = ["analysis", "fuzzing", "proof checks"]
deny = ["write to core modules", "write to bindings"]
notes = "May open findings; code changes require explicit approval"
"#,
        );

        let directive = check_directive(dir.path(), "echidnabot").expect("should parse");
        assert_eq!(directive.bot, "echidnabot");
        assert!(directive.allow);
        assert!(directive.scopes.contains(&"fuzzing".to_string()));
        assert!(directive.scopes.contains(&"formal verification and fuzzing".to_string()));
        assert!(directive.deny.contains(&"write to core modules".to_string()));
        assert!(directive.notes.is_some());
    }

    #[test]
    fn test_parse_allow_false() {
        let dir = TempDir::new().unwrap();
        write_directive(
            dir.path(),
            "rhodibot",
            r#"
schema_version = "1.0"
bot = "rhodibot"
allow = false
"#,
        );

        let directive = check_directive(dir.path(), "rhodibot").expect("should parse");
        assert!(!directive.allow);
        assert!(!is_scope_allowed(&directive, "anything"));
    }

    #[test]
    fn test_missing_file_returns_none() {
        let dir = TempDir::new().unwrap();
        assert!(check_directive(dir.path(), "nonexistent").is_none());
    }
}
