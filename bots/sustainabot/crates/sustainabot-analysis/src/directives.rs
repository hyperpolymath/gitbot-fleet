// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Parser for `.bot_directives/*.scm` S-expression files.
//!
//! These files control what bots are allowed to do in a given repository.

use anyhow::{Context, Result};
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

/// Check if a specific bot has a directive in the given repo.
///
/// Looks for `.bot_directives/{bot_name}.scm` in the repo root.
pub fn check_directive(repo_path: &Path, bot_name: &str) -> Option<BotDirective> {
    let directive_path = repo_path
        .join(".bot_directives")
        .join(format!("{}.scm", bot_name));

    if !directive_path.exists() {
        return None;
    }

    match parse_directive(&directive_path, bot_name) {
        Ok(d) => Some(d),
        Err(e) => {
            tracing::warn!("Failed to parse directive {}: {}", directive_path.display(), e);
            None
        }
    }
}

/// Parse a bot directive SCM file.
fn parse_directive(path: &Path, bot_name: &str) -> Result<BotDirective> {
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Failed to read directive: {}", path.display()))?;

    let value = lexpr::from_str(&content)
        .with_context(|| format!("Failed to parse S-expression: {}", path.display()))?;

    let mut directive = BotDirective {
        bot: bot_name.to_string(),
        allow: true,
        scopes: Vec::new(),
        deny: Vec::new(),
        notes: None,
        thresholds: Vec::new(),
    };

    // Walk the S-expression looking for known keys
    if let Some(list) = value.as_cons() {
        parse_sexp_list(list, &mut directive);
    }

    Ok(directive)
}

/// Recursively parse S-expression list pairs for directive fields.
fn parse_sexp_list(cons: &lexpr::Cons, directive: &mut BotDirective) {
    // Try to interpret as (key value) pairs
    if let Some(car) = cons.car().as_symbol() {
        match car {
            "bot-directive" | "directive" => {
                // Top-level wrapper, recurse into cdr
                if let Some(rest) = cons.cdr().as_cons() {
                    parse_sexp_list(rest, directive);
                }
            }
            "allow" => {
                if let Some(val) = cons.cdr().as_cons() {
                    if let Some(b) = val.car().as_bool() {
                        directive.allow = b;
                    } else if let Some(s) = val.car().as_symbol() {
                        directive.allow = s == "#t" || s == "true" || s == "yes";
                    }
                }
            }
            "deny" => {
                if let Some(val) = cons.cdr().as_cons() {
                    extract_string_list(val, &mut directive.deny);
                }
            }
            "scope" | "scopes" => {
                if let Some(val) = cons.cdr().as_cons() {
                    extract_string_list(val, &mut directive.scopes);
                }
            }
            "notes" | "note" => {
                if let Some(val) = cons.cdr().as_cons() {
                    if let Some(s) = val.car().as_str() {
                        directive.notes = Some(s.to_string());
                    }
                }
            }
            "threshold" | "thresholds" => {
                if let Some(val) = cons.cdr().as_cons() {
                    parse_thresholds(val, &mut directive.thresholds);
                }
            }
            _ => {}
        }
    }

    // Try to recurse through sibling pairs
    if let Some(next) = cons.cdr().as_cons() {
        // Check if the next item is itself a list (not just a value)
        if next.car().is_cons() {
            if let Some(inner) = next.car().as_cons() {
                parse_sexp_list(inner, directive);
            }
        }
        // Continue with the rest
        if next.cdr().is_cons() {
            if let Some(rest) = next.cdr().as_cons() {
                if rest.car().is_cons() {
                    if let Some(inner) = rest.car().as_cons() {
                        parse_sexp_list(inner, directive);
                    }
                }
            }
        }
    }
}

fn extract_string_list(cons: &lexpr::Cons, target: &mut Vec<String>) {
    if let Some(s) = cons.car().as_str() {
        target.push(s.to_string());
    } else if let Some(s) = cons.car().as_symbol() {
        target.push(s.to_string());
    }
    if let Some(rest) = cons.cdr().as_cons() {
        extract_string_list(rest, target);
    }
}

fn parse_thresholds(cons: &lexpr::Cons, target: &mut Vec<(String, f64)>) {
    // Expect pairs like (energy 100.0) or (carbon 0.5)
    if let Some(inner) = cons.car().as_cons() {
        if let Some(key) = inner.car().as_symbol() {
            if let Some(val_cons) = inner.cdr().as_cons() {
                if let Some(v) = val_cons.car().as_f64() {
                    target.push((key.to_string(), v));
                }
            }
        }
    }
    if let Some(rest) = cons.cdr().as_cons() {
        parse_thresholds(rest, target);
    }
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
}
