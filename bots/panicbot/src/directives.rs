// SPDX-License-Identifier: MPL-2.0
//! Directives — per-repo configuration parser for `.machine_readable/bot_directives/panicbot.a2ml`.
//!
//! Each repository can customise panicbot's behaviour via a TOML-shaped file.
//! A2ML is retired and the deed has no per-bot directive clause yet (the
//! standards mapping only covers the `instructions-dir` pointer), so this
//! module only READS the legacy file; it never writes or extends one.
//!
//! ## What a repository gets
//!
//! - **No directive file: the full suite** — every class (a) panic-attack
//!   subcommand (see [`crate::suite`]), all severities, 300 s per command
//!   inside the suite budget (owner ruling 2026-11-13).
//! - **A directive file keeps exactly what it allows and denies.** Before the
//!   full-suite ruling a file with no `allow` key granted the static trio
//!   (`assail`, `adjudicate`, `diagnostics`); it still grants exactly that, so
//!   no repository is widened behind its back. `allow = [...]` replaces that
//!   set; `deny = [...]` removes from it, and a deny wins over an allow of the
//!   same name. Widening needs an explicit `allow`.
//! - **A file that exists but cannot be parsed is an error**, not a fallback to
//!   the (now full) default: failing open would widen a repository whose
//!   directive is broken. Callers must surface the error.
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
//! Names are panic-attack subcommands: `assail`, `assay`, `adjudicate`,
//! `diagnostics`, `attack`, `assault`, `ambush`, `amuck`, `abduct`, `axial`.
//! An unknown name is ignored with a note (it can only ever narrow, never
//! widen), and the note is printed.
//!
//! The SCM form was retired 2026-04-17; no fallback is supported.

use crate::config::{AllowedCommands, MinSeverity, PanicbotConfig, Subcommand};
use anyhow::{Context, Result};
use serde::Deserialize;
use std::path::Path;
use std::time::Duration;

/// Path to the bot directive file within a repository.
const DIRECTIVE_PATH: &str = ".machine_readable/bot_directives/panicbot.a2ml";

/// Load panicbot configuration from a repository's directive file.
///
/// If the file doesn't exist, returns the default config: the full suite, all
/// severities, 300 s per command. If the file exists but can't be read or
/// parsed, returns an error (never the default: see the module docs).
pub fn load_directives(repo_path: &Path) -> Result<PanicbotConfig> {
    let directive_file = repo_path.join(DIRECTIVE_PATH);

    if !directive_file.exists() {
        tracing::debug!(
            "No directive file at {}, using the full suite",
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
    deny: Option<Vec<String>>,
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
///
/// The content is a directive FILE, so the baseline is what a file granted
/// before the full-suite ruling (the static trio), not the no-file default.
fn parse_directive(content: &str) -> Result<PanicbotConfig> {
    let mut config = PanicbotConfig {
        allowed_commands: AllowedCommands::legacy_static(),
        ..PanicbotConfig::default()
    };
    let file: DirectiveFile = toml::from_str(content).context("Failed to parse TOML")?;

    // Allow list: an explicit allow REPLACES the baseline.
    if let Some(allowed) = &file.allow {
        config.allowed_commands = AllowedCommands::none();
        for name in allowed {
            match Subcommand::parse(name) {
                Some(cmd) => config.allowed_commands.set(cmd, true),
                None => config
                    .notes
                    .push(format!("directive allow: unknown command '{name}' ignored")),
            }
        }
    }

    // Deny list: removes from whatever is allowed; a deny wins.
    if let Some(denied) = &file.deny {
        for name in denied {
            match Subcommand::parse(name) {
                Some(cmd) => {
                    if file
                        .allow
                        .as_ref()
                        .is_some_and(|a| a.iter().any(|n| n == name))
                    {
                        config.notes.push(format!(
                            "directive lists '{name}' in both allow and deny: deny wins"
                        ));
                    }
                    config.allowed_commands.set(cmd, false);
                }
                None => config
                    .notes
                    .push(format!("directive deny: unknown command '{name}' ignored")),
            }
        }
    }

    // min-severity: prefer [config] block, fall back to top-level kebab key.
    let severity_str = file
        .config
        .as_ref()
        .and_then(|c| c.min_severity.clone())
        .or(file.min_severity_top);
    if let Some(s) = severity_str {
        if let Ok(sev) = s.parse::<MinSeverity>() {
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

#[cfg(test)]
mod suite_tests {
    use super::*;

    fn allowed_names(config: &PanicbotConfig) -> Vec<&'static str> {
        Subcommand::ALL
            .into_iter()
            .filter(|c| config.allowed_commands.allows(*c))
            .map(|c| c.name())
            .collect()
    }

    fn repo_with_directive(content: &str) -> tempfile::TempDir {
        let d = tempfile::tempdir().unwrap();
        let dir = d.path().join(".machine_readable/bot_directives");
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("panicbot.a2ml"), content).unwrap();
        d
    }

    #[test]
    fn no_directive_file_means_the_full_suite() {
        let d = tempfile::tempdir().unwrap();
        let config = load_directives(d.path()).unwrap();
        assert_eq!(config.allowed_commands, AllowedCommands::all());
        assert_eq!(allowed_names(&config).len(), Subcommand::ALL.len());
    }

    #[test]
    fn a_directive_file_that_matches_todays_denies_keeps_exactly_todays_scope() {
        // The directive shape the estate ships: static trio allowed, the six
        // dynamic modes denied. `assay` is new and is NOT in the file, so it
        // must stay off: the file's scope is not silently widened.
        let d = repo_with_directive(
            r#"
schema_version = "1.0"
bot = "panicbot"
scope = "static-analysis"
allow = ["assail", "adjudicate", "diagnostics"]
deny = ["attack", "assault", "ambush", "amuck", "abduct", "axial"]

[config]
min-severity = "low"
timeout-seconds = 300
"#,
        );
        let config = load_directives(d.path()).unwrap();
        assert_eq!(
            allowed_names(&config),
            ["assail", "adjudicate", "diagnostics"]
        );
        assert!(!config.allowed_commands.assay);
        assert!(config.notes.is_empty());
    }

    #[test]
    fn a_file_with_no_allow_key_keeps_the_static_trio_and_is_not_widened() {
        let d = repo_with_directive("[config]\nmin-severity = \"high\"\n");
        let config = load_directives(d.path()).unwrap();
        assert_eq!(
            allowed_names(&config),
            ["assail", "adjudicate", "diagnostics"]
        );
        assert_eq!(config.min_severity, MinSeverity::High);
    }

    #[test]
    fn deny_alone_narrows_the_baseline() {
        let config = parse_directive("deny = [\"adjudicate\"]\n").unwrap();
        assert_eq!(allowed_names(&config), ["assail", "diagnostics"]);
    }

    #[test]
    fn an_explicit_allow_can_widen_to_any_subcommand() {
        let config = parse_directive("allow = [\"assail\", \"assay\", \"assault\"]\n").unwrap();
        assert_eq!(allowed_names(&config), ["assail", "assay", "assault"]);
    }

    #[test]
    fn deny_can_remove_any_subcommand_from_an_allow() {
        for cmd in Subcommand::ALL {
            let content = format!(
                "allow = [{}]\ndeny = [\"{}\"]\n",
                Subcommand::ALL
                    .iter()
                    .map(|c| format!("\"{}\"", c.name()))
                    .collect::<Vec<_>>()
                    .join(", "),
                cmd.name()
            );
            let config = parse_directive(&content).unwrap();
            assert!(
                !config.allowed_commands.allows(cmd),
                "{} not denied",
                cmd.name()
            );
            for other in Subcommand::ALL.into_iter().filter(|o| *o != cmd) {
                assert!(
                    config.allowed_commands.allows(other),
                    "{} lost",
                    other.name()
                );
            }
        }
    }

    #[test]
    fn a_name_in_both_lists_is_denied_and_noted() {
        let config = parse_directive("allow = [\"assail\"]\ndeny = [\"assail\"]\n").unwrap();
        assert!(!config.allowed_commands.assail);
        assert!(
            config.notes.iter().any(|n| n.contains("deny wins")),
            "{:?}",
            config.notes
        );
    }

    #[test]
    fn unknown_names_are_ignored_loudly_and_never_widen() {
        let config =
            parse_directive("allow = [\"assail\", \"tui\"]\ndeny = [\"asail\"]\n").unwrap();
        assert_eq!(allowed_names(&config), ["assail"]);
        assert_eq!(config.notes.len(), 2, "{:?}", config.notes);
        assert!(config.notes.iter().any(|n| n.contains("'tui'")));
        assert!(config.notes.iter().any(|n| n.contains("'asail'")));
    }

    #[test]
    fn a_broken_directive_file_is_an_error_not_the_default() {
        // Falling back to the default would hand a repository whose directive
        // is broken the FULL suite.
        let d = repo_with_directive("allow = [ this is not toml\n");
        assert!(load_directives(d.path()).is_err());
    }
}
