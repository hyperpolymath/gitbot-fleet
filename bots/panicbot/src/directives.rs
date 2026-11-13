// SPDX-License-Identifier: MPL-2.0
//! Directives — per-repo configuration for panicbot.
//!
//! ## Where directives come from (owner rulings 2026-11-13)
//!
//! 1. **The repo deed**, `<repo>_chora.deed` at the scanned directory's root:
//!    the `(bot :name panicbot ...)` entry inside `(bot-directives ...)`. Canonical.
//! 2. **The legacy file** `.machine_readable/bot_directives/panicbot.a2ml`
//!    (TOML-shaped), READ ONLY, used only when the deed has no panicbot entry.
//!    A2ML is retired: this module never writes or extends one.
//! 3. **Neither: the full suite**, every class (a) subcommand (see
//!    [`crate::suite`]), all severities, 300 s per command, no target.
//!
//! If the deed has a panicbot entry it wins and the legacy file is ignored
//! (with a note). A deed or legacy file that exists but cannot be read or
//! parsed is an **error**, never a fallback to the (full) default: failing open
//! would widen a repository whose directive is broken. Callers must surface it.
//!
//! ## Deed clause
//!
//! ```text
//! (bot-directives
//!   (bot :name panicbot
//!     :allow (assail assay diagnostics)   ; optional; absent = full suite
//!     :deny (assault ambush)              ; optional; deny wins over allow
//!     (target :build "cargo build --release" :program "target/release/foo")
//!     (config :min-severity low :timeout-seconds 300)))
//! ```
//!
//! The bot is named with `:name`, the one spelling (standards #1502); the bare
//! form `(bot panicbot ...)` is an error that says so. Every part is optional except that a `target` needs `:program`.
//! Unknown fields, unknown subcommand names, a wrong value type, a duplicate
//! entry and an ambiguous deed (more than one `*_chora.deed` at the root) are
//! errors: the deed reader is strict where the legacy file only notes.
//!
//! - **Deny-only means the full suite minus the deny.** An explicit `allow`
//!   narrows to exactly what it names; a deny always wins.
//! - **`target`** declares the program for the dynamic modes (assault, ambush,
//!   then adjudicate). `:build`, if present, is run with `sh -c` in the scanned
//!   directory before them; `:program` is what is passed as the target. The
//!   CLI `--target` replaces the declared target, build included. See
//!   [`crate::suite`] for the checks and the privileged-trigger refusal.
//! - **`config`**: `:min-severity` (`low`, `medium`, `high`, `critical`) and
//!   `:timeout-seconds` (a positive integer).
//!
//! ## Legacy file format (read-only)
//!
//! ```toml
//! bot = "panicbot"
//! allow = ["assail", "adjudicate", "diagnostics"]
//! deny = ["attack", "assault", "ambush", "amuck", "abduct", "axial"]
//!
//! [config]
//! min-severity = "low"
//! timeout-seconds = 300
//! ```
//!
//! The same allow/deny rules apply (a file with no `allow` is the full suite
//! minus its `deny`). Unknown names are ignored with a note, which can only
//! narrow. The legacy file has no `target`: only the deed declares one.
//!
//! Names are panic-attack subcommands: `assail`, `assay`, `adjudicate`,
//! `diagnostics`, `attack`, `assault`, `ambush`, `amuck`, `abduct`, `axial`.

use crate::config::{AllowedCommands, MinSeverity, PanicbotConfig, Subcommand, TargetDecl};
use crate::deed::{self, Clause, Item};
use anyhow::{bail, Context, Result};
use serde::Deserialize;
use std::path::Path;
use std::time::Duration;

/// Path to the legacy bot directive file within a repository.
const DIRECTIVE_PATH: &str = ".machine_readable/bot_directives/panicbot.a2ml";

/// Load panicbot configuration for a repository: the deed clause, else the
/// legacy file, else the full suite (see the module docs).
pub fn load_directives(repo_path: &Path) -> Result<PanicbotConfig> {
    let legacy_file = repo_path.join(DIRECTIVE_PATH);
    let legacy_present = legacy_file.exists();

    if let Some(deed_path) = find_deed(repo_path)? {
        let text = std::fs::read_to_string(&deed_path)
            .with_context(|| format!("Failed to read {}", deed_path.display()))?;
        let root = deed::parse(&text)
            .with_context(|| format!("Failed to parse {}", deed_path.display()))?;
        if let Some(mut config) = from_deed(&root).with_context(|| {
            format!(
                "Invalid (bot :name panicbot) clause in {}",
                deed_path.display()
            )
        })? {
            if legacy_present {
                config.notes.push(format!(
                    "{DIRECTIVE_PATH} exists but is ignored: the deed's (bot :name panicbot) clause wins"
                ));
            }
            return Ok(config);
        }
        tracing::debug!("{} has no (bot :name panicbot) entry", deed_path.display());
    }

    if !legacy_present {
        tracing::debug!(
            "No directives for {}, using the full suite",
            repo_path.display()
        );
        return Ok(PanicbotConfig::default());
    }

    let content = std::fs::read_to_string(&legacy_file)
        .with_context(|| format!("Failed to read {}", legacy_file.display()))?;
    parse_directive(&content).with_context(|| format!("Failed to parse {}", legacy_file.display()))
}

/// The one `*_chora.deed` at the repo root, if any. More than one is an error.
fn find_deed(repo_path: &Path) -> Result<Option<std::path::PathBuf>> {
    let entries = match std::fs::read_dir(repo_path) {
        Ok(e) => e,
        // A scan target that is not a directory has no deed.
        Err(_) => return Ok(None),
    };
    let mut found: Vec<std::path::PathBuf> = Vec::new();
    for entry in entries {
        let entry = entry.with_context(|| format!("Failed to list {}", repo_path.display()))?;
        let name = entry.file_name();
        let name = name.to_string_lossy();
        if name.ends_with("_chora.deed") && entry.path().is_file() {
            found.push(entry.path());
        }
    }
    found.sort();
    match found.len() {
        0 => Ok(None),
        1 => Ok(found.pop()),
        _ => bail!(
            "more than one *_chora.deed at the repository root ({}); cannot tell which is the repo deed",
            found
                .iter()
                .filter_map(|p| p.file_name())
                .map(|n| n.to_string_lossy().into_owned())
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

fn shape(line: usize, message: impl Into<String>) -> anyhow::Error {
    anyhow::Error::new(deed::DeedError {
        line,
        message: message.into(),
    })
}

/// Read the `(bot :name panicbot ...)` entry of a parsed deed. `None`: the deed has
/// no such entry (not an error). Anything wrong inside an entry is an error.
fn from_deed(root: &Clause) -> Result<Option<PanicbotConfig>> {
    let mut sections = root.clauses("bot-directives");
    let Some(section) = sections.next() else {
        return Ok(None);
    };
    if let Some(second) = sections.next() {
        return Err(shape(second.line, "more than one (bot-directives) clause"));
    }

    let mut mine: Option<&Clause> = None;
    for bot in section.clauses("bot") {
        if bot_name(bot)? == "panicbot" {
            if let Some(first) = mine {
                return Err(shape(
                    bot.line,
                    format!(
                        "(bot :name panicbot) appears twice (first at line {})",
                        first.line
                    ),
                ));
            }
            mine = Some(bot);
        }
    }
    let Some(bot) = mine else {
        return Ok(None);
    };

    let mut config = PanicbotConfig::default();
    let mut allow: Option<Vec<Subcommand>> = None;
    let mut deny: Vec<Subcommand> = Vec::new();
    let mut seen: Vec<&str> = Vec::new();
    let mut target_seen = false;
    let mut config_seen = false;

    for item in &bot.items {
        match item {
            Item::Field { key, value, line } => {
                if seen.contains(&key.as_str()) {
                    return Err(shape(*line, format!("duplicate :{key}")));
                }
                match key.as_str() {
                    "name" => {
                        value.as_symbol(":name", *line)?;
                    }
                    "allow" => allow = Some(subcommands(":allow", value, *line)?),
                    "deny" => deny = subcommands(":deny", value, *line)?,
                    other => {
                        return Err(shape(
                            *line,
                            format!("unknown field :{other} in (bot :name panicbot)"),
                        ))
                    }
                }
                seen.push(key);
            }
            Item::Clause(c) => match c.head.as_str() {
                "target" => {
                    if std::mem::replace(&mut target_seen, true) {
                        return Err(shape(c.line, "more than one (target) clause"));
                    }
                    config.target = Some(read_target(c)?);
                }
                "config" => {
                    if std::mem::replace(&mut config_seen, true) {
                        return Err(shape(c.line, "more than one (config) clause"));
                    }
                    read_config(c, &mut config)?;
                }
                other => {
                    return Err(shape(
                        c.line,
                        format!("unknown clause ({other} ...) in (bot :name panicbot)"),
                    ))
                }
            },
        }
    }

    // Absent allow = the full suite; an allow narrows to what it names.
    config.allowed_commands = match &allow {
        None => AllowedCommands::all(),
        Some(names) => {
            let mut a = AllowedCommands::none();
            for c in names {
                a.set(*c, true);
            }
            a
        }
    };
    for c in &deny {
        if allow.as_ref().is_some_and(|a| a.contains(c)) {
            config.notes.push(format!(
                "directive lists '{}' in both allow and deny: deny wins",
                c.name()
            ));
        }
        config.allowed_commands.set(*c, false);
    }
    Ok(Some(config))
}

/// The bot's name: its `:name SYM` field.
fn bot_name(bot: &Clause) -> Result<&str> {
    let mut name: Option<&str> = None;
    for item in &bot.items {
        let (n, line) = match item {
            Item::Field {
                key,
                value: deed::Value::Sym(s),
                line,
            } if key == "name" => (s.as_str(), *line),
            _ => continue,
        };
        if name.is_some() {
            return Err(shape(line, "a (bot) clause names one bot"));
        }
        name = Some(n);
    }
    name.ok_or_else(|| {
        shape(
            bot.line,
            "a (bot) clause needs a name: (bot :name NAME ...)",
        )
    })
}

fn subcommands(what: &str, value: &deed::Value, line: usize) -> Result<Vec<Subcommand>> {
    value
        .as_symbol_list(what, line)?
        .into_iter()
        .map(|n| {
            Subcommand::parse(n).ok_or_else(|| {
                shape(
                    line,
                    format!("{what}: unknown subcommand '{n}' (expected one of {})", {
                        Subcommand::ALL
                            .iter()
                            .map(|c| c.name())
                            .collect::<Vec<_>>()
                            .join(", ")
                    }),
                )
            })
        })
        .collect()
}

fn read_target(c: &Clause) -> Result<TargetDecl> {
    let (mut build, mut program) = (None, None);
    for item in &c.items {
        match item {
            Item::Field { key, value, line } => match key.as_str() {
                "build" => {
                    if build.is_some() {
                        return Err(shape(*line, "duplicate :build"));
                    }
                    let s = value.as_string(":build", *line)?;
                    if s.trim().is_empty() {
                        return Err(shape(*line, ":build is empty"));
                    }
                    build = Some(s.to_string());
                }
                "program" => {
                    if program.is_some() {
                        return Err(shape(*line, "duplicate :program"));
                    }
                    let s = value.as_string(":program", *line)?;
                    if s.trim().is_empty() {
                        return Err(shape(*line, ":program is empty"));
                    }
                    program = Some(s.to_string());
                }
                other => return Err(shape(*line, format!("unknown field :{other} in (target)"))),
            },
            Item::Clause(n) => {
                return Err(shape(
                    n.line,
                    format!("unexpected clause ({}) in (target)", n.head),
                ))
            }
        }
    }
    let program =
        program.ok_or_else(|| shape(c.line, "(target) needs :program, the built executable"))?;
    Ok(TargetDecl { build, program })
}

fn read_config(c: &Clause, config: &mut PanicbotConfig) -> Result<()> {
    let mut seen: Vec<&str> = Vec::new();
    for item in &c.items {
        let Item::Field { key, value, line } = item else {
            return Err(shape(c.line, "(config) takes only :fields"));
        };
        if seen.contains(&key.as_str()) {
            return Err(shape(*line, format!("duplicate :{key}")));
        }
        seen.push(key);
        match key.as_str() {
            "min-severity" => {
                let s = value.as_symbol(":min-severity", *line)?;
                config.min_severity = s.parse::<MinSeverity>().map_err(|_| {
                    shape(
                        *line,
                        format!("unknown :min-severity '{s}' (low, medium, high, critical)"),
                    )
                })?;
            }
            "timeout-seconds" => match value {
                deed::Value::Int(n) if *n > 0 => config.timeout = Duration::from_secs(*n as u64),
                _ => return Err(shape(*line, ":timeout-seconds must be a positive integer")),
            },
            other => return Err(shape(*line, format!("unknown field :{other} in (config)"))),
        }
    }
    Ok(())
}

/// Raw legacy A2ML (TOML) shape.
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

/// Parse a legacy panicbot directive from its A2ML/TOML content.
///
/// With no `allow` the baseline is the full suite (owner ruling 2026-11-13:
/// a deny-only directive means everything minus the deny).
fn parse_directive(content: &str) -> Result<PanicbotConfig> {
    let mut config = PanicbotConfig::default();
    let file: DirectiveFile = toml::from_str(content).context("Failed to parse TOML")?;

    // Allow list: an explicit allow REPLACES the full-suite baseline.
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
        // Empty is the full suite (TOML parses empty as an empty struct).
        assert_eq!(config.allowed_commands, AllowedCommands::all());
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
    fn a_file_with_no_allow_key_is_the_full_suite_per_the_2026_11_13_ruling() {
        let d = repo_with_directive("[config]\nmin-severity = \"high\"\n");
        let config = load_directives(d.path()).unwrap();
        assert_eq!(config.allowed_commands, AllowedCommands::all());
        assert_eq!(config.min_severity, MinSeverity::High);
    }

    #[test]
    fn deny_alone_is_the_full_suite_minus_the_deny() {
        let config = parse_directive("deny = [\"adjudicate\"]\n").unwrap();
        let expect: Vec<&str> = Subcommand::ALL
            .into_iter()
            .map(|c| c.name())
            .filter(|n| *n != "adjudicate")
            .collect();
        assert_eq!(allowed_names(&config), expect);
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

#[cfg(test)]
mod deed_tests {
    use super::*;

    fn names(config: &PanicbotConfig) -> Vec<&'static str> {
        Subcommand::ALL
            .into_iter()
            .filter(|c| config.allowed_commands.allows(*c))
            .map(|c| c.name())
            .collect()
    }

    fn deed_for(clause: &str) -> String {
        format!(
            ";; SPDX-License-Identifier: MPL-2.0\n(repo-deed\n  :schema-version \"1.0.0\"\n  :canonical-name \"x\"\n  (bot-directives\n    (methodology :mode hybrid)\n    {clause}))\n"
        )
    }

    fn repo(deed: Option<&str>, legacy: Option<&str>) -> tempfile::TempDir {
        let d = tempfile::tempdir().unwrap();
        if let Some(t) = deed {
            std::fs::write(d.path().join("x_chora.deed"), t).unwrap();
        }
        if let Some(t) = legacy {
            let dir = d.path().join(".machine_readable/bot_directives");
            std::fs::create_dir_all(&dir).unwrap();
            std::fs::write(dir.join("panicbot.a2ml"), t).unwrap();
        }
        d
    }

    #[test]
    fn deny_only_in_the_deed_is_the_full_suite_minus_the_deny() {
        let d = repo(
            Some(&deed_for("(bot :name panicbot :deny (assault ambush))")),
            None,
        );
        let c = load_directives(d.path()).unwrap();
        let expect: Vec<&str> = Subcommand::ALL
            .into_iter()
            .map(|c| c.name())
            .filter(|n| *n != "assault" && *n != "ambush")
            .collect();
        assert_eq!(names(&c), expect);
        assert!(c.target.is_none());
    }

    #[test]
    fn an_explicit_allow_narrows_and_deny_wins_over_it() {
        let d = repo(
            Some(&deed_for(
                "(bot :name panicbot :allow (assail assay diagnostics) :deny (assay))",
            )),
            None,
        );
        let c = load_directives(d.path()).unwrap();
        assert_eq!(names(&c), ["assail", "diagnostics"]);
        assert!(c.notes.iter().any(|n| n.contains("deny wins")));
    }

    #[test]
    fn the_full_clause_reads_target_and_config() {
        let d = repo(
            Some(&deed_for(
                "(bot :name panicbot\n      :allow (assail assault)\n      (target :build \"cargo build --release\" :program \"target/release/foo\")\n      (config :min-severity high :timeout-seconds 60))",
            )),
            None,
        );
        let c = load_directives(d.path()).unwrap();
        assert_eq!(names(&c), ["assail", "assault"]);
        assert_eq!(
            c.target,
            Some(TargetDecl {
                build: Some("cargo build --release".into()),
                program: "target/release/foo".into()
            })
        );
        assert_eq!(c.min_severity, MinSeverity::High);
        assert_eq!(c.timeout, Duration::from_secs(60));
    }

    #[test]
    fn a_target_without_a_build_is_allowed() {
        let d = repo(
            Some(&deed_for(
                "(bot :name panicbot (target :program \"bin/x\"))",
            )),
            None,
        );
        let c = load_directives(d.path()).unwrap();
        assert_eq!(c.target.unwrap().build, None);
    }

    #[test]
    fn the_deed_clause_wins_over_the_legacy_file() {
        let d = repo(
            Some(&deed_for("(bot :name panicbot :deny (assay))")),
            Some("allow = [\"assail\"]\n"),
        );
        let c = load_directives(d.path()).unwrap();
        assert!(c.allowed_commands.assail && c.allowed_commands.assault);
        assert!(!c.allowed_commands.assay);
        assert!(
            c.notes.iter().any(|n| n.contains("ignored")),
            "{:?}",
            c.notes
        );
    }

    #[test]
    fn the_legacy_file_is_the_fallback_when_the_deed_has_no_panicbot_entry() {
        for deed in [
            "(repo-deed :schema-version \"1.0.0\")\n".to_string(),
            deed_for("(bot :name hypatia :deny (assay))"),
            deed_for("(bot :name rhodibot)"),
        ] {
            let d = repo(Some(&deed), Some("allow = [\"assail\"]\n"));
            let c = load_directives(d.path()).unwrap();
            assert_eq!(names(&c), ["assail"], "{deed}");
        }
    }

    #[test]
    fn neither_deed_nor_legacy_is_the_full_suite_with_no_target() {
        let d = repo(None, None);
        let c = load_directives(d.path()).unwrap();
        assert_eq!(c.allowed_commands, AllowedCommands::all());
        assert!(c.target.is_none());
        // A deed without the clause behaves the same.
        let d = repo(Some("(repo-deed :schema-version \"1.0.0\")\n"), None);
        assert_eq!(
            load_directives(d.path()).unwrap().allowed_commands,
            AllowedCommands::all()
        );
    }

    #[test]
    fn a_malformed_deed_or_clause_is_an_error_never_a_default() {
        let legacy = Some("allow = [\"assail\"]\n");
        let cases: Vec<(&str, String)> = vec![
            (
                "unbalanced deed",
                "(repo-deed :schema-version \"1.0.0\"\n".into(),
            ),
            (
                "unknown subcommand",
                deed_for("(bot :name panicbot :deny (asault))"),
            ),
            ("allow not a list", deed_for("(bot :name panicbot :allow assail)")),
            (
                "allow of strings",
                deed_for("(bot :name panicbot :allow (\"assail\"))"),
            ),
            ("unknown field", deed_for("(bot :name panicbot :dney (assault))")),
            (
                "unknown clause",
                deed_for("(bot :name panicbot (targt :program \"x\"))"),
            ),
            (
                "target without program",
                deed_for("(bot :name panicbot (target :build \"make\"))"),
            ),
            (
                "build not a string",
                deed_for("(bot :name panicbot (target :build make :program \"x\"))"),
            ),
            (
                "empty build",
                deed_for("(bot :name panicbot (target :build \"\" :program \"x\"))"),
            ),
            (
                "two targets",
                deed_for("(bot :name panicbot (target :program \"a\") (target :program \"b\"))"),
            ),
            (
                "bad severity",
                deed_for("(bot :name panicbot (config :min-severity severe))"),
            ),
            (
                "bad timeout",
                deed_for("(bot :name panicbot (config :timeout-seconds 0))"),
            ),
            (
                "string timeout",
                deed_for("(bot :name panicbot (config :timeout-seconds \"5\"))"),
            ),
            (
                "duplicate deny",
                deed_for("(bot :name panicbot :deny (assay) :deny (assault))"),
            ),
            (
                "two panicbot entries",
                deed_for("(bot :name panicbot) (bot :name panicbot)"),
            ),
            ("nameless bot", deed_for("(bot :deny (assay))")),
            (
                "two bot-directives",
                format!(
                    "{}{}",
                    "(repo-deed (bot-directives (bot :name panicbot)) (bot-directives (bot :name panicbot)))",
                    "\n"
                ),
            ),
        ];
        for (label, text) in cases {
            let d = repo(Some(&text), legacy);
            assert!(
                load_directives(d.path()).is_err(),
                "{label} must be an error"
            );
        }
    }

    #[test]
    fn the_bare_bot_form_is_an_error_naming_the_canonical_spelling() {
        for clause in ["(bot panicbot)", "(bot panicbot :deny (assay))"] {
            let d = repo(Some(&deed_for(clause)), Some("allow = [\"assail\"]\n"));
            let e = format!("{:#}", load_directives(d.path()).unwrap_err());
            assert!(e.contains("(bot :name panicbot ...)"), "{clause}: {e}");
        }
        // Another bot written the bare way breaks the deed too: one spelling.
        let d = repo(Some(&deed_for("(bot hypatia)")), None);
        assert!(load_directives(d.path()).is_err());
    }

    #[test]
    fn two_deeds_at_the_root_are_an_error() {
        let d = repo(Some(&deed_for("(bot :name panicbot)")), None);
        std::fs::write(
            d.path().join("y_chora.deed"),
            deed_for("(bot :name panicbot)"),
        )
        .unwrap();
        let e = load_directives(d.path()).unwrap_err();
        assert!(format!("{e:#}").contains("more than one"), "{e:#}");
    }

    #[test]
    fn the_estate_template_deed_parses_with_this_reader() {
        // Guards the reader against rejecting a real, valid deed.
        let p = std::path::Path::new("/home/user/rsr-template-repo/rsr-template-repo_chora.deed");
        if let Ok(text) = std::fs::read_to_string(p) {
            deed::parse(&text).expect("the template deed must read");
        }
    }
}
