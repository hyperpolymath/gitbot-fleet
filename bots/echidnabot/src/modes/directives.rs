// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! Bot-directive resolution + cascade.
//!
//! Per the bit-4(c) decision: target-repo `.machine_readable/bot_directives/`
//! takes priority over the per-repo DB column, which takes priority over the
//! daemon-wide config default. The cascade is:
//!
//!   1. `.machine_readable/bot_directives/echidnabot.a2ml` (if present)
//!   2. `.machine_readable/bot_directives/all.a2ml`        (if present)
//!   3. `repositories.mode` column                         (per-repo DB)
//!   4. `BotMode::default()` = Verifier
//!
//! The directive content can be either A2ML (TOML-flavoured, post-2026-04-12
//! migration) or Scheme (legacy `.scm`). Both formats look for `(mode "X")`
//! or `mode = "X"` and accept the same four values.
//!
//! NOTE: this module does NOT fetch the directive from the target repo's
//! filesystem — that requires either cloning the repo locally (handled by the
//! sandboxed executor, currently empty) or hitting the platform API. Callers
//! are responsible for providing the directive content; if they pass `None`,
//! the cascade falls through to DB → default.

use crate::adapters::{PlatformAdapter, RepoId};
use crate::modes::BotMode;
use crate::store::models::Repository;

/// Canonical per-bot directive path, walked first in the cascade.
const DIRECTIVE_PATH_ECHIDNABOT: &str = ".machine_readable/bot_directives/echidnabot.a2ml";

/// Fleet-wide directive path, walked second.
const DIRECTIVE_PATH_ALL: &str = ".machine_readable/bot_directives/all.a2ml";

/// Fetch a directive from the target repo via the platform API. Walks
/// `echidnabot.a2ml` first, then `all.a2ml`. Returns `None` if neither
/// exists (the resolver then falls back to the DB column).
///
/// Errors from the underlying API are logged and treated as "no
/// directive" so a 502/rate-limit doesn't crash the webhook handler —
/// graceful degradation is the right shape for an advisory-only signal.
pub async fn fetch_directive_via_adapter(
    adapter: &dyn PlatformAdapter,
    repo: &RepoId,
    branch: Option<&str>,
) -> Option<String> {
    for path in [DIRECTIVE_PATH_ECHIDNABOT, DIRECTIVE_PATH_ALL] {
        match adapter.get_file_contents(repo, branch, path).await {
            Ok(Some(content)) => {
                tracing::debug!("Fetched directive from {}", path);
                return Some(content);
            }
            Ok(None) => continue,
            Err(e) => {
                tracing::warn!(
                    "Directive fetch failed for {} on {}/{}: {} — falling through cascade",
                    path,
                    repo.owner,
                    repo.name,
                    e
                );
            }
        }
    }
    None
}

/// Parse an A2ML/TOML directive looking for `[bot]` table with `mode = "X"`.
/// Returns `None` if no directive can be parsed.
pub fn parse_a2ml_directive(content: &str) -> Option<BotMode> {
    let parsed: toml::Value = toml::from_str(content).ok()?;
    let mode_str = parsed
        .get("bot")
        .and_then(|t| t.get("mode"))
        .and_then(|m| m.as_str())?;
    serde_json::from_value(serde_json::Value::String(mode_str.to_lowercase())).ok()
}

/// Resolve the bot mode for a repo + given directive content.
///
/// Cascade:
///   1. directive content (echidnabot.a2ml > all.a2ml > .scm fallback)
///   2. repo.mode (DB column, when non-default)
///   3. `BotMode::default()`
///
/// For the extended cascade that includes a daemon-wide default between
/// steps 2 and 3, use `resolve_mode_with_daemon_default`.
pub fn resolve_mode(repo: &Repository, directive_content: Option<&str>) -> BotMode {
    resolve_mode_with_daemon_default(repo, directive_content, BotMode::default())
}

/// Resolve the bot mode using the full four-level cascade.
///
/// Cascade:
///   1. directive content (echidnabot.a2ml > all.a2ml > .scm fallback)
///   2. repo.mode (DB column, when non-default)
///   3. `daemon_default` — daemon-wide `[bot] mode` from the TOML config
///   4. `BotMode::default()` (= Verifier)
///
/// `daemon_default` is the `ModeSelector.default_mode` stored on `AppState`.
/// Pass `BotMode::default()` to reproduce the pre-T3 three-level cascade.
///
/// Note: a repo registered with `--mode verifier` (the default) is
/// indistinguishable from "not explicitly set". The daemon-wide setting
/// therefore wins for repos that were never given an explicit mode. Repos
/// that need to stay at Verifier when the daemon default is something else
/// should set a per-repo directive file.
pub fn resolve_mode_with_daemon_default(
    repo: &Repository,
    directive_content: Option<&str>,
    daemon_default: BotMode,
) -> BotMode {
    if let Some(content) = directive_content {
        // Try A2ML first (post-2026-04-12 canonical), then legacy Scheme.
        if let Some(mode) = parse_a2ml_directive(content) {
            tracing::debug!("Mode {} resolved from A2ML directive", mode);
            return mode;
        }
        let scheme_mode = super::parse_mode_from_directive(content);
        // parse_mode_from_directive returns Verifier on parse failure. To
        // detect "no directive matched", check whether the content contained
        // anything mode-shaped at all.
        if content.to_lowercase().contains("mode") {
            tracing::debug!("Mode {} resolved from Scheme directive", scheme_mode);
            return scheme_mode;
        }
    }
    // Use the per-repo DB setting when it differs from the built-in default.
    if repo.mode != BotMode::default() {
        tracing::debug!("Mode {} resolved from repository.mode (DB)", repo.mode);
        return repo.mode;
    }
    // Fall back to the daemon-wide configured default.
    tracing::debug!(
        "Mode {} resolved from daemon-wide config default (repo.mode is default)",
        daemon_default
    );
    daemon_default
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::adapters::Platform;

    fn fixture_repo(mode: BotMode) -> Repository {
        let mut repo = Repository::new(Platform::GitHub, "owner".into(), "name".into());
        repo.mode = mode;
        repo
    }

    #[test]
    fn parses_a2ml_directive() {
        let content = r#"
            [bot]
            mode = "advisor"
        "#;
        assert_eq!(parse_a2ml_directive(content), Some(BotMode::Advisor));
    }

    #[test]
    fn parses_a2ml_each_mode() {
        for (s, expected) in [
            ("verifier", BotMode::Verifier),
            ("advisor", BotMode::Advisor),
            ("consultant", BotMode::Consultant),
            ("regulator", BotMode::Regulator),
        ] {
            let content = format!("[bot]\nmode = \"{}\"", s);
            assert_eq!(parse_a2ml_directive(&content), Some(expected));
        }
    }

    #[test]
    fn parses_a2ml_case_insensitive() {
        let content = r#"
            [bot]
            mode = "ADVISOR"
        "#;
        assert_eq!(parse_a2ml_directive(content), Some(BotMode::Advisor));
    }

    #[test]
    fn returns_none_for_invalid_a2ml() {
        // Not TOML at all
        assert_eq!(parse_a2ml_directive("not even close"), None);
    }

    #[test]
    fn returns_none_for_a2ml_without_bot_mode() {
        let content = r#"
            [other]
            field = "value"
        "#;
        assert_eq!(parse_a2ml_directive(content), None);
    }

    #[test]
    fn cascade_directive_a2ml_wins_over_db() {
        let repo = fixture_repo(BotMode::Verifier);
        let directive = r#"
            [bot]
            mode = "regulator"
        "#;
        assert_eq!(resolve_mode(&repo, Some(directive)), BotMode::Regulator);
    }

    #[test]
    fn cascade_directive_scheme_wins_over_db() {
        let repo = fixture_repo(BotMode::Verifier);
        let directive = r#"(echidnabot (mode "advisor"))"#;
        assert_eq!(resolve_mode(&repo, Some(directive)), BotMode::Advisor);
    }

    #[test]
    fn cascade_falls_back_to_db_when_no_directive() {
        let repo = fixture_repo(BotMode::Consultant);
        assert_eq!(resolve_mode(&repo, None), BotMode::Consultant);
    }

    #[test]
    fn cascade_falls_back_to_db_when_directive_has_no_mode() {
        let repo = fixture_repo(BotMode::Advisor);
        let directive = "(echidnabot (provers \"lean\" \"coq\"))"; // no mode
                                                                   // Scheme parser returns Verifier on no-match, but our resolver's
                                                                   // "contains 'mode'" check makes us NOT trust that fallback. So we
                                                                   // fall through to DB.
        assert_eq!(resolve_mode(&repo, Some(directive)), BotMode::Advisor);
    }

    #[test]
    fn cascade_default_is_verifier_when_db_mode_default() {
        // Repository::new sets mode to BotMode::default() (Verifier).
        let repo = Repository::new(Platform::GitHub, "owner".into(), "name".into());
        assert_eq!(resolve_mode(&repo, None), BotMode::Verifier);
    }

    // ─── resolve_mode_with_daemon_default tests ──────────────────────────

    #[test]
    fn daemon_default_wins_over_built_in_default() {
        // Repo has no explicit mode (defaults to Verifier). Daemon says Advisor.
        let repo = Repository::new(Platform::GitHub, "owner".into(), "name".into());
        assert_eq!(
            resolve_mode_with_daemon_default(&repo, None, BotMode::Advisor),
            BotMode::Advisor,
        );
    }

    #[test]
    fn explicit_db_mode_wins_over_daemon_default() {
        // Repo explicitly set to Regulator → wins over Advisor daemon default.
        let repo = fixture_repo(BotMode::Regulator);
        assert_eq!(
            resolve_mode_with_daemon_default(&repo, None, BotMode::Advisor),
            BotMode::Regulator,
        );
    }

    #[test]
    fn directive_wins_over_daemon_default() {
        let repo = Repository::new(Platform::GitHub, "owner".into(), "name".into());
        let directive = r#"[bot]
mode = "consultant"
"#;
        assert_eq!(
            resolve_mode_with_daemon_default(&repo, Some(directive), BotMode::Regulator),
            BotMode::Consultant,
        );
    }

    #[test]
    fn daemon_default_verifier_leaves_default_unchanged() {
        let repo = Repository::new(Platform::GitHub, "owner".into(), "name".into());
        assert_eq!(
            resolve_mode_with_daemon_default(&repo, None, BotMode::Verifier),
            BotMode::Verifier,
        );
    }
}
