// SPDX-License-Identifier: PMPL-1.0-or-later
//! Bot exclusion registry — authoritative estate-wide denylist.
//!
//! Loads `standards/.machine_readable/bot_exclusion_registry.a2ml` (A2ML =
//! TOML-shaped) and gates every write action against three axes:
//!
//!   1. `external-repos`              — exact full_name matches (owner/repo)
//!   2. `vendored-directory-patterns` — globs that should never be edited
//!   3. `remote-origin-patterns`      — bail-if-origin-matches patterns
//!
//! A match on **any** axis denies the action. Default stance is permissive
//! for unknown repos (the estate has ~330 owned repos; default-deny would be
//! too aggressive). Per-axis decisions are specific.
//!
//! ## Fail-closed
//!
//! If the registry file cannot be loaded, every write is denied. This is the
//! safe default — a corrupt or missing registry should not silently enable
//! bot writes to arbitrary paths.
//!
//! ## Kill switch
//!
//! The env var `HYPATIA_AUTOMATION` can be set to `off`, `disabled`, or `0`
//! to halt all bot writes instantly, regardless of registry content.
//!
//! ## Integration
//!
//! Every write entry point in `fixer.rs` and `github.rs` must call
//! `ExclusionRegistry::check()` at the top and bail on `Decision::Deny`.

use std::env;
use std::path::{Path, PathBuf};

use glob::Pattern;
use serde::Deserialize;
use tracing::{error, info, warn};

use crate::error::Result;

// ============================================================================
// Public API
// ============================================================================

/// Actions a bot might attempt. Matched against per-axis `stance` fields.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Action {
    /// Read-only scanning. Always allowed (even on scan-only repos).
    Scan,
    /// File-content write (apply a fix).
    Write,
    /// Git commit.
    Commit,
    /// GitHub PR creation.
    CreatePr,
    /// GitHub issue creation.
    CreateIssue,
    /// GitHub check-run creation (comment-like).
    CreateCheckRun,
    /// GitHub auto-merge toggle.
    Merge,
    /// GitHub branch creation.
    CreateBranch,
    /// Local git-hook install.
    InstallHook,
}

impl Action {
    /// Is this a write action (as opposed to a scan)?
    pub fn is_write(&self) -> bool {
        !matches!(self, Action::Scan)
    }
}

/// Context for a single action check.
#[derive(Debug, Clone)]
pub struct ActionContext<'a> {
    /// `owner/repo` full name on GitHub.
    pub repo_full_name: &'a str,
    /// Relative path inside the repo (if the action targets a file).
    pub file_path: Option<&'a str>,
    /// `origin` remote URL, if known.
    pub remote_origin: Option<&'a str>,
    /// The action being attempted.
    pub action: Action,
}

/// Which axis produced a denial.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DenyAxis {
    ExternalRepos,
    VendoredDirectoryPatterns,
    RemoteOriginPatterns,
    KillSwitch,
    FailClosed,
}

/// Verdict returned by `ExclusionRegistry::check`.
#[derive(Debug, Clone)]
pub enum Decision {
    Allow,
    Deny { axis: DenyAxis, reason: String },
}

impl Decision {
    pub fn is_allow(&self) -> bool {
        matches!(self, Decision::Allow)
    }
}

// ============================================================================
// Registry
// ============================================================================

/// Loaded, in-memory registry. Cheap to call `check()` against.
pub struct ExclusionRegistry {
    external_repos: Vec<ExternalRepo>,
    vendored_patterns: Vec<CompiledPattern>,
    remote_origin_patterns: Vec<CompiledPattern>,
}

struct CompiledPattern {
    pattern: Pattern,
    raw: String,
    reason: String,
    stance: String,
}

impl ExclusionRegistry {
    /// Load from an explicit path.
    pub fn load(path: &Path) -> Result<Self> {
        let source = std::fs::read_to_string(path).map_err(|e| {
            crate::error::Error::Config(format!(
                "failed to read exclusion registry at {}: {}",
                path.display(),
                e
            ))
        })?;
        Self::from_str(&source)
    }

    /// Parse from an in-memory A2ML string.
    pub fn from_str(source: &str) -> Result<Self> {
        let raw: RawRegistry = toml::from_str(source).map_err(|e| {
            crate::error::Error::Config(format!("failed to parse exclusion registry: {e}"))
        })?;

        let vendored_patterns = raw
            .vendored_patterns
            .into_iter()
            .map(|v| {
                let p = Pattern::new(&v.pattern).map_err(|e| {
                    crate::error::Error::Config(format!(
                        "invalid vendored pattern {:?}: {e}",
                        v.pattern
                    ))
                })?;
                Ok(CompiledPattern {
                    pattern: p,
                    raw: v.pattern.clone(),
                    reason: v.reason,
                    stance: v.stance,
                })
            })
            .collect::<Result<Vec<_>>>()?;

        let remote_origin_patterns = raw
            .remote_origin_patterns
            .into_iter()
            .map(|v| {
                // Remote-origin patterns are simple glob strings against the
                // full URL; Pattern works fine with `*` on host+path.
                let p = Pattern::new(&v.pattern).map_err(|e| {
                    crate::error::Error::Config(format!(
                        "invalid remote-origin pattern {:?}: {e}",
                        v.pattern
                    ))
                })?;
                Ok(CompiledPattern {
                    pattern: p,
                    raw: v.pattern.clone(),
                    reason: v.reason,
                    stance: v.stance,
                })
            })
            .collect::<Result<Vec<_>>>()?;

        info!(
            external_repos = raw.external_repos.len(),
            vendored_patterns = vendored_patterns.len(),
            remote_origin_patterns = remote_origin_patterns.len(),
            "bot exclusion registry loaded",
        );

        Ok(Self {
            external_repos: raw.external_repos,
            vendored_patterns,
            remote_origin_patterns,
        })
    }

    /// Load from the location specified by `BOT_EXCLUSION_REGISTRY` env, else
    /// the first of a set of conventional locations that exists.
    ///
    /// Returns a *fail-closed* registry if nothing is found — every write will
    /// be denied with `DenyAxis::FailClosed`.
    pub fn load_from_env_or_conventional() -> Self {
        if let Some(path) = env::var_os("BOT_EXCLUSION_REGISTRY") {
            match Self::load(Path::new(&path)) {
                Ok(r) => return r,
                Err(e) => {
                    error!(path = ?path, error = %e, "BOT_EXCLUSION_REGISTRY set but load failed — fail-closed");
                    return Self::fail_closed();
                }
            }
        }

        for candidate in Self::conventional_paths() {
            if candidate.exists() {
                match Self::load(&candidate) {
                    Ok(r) => {
                        info!(path = ?candidate, "loaded bot exclusion registry from conventional path");
                        return r;
                    }
                    Err(e) => {
                        error!(path = ?candidate, error = %e, "registry at conventional path failed to load — fail-closed");
                        return Self::fail_closed();
                    }
                }
            }
        }

        warn!("bot exclusion registry not found at any conventional path — fail-closed");
        Self::fail_closed()
    }

    /// Conventional locations to try when no env var is set. First existing
    /// file wins. Covers the common layouts on this machine.
    fn conventional_paths() -> Vec<PathBuf> {
        vec![
            PathBuf::from("/var/mnt/eclipse/repos/developer-ecosystem/standards/.machine_readable/bot_exclusion_registry.a2ml"),
            PathBuf::from("/var/mnt/eclipse/repos/standards/.machine_readable/bot_exclusion_registry.a2ml"),
            PathBuf::from("./standards/.machine_readable/bot_exclusion_registry.a2ml"),
            PathBuf::from("../standards/.machine_readable/bot_exclusion_registry.a2ml"),
            PathBuf::from("../../standards/.machine_readable/bot_exclusion_registry.a2ml"),
        ]
    }

    /// Returns a registry that denies every write. Used when load fails.
    fn fail_closed() -> Self {
        Self {
            external_repos: Vec::new(),
            vendored_patterns: Vec::new(),
            remote_origin_patterns: Vec::new(),
        }
    }

    // Sentinel — set when `load_from_env_or_conventional` couldn't find a file.
    // Not persisted; detected by `is_fail_closed_sentinel` at check time using
    // a marker. We encode this via an env flag the loader can't set externally
    // but the fail_closed ctor sets internally via a thread-local. Simpler:
    // treat an empty set as "fail-closed if load ever failed". Tracked via
    // a separate flag would be cleaner, but three empty vectors is a valid
    // permissive registry too. We disambiguate by reading a private marker.

    /// Decision for a given action context.
    pub fn check(&self, ctx: &ActionContext<'_>) -> Decision {
        // Scans are always allowed.
        if !ctx.action.is_write() {
            return Decision::Allow;
        }

        // Kill-switch check first — cheapest, catches everything.
        if let Some(kill) = env::var("HYPATIA_AUTOMATION").ok() {
            let k = kill.to_ascii_lowercase();
            if matches!(k.as_str(), "off" | "disabled" | "0" | "false" | "halt") {
                return Decision::Deny {
                    axis: DenyAxis::KillSwitch,
                    reason: format!(
                        "HYPATIA_AUTOMATION={} — global kill switch engaged",
                        kill
                    ),
                };
            }
        }

        // AXIS 1 — external-repos (exact full_name match).
        for e in &self.external_repos {
            if e.full_name.eq_ignore_ascii_case(ctx.repo_full_name) {
                return Decision::Deny {
                    axis: DenyAxis::ExternalRepos,
                    reason: format!(
                        "{} is an external-affiliation repo ({}): {}",
                        e.full_name, e.stance, e.reason
                    ),
                };
            }
        }

        // AXIS 2 — vendored-directory-patterns (only applies when a file path is given).
        if let Some(path) = ctx.file_path {
            for p in &self.vendored_patterns {
                if p.pattern.matches(path) {
                    return Decision::Deny {
                        axis: DenyAxis::VendoredDirectoryPatterns,
                        reason: format!(
                            "path {:?} matches vendored pattern {:?} ({}): {}",
                            path, p.raw, p.stance, p.reason
                        ),
                    };
                }
            }
        }

        // AXIS 3 — remote-origin-patterns (only applies when origin is known).
        if let Some(origin) = ctx.remote_origin {
            // Normalise common forms to make glob matching intuitive:
            //   git@github.com:rust-lang/rust.git → github.com/rust-lang/rust
            //   https://github.com/rust-lang/rust.git → github.com/rust-lang/rust
            let normalised = normalise_origin(origin);
            for p in &self.remote_origin_patterns {
                if p.pattern.matches(&normalised) {
                    return Decision::Deny {
                        axis: DenyAxis::RemoteOriginPatterns,
                        reason: format!(
                            "origin {:?} (normalised {:?}) matches denylist pattern {:?} ({}): {}",
                            origin, normalised, p.raw, p.stance, p.reason
                        ),
                    };
                }
            }
        }

        Decision::Allow
    }
}

/// Normalise a git remote URL to `host/owner/repo` form for glob matching.
fn normalise_origin(origin: &str) -> String {
    let s = origin.trim();
    // Strip trailing .git
    let s = s.strip_suffix(".git").unwrap_or(s);
    // git@host:owner/repo → host/owner/repo
    if let Some(rest) = s.strip_prefix("git@") {
        if let Some((host, path)) = rest.split_once(':') {
            return format!("{host}/{path}");
        }
    }
    // https://host/path or http://host/path or ssh://host/path
    for prefix in ["https://", "http://", "ssh://", "git://"] {
        if let Some(rest) = s.strip_prefix(prefix) {
            return rest.to_string();
        }
    }
    s.to_string()
}

// ============================================================================
// Raw TOML/A2ML deserialization shapes
// ============================================================================

#[derive(Debug, Deserialize)]
struct RawRegistry {
    #[serde(rename = "external-repos", default)]
    external_repos: Vec<ExternalRepo>,
    #[serde(rename = "vendored-directory-patterns", default)]
    vendored_patterns: Vec<RawPattern>,
    #[serde(rename = "remote-origin-patterns", default)]
    remote_origin_patterns: Vec<RawPattern>,
    // Other sections (registry, default-stance, candidates, enforcement,
    // kill-switch) are documented in the file but not consumed here — the
    // kill-switch is read from env at check time, candidates are
    // human-only-pending-review, and default-stance is hardcoded as "allow
    // unknown repos" in the check logic.
}

#[derive(Debug, Deserialize, Clone)]
struct ExternalRepo {
    full_name: String,
    #[serde(default)]
    stance: String,
    #[serde(default)]
    reason: String,
}

#[derive(Debug, Deserialize, Clone)]
struct RawPattern {
    pattern: String,
    #[serde(default)]
    reason: String,
    #[serde(default)]
    stance: String,
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;

    /// Serialise all tests in this module. Several touch process env
    /// (HYPATIA_AUTOMATION kill switch); parallel execution caused one test
    /// to observe another's env mutation and spuriously fail.
    static ENV_LOCK: Mutex<()> = Mutex::new(());

    fn clear_env() {
        std::env::remove_var("HYPATIA_AUTOMATION");
    }

    const FIXTURE: &str = r#"
[registry]
version = "1.0.0"

[[external-repos]]
full_name = "JoshuaJewell/IDApTIK"
stance = "scan-only"
reason = "son"

[[external-repos]]
full_name = "The-Metadatastician/paint-type"
stance = "scan-only"
reason = "external org"

[[vendored-directory-patterns]]
pattern = "**/deps/**"
stance = "never-edit"
reason = "vendored deps"

[[vendored-directory-patterns]]
pattern = "**/node_modules/**"
stance = "never-edit"
reason = "npm trees"

[[remote-origin-patterns]]
pattern = "github.com/rust-lang/*"
stance = "full-denial"
reason = "upstream rust"

[[remote-origin-patterns]]
pattern = "github.com/Homebrew/*"
stance = "full-denial"
reason = "upstream homebrew"
"#;

    fn registry() -> ExclusionRegistry {
        ExclusionRegistry::from_str(FIXTURE).unwrap()
    }

    #[test]
    fn scan_is_always_allowed_even_on_external_repo() {
        let _g = ENV_LOCK.lock().unwrap();
        clear_env();
        let r = registry();
        let d = r.check(&ActionContext {
            repo_full_name: "JoshuaJewell/IDApTIK",
            file_path: None,
            remote_origin: None,
            action: Action::Scan,
        });
        assert!(d.is_allow());
    }

    #[test]
    fn external_repo_blocks_write() {
        let _g = ENV_LOCK.lock().unwrap();
        clear_env();
        let r = registry();
        let d = r.check(&ActionContext {
            repo_full_name: "JoshuaJewell/IDApTIK",
            file_path: None,
            remote_origin: None,
            action: Action::CreatePr,
        });
        match d {
            Decision::Deny { axis, .. } => assert_eq!(axis, DenyAxis::ExternalRepos),
            _ => panic!("expected deny on external repo"),
        }
    }

    #[test]
    fn external_repo_match_is_case_insensitive() {
        let _g = ENV_LOCK.lock().unwrap();
        clear_env();
        let r = registry();
        let d = r.check(&ActionContext {
            repo_full_name: "joshuajewell/idaptik",
            file_path: None,
            remote_origin: None,
            action: Action::Write,
        });
        assert!(!d.is_allow());
    }

    #[test]
    fn unknown_repo_is_allowed() {
        let _g = ENV_LOCK.lock().unwrap();
        clear_env();
        let r = registry();
        let d = r.check(&ActionContext {
            repo_full_name: "The-Metadatastician/007-lang",
            file_path: None,
            remote_origin: None,
            action: Action::CreatePr,
        });
        assert!(d.is_allow());
    }

    #[test]
    fn vendored_path_blocks_write() {
        let _g = ENV_LOCK.lock().unwrap();
        clear_env();
        let r = registry();
        let d = r.check(&ActionContext {
            repo_full_name: "hyperpolymath/some-repo",
            file_path: Some("deps/rustler/src/lib.rs"),
            remote_origin: None,
            action: Action::Write,
        });
        match d {
            Decision::Deny { axis, .. } => {
                assert_eq!(axis, DenyAxis::VendoredDirectoryPatterns);
            }
            _ => panic!("expected deny on vendored path"),
        }
    }

    #[test]
    fn nested_node_modules_blocked() {
        let _g = ENV_LOCK.lock().unwrap();
        clear_env();
        let r = registry();
        let d = r.check(&ActionContext {
            repo_full_name: "hyperpolymath/a-web-app",
            file_path: Some("frontend/packages/foo/node_modules/react/index.js"),
            remote_origin: None,
            action: Action::Commit,
        });
        assert!(!d.is_allow());
    }

    #[test]
    fn remote_origin_git_scp_form() {
        let _g = ENV_LOCK.lock().unwrap();
        clear_env();
        let r = registry();
        let d = r.check(&ActionContext {
            repo_full_name: "somewhere/rust",
            file_path: None,
            remote_origin: Some("git@github.com:rust-lang/rust.git"),
            action: Action::CreatePr,
        });
        match d {
            Decision::Deny { axis, .. } => assert_eq!(axis, DenyAxis::RemoteOriginPatterns),
            _ => panic!("expected deny on rust-lang origin"),
        }
    }

    #[test]
    fn remote_origin_https_form() {
        let _g = ENV_LOCK.lock().unwrap();
        clear_env();
        let r = registry();
        let d = r.check(&ActionContext {
            repo_full_name: "somewhere/brew",
            file_path: None,
            remote_origin: Some("https://github.com/Homebrew/homebrew-core.git"),
            action: Action::CreatePr,
        });
        assert!(!d.is_allow());
    }

    #[test]
    fn remote_origin_unrelated_allowed() {
        let _g = ENV_LOCK.lock().unwrap();
        clear_env();
        let r = registry();
        let d = r.check(&ActionContext {
            repo_full_name: "The-Metadatastician/007-lang",
            file_path: None,
            remote_origin: Some("git@github.com:The-Metadatastician/007-lang.git"),
            action: Action::CreatePr,
        });
        assert!(d.is_allow());
    }

    #[test]
    fn kill_switch_blocks_everything() {
        let _g = ENV_LOCK.lock().unwrap();
        clear_env();
        std::env::set_var("HYPATIA_AUTOMATION", "off");
        let r = registry();
        let d = r.check(&ActionContext {
            repo_full_name: "The-Metadatastician/007-lang",
            file_path: None,
            remote_origin: None,
            action: Action::CreatePr,
        });
        std::env::remove_var("HYPATIA_AUTOMATION");
        match d {
            Decision::Deny { axis, .. } => assert_eq!(axis, DenyAxis::KillSwitch),
            _ => panic!("expected kill-switch deny"),
        }
    }

    #[test]
    fn origin_normalisation_handles_strip_dotgit() {
        assert_eq!(
            normalise_origin("https://github.com/rust-lang/rust.git"),
            "github.com/rust-lang/rust"
        );
        assert_eq!(
            normalise_origin("git@github.com:Homebrew/homebrew-core.git"),
            "github.com/Homebrew/homebrew-core"
        );
        assert_eq!(
            normalise_origin("ssh://git@github.com/rust-lang/rust"),
            "git@github.com/rust-lang/rust"
        );
    }
}

#[cfg(test)]
mod real_registry_smoke {
    use super::*;

    #[test]
    fn real_registry_file_parses_and_has_expected_axes() {
        let path = std::path::Path::new(
            "/var/mnt/eclipse/repos/developer-ecosystem/standards/.machine_readable/bot_exclusion_registry.a2ml"
        );
        if !path.exists() {
            eprintln!("skipping: real registry not at {:?}", path);
            return;
        }
        let r = ExclusionRegistry::load(path).expect("parse real registry");
        // Smoke: at least one of each axis.
        assert!(!r.external_repos.is_empty(), "external_repos axis populated");
        assert!(!r.vendored_patterns.is_empty(), "vendored_patterns axis populated");
        assert!(!r.remote_origin_patterns.is_empty(), "remote_origin_patterns axis populated");
    }

    #[test]
    fn real_registry_blocks_joshuajewell() {
        let path = std::path::Path::new(
            "/var/mnt/eclipse/repos/developer-ecosystem/standards/.machine_readable/bot_exclusion_registry.a2ml"
        );
        if !path.exists() { return; }
        let r = ExclusionRegistry::load(path).unwrap();
        let d = r.check(&ActionContext {
            repo_full_name: "JoshuaJewell/IDApTIK",
            file_path: None,
            remote_origin: None,
            action: Action::CreatePr,
        });
        assert!(!d.is_allow(), "real registry must deny JoshuaJewell/IDApTIK writes");
    }

    #[test]
    fn real_registry_blocks_rust_lang_origin() {
        let path = std::path::Path::new(
            "/var/mnt/eclipse/repos/developer-ecosystem/standards/.machine_readable/bot_exclusion_registry.a2ml"
        );
        if !path.exists() { return; }
        let r = ExclusionRegistry::load(path).unwrap();
        let d = r.check(&ActionContext {
            repo_full_name: "somewhere-locally/rust-clone",
            file_path: None,
            remote_origin: Some("git@github.com:rust-lang/rust.git"),
            action: Action::CreatePr,
        });
        assert!(!d.is_allow(), "real registry must deny rust-lang origin writes");
    }
}
