// SPDX-License-Identifier: PMPL-1.0-or-later
//! Write-action guard — single place to call before any fs/GitHub write.
//!
//! Wraps `exclusion_registry::ExclusionRegistry` with a process-wide cache
//! and the "extract repo identity from disk" logic so each entry point in
//! `fixer.rs` / `github.rs` / `hooks.rs` is a one-line guard call.
//!
//! ## Usage
//!
//! ```ignore
//! use crate::registry_guard;
//! use crate::exclusion_registry::Action;
//!
//! pub fn commit(&self, ...) -> Result<()> {
//!     registry_guard::check_write(&self.repo_path, Action::Commit, None)?;
//!     // ... proceed with commit
//! }
//! ```
//!
//! ## Design notes
//!
//! - The registry is loaded once per process via `OnceLock` and never
//!   re-read. SIGHUP-driven reload is future work; for the short-lived
//!   CLI workflow that today's callers use, once-per-process is fine.
//! - If the registry file is missing, every write is denied via the
//!   fail-closed path in `ExclusionRegistry::load_from_env_or_conventional`.
//! - `check_write` returns `Result<()>` so callers can use `?`. A denial
//!   is mapped to `Error::Config` with a specific reason string.

use std::path::Path;
use std::sync::OnceLock;

use git2::Repository;
use tracing::{debug, warn};

use crate::error::{Error, Result};
use crate::exclusion_registry::{Action, ActionContext, Decision, ExclusionRegistry};

/// Process-wide cache of the loaded registry.
static REGISTRY: OnceLock<ExclusionRegistry> = OnceLock::new();

fn registry() -> &'static ExclusionRegistry {
    REGISTRY.get_or_init(ExclusionRegistry::load_from_env_or_conventional)
}

/// Guard a write action rooted at `repo_path`.
///
/// - `repo_path`  the on-disk path to the repo the action would affect.
/// - `action`     the action class being attempted.
/// - `target`     optional relative path inside the repo (for Write/Commit).
///
/// Returns `Ok(())` if allowed. Returns `Err(Error::Config(reason))` if
/// the registry denies, with the specific axis + reason embedded in the
/// error string so logs/check-runs can explain the denial.
pub fn check_write(repo_path: &Path, action: Action, target: Option<&str>) -> Result<()> {
    // Read-only actions: always allow.
    if !action.is_write() {
        return Ok(());
    }

    let (repo_full_name, origin) = repo_identity(repo_path);
    let ctx = ActionContext {
        repo_full_name: repo_full_name.as_deref().unwrap_or("<unknown>"),
        file_path: target,
        remote_origin: origin.as_deref(),
        action,
    };

    match registry().check(&ctx) {
        Decision::Allow => Ok(()),
        Decision::Deny { axis, reason } => {
            warn!(
                axis = ?axis,
                reason = %reason,
                repo = ?repo_full_name,
                origin = ?origin,
                action = ?action,
                target = ?target,
                "exclusion-registry DENIED write",
            );
            Err(Error::Config(format!(
                "exclusion-registry deny [{:?}]: {}",
                axis, reason
            )))
        }
    }
}

/// Guard a GitHub-API write (where the caller already knows the
/// full_name — no need to open the on-disk repo).
pub fn check_github_write(repo_full_name: &str, action: Action) -> Result<()> {
    if !action.is_write() {
        return Ok(());
    }
    let ctx = ActionContext {
        repo_full_name,
        file_path: None,
        remote_origin: None,
        action,
    };
    match registry().check(&ctx) {
        Decision::Allow => Ok(()),
        Decision::Deny { axis, reason } => {
            warn!(
                axis = ?axis,
                reason = %reason,
                repo = %repo_full_name,
                action = ?action,
                "exclusion-registry DENIED github-api write",
            );
            Err(Error::Config(format!(
                "exclusion-registry deny [{:?}] on {}: {}",
                axis, repo_full_name, reason
            )))
        }
    }
}

/// Read origin URL and parse owner/repo from a git repo on disk.
/// Returns `(None, None)` on failure — the registry then falls back to
/// path/origin-less check behaviour (external-repos axis can't match
/// without a name, but vendored-patterns still can on `target`).
fn repo_identity(repo_path: &Path) -> (Option<String>, Option<String>) {
    let Ok(repo) = Repository::open(repo_path) else {
        debug!(path = ?repo_path, "registry_guard: not a git repo — skipping identity lookup");
        return (None, None);
    };
    let origin_url = repo
        .find_remote("origin")
        .ok()
        .and_then(|r| r.url().map(|s| s.to_string()));
    let full_name = origin_url.as_deref().and_then(parse_full_name);
    (full_name, origin_url)
}

/// Parse `owner/repo` out of a common git URL shape.
///
///   git@github.com:owner/repo.git         → owner/repo
///   https://github.com/owner/repo.git     → owner/repo
///   ssh://git@github.com/owner/repo       → owner/repo
fn parse_full_name(url: &str) -> Option<String> {
    let s = url.strip_suffix(".git").unwrap_or(url);

    // git@host:owner/repo
    if let Some(rest) = s.strip_prefix("git@") {
        if let Some((_host, path)) = rest.split_once(':') {
            return Some(path.to_string());
        }
    }

    // https://host/owner/repo or ssh://host/owner/repo
    for prefix in ["https://", "http://", "ssh://", "git://"] {
        if let Some(rest) = s.strip_prefix(prefix) {
            // Skip the host segment.
            let mut parts = rest.splitn(2, '/');
            let _host = parts.next()?;
            let path = parts.next()?;
            return Some(path.to_string());
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_full_name_git_scp_form() {
        assert_eq!(
            parse_full_name("git@github.com:The-Metadatastician/007-lang.git").as_deref(),
            Some("The-Metadatastician/007-lang")
        );
    }

    #[test]
    fn parse_full_name_https_form() {
        assert_eq!(
            parse_full_name("https://github.com/hyperpolymath/standards.git").as_deref(),
            Some("hyperpolymath/standards")
        );
    }

    #[test]
    fn parse_full_name_without_dotgit() {
        assert_eq!(
            parse_full_name("git@github.com:JoshuaJewell/IDApTIK").as_deref(),
            Some("JoshuaJewell/IDApTIK")
        );
    }
}
