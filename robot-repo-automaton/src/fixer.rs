// SPDX-License-Identifier: MPL-2.0
//! Fix application - delete, modify, create, disable.

use regex::Regex;
use std::fs;
use std::path::{Component, Path, PathBuf};

use crate::catalog::{Fix, FixAction};
use crate::detector::DetectedIssue;
use crate::error::{Error, Result};

/// Result of applying a single fix.
#[derive(Debug, Clone)]
pub struct FixResult {
    pub success: bool,
    pub files_modified: Vec<PathBuf>,
    pub action_taken: String,
    pub error: Option<String>,
}

/// Applies fixes to a repository on disk.
pub struct Fixer {
    repo_path: PathBuf,
    dry_run: bool,
}

impl Fixer {
    /// Create a new fixer for the given repository.
    pub fn new(repo_path: PathBuf, dry_run: bool) -> Self {
        Self { repo_path, dry_run }
    }

    /// Resolve a fix target relative to the repository root, rejecting any
    /// path that would escape the repository. The target need not exist
    /// (e.g. for `Create`), so this is pure path arithmetic.
    fn resolve_target(&self, target: &str) -> Result<PathBuf> {
        let joined = self.repo_path.join(target);

        let mut normalized = PathBuf::new();
        for component in joined.components() {
            match component {
                Component::ParentDir => {
                    if !normalized.pop() {
                        return Err(Error::Fix(format!(
                            "Fix target '{target}' resolves outside the repository"
                        )));
                    }
                }
                Component::CurDir => {}
                other => normalized.push(other),
            }
        }

        if !normalized.starts_with(&self.repo_path) {
            return Err(Error::Fix(format!(
                "Fix target '{target}' resolves outside the repository"
            )));
        }

        Ok(normalized)
    }

    /// Apply a single fix, returning the outcome. A rejected or failed fix
    /// is reported via `FixResult`, not `Err` (the operation itself did not
    /// error; the requested change simply could not be made safely).
    pub fn apply(&self, _issue: &DetectedIssue, fix: &Fix) -> Result<FixResult> {
        let target = match self.resolve_target(&fix.target) {
            Ok(path) => path,
            Err(e) => {
                return Ok(FixResult {
                    success: false,
                    files_modified: Vec::new(),
                    action_taken: "rejected".to_string(),
                    error: Some(e.to_string()),
                });
            }
        };

        let result = match fix.action {
            FixAction::Delete => self.apply_delete(&target),
            FixAction::Modify => self.apply_modify(&target, fix),
            FixAction::Create => self.apply_create(&target, fix),
            FixAction::Disable => FixResult {
                success: true,
                files_modified: Vec::new(),
                action_taken: "Disable: no-op, manual review required".to_string(),
                error: None,
            },
        };

        Ok(result)
    }

    fn apply_delete(&self, target: &Path) -> FixResult {
        if !target.exists() {
            return FixResult {
                success: true,
                files_modified: Vec::new(),
                action_taken: format!("Delete: {} already absent", target.display()),
                error: None,
            };
        }

        if self.dry_run {
            return FixResult {
                success: true,
                files_modified: Vec::new(),
                action_taken: format!("DRY RUN: would delete {}", target.display()),
                error: None,
            };
        }

        match fs::remove_file(target) {
            Ok(()) => FixResult {
                success: true,
                files_modified: vec![target.to_path_buf()],
                action_taken: format!("Deleted {}", target.display()),
                error: None,
            },
            Err(e) => FixResult {
                success: false,
                files_modified: Vec::new(),
                action_taken: "Delete: failed".to_string(),
                error: Some(e.to_string()),
            },
        }
    }

    fn apply_create(&self, target: &Path, fix: &Fix) -> FixResult {
        if target.exists() {
            return FixResult {
                success: true,
                files_modified: Vec::new(),
                action_taken: format!("Create: {} already exists", target.display()),
                error: None,
            };
        }

        if self.dry_run {
            return FixResult {
                success: true,
                files_modified: Vec::new(),
                action_taken: format!("DRY RUN: would create {}", target.display()),
                error: None,
            };
        }

        if let Some(parent) = target.parent() {
            if let Err(e) = fs::create_dir_all(parent) {
                return FixResult {
                    success: false,
                    files_modified: Vec::new(),
                    action_taken: "Create: failed".to_string(),
                    error: Some(e.to_string()),
                };
            }
        }

        let content = fix.fallback.clone().unwrap_or_default();
        match fs::write(target, content) {
            Ok(()) => FixResult {
                success: true,
                files_modified: vec![target.to_path_buf()],
                action_taken: format!("Created {}", target.display()),
                error: None,
            },
            Err(e) => FixResult {
                success: false,
                files_modified: Vec::new(),
                action_taken: "Create: failed".to_string(),
                error: Some(e.to_string()),
            },
        }
    }

    fn apply_modify(&self, target: &Path, fix: &Fix) -> FixResult {
        let Some(modification) = fix.modification.as_deref() else {
            return FixResult {
                success: false,
                files_modified: Vec::new(),
                action_taken: "Modify: failed".to_string(),
                error: Some("Modify fix has no modification instruction".to_string()),
            };
        };

        let original = match fs::read(target) {
            Ok(bytes) => bytes,
            Err(e) => {
                return FixResult {
                    success: false,
                    files_modified: Vec::new(),
                    action_taken: "Modify: failed".to_string(),
                    error: Some(e.to_string()),
                };
            }
        };

        let original_text = match String::from_utf8(original) {
            Ok(text) => text,
            Err(_) => {
                return FixResult {
                    success: false,
                    files_modified: Vec::new(),
                    action_taken: "Modify: failed".to_string(),
                    error: Some(format!(
                        "Refusing to modify binary file {}",
                        target.display()
                    )),
                };
            }
        };

        let new_text = match Self::apply_modification(&original_text, modification) {
            Ok(text) => text,
            Err(e) => {
                return FixResult {
                    success: false,
                    files_modified: Vec::new(),
                    action_taken: "Modify: failed".to_string(),
                    error: Some(e),
                };
            }
        };

        if self.dry_run {
            return FixResult {
                success: true,
                files_modified: Vec::new(),
                action_taken: format!("DRY RUN: would modify {}", target.display()),
                error: None,
            };
        }

        if new_text == original_text {
            return FixResult {
                success: true,
                files_modified: Vec::new(),
                action_taken: format!("Modify: {} already up to date", target.display()),
                error: None,
            };
        }

        match fs::write(target, new_text) {
            Ok(()) => FixResult {
                success: true,
                files_modified: vec![target.to_path_buf()],
                action_taken: format!("Modified {}", target.display()),
                error: None,
            },
            Err(e) => FixResult {
                success: false,
                files_modified: Vec::new(),
                action_taken: "Modify: failed".to_string(),
                error: Some(e.to_string()),
            },
        }
    }

    /// Apply a `replace-line:`, `replace-pattern:`, `insert-before:` or
    /// `insert-after:` modification instruction to `content`.
    fn apply_modification(
        content: &str,
        modification: &str,
    ) -> std::result::Result<String, String> {
        if let Some(rest) = modification.strip_prefix("replace-pattern:") {
            let (pattern, replacement) = rest
                .split_once(':')
                .ok_or_else(|| "Invalid replace-pattern instruction".to_string())?;
            let re = Regex::new(pattern).map_err(|e| format!("Invalid regex: {e}"))?;
            return Ok(re.replace_all(content, replacement).into_owned());
        }

        let mut lines: Vec<String> = content.lines().map(str::to_string).collect();
        let trailing_newline = content.ends_with('\n');

        if let Some(rest) = modification.strip_prefix("replace-line:") {
            let (line_no, replacement) = rest
                .split_once(':')
                .ok_or_else(|| "Invalid replace-line instruction".to_string())?;
            let line_no: usize = line_no
                .parse()
                .map_err(|_| "Invalid line number in replace-line instruction".to_string())?;
            if line_no == 0 || line_no > lines.len() {
                return Err(format!(
                    "replace-line: line {line_no} does not exist (file has {} lines)",
                    lines.len()
                ));
            }
            lines[line_no - 1] = replacement.to_string();
        } else if let Some(rest) = modification.strip_prefix("insert-before:") {
            let (line_no, text) = rest
                .split_once(':')
                .ok_or_else(|| "Invalid insert-before instruction".to_string())?;
            let line_no: usize = line_no
                .parse()
                .map_err(|_| "Invalid line number in insert-before instruction".to_string())?;
            if line_no == 0 || line_no > lines.len() + 1 {
                return Err(format!(
                    "insert-before: line {line_no} does not exist (file has {} lines)",
                    lines.len()
                ));
            }
            lines.insert(line_no - 1, text.to_string());
        } else if let Some(rest) = modification.strip_prefix("insert-after:") {
            let (line_no, text) = rest
                .split_once(':')
                .ok_or_else(|| "Invalid insert-after instruction".to_string())?;
            let line_no: usize = line_no
                .parse()
                .map_err(|_| "Invalid line number in insert-after instruction".to_string())?;
            if line_no == 0 || line_no > lines.len() {
                return Err(format!(
                    "insert-after: line {line_no} does not exist (file has {} lines)",
                    lines.len()
                ));
            }
            lines.insert(line_no, text.to_string());
        } else {
            return Err(format!("Unknown modification instruction: {modification}"));
        }

        let mut result = lines.join("\n");
        if trailing_newline {
            result.push('\n');
        }
        Ok(result)
    }

    /// Apply a batch of auto-approved fixes and commit the results locally.
    pub fn apply_and_commit(
        &self,
        _issues: &[DetectedIssue],
        auto_fixes: &[(DetectedIssue, Fix)],
    ) -> Result<Vec<FixResult>> {
        let mut results = Vec::with_capacity(auto_fixes.len());
        let mut modified: Vec<PathBuf> = Vec::new();
        let mut messages: Vec<String> = Vec::new();

        for (issue, fix) in auto_fixes {
            let result = self.apply(issue, fix)?;
            if result.success && !result.files_modified.is_empty() {
                modified.extend(result.files_modified.clone());
                messages.push(issue.commit_message.clone());
            }
            results.push(result);
        }

        if !self.dry_run && !modified.is_empty() {
            self.commit_changes(&modified, &messages)?;
        }

        Ok(results)
    }

    /// Stage and commit the given files in the local repository.
    fn commit_changes(&self, files: &[PathBuf], messages: &[String]) -> Result<()> {
        let repo = git2::Repository::open(&self.repo_path)?;
        let mut index = repo.index()?;

        for file in files {
            let relative = file.strip_prefix(&self.repo_path).unwrap_or(file);
            if file.exists() {
                index.add_path(relative)?;
            } else {
                let _ = index.remove_path(relative);
            }
        }
        index.write()?;

        let tree_id = index.write_tree()?;
        let tree = repo.find_tree(tree_id)?;
        let signature = git2::Signature::now("robot-repo-automaton", "noreply@hyperpolymath.dev")?;

        let message = if messages.is_empty() {
            "fix: automated compliance fixes".to_string()
        } else {
            messages.join("\n")
        };

        let parent_commit = repo.head().ok().and_then(|h| h.peel_to_commit().ok());
        let parents: Vec<&git2::Commit> = parent_commit.iter().collect();

        repo.commit(
            Some("HEAD"),
            &signature,
            &signature,
            &message,
            &tree,
            &parents,
        )?;

        Ok(())
    }
}
