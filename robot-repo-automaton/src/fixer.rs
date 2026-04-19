// SPDX-License-Identifier: PMPL-1.0-or-later
//! Fix application for detected issues
//!
//! Provides functionality to apply automated fixes to repositories:
//! - **Delete**: Remove files that should not exist
//! - **Modify**: Apply line-level transformations with safety checks and rollback
//! - **Create**: Create missing files from templates with variable expansion
//! - **Disable**: Rename files to .disabled extension

use git2::{Repository, Signature};
use regex::Regex;
use std::path::{Path, PathBuf};
use tracing::{debug, info, warn};

use crate::catalog::{Fix, FixAction};
use crate::detector::DetectedIssue;
use crate::error::{Error, Result};

/// Result of applying a fix
#[derive(Debug)]
pub struct FixResult {
    /// The issue ID that was addressed
    pub issue_id: String,
    /// Whether the fix was successfully applied
    pub success: bool,
    /// Human-readable description of the action taken
    pub action_taken: String,
    /// Files that were modified by this fix
    pub files_modified: Vec<PathBuf>,
    /// Error message if the fix failed
    pub error: Option<String>,
}

/// Specification for a line-level modification
#[derive(Debug, Clone)]
pub enum ModifySpec {
    /// Replace entire line content at a specific line number (1-indexed)
    ReplaceLine { line: usize, content: String },
    /// Insert content before a specific line number (1-indexed)
    InsertBefore { line: usize, content: String },
    /// Insert content after a specific line number (1-indexed)
    InsertAfter { line: usize, content: String },
    /// Replace all occurrences of a regex pattern with a replacement string
    ReplacePattern { pattern: String, replacement: String },
    /// Prepend content to the beginning of the file
    Prepend { content: String },
    /// Append content to the end of the file
    Append { content: String },
}

/// Repository fixer that applies automated corrections
pub struct Fixer {
    /// Root path of the repository being fixed
    repo_path: PathBuf,
    /// When true, no actual changes are made (only logged)
    dry_run: bool,
}

/// Known binary file extensions that should never be modified
const BINARY_EXTENSIONS: &[&str] = &[
    "png", "jpg", "jpeg", "gif", "bmp", "ico", "webp", "svg",
    "pdf", "doc", "docx", "xls", "xlsx", "ppt", "pptx",
    "zip", "tar", "gz", "bz2", "xz", "7z", "rar",
    "exe", "dll", "so", "dylib", "o", "a",
    "wasm", "pyc", "class",
    "ttf", "otf", "woff", "woff2", "eot",
    "mp3", "mp4", "avi", "mkv", "flac", "ogg", "wav",
    "db", "sqlite", "sqlite3",
];

impl Fixer {
    /// Create a new fixer for a repository
    pub fn new(repo_path: PathBuf, dry_run: bool) -> Self {
        Fixer { repo_path, dry_run }
    }

    /// Apply a fix for a detected issue
    pub fn apply(&self, issue: &DetectedIssue, fix: &Fix) -> Result<FixResult> {
        // EXCLUSION REGISTRY GUARD: refuse the write if the target repo,
        // origin, or target path is on the estate-wide denylist. In dry-run
        // mode we still check so operators can preview denials without
        // surprises. The guard returns Err on denial; map it to a
        // FixResult::failure so one denied fix does not abort a batch.
        if let Err(e) = crate::registry_guard::check_write(
            &self.repo_path,
            crate::exclusion_registry::Action::Write,
            Some(&fix.target),
        ) {
            warn!(target = %fix.target, error = %e, "registry guard denied fix");
            return Ok(FixResult {
                issue_id: issue.error_type_id.clone(),
                success: false,
                action_taken: format!("DENIED by bot_exclusion_registry: {e}"),
                files_modified: vec![],
                error: Some(e.to_string()),
            });
        }

        let target_path = self.repo_path.join(&fix.target);

        // SECURITY: Reject any target path that escapes the repository root.
        // Normalise both paths and verify the target is a child of repo_path.
        // This prevents path traversal attacks (e.g. target = "../../etc/passwd").
        let canonical_repo = self.repo_path.canonicalize().unwrap_or_else(|_| self.repo_path.clone());
        // For the target we normalise without requiring the path to exist yet
        // (it may be a to-be-created file), so we use a manual component walk.
        let normalised_target = normalise_path(&target_path);
        if !normalised_target.starts_with(&canonical_repo) {
            warn!(
                target = %fix.target,
                repo = %self.repo_path.display(),
                "SECURITY: fix target escapes repository root — rejecting"
            );
            return Ok(FixResult {
                issue_id: issue.error_type_id.clone(),
                success: false,
                action_taken: format!(
                    "REJECTED: target '{}' escapes repository boundary",
                    fix.target
                ),
                files_modified: vec![],
                error: Some(format!(
                    "Security violation: target path '{}' is outside the repository directory",
                    fix.target
                )),
            });
        }

        match fix.action {
            FixAction::Delete => self.apply_delete(&target_path, issue),
            FixAction::Modify => self.apply_modify(&target_path, issue, fix),
            FixAction::Create => self.apply_create(&target_path, issue, fix),
            FixAction::Disable => self.apply_disable(&target_path, issue),
        }
    }

    /// Check if a file is binary based on its extension
    fn is_binary(path: &Path) -> bool {
        path.extension()
            .and_then(|ext| ext.to_str())
            .map(|ext| BINARY_EXTENSIONS.contains(&ext.to_lowercase().as_str()))
            .unwrap_or(false)
    }

    /// Parse a modification specification string into structured operations
    ///
    /// Supported formats:
    /// - `replace-line:<N>:<content>` - Replace line N with content
    /// - `insert-before:<N>:<content>` - Insert content before line N
    /// - `insert-after:<N>:<content>` - Insert content after line N
    /// - `replace-pattern:<regex>:<replacement>` - Replace regex matches
    /// - `prepend:<content>` - Add content at file beginning
    /// - `append:<content>` - Add content at file end
    fn parse_modification(spec: &str) -> Result<ModifySpec> {
        let parts: Vec<&str> = spec.splitn(3, ':').collect();

        match parts.first().copied() {
            Some("replace-line") => {
                if parts.len() < 3 {
                    return Err(Error::Fix("replace-line requires line number and content".into()));
                }
                let line: usize = parts[1]
                    .parse()
                    .map_err(|_| Error::Fix(format!("Invalid line number: {}", parts[1])))?;
                Ok(ModifySpec::ReplaceLine {
                    line,
                    content: parts[2].to_string(),
                })
            }
            Some("insert-before") => {
                if parts.len() < 3 {
                    return Err(Error::Fix("insert-before requires line number and content".into()));
                }
                let line: usize = parts[1]
                    .parse()
                    .map_err(|_| Error::Fix(format!("Invalid line number: {}", parts[1])))?;
                Ok(ModifySpec::InsertBefore {
                    line,
                    content: parts[2].to_string(),
                })
            }
            Some("insert-after") => {
                if parts.len() < 3 {
                    return Err(Error::Fix("insert-after requires line number and content".into()));
                }
                let line: usize = parts[1]
                    .parse()
                    .map_err(|_| Error::Fix(format!("Invalid line number: {}", parts[1])))?;
                Ok(ModifySpec::InsertAfter {
                    line,
                    content: parts[2].to_string(),
                })
            }
            Some("replace-pattern") => {
                if parts.len() < 3 {
                    return Err(Error::Fix("replace-pattern requires pattern and replacement".into()));
                }
                Ok(ModifySpec::ReplacePattern {
                    pattern: parts[1].to_string(),
                    replacement: parts[2].to_string(),
                })
            }
            Some("prepend") => {
                if parts.len() < 2 {
                    return Err(Error::Fix("prepend requires content".into()));
                }
                // Rejoin parts[1..] in case content contained ':'
                let content = parts[1..].join(":");
                Ok(ModifySpec::Prepend { content })
            }
            Some("append") => {
                if parts.len() < 2 {
                    return Err(Error::Fix("append requires content".into()));
                }
                let content = parts[1..].join(":");
                Ok(ModifySpec::Append { content })
            }
            _ => Err(Error::Fix(format!("Unknown modification type: {}", spec))),
        }
    }

    /// Apply a modification specification to file content
    fn apply_modification(content: &str, spec: &ModifySpec) -> Result<String> {
        let mut lines: Vec<String> = content.lines().map(|l| l.to_string()).collect();

        match spec {
            ModifySpec::ReplaceLine { line, content: new_content } => {
                if *line == 0 || *line > lines.len() {
                    return Err(Error::Fix(format!(
                        "Line {} out of range (file has {} lines)",
                        line,
                        lines.len()
                    )));
                }
                lines[*line - 1] = new_content.clone();
            }
            ModifySpec::InsertBefore { line, content: new_content } => {
                if *line == 0 || *line > lines.len() + 1 {
                    return Err(Error::Fix(format!(
                        "Line {} out of range for insertion (file has {} lines)",
                        line,
                        lines.len()
                    )));
                }
                lines.insert(*line - 1, new_content.clone());
            }
            ModifySpec::InsertAfter { line, content: new_content } => {
                if *line == 0 || *line > lines.len() {
                    return Err(Error::Fix(format!(
                        "Line {} out of range for insertion (file has {} lines)",
                        line,
                        lines.len()
                    )));
                }
                lines.insert(*line, new_content.clone());
            }
            ModifySpec::ReplacePattern { pattern, replacement } => {
                let re = Regex::new(pattern)
                    .map_err(|e| Error::Fix(format!("Invalid regex pattern '{}': {}", pattern, e)))?;
                let result = re.replace_all(content, replacement.as_str());
                return Ok(result.into_owned());
            }
            ModifySpec::Prepend { content: new_content } => {
                lines.insert(0, new_content.clone());
            }
            ModifySpec::Append { content: new_content } => {
                lines.push(new_content.clone());
            }
        }

        // Preserve trailing newline if original had one
        let mut result = lines.join("\n");
        if content.ends_with('\n') {
            result.push('\n');
        }
        Ok(result)
    }

    /// Delete a file
    fn apply_delete(
        &self,
        target_path: &Path,
        issue: &DetectedIssue,
    ) -> Result<FixResult> {
        if !target_path.exists() {
            return Ok(FixResult {
                issue_id: issue.error_type_id.clone(),
                success: true,
                action_taken: "File already deleted".to_string(),
                files_modified: vec![],
                error: None,
            });
        }

        if self.dry_run {
            info!("[DRY RUN] Would delete: {}", target_path.display());
            return Ok(FixResult {
                issue_id: issue.error_type_id.clone(),
                success: true,
                action_taken: format!("[DRY RUN] Would delete {}", target_path.display()),
                files_modified: vec![target_path.to_path_buf()],
                error: None,
            });
        }

        std::fs::remove_file(target_path)?;
        info!("Deleted: {}", target_path.display());

        Ok(FixResult {
            issue_id: issue.error_type_id.clone(),
            success: true,
            action_taken: format!("Deleted {}", target_path.display()),
            files_modified: vec![target_path.to_path_buf()],
            error: None,
        })
    }

    /// Modify a file with safety checks and rollback support
    ///
    /// Reads the modification specification from the fix, applies it to the file,
    /// and rolls back if the modification produces invalid content.
    fn apply_modify(
        &self,
        target_path: &Path,
        issue: &DetectedIssue,
        fix: &Fix,
    ) -> Result<FixResult> {
        if !target_path.exists() {
            return Ok(FixResult {
                issue_id: issue.error_type_id.clone(),
                success: false,
                action_taken: "File does not exist".to_string(),
                files_modified: vec![],
                error: Some("Cannot modify non-existent file".to_string()),
            });
        }

        // Safety: never modify binary files
        if Self::is_binary(target_path) {
            warn!("Skipping binary file: {}", target_path.display());
            return Ok(FixResult {
                issue_id: issue.error_type_id.clone(),
                success: false,
                action_taken: "Skipped binary file".to_string(),
                files_modified: vec![],
                error: Some("Cannot modify binary file".to_string()),
            });
        }

        let modification = fix
            .modification
            .as_deref()
            .unwrap_or("unspecified modification");

        if self.dry_run {
            info!(
                "[DRY RUN] Would modify {}: {}",
                target_path.display(),
                modification
            );
            return Ok(FixResult {
                issue_id: issue.error_type_id.clone(),
                success: true,
                action_taken: format!(
                    "[DRY RUN] Would modify {}: {}",
                    target_path.display(),
                    modification
                ),
                files_modified: vec![target_path.to_path_buf()],
                error: None,
            });
        }

        // Snapshot original content for rollback
        let original_content = std::fs::read_to_string(target_path)
            .map_err(|e| Error::Fix(format!("Failed to read {}: {}", target_path.display(), e)))?;

        // Parse and apply the modification
        let spec = Self::parse_modification(modification)?;
        let new_content = match Self::apply_modification(&original_content, &spec) {
            Ok(content) => content,
            Err(e) => {
                warn!(
                    "Modification failed for {}: {}",
                    target_path.display(),
                    e
                );
                return Ok(FixResult {
                    issue_id: issue.error_type_id.clone(),
                    success: false,
                    action_taken: format!("Modification failed: {}", e),
                    files_modified: vec![],
                    error: Some(format!("Modification failed: {}", e)),
                });
            }
        };

        // Verify the modification produced different content
        if new_content == original_content {
            debug!("No changes needed for {}", target_path.display());
            return Ok(FixResult {
                issue_id: issue.error_type_id.clone(),
                success: true,
                action_taken: "No changes needed".to_string(),
                files_modified: vec![],
                error: None,
            });
        }

        // Write modified content
        if let Err(e) = std::fs::write(target_path, &new_content) {
            // Attempt rollback on write failure
            let _ = std::fs::write(target_path, &original_content);
            return Err(Error::Fix(format!(
                "Failed to write modified file {}: {}",
                target_path.display(),
                e
            )));
        }

        info!(
            "Modified {}: {}",
            target_path.display(),
            modification
        );

        Ok(FixResult {
            issue_id: issue.error_type_id.clone(),
            success: true,
            action_taken: format!("Modified {}: {}", target_path.display(), modification),
            files_modified: vec![target_path.to_path_buf()],
            error: None,
        })
    }

    /// Create a file with template expansion
    ///
    /// Supports template variables:
    /// - `gitbot-fleet` - Repository name
    /// - `hyperpolymath` - Repository owner
    /// - `{{LICENSE}}` - License identifier
    /// - `{{YEAR}}` - Current year
    fn apply_create(
        &self,
        target_path: &Path,
        issue: &DetectedIssue,
        fix: &Fix,
    ) -> Result<FixResult> {
        if target_path.exists() {
            return Ok(FixResult {
                issue_id: issue.error_type_id.clone(),
                success: true,
                action_taken: "File already exists".to_string(),
                files_modified: vec![],
                error: None,
            });
        }

        // Check if the file would be gitignored
        if self.would_be_gitignored(target_path) {
            warn!(
                "Skipping creation of gitignored file: {}",
                target_path.display()
            );
            return Ok(FixResult {
                issue_id: issue.error_type_id.clone(),
                success: false,
                action_taken: "File would be gitignored".to_string(),
                files_modified: vec![],
                error: Some("Cannot create file that would be gitignored".to_string()),
            });
        }

        if self.dry_run {
            info!("[DRY RUN] Would create: {}", target_path.display());
            return Ok(FixResult {
                issue_id: issue.error_type_id.clone(),
                success: true,
                action_taken: format!("[DRY RUN] Would create {}", target_path.display()),
                files_modified: vec![target_path.to_path_buf()],
                error: None,
            });
        }

        // Create parent directories if needed
        if let Some(parent) = target_path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        // Get content from template or fix specification
        let content = self.get_template_content(&fix.target, fix);
        let expanded = self.expand_template(&content);

        // Guard: refuse to create files with empty or near-empty content.
        // This prevents bots from pushing useless boilerplate when no
        // template exists for the target file.
        if expanded.trim().is_empty() {
            warn!(
                "Refusing to create {} — template produced empty content",
                target_path.display()
            );
            return Ok(FixResult {
                issue_id: issue.error_type_id.clone(),
                success: false,
                action_taken: "Skipped — no template content available".to_string(),
                files_modified: vec![],
                error: Some(format!(
                    "No template for '{}'; file would be empty",
                    fix.target
                )),
            });
        }

        std::fs::write(target_path, &expanded)?;
        info!("Created: {}", target_path.display());

        Ok(FixResult {
            issue_id: issue.error_type_id.clone(),
            success: true,
            action_taken: format!("Created {}", target_path.display()),
            files_modified: vec![target_path.to_path_buf()],
            error: None,
        })
    }

    /// Disable a workflow (rename to .disabled)
    fn apply_disable(
        &self,
        target_path: &Path,
        issue: &DetectedIssue,
    ) -> Result<FixResult> {
        if !target_path.exists() {
            return Ok(FixResult {
                issue_id: issue.error_type_id.clone(),
                success: true,
                action_taken: "File already absent".to_string(),
                files_modified: vec![],
                error: None,
            });
        }

        let disabled_path = target_path.with_extension("yml.disabled");

        if self.dry_run {
            info!(
                "[DRY RUN] Would disable: {} -> {}",
                target_path.display(),
                disabled_path.display()
            );
            return Ok(FixResult {
                issue_id: issue.error_type_id.clone(),
                success: true,
                action_taken: format!(
                    "[DRY RUN] Would rename {} to {}",
                    target_path.display(),
                    disabled_path.display()
                ),
                files_modified: vec![target_path.to_path_buf()],
                error: None,
            });
        }

        std::fs::rename(target_path, &disabled_path)?;
        info!(
            "Disabled: {} -> {}",
            target_path.display(),
            disabled_path.display()
        );

        Ok(FixResult {
            issue_id: issue.error_type_id.clone(),
            success: true,
            action_taken: format!(
                "Renamed {} to {}",
                target_path.display(),
                disabled_path.display()
            ),
            files_modified: vec![target_path.to_path_buf(), disabled_path],
            error: None,
        })
    }

    /// Check if a path would be gitignored
    fn would_be_gitignored(&self, path: &Path) -> bool {
        if let Ok(repo) = Repository::open(&self.repo_path) {
            if let Ok(relative) = path.strip_prefix(&self.repo_path) {
                return repo.is_path_ignored(relative).unwrap_or(false);
            }
        }
        false
    }

    /// Get template content for a file creation
    fn get_template_content(&self, target: &str, fix: &Fix) -> String {
        // If the fix has explicit content in the fallback field, use it
        if let Some(ref fallback) = fix.fallback {
            return fallback.clone();
        }

        // Built-in templates for common files
        match target {
            "LICENSE" | "LICENSE.txt" => include_str!("../templates/LICENSE.tmpl").to_string(),
            ".editorconfig" => include_str!("../templates/editorconfig.tmpl").to_string(),
            "SECURITY.md" => include_str!("../templates/SECURITY.tmpl").to_string(),
            _ => String::new(),
        }
    }

    /// Expand template variables in content
    fn expand_template(&self, content: &str) -> String {
        let repo_name = self
            .repo_path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown-repo");

        let year = chrono::Utc::now().format("%Y").to_string();

        content
            .replace("gitbot-fleet", repo_name)
            .replace("hyperpolymath", "hyperpolymath")
            .replace("{{LICENSE}}", "PMPL-1.0-or-later")
            .replace("{{YEAR}}", &year)
            .replace("{{AUTHOR}}", "Jonathan D.A. Jewell")
            .replace("{{EMAIL}}", "j.d.a.jewell@open.ac.uk")
    }

    /// Commit changes to the repository
    pub fn commit(&self, message: &str, files: &[PathBuf]) -> Result<()> {
        // EXCLUSION REGISTRY GUARD: a commit is a write action even though
        // apply() has already checked each file individually, because some
        // commits come from non-apply paths (bulk tooling). Fail closed.
        crate::registry_guard::check_write(
            &self.repo_path,
            crate::exclusion_registry::Action::Commit,
            None,
        )?;

        if self.dry_run {
            info!("[DRY RUN] Would commit: {}", message);
            return Ok(());
        }

        let repo = Repository::open(&self.repo_path)?;
        let mut index = repo.index()?;

        // Stage the modified files
        for file in files {
            if let Ok(relative) = file.strip_prefix(&self.repo_path) {
                if file.exists() {
                    index.add_path(relative)?;
                } else {
                    index.remove_path(relative)?;
                }
            }
        }

        index.write()?;
        let tree_id = index.write_tree()?;
        let tree = repo.find_tree(tree_id)?;

        let sig = Signature::now("robot-repo-automaton", "robot@hyperpolymath.dev")?;
        let parent = repo.head()?.peel_to_commit()?;

        repo.commit(
            Some("HEAD"),
            &sig,
            &sig,
            message,
            &tree,
            &[&parent],
        )?;

        info!("Committed: {}", message);
        Ok(())
    }

    /// Apply multiple fixes and commit
    pub fn apply_and_commit(
        &self,
        _issues: &[DetectedIssue],
        fixes: &[(DetectedIssue, Fix)],
    ) -> Result<Vec<FixResult>> {
        let mut results = Vec::new();
        let mut all_modified_files = Vec::new();

        for (issue, fix) in fixes {
            let result = self.apply(issue, fix)?;
            if result.success {
                all_modified_files.extend(result.files_modified.clone());
            }
            results.push(result);
        }

        if !all_modified_files.is_empty() && !self.dry_run {
            let commit_message = if fixes.len() == 1 {
                fixes[0].0.commit_message.clone()
            } else {
                format!("fix: apply {} automated fixes", fixes.len())
            };
            self.commit(&commit_message, &all_modified_files)?;
        }

        Ok(results)
    }
}

/// Normalise a path by resolving `.` and `..` components without requiring the
/// path to exist on disk (unlike `Path::canonicalize`).
///
/// This is used for security validation: after normalisation we can check that
/// the path starts with the repository root and has not escaped via `..` traversal.
fn normalise_path(path: &Path) -> PathBuf {
    use std::path::Component;
    let mut normalised = PathBuf::new();
    for component in path.components() {
        match component {
            Component::ParentDir => {
                // Pop the last element, effectively resolving ".."
                normalised.pop();
            }
            Component::CurDir => {
                // Skip "." — it contributes nothing
            }
            other => {
                normalised.push(other);
            }
        }
    }
    normalised
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn make_issue(id: &str) -> DetectedIssue {
        DetectedIssue {
            error_type_id: id.to_string(),
            error_name: "Test Issue".to_string(),
            severity: crate::catalog::Severity::Medium,
            description: "Test issue description".to_string(),
            affected_files: vec![],
            confidence: 1.0,
            suggested_fix: "Test fix".to_string(),
            commit_message: "fix: test".to_string(),
        }
    }

    fn make_fix(action: FixAction, target: &str) -> Fix {
        Fix {
            action,
            target: target.to_string(),
            reason: None,
            modification: None,
            fallback: None,
        }
    }

    #[test]
    fn test_modify_replace_line() {
        let temp = TempDir::new().unwrap();
        let file_path = temp.path().join("test.txt");
        std::fs::write(&file_path, "line 1\nline 2\nline 3\n").unwrap();

        let fixer = Fixer::new(temp.path().to_path_buf(), false);
        let issue = make_issue("TEST-001");
        let fix = Fix {
            action: FixAction::Modify,
            target: "test.txt".to_string(),
            reason: None,
            modification: Some("replace-line:2:replaced line".to_string()),
            fallback: None,
        };

        let result = fixer.apply(&issue, &fix).unwrap();
        assert!(result.success);

        let content = std::fs::read_to_string(&file_path).unwrap();
        assert!(content.contains("replaced line"));
        assert!(!content.contains("line 2"));
    }

    #[test]
    fn test_modify_replace_pattern() {
        let temp = TempDir::new().unwrap();
        let file_path = temp.path().join("test.txt");
        std::fs::write(&file_path, "old_value = 42\nold_value = 99\n").unwrap();

        let fixer = Fixer::new(temp.path().to_path_buf(), false);
        let issue = make_issue("TEST-002");
        let fix = Fix {
            action: FixAction::Modify,
            target: "test.txt".to_string(),
            reason: None,
            modification: Some("replace-pattern:old_value:new_value".to_string()),
            fallback: None,
        };

        let result = fixer.apply(&issue, &fix).unwrap();
        assert!(result.success);

        let content = std::fs::read_to_string(&file_path).unwrap();
        assert!(content.contains("new_value"));
        assert!(!content.contains("old_value"));
    }

    #[test]
    fn test_modify_invalid_line_rollback() {
        let temp = TempDir::new().unwrap();
        let file_path = temp.path().join("test.txt");
        let original = "line 1\nline 2\n";
        std::fs::write(&file_path, original).unwrap();

        let fixer = Fixer::new(temp.path().to_path_buf(), false);
        let issue = make_issue("TEST-003");
        let fix = Fix {
            action: FixAction::Modify,
            target: "test.txt".to_string(),
            reason: None,
            modification: Some("replace-line:999:impossible".to_string()),
            fallback: None,
        };

        let result = fixer.apply(&issue, &fix).unwrap();
        assert!(!result.success);

        // Verify file content unchanged
        let content = std::fs::read_to_string(&file_path).unwrap();
        assert_eq!(content, original);
    }

    #[test]
    fn test_modify_binary_file_skipped() {
        let temp = TempDir::new().unwrap();
        let file_path = temp.path().join("image.png");
        std::fs::write(&file_path, b"\x89PNG\r\n").unwrap();

        let fixer = Fixer::new(temp.path().to_path_buf(), false);
        let issue = make_issue("TEST-004");
        let fix = Fix {
            action: FixAction::Modify,
            target: "image.png".to_string(),
            reason: None,
            modification: Some("replace-line:1:hacked".to_string()),
            fallback: None,
        };

        let result = fixer.apply(&issue, &fix).unwrap();
        assert!(!result.success);
        assert!(result.error.unwrap().contains("binary"));
    }

    #[test]
    fn test_modify_prepend() {
        let temp = TempDir::new().unwrap();
        let file_path = temp.path().join("test.rs");
        std::fs::write(&file_path, "fn main() {}\n").unwrap();

        let fixer = Fixer::new(temp.path().to_path_buf(), false);
        let issue = make_issue("TEST-005");
        let fix = Fix {
            action: FixAction::Modify,
            target: "test.rs".to_string(),
            reason: None,
            modification: Some("prepend:// SPDX-License-Identifier: PMPL-1.0-or-later".to_string()),
            fallback: None,
        };

        let result = fixer.apply(&issue, &fix).unwrap();
        assert!(result.success);

        let content = std::fs::read_to_string(&file_path).unwrap();
        assert!(content.starts_with("// SPDX-License-Identifier: PMPL-1.0-or-later"));
    }

    #[test]
    fn test_modify_nonexistent_file() {
        let temp = TempDir::new().unwrap();
        let fixer = Fixer::new(temp.path().to_path_buf(), false);
        let issue = make_issue("TEST-006");
        let fix = Fix {
            action: FixAction::Modify,
            target: "nonexistent.txt".to_string(),
            reason: None,
            modification: Some("replace-line:1:test".to_string()),
            fallback: None,
        };

        let result = fixer.apply(&issue, &fix).unwrap();
        assert!(!result.success);
        assert!(result.error.unwrap().contains("non-existent"));
    }

    #[test]
    fn test_delete_removes_file() {
        let temp = TempDir::new().unwrap();
        let file_path = temp.path().join("to_delete.txt");
        std::fs::write(&file_path, "content").unwrap();
        assert!(file_path.exists());

        let fixer = Fixer::new(temp.path().to_path_buf(), false);
        let issue = make_issue("TEST-007");
        let fix = make_fix(FixAction::Delete, "to_delete.txt");

        let result = fixer.apply(&issue, &fix).unwrap();
        assert!(result.success);
        assert!(!file_path.exists());
    }

    #[test]
    fn test_parse_modification_specs() {
        // Test replace-line
        let spec = Fixer::parse_modification("replace-line:5:new content").unwrap();
        assert!(matches!(spec, ModifySpec::ReplaceLine { line: 5, .. }));

        // Test insert-before
        let spec = Fixer::parse_modification("insert-before:1:header").unwrap();
        assert!(matches!(spec, ModifySpec::InsertBefore { line: 1, .. }));

        // Test insert-after
        let spec = Fixer::parse_modification("insert-after:10:footer").unwrap();
        assert!(matches!(spec, ModifySpec::InsertAfter { line: 10, .. }));

        // Test replace-pattern
        let spec = Fixer::parse_modification("replace-pattern:old:new").unwrap();
        assert!(matches!(spec, ModifySpec::ReplacePattern { .. }));

        // Test prepend
        let spec = Fixer::parse_modification("prepend:header line").unwrap();
        assert!(matches!(spec, ModifySpec::Prepend { .. }));

        // Test append
        let spec = Fixer::parse_modification("append:footer line").unwrap();
        assert!(matches!(spec, ModifySpec::Append { .. }));

        // Test invalid
        assert!(Fixer::parse_modification("invalid-spec").is_err());
    }
}
