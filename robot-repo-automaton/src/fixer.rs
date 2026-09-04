// SPDX-License-Identifier: MPL-2.0
//! Fix application for detected issues
//!
//! Provides functionality to apply automated fixes to repositories:
//! - **Delete**: Remove files that should not exist
//! - **Modify**: Apply line-level transformations with safety checks and rollback
//! - **Create**: Create missing files from templates with variable expansion
//! - **Disable**: Rename files to .disabled extension

use git2::{Repository, Signature};
use regex::Regex;
use std::ffi::OsString;
use std::io::Write;
use std::path::{Path, PathBuf};
use tempfile::NamedTempFile;
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

        // Resolve existing path components before comparing the target with
        // the canonical repository root. This catches lexical traversal and
        // symlink escapes while permitting a final path that does not exist.
        let resolved_target = match resolve_target_within_repo(&self.repo_path, &target_path) {
            Ok(path) => path,
            Err(error) => {
                warn!(
                    target = %fix.target,
                    repo = %self.repo_path.display(),
                    %error,
                    "SECURITY: fix target failed repository-boundary validation"
                );
                return Ok(FixResult {
                    issue_id: issue.error_type_id.clone(),
                    success: false,
                    action_taken: format!(
                        "REJECTED: target '{}' failed repository-boundary validation",
                        fix.target
                    ),
                    files_modified: vec![],
                    error: Some(format!(
                        "Security violation: target path '{}' is outside the repository directory or could not be resolved safely: {}",
                        fix.target, error
                    )),
                });
            }
        };

        match fix.action {
            // Delete and Disable affect the validated directory entry, not an
            // in-repository symlink's referent. Modify and Create use the
            // resolved path so their writes do not follow that symlink chain.
            FixAction::Delete => self.apply_delete(&target_path, issue),
            FixAction::Modify => self.apply_modify(&resolved_target, issue, fix),
            FixAction::Create => self.apply_create(&resolved_target, issue, fix),
            FixAction::Disable => self.apply_disable(&target_path, issue),
        }
    }

    /// Check whether a file should be treated as binary.
    fn is_binary(path: &Path, content: &[u8]) -> bool {
        let binary_extension = path.extension()
            .and_then(|ext| ext.to_str())
            .map(|ext| BINARY_EXTENSIONS.contains(&ext.to_lowercase().as_str()))
            .unwrap_or(false);

        binary_extension || content.contains(&0) || std::str::from_utf8(content).is_err()
    }

    /// Validate complete source files for formats with parsers in the
    /// automaton's trusted dependency set. Unknown formats are left alone
    /// because guessing their grammar would cause false failures.
    fn validate_source(path: &Path, content: &str) -> Result<()> {
        let extension = path.extension()
            .and_then(|ext| ext.to_str())
            .unwrap_or_default()
            .to_ascii_lowercase();

        match extension.as_str() {
            "rs" => syn::parse_file(content)
                .map(|_| ())
                .map_err(|error| Error::Fix(format!("Rust syntax validation failed: {error}"))),
            "json" => serde_json::from_str::<serde_json::Value>(content)
                .map(|_| ())
                .map_err(|error| Error::Fix(format!("JSON syntax validation failed: {error}"))),
            "jsonl" => {
                for (index, line) in content.lines().enumerate() {
                    if !line.trim().is_empty() {
                        serde_json::from_str::<serde_json::Value>(line).map_err(|error| {
                            Error::Fix(format!(
                                "JSONL syntax validation failed on line {}: {}",
                                index + 1,
                                error
                            ))
                        })?;
                    }
                }
                Ok(())
            }
            "yaml" | "yml" => serde_yaml_ng::from_str::<serde_yaml_ng::Value>(content)
                .map(|_| ())
                .map_err(|error| Error::Fix(format!("YAML syntax validation failed: {error}"))),
            "toml" => toml::from_str::<toml::Value>(content)
                .map(|_| ())
                .map_err(|error| Error::Fix(format!("TOML syntax validation failed: {error}"))),
            "scm" => lexpr::from_str(content)
                .map(|_| ())
                .map_err(|error| Error::Fix(format!("Scheme syntax validation failed: {error}"))),
            _ => Ok(()),
        }
    }

    /// Parse a modification specification string into structured operations
    ///
    /// Supported formats:
    /// - `replace-line:<N>:<content>` - Replace line N with content
    /// - `insert-before:<N>:<content>` - Insert content before line N
    /// - `insert-after:<N>:<content>` - Insert content after line N
    /// - `replace-pattern:<regex>:<replacement>` - Replace regex matches; the
    ///   final unescaped colon separates the fields and replacement colons use `\:`
    /// - `replace-pattern-json:{"pattern":"...","replacement":"..."}` -
    ///   Replace regex matches using an unambiguous structured representation
    /// - `prepend:<content>` - Add content at file beginning
    /// - `append:<content>` - Add content at file end
    fn parse_modification(spec: &str) -> Result<ModifySpec> {
        let (kind, payload) = spec.split_once(':')
            .ok_or_else(|| Error::Fix(format!("Invalid modification specification: {spec}")))?;

        match kind {
            "replace-line" => {
                let (line, content) = payload.split_once(':').ok_or_else(|| {
                    Error::Fix("replace-line requires line number and content".into())
                })?;
                let line: usize = line.parse()
                    .map_err(|_| Error::Fix(format!("Invalid line number: {line}")))?;
                Ok(ModifySpec::ReplaceLine { line, content: content.to_string() })
            }
            "insert-before" => {
                let (line, content) = payload.split_once(':').ok_or_else(|| {
                    Error::Fix("insert-before requires line number and content".into())
                })?;
                let line: usize = line.parse()
                    .map_err(|_| Error::Fix(format!("Invalid line number: {line}")))?;
                Ok(ModifySpec::InsertBefore { line, content: content.to_string() })
            }
            "insert-after" => {
                let (line, content) = payload.split_once(':').ok_or_else(|| {
                    Error::Fix("insert-after requires line number and content".into())
                })?;
                let line: usize = line.parse()
                    .map_err(|_| Error::Fix(format!("Invalid line number: {line}")))?;
                Ok(ModifySpec::InsertAfter { line, content: content.to_string() })
            }
            "replace-pattern" => Self::parse_replace_pattern(payload),
            "replace-pattern-json" => Self::parse_replace_pattern_json(payload),
            "prepend" => Ok(ModifySpec::Prepend { content: payload.to_string() }),
            "append" => Ok(ModifySpec::Append { content: payload.to_string() }),
            _ => Err(Error::Fix(format!("Unknown modification type: {}", spec))),
        }
    }

    /// Split a legacy replacement at its final unescaped colon. This preserves
    /// colons in URL-like regex patterns. Colons in a replacement use `\:`.
    fn parse_replace_pattern(payload: &str) -> Result<ModifySpec> {
        let separator = payload.char_indices().rev()
            .find_map(|(index, character)| {
                (character == ':' && !is_escaped(payload, index)).then_some(index)
            })
            .ok_or_else(|| Error::Fix(
                "replace-pattern requires a pattern and replacement separated by ':'".into()
            ))?;

        let pattern = unescape_colons(&payload[..separator]);
        let replacement = unescape_colons(&payload[separator + 1..]);
        if pattern.is_empty() {
            return Err(Error::Fix("replace-pattern requires a non-empty pattern".into()));
        }

        Ok(ModifySpec::ReplacePattern { pattern, replacement })
    }

    /// Parse an unambiguous JSON representation of a regex replacement.
    fn parse_replace_pattern_json(payload: &str) -> Result<ModifySpec> {
        let value: serde_json::Value = serde_json::from_str(payload)
            .map_err(|error| Error::Fix(format!("Invalid replace-pattern-json payload: {error}")))?;
        let pattern = value.get("pattern")
            .and_then(serde_json::Value::as_str)
            .filter(|pattern| !pattern.is_empty())
            .ok_or_else(|| Error::Fix("replace-pattern-json requires a string 'pattern'".into()))?;
        let replacement = value.get("replacement")
            .and_then(serde_json::Value::as_str)
            .ok_or_else(|| Error::Fix("replace-pattern-json requires a string 'replacement'".into()))?;

        Ok(ModifySpec::ReplacePattern {
            pattern: pattern.to_string(),
            replacement: replacement.to_string(),
        })
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

        let original_bytes = std::fs::read(target_path)
            .map_err(|e| Error::Fix(format!("Failed to read {}: {}", target_path.display(), e)))?;

        // Safety: never modify binary files, including extensionless files
        // whose content contains NUL bytes or is not valid UTF-8.
        if Self::is_binary(target_path, &original_bytes) {
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

        let original_content = String::from_utf8(original_bytes)
            .map_err(|e| Error::Fix(format!("Failed to decode {}: {}", target_path.display(), e)))?;

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

        if let Err(error) = Self::validate_source(target_path, &new_content) {
            warn!(path = %target_path.display(), %error, "source validation rejected modification");
            return Ok(FixResult {
                issue_id: issue.error_type_id.clone(),
                success: false,
                action_taken: format!("Modification rejected by source validation: {error}"),
                files_modified: vec![],
                error: Some(error.to_string()),
            });
        }

        atomic_replace(target_path, new_content.as_bytes())?;

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

        match persist_new_file(target_path, expanded.as_bytes()) {
            Ok(()) => {}
            Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {
                return Ok(FixResult {
                    issue_id: issue.error_type_id.clone(),
                    success: true,
                    action_taken: "File already exists".to_string(),
                    files_modified: vec![],
                    error: None,
                });
            }
            Err(error) => return Err(error.into()),
        }
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

        rename_noreplace(target_path, &disabled_path)?;
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
            .replace("{{LICENSE}}", "MPL-2.0")
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

        let canonical_repo = self.repo_path.canonicalize().map_err(|error| {
            Error::Fix(format!(
                "Failed to canonicalize repository {} before commit: {}",
                self.repo_path.display(), error
            ))
        })?;
        let repo = Repository::open(&canonical_repo)?;
        let mut index = repo.index()?;

        // Stage the modified files
        for file in files {
            if let Ok(relative) = file.strip_prefix(&canonical_repo) {
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

/// Resolve a target using its nearest existing ancestor and verify that the
/// result remains under the canonical repository root.
fn resolve_target_within_repo(repo_path: &Path, target_path: &Path) -> Result<PathBuf> {
    let canonical_repo = repo_path.canonicalize().map_err(|error| {
        Error::Fix(format!(
            "failed to canonicalize repository {}: {}",
            repo_path.display(), error
        ))
    })?;
    let absolute_target = if target_path.is_absolute() {
        target_path.to_path_buf()
    } else {
        std::env::current_dir()
            .map_err(|error| Error::Fix(format!("failed to resolve current directory: {error}")))?
            .join(target_path)
    };
    let lexical_target = normalise_path(&absolute_target);
    let resolved_target = resolve_from_existing_ancestor(&lexical_target)?;

    if !resolved_target.starts_with(&canonical_repo) {
        return Err(Error::Fix(format!(
            "resolved target {} is outside repository {}",
            resolved_target.display(), canonical_repo.display()
        )));
    }

    Ok(resolved_target)
}

/// Canonicalize the nearest existing ancestor, then append any missing final
/// components. A dangling symlink fails the boundary check closed.
fn resolve_from_existing_ancestor(path: &Path) -> Result<PathBuf> {
    let mut ancestor = path.to_path_buf();
    let mut missing_components: Vec<OsString> = Vec::new();

    loop {
        match std::fs::symlink_metadata(&ancestor) {
            Ok(_) => {
                let mut resolved = ancestor.canonicalize().map_err(|error| {
                    Error::Fix(format!(
                        "failed to canonicalize target ancestor {}: {}",
                        ancestor.display(), error
                    ))
                })?;
                for component in missing_components.iter().rev() {
                    resolved.push(component);
                }
                return Ok(normalise_path(&resolved));
            }
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                let component = ancestor.file_name().ok_or_else(|| {
                    Error::Fix(format!("no existing ancestor for target {}", path.display()))
                })?;
                missing_components.push(component.to_os_string());
                if !ancestor.pop() {
                    return Err(Error::Fix(format!(
                        "no existing ancestor for target {}", path.display()
                    )));
                }
            }
            Err(error) => return Err(Error::Fix(format!(
                "failed to inspect target ancestor {}: {}",
                ancestor.display(), error
            ))),
        }
    }
}

fn is_escaped(value: &str, index: usize) -> bool {
    value[..index].bytes().rev()
        .take_while(|byte| *byte == b'\\')
        .count() % 2 == 1
}

fn unescape_colons(value: &str) -> String {
    let mut output = String::with_capacity(value.len());
    let mut characters = value.chars().peekable();
    while let Some(character) = characters.next() {
        if character == '\\' && characters.peek() == Some(&':') {
            characters.next();
            output.push(':');
        } else {
            output.push(character);
        }
    }
    output
}

/// Stage replacement bytes beside the destination and atomically rename them
/// over it only after a complete, synced write.
fn atomic_replace(target_path: &Path, content: &[u8]) -> Result<()> {
    let parent = target_path.parent().ok_or_else(|| {
        Error::Fix(format!("Target {} has no parent directory", target_path.display()))
    })?;
    let permissions = std::fs::metadata(target_path)?.permissions();
    let mut temporary = NamedTempFile::new_in(parent)?;
    temporary.write_all(content)?;
    temporary.as_file_mut().flush()?;
    temporary.as_file().sync_all()?;
    temporary.as_file().set_permissions(permissions)?;
    temporary.persist(target_path).map_err(|error| Error::Fix(format!(
        "Failed to atomically replace {}: {}",
        target_path.display(), error.error
    )))?;
    Ok(())
}

/// Stage a complete new file and publish it with no-clobber semantics.
fn persist_new_file(target_path: &Path, content: &[u8]) -> std::io::Result<()> {
    let parent = target_path.parent().ok_or_else(|| std::io::Error::new(
        std::io::ErrorKind::InvalidInput,
        format!("Target {} has no parent directory", target_path.display()),
    ))?;
    let mut temporary = NamedTempFile::new_in(parent)?;
    temporary.write_all(content)?;
    temporary.as_file_mut().flush()?;
    temporary.as_file().sync_all()?;
    temporary.persist_noclobber(target_path)
        .map(|_| ())
        .map_err(|error| error.error)
}

/// Rename without replacing an existing destination.
#[cfg(any(target_os = "linux", target_os = "android"))]
fn rename_noreplace(source: &Path, destination: &Path) -> std::io::Result<()> {
    rustix::fs::renameat_with(
        rustix::fs::CWD,
        source,
        rustix::fs::CWD,
        destination,
        rustix::fs::RenameFlags::NOREPLACE,
    )?;
    Ok(())
}

/// Portable, data-preserving fallback for platforms without renameat2.
#[cfg(not(any(target_os = "linux", target_os = "android")))]
fn rename_noreplace(source: &Path, destination: &Path) -> std::io::Result<()> {
    std::fs::hard_link(source, destination)?;
    if let Err(error) = std::fs::remove_file(source) {
        let _cleanup_result = std::fs::remove_file(destination);
        return Err(error);
    }
    Ok(())
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
            modification: Some("prepend:// SPDX-License-Identifier: MPL-2.0".to_string()),
            fallback: None,
        };

        let result = fixer.apply(&issue, &fix).unwrap();
        assert!(result.success);

        let content = std::fs::read_to_string(&file_path).unwrap();
        assert!(content.starts_with("// SPDX-License-Identifier: MPL-2.0"));
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

    #[test]
    fn test_replace_pattern_preserves_url_colons() {
        let spec = Fixer::parse_modification("replace-pattern:https?://old:new").unwrap();
        match spec {
            ModifySpec::ReplacePattern { pattern, replacement } => {
                assert_eq!(pattern, "https?://old");
                assert_eq!(replacement, "new");
            }
            other => panic!("unexpected specification: {other:?}"),
        }
    }

    #[test]
    fn test_replace_pattern_supports_escaped_replacement_colons() {
        let spec = Fixer::parse_modification("replace-pattern:old:urn\\:new").unwrap();
        match spec {
            ModifySpec::ReplacePattern { pattern, replacement } => {
                assert_eq!(pattern, "old");
                assert_eq!(replacement, "urn:new");
            }
            other => panic!("unexpected specification: {other:?}"),
        }
    }

    #[test]
    fn test_replace_pattern_json_is_unambiguous() {
        let spec = Fixer::parse_modification(
            r#"replace-pattern-json:{"pattern":"https?://old","replacement":"urn:new"}"#,
        ).unwrap();
        match spec {
            ModifySpec::ReplacePattern { pattern, replacement } => {
                assert_eq!(pattern, "https?://old");
                assert_eq!(replacement, "urn:new");
            }
            other => panic!("unexpected specification: {other:?}"),
        }
    }

    #[test]
    fn test_extensionless_binary_content_is_rejected() {
        let temp = TempDir::new().unwrap();
        let file_path = temp.path().join("opaque-data");
        let original = b"text prefix\0binary payload";
        std::fs::write(&file_path, original).unwrap();

        let fixer = Fixer::new(temp.path().to_path_buf(), false);
        let issue = make_issue("TEST-BINARY-CONTENT");
        let mut fix = make_fix(FixAction::Modify, "opaque-data");
        fix.modification = Some("replace-line:1:hacked".to_string());

        let result = fixer.apply(&issue, &fix).unwrap();
        assert!(!result.success);
        assert_eq!(std::fs::read(&file_path).unwrap(), original);
    }

    #[test]
    fn test_invalid_rust_is_rejected_without_writing() {
        let temp = TempDir::new().unwrap();
        let file_path = temp.path().join("main.rs");
        let original = "fn main() {}\n";
        std::fs::write(&file_path, original).unwrap();

        let fixer = Fixer::new(temp.path().to_path_buf(), false);
        let issue = make_issue("TEST-RUST-VALIDATION");
        let mut fix = make_fix(FixAction::Modify, "main.rs");
        fix.modification = Some("replace-line:1:fn main( {".to_string());

        let result = fixer.apply(&issue, &fix).unwrap();
        assert!(!result.success);
        assert!(result.error.as_deref().unwrap().contains("Rust syntax validation"));
        assert_eq!(std::fs::read_to_string(&file_path).unwrap(), original);
    }

    #[test]
    fn test_structured_source_validators_reject_invalid_content() {
        let invalid_sources = [
            ("data.json", "{"),
            ("events.jsonl", "{}\n{"),
            ("workflow.yml", "key: [unterminated"),
            ("config.toml", "key = ["),
            ("rules.scm", "("),
        ];

        for (path, content) in invalid_sources {
            assert!(
                Fixer::validate_source(Path::new(path), content).is_err(),
                "expected invalid {path} content to be rejected"
            );
        }
    }

    #[test]
    fn test_disable_preserves_existing_disabled_file() {
        let temp = TempDir::new().unwrap();
        let source = temp.path().join("workflow.yml");
        let disabled = temp.path().join("workflow.yml.disabled");
        std::fs::write(&source, "active workflow\n").unwrap();
        std::fs::write(&disabled, "previous disabled workflow\n").unwrap();

        let fixer = Fixer::new(temp.path().to_path_buf(), false);
        let issue = make_issue("TEST-DISABLE-NOCLOBBER");
        let fix = make_fix(FixAction::Disable, "workflow.yml");

        assert!(fixer.apply(&issue, &fix).is_err());
        assert_eq!(std::fs::read_to_string(&source).unwrap(), "active workflow\n");
        assert_eq!(
            std::fs::read_to_string(&disabled).unwrap(),
            "previous disabled workflow\n"
        );
    }

    #[cfg(unix)]
    #[test]
    fn test_symlink_escape_create_is_rejected() {
        use std::os::unix::fs::symlink;

        let temp = TempDir::new().unwrap();
        let outer = TempDir::new().unwrap();
        symlink(outer.path(), temp.path().join("outside-link")).unwrap();

        let fixer = Fixer::new(temp.path().to_path_buf(), false);
        let issue = make_issue("TEST-SYMLINK-ESCAPE");
        let mut fix = make_fix(FixAction::Create, "outside-link/injected.txt");
        fix.fallback = Some("must not escape".to_string());

        let result = fixer.apply(&issue, &fix).unwrap();
        assert!(!result.success);
        assert!(!outer.path().join("injected.txt").exists());
    }

    #[test]
    fn test_relative_repository_path_accepts_in_repo_target() {
        let current = std::env::current_dir().unwrap();
        let temp = tempfile::Builder::new()
            .prefix("fixer-relative-")
            .tempdir_in(&current)
            .unwrap();
        let canonical_temp = temp.path().canonicalize().unwrap();
        let relative_repo = canonical_temp.strip_prefix(&current).unwrap().to_path_buf();
        let file_path = canonical_temp.join("safe.txt");
        std::fs::write(&file_path, "before\n").unwrap();

        let fixer = Fixer::new(relative_repo, false);
        let issue = make_issue("TEST-RELATIVE-REPO");
        let mut fix = make_fix(FixAction::Modify, "safe.txt");
        fix.modification = Some("replace-line:1:after".to_string());

        let result = fixer.apply(&issue, &fix).unwrap();
        assert!(result.success);
        assert_eq!(std::fs::read_to_string(file_path).unwrap(), "after\n");
    }

    #[cfg(unix)]
    #[test]
    fn test_atomic_replace_failure_preserves_original_bytes() {
        use std::os::unix::fs::PermissionsExt;

        let temp = TempDir::new().unwrap();
        let file_path = temp.path().join("protected.txt");
        let original = b"original content\n";
        std::fs::write(&file_path, original).unwrap();

        let original_permissions = std::fs::metadata(temp.path()).unwrap().permissions();
        std::fs::set_permissions(temp.path(), std::fs::Permissions::from_mode(0o500)).unwrap();
        let result = atomic_replace(&file_path, b"replacement content\n");
        std::fs::set_permissions(temp.path(), original_permissions).unwrap();

        assert!(result.is_err());
        assert_eq!(std::fs::read(&file_path).unwrap(), original);
    }
}
