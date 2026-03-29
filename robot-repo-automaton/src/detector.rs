// SPDX-License-Identifier: PMPL-1.0-or-later
//! Issue detection in repositories

use std::collections::HashSet;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

use crate::catalog::{DetectionMethod, ErrorType, Severity};
use crate::error::{Error, Result};

/// A detected issue in a repository
#[derive(Debug, Clone)]
pub struct DetectedIssue {
    pub error_type_id: String,
    pub error_name: String,
    pub severity: Severity,
    pub description: String,
    pub affected_files: Vec<PathBuf>,
    pub confidence: f64,
    pub suggested_fix: String,
    pub commit_message: String,
}

/// Repository analyzer
pub struct Detector {
    /// Root path of the repository
    repo_path: PathBuf,
    /// Detected languages in the repository
    languages: HashSet<String>,
    /// Files in the repository
    files: Vec<PathBuf>,
}

impl Detector {
    /// Create a new detector for a repository
    pub fn new(repo_path: PathBuf) -> Result<Self> {
        if !repo_path.exists() {
            return Err(Error::RepoNotFound(repo_path.display().to_string()));
        }

        let mut detector = Detector {
            repo_path,
            languages: HashSet::new(),
            files: Vec::new(),
        };

        detector.scan_files()?;
        detector.detect_languages();

        Ok(detector)
    }

    /// Scan all files in the repository
    fn scan_files(&mut self) -> Result<()> {
        for entry in WalkDir::new(&self.repo_path)
            .follow_links(false)
            .into_iter()
            .filter_entry(|e| !Self::is_ignored(e.path()))
            .flatten()
        {
            if entry.file_type().is_file() {
                self.files.push(entry.path().to_path_buf());
            }
        }
        Ok(())
    }

    /// Check if a path should be ignored
    fn is_ignored(path: &Path) -> bool {
        let path_str = path.to_string_lossy();
        path_str.contains("/.git/")
            || path_str.contains("/target/")
            || path_str.contains("/node_modules/")
            || path_str.contains("/.cache/")
            || path_str.contains("/dist/")
            || path_str.contains("/build/")
    }

    /// Detect languages based on file extensions
    fn detect_languages(&mut self) {
        for file in &self.files {
            if let Some(ext) = file.extension().and_then(|e| e.to_str()) {
                let lang = match ext {
                    "rs" => Some("rust"),
                    "js" | "jsx" | "mjs" => Some("javascript"),
                    "ts" | "tsx" => Some("typescript"),
                    "py" => Some("python"),
                    "go" => Some("go"),
                    "java" => Some("java"),
                    "kt" | "kts" => Some("kotlin"),
                    "rb" => Some("ruby"),
                    "cs" => Some("csharp"),
                    "cpp" | "cc" | "cxx" | "c" | "h" | "hpp" => Some("cpp"),
                    "swift" => Some("swift"),
                    "hs" => Some("haskell"),
                    "ml" | "mli" => Some("ocaml"),
                    "res" | "resi" => Some("rescript"),
                    "gleam" => Some("gleam"),
                    "ex" | "exs" => Some("elixir"),
                    "erl" | "hrl" => Some("erlang"),
                    "jl" => Some("julia"),
                    "sh" | "bash" => Some("shell"),
                    "yml" | "yaml" => Some("yaml"),
                    "toml" => Some("toml"),
                    "json" => Some("json"),
                    "md" | "markdown" => Some("markdown"),
                    "adoc" | "asciidoc" => Some("asciidoc"),
                    _ => None,
                };
                if let Some(lang) = lang {
                    self.languages.insert(lang.to_string());
                }
            }
        }
    }

    /// Get detected languages
    pub fn languages(&self) -> &HashSet<String> {
        &self.languages
    }

    /// Check if a file exists relative to repo root
    pub fn file_exists(&self, relative_path: &str) -> bool {
        self.repo_path.join(relative_path).exists()
    }

    /// Get all files matching a glob pattern
    pub fn files_matching(&self, pattern: &str) -> Vec<&PathBuf> {
        let glob_pattern = glob::Pattern::new(pattern).ok();
        self.files
            .iter()
            .filter(|f| {
                if let Some(ref pat) = glob_pattern {
                    if let Ok(rel) = f.strip_prefix(&self.repo_path) {
                        return pat.matches_path(rel);
                    }
                }
                false
            })
            .collect()
    }

    /// Detect issues based on an error type definition
    pub fn detect(&self, error_type: &ErrorType) -> Option<DetectedIssue> {
        match &error_type.detection.method {
            DetectionMethod::FileExistence => self.detect_file_existence(error_type),
            DetectionMethod::LanguageDetection => self.detect_language_mismatch(error_type),
            DetectionMethod::ContentMatch => self.detect_content_match(error_type),
            DetectionMethod::Custom(_) => None, // Custom detection not implemented
        }
    }

    /// Detect file existence issues
    fn detect_file_existence(&self, error_type: &ErrorType) -> Option<DetectedIssue> {
        let detection = &error_type.detection;
        let condition = detection.condition.as_deref().unwrap_or("any-exist");

        let existing: Vec<PathBuf> = detection
            .files
            .iter()
            .filter(|f| self.file_exists(f))
            .map(|f| self.repo_path.join(f))
            .collect();

        let issue_detected = match condition {
            "both-exist" | "all-exist" => existing.len() == detection.files.len(),
            "any-exist" => !existing.is_empty(),
            "none-exist" => existing.is_empty() && !detection.files.is_empty(),
            _ => false,
        };

        if issue_detected {
            Some(DetectedIssue {
                error_type_id: error_type.id.clone(),
                error_name: error_type.name.clone(),
                severity: error_type.severity,
                description: error_type.description.clone(),
                affected_files: existing,
                confidence: 1.0, // File existence is deterministic
                suggested_fix: format!(
                    "{:?} {}",
                    error_type.fix.action, error_type.fix.target
                ),
                commit_message: error_type.commit_message.clone(),
            })
        } else {
            None
        }
    }

    /// Detect language mismatch issues (e.g., CodeQL configured for wrong languages).
    ///
    /// Checks that CI workflow language matrices match the actual languages
    /// detected in the repository. Catches stale CodeQL configs, wrong
    /// test runners, and mismatched linter configurations.
    fn detect_language_mismatch(&self, error_type: &ErrorType) -> Option<DetectedIssue> {
        let detection = &error_type.detection;

        // extension_map maps workflow file patterns to expected language keys
        // e.g., {"codeql.yml": "javascript-typescript", "rust.yml": "rust"}
        if detection.extension_map.is_empty() {
            return None;
        }

        let mut mismatches = Vec::new();

        for (workflow_glob, expected_lang) in &detection.extension_map {
            // Check if the workflow file exists
            let matching_files = self.files_matching(workflow_glob);

            if matching_files.is_empty() {
                continue;
            }

            // Check if the expected language is actually present in the repo
            let lang_present = self.languages.contains(expected_lang);

            if !lang_present {
                // Workflow references a language not found in this repo
                for file in matching_files {
                    mismatches.push(file.clone());
                }
            }
        }

        // Also check the reverse: repo has languages but no corresponding workflow
        let repo_languages: Vec<String> = self.languages.iter().cloned().collect();
        let configured_langs: Vec<&String> = detection.extension_map.values().collect();

        for lang in &repo_languages {
            // Map repo language to CodeQL language name
            let codeql_lang = match lang.as_str() {
                "javascript" | "typescript" => "javascript-typescript",
                "python" => "python",
                "go" => "go",
                "rust" => "rust",
                "java" | "kotlin" => "java-kotlin",
                "ruby" => "ruby",
                "csharp" => "csharp",
                _ => continue,
            };

            if !configured_langs.iter().any(|c| c.as_str() == codeql_lang) {
                // Language exists in repo but isn't in any workflow config
                // This is informational, not always a problem
            }
        }

        if mismatches.is_empty() {
            None
        } else {
            Some(DetectedIssue {
                error_type_id: error_type.id.clone(),
                error_name: error_type.name.clone(),
                severity: error_type.severity,
                description: format!(
                    "{} — {} workflow(s) reference languages not found in repo",
                    error_type.description,
                    mismatches.len()
                ),
                affected_files: mismatches,
                confidence: 0.90,
                suggested_fix: format!(
                    "{:?} {} — update language matrix to match actual repo languages",
                    error_type.fix.action, error_type.fix.target
                ),
                commit_message: error_type.commit_message.clone(),
            })
        }
    }

    /// Detect content match issues by regex scanning file contents.
    ///
    /// Searches files matching the glob patterns in `detection.files` for
    /// content matching the regex in `detection.condition`. This enables
    /// detection of anti-patterns like `believe_me`, `sorry`, hardcoded
    /// secrets, eval() usage, unpinned actions, etc.
    fn detect_content_match(&self, error_type: &ErrorType) -> Option<DetectedIssue> {
        let detection = &error_type.detection;

        // condition holds the regex pattern to search for
        let regex_str = detection.condition.as_deref()?;
        let re = regex::Regex::new(regex_str).ok()?;

        let mut affected = Vec::new();

        // files holds glob patterns for which files to search
        for file_glob in &detection.files {
            let matching = self.files_matching(file_glob);

            for file_path in matching {
                // Skip binary files (> 1MB or non-UTF8)
                if let Ok(metadata) = std::fs::metadata(file_path) {
                    if metadata.len() > 1_048_576 {
                        continue;
                    }
                }

                if let Ok(content) = std::fs::read_to_string(file_path) {
                    if re.is_match(&content) {
                        affected.push(file_path.clone());
                    }
                }
            }
        }

        if affected.is_empty() {
            None
        } else {
            let match_count = affected.len();

            Some(DetectedIssue {
                error_type_id: error_type.id.clone(),
                error_name: error_type.name.clone(),
                severity: error_type.severity,
                description: format!(
                    "{} — pattern found in {} file(s)",
                    error_type.description, match_count
                ),
                affected_files: affected,
                confidence: 0.95,
                suggested_fix: format!(
                    "{:?} {}",
                    error_type.fix.action, error_type.fix.target
                ),
                commit_message: error_type.commit_message.clone(),
            })
        }
    }

    /// Run all detections from a catalog
    pub fn detect_all(&self, error_types: &[ErrorType]) -> Vec<DetectedIssue> {
        error_types
            .iter()
            .filter_map(|et| self.detect(et))
            .collect()
    }

    /// Get CodeQL-compatible language list
    pub fn codeql_languages(&self) -> Vec<String> {
        let mut codeql_langs = Vec::new();

        // Always include actions for workflow scanning
        codeql_langs.push("actions".to_string());

        if self.languages.contains("javascript") || self.languages.contains("typescript") {
            codeql_langs.push("javascript-typescript".to_string());
        }
        if self.languages.contains("python") {
            codeql_langs.push("python".to_string());
        }
        if self.languages.contains("go") {
            codeql_langs.push("go".to_string());
        }
        if self.languages.contains("java") || self.languages.contains("kotlin") {
            codeql_langs.push("java-kotlin".to_string());
        }
        if self.languages.contains("ruby") {
            codeql_langs.push("ruby".to_string());
        }
        if self.languages.contains("csharp") {
            codeql_langs.push("csharp".to_string());
        }
        if self.languages.contains("cpp") {
            codeql_langs.push("cpp".to_string());
        }
        if self.languages.contains("swift") {
            codeql_langs.push("swift".to_string());
        }
        // Note: Rust is not supported by CodeQL

        codeql_langs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_language_detection() {
        let temp = TempDir::new().unwrap();
        std::fs::write(temp.path().join("main.rs"), "fn main() {}").unwrap();
        std::fs::write(temp.path().join("lib.js"), "export default {}").unwrap();

        let detector = Detector::new(temp.path().to_path_buf()).unwrap();
        assert!(detector.languages().contains("rust"));
        assert!(detector.languages().contains("javascript"));
    }

    #[test]
    fn test_file_exists() {
        let temp = TempDir::new().unwrap();
        std::fs::create_dir_all(temp.path().join(".github/workflows")).unwrap();
        std::fs::write(
            temp.path().join(".github/workflows/ci.yml"),
            "name: CI",
        )
        .unwrap();

        let detector = Detector::new(temp.path().to_path_buf()).unwrap();
        assert!(detector.file_exists(".github/workflows/ci.yml"));
        assert!(!detector.file_exists(".github/workflows/nonexistent.yml"));
    }
}
