// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Placeholder detection analyzer

use super::{AnalysisResult, Analyzer, Finding, Severity};
use crate::config::{Config, PlaceholderAction};
use crate::error::Result;
use regex::{Regex, RegexSet};
use std::path::Path;
use tracing::debug;
use walkdir::WalkDir;

/// Placeholder detection analyzer
pub struct PlaceholderAnalyzer;

impl Default for PlaceholderAnalyzer {
    fn default() -> Self {
        Self
    }
}

impl Analyzer for PlaceholderAnalyzer {
    fn name(&self) -> &str {
        "Placeholder Detection"
    }

    fn analyze(&self, path: &Path, config: &Config) -> Result<AnalysisResult> {
        let mut result = AnalysisResult::new();
        let start = std::time::Instant::now();

        // Build pattern matchers
        let mut patterns: Vec<String> = config
            .placeholders
            .patterns
            .iter()
            .map(|p| format!(r"\b{}\b", regex::escape(p)))
            .collect();

        // Add custom regex patterns
        patterns.extend(config.placeholders.custom_patterns.clone());

        let pattern_set = RegexSet::new(&patterns)?;
        let patterns_with_context: Vec<Regex> = patterns
            .iter()
            .map(|p| Regex::new(p).unwrap())
            .collect();

        // Scan files
        for entry in WalkDir::new(path)
            .follow_links(false)
            .into_iter()
            .filter_entry(|e| {
                let name = e.file_name().to_string_lossy();
                !config.exclude.iter().any(|ex| name.contains(ex))
            })
            .filter_map(|e| e.ok())
        {
            let entry_path = entry.path();

            if !entry_path.is_file() {
                continue;
            }

            // Skip binary files and non-text
            if self.is_binary_file(entry_path) {
                continue;
            }

            result.files_checked += 1;

            if let Ok(content) = std::fs::read_to_string(entry_path) {
                self.scan_file_content(
                    entry_path,
                    &content,
                    &pattern_set,
                    &patterns_with_context,
                    &config.placeholders.patterns,
                    config,
                    &mut result,
                );
            }
        }

        // Check if total exceeds max allowed
        if result.findings.len() > config.placeholders.max_allowed {
            let excess = result.findings.len() - config.placeholders.max_allowed;
            result.add(
                Finding::new(
                    "PH-100",
                    "Too Many Placeholders",
                    Severity::Error,
                    &format!(
                        "Found {} placeholders, exceeds maximum allowed ({})",
                        result.findings.len(),
                        config.placeholders.max_allowed
                    ),
                )
                .with_suggestion(&format!("Remove at least {} placeholders before release", excess)),
            );
        }

        result.duration_ms = start.elapsed().as_millis() as u64;
        Ok(result)
    }

    fn fix(&self, path: &Path, config: &Config, findings: &[Finding]) -> Result<Vec<String>> {
        let mut fixed = Vec::new();

        match config.placeholders.action {
            PlaceholderAction::Remove => {
                fixed.extend(self.remove_placeholders(path, config, findings)?);
            }
            PlaceholderAction::Comment => {
                fixed.extend(self.convert_to_comments(path, config, findings)?);
            }
            PlaceholderAction::Flag => {
                // No automatic fix for flag mode
            }
        }

        Ok(fixed)
    }
}

impl PlaceholderAnalyzer {
    /// Check if a file is likely binary
    fn is_binary_file(&self, path: &Path) -> bool {
        let binary_extensions = [
            "exe", "dll", "so", "dylib", "bin", "obj", "o", "a", "lib",
            "png", "jpg", "jpeg", "gif", "bmp", "ico", "webp", "svg",
            "mp3", "mp4", "wav", "ogg", "flac", "avi", "mkv", "mov",
            "zip", "tar", "gz", "bz2", "xz", "7z", "rar",
            "pdf", "doc", "docx", "xls", "xlsx", "ppt", "pptx",
            "wasm", "pyc", "class", "lock",
        ];

        path.extension()
            .and_then(|e| e.to_str())
            .map(|ext| binary_extensions.contains(&ext.to_lowercase().as_str()))
            .unwrap_or(false)
    }

    /// Scan file content for placeholders
    fn scan_file_content(
        &self,
        path: &Path,
        content: &str,
        pattern_set: &RegexSet,
        patterns: &[Regex],
        pattern_names: &[String],
        config: &Config,
        result: &mut AnalysisResult,
    ) {
        for (line_num, line) in content.lines().enumerate() {
            let matches: Vec<usize> = pattern_set.matches(line).into_iter().collect();

            for match_idx in matches {
                if let Some(pattern) = patterns.get(match_idx) {
                    if let Some(m) = pattern.find(line) {
                        let pattern_name = pattern_names.get(match_idx).map(|s| s.as_str()).unwrap_or("custom");

                        debug!(
                            file = %path.display(),
                            line = line_num + 1,
                            pattern = pattern_name,
                            "Found placeholder"
                        );

                        let severity = match config.placeholders.action {
                            PlaceholderAction::Remove => Severity::Error,
                            PlaceholderAction::Comment => Severity::Warning,
                            PlaceholderAction::Flag => Severity::Warning,
                        };

                        result.add(
                            Finding::new(
                                &format!("PH-{:03}", match_idx + 1),
                                &format!("Placeholder: {}", pattern_name),
                                severity,
                                &format!("Found '{}' in: {}", m.as_str(), line.trim()),
                            )
                            .with_file(path.to_path_buf())
                            .with_location(line_num + 1, m.start() + 1)
                            .with_suggestion(&format!(
                                "{}",
                                match config.placeholders.action {
                                    PlaceholderAction::Remove => "Remove or address this placeholder before release",
                                    PlaceholderAction::Comment => "This will be converted to a release note",
                                    PlaceholderAction::Flag => "Address this placeholder or mark as intentional",
                                }
                            ))
                            .fixable(),
                        );
                    }
                }
            }
        }
    }

    /// Remove placeholder lines from files
    fn remove_placeholders(
        &self,
        _path: &Path,
        config: &Config,
        findings: &[Finding],
    ) -> Result<Vec<String>> {
        let mut fixed = Vec::new();

        // Group findings by file
        let mut by_file: std::collections::HashMap<&Path, Vec<&Finding>> =
            std::collections::HashMap::new();

        for finding in findings.iter().filter(|f| f.fixable && f.file.is_some()) {
            if let Some(ref file) = finding.file {
                by_file.entry(file.as_path()).or_default().push(finding);
            }
        }

        for (file_path, file_findings) in by_file {
            if config.dry_run {
                fixed.push(format!(
                    "[DRY RUN] Would remove {} placeholders from {}",
                    file_findings.len(),
                    file_path.display()
                ));
                continue;
            }

            let content = std::fs::read_to_string(file_path)?;
            let lines: Vec<&str> = content.lines().collect();

            // Get line numbers to remove (1-indexed in findings)
            let lines_to_remove: std::collections::HashSet<usize> = file_findings
                .iter()
                .filter_map(|f| f.line)
                .collect();

            // Filter out lines with placeholders (convert to 0-indexed)
            let new_lines: Vec<&str> = lines
                .iter()
                .enumerate()
                .filter(|(i, _)| !lines_to_remove.contains(&(i + 1)))
                .map(|(_, line)| *line)
                .collect();

            let new_content = new_lines.join("\n");
            std::fs::write(file_path, new_content)?;

            fixed.push(format!(
                "Removed {} placeholder lines from {}",
                file_findings.len(),
                file_path.display()
            ));
        }

        Ok(fixed)
    }

    /// Convert placeholders to structured comments
    fn convert_to_comments(
        &self,
        _path: &Path,
        config: &Config,
        findings: &[Finding],
    ) -> Result<Vec<String>> {
        let mut fixed = Vec::new();

        // Group findings by file
        let mut by_file: std::collections::HashMap<&Path, Vec<&Finding>> =
            std::collections::HashMap::new();

        for finding in findings.iter().filter(|f| f.fixable && f.file.is_some()) {
            if let Some(ref file) = finding.file {
                by_file.entry(file.as_path()).or_default().push(finding);
            }
        }

        for (file_path, file_findings) in by_file {
            if config.dry_run {
                fixed.push(format!(
                    "[DRY RUN] Would convert {} placeholders to comments in {}",
                    file_findings.len(),
                    file_path.display()
                ));
                continue;
            }

            let content = std::fs::read_to_string(file_path)?;
            let mut lines: Vec<String> = content.lines().map(|s| s.to_string()).collect();

            // Get lines to modify (1-indexed in findings, convert to 0-indexed)
            for finding in &file_findings {
                if let Some(line_num) = finding.line {
                    let idx = line_num - 1;
                    if idx < lines.len() {
                        // Convert TODO/FIXME to [DEFERRED] format
                        lines[idx] = lines[idx]
                            .replace("TODO", "[DEFERRED:TODO]")
                            .replace("FIXME", "[DEFERRED:FIXME]")
                            .replace("XXX", "[DEFERRED:XXX]")
                            .replace("HACK", "[DEFERRED:HACK]");
                    }
                }
            }

            let new_content = lines.join("\n");
            std::fs::write(file_path, new_content)?;

            fixed.push(format!(
                "Converted {} placeholders to deferred comments in {}",
                file_findings.len(),
                file_path.display()
            ));
        }

        Ok(fixed)
    }
}
