// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Placeholder detection analyzer

use super::{AnalysisResult, Analyzer, Finding, Severity};
use crate::baseline::Baseline;
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

        // Findings the repository has acknowledged in its .hypatia-baseline.json
        // (rule_module "finishing_bot", type "PH-nnn").
        let baseline = Baseline::load(path);

        let pattern_set = RegexSet::new(&patterns)?;
        let patterns_with_context: Vec<Regex> = patterns
            .iter()
            .map(|p| Regex::new(p))
            .collect::<std::result::Result<Vec<Regex>, regex::Error>>(
        )?;

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
                    config,
                    &baseline,
                    path,
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
                .with_file(Path::new(".finishing-bot/config.yml").to_path_buf())
                .with_suggestion(&format!(
                    "Remove at least {} placeholders before release",
                    excess
                )),
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
            "exe", "dll", "so", "dylib", "bin", "obj", "o", "a", "lib", "png", "jpg", "jpeg",
            "gif", "bmp", "ico", "webp", "svg", "mp3", "mp4", "wav", "ogg", "flac", "avi", "mkv",
            "mov", "zip", "tar", "gz", "bz2", "xz", "7z", "rar", "pdf", "doc", "docx", "xls",
            "xlsx", "ppt", "pptx", "wasm", "pyc", "class", "lock",
        ];

        path.extension()
            .and_then(|e| e.to_str())
            .map(|ext| binary_extensions.contains(&ext.to_lowercase().as_str()))
            .unwrap_or(false)
    }

    /// Scan file content for placeholders
    #[allow(clippy::too_many_arguments)]
    fn scan_file_content(
        &self,
        path: &Path,
        content: &str,
        pattern_set: &RegexSet,
        patterns: &[Regex],
        config: &Config,
        baseline: &Baseline,
        root: &Path,
        result: &mut AnalysisResult,
    ) {
        let pattern_names = &config.placeholders.patterns;
        for (line_num, line) in content.lines().enumerate() {
            let matches: Vec<usize> = pattern_set.matches(line).into_iter().collect();

            for match_idx in matches {
                if let Some(pattern) = patterns.get(match_idx) {
                    if let Some(m) = pattern.find(line) {
                        let pattern_name = pattern_names
                            .get(match_idx)
                            .map(|s| s.as_str())
                            .unwrap_or("custom");
                        let rule_id = format!("PH-{:03}", match_idx + 1);
                        let rel = normalise_rel(path.strip_prefix(root).unwrap_or(path));
                        let severity = match config.placeholders.action {
                            PlaceholderAction::Remove => Severity::Error,
                            PlaceholderAction::Comment => Severity::Warning,
                            PlaceholderAction::Flag => Severity::Warning,
                        };
                        if baseline.acknowledges(&rule_id, severity, &rel) {
                            debug!(file = %rel, rule = %rule_id, "Acknowledged in .hypatia-baseline.json");
                            continue;
                        }

                        debug!(
                            file = %path.display(),
                            line = line_num + 1,
                            pattern = pattern_name,
                            "Found placeholder"
                        );

                        result.add(
                            Finding::new(
                                &format!("PH-{:03}", match_idx + 1),
                                &format!("Placeholder: {}", pattern_name),
                                severity,
                                &format!("Found '{}' in: {}", m.as_str(), line.trim()),
                            )
                            .with_file(path.to_path_buf())
                            .with_location(line_num + 1, m.start() + 1)
                            .with_suggestion(match config.placeholders.action {
                                PlaceholderAction::Remove => {
                                    "Remove or address this placeholder before release"
                                }
                                PlaceholderAction::Comment => {
                                    "This will be converted to a release note"
                                }
                                PlaceholderAction::Flag => {
                                    "Address this placeholder or mark as intentional"
                                }
                            })
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
            let lines_to_remove: std::collections::HashSet<usize> =
                file_findings.iter().filter_map(|f| f.line).collect();

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

/// Repo-relative path with `/` separators, so `.hypatia-baseline.json` paths
/// and globs (always written with `/`) match on Windows too.
fn normalise_rel(p: &Path) -> String {
    p.to_string_lossy().replace('\\', "/")
}

#[cfg(test)]
mod baseline_tests {
    use super::*;
    use tempfile::TempDir;

    fn write(dir: &Path, rel: &str, body: &str) {
        let p = dir.join(rel);
        std::fs::create_dir_all(p.parent().unwrap()).unwrap();
        std::fs::write(p, body).unwrap();
    }

    /// A template-shaped repo: its own scanners legitimately name the tokens.
    fn repo(baseline: Option<&str>) -> TempDir {
        let d = TempDir::new().unwrap();
        write(d.path(), "build/just/scan.just", "grep -E 'TODO|FIXME' .\n");
        write(d.path(), "src/main.rs", "// TODO: real placeholder\n");
        if let Some(text) = baseline {
            write(d.path(), ".hypatia-baseline.json", text);
        }
        d
    }

    /// Repo-relative, `/`-separated paths of every PH-0nn finding.
    fn files(d: &TempDir) -> Vec<String> {
        PlaceholderAnalyzer
            .analyze(d.path(), &Config::default())
            .unwrap()
            .findings
            .into_iter()
            .filter(|f| f.id.starts_with("PH-0"))
            .map(|f| normalise_rel(f.file.unwrap().strip_prefix(d.path()).unwrap()))
            .collect()
    }

    /// The default config flags placeholders as warnings (estate word `medium`);
    /// PH-001 is TODO and PH-002 is FIXME.
    const BASELINE_JUST: &str = r#"[
      {"severity":"medium","rule_module":"finishing_bot","type":"PH-001","file_pattern":"build/just/**","note":"scanner names the tokens"},
      {"severity":"medium","rule_module":"finishing_bot","type":"PH-002","file_pattern":"build/just/**","note":"scanner names the tokens"}
    ]"#;

    #[test]
    fn backslash_paths_are_normalised_before_matching() {
        let rel = normalise_rel(Path::new(r"build\just\scan.just"));
        assert_eq!(rel, "build/just/scan.just");
        let b = Baseline::parse(
            BASELINE_JUST,
            chrono::NaiveDate::from_ymd_opt(2026, 11, 13).unwrap(),
        );
        assert!(b.acknowledges("PH-001", Severity::Warning, &rel));
    }

    #[test]
    fn without_a_baseline_every_placeholder_is_reported() {
        let d = repo(None);
        let got = files(&d);
        assert!(got.contains(&"build/just/scan.just".to_string()), "{got:?}");
        assert!(got.contains(&"src/main.rs".to_string()), "{got:?}");
    }

    #[test]
    fn hypatia_baseline_acknowledges_the_named_path_only() {
        let d = repo(Some(BASELINE_JUST));
        let got = files(&d);
        assert!(
            !got.contains(&"build/just/scan.just".to_string()),
            "{got:?}"
        );
        // A real placeholder elsewhere still counts.
        assert!(got.contains(&"src/main.rs".to_string()), "{got:?}");
    }

    #[test]
    fn hypatia_ignore_is_not_read() {
        let d = repo(None);
        write(
            d.path(),
            ".hypatia-ignore",
            "finishing_bot/PH:build/just/\n",
        );
        assert!(files(&d).contains(&"build/just/scan.just".to_string()));
    }

    #[test]
    fn another_modules_or_severitys_entry_acknowledges_nothing_here() {
        let hypatia = r#"[{"severity":"high","rule_module":"cicd_rules","type":"PH-001","file_pattern":"build/just/**"}]"#;
        assert!(files(&repo(Some(hypatia))).contains(&"build/just/scan.just".to_string()));
        let wrong_severity = BASELINE_JUST.replace("medium", "high");
        assert!(files(&repo(Some(&wrong_severity))).contains(&"build/just/scan.just".to_string()));
    }

    #[test]
    fn an_expired_baseline_entry_lets_the_finding_reappear() {
        let expired = BASELINE_JUST.replace("\"note\"", "\"expires_at\":\"2020-01-01\",\"note\"");
        assert!(files(&repo(Some(&expired))).contains(&"build/just/scan.just".to_string()));
    }

    #[test]
    fn acknowledged_scanner_files_no_longer_inflate_ph_100() {
        let mut cfg = Config::default();
        cfg.placeholders.max_allowed = 2; // 2 hits in the scanner file, none elsewhere
        let d = TempDir::new().unwrap();
        write(d.path(), "build/just/scan.just", "grep -E 'TODO|FIXME' .\n");
        let over = PlaceholderAnalyzer.analyze(d.path(), &cfg).unwrap();
        assert!(
            !over.findings.iter().any(|f| f.id == "PH-100"),
            "at the limit"
        );
        cfg.placeholders.max_allowed = 1;
        let over = PlaceholderAnalyzer.analyze(d.path(), &cfg).unwrap();
        assert!(
            over.findings.iter().any(|f| f.id == "PH-100"),
            "over the limit"
        );
        write(d.path(), ".hypatia-baseline.json", BASELINE_JUST);
        let ok = PlaceholderAnalyzer.analyze(d.path(), &cfg).unwrap();
        assert!(ok.findings.is_empty(), "{:?}", ok.findings);
    }
}
