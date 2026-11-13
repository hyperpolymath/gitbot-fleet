// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Machine-readability analyzer

use super::{AnalysisResult, Analyzer, Finding, Severity};
use crate::config::Config;
use crate::error::Result;
use std::path::Path;
use tracing::debug;
use walkdir::WalkDir;

/// Machine-readability analyzer
pub struct MachineAnalyzer;

impl Default for MachineAnalyzer {
    fn default() -> Self {
        Self
    }
}

impl Analyzer for MachineAnalyzer {
    fn name(&self) -> &str {
        "Machine-Readability"
    }

    fn analyze(&self, path: &Path, config: &Config) -> Result<AnalysisResult> {
        let mut result = AnalysisResult::new();
        let start = std::time::Instant::now();

        // Validate JSON files
        if config.machine.validate_json {
            self.check_json_validity(path, config, &mut result);
        }

        // Validate YAML files
        if config.machine.validate_yaml {
            self.check_yaml_validity(path, config, &mut result);
        }

        // Check for structured data
        if config.machine.check_structured_data {
            self.check_structured_data(path, config, &mut result);
        }

        // Check for robots.txt
        self.check_robots_txt(path, &mut result);

        // Check for .machine_readable/ directory
        self.check_machine_readable_dir(path, &mut result);

        result.duration_ms = start.elapsed().as_millis() as u64;
        Ok(result)
    }

    fn fix(&self, path: &Path, _config: &Config, findings: &[Finding]) -> Result<Vec<String>> {
        let mut fixes = Vec::new();

        for finding in findings {
            match finding.id.as_str() {
                "MACH-003" => {
                    // Add structured data (schema.org JSON-LD) template
                    if let Some(ref file) = finding.file {
                        if let Ok(content) = std::fs::read_to_string(file) {
                            if content.contains("<head>")
                                && !content.contains("application/ld+json")
                            {
                                let json_ld = "\n    <script type=\"application/ld+json\">\n    {\n      \
                                    \"@context\": \"https://schema.org\",\n      \
                                    \"@type\": \"SoftwareSourceCode\",\n      \
                                    \"name\": \"TODO: Project Name\",\n      \
                                    \"description\": \"TODO: Project Description\",\n      \
                                    \"license\": \"https://spdx.org/licenses/MPL-2.0\",\n      \
                                    \"codeRepository\": \"TODO: Repository URL\"\n    }\n    </script>\n";
                                let updated =
                                    content.replace("</head>", &format!("{}</head>", json_ld));
                                std::fs::write(file, &updated)?;
                                fixes.push(format!(
                                    "Added schema.org JSON-LD template to {}",
                                    file.display()
                                ));
                            }
                        }
                    }
                }
                "MACH-005" => {
                    // Add robots.txt
                    let robots_path = path.join("robots.txt");
                    if !robots_path.exists() {
                        let content =
                            "# robots.txt for repository documentation\nUser-agent: *\nAllow: /\n\nSitemap: TODO\n";
                        std::fs::write(&robots_path, content)?;
                        fixes.push(format!("Created robots.txt at {}", robots_path.display()));
                    }
                }
                _ => {}
            }
        }

        if fixes.is_empty() {
            fixes.push("No machine-readability fixes applicable".to_string());
        }

        Ok(fixes)
    }
}

impl MachineAnalyzer {
    fn check_json_validity(&self, path: &Path, config: &Config, result: &mut AnalysisResult) {
        for entry in WalkDir::new(path)
            .into_iter()
            .filter_entry(|e| {
                let name = e.file_name().to_string_lossy();
                !config.exclude.iter().any(|ex| name.contains(ex))
            })
            .filter_map(|e| e.ok())
        {
            if entry.path().extension().and_then(|s| s.to_str()) == Some("json") {
                result.files_checked += 1;

                if let Ok(content) = std::fs::read_to_string(entry.path()) {
                    // .devcontainer/*.json and .vscode/*.json are JSONC (comments and
                    // trailing commas) because the tools that read them demand it; the
                    // estate JSON policy carves them out. Parse them as JSONC.
                    let parsed = if is_tool_demanded_jsonc(path, entry.path()) {
                        serde_json::from_str::<serde_json::Value>(&strip_jsonc(&content))
                    } else {
                        serde_json::from_str::<serde_json::Value>(&content)
                    };
                    if let Err(e) = parsed {
                        result.add(
                            Finding::new(
                                "MACH-001",
                                "Invalid JSON",
                                Severity::Error,
                                &format!("JSON file is not valid: {}", e),
                            )
                            .with_file(entry.path().to_path_buf())
                            .with_suggestion("Fix JSON syntax errors"),
                        );
                    } else {
                        debug!("Valid JSON: {}", entry.path().display());
                    }
                }
            }
        }
    }

    fn check_yaml_validity(&self, path: &Path, config: &Config, result: &mut AnalysisResult) {
        for entry in WalkDir::new(path)
            .into_iter()
            .filter_entry(|e| {
                let name = e.file_name().to_string_lossy();
                !config.exclude.iter().any(|ex| name.contains(ex))
            })
            .filter_map(|e| e.ok())
        {
            let ext = entry.path().extension().and_then(|s| s.to_str());
            if ext == Some("yaml") || ext == Some("yml") {
                result.files_checked += 1;

                if let Ok(content) = std::fs::read_to_string(entry.path()) {
                    if let Err(e) = serde_yaml::from_str::<serde_yaml::Value>(&content) {
                        result.add(
                            Finding::new(
                                "MACH-002",
                                "Invalid YAML",
                                Severity::Error,
                                &format!("YAML file is not valid: {}", e),
                            )
                            .with_file(entry.path().to_path_buf())
                            .with_suggestion("Fix YAML syntax errors"),
                        );
                    } else {
                        debug!("Valid YAML: {}", entry.path().display());
                    }
                }
            }
        }
    }

    fn check_structured_data(&self, path: &Path, config: &Config, result: &mut AnalysisResult) {
        for entry in WalkDir::new(path)
            .into_iter()
            .filter_entry(|e| {
                let name = e.file_name().to_string_lossy();
                !config.exclude.iter().any(|ex| name.contains(ex))
            })
            .filter_map(|e| e.ok())
        {
            if entry.path().extension().and_then(|s| s.to_str()) == Some("html") {
                result.files_checked += 1;

                if let Ok(content) = std::fs::read_to_string(entry.path()) {
                    let has_json_ld = content.contains("application/ld+json");
                    let has_microdata =
                        content.contains("itemscope") || content.contains("itemprop");
                    let has_schema_org = content.contains("schema.org");

                    if !has_json_ld && !has_microdata && config.machine.require_schema_org {
                        result.add(
                            Finding::new(
                                "MACH-003",
                                "Missing structured data",
                                Severity::Warning,
                                "HTML file has no structured data (JSON-LD or microdata).",
                            )
                            .with_file(entry.path().to_path_buf())
                            .with_suggestion(
                                "Add schema.org structured data for better machine readability",
                            ),
                        );
                    }

                    if has_json_ld {
                        // Try to validate JSON-LD
                        if let Some(json_ld_start) =
                            content.find("<script type=\"application/ld+json\">")
                        {
                            if let Some(json_ld_end) = content[json_ld_start..].find("</script>") {
                                let json_ld =
                                    &content[json_ld_start + 36..json_ld_start + json_ld_end];
                                if serde_json::from_str::<serde_json::Value>(json_ld).is_err() {
                                    result.add(
                                        Finding::new(
                                            "MACH-004",
                                            "Invalid JSON-LD",
                                            Severity::Error,
                                            "JSON-LD structured data is not valid JSON.",
                                        )
                                        .with_file(entry.path().to_path_buf())
                                        .with_suggestion("Fix JSON-LD syntax"),
                                    );
                                }
                            }
                        }
                    }

                    if has_schema_org {
                        debug!("Found schema.org markup in {}", entry.path().display());
                    }
                }
            }
        }
    }

    fn check_robots_txt(&self, path: &Path, result: &mut AnalysisResult) {
        let robots_path = path.join("robots.txt");

        if !robots_path.exists() {
            result.add(
                Finding::new(
                    "MACH-005",
                    "Missing robots.txt",
                    Severity::Info,
                    "No robots.txt found. Consider adding one for crawler control.",
                )
                .with_suggestion("Add robots.txt to repository root if hosting documentation"),
            );
        } else {
            result.files_checked += 1;
            debug!("Found robots.txt");
        }
    }

    fn check_machine_readable_dir(&self, path: &Path, result: &mut AnalysisResult) {
        let mr_dir = path.join(".machine_readable");

        if !mr_dir.exists() {
            result.add(
                Finding::new(
                    "MACH-006",
                    "Missing .machine_readable/ directory",
                    Severity::Warning,
                    "No .machine_readable/ directory found (contractiles and other machine-readable estate metadata live here).",
                )
                .with_file(mr_dir.clone())
                // Not auto-fixable: git cannot track an empty directory, and a
                // placeholder file would be invented content. What belongs here
                // (contractiles, the chora deed) is owner-authored.
                .with_suggestion("Create the .machine_readable/ directory"),
            );
            return;
        }

        result.files_checked += 1;

        // MACH-007 (missing STATE/META/ECOSYSTEM .scm) is retired: those
        // documents are clauses of the repository's `<name>_chora.deed` now,
        // not files under .machine_readable/, so their absence is canonical.
    }
}

/// True for JSON files whose consumer demands JSONC: anything under a
/// `.devcontainer/` or `.vscode/` directory of the repository. Only the
/// repo-relative components count: a checkout that itself lives under a
/// `.vscode/` or `.devcontainer/` ancestor does not make every JSON JSONC.
fn is_tool_demanded_jsonc(root: &Path, path: &Path) -> bool {
    let Ok(rel) = path.strip_prefix(root) else {
        return false;
    };
    rel.parent().is_some_and(|dir| {
        dir.components()
            .any(|c| c.as_os_str() == ".devcontainer" || c.as_os_str() == ".vscode")
    })
}

/// Reduce JSONC to JSON: drop a BOM, `//` and `/* */` comments and trailing
/// commas, leaving string contents untouched.
fn strip_jsonc(src: &str) -> String {
    let src = src.strip_prefix('\u{feff}').unwrap_or(src);
    let chars: Vec<char> = src.chars().collect();
    let mut out = String::with_capacity(src.len());
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        if c == '"' {
            out.push(c);
            i += 1;
            while i < chars.len() {
                out.push(chars[i]);
                if chars[i] == '\\' && i + 1 < chars.len() {
                    out.push(chars[i + 1]);
                    i += 2;
                    continue;
                }
                if chars[i] == '"' {
                    i += 1;
                    break;
                }
                i += 1;
            }
        } else if c == '/' && chars.get(i + 1) == Some(&'/') {
            while i < chars.len() && chars[i] != '\n' {
                i += 1;
            }
        } else if c == '/' && chars.get(i + 1) == Some(&'*') {
            let start = i;
            i += 2;
            while i + 1 < chars.len() && !(chars[i] == '*' && chars[i + 1] == '/') {
                i += 1;
            }
            if i + 1 < chars.len() {
                // A comment is whitespace, not nothing: `1/*c*/2` must not
                // fuse into `12`. Newlines inside it are kept (line numbers).
                out.push(' ');
                out.extend(chars[start..i].iter().filter(|c| **c == '\n'));
                i += 2;
            } else {
                // Unterminated block comment: keep the text so parsing fails.
                out.extend(&chars[start..]);
                i = chars.len();
            }
        } else {
            out.push(c);
            i += 1;
        }
    }
    // Trailing commas: a comma whose next non-space char closes a container.
    let b: Vec<char> = out.chars().collect();
    let mut res = String::with_capacity(b.len());
    let mut in_str = false;
    let mut j = 0;
    while j < b.len() {
        let c = b[j];
        if in_str {
            res.push(c);
            if c == '\\' && j + 1 < b.len() {
                res.push(b[j + 1]);
                j += 1;
            } else if c == '"' {
                in_str = false;
            }
        } else if c == '"' {
            in_str = true;
            res.push(c);
        } else if c == ',' {
            let next = b[j + 1..].iter().find(|x| !x.is_whitespace());
            // Only a comma after a completed member or element is trailing;
            // `{,}`, `[,]` and `,,` stay so the parse fails.
            let prev = res.chars().rev().find(|x| !x.is_whitespace());
            let after_value = !matches!(prev, None | Some('{' | '[' | ',' | ':'));
            if !(after_value && matches!(next, Some('}') | Some(']'))) {
                res.push(c);
            }
        } else {
            res.push(c);
        }
        j += 1;
    }
    res
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn default_config() -> Config {
        Config::default()
    }

    #[test]
    fn test_well_structured_repo() {
        let dir = TempDir::new().unwrap();
        // Create valid JSON file
        std::fs::write(
            dir.path().join("package.json"),
            r#"{"name": "test", "version": "1.0.0"}"#,
        )
        .unwrap();
        // Create valid YAML file
        std::fs::write(
            dir.path().join("config.yml"),
            "name: test\nversion: 1.0.0\n",
        )
        .unwrap();
        // Create .machine_readable/ with SCM files
        let mr_dir = dir.path().join(".machine_readable");
        std::fs::create_dir_all(&mr_dir).unwrap();
        std::fs::write(mr_dir.join("STATE.scm"), "(state)").unwrap();
        std::fs::write(mr_dir.join("META.scm"), "(meta)").unwrap();
        std::fs::write(mr_dir.join("ECOSYSTEM.scm"), "(ecosystem)").unwrap();
        // Create robots.txt
        std::fs::write(dir.path().join("robots.txt"), "User-agent: *\nAllow: /\n").unwrap();

        let analyzer = MachineAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(dir.path(), &config).unwrap();

        assert!(
            !result.has_errors(),
            "Well-structured repo should have no errors, got: {:?}",
            result.errors()
        );
    }

    #[test]
    fn test_invalid_json_detected() {
        let dir = TempDir::new().unwrap();
        // Create invalid JSON
        std::fs::write(dir.path().join("broken.json"), "{invalid json}").unwrap();

        let analyzer = MachineAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(dir.path(), &config).unwrap();

        assert!(result.has_errors(), "Invalid JSON should produce errors");
        let error_ids: Vec<&str> = result.errors().iter().map(|f| f.id.as_str()).collect();
        assert!(
            error_ids.contains(&"MACH-001"),
            "Should report MACH-001 for invalid JSON"
        );
    }

    #[test]
    fn test_missing_machine_readable_dir() {
        let dir = TempDir::new().unwrap();
        // No .machine_readable/ directory

        let analyzer = MachineAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(dir.path(), &config).unwrap();

        let ids: Vec<&str> = result.findings.iter().map(|f| f.id.as_str()).collect();
        assert!(
            ids.contains(&"MACH-006"),
            "Should report MACH-006 for missing .machine_readable/"
        );
    }

    #[test]
    fn mach_006_is_not_auto_fixable_and_fix_creates_nothing() {
        let dir = TempDir::new().unwrap();
        let analyzer = MachineAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(dir.path(), &config).unwrap();
        let f = result.findings.iter().find(|f| f.id == "MACH-006").unwrap();
        assert!(!f.fixable, "an empty dir is untracked by git; not fixable");
        let findings = vec![f.clone()];
        let fixes = analyzer.fix(dir.path(), &config, &findings).unwrap();
        assert_eq!(
            fixes,
            vec!["No machine-readability fixes applicable".to_string()]
        );
        assert!(!dir.path().join(".machine_readable").exists());
    }

    fn ids(dir: &Path) -> Vec<String> {
        MachineAnalyzer::default()
            .analyze(dir, &default_config())
            .unwrap()
            .findings
            .into_iter()
            .map(|f| f.id)
            .collect()
    }

    #[test]
    fn mach_007_is_retired() {
        let dir = TempDir::new().unwrap();
        std::fs::create_dir_all(dir.path().join(".machine_readable")).unwrap();
        let got = ids(dir.path());
        assert!(!got.contains(&"MACH-007".to_string()), "{got:?}");
        assert!(!got.contains(&"MACH-006".to_string()), "{got:?}");
        // The directory itself is still expected.
        let empty = TempDir::new().unwrap();
        assert!(ids(empty.path()).contains(&"MACH-006".to_string()));
    }

    #[test]
    fn mach_006_is_not_auto_fixable() {
        // The README documents the auto-fix list; MACH-006 must stay off it.
        let dir = TempDir::new().unwrap();
        let a = MachineAnalyzer::default();
        let findings = a.analyze(dir.path(), &default_config()).unwrap().findings;
        assert!(findings.iter().any(|f| f.id == "MACH-006"));
        a.fix(dir.path(), &default_config(), &findings).unwrap();
        assert!(!dir.path().join(".machine_readable").exists());
    }

    const JSONC: &str =
        "{\n  // a comment\n  \"url\": \"http://x//y\", /* inline */\n  \"list\": [1, 2,],\n}\n";

    #[test]
    fn devcontainer_and_vscode_json_are_parsed_as_jsonc() {
        for rel in [
            ".devcontainer/devcontainer.json",
            ".vscode/settings.json",
            ".devcontainer/sub/devcontainer.json",
        ] {
            let dir = TempDir::new().unwrap();
            let p = dir.path().join(rel);
            std::fs::create_dir_all(p.parent().unwrap()).unwrap();
            std::fs::write(&p, JSONC).unwrap();
            assert!(!ids(dir.path()).contains(&"MACH-001".to_string()), "{rel}");
        }
    }

    #[test]
    fn the_same_text_elsewhere_is_still_invalid_json() {
        let dir = TempDir::new().unwrap();
        std::fs::write(dir.path().join("config.json"), JSONC).unwrap();
        assert!(ids(dir.path()).contains(&"MACH-001".to_string()));
    }

    #[test]
    fn genuinely_broken_jsonc_is_still_reported() {
        let dir = TempDir::new().unwrap();
        let p = dir.path().join(".vscode/settings.json");
        std::fs::create_dir_all(p.parent().unwrap()).unwrap();
        std::fs::write(&p, "{ \"a\": }").unwrap();
        assert!(ids(dir.path()).contains(&"MACH-001".to_string()));
    }

    #[test]
    fn unterminated_block_comment_is_invalid() {
        let dir = TempDir::new().unwrap();
        let p = dir.path().join(".vscode/settings.json");
        std::fs::create_dir_all(p.parent().unwrap()).unwrap();
        std::fs::write(&p, "{\"a\": 1} /*").unwrap();
        assert!(ids(dir.path()).contains(&"MACH-001".to_string()));
        std::fs::write(&p, "{\"a\": 1} /* ok */").unwrap();
        assert!(!ids(dir.path()).contains(&"MACH-001".to_string()));
    }

    #[test]
    fn empty_containers_with_a_lone_comma_are_invalid() {
        for body in ["{,}", "[,]", "{ , }", "[1,,]", "{\"a\":1,,}", "{\"a\":,}"] {
            let dir = TempDir::new().unwrap();
            let p = dir.path().join(".vscode/settings.json");
            std::fs::create_dir_all(p.parent().unwrap()).unwrap();
            std::fs::write(&p, body).unwrap();
            assert!(ids(dir.path()).contains(&"MACH-001".to_string()), "{body}");
        }
    }

    #[test]
    fn valid_trailing_commas_still_work() {
        for body in [
            "{\"a\":1,}",
            "[1,2,]",
            "{\"a\":[1,],}",
            "{\"a\":1 , \n}",
            "[\"x\", /* c */ ]",
        ] {
            let v: std::result::Result<serde_json::Value, _> =
                serde_json::from_str(&strip_jsonc(body));
            assert!(v.is_ok(), "{body}");
        }
    }

    #[test]
    fn jsonc_detection_ignores_ancestors_above_the_repo_root() {
        let dir = TempDir::new().unwrap();
        let root = dir.path().join(".vscode/checkout");
        std::fs::create_dir_all(&root).unwrap();
        std::fs::write(root.join("config.json"), JSONC).unwrap();
        assert!(ids(&root).contains(&"MACH-001".to_string()));
        assert!(!is_tool_demanded_jsonc(&root, &root.join("config.json")));
        assert!(is_tool_demanded_jsonc(
            &root,
            &root.join(".vscode/settings.json")
        ));
    }

    #[test]
    fn strip_jsonc_leaves_string_contents_alone() {
        let v: serde_json::Value = serde_json::from_str(&strip_jsonc(JSONC)).unwrap();
        assert_eq!(v["url"], "http://x//y");
        assert_eq!(v["list"], serde_json::json!([1, 2]));
    }

    #[test]
    fn a_block_comment_is_whitespace_so_it_cannot_fuse_tokens() {
        // `1/*c*/2` must not become the valid number 12.
        let stripped = strip_jsonc(r#"{"value": 1/*c*/2}"#);
        assert!(
            serde_json::from_str::<serde_json::Value>(&stripped).is_err(),
            "fused into valid JSON: {stripped}"
        );
        // A comment between tokens that need no separator still parses.
        let ok = strip_jsonc("{\"a\": 1, /* c */ \"b\": 2}");
        assert!(serde_json::from_str::<serde_json::Value>(&ok).is_ok());
    }

    #[test]
    fn a_multiline_block_comment_keeps_its_newlines() {
        let stripped = strip_jsonc("{\"a\":\n/* one\ntwo\nthree */ 1}");
        assert_eq!(stripped.matches('\n').count(), 3, "{stripped:?}");
    }
}
