// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Tooling and version management analyzer

use super::{AnalysisResult, Analyzer, Finding, Severity};
use crate::config::Config;
use crate::error::Result;
use std::path::Path;
use tracing::debug;

/// mise manifests that pin tool versions (accepted in place of `.tool-versions`).
const MISE_MANIFESTS: &[&str] = &["mise.toml", ".mise.toml"];

/// The tool names a parseable mise manifest pins in a non-empty `[tools]`
/// table (`None` if it is unparseable or pins nothing). A backend prefix
/// (`asdf:just`, `core:rust`) is dropped, so the name is the tool itself.
fn mise_tool_names(p: &Path) -> Option<Vec<String>> {
    let table = std::fs::read_to_string(p)
        .ok()?
        .parse::<toml::Table>()
        .ok()?;
    let tools = table.get("tools")?.as_table()?;
    if tools.is_empty() {
        return None;
    }
    Some(
        tools
            .keys()
            .map(|k| k.rsplit(':').next().unwrap_or(k).to_string())
            .collect(),
    )
}

/// Tooling and version management analyzer
pub struct ToolingAnalyzer;

impl Default for ToolingAnalyzer {
    fn default() -> Self {
        Self
    }
}

impl Analyzer for ToolingAnalyzer {
    fn name(&self) -> &str {
        "Tooling & Version Management"
    }

    fn analyze(&self, path: &Path, _config: &Config) -> Result<AnalysisResult> {
        let mut result = AnalysisResult::new();
        let start = std::time::Instant::now();

        // Check for a tool-version pin: .tool-versions (asdf/mise) or mise.toml
        // (mise's own manifest, which the estate uses). Either satisfies TOOL-001.
        let tool_versions = path.join(".tool-versions");
        let mise_manifest = MISE_MANIFESTS
            .iter()
            .map(|n| path.join(n))
            .find_map(|p| mise_tool_names(&p).map(|names| (p, names)));
        if !tool_versions.exists() && mise_manifest.is_some() {
            debug!("Found mise manifest instead of .tool-versions");
            // Same project-relevant tools as the .tool-versions branch: a
            // manifest pinning only unrelated tools is not a clean pass.
            if let Some((manifest, names)) = &mise_manifest {
                self.check_project_tools(
                    |t| names.iter().any(|n| n == t),
                    manifest,
                    "the mise manifest",
                    &mut result,
                );
            }
        } else if !tool_versions.exists() {
            result.add(
                Finding::new(
                    "TOOL-001",
                    "Missing tool-version pin",
                    Severity::Error,
                    "Neither .tool-versions nor a mise.toml with a non-empty [tools] table found for asdf/mise version management.",
                )
                .with_file(path.join("mise.toml"))
                .with_suggestion("Create mise.toml (or .tool-versions) with required tool versions (rust, just, etc.)"),
            );
        } else {
            debug!("Found .tool-versions");

            // Validate .tool-versions content
            if let Ok(content) = std::fs::read_to_string(&tool_versions) {
                self.validate_tool_versions(&content, &tool_versions, &mut result);
            }
        }

        // Check for editor config
        let editorconfig = path.join(".editorconfig");
        if !editorconfig.exists() {
            result.add(
                Finding::new(
                    "TOOL-002",
                    "Missing .editorconfig",
                    Severity::Warning,
                    ".editorconfig not found for consistent code formatting.",
                )
                .with_suggestion("Add .editorconfig for consistent editor settings"),
            );
        } else {
            debug!("Found .editorconfig");
        }

        // Check for CI workflows
        let workflows_dir = path.join(".github/workflows");
        if !workflows_dir.exists() {
            result.add(
                Finding::new(
                    "TOOL-003",
                    "Missing CI workflows",
                    Severity::Error,
                    ".github/workflows/ directory not found.",
                )
                .with_suggestion("Add CI/CD workflows for automated testing and quality checks"),
            );
        } else {
            debug!("Found .github/workflows/");

            // Count workflows
            if let Ok(entries) = std::fs::read_dir(&workflows_dir) {
                let workflow_count = entries
                    .filter_map(|e| e.ok())
                    .filter(|e| {
                        e.path()
                            .extension()
                            .is_some_and(|ext| ext == "yml" || ext == "yaml")
                    })
                    .count();

                if workflow_count < 10 {
                    result.add(
                        Finding::new(
                            "TOOL-004",
                            "Few CI workflows",
                            Severity::Warning,
                            &format!("Only {} workflows found. V1-ready repos typically have 15-35 workflows.", workflow_count),
                        )
                        .with_file(workflows_dir.clone())
                        .with_suggestion("Add comprehensive CI workflows (CodeQL, security scanning, tests, linting)"),
                    );
                } else {
                    debug!("Found {} workflows", workflow_count);
                }
            }
        }

        result.duration_ms = start.elapsed().as_millis() as u64;
        Ok(result)
    }

    fn fix(&self, _path: &Path, _config: &Config, _findings: &[Finding]) -> Result<Vec<String>> {
        Ok(vec![
            "Tooling files should be manually created.".to_string(),
            "Use templates from hyperpolymath repos.".to_string(),
        ])
    }
}

impl ToolingAnalyzer {
    /// Validate .tool-versions content
    fn validate_tool_versions(&self, content: &str, file_path: &Path, result: &mut AnalysisResult) {
        self.check_project_tools(
            |t| content.contains(&format!("{t} ")),
            file_path,
            ".tool-versions",
            result,
        );

        // Check for empty or very short file
        if content.trim().is_empty() {
            result.add(
                Finding::new(
                    "TOOL-007",
                    "Empty .tool-versions",
                    Severity::Error,
                    ".tool-versions file is empty.",
                )
                .with_file(file_path.to_path_buf())
                .with_suggestion("Add tool versions (e.g., 'rust 1.83.0')"),
            );
        }
    }

    /// The project-relevant tools a pin file (`.tool-versions` or a mise
    /// manifest) must carry: rust (TOOL-005) and just (TOOL-006) for a Rust
    /// project. `pins(t)` says whether the pin file pins tool `t`; `label`
    /// names the pin file in the finding.
    fn check_project_tools(
        &self,
        pins: impl Fn(&str) -> bool,
        file_path: &Path,
        label: &str,
        result: &mut AnalysisResult,
    ) {
        let is_rust = file_path
            .parent()
            .map(|p| p.join("Cargo.toml").exists())
            .unwrap_or(false);

        // Check for rust version if Rust project
        if is_rust && !pins("rust") {
            result.add(
                Finding::new(
                    "TOOL-005",
                    &format!("Missing rust version in {label}"),
                    Severity::Warning,
                    &format!("Rust project should specify rust version in {label}."),
                )
                .with_file(file_path.to_path_buf())
                .with_suggestion(&format!("Add rust to {label}")),
            );
        }

        // Check for just (task runner)
        if is_rust && !pins("just") {
            result.add(
                Finding::new(
                    "TOOL-006",
                    &format!("Missing just version in {label}"),
                    Severity::Info,
                    &format!("Consider adding the 'just' task runner to {label}."),
                )
                .with_file(file_path.to_path_buf())
                .with_suggestion(&format!("Add just to {label} if using justfile")),
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn ids(dir: &Path) -> Vec<String> {
        ToolingAnalyzer
            .analyze(dir, &Config::default())
            .unwrap()
            .findings
            .into_iter()
            .map(|f| f.id)
            .collect()
    }

    #[test]
    fn tool_001_requires_mise_manifest_to_pin_tools() {
        for (body, ok) in [
            ("", false),
            ("not [valid toml", false),
            ("[env]\nA = \"1\"\n", false),
            ("[tools]\n", false),
            ("[tools]\nrust = \"1.83\"\n", true),
        ] {
            for name in ["mise.toml", ".mise.toml"] {
                let d = TempDir::new().unwrap();
                std::fs::write(d.path().join(name), body).unwrap();
                assert_eq!(
                    !ids(d.path()).contains(&"TOOL-001".to_string()),
                    ok,
                    "{name}: {body:?}"
                );
            }
        }
    }

    #[test]
    fn a_mise_manifest_must_pin_the_tools_the_project_needs() {
        let write = |d: &TempDir, mise: &str| {
            std::fs::write(d.path().join("Cargo.toml"), "[package]\nname = \"x\"\n").unwrap();
            std::fs::write(d.path().join("mise.toml"), mise).unwrap();
        };
        // Unrelated tools only: a Rust repo gets the same findings as a
        // `.tool-versions` that lacks rust and just.
        let d = TempDir::new().unwrap();
        write(&d, "[tools]\nnode = \"22\"\n");
        let got = ids(d.path());
        assert!(got.contains(&"TOOL-005".to_string()), "{got:?}");
        assert!(got.contains(&"TOOL-006".to_string()), "{got:?}");
        // rust without just: only the just note.
        let d = TempDir::new().unwrap();
        write(&d, "[tools]\nrust = \"1.90\"\n");
        let got = ids(d.path());
        assert!(!got.contains(&"TOOL-005".to_string()), "{got:?}");
        assert!(got.contains(&"TOOL-006".to_string()), "{got:?}");
        // Both pinned (also behind a backend prefix): clean.
        let d = TempDir::new().unwrap();
        write(&d, "[tools]\nrust = \"1.90\"\n\"asdf:just\" = \"1.40\"\n");
        let got = ids(d.path());
        assert!(
            !got.iter().any(|i| i == "TOOL-005" || i == "TOOL-006"),
            "{got:?}"
        );
        // A non-Rust repo is not asked for rust or just.
        let d = TempDir::new().unwrap();
        std::fs::write(d.path().join("mise.toml"), "[tools]\nnode = \"22\"\n").unwrap();
        let got = ids(d.path());
        assert!(
            !got.iter().any(|i| i == "TOOL-005" || i == "TOOL-006"),
            "{got:?}"
        );
    }

    #[test]
    fn tool_001_fires_when_neither_pin_exists_and_names_mise_toml() {
        let d = TempDir::new().unwrap();
        let f = ToolingAnalyzer
            .analyze(d.path(), &Config::default())
            .unwrap()
            .findings
            .into_iter()
            .find(|f| f.id == "TOOL-001")
            .expect("TOOL-001 on a repo with no pin");
        assert_eq!(
            f.file.as_deref(),
            Some(d.path().join("mise.toml").as_path())
        );
    }

    #[test]
    fn mise_toml_satisfies_tool_001() {
        for name in ["mise.toml", ".mise.toml"] {
            let d = TempDir::new().unwrap();
            std::fs::write(d.path().join(name), "[tools]\nrust = \"1.90\"\n").unwrap();
            assert!(!ids(d.path()).contains(&"TOOL-001".to_string()), "{name}");
        }
    }

    #[test]
    fn tool_versions_still_satisfies_tool_001() {
        let d = TempDir::new().unwrap();
        std::fs::write(d.path().join(".tool-versions"), "rust 1.90.0\n").unwrap();
        assert!(!ids(d.path()).contains(&"TOOL-001".to_string()));
    }
}
