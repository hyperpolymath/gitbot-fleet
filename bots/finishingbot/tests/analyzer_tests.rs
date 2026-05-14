// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Comprehensive tests for finishing-bot analyzers

use finishing_bot::analyzers::{
    claims::ClaimsAnalyzer, license::LicenseAnalyzer, placeholder::PlaceholderAnalyzer,
    release::ReleaseAnalyzer, scm_files::ScmFilesAnalyzer, testing::TestingAnalyzer,
    tooling::ToolingAnalyzer, v1_readiness::V1ReadinessAnalyzer, Analyzer, AuditResult, Severity,
};
use finishing_bot::config::Config;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

// Re-export for serialization tests
extern crate serde_json;

/// Create a temporary directory representing a test repo
fn setup_test_repo() -> (TempDir, PathBuf) {
    let temp = TempDir::new().expect("Create temp dir");
    let path = temp.path().to_path_buf();
    (temp, path)
}

/// Return default configuration
fn default_config() -> Config {
    Config::default()
}

// ============================================================================
// 2.1 License analyzer tests
// ============================================================================
mod license_analyzer {
    use super::*;

    #[test]
    fn test_repo_with_valid_license_no_findings() {
        let (_temp, path) = setup_test_repo();
        fs::write(
            path.join("LICENSE"),
            "MIT License\n\nCopyright (c) 2025\n\nPermission is hereby granted, free of charge, to any person obtaining a copy...\n",
        )
        .unwrap();

        let analyzer = LicenseAnalyzer::default();
        let mut config = default_config();
        config.licenses.require_spdx_headers = false;
        let result = analyzer.analyze(&path, &config).unwrap();

        let license_file_errors: Vec<_> = result
            .findings
            .iter()
            .filter(|f| f.id == "LIC-001")
            .collect();
        assert!(
            license_file_errors.is_empty(),
            "Should not report missing LICENSE when file exists"
        );
    }

    #[test]
    fn test_repo_with_no_license_error() {
        let (_temp, path) = setup_test_repo();
        fs::write(path.join("main.rs"), "fn main() {}\n").unwrap();

        let analyzer = LicenseAnalyzer::default();
        let mut config = default_config();
        config.licenses.require_spdx_headers = false;
        let result = analyzer.analyze(&path, &config).unwrap();

        assert!(
            result.has_errors(),
            "Should report error when LICENSE is missing"
        );
        assert!(
            result.findings.iter().any(|f| f.id == "LIC-001"),
            "Finding ID should be LIC-001 for missing LICENSE"
        );
        assert!(
            result
                .findings
                .iter()
                .any(|f| f.severity == Severity::Error),
            "Missing LICENSE should be an Error severity"
        );
    }

    #[test]
    fn test_agpl_license_detection() {
        let (_temp, path) = setup_test_repo();
        fs::write(
            path.join("LICENSE"),
            "GNU Affero General Public License\nVersion 3\n\nPreamble...\n",
        )
        .unwrap();

        let analyzer = LicenseAnalyzer::default();
        let mut config = default_config();
        config.licenses.strict = true;
        config.licenses.require_spdx_headers = false;
        // AGPL is not in allowed list (we use PMPL now)
        let result = analyzer.analyze(&path, &config).unwrap();

        // In strict mode, AGPL should be flagged as disallowed
        let disallowed: Vec<_> = result
            .findings
            .iter()
            .filter(|f| f.id == "LIC-003")
            .collect();
        assert!(
            !disallowed.is_empty(),
            "AGPL should be flagged as disallowed in strict mode"
        );
    }

    #[test]
    fn test_missing_spdx_header_warning() {
        let (_temp, path) = setup_test_repo();
        fs::create_dir_all(path.join("src")).unwrap();
        fs::write(
            path.join("src/main.rs"),
            "// A file without SPDX header\nfn main() {\n    println!(\"Hello\");\n}\n",
        )
        .unwrap();

        let mut config = default_config();
        config.licenses.require_spdx_headers = true;

        let analyzer = LicenseAnalyzer::default();
        let result = analyzer.analyze(&path, &config).unwrap();

        let spdx_findings: Vec<_> = result
            .findings
            .iter()
            .filter(|f| f.id.starts_with("LIC-002"))
            .collect();
        assert!(
            !spdx_findings.is_empty(),
            "Should find missing SPDX header"
        );
        assert_eq!(
            spdx_findings[0].severity,
            Severity::Warning,
            "Missing SPDX header should be a Warning"
        );
    }

    #[test]
    fn test_valid_spdx_header_no_findings() {
        let (_temp, path) = setup_test_repo();
        fs::create_dir_all(path.join("src")).unwrap();
        fs::write(
            path.join("src/main.rs"),
            "// SPDX-License-Identifier: PMPL-1.0-or-later\nfn main() {}\n",
        )
        .unwrap();

        let mut config = default_config();
        config.licenses.require_spdx_headers = true;

        let analyzer = LicenseAnalyzer::default();
        let result = analyzer.analyze(&path, &config).unwrap();

        let spdx_findings: Vec<_> = result
            .findings
            .iter()
            .filter(|f| {
                f.id.starts_with("LIC-002")
                    && f.file.as_ref().map_or(false, |p| p.ends_with("main.rs"))
            })
            .collect();
        assert!(
            spdx_findings.is_empty(),
            "Should not flag file with valid SPDX header"
        );
    }
}

// ============================================================================
// 2.2 Placeholder analyzer tests
// ============================================================================
mod placeholder_analyzer {
    use super::*;

    #[test]
    fn test_detect_todo_fixme_xxx() {
        let (_temp, path) = setup_test_repo();
        fs::write(
            path.join("code.rs"),
            "// TODO: implement this\n// FIXME: broken\n// XXX: review\nfn main() {}\n",
        )
        .unwrap();

        let analyzer = PlaceholderAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        assert!(
            result.findings.iter().any(|f| f.message.contains("TODO")),
            "Should detect TODO placeholder"
        );
        assert!(
            result.findings.iter().any(|f| f.message.contains("FIXME")),
            "Should detect FIXME placeholder"
        );
        assert!(
            result.findings.iter().any(|f| f.message.contains("XXX")),
            "Should detect XXX placeholder"
        );
    }

    #[test]
    fn test_clean_files_no_findings() {
        let (_temp, path) = setup_test_repo();
        fs::write(
            path.join("clean.rs"),
            "/// Clean documented function\nfn clean_function() -> i32 {\n    42\n}\n",
        )
        .unwrap();

        let analyzer = PlaceholderAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        let file_findings: Vec<_> = result
            .findings
            .iter()
            .filter(|f| {
                f.file
                    .as_ref()
                    .map_or(false, |p| p.ends_with("clean.rs"))
            })
            .collect();
        assert!(
            file_findings.is_empty(),
            "Clean file should have no placeholder findings"
        );
    }

    #[test]
    fn test_placeholder_in_different_file_types() {
        let (_temp, path) = setup_test_repo();
        fs::write(path.join("code.rs"), "// TODO: rust task\n").unwrap();
        fs::write(path.join("notes.md"), "TODO: markdown task\n").unwrap();
        fs::write(
            path.join("Cargo.toml"),
            "[package]\nname = \"test\"\n# TODO: add more fields\n",
        )
        .unwrap();

        let analyzer = PlaceholderAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        let rs_findings = result.findings.iter().filter(|f| {
            f.file
                .as_ref()
                .map_or(false, |p| p.ends_with("code.rs"))
        });
        let md_findings = result.findings.iter().filter(|f| {
            f.file
                .as_ref()
                .map_or(false, |p| p.ends_with("notes.md"))
        });
        let toml_findings = result.findings.iter().filter(|f| {
            f.file
                .as_ref()
                .map_or(false, |p| p.ends_with("Cargo.toml"))
        });

        assert!(rs_findings.count() > 0, "Should detect in .rs files");
        assert!(md_findings.count() > 0, "Should detect in .md files");
        assert!(toml_findings.count() > 0, "Should detect in .toml files");
    }

    #[test]
    fn test_placeholder_severity_levels() {
        let (_temp, path) = setup_test_repo();
        fs::write(path.join("code.rs"), "// TODO: something\n").unwrap();

        let analyzer = PlaceholderAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        // In Flag mode, placeholders should be Warning severity
        let todo_findings: Vec<_> = result
            .findings
            .iter()
            .filter(|f| f.message.contains("TODO"))
            .collect();
        assert!(!todo_findings.is_empty());
        assert_eq!(
            todo_findings[0].severity,
            Severity::Warning,
            "Placeholders in Flag mode should be Warning severity"
        );
    }

    #[test]
    fn test_max_allowed_placeholders_no_error() {
        let (_temp, path) = setup_test_repo();
        fs::write(path.join("code.rs"), "// TODO: one\n// TODO: two\nfn main() {}\n").unwrap();

        let mut config = default_config();
        config.placeholders.max_allowed = 5;

        let analyzer = PlaceholderAnalyzer::default();
        let result = analyzer.analyze(&path, &config).unwrap();

        // With max_allowed=5, 2 TODOs should not trigger the "too many" error
        let too_many_errors: Vec<_> = result
            .findings
            .iter()
            .filter(|f| f.id == "PH-100")
            .collect();
        assert!(
            too_many_errors.is_empty(),
            "Should not error when within max_allowed"
        );
    }
}

// ============================================================================
// 2.3 Claims analyzer tests
// ============================================================================
mod claims_analyzer {
    use super::*;

    #[test]
    fn test_missing_readme_error() {
        let (_temp, path) = setup_test_repo();
        fs::write(path.join("main.rs"), "fn main() {}\n").unwrap();

        let mut config = default_config();
        config.claims.require_readme = true;

        let analyzer = ClaimsAnalyzer::default();
        let result = analyzer.analyze(&path, &config).unwrap();

        assert!(
            result.findings.iter().any(|f| f.id == "CLM-001"),
            "Should report missing README"
        );
        assert!(
            result
                .findings
                .iter()
                .any(|f| f.id == "CLM-001" && f.severity == Severity::Error),
            "Missing README should be Error severity"
        );
    }

    #[test]
    fn test_complete_readme_no_errors() {
        let (_temp, path) = setup_test_repo();
        let readme_content = "# My Project\n\nA great project that does many things.\n\n\
            ## Installation\n\nRun `cargo install my-project`\n\n\
            ## Usage\n\nUse it wisely with care and diligence.\n\n\
            ## License\n\nPMPL-1.0-or-later\n\n\
            Additional text to make this README substantial enough.\n\
            More words to reach the minimum threshold for a good README.\n\
            Even more content here describing what the project does.\n";
        fs::write(path.join("README.md"), readme_content).unwrap();

        let mut config = default_config();
        config.claims.require_readme = true;

        let analyzer = ClaimsAnalyzer::default();
        let result = analyzer.analyze(&path, &config).unwrap();

        let readme_errors: Vec<_> = result
            .findings
            .iter()
            .filter(|f| f.id == "CLM-001")
            .collect();
        assert!(
            readme_errors.is_empty(),
            "Should not report missing README when it exists"
        );

        let sparse_findings: Vec<_> = result
            .findings
            .iter()
            .filter(|f| f.id == "CLM-002")
            .collect();
        assert!(
            sparse_findings.is_empty(),
            "README with enough content should not be flagged as sparse"
        );
    }

    #[test]
    fn test_incomplete_features_in_docs() {
        let (_temp, path) = setup_test_repo();
        let readme = "# Project\n\n\
            More text here to fill it up enough words.\n\
            And even more text to pass minimum checks.\n\
            Installation is straightforward. Usage is simple. License is PMPL.\n\n\
            ## Features\n\n\
            - [x] Feature A\n\
            - [ ] Feature B (incomplete)\n\
            - [ ] Feature C (incomplete)\n";
        fs::write(path.join("README.md"), readme).unwrap();

        let mut config = default_config();
        config.claims.verify_docs = true;

        let analyzer = ClaimsAnalyzer::default();
        let result = analyzer.analyze(&path, &config).unwrap();

        assert!(
            result.findings.iter().any(|f| f.id == "CLM-007"),
            "Should detect incomplete feature markers in documentation"
        );
    }

    #[test]
    fn test_missing_changelog() {
        let (_temp, path) = setup_test_repo();
        fs::write(path.join("README.md"), "# Project\n").unwrap();

        let mut config = default_config();
        config.claims.require_changelog = true;

        let analyzer = ClaimsAnalyzer::default();
        let result = analyzer.analyze(&path, &config).unwrap();

        assert!(
            result.findings.iter().any(|f| f.id == "CLM-004"),
            "Should report missing CHANGELOG"
        );
    }

    #[test]
    fn test_consistent_claims_no_findings() {
        let (_temp, path) = setup_test_repo();
        let readme = "# Project\n\n\
            A project with plenty of content for testing purposes.\n\
            It has installation instructions, usage docs, and license info.\n\n\
            ## Installation\n\nRun the installer.\n\n\
            ## Usage\n\nUse it.\n\n\
            ## License\n\nPMPL\n";
        fs::write(path.join("README.md"), readme).unwrap();
        fs::write(
            path.join("CHANGELOG.md"),
            "# Changelog\n\n## 0.1.0\n- Initial release\n",
        )
        .unwrap();
        fs::create_dir_all(path.join(".github/workflows")).unwrap();
        fs::write(
            path.join(".github/workflows/test.yml"),
            "name: test\non: push\njobs: {}\n",
        )
        .unwrap();
        fs::create_dir_all(path.join("tests")).unwrap();
        fs::write(path.join("tests/test_main.rs"), "#[test]\nfn it_works() { assert!(true); }\n").unwrap();

        let mut config = default_config();
        config.claims.require_readme = true;
        config.claims.require_changelog = true;

        let analyzer = ClaimsAnalyzer::default();
        let result = analyzer.analyze(&path, &config).unwrap();

        // No critical errors for a well-structured repo
        let errors: Vec<_> = result
            .findings
            .iter()
            .filter(|f| f.severity == Severity::Error)
            .collect();
        assert!(
            errors.is_empty(),
            "Well-structured repo should have no Error findings, got: {:?}",
            errors
                .iter()
                .map(|f| format!("{}: {}", f.id, f.message))
                .collect::<Vec<_>>()
        );
    }
}

// ============================================================================
// 2.4 Release analyzer tests
// ============================================================================
mod release_analyzer {
    use super::*;

    #[test]
    fn test_missing_changelog_and_security() {
        let (_temp, path) = setup_test_repo();
        // Empty repo with no security file
        fs::write(path.join("main.rs"), "fn main() {}\n").unwrap();

        let analyzer = ReleaseAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        assert!(
            result.findings.iter().any(|f| f.id == "REL-004"),
            "Should report missing SECURITY.md"
        );
    }

    #[test]
    fn test_version_mismatch() {
        let (_temp, path) = setup_test_repo();
        fs::write(
            path.join("Cargo.toml"),
            "[package]\nname = \"test\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        fs::write(path.join("VERSION"), "0.2.0\n").unwrap();

        let analyzer = ReleaseAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        assert!(
            result.findings.iter().any(|f| f.id == "REL-001"),
            "Should detect version inconsistency between Cargo.toml and VERSION file"
        );
        assert!(
            result
                .findings
                .iter()
                .any(|f| f.id == "REL-001" && f.severity == Severity::Error),
            "Version mismatch should be Error severity"
        );
    }

    #[test]
    fn test_debug_statement_detection() {
        let (_temp, path) = setup_test_repo();
        fs::write(
            path.join("main.js"),
            "console.log(\"debug\");\ndebugger;\nconsole.log(\"production\");\n",
        )
        .unwrap();

        let analyzer = ReleaseAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        assert!(
            result
                .findings
                .iter()
                .any(|f| f.id == "REL-003" && f.message.contains("console.log")),
            "Should detect debug statements like console.log"
        );
    }

    #[test]
    fn test_sensitive_data_detection() {
        let (_temp, path) = setup_test_repo();
        fs::write(
            path.join("config.yml"),
            "api_key = \"abcdefghijklmnopqrstuvwxyz1234567890\"\n",
        )
        .unwrap();

        let analyzer = ReleaseAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        let sensitive_findings: Vec<_> = result
            .findings
            .iter()
            .filter(|f| f.id == "REL-008")
            .collect();
        assert!(
            !sensitive_findings.is_empty(),
            "Should detect sensitive data patterns"
        );
        assert_eq!(
            sensitive_findings[0].severity,
            Severity::Error,
            "Sensitive data should be Error severity"
        );
    }

    #[test]
    fn test_proper_release_setup_minimal_findings() {
        let (_temp, path) = setup_test_repo();
        fs::write(
            path.join("Cargo.toml"),
            "[package]\nname = \"test\"\nversion = \"1.0.0\"\n\n[profile.release]\nlto = true\n",
        )
        .unwrap();
        fs::write(
            path.join("SECURITY.md"),
            "# Security Policy\n\nReport vulnerabilities to security@example.com\n",
        )
        .unwrap();
        fs::create_dir_all(path.join(".github/workflows")).unwrap();
        fs::write(
            path.join(".github/workflows/release.yml"),
            "name: release\non: push\njobs: {}\n",
        )
        .unwrap();

        let analyzer = ReleaseAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        // Should not have security file or release profile errors
        let critical_findings: Vec<_> = result
            .findings
            .iter()
            .filter(|f| {
                (f.id == "REL-004" || f.id == "REL-009" || f.id == "REL-010")
                    && f.severity == Severity::Error
            })
            .collect();
        assert!(
            critical_findings.is_empty(),
            "Well-configured release should not have critical findings"
        );
    }
}

// ============================================================================
// 2.5 SCM files analyzer tests
// ============================================================================
mod scm_files_analyzer {
    use super::*;

    #[test]
    fn test_missing_state_scm_error() {
        let (_temp, path) = setup_test_repo();
        // No .machine_readable/ directory at all

        let analyzer = ScmFilesAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        assert!(
            result.findings.iter().any(|f| f.id == "SCM-001-STATE.scm"),
            "Should report missing STATE.scm"
        );
        assert!(
            result
                .findings
                .iter()
                .any(|f| f.id == "SCM-001-STATE.scm" && f.severity == Severity::Error),
            "Missing STATE.scm should be Error severity"
        );
    }

    #[test]
    fn test_state_scm_in_root_warns_wrong_location() {
        let (_temp, path) = setup_test_repo();
        fs::write(
            path.join("STATE.scm"),
            ";; SPDX-License-Identifier: PMPL-1.0-or-later\n\
             (define state\n  '((metadata (version . \"0.1.0\"))\n\
             (current-position (phase . \"mvp\"))))\n\
             ;; padding content to meet minimum size requirement\n\
             ;; more content here to make the file substantial enough\n\
             ;; and even more lines of content for the validator\n",
        )
        .unwrap();

        let analyzer = ScmFilesAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        assert!(
            result
                .findings
                .iter()
                .any(|f| f.id == "SCM-005-STATE.scm"),
            "Should warn about STATE.scm in root instead of .machine_readable/"
        );
        assert!(
            result
                .findings
                .iter()
                .any(|f| f.id == "SCM-005-STATE.scm" && f.severity == Severity::Warning),
            "Wrong location should be Warning severity"
        );
    }

    #[test]
    fn test_all_scm_files_present_in_machine_readable() {
        let (_temp, path) = setup_test_repo();
        let mr_dir = path.join(".machine_readable");
        fs::create_dir_all(&mr_dir).unwrap();

        let scm_files = [
            ("STATE.scm", ";; SPDX-License-Identifier: PMPL-1.0-or-later\n(define state '((metadata (version . \"0.1.0\"))(current-position (phase . \"mvp\"))))\n;; padding\n;; more padding to meet minimum content\n;; and more\n;; and more\n;; and more\n;; and more\n"),
            ("META.scm", ";; SPDX-License-Identifier: PMPL-1.0-or-later\n(define meta '((architecture-decisions ())(development-practices ())))\n;; padding\n;; more padding\n;; and more\n;; and more\n;; and more\n"),
            ("ECOSYSTEM.scm", ";; SPDX-License-Identifier: PMPL-1.0-or-later\n(define ecosystem '((position-in-ecosystem \"tools\")(related-projects ())))\n;; padding\n;; more padding\n;; and more\n;; and more\n"),
            ("PLAYBOOK.scm", ";; SPDX-License-Identifier: PMPL-1.0-or-later\n(define playbook '((operations ())))\n;; padding\n;; more padding\n;; and more\n;; and more content to fill\n;; even more filler for length\n"),
            ("AGENTIC.scm", ";; SPDX-License-Identifier: PMPL-1.0-or-later\n(define agentic '((ai-integration ())))\n;; padding\n;; more padding\n;; and more\n;; and more content to fill\n;; even more filler for length\n"),
            ("NEUROSYM.scm", ";; SPDX-License-Identifier: PMPL-1.0-or-later\n(define neurosym '((neurosymbolic ())))\n;; padding\n;; more padding\n;; and more\n;; and more content to fill\n;; even more filler for length\n"),
        ];

        for (name, content) in &scm_files {
            fs::write(mr_dir.join(name), content).unwrap();
        }

        let analyzer = ScmFilesAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        let missing_errors: Vec<_> = result
            .findings
            .iter()
            .filter(|f| f.id.starts_with("SCM-001"))
            .collect();
        assert!(
            missing_errors.is_empty(),
            "Should not report missing SCM files when all are present in .machine_readable/"
        );
        let wrong_location: Vec<_> = result
            .findings
            .iter()
            .filter(|f| f.id.starts_with("SCM-005"))
            .collect();
        assert!(
            wrong_location.is_empty(),
            "Should not report wrong location for files in .machine_readable/"
        );
    }

    #[test]
    fn test_scm_file_missing_spdx_header() {
        let (_temp, path) = setup_test_repo();
        let mr_dir = path.join(".machine_readable");
        fs::create_dir_all(&mr_dir).unwrap();
        // STATE.scm without SPDX header
        fs::write(
            mr_dir.join("STATE.scm"),
            "(define state '((metadata (version . \"0.1.0\"))(current-position (phase . \"mvp\"))))\n\
             ;; padding to meet length requirements for the validator\n\
             ;; more padding content here for the minimum length check\n\
             ;; and even more padding to ensure we pass the length check\n",
        )
        .unwrap();

        let analyzer = ScmFilesAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        assert!(
            result
                .findings
                .iter()
                .any(|f| f.id == "SCM-002-STATE.scm"),
            "Should warn about missing SPDX header in STATE.scm"
        );
    }
}

// ============================================================================
// 2.6 Testing analyzer tests
// ============================================================================
mod testing_analyzer {
    use super::*;

    #[test]
    fn test_repo_with_no_test_files_error() {
        let (_temp, path) = setup_test_repo();
        fs::write(
            path.join("Cargo.toml"),
            "[package]\nname = \"test\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        fs::create_dir_all(path.join("src")).unwrap();
        fs::write(path.join("src/main.rs"), "fn main() {}\n").unwrap();

        let analyzer = TestingAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        // Should flag missing benchmarks, fuzzing, etc.
        let error_findings: Vec<_> = result
            .findings
            .iter()
            .filter(|f| f.severity == Severity::Error)
            .collect();
        assert!(
            !error_findings.is_empty(),
            "Should report errors for missing testing infrastructure"
        );
    }

    #[test]
    fn test_repo_with_benchmarks_no_bench_error() {
        let (_temp, path) = setup_test_repo();
        fs::write(
            path.join("Cargo.toml"),
            "[package]\nname = \"test\"\nversion = \"0.1.0\"\n\n[dev-dependencies]\ncriterion = \"0.5\"\n",
        )
        .unwrap();
        fs::create_dir_all(path.join("benches")).unwrap();
        fs::write(
            path.join("benches/my_bench.rs"),
            "use criterion::*;\nfn bench(c: &mut Criterion) {}\ncriterion_main!(bench);\n",
        )
        .unwrap();

        let analyzer = TestingAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        let bench_errors: Vec<_> = result
            .findings
            .iter()
            .filter(|f| f.id == "TEST-RU-001" || f.id == "TEST-RU-002")
            .collect();
        assert!(
            bench_errors.is_empty(),
            "Should not report bench errors when benches/ has .rs files"
        );
    }

    #[test]
    fn test_well_tested_repo_note_level() {
        let (_temp, path) = setup_test_repo();
        fs::write(
            path.join("Cargo.toml"),
            "[package]\nname = \"test\"\nversion = \"0.1.0\"\n\n[dev-dependencies]\ncriterion = \"0.5\"\n",
        )
        .unwrap();
        fs::create_dir_all(path.join("benches")).unwrap();
        fs::write(path.join("benches/bench.rs"), "fn main() {}\n").unwrap();
        fs::create_dir_all(path.join("fuzz")).unwrap();
        fs::write(path.join("fuzz/fuzz_target.rs"), "fn main() {}\n").unwrap();
        fs::create_dir_all(path.join(".github/workflows")).unwrap();
        fs::write(
            path.join(".github/workflows/stress-test.yml"),
            "name: stress\non: push\njobs: {}\n",
        )
        .unwrap();

        let analyzer = TestingAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        // With benches, fuzz, and stress test -- no major errors from testing infra
        let testing_errors: Vec<_> = result
            .findings
            .iter()
            .filter(|f| {
                f.severity == Severity::Error
                    && (f.id == "TEST-RU-001"
                        || f.id == "TEST-RU-003"
                        || f.id == "TEST-RU-005")
            })
            .collect();
        assert!(
            testing_errors.is_empty(),
            "Well-tested repo should have no testing infrastructure errors, got: {:?}",
            testing_errors
                .iter()
                .map(|f| &f.id)
                .collect::<Vec<_>>()
        );
    }
}

// ============================================================================
// 2.7 Tooling analyzer tests
// ============================================================================
mod tooling_analyzer {
    use super::*;

    #[test]
    fn test_missing_editorconfig() {
        let (_temp, path) = setup_test_repo();
        // No .editorconfig

        let analyzer = ToolingAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        assert!(
            result.findings.iter().any(|f| f.id == "TOOL-002"),
            "Should report missing .editorconfig"
        );
        assert!(
            result
                .findings
                .iter()
                .any(|f| f.id == "TOOL-002" && f.severity == Severity::Warning),
            "Missing .editorconfig should be Warning"
        );
    }

    #[test]
    fn test_missing_ci_workflows() {
        let (_temp, path) = setup_test_repo();
        // No .github/workflows/

        let analyzer = ToolingAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        assert!(
            result.findings.iter().any(|f| f.id == "TOOL-003"),
            "Should report missing CI workflows directory"
        );
        assert!(
            result
                .findings
                .iter()
                .any(|f| f.id == "TOOL-003" && f.severity == Severity::Error),
            "Missing CI workflows should be Error"
        );
    }

    #[test]
    fn test_complete_tooling_no_critical_findings() {
        let (_temp, path) = setup_test_repo();
        fs::write(
            path.join("Cargo.toml"),
            "[package]\nname = \"test\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        fs::write(path.join(".tool-versions"), "rust nightly\njust 1.30.0\n").unwrap();
        fs::write(path.join(".editorconfig"), "root = true\n").unwrap();
        fs::create_dir_all(path.join(".github/workflows")).unwrap();
        // Create enough workflow files (>= 10)
        for i in 0..12 {
            fs::write(
                path.join(format!(".github/workflows/workflow-{}.yml", i)),
                format!("name: workflow-{}\non: push\njobs: {{}}\n", i),
            )
            .unwrap();
        }

        let analyzer = ToolingAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        let critical_findings: Vec<_> = result
            .findings
            .iter()
            .filter(|f| f.severity == Severity::Error)
            .collect();
        assert!(
            critical_findings.is_empty(),
            "Complete tooling should have no Error findings, got: {:?}",
            critical_findings
                .iter()
                .map(|f| format!("{}: {}", f.id, f.message))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_missing_tool_versions() {
        let (_temp, path) = setup_test_repo();

        let analyzer = ToolingAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        assert!(
            result.findings.iter().any(|f| f.id == "TOOL-001"),
            "Should report missing .tool-versions"
        );
    }
}

// ============================================================================
// 2.8 V1 readiness analyzer tests
// ============================================================================
mod v1_readiness_analyzer {
    use super::*;

    #[test]
    fn test_repo_meeting_v1_criteria() {
        let (_temp, path) = setup_test_repo();
        fs::write(path.join("README.adoc"), "= Project\n\nDescription\n").unwrap();
        fs::write(path.join("ROADMAP.adoc"), "= Roadmap\n\n* Phase 1\n").unwrap();
        fs::write(path.join("LICENSE"), "PMPL-1.0-or-later\n").unwrap();
        fs::write(
            path.join("SECURITY.md"),
            "# Security\n\nReport issues to security@example.com\n",
        )
        .unwrap();
        fs::write(
            path.join("CODE_OF_CONDUCT.md"),
            "# Code of Conduct\n\nBe kind.\n",
        )
        .unwrap();
        fs::write(
            path.join("CONTRIBUTING.md"),
            "# Contributing\n\nSubmit a PR.\n",
        )
        .unwrap();

        let analyzer = V1ReadinessAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        let doc_errors: Vec<_> = result
            .findings
            .iter()
            .filter(|f| f.id.starts_with("V1-DOC") && f.severity == Severity::Error)
            .collect();
        assert!(
            doc_errors.is_empty(),
            "Repo with all docs should have no V1-DOC errors"
        );

        let sec_warnings: Vec<_> = result
            .findings
            .iter()
            .filter(|f| f.id == "V1-SEC-001")
            .collect();
        assert!(
            sec_warnings.is_empty(),
            "Repo with SECURITY.md should have no security warnings"
        );

        let community_warnings: Vec<_> = result
            .findings
            .iter()
            .filter(|f| f.id.starts_with("V1-COM"))
            .collect();
        assert!(
            community_warnings.is_empty(),
            "Repo with community files should have no community warnings"
        );
    }

    #[test]
    fn test_repo_missing_critical_items_error() {
        let (_temp, path) = setup_test_repo();
        // Empty repo -- missing everything

        let analyzer = V1ReadinessAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        // Should flag missing README.adoc, ROADMAP.adoc, LICENSE
        assert!(
            result
                .findings
                .iter()
                .any(|f| f.id == "V1-DOC-README.adoc" && f.severity == Severity::Error),
            "Should report missing README.adoc as Error"
        );
        assert!(
            result
                .findings
                .iter()
                .any(|f| f.id == "V1-DOC-ROADMAP.adoc" && f.severity == Severity::Error),
            "Should report missing ROADMAP.adoc as Error"
        );
        assert!(
            result
                .findings
                .iter()
                .any(|f| f.id == "V1-DOC-LICENSE" && f.severity == Severity::Error),
            "Should report missing LICENSE as Error"
        );
    }

    #[test]
    fn test_language_compliance_clean_repo() {
        let (_temp, path) = setup_test_repo();
        fs::create_dir_all(path.join("src")).unwrap();
        fs::write(path.join("src/main.rs"), "fn main() {}\n").unwrap();

        let analyzer = V1ReadinessAnalyzer::default();
        let config = default_config();
        let result = analyzer.analyze(&path, &config).unwrap();

        let lang_errors: Vec<_> = result
            .findings
            .iter()
            .filter(|f| f.id.starts_with("V1-LANG"))
            .collect();
        assert!(
            lang_errors.is_empty(),
            "Rust-only repo should have no language compliance errors"
        );
    }
}

// ============================================================================
// Integration test: full audit
// ============================================================================
mod integration {
    use super::*;

    #[test]
    fn test_full_audit_well_structured_repo() {
        let (_temp, path) = setup_test_repo();

        // Create a fairly complete repo
        fs::write(
            path.join("LICENSE"),
            "Palimpsest License\nPMPL-1.0-or-later\n",
        )
        .unwrap();
        let readme = "# My Project\n\n\
            Description of the project with enough words.\n\n\
            ## Installation\n\nRun the install command.\n\n\
            ## Usage\n\nUse the tool.\n\n\
            ## License\n\nPMPL-1.0-or-later\n";
        fs::write(path.join("README.md"), readme).unwrap();
        fs::write(
            path.join("CHANGELOG.md"),
            "# Changelog\n\n## 1.0.0\n- Initial\n",
        )
        .unwrap();
        fs::write(
            path.join("SECURITY.md"),
            "# Security\n\nReport to security@example.com\n",
        )
        .unwrap();
        fs::create_dir_all(path.join("src")).unwrap();
        fs::write(
            path.join("src/main.rs"),
            "// SPDX-License-Identifier: PMPL-1.0-or-later\nfn main() {}\n",
        )
        .unwrap();

        let config = default_config();

        let license = LicenseAnalyzer::default()
            .analyze(&path, &config)
            .unwrap();
        let placeholder = PlaceholderAnalyzer::default()
            .analyze(&path, &config)
            .unwrap();
        let claims = ClaimsAnalyzer::default()
            .analyze(&path, &config)
            .unwrap();
        let release = ReleaseAnalyzer::default()
            .analyze(&path, &config)
            .unwrap();

        let mut result = AuditResult::default();
        result.license = license;
        result.placeholder = placeholder;
        result.claims = claims;
        result.release = release;

        // A well-structured repo should have no license or placeholder errors
        assert!(
            !result.license.has_errors(),
            "Well-structured repo should pass license checks"
        );
    }

    #[test]
    fn test_finding_serialization() {
        let finding = finishing_bot::Finding::new(
            "LIC-001",
            "Missing License",
            Severity::Error,
            "No LICENSE file found in repository root",
        )
        .with_file(std::path::PathBuf::from("LICENSE"))
        .with_suggestion("Add a LICENSE file");

        // Verify Finding serializes to JSON correctly
        let json = serde_json::to_string(&finding).expect("Finding should serialize to JSON");
        assert!(json.contains("LIC-001"), "JSON should contain finding ID");
        assert!(
            json.contains("error"),
            "JSON should contain severity level"
        );

        // Verify round-trip
        let deserialized: finishing_bot::Finding =
            serde_json::from_str(&json).expect("Finding should deserialize from JSON");
        assert_eq!(deserialized.id, "LIC-001");
        assert_eq!(deserialized.severity, Severity::Error);
    }

    #[test]
    fn test_audit_result_aggregation() {
        let mut result = AuditResult::default();

        // Add some findings to different analyzers
        result.license.findings.push(finishing_bot::Finding::new(
            "LIC-001",
            "Missing License",
            Severity::Error,
            "No LICENSE file found",
        ));
        result.placeholder.findings.push(finishing_bot::Finding::new(
            "PH-001",
            "Placeholder: TODO",
            Severity::Warning,
            "Found TODO placeholder",
        ));

        assert_eq!(result.total_findings(), 2);
        assert_eq!(result.total_errors(), 1);
        assert_eq!(result.total_warnings(), 1);
        assert!(result.should_block_release());
    }
}
