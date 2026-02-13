// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Integration tests for glambot analyzers

use glambot::{
    AccessibilityAnalyzer, Analyzer, Config, Finding, MachineAnalyzer, SeoAnalyzer, Severity,
    VisualAnalyzer,
};
use tempfile::TempDir;

/// Create a well-formed test repository with known good structure
fn create_well_formed_repo() -> TempDir {
    let dir = TempDir::new().unwrap();
    let path = dir.path();

    // Good README with title, description, badges, and keywords
    let readme = "# Test Project\n\n\
        ![Build](https://img.shields.io/badge/build-passing-green)\n\n\
        A comprehensive test project for validation purposes.\n\
        This project demonstrates installation, usage, and examples.\n\n\
        ## Installation\n\nRun `cargo install test-project`.\n\n\
        ## Usage\n\n```bash\ntest-project --help\n```\n\n\
        ## Examples\n\nSee the examples directory.\n\n\
        ## Documentation\n\nFull docs available online.\n\n\
        ## License\n\nPMPL-1.0-or-later\n";
    std::fs::write(path.join("README.md"), readme).unwrap();

    // Valid JSON
    std::fs::write(
        path.join("config.json"),
        r#"{"name": "test", "version": "1.0"}"#,
    )
    .unwrap();

    // Valid YAML
    std::fs::write(path.join("config.yml"), "name: test\nversion: 1.0\n").unwrap();

    // .machine_readable/ with SCM files
    let mr_dir = path.join(".machine_readable");
    std::fs::create_dir_all(&mr_dir).unwrap();
    std::fs::write(mr_dir.join("STATE.scm"), "(state (metadata))").unwrap();
    std::fs::write(mr_dir.join("META.scm"), "(meta)").unwrap();
    std::fs::write(mr_dir.join("ECOSYSTEM.scm"), "(ecosystem)").unwrap();

    // robots.txt
    std::fs::write(path.join("robots.txt"), "User-agent: *\nAllow: /\n").unwrap();

    dir
}

/// Create a bare/deficient test repository with known deficiencies
fn create_deficient_repo() -> TempDir {
    let dir = TempDir::new().unwrap();
    let path = dir.path();

    // Short README without badges or keywords
    std::fs::write(path.join("README.md"), "# Bad\n").unwrap();

    // Markdown with empty alt text
    std::fs::write(
        path.join("docs.md"),
        "# Docs\n\n![](screenshot.png)\n\n[click here](https://example.com)\n",
    )
    .unwrap();

    // Invalid JSON
    std::fs::write(path.join("broken.json"), "{not valid json}").unwrap();

    // No .machine_readable/ directory

    dir
}

#[test]
fn test_all_analyzers_well_formed_repo() {
    let dir = create_well_formed_repo();
    let config = Config::default();

    // Visual
    let visual = VisualAnalyzer::default();
    let visual_result = visual.analyze(dir.path(), &config).unwrap();
    assert!(
        !visual_result.has_errors(),
        "Visual: well-formed repo should have no errors"
    );

    // Accessibility
    let accessibility = AccessibilityAnalyzer::default();
    let acc_result = accessibility.analyze(dir.path(), &config).unwrap();
    assert!(
        !acc_result.has_errors(),
        "Accessibility: well-formed repo should have no errors"
    );

    // SEO
    let seo = SeoAnalyzer::default();
    let seo_result = seo.analyze(dir.path(), &config).unwrap();
    assert!(
        !seo_result.has_errors(),
        "SEO: well-formed repo should have no errors"
    );

    // Machine
    let machine = MachineAnalyzer::default();
    let machine_result = machine.analyze(dir.path(), &config).unwrap();
    assert!(
        !machine_result.has_errors(),
        "Machine: well-formed repo should have no errors"
    );
}

#[test]
fn test_all_analyzers_deficient_repo() {
    let dir = create_deficient_repo();
    let config = Config::default();

    // Visual
    let visual = VisualAnalyzer::default();
    let visual_result = visual.analyze(dir.path(), &config).unwrap();
    assert!(
        !visual_result.findings.is_empty(),
        "Visual: deficient repo should have findings"
    );

    // Accessibility
    let accessibility = AccessibilityAnalyzer::default();
    let acc_result = accessibility.analyze(dir.path(), &config).unwrap();
    assert!(
        acc_result.has_errors(),
        "Accessibility: deficient repo should have errors (missing alt text)"
    );

    // Machine
    let machine = MachineAnalyzer::default();
    let machine_result = machine.analyze(dir.path(), &config).unwrap();
    assert!(
        machine_result.has_errors(),
        "Machine: deficient repo should have errors (invalid JSON)"
    );

    // Verify correct total count
    let total = visual_result.findings.len()
        + acc_result.findings.len()
        + machine_result.findings.len();
    assert!(
        total >= 4,
        "Should have at least 4 total findings across analyzers, got {}",
        total
    );
}

#[test]
fn test_findings_serialize_deserialize() {
    let finding = Finding::new(
        "TEST-001",
        "Test finding",
        Severity::Warning,
        "This is a test finding for serialization",
    )
    .with_file(std::path::PathBuf::from("test.rs"))
    .with_line(42)
    .with_suggestion("Fix the issue");

    // Serialize to JSON
    let json = serde_json::to_string(&finding).unwrap();

    // Deserialize back
    let deserialized: Finding = serde_json::from_str(&json).unwrap();

    assert_eq!(deserialized.id, "TEST-001");
    assert_eq!(deserialized.name, "Test finding");
    assert_eq!(deserialized.severity, Severity::Warning);
    assert_eq!(
        deserialized.message,
        "This is a test finding for serialization"
    );
    assert_eq!(
        deserialized.file.as_ref().unwrap().display().to_string(),
        "test.rs"
    );
    assert_eq!(deserialized.line, Some(42));
    assert_eq!(deserialized.suggestion.as_deref(), Some("Fix the issue"));
}

#[test]
fn test_finding_severity_levels() {
    let dir = create_deficient_repo();
    let config = Config::default();

    let machine = MachineAnalyzer::default();
    let result = machine.analyze(dir.path(), &config).unwrap();

    // Verify we get different severity levels
    let severities: Vec<Severity> = result
        .findings
        .iter()
        .map(|f| f.severity)
        .collect();

    // Invalid JSON should be Error
    assert!(
        severities.contains(&Severity::Error),
        "Should have at least one Error severity finding"
    );
}

#[test]
fn test_analysis_result_methods() {
    let dir = create_deficient_repo();
    let config = Config::default();

    let machine = MachineAnalyzer::default();
    let result = machine.analyze(dir.path(), &config).unwrap();

    // Test the result helper methods
    let errors = result.errors();
    let warnings = result.warnings();

    assert!(
        !errors.is_empty() || !warnings.is_empty(),
        "Deficient repo should produce some errors or warnings"
    );
    assert!(result.has_errors(), "has_errors() should return true");
    assert!(result.files_checked > 0, "Should have checked some files");
}
