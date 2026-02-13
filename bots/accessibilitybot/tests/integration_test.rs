// SPDX-License-Identifier: PMPL-1.0-or-later
//! Integration tests for accessibilitybot

use accessibilitybot::fleet::WcagLevel;
use accessibilitybot::report::{generate_report, OutputFormat};
use accessibilitybot::scanner;
use std::path::Path;

#[test]
fn test_scan_accessible_fixture() {
    let findings = scanner::scan_file(
        Path::new("tests/fixtures/accessible.html"),
        WcagLevel::AAA,
    ).expect("scan should succeed");

    // Accessible page should have very few or no error-level findings
    let errors = findings.errors();
    assert!(
        errors.is_empty(),
        "Accessible fixture should have no errors, got {} errors: {:?}",
        errors.len(),
        errors.iter().map(|f| &f.rule_id).collect::<Vec<_>>()
    );
}

#[test]
fn test_scan_inaccessible_fixture() {
    let findings = scanner::scan_file(
        Path::new("tests/fixtures/inaccessible.html"),
        WcagLevel::AAA,
    ).expect("scan should succeed");

    // Inaccessible page should have multiple findings
    assert!(
        findings.len() >= 5,
        "Inaccessible fixture should have many findings, got {}",
        findings.len()
    );

    // Should have errors
    assert!(
        findings.has_errors(),
        "Inaccessible fixture should have errors"
    );
}

#[test]
fn test_scan_partial_fixture() {
    let findings = scanner::scan_file(
        Path::new("tests/fixtures/partial.html"),
        WcagLevel::AAA,
    ).expect("scan should succeed");

    // Partial page has some issues but not as many as inaccessible
    assert!(
        findings.len() >= 1,
        "Partial fixture should have some findings, got {}",
        findings.len()
    );
}

#[test]
fn test_scan_css_fixture() {
    let findings = scanner::scan_file(
        Path::new("tests/fixtures/styles.css"),
        WcagLevel::AAA,
    ).expect("scan should succeed");

    // CSS fixture has contrast, outline, animation, and sr-only issues
    assert!(
        findings.len() >= 3,
        "CSS fixture should have multiple findings, got {}",
        findings.len()
    );
}

#[test]
fn test_scan_fixtures_directory() {
    let findings = scanner::scan_directory(
        Path::new("tests/fixtures"),
        WcagLevel::AAA,
    ).expect("scan should succeed");

    // Directory scan should find issues across all fixture files
    assert!(
        findings.len() >= 10,
        "Fixture directory should have many total findings, got {}",
        findings.len()
    );
}

#[test]
fn test_json_report_valid() {
    let findings = scanner::scan_file(
        Path::new("tests/fixtures/inaccessible.html"),
        WcagLevel::AAA,
    ).expect("scan should succeed");

    let report = generate_report(&findings, OutputFormat::Json);
    let parsed: serde_json::Value = serde_json::from_str(&report)
        .expect("JSON report should be valid JSON");

    assert!(parsed["findings"].is_array());
    assert!(parsed["findings"].as_array().unwrap().len() > 0);
}

#[test]
fn test_sarif_report_valid() {
    let findings = scanner::scan_file(
        Path::new("tests/fixtures/inaccessible.html"),
        WcagLevel::AAA,
    ).expect("scan should succeed");

    let report = generate_report(&findings, OutputFormat::Sarif);
    let parsed: serde_json::Value = serde_json::from_str(&report)
        .expect("SARIF report should be valid JSON");

    assert_eq!(parsed["version"], "2.1.0");
    assert!(parsed["runs"].is_array());
    assert!(parsed["runs"][0]["results"].is_array());
    assert!(parsed["runs"][0]["tool"]["driver"]["name"] == "accessibilitybot");
}

#[test]
fn test_text_report_format() {
    let findings = scanner::scan_file(
        Path::new("tests/fixtures/inaccessible.html"),
        WcagLevel::AAA,
    ).expect("scan should succeed");

    let report = generate_report(&findings, OutputFormat::Text);

    assert!(report.contains("Accessibilitybot WCAG Analysis Report"));
    assert!(report.contains("WCAG"));
    assert!(report.contains("RELEASE BLOCKED"));
}

#[test]
fn test_wcag_level_a_filters() {
    let findings_a = scanner::scan_file(
        Path::new("tests/fixtures/inaccessible.html"),
        WcagLevel::A,
    ).expect("scan should succeed");

    let findings_aaa = scanner::scan_file(
        Path::new("tests/fixtures/inaccessible.html"),
        WcagLevel::AAA,
    ).expect("scan should succeed");

    // AAA should find at least as many issues as A
    assert!(
        findings_aaa.len() >= findings_a.len(),
        "AAA ({}) should find >= A ({}) findings",
        findings_aaa.len(),
        findings_a.len()
    );
}
