// SPDX-License-Identifier: PMPL-1.0-or-later
//! Tests for error catalog parsing

use robot_repo_automaton::catalog::{
    DetectionMethod, ErrorCatalog, FixAction, Severity,
};

const SAMPLE_CATALOG: &str = r#"
(define error-catalog
  '((metadata
      (format-version . "1.0")
      (schema-version . "1.0")
      (purpose . "Test error catalog"))

    (error-type
      (id . "ERR-TEST-001")
      (name . "Test Error")
      (severity . "high")
      (category . "test")
      (description . "A test error for unit testing")
      (detection
        (method . "file-existence")
        (files . ("test.txt" "other.txt")))
      (affected-repos . ("repo1" "repo2"))
      (fix
        (action . "delete")
        (target . "test.txt")
        (reason . "Not needed"))
      (commit-message . "fix: remove test file"))

    (error-type
      (id . "ERR-TEST-002")
      (name . "Critical Error")
      (severity . "critical")
      (category . "security")
      (description . "A critical security issue")
      (detection
        (method . "content-match")
        (files . (".env"))
        (condition . "contains API_KEY"))
      (affected-repos . ("repo1"))
      (fix
        (action . "modify")
        (target . ".env")
        (modification . "redact"))
      (commit-message . "security: redact API keys"))))
"#;

#[test]
fn test_parse_catalog_metadata() {
    let catalog = ErrorCatalog::parse(SAMPLE_CATALOG).expect("Should parse sample catalog");

    assert_eq!(catalog.metadata.format_version, "1.0");
    assert_eq!(catalog.metadata.schema_version, "1.0");
    assert_eq!(catalog.metadata.purpose, "Test error catalog");
}

#[test]
fn test_parse_error_types() {
    let catalog = ErrorCatalog::parse(SAMPLE_CATALOG).expect("Should parse sample catalog");

    assert_eq!(catalog.error_types.len(), 2);

    let first = &catalog.error_types[0];
    assert_eq!(first.id, "ERR-TEST-001");
    assert_eq!(first.name, "Test Error");
    assert_eq!(first.severity, Severity::High);
    assert_eq!(first.category, "test");

    let second = &catalog.error_types[1];
    assert_eq!(second.id, "ERR-TEST-002");
    assert_eq!(second.severity, Severity::Critical);
}

#[test]
fn test_parse_detection() {
    let catalog = ErrorCatalog::parse(SAMPLE_CATALOG).expect("Should parse sample catalog");

    let first = &catalog.error_types[0];
    assert!(matches!(first.detection.method, DetectionMethod::FileExistence));
    assert!(first.detection.files.contains(&"test.txt".to_string()));

    let second = &catalog.error_types[1];
    assert!(matches!(second.detection.method, DetectionMethod::ContentMatch));
    assert_eq!(second.detection.condition, Some("contains API_KEY".to_string()));
}

#[test]
fn test_parse_fix() {
    let catalog = ErrorCatalog::parse(SAMPLE_CATALOG).expect("Should parse sample catalog");

    let first = &catalog.error_types[0];
    assert!(matches!(first.fix.action, FixAction::Delete));
    assert_eq!(first.fix.target, "test.txt");
    assert_eq!(first.fix.reason, Some("Not needed".to_string()));

    let second = &catalog.error_types[1];
    assert!(matches!(second.fix.action, FixAction::Modify));
    assert_eq!(second.fix.modification, Some("redact".to_string()));
}

#[test]
fn test_get_by_id() {
    let catalog = ErrorCatalog::parse(SAMPLE_CATALOG).expect("Should parse sample catalog");

    let err = catalog.get("ERR-TEST-001");
    assert!(err.is_some());
    assert_eq!(err.unwrap().name, "Test Error");

    let not_found = catalog.get("NONEXISTENT");
    assert!(not_found.is_none());
}

#[test]
fn test_get_by_severity() {
    let catalog = ErrorCatalog::parse(SAMPLE_CATALOG).expect("Should parse sample catalog");

    let critical = catalog.by_severity(Severity::Critical);
    assert_eq!(critical.len(), 1);
    assert_eq!(critical[0].id, "ERR-TEST-002");

    let high = catalog.by_severity(Severity::High);
    assert_eq!(high.len(), 1);
    assert_eq!(high[0].id, "ERR-TEST-001");

    let low = catalog.by_severity(Severity::Low);
    assert!(low.is_empty());
}

#[test]
fn test_errors_for_repo() {
    let catalog = ErrorCatalog::parse(SAMPLE_CATALOG).expect("Should parse sample catalog");

    let repo1_errors = catalog.errors_for_repo("repo1");
    assert_eq!(repo1_errors.len(), 2);

    let repo2_errors = catalog.errors_for_repo("repo2");
    assert_eq!(repo2_errors.len(), 1);

    let repo3_errors = catalog.errors_for_repo("repo3");
    assert!(repo3_errors.is_empty());
}

#[test]
fn test_severity_ordering() {
    assert!(Severity::Critical > Severity::High);
    assert!(Severity::High > Severity::Medium);
    assert!(Severity::Medium > Severity::Low);
    assert!(Severity::Low > Severity::Info);
}

#[test]
fn test_severity_from_str() {
    let catalog_critical = r#"
    (define error-catalog
      '((error-type
          (id . "TEST")
          (severity . "critical"))))
    "#;

    let catalog = ErrorCatalog::parse(catalog_critical).unwrap();
    assert_eq!(catalog.error_types[0].severity, Severity::Critical);

    let catalog_high = r#"
    (define error-catalog
      '((error-type
          (id . "TEST")
          (severity . "HIGH"))))
    "#;

    let catalog = ErrorCatalog::parse(catalog_high).unwrap();
    assert_eq!(catalog.error_types[0].severity, Severity::High);
}
