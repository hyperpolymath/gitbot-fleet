// SPDX-License-Identifier: PMPL-1.0-or-later
//! Tests for issue detection

use robot_repo_automaton::prelude::*;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

fn setup_test_repo() -> (TempDir, PathBuf) {
    let temp = TempDir::new().expect("Create temp dir");
    let path = temp.path().to_path_buf();

    // Create basic repo structure
    fs::write(path.join("README.md"), "# Test Repo\n").unwrap();
    fs::write(path.join("LICENSE"), "MIT License\n").unwrap();
    fs::create_dir_all(path.join("src")).unwrap();
    fs::write(path.join("src/main.rs"), "fn main() {}\n").unwrap();

    (temp, path)
}

/// Catalog entry that triggers when none of the listed files exist (missing README)
const CATALOG_WITH_README: &str = r#"
(define error-catalog
  '((error-type
      (id . "ERR-MISSING-README")
      (name . "Missing README")
      (severity . "high")
      (category . "docs")
      (description . "Repository missing README file")
      (detection
        (method . "file-existence")
        (files . ("README.md" "README.adoc" "README"))
        (condition . "none-exist"))
      (affected-repos . ("*"))
      (fix
        (action . "create")
        (target . "README.md"))
      (commit-message . "docs: add README"))))
"#;

/// Catalog entry that triggers when .env exists (exposed secret)
const CATALOG_WITH_SECRET: &str = r#"
(define error-catalog
  '((error-type
      (id . "ERR-SECRET-EXPOSED")
      (name . "Secret Exposed")
      (severity . "critical")
      (category . "security")
      (description . "Secret key exposed in repository")
      (detection
        (method . "file-existence")
        (files . (".env"))
        (condition . "any-exist"))
      (affected-repos . ("*"))
      (fix
        (action . "delete")
        (target . ".env"))
      (commit-message . "security: remove exposed secrets"))))
"#;

#[test]
fn test_detector_creation() {
    let (_temp, path) = setup_test_repo();
    let detector = Detector::new(path);
    assert!(detector.is_ok());
}

#[test]
fn test_detector_invalid_path() {
    let detector = Detector::new(PathBuf::from("/nonexistent/path/to/repo"));
    assert!(detector.is_err());
}

#[test]
fn test_detect_no_issues() {
    let (_temp, path) = setup_test_repo();
    let detector = Detector::new(path).unwrap();

    let catalog = ErrorCatalog::parse(CATALOG_WITH_README).unwrap();
    let issues = detector.detect_all(&catalog.error_types);

    // README.md exists, so "none-exist" condition is false -> no issue
    let readme_issues: Vec<_> = issues
        .iter()
        .filter(|i| i.error_type_id == "ERR-MISSING-README")
        .collect();
    assert!(readme_issues.is_empty());
}

#[test]
fn test_detect_missing_file() {
    let temp = TempDir::new().unwrap();
    let path = temp.path().to_path_buf();

    // Create repo WITHOUT README
    fs::write(path.join("main.rs"), "fn main() {}\n").unwrap();

    let detector = Detector::new(path).unwrap();
    let catalog = ErrorCatalog::parse(CATALOG_WITH_README).unwrap();
    let issues = detector.detect_all(&catalog.error_types);

    // None of README.md, README.adoc, README exist -> "none-exist" triggers
    let readme_issues: Vec<_> = issues
        .iter()
        .filter(|i| i.error_type_id == "ERR-MISSING-README")
        .collect();
    assert_eq!(readme_issues.len(), 1);
}

#[test]
fn test_detect_exposed_secret() {
    let temp = TempDir::new().unwrap();
    let path = temp.path().to_path_buf();

    // Create .env with exposed secret
    fs::write(path.join(".env"), "API_KEY=secret123\n").unwrap();

    let detector = Detector::new(path).unwrap();
    let catalog = ErrorCatalog::parse(CATALOG_WITH_SECRET).unwrap();
    let issues = detector.detect_all(&catalog.error_types);

    // .env exists -> "any-exist" triggers
    let secret_issues: Vec<_> = issues
        .iter()
        .filter(|i| i.error_type_id == "ERR-SECRET-EXPOSED")
        .collect();
    assert_eq!(secret_issues.len(), 1);
}

#[test]
fn test_detect_language() {
    let (_temp, path) = setup_test_repo();
    let detector = Detector::new(path).unwrap();

    // Detect Rust via the public languages() method
    assert!(detector.languages().contains("rust"));
}

#[test]
fn test_detect_multiple_languages() {
    let temp = TempDir::new().unwrap();
    let path = temp.path().to_path_buf();

    fs::create_dir_all(path.join("src")).unwrap();
    fs::write(path.join("src/main.rs"), "fn main() {}\n").unwrap();
    fs::write(path.join("src/app.js"), "console.log('hi');\n").unwrap();
    fs::write(path.join("src/lib.py"), "print('hello')\n").unwrap();

    let detector = Detector::new(path).unwrap();

    assert!(detector.languages().contains("rust"));
    assert!(detector.languages().contains("javascript"));
    assert!(detector.languages().contains("python"));
}

#[test]
fn test_detected_issue_structure() {
    let temp = TempDir::new().unwrap();
    let path = temp.path().to_path_buf();

    // Create repo WITHOUT README -> triggers "none-exist"
    fs::write(path.join("main.rs"), "fn main() {}\n").unwrap();

    let detector = Detector::new(path.clone()).unwrap();
    let catalog = ErrorCatalog::parse(CATALOG_WITH_README).unwrap();
    let issues = detector.detect_all(&catalog.error_types);

    assert!(!issues.is_empty());
    let issue = &issues[0];

    assert_eq!(issue.error_type_id, "ERR-MISSING-README");
    assert!(issue.confidence > 0.0);
}
