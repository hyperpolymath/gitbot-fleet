// SPDX-License-Identifier: PMPL-1.0-or-later
//! Tests for fix application - delete, modify, create, rollback

use robot_repo_automaton::catalog::{Fix, FixAction};
use robot_repo_automaton::detector::DetectedIssue;
use robot_repo_automaton::catalog::Severity;
use robot_repo_automaton::fixer::Fixer;
use tempfile::TempDir;

fn make_issue(id: &str) -> DetectedIssue {
    DetectedIssue {
        error_type_id: id.to_string(),
        error_name: "Test Issue".to_string(),
        severity: Severity::Medium,
        description: "Test issue".to_string(),
        affected_files: vec![],
        confidence: 1.0,
        suggested_fix: "Fix it".to_string(),
        commit_message: "fix: test".to_string(),
    }
}

// =========================================================================
// DELETE FIX TESTS
// =========================================================================

#[test]
fn test_delete_fix_removes_file() {
    let temp = TempDir::new().unwrap();
    let file_path = temp.path().join("unwanted.txt");
    std::fs::write(&file_path, "should be deleted").unwrap();
    assert!(file_path.exists());

    let fixer = Fixer::new(temp.path().to_path_buf(), false);
    let issue = make_issue("DEL-001");
    let fix = Fix {
        action: FixAction::Delete,
        target: "unwanted.txt".to_string(),
        reason: Some("Not needed".to_string()),
        modification: None,
        fallback: None,
    };

    let result = fixer.apply(&issue, &fix).unwrap();
    assert!(result.success);
    assert!(!file_path.exists());
    assert!(!result.files_modified.is_empty());
}

#[test]
fn test_delete_fix_already_deleted() {
    let temp = TempDir::new().unwrap();

    let fixer = Fixer::new(temp.path().to_path_buf(), false);
    let issue = make_issue("DEL-002");
    let fix = Fix {
        action: FixAction::Delete,
        target: "nonexistent.txt".to_string(),
        reason: None,
        modification: None,
        fallback: None,
    };

    let result = fixer.apply(&issue, &fix).unwrap();
    assert!(result.success);
    assert!(result.files_modified.is_empty());
}

// =========================================================================
// MODIFY FIX TESTS
// =========================================================================

#[test]
fn test_modify_fix_replaces_line() {
    let temp = TempDir::new().unwrap();
    let file_path = temp.path().join("config.txt");
    std::fs::write(&file_path, "key1=old\nkey2=value\nkey3=value\n").unwrap();

    let fixer = Fixer::new(temp.path().to_path_buf(), false);
    let issue = make_issue("MOD-001");
    let fix = Fix {
        action: FixAction::Modify,
        target: "config.txt".to_string(),
        reason: None,
        modification: Some("replace-line:1:key1=new".to_string()),
        fallback: None,
    };

    let result = fixer.apply(&issue, &fix).unwrap();
    assert!(result.success);

    let content = std::fs::read_to_string(&file_path).unwrap();
    assert!(content.contains("key1=new"));
    assert!(!content.contains("key1=old"));
    // Other lines preserved
    assert!(content.contains("key2=value"));
}

#[test]
fn test_modify_fix_regex_pattern() {
    let temp = TempDir::new().unwrap();
    let file_path = temp.path().join("version.txt");
    std::fs::write(&file_path, "version = \"1.0.0\"\nrelease = true\n").unwrap();

    let fixer = Fixer::new(temp.path().to_path_buf(), false);
    let issue = make_issue("MOD-002");
    let fix = Fix {
        action: FixAction::Modify,
        target: "version.txt".to_string(),
        reason: None,
        modification: Some("replace-pattern:\"1\\.0\\.0\":\"2.0.0\"".to_string()),
        fallback: None,
    };

    let result = fixer.apply(&issue, &fix).unwrap();
    assert!(result.success);

    let content = std::fs::read_to_string(&file_path).unwrap();
    assert!(content.contains("\"2.0.0\""));
    assert!(!content.contains("\"1.0.0\""));
}

#[test]
fn test_modify_fix_rollback_on_invalid_line() {
    let temp = TempDir::new().unwrap();
    let file_path = temp.path().join("data.txt");
    let original = "line 1\nline 2\n";
    std::fs::write(&file_path, original).unwrap();

    let fixer = Fixer::new(temp.path().to_path_buf(), false);
    let issue = make_issue("MOD-003");
    let fix = Fix {
        action: FixAction::Modify,
        target: "data.txt".to_string(),
        reason: None,
        modification: Some("replace-line:500:impossible".to_string()),
        fallback: None,
    };

    let result = fixer.apply(&issue, &fix).unwrap();
    assert!(!result.success);

    // Original content preserved
    let content = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(content, original);
}

#[test]
fn test_modify_fix_binary_file_rejected() {
    let temp = TempDir::new().unwrap();
    let file_path = temp.path().join("image.png");
    std::fs::write(&file_path, b"\x89PNG\r\n\x1a\n").unwrap();

    let fixer = Fixer::new(temp.path().to_path_buf(), false);
    let issue = make_issue("MOD-004");
    let fix = Fix {
        action: FixAction::Modify,
        target: "image.png".to_string(),
        reason: None,
        modification: Some("replace-line:1:hacked".to_string()),
        fallback: None,
    };

    let result = fixer.apply(&issue, &fix).unwrap();
    assert!(!result.success);
    assert!(result.error.as_ref().unwrap().contains("binary"));
}

#[test]
fn test_modify_fix_insert_before() {
    let temp = TempDir::new().unwrap();
    let file_path = temp.path().join("code.rs");
    std::fs::write(&file_path, "fn main() {\n    println!(\"hello\");\n}\n").unwrap();

    let fixer = Fixer::new(temp.path().to_path_buf(), false);
    let issue = make_issue("MOD-005");
    let fix = Fix {
        action: FixAction::Modify,
        target: "code.rs".to_string(),
        reason: None,
        modification: Some("insert-before:1:// SPDX-License-Identifier: PMPL-1.0-or-later".to_string()),
        fallback: None,
    };

    let result = fixer.apply(&issue, &fix).unwrap();
    assert!(result.success);

    let content = std::fs::read_to_string(&file_path).unwrap();
    assert!(content.starts_with("// SPDX-License-Identifier: PMPL-1.0-or-later"));
    assert!(content.contains("fn main()"));
}

#[test]
fn test_modify_fix_insert_after() {
    let temp = TempDir::new().unwrap();
    let file_path = temp.path().join("list.txt");
    std::fs::write(&file_path, "item 1\nitem 2\nitem 3\n").unwrap();

    let fixer = Fixer::new(temp.path().to_path_buf(), false);
    let issue = make_issue("MOD-006");
    let fix = Fix {
        action: FixAction::Modify,
        target: "list.txt".to_string(),
        reason: None,
        modification: Some("insert-after:2:item 2.5".to_string()),
        fallback: None,
    };

    let result = fixer.apply(&issue, &fix).unwrap();
    assert!(result.success);

    let content = std::fs::read_to_string(&file_path).unwrap();
    let lines: Vec<&str> = content.lines().collect();
    assert_eq!(lines[2], "item 2.5");
}

// =========================================================================
// CREATE FIX TESTS
// =========================================================================

#[test]
fn test_create_fix_creates_file() {
    let temp = TempDir::new().unwrap();
    let file_path = temp.path().join("NEW_FILE.txt");
    assert!(!file_path.exists());

    let fixer = Fixer::new(temp.path().to_path_buf(), false);
    let issue = make_issue("CRT-001");
    let fix = Fix {
        action: FixAction::Create,
        target: "NEW_FILE.txt".to_string(),
        reason: None,
        modification: None,
        fallback: Some("This is new content".to_string()),
    };

    let result = fixer.apply(&issue, &fix).unwrap();
    assert!(result.success);
    assert!(file_path.exists());

    let content = std::fs::read_to_string(&file_path).unwrap();
    assert!(content.contains("This is new content"));
}

#[test]
fn test_create_fix_creates_directory() {
    let temp = TempDir::new().unwrap();
    let dir_path = temp.path().join("new_dir");
    let file_path = dir_path.join("file.txt");
    assert!(!dir_path.exists());

    let fixer = Fixer::new(temp.path().to_path_buf(), false);
    let issue = make_issue("CRT-002");
    let fix = Fix {
        action: FixAction::Create,
        target: "new_dir/file.txt".to_string(),
        reason: None,
        modification: None,
        fallback: Some("Created in new dir".to_string()),
    };

    let result = fixer.apply(&issue, &fix).unwrap();
    assert!(result.success);
    assert!(dir_path.exists());
    assert!(file_path.exists());
}

#[test]
fn test_create_fix_skips_existing_file() {
    let temp = TempDir::new().unwrap();
    let file_path = temp.path().join("existing.txt");
    std::fs::write(&file_path, "original content").unwrap();

    let fixer = Fixer::new(temp.path().to_path_buf(), false);
    let issue = make_issue("CRT-003");
    let fix = Fix {
        action: FixAction::Create,
        target: "existing.txt".to_string(),
        reason: None,
        modification: None,
        fallback: Some("New content that should not overwrite".to_string()),
    };

    let result = fixer.apply(&issue, &fix).unwrap();
    assert!(result.success);
    assert!(result.files_modified.is_empty()); // No files modified

    // Original content preserved
    let content = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(content, "original content");
}

// =========================================================================
// DRY RUN TESTS
// =========================================================================

#[test]
fn test_dry_run_does_not_modify() {
    let temp = TempDir::new().unwrap();
    let file_path = temp.path().join("protected.txt");
    let original = "do not change me";
    std::fs::write(&file_path, original).unwrap();

    let fixer = Fixer::new(temp.path().to_path_buf(), true); // dry_run = true
    let issue = make_issue("DRY-001");
    let fix = Fix {
        action: FixAction::Modify,
        target: "protected.txt".to_string(),
        reason: None,
        modification: Some("replace-line:1:changed".to_string()),
        fallback: None,
    };

    let result = fixer.apply(&issue, &fix).unwrap();
    assert!(result.success);
    assert!(result.action_taken.contains("DRY RUN"));

    // Content unchanged
    let content = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(content, original);
}

#[test]
fn test_dry_run_does_not_create() {
    let temp = TempDir::new().unwrap();
    let file_path = temp.path().join("new_file.txt");

    let fixer = Fixer::new(temp.path().to_path_buf(), true); // dry_run = true
    let issue = make_issue("DRY-002");
    let fix = Fix {
        action: FixAction::Create,
        target: "new_file.txt".to_string(),
        reason: None,
        modification: None,
        fallback: Some("content".to_string()),
    };

    let result = fixer.apply(&issue, &fix).unwrap();
    assert!(result.success);
    assert!(!file_path.exists()); // File NOT created
}

#[test]
fn test_dry_run_does_not_delete() {
    let temp = TempDir::new().unwrap();
    let file_path = temp.path().join("keep_me.txt");
    std::fs::write(&file_path, "keep").unwrap();

    let fixer = Fixer::new(temp.path().to_path_buf(), true); // dry_run = true
    let issue = make_issue("DRY-003");
    let fix = Fix {
        action: FixAction::Delete,
        target: "keep_me.txt".to_string(),
        reason: None,
        modification: None,
        fallback: None,
    };

    let result = fixer.apply(&issue, &fix).unwrap();
    assert!(result.success);
    assert!(file_path.exists()); // File still exists
}

// =========================================================================
// IDEMPOTENCY TESTS
// =========================================================================
//
// Every fix must be idempotent: applying a fix twice produces the same result
// as applying it once. This is critical for robot-repo-automaton, which may
// re-run fixes when it is retried or when CI reruns a failed job.

#[test]
fn test_delete_fix_is_idempotent() {
    let temp = TempDir::new().unwrap();
    let file_path = temp.path().join("to_delete.txt");
    std::fs::write(&file_path, "delete me").unwrap();

    let fixer = Fixer::new(temp.path().to_path_buf(), false);
    let issue = make_issue("IDEM-001");
    let fix = Fix {
        action: FixAction::Delete,
        target: "to_delete.txt".to_string(),
        reason: None,
        modification: None,
        fallback: None,
    };

    // First application: file is deleted
    let result1 = fixer.apply(&issue, &fix).unwrap();
    assert!(result1.success);
    assert!(!file_path.exists());

    // Second application: file is already gone — must still succeed (no panic, no error)
    let result2 = fixer.apply(&issue, &fix).unwrap();
    assert!(result2.success, "Second delete should succeed (idempotent)");
    assert!(!file_path.exists());
}

#[test]
fn test_create_fix_is_idempotent() {
    let temp = TempDir::new().unwrap();
    let file_path = temp.path().join("new_file.txt");

    let fixer = Fixer::new(temp.path().to_path_buf(), false);
    let issue = make_issue("IDEM-002");
    let fix = Fix {
        action: FixAction::Create,
        target: "new_file.txt".to_string(),
        reason: None,
        modification: None,
        fallback: Some("expected content".to_string()),
    };

    // First application: file is created
    let result1 = fixer.apply(&issue, &fix).unwrap();
    assert!(result1.success);
    let content_after_first = std::fs::read_to_string(&file_path).unwrap();
    assert!(content_after_first.contains("expected content"));

    // Second application: file already exists — must succeed with no change
    let result2 = fixer.apply(&issue, &fix).unwrap();
    assert!(result2.success, "Second create should succeed (idempotent)");

    // Content must not have been overwritten or doubled
    let content_after_second = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(content_after_first, content_after_second,
        "File content changed on second create — not idempotent");
}

#[test]
fn test_modify_fix_is_idempotent() {
    let temp = TempDir::new().unwrap();
    let file_path = temp.path().join("config.txt");
    std::fs::write(&file_path, "version = \"1.0.0\"\n").unwrap();

    let fixer = Fixer::new(temp.path().to_path_buf(), false);
    let issue = make_issue("IDEM-003");
    let fix = Fix {
        action: FixAction::Modify,
        target: "config.txt".to_string(),
        reason: None,
        modification: Some("replace-pattern:\"1\\.0\\.0\":\"2.0.0\"".to_string()),
        fallback: None,
    };

    // First application: version bumped
    let result1 = fixer.apply(&issue, &fix).unwrap();
    assert!(result1.success);
    let content_v1 = std::fs::read_to_string(&file_path).unwrap();
    assert!(content_v1.contains("\"2.0.0\""));

    // Second application: pattern no longer matches (1.0.0 is gone)
    // Must succeed without corrupting the file
    let result2 = fixer.apply(&issue, &fix).unwrap();
    assert!(result2.success, "Second modify should succeed (idempotent)");
    let content_v2 = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(content_v1, content_v2,
        "File content changed on second modify — not idempotent");
}

// =========================================================================
// SECURITY: PATH TRAVERSAL TESTS
// =========================================================================
//
// The fixer MUST reject any target that would escape the repository root.
// A compromised or malformed ErrorCatalog must not be able to write/delete
// files outside the repository being fixed.

#[test]
fn test_path_traversal_delete_rejected() {
    let temp = TempDir::new().unwrap();
    // Create a "sentinel" file outside the temp dir to ensure it is never deleted
    let outer = TempDir::new().unwrap();
    let sentinel = outer.path().join("sentinel.txt");
    std::fs::write(&sentinel, "must not be deleted").unwrap();

    let fixer = Fixer::new(temp.path().to_path_buf(), false);
    let issue = make_issue("SEC-001");

    // Attempt to delete a file using a path traversal sequence
    let traversal = format!("../../{}/sentinel.txt", outer.path().file_name().unwrap().to_str().unwrap());
    let fix = Fix {
        action: FixAction::Delete,
        target: traversal,
        reason: Some("Attempted path traversal".to_string()),
        modification: None,
        fallback: None,
    };

    let result = fixer.apply(&issue, &fix).unwrap();
    // Must fail with a security error, not succeed
    assert!(!result.success);
    assert!(
        result.error.as_deref().unwrap_or("").contains("outside the repository"),
        "Expected path traversal error, got: {:?}",
        result.error
    );
    // Sentinel file must be untouched
    assert!(sentinel.exists(), "Sentinel file was deleted — path traversal succeeded");
}

#[test]
fn test_path_traversal_create_rejected() {
    let temp = TempDir::new().unwrap();
    let outer = TempDir::new().unwrap();

    let fixer = Fixer::new(temp.path().to_path_buf(), false);
    let issue = make_issue("SEC-002");

    let traversal = format!("../../../{}/injected.txt", outer.path().file_name().unwrap().to_str().unwrap());
    let fix = Fix {
        action: FixAction::Create,
        target: traversal,
        reason: None,
        modification: None,
        fallback: Some("injected content".to_string()),
    };

    let result = fixer.apply(&issue, &fix).unwrap();
    assert!(!result.success);
    assert!(
        result.error.as_deref().unwrap_or("").contains("outside the repository"),
        "Expected path traversal error, got: {:?}",
        result.error
    );
}

#[test]
fn test_path_traversal_modify_rejected() {
    let temp = TempDir::new().unwrap();
    let outer = TempDir::new().unwrap();
    let victim = outer.path().join("victim.conf");
    std::fs::write(&victim, "original=true\n").unwrap();

    let fixer = Fixer::new(temp.path().to_path_buf(), false);
    let issue = make_issue("SEC-003");

    let traversal = format!("../../{}/victim.conf", outer.path().file_name().unwrap().to_str().unwrap());
    let fix = Fix {
        action: FixAction::Modify,
        target: traversal,
        reason: None,
        modification: Some("replace-line:1:original=false".to_string()),
        fallback: None,
    };

    let result = fixer.apply(&issue, &fix).unwrap();
    assert!(!result.success);
    assert!(
        result.error.as_deref().unwrap_or("").contains("outside the repository"),
        "Expected path traversal error, got: {:?}",
        result.error
    );
    // Original file must be untouched
    let content = std::fs::read_to_string(&victim).unwrap();
    assert_eq!(content, "original=true\n", "Victim file was modified — path traversal succeeded");
}

#[test]
fn test_path_within_repo_is_not_rejected() {
    let temp = TempDir::new().unwrap();
    let file_path = temp.path().join("safe.txt");
    std::fs::write(&file_path, "safe content").unwrap();

    let fixer = Fixer::new(temp.path().to_path_buf(), false);
    let issue = make_issue("SEC-004");
    let fix = Fix {
        action: FixAction::Delete,
        target: "safe.txt".to_string(),
        reason: None,
        modification: None,
        fallback: None,
    };

    let result = fixer.apply(&issue, &fix).unwrap();
    // A legitimate in-repo path must succeed
    assert!(result.success, "Legitimate in-repo path was incorrectly rejected");
    assert!(!file_path.exists());
}
