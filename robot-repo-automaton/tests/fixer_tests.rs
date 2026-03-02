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
