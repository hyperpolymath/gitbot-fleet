// SPDX-License-Identifier: PMPL-1.0-or-later

//! Security hardening utilities
//!
//! Provides input validation, path traversal prevention, and
//! security-focused utilities for seam hygiene operations.

use anyhow::{bail, Result};
use std::path::{Component, Path, PathBuf};

/// Validate and sanitize a file path to prevent path traversal attacks.
///
/// Rejects paths containing:
/// - `..` components (parent directory traversal)
/// - Absolute paths when `allow_absolute` is false
/// - Null bytes
/// - Paths that escape the repository root
///
/// Returns the canonicalized path relative to `repo_root`.
pub fn validate_path(path: &str, repo_root: &Path, allow_absolute: bool) -> Result<PathBuf> {
    // Reject null bytes
    if path.contains('\0') {
        bail!("Path contains null bytes: rejected for security");
    }

    // Reject empty paths
    if path.trim().is_empty() {
        bail!("Empty path: rejected for security");
    }

    let path = Path::new(path);

    // Reject absolute paths if not allowed
    if !allow_absolute && path.is_absolute() {
        bail!(
            "Absolute path not allowed: '{}'. Use relative paths within the repository.",
            path.display()
        );
    }

    // Check for path traversal via .. components
    for component in path.components() {
        if component == Component::ParentDir {
            bail!(
                "Path traversal detected: '{}' contains '..' component",
                path.display()
            );
        }
    }

    // Resolve the full path
    let full_path = if path.is_absolute() {
        path.to_path_buf()
    } else {
        repo_root.join(path)
    };

    // Verify the resolved path is within the repo root
    // Use string prefix comparison to handle cases where canonicalize might fail
    // (e.g., the path doesn't exist yet)
    let repo_str = repo_root.to_string_lossy();
    let full_str = full_path.to_string_lossy();
    if !full_str.starts_with(repo_str.as_ref()) {
        bail!(
            "Path '{}' escapes repository root '{}'",
            path.display(),
            repo_root.display()
        );
    }

    Ok(full_path)
}

/// Validate a git branch name for safety.
///
/// Part of the security API for consumers who process branch names
/// from untrusted input (e.g., webhook payloads).
#[allow(dead_code)]
///
/// Rejects branch names containing:
/// - Shell metacharacters
/// - Path traversal sequences
/// - Control characters
/// - Spaces
pub fn validate_branch_name(name: &str) -> Result<()> {
    if name.is_empty() {
        bail!("Empty branch name");
    }

    // Reject control characters
    if name.chars().any(|c| c.is_control()) {
        bail!("Branch name contains control characters: rejected");
    }

    // Reject shell metacharacters and dangerous characters
    let forbidden = ['`', '$', '|', ';', '&', '>', '<', '!', '~', '{', '}', '\\'];
    for ch in &forbidden {
        if name.contains(*ch) {
            bail!("Branch name contains forbidden character '{}': rejected", ch);
        }
    }

    // Reject path traversal
    if name.contains("..") {
        bail!("Branch name contains '..': path traversal rejected");
    }

    // Reject names starting with -
    if name.starts_with('-') {
        bail!("Branch name starts with '-': rejected (could be interpreted as flag)");
    }

    Ok(())
}

/// Validate a seam register JSON structure.
///
/// Performs basic schema validation to ensure the register has
/// the required fields and correct types before processing.
pub fn validate_register_json(value: &serde_json::Value) -> Result<()> {
    let obj = value.as_object().ok_or_else(|| {
        anyhow::anyhow!("Seam register must be a JSON object")
    })?;

    // Required top-level fields
    let required_fields = ["version", "repository", "seams", "metadata"];
    for field in &required_fields {
        if !obj.contains_key(*field) {
            bail!("Seam register missing required field: '{}'", field);
        }
    }

    // Version must be a string
    if !obj["version"].is_string() {
        bail!("Seam register 'version' must be a string");
    }

    // Repository must be a string
    if !obj["repository"].is_string() {
        bail!("Seam register 'repository' must be a string");
    }

    // Seams must be an array
    let seams = obj["seams"].as_array().ok_or_else(|| {
        anyhow::anyhow!("Seam register 'seams' must be an array")
    })?;

    // Validate each seam
    for (i, seam) in seams.iter().enumerate() {
        validate_seam_entry(seam, i)?;
    }

    // Metadata must be an object
    if !obj["metadata"].is_object() {
        bail!("Seam register 'metadata' must be an object");
    }

    Ok(())
}

/// Validate a single seam entry in the register
fn validate_seam_entry(seam: &serde_json::Value, index: usize) -> Result<()> {
    let obj = seam.as_object().ok_or_else(|| {
        anyhow::anyhow!("Seam at index {} must be a JSON object", index)
    })?;

    // Required fields
    let required = ["id", "name", "seam_type"];
    for field in &required {
        if !obj.contains_key(*field) {
            bail!("Seam at index {} missing required field: '{}'", index, field);
        }
    }

    // ID must be a non-empty string
    if let Some(id) = obj["id"].as_str() {
        if id.is_empty() {
            bail!("Seam at index {} has empty 'id'", index);
        }
    } else {
        bail!("Seam at index {} 'id' must be a string", index);
    }

    // Validate file paths in seam definitions for path traversal
    if let Some(paths) = obj.get("conformance_paths").and_then(|v| v.as_array()) {
        for path in paths {
            if let Some(p) = path.as_str() {
                if p.contains("..") {
                    bail!(
                        "Seam '{}' conformance path contains '..': path traversal rejected",
                        obj["id"].as_str().unwrap_or("unknown")
                    );
                }
            }
        }
    }

    if let Some(checklist) = obj.get("checklist_path").and_then(|v| v.as_str()) {
        if checklist.contains("..") {
            bail!(
                "Seam '{}' checklist_path contains '..': path traversal rejected",
                obj["id"].as_str().unwrap_or("unknown")
            );
        }
    }

    if let Some(boundary) = obj.get("boundary_path").and_then(|v| v.as_str()) {
        if boundary.contains("..") {
            bail!(
                "Seam '{}' boundary_path contains '..': path traversal rejected",
                obj["id"].as_str().unwrap_or("unknown")
            );
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_valid_relative_path() {
        let tmp = TempDir::new().unwrap();
        let result = validate_path("src/main.rs", tmp.path(), false);
        assert!(result.is_ok());
    }

    #[test]
    fn test_path_traversal_rejected() {
        let tmp = TempDir::new().unwrap();
        let result = validate_path("../../etc/passwd", tmp.path(), false);
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string().to_lowercase();
        assert!(err_msg.contains("path traversal") || err_msg.contains("traversal"),
            "Expected path traversal error, got: {}", err_msg);
    }

    #[test]
    fn test_absolute_path_rejected() {
        let tmp = TempDir::new().unwrap();
        let result = validate_path("/etc/passwd", tmp.path(), false);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Absolute path"));
    }

    #[test]
    fn test_null_byte_rejected() {
        let tmp = TempDir::new().unwrap();
        let result = validate_path("src/\0main.rs", tmp.path(), false);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("null bytes"));
    }

    #[test]
    fn test_empty_path_rejected() {
        let tmp = TempDir::new().unwrap();
        let result = validate_path("", tmp.path(), false);
        assert!(result.is_err());
    }

    #[test]
    fn test_valid_branch_name() {
        assert!(validate_branch_name("main").is_ok());
        assert!(validate_branch_name("feature/add-tests").is_ok());
        assert!(validate_branch_name("fix/123-bug").is_ok());
    }

    #[test]
    fn test_branch_traversal_rejected() {
        assert!(validate_branch_name("..").is_err());
        assert!(validate_branch_name("main/../../etc").is_err());
    }

    #[test]
    fn test_branch_shell_metachar_rejected() {
        assert!(validate_branch_name("main; rm -rf /").is_err());
        assert!(validate_branch_name("main`whoami`").is_err());
        assert!(validate_branch_name("main|cat /etc/passwd").is_err());
        assert!(validate_branch_name("main$HOME").is_err());
    }

    #[test]
    fn test_branch_dash_prefix_rejected() {
        assert!(validate_branch_name("-n").is_err());
        assert!(validate_branch_name("--force").is_err());
    }

    #[test]
    fn test_valid_register_json() {
        let register = serde_json::json!({
            "version": "1.0",
            "repository": "test-repo",
            "seams": [
                {
                    "id": "test-seam",
                    "name": "Test Seam",
                    "seam_type": "module"
                }
            ],
            "metadata": {
                "updated_at": "2026-01-01",
                "updated_by": "test"
            }
        });

        assert!(validate_register_json(&register).is_ok());
    }

    #[test]
    fn test_register_missing_field() {
        let register = serde_json::json!({
            "version": "1.0",
            "seams": []
        });

        let result = validate_register_json(&register);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("missing required field"));
    }

    #[test]
    fn test_register_path_traversal_in_conformance() {
        let register = serde_json::json!({
            "version": "1.0",
            "repository": "test",
            "seams": [
                {
                    "id": "evil-seam",
                    "name": "Evil",
                    "seam_type": "module",
                    "conformance_paths": ["../../etc/passwd"]
                }
            ],
            "metadata": {"updated_at": "", "updated_by": ""}
        });

        let result = validate_register_json(&register);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("path traversal"));
    }

    #[test]
    fn test_register_path_traversal_in_checklist() {
        let register = serde_json::json!({
            "version": "1.0",
            "repository": "test",
            "seams": [
                {
                    "id": "evil-seam",
                    "name": "Evil",
                    "seam_type": "module",
                    "checklist_path": "../../../etc/shadow"
                }
            ],
            "metadata": {"updated_at": "", "updated_by": ""}
        });

        let result = validate_register_json(&register);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("path traversal"));
    }

    #[test]
    fn test_register_path_traversal_in_boundary() {
        let register = serde_json::json!({
            "version": "1.0",
            "repository": "test",
            "seams": [
                {
                    "id": "evil-seam",
                    "name": "Evil",
                    "seam_type": "module",
                    "boundary_path": "../../sensitive"
                }
            ],
            "metadata": {"updated_at": "", "updated_by": ""}
        });

        let result = validate_register_json(&register);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("path traversal"));
    }
}
