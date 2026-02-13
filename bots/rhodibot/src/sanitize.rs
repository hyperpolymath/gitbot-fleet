// SPDX-License-Identifier: PMPL-1.0-or-later

//! Input sanitization and validation module.
//!
//! Provides validation for GitHub usernames, repository names, file paths,
//! and markdown content to prevent injection and path traversal attacks.
//!
//! # Security guarantees
//!
//! - Repository and owner names are validated against GitHub's naming rules
//! - File paths are checked for path traversal sequences (`..`, absolute paths)
//! - Markdown output sanitizes user-controlled content to prevent injection

use anyhow::{bail, Result};

/// Maximum allowed length for GitHub owner/repo names.
/// GitHub allows up to 100 characters for org/user names and 100 for repo names.
const MAX_NAME_LENGTH: usize = 100;

/// Validate a GitHub username or organization name.
///
/// GitHub names must:
/// - Be 1-100 characters
/// - Contain only alphanumeric characters, hyphens, or periods
/// - Not start or end with a hyphen
/// - Not contain consecutive hyphens
pub fn validate_github_name(name: &str, field_label: &str) -> Result<()> {
    if name.is_empty() {
        bail!("Invalid {}: name cannot be empty", field_label);
    }

    if name.len() > MAX_NAME_LENGTH {
        bail!(
            "Invalid {}: name exceeds maximum length of {} characters",
            field_label,
            MAX_NAME_LENGTH
        );
    }

    if name.starts_with('-') || name.ends_with('-') {
        bail!(
            "Invalid {}: '{}' cannot start or end with a hyphen",
            field_label,
            name
        );
    }

    if name.contains("--") {
        bail!(
            "Invalid {}: '{}' cannot contain consecutive hyphens",
            field_label,
            name
        );
    }

    // GitHub allows: alphanumeric, hyphen, period (for usernames), underscore
    for ch in name.chars() {
        if !ch.is_ascii_alphanumeric() && ch != '-' && ch != '.' && ch != '_' {
            bail!(
                "Invalid {}: '{}' contains disallowed character '{}'",
                field_label,
                name,
                ch
            );
        }
    }

    Ok(())
}

/// Validate both owner and repository name from a webhook payload.
pub fn validate_owner_repo(owner: &str, repo: &str) -> Result<()> {
    validate_github_name(owner, "repository owner")?;
    validate_github_name(repo, "repository name")?;
    Ok(())
}

/// Validate a file path to prevent path traversal attacks.
///
/// Rejects paths that:
/// - Contain `..` (parent directory traversal)
/// - Start with `/` (absolute paths)
/// - Contain null bytes
/// - Are empty
pub fn validate_file_path(file_path: &str) -> Result<()> {
    if file_path.is_empty() {
        bail!("File path cannot be empty");
    }

    if file_path.contains('\0') {
        bail!("File path contains null byte");
    }

    if file_path.starts_with('/') {
        bail!("File path cannot be absolute: '{}'", file_path);
    }

    // Check for path traversal sequences
    for component in file_path.split('/') {
        if component == ".." {
            bail!(
                "File path contains traversal sequence: '{}'",
                file_path
            );
        }
    }

    Ok(())
}

/// Sanitize a string for safe inclusion in GitHub markdown output.
///
/// This prevents markdown injection by escaping characters that could
/// alter the document structure when user-controlled content (like
/// repository names or file paths) is included in check run output.
///
/// Specifically handles:
/// - HTML tags (angle brackets)
/// - Markdown link/image syntax
/// - Code block delimiters
pub fn sanitize_markdown(input: &str) -> String {
    let mut output = String::with_capacity(input.len());

    for ch in input.chars() {
        match ch {
            '<' => output.push_str("&lt;"),
            '>' => output.push_str("&gt;"),
            '`' => output.push_str("\\`"),
            '[' => output.push_str("\\["),
            ']' => output.push_str("\\]"),
            _ => output.push(ch),
        }
    }

    output
}

#[cfg(test)]
mod tests {
    use super::*;

    // ---- GitHub name validation ----

    #[test]
    fn test_valid_github_names() {
        assert!(validate_github_name("hyperpolymath", "owner").is_ok());
        assert!(validate_github_name("test-org", "owner").is_ok());
        assert!(validate_github_name("user.name", "owner").is_ok());
        assert!(validate_github_name("a", "owner").is_ok());
        assert!(validate_github_name("my_repo", "repo").is_ok());
    }

    #[test]
    fn test_empty_name_rejected() {
        assert!(validate_github_name("", "owner").is_err());
    }

    #[test]
    fn test_too_long_name_rejected() {
        let long_name = "a".repeat(MAX_NAME_LENGTH + 1);
        assert!(validate_github_name(&long_name, "owner").is_err());
    }

    #[test]
    fn test_leading_hyphen_rejected() {
        assert!(validate_github_name("-badname", "owner").is_err());
    }

    #[test]
    fn test_trailing_hyphen_rejected() {
        assert!(validate_github_name("badname-", "owner").is_err());
    }

    #[test]
    fn test_consecutive_hyphens_rejected() {
        assert!(validate_github_name("bad--name", "owner").is_err());
    }

    #[test]
    fn test_special_characters_rejected() {
        assert!(validate_github_name("bad/name", "owner").is_err());
        assert!(validate_github_name("bad name", "owner").is_err());
        assert!(validate_github_name("bad@name", "owner").is_err());
        assert!(validate_github_name("bad\x00name", "owner").is_err());
        assert!(validate_github_name("bad;name", "owner").is_err());
    }

    #[test]
    fn test_validate_owner_repo_both_valid() {
        assert!(validate_owner_repo("hyperpolymath", "rhodibot").is_ok());
    }

    #[test]
    fn test_validate_owner_repo_bad_owner() {
        assert!(validate_owner_repo("", "rhodibot").is_err());
    }

    #[test]
    fn test_validate_owner_repo_bad_repo() {
        assert!(validate_owner_repo("hyperpolymath", "bad/repo").is_err());
    }

    // ---- File path validation ----

    #[test]
    fn test_valid_file_paths() {
        assert!(validate_file_path("README.adoc").is_ok());
        assert!(validate_file_path(".github/workflows/ci.yml").is_ok());
        assert!(validate_file_path(".claude/CLAUDE.md").is_ok());
        assert!(validate_file_path("src/main.rs").is_ok());
    }

    #[test]
    fn test_empty_path_rejected() {
        assert!(validate_file_path("").is_err());
    }

    #[test]
    fn test_null_byte_rejected() {
        assert!(validate_file_path("file\0.txt").is_err());
    }

    #[test]
    fn test_absolute_path_rejected() {
        assert!(validate_file_path("/etc/passwd").is_err());
    }

    #[test]
    fn test_path_traversal_rejected() {
        assert!(validate_file_path("../../../etc/passwd").is_err());
        assert!(validate_file_path("foo/../bar").is_err());
        assert!(validate_file_path("foo/bar/..").is_err());
    }

    #[test]
    fn test_dotfile_not_rejected() {
        // Single dots in filenames are fine (e.g., .gitignore, .editorconfig)
        assert!(validate_file_path(".gitignore").is_ok());
        assert!(validate_file_path(".github/workflows").is_ok());
    }

    // ---- Markdown sanitization ----

    #[test]
    fn test_sanitize_plain_text_unchanged() {
        assert_eq!(sanitize_markdown("hello world"), "hello world");
        assert_eq!(sanitize_markdown("README.adoc"), "README.adoc");
    }

    #[test]
    fn test_sanitize_html_tags() {
        assert_eq!(sanitize_markdown("<script>alert(1)</script>"), "&lt;script&gt;alert(1)&lt;/script&gt;");
    }

    #[test]
    fn test_sanitize_markdown_links() {
        assert_eq!(sanitize_markdown("[click](http://evil.com)"), "\\[click\\](http://evil.com)");
    }

    #[test]
    fn test_sanitize_code_blocks() {
        assert_eq!(sanitize_markdown("```injection"), "\\`\\`\\`injection");
    }

    #[test]
    fn test_sanitize_mixed_content() {
        let input = "<b>bold</b> [link](url) `code`";
        let output = sanitize_markdown(input);
        // HTML angle brackets should be entity-escaped
        assert!(!output.contains('<'), "should not contain raw '<'");
        assert!(!output.contains('>'), "should not contain raw '>'");
        // Markdown brackets should be backslash-escaped
        assert!(output.contains("\\["), "should escape '['");
        assert!(output.contains("\\]"), "should escape ']'");
        // Backticks should be backslash-escaped
        assert!(output.contains("\\`"), "should escape '`'");
        // Verify the full output
        assert_eq!(
            output,
            "&lt;b&gt;bold&lt;/b&gt; \\[link\\](url) \\`code\\`"
        );
    }
}
