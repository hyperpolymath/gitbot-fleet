// SPDX-License-Identifier: MPL-2.0
//! Translate findings/v0 (reconcile's output) into the fleet's finding shape.
//!
//! Mapping (sillybot spec v0):
//! - `path` to `file` (repo-relative, no `./`); every finding carries one,
//! - `span.start.line` to `line`,
//! - `class` to `rule_id` as `SB-<class>`,
//! - `severity` unchanged (`error`, `warning`, `info`),
//! - `message` unchanged,
//! - an inherited finding (`introduced: false`) is capped at `info`, so the
//!   grid's change-scope grading never blocks on it. Its own severity stays in
//!   `reconcile_severity`.
//!
//! A report that does not match this shape is rejected as a whole: one bad
//! finding means the report is not trusted, so nothing is silently dropped.

use serde::Serialize;
use serde_json::Value;

/// One finding as printed on stdout.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct Finding {
    pub rule_id: String,
    /// Fleet word the grid grades: `error`, `warning` or `info`.
    pub severity: String,
    /// reconcile's own severity, unchanged.
    pub reconcile_severity: String,
    pub file: String,
    pub line: u64,
    pub message: String,
    /// False when the finding was already present at the base revision.
    pub introduced: bool,
    /// True only for an introduced `error`.
    pub blocking: bool,
    /// reconcile's stable finding id.
    pub id: String,
}

fn str_field<'a>(v: &'a Value, key: &str, i: usize) -> Result<&'a str, String> {
    v.get(key)
        .and_then(Value::as_str)
        .ok_or_else(|| format!("finding {i}: missing string field '{key}'"))
}

/// A safe repo-relative path: not empty, not absolute, no `..` or empty
/// component, no control characters. A leading `./` is dropped.
fn clean_path(p: &str) -> Option<String> {
    let p = p.strip_prefix("./").unwrap_or(p);
    if p.is_empty() || p.starts_with('/') || p.chars().any(char::is_control) {
        return None;
    }
    if p.split('/').any(|c| c.is_empty() || c == "." || c == "..") {
        return None;
    }
    Some(p.to_string())
}

/// Translate a whole findings/v0 document.
pub fn translate_report(report: &Value) -> Result<Vec<Finding>, String> {
    let list = report
        .get("findings")
        .and_then(Value::as_array)
        .ok_or("reconcile output has no 'findings' array")?;
    list.iter()
        .enumerate()
        .map(|(i, f)| translate_one(f, i))
        .collect()
}

fn translate_one(f: &Value, i: usize) -> Result<Finding, String> {
    let class = str_field(f, "class", i)?;
    if class.is_empty()
        || !class
            .bytes()
            .all(|b| b.is_ascii_lowercase() || b.is_ascii_digit() || b == b'-')
    {
        return Err(format!(
            "finding {i}: class '{class}' is not a lowercase slug"
        ));
    }
    let sev = str_field(f, "severity", i)?;
    if !matches!(sev, "error" | "warning" | "info") {
        return Err(format!("finding {i}: unknown severity '{sev}'"));
    }
    let file = clean_path(str_field(f, "path", i)?)
        .ok_or_else(|| format!("finding {i}: path is not a safe repo-relative path"))?;
    let line = f
        .pointer("/span/start/line")
        .and_then(Value::as_u64)
        .filter(|&l| l >= 1)
        .ok_or_else(|| format!("finding {i}: span.start.line must be a positive integer"))?;
    let message = str_field(f, "message", i)?.to_string();
    let introduced = f
        .get("introduced")
        .and_then(Value::as_bool)
        .ok_or_else(|| format!("finding {i}: missing boolean field 'introduced'"))?;
    let id = str_field(f, "id", i)?.to_string();
    let severity = if introduced { sev } else { "info" };
    Ok(Finding {
        rule_id: format!("SB-{class}"),
        severity: severity.to_string(),
        reconcile_severity: sev.to_string(),
        file,
        line,
        message,
        introduced,
        blocking: introduced && sev == "error",
        id,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn finding(introduced: bool, sev: &str, path: &str) -> Value {
        json!({
            "class": "dead-internal-link", "severity": sev, "path": path,
            "span": {"start": {"line": 5, "col": 1}, "end": {"line": 5, "col": 20}},
            "message": "m", "introduced": introduced, "id": "abc", "tool": "sillybot"
        })
    }

    #[test]
    fn introduced_error_blocks() {
        let out = translate_report(&json!({"findings": [finding(true, "error", "README.adoc")]}))
            .unwrap();
        assert_eq!(out[0].rule_id, "SB-dead-internal-link");
        assert_eq!(out[0].severity, "error");
        assert_eq!(out[0].line, 5);
        assert!(out[0].blocking);
    }

    #[test]
    fn inherited_is_capped_at_info() {
        let out = translate_report(&json!({"findings": [finding(false, "error", "./docs/a.md")]}))
            .unwrap();
        assert_eq!(out[0].severity, "info");
        assert_eq!(out[0].reconcile_severity, "error");
        assert_eq!(out[0].file, "docs/a.md");
        assert!(!out[0].blocking);
    }

    #[test]
    fn bad_paths_reject_the_report() {
        for p in [
            "",
            "/etc/passwd",
            "../x.md",
            "a//b.md",
            "a/./b.md",
            "a\nb.md",
        ] {
            assert!(
                translate_report(&json!({"findings": [finding(true, "error", p)]})).is_err(),
                "{p:?} accepted"
            );
        }
    }

    #[test]
    fn missing_fields_reject_the_report() {
        let mut f = finding(true, "error", "a.md");
        f.as_object_mut().unwrap().remove("introduced");
        assert!(translate_report(&json!({"findings": [f]})).is_err());
        assert!(translate_report(&json!({"findings": [finding(true, "fatal", "a.md")]})).is_err());
        assert!(translate_report(&json!({})).is_err());
    }
}
