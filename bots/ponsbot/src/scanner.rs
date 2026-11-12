// SPDX-License-Identifier: MPL-2.0
//! Subprocess wrapper for the `pons` binary.
//!
//! Every path out of here is an [`Outcome`] that is `Ran`, `Error` or
//! `NotRun`. There is no "pass" outcome and no way to turn a failure into an
//! empty `Ran`: a report is accepted only if pons exited 0 or 1, the report
//! parses and is well-formed, and pons stamps the pinned commit.
//!
//! Exit-code mapping (design, section 4):
//! - exit 0 or 1 with a valid report: `Ran` (1 means pons' threshold was
//!   reached; it is a finding, not a failure),
//! - exit 2 (invalid input, no applicable input, failed coverage): `Error`
//!   carrying pons' own message,
//! - any other exit, a signal, a timeout, an unreadable report, a binary at a
//!   different commit: `Error`,
//! - no binary: `NotRun`.
//!
//! pons runs with the scanned directory as its working directory and `.` as
//! the path; pons itself writes nothing (`scan` is read-only) and ponsbot adds
//! no exception mechanism: `.ponsignore` and `pons.toml` stay pons' business.
//! SPECULATIVE findings are not suppressed (no `--no-speculative`): they are
//! carried (owner ruling D1).

use crate::translator::PonsFinding;
use serde_json::Value;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::thread;
use std::time::{Duration, Instant};

/// What a scan amounted to. Never a bare pass.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Outcome {
    Ran(Vec<PonsFinding>),
    Error(String),
    NotRun(String),
}

/// Environment variable naming an explicit pons binary.
pub const PONS_BIN_ENV: &str = "PONS_BIN";

/// Locate pons: `$PONS_BIN`, then PATH, then the locations the estate's
/// workflows install it to. Err carries the reason it was not found.
pub fn find_pons() -> Result<PathBuf, String> {
    if let Some(p) = std::env::var_os(PONS_BIN_ENV).filter(|p| !p.is_empty()) {
        let p = PathBuf::from(p);
        return if is_executable(&p) {
            Ok(p)
        } else {
            Err(format!(
                "{PONS_BIN_ENV} is set to {} but that is not an executable file",
                p.display()
            ))
        };
    }
    if let Some(path) = std::env::var_os("PATH") {
        for dir in std::env::split_paths(&path) {
            let cand = dir.join("pons");
            if is_executable(&cand) {
                return Ok(cand);
            }
        }
    }
    if let Some(home) = std::env::var_os("HOME") {
        for rel in [".pons/bin/pons", ".cargo/bin/pons"] {
            let cand = Path::new(&home).join(rel);
            if is_executable(&cand) {
                return Ok(cand);
            }
        }
    }
    Err("pons is not installed (not on PATH, not at ~/.pons/bin/pons or ~/.cargo/bin/pons, PONS_BIN unset)".into())
}

fn is_executable(p: &Path) -> bool {
    use std::os::unix::fs::PermissionsExt;
    p.metadata()
        .map(|m| m.is_file() && m.permissions().mode() & 0o111 != 0)
        .unwrap_or(false)
}

/// Run pons on `dir`. `expected_commit` is the pin pons must report.
pub fn run_pons(dir: &Path, timeout: Duration, expected_commit: &str) -> Outcome {
    if !dir.is_dir() {
        return Outcome::Error(format!("{} is not a directory", dir.display()));
    }
    let bin = match find_pons() {
        Ok(b) => b,
        Err(reason) => return Outcome::NotRun(reason),
    };
    let mut child = match Command::new(&bin)
        .args(["scan", "--format", "json", "."])
        .current_dir(dir)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
    {
        Ok(c) => c,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            return Outcome::NotRun(format!(
                "pons binary {} vanished before it could run",
                bin.display()
            ))
        }
        Err(e) => return Outcome::Error(format!("could not start {}: {e}", bin.display())),
    };
    // Drain both pipes on threads so a large report cannot fill a pipe and
    // wedge the child before the deadline logic sees it.
    let (Some(mut out_pipe), Some(mut err_pipe)) = (child.stdout.take(), child.stderr.take())
    else {
        let _ = child.kill();
        let _ = child.wait();
        return Outcome::Error("pons pipes were not captured".to_string());
    };
    let out_t = thread::spawn(move || {
        let mut b = Vec::new();
        let _ = out_pipe.read_to_end(&mut b);
        b
    });
    let err_t = thread::spawn(move || {
        let mut b = Vec::new();
        let _ = err_pipe.read_to_end(&mut b);
        b
    });
    let deadline = Instant::now() + timeout;
    let status = loop {
        match child.try_wait() {
            Ok(Some(s)) => break Ok(s),
            Ok(None) if Instant::now() >= deadline => {
                let _ = child.kill();
                let _ = child.wait();
                break Err(());
            }
            Ok(None) => thread::sleep(Duration::from_millis(20)),
            Err(e) => {
                let _ = child.kill();
                let _ = child.wait();
                let _ = out_t.join();
                let _ = err_t.join();
                return Outcome::Error(format!("could not wait for pons: {e}"));
            }
        }
    };
    let stdout = String::from_utf8_lossy(&out_t.join().unwrap_or_default()).into_owned();
    let stderr = String::from_utf8_lossy(&err_t.join().unwrap_or_default()).into_owned();
    match status {
        Err(()) => Outcome::Error(format!("pons did not finish within {}s", timeout.as_secs())),
        Ok(s) => interpret(s.code(), &stdout, &stderr, expected_commit),
    }
}

/// First nonblank stderr line, at most 240 characters.
fn first_line(stderr: &str) -> String {
    stderr
        .lines()
        .map(str::trim)
        .find(|l| !l.is_empty())
        .map(|l| l.chars().take(240).collect())
        .unwrap_or_else(|| "no diagnostic".into())
}

/// Map pons' exit status, stdout and stderr to an [`Outcome`]. Pure, so every
/// failure mode is unit-testable without a pons binary.
pub fn interpret(code: Option<i32>, stdout: &str, stderr: &str, expected_commit: &str) -> Outcome {
    let code = match code {
        Some(c) => c,
        None => {
            return Outcome::Error(format!(
                "pons was terminated by a signal: {}",
                first_line(stderr)
            ))
        }
    };
    let report: Option<Value> = serde_json::from_str(stdout).ok();
    match code {
        0 | 1 => match validate(report.as_ref(), expected_commit) {
            Ok(findings) => Outcome::Ran(findings),
            Err(why) => Outcome::Error(format!("pons exited {code} without a valid report: {why}")),
        },
        2 => {
            let coverage = report
                .as_ref()
                .and_then(|r| r.pointer("/scanned/coverage_status"))
                .and_then(Value::as_str)
                .map(|c| format!(" ({c})"))
                .unwrap_or_default();
            Outcome::Error(format!("pons exited 2{coverage}: {}", first_line(stderr)))
        }
        n => Outcome::Error(format!("pons exited {n}: {}", first_line(stderr))),
    }
}

/// Validate a pons scan report and extract its findings. A report that is not
/// exactly what pons documents is an error, never an empty finding list.
fn validate(report: Option<&Value>, expected_commit: &str) -> Result<Vec<PonsFinding>, String> {
    let r = report.ok_or("stdout is not JSON")?;
    let tool = r
        .get("tool")
        .and_then(Value::as_object)
        .ok_or("no tool block")?;
    match tool.get("source_commit").and_then(Value::as_str) {
        Some(c) if c == expected_commit => {}
        Some(c) => {
            return Err(format!("pons reports commit {c}, ponsbot pins {expected_commit}; refusing to report"))
        }
        None => {
            return Err(format!("pons does not report its source commit, so the pin {expected_commit} cannot be verified"))
        }
    }
    if tool.get("source_dirty").and_then(Value::as_str) == Some("true") {
        return Err("pons was built from a dirty tree, so it is not the pinned commit".into());
    }
    let arr = r
        .get("findings")
        .and_then(Value::as_array)
        .ok_or("no findings array")?;
    arr.iter().enumerate().map(|(i, f)| finding(i, f)).collect()
}

fn finding(i: usize, f: &Value) -> Result<PonsFinding, String> {
    let s = |k: &str| {
        f.get(k)
            .and_then(Value::as_str)
            .map(str::to_owned)
            .ok_or_else(|| format!("finding {i} has no string `{k}`"))
    };
    let severity = s("severity")?.to_ascii_uppercase();
    if !matches!(severity.as_str(), "INFO" | "WARN" | "ERROR") {
        return Err(format!("finding {i} has unknown severity {severity}"));
    }
    let loc = f
        .get("location")
        .ok_or_else(|| format!("finding {i} has no location"))?;
    let file = loc
        .get("file")
        .and_then(Value::as_str)
        .ok_or_else(|| format!("finding {i} has no location.file"))?;
    let line = loc.get("line_start").and_then(Value::as_u64).unwrap_or(0);
    let opt = |k: &str| f.get(k).and_then(Value::as_str).map(str::to_owned);
    Ok(PonsFinding {
        rule_id: s("rule_id")?,
        severity,
        tier: s("tier")?,
        evidence: s("evidence")?.to_ascii_uppercase(),
        file: file.strip_prefix("./").unwrap_or(file).to_owned(),
        line,
        message: s("message")?,
        evidence_note: opt("evidence_note"),
        counter_condition: opt("counter_condition"),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    const PIN: &str = "bca1ea289a4f8a86bac6212731e6f8ec15c67ad4";

    fn report(findings: &str) -> String {
        format!(
            r#"{{"tool":{{"name":"pons","source_commit":"{PIN}","source_dirty":"false"}},"findings":{findings}}}"#
        )
    }
    const ONE: &str = r#"[{"rule_id":"constant-condition","tier":"T0","evidence":"HEURISTIC","severity":"WARN","location":{"file":"./a.js","line_start":4},"message":"constant condition","evidence_note":"n"}]"#;

    #[test]
    fn exit_0_with_findings_is_ran_and_keeps_tier_and_evidence() {
        match interpret(Some(0), &report(ONE), "", PIN) {
            Outcome::Ran(v) => {
                assert_eq!(v.len(), 1);
                assert_eq!(
                    (v[0].tier.as_str(), v[0].evidence.as_str()),
                    ("T0", "HEURISTIC")
                );
                assert_eq!((v[0].file.as_str(), v[0].line), ("a.js", 4));
            }
            o => panic!("{o:?}"),
        }
    }

    #[test]
    fn exit_1_with_findings_is_ran() {
        assert!(
            matches!(interpret(Some(1), &report(ONE), "", PIN), Outcome::Ran(v) if v.len() == 1)
        );
    }

    #[test]
    fn exit_2_is_error_with_pons_message() {
        let o = interpret(Some(2), "", "pons: no such path: /x\n", PIN);
        assert_eq!(
            o,
            Outcome::Error("pons exited 2: pons: no such path: /x".into())
        );
    }

    #[test]
    fn exit_2_names_the_coverage_status_when_the_report_has_one() {
        let out = r#"{"scanned":{"coverage_status":"no-applicable-input"}}"#;
        match interpret(Some(2), out, "pons: scanned 0 files\n", PIN) {
            Outcome::Error(r) => assert!(
                r.contains("no-applicable-input") && r.contains("scanned 0 files"),
                "{r}"
            ),
            o => panic!("{o:?}"),
        }
    }

    #[test]
    fn exit_0_without_a_valid_report_is_error_not_empty_ran() {
        for bad in [
            "",
            "not json",
            "{}",
            "[]",
            "42",
            r#"{"tool":{},"findings":[]}"#,
        ] {
            assert!(
                matches!(interpret(Some(0), bad, "", PIN), Outcome::Error(_)),
                "{bad}"
            );
        }
        // a findings array that is missing is not an empty one
        let no_findings = format!(r#"{{"tool":{{"source_commit":"{PIN}"}}}}"#);
        assert!(matches!(
            interpret(Some(0), &no_findings, "", PIN),
            Outcome::Error(_)
        ));
    }

    #[test]
    fn malformed_finding_is_error() {
        let bad = r#"[{"rule_id":"x","tier":"T0","evidence":"HEURISTIC","severity":"LOUD","location":{"file":"a"},"message":"m"}]"#;
        assert!(
            matches!(interpret(Some(0), &report(bad), "", PIN), Outcome::Error(r) if r.contains("unknown severity"))
        );
        let no_ev = r#"[{"rule_id":"x","tier":"T0","severity":"WARN","location":{"file":"a"},"message":"m"}]"#;
        assert!(
            matches!(interpret(Some(0), &report(no_ev), "", PIN), Outcome::Error(r) if r.contains("evidence"))
        );
    }

    #[test]
    fn wrong_or_missing_commit_is_error() {
        let other = report("[]").replace(PIN, "0000000000000000000000000000000000000000");
        assert!(
            matches!(interpret(Some(0), &other, "", PIN), Outcome::Error(r) if r.contains("refusing"))
        );
        let none = r#"{"tool":{"name":"pons"},"findings":[]}"#;
        assert!(
            matches!(interpret(Some(0), none, "", PIN), Outcome::Error(r) if r.contains("cannot be verified"))
        );
        let dirty = report("[]").replace("\"false\"", "\"true\"");
        assert!(
            matches!(interpret(Some(0), &dirty, "", PIN), Outcome::Error(r) if r.contains("dirty"))
        );
    }

    #[test]
    fn other_exits_and_signals_are_errors() {
        assert!(
            matches!(interpret(Some(101), &report("[]"), "panicked\n", PIN), Outcome::Error(r) if r.contains("101"))
        );
        assert!(matches!(interpret(None, "", "", PIN), Outcome::Error(_)));
    }

    #[test]
    fn a_clean_valid_report_is_ran_with_no_findings() {
        assert_eq!(
            interpret(Some(0), &report("[]"), "", PIN),
            Outcome::Ran(vec![])
        );
    }
}
