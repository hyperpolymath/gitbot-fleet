// SPDX-License-Identifier: MPL-2.0
//! End-to-end tests of the `ponsbot` binary against a stub `pons` shell script.
//!
//! The stub prints canned reports in pons' documented shape (schema 0.4.0,
//! taken from a live run of pons at the pinned commit). Nothing here proves
//! pons' own analysis; `real_pons_*` runs the genuine binary when
//! `PONSBOT_REAL_PONS` names one, and says so when it skips.

use serde_json::Value;
use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use tempfile::TempDir;

fn pin_commit() -> String {
    ponsbot::pin::pinned().map(|p| p.commit).unwrap_or_default()
}

fn stub(dir: &Path, body: &str) -> PathBuf {
    let p = dir.join("pons");
    fs::write(&p, format!("#!/usr/bin/env bash\n{body}\n")).unwrap();
    fs::set_permissions(&p, fs::Permissions::from_mode(0o755)).unwrap();
    p
}

fn report(findings: &str) -> String {
    format!(
        r#"{{"schema_version":"0.4.0","tool":{{"name":"pons","version":"0.1.0","source_commit":"{}","source_dirty":"false"}},"findings":{findings}}}"#,
        pin_commit()
    )
}

fn emit(json: &str, code: i32) -> String {
    format!("cat <<'J'\n{json}\nJ\nexit {code}")
}

fn ponsbot(pons: &Path, scan: &Path, extra: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_ponsbot"))
        .env_clear()
        .env("PATH", "/usr/bin:/bin")
        .env("PONS_BIN", pons)
        .args(["--format", "json", "scan"])
        .arg(scan)
        .args(extra)
        .output()
        .unwrap()
}

fn tree() -> TempDir {
    let t = TempDir::new().unwrap();
    fs::write(t.path().join("a.js"), "if (true) {}\n").unwrap();
    t
}

fn out_json(o: &Output) -> Value {
    serde_json::from_slice(&o.stdout).unwrap_or_else(|e| {
        panic!(
            "stdout not JSON: {e}: {}",
            String::from_utf8_lossy(&o.stdout)
        )
    })
}

const WARN: &str = r#"{"rule_id":"constant-condition","tier":"T0","evidence":"HEURISTIC","severity":"WARN","location":{"file":"./a.js","line_start":1},"message":"constant condition","evidence_note":"`if (true)`"}"#;
const ERR: &str = r#"{"rule_id":"div-by-literal-zero","tier":"T1","evidence":"DATAFLOW","severity":"ERROR","location":{"file":"./a.js","line_start":2},"message":"division by zero"}"#;
const SPEC: &str = r#"{"rule_id":"spec-rule","tier":"T3","evidence":"SPECULATIVE","severity":"ERROR","location":{"file":"./a.js","line_start":3},"message":"maybe"}"#;

#[test]
fn findings_are_printed_with_tier_and_evidence_class() {
    let (bin, scan) = (TempDir::new().unwrap(), tree());
    let pons = stub(bin.path(), &emit(&report(&format!("[{WARN}]")), 0));
    let o = ponsbot(&pons, scan.path(), &[]);
    assert_eq!(o.status.code(), Some(0));
    let v = out_json(&o);
    let f = &v[0];
    assert_eq!(f["rule_id"], "constant-condition");
    assert_eq!(f["tier"], "T0");
    assert_eq!(f["evidence_class"], "HEURISTIC");
    assert_eq!(f["severity"], "warning");
    assert_eq!(f["file"], "a.js");
    assert_eq!(f["line"], 1);
}

#[test]
fn a_blocking_finding_exits_1_but_is_still_a_findings_array() {
    let (bin, scan) = (TempDir::new().unwrap(), tree());
    let pons = stub(bin.path(), &emit(&report(&format!("[{ERR}]")), 1));
    let o = ponsbot(&pons, scan.path(), &[]);
    assert_eq!(o.status.code(), Some(1));
    assert_eq!(out_json(&o)[0]["severity"], "error");
    assert_eq!(out_json(&o)[0]["blocking"], true);
}

#[test]
fn speculative_findings_are_carried_and_never_blocking() {
    let (bin, scan) = (TempDir::new().unwrap(), tree());
    let pons = stub(bin.path(), &emit(&report(&format!("[{SPEC}]")), 0));
    let o = ponsbot(&pons, scan.path(), &[]);
    assert_eq!(
        o.status.code(),
        Some(0),
        "a speculative finding must not make ponsbot exit 1"
    );
    let f = &out_json(&o)[0];
    assert_eq!(f["evidence_class"], "SPECULATIVE");
    assert_eq!(f["pons_severity"], "ERROR");
    assert_eq!(f["severity"], "info");
    assert_eq!(f["blocking"], false);
}

#[test]
fn a_clean_scan_is_an_empty_array_and_exit_0() {
    let (bin, scan) = (TempDir::new().unwrap(), tree());
    let pons = stub(bin.path(), &emit(&report("[]"), 0));
    let o = ponsbot(&pons, scan.path(), &[]);
    assert_eq!(
        (o.status.code(), out_json(&o)),
        (Some(0), serde_json::json!([]))
    );
}

#[test]
fn pons_exit_2_is_error_with_pons_message_never_a_pass() {
    let (bin, scan) = (TempDir::new().unwrap(), tree());
    let pons = stub(bin.path(), "echo 'pons: no applicable input' >&2; exit 2");
    let o = ponsbot(&pons, scan.path(), &[]);
    assert_eq!(o.status.code(), Some(2));
    let v = out_json(&o);
    assert_eq!(v["status"], "error");
    assert!(
        v["reason"]
            .as_str()
            .unwrap()
            .contains("no applicable input"),
        "{v}"
    );
}

#[test]
fn missing_binary_is_not_run() {
    let scan = tree();
    let home = TempDir::new().unwrap();
    let o = Command::new(env!("CARGO_BIN_EXE_ponsbot"))
        .env_clear()
        .env("PATH", "")
        .env("HOME", home.path())
        .args(["--format", "json", "scan"])
        .arg(scan.path())
        .output()
        .unwrap();
    assert_eq!(o.status.code(), Some(3));
    let v = out_json(&o);
    assert_eq!(v["status"], "not_run");
    assert!(v["reason"].as_str().unwrap().contains("not installed"));
}

#[test]
fn explicit_binary_that_does_not_exist_is_not_run() {
    let scan = tree();
    let o = ponsbot(Path::new("/nonexistent/pons"), scan.path(), &[]);
    assert_eq!(o.status.code(), Some(3));
    assert_eq!(out_json(&o)["status"], "not_run");
}

#[test]
fn timeout_is_error_not_a_hang_and_not_empty_ran() {
    let (bin, scan) = (TempDir::new().unwrap(), tree());
    let pons = stub(bin.path(), "exec sleep 30");
    let t = std::time::Instant::now();
    let o = ponsbot(&pons, scan.path(), &["--timeout", "1"]);
    assert!(t.elapsed().as_secs() < 10, "the timeout must be enforced");
    assert_eq!(o.status.code(), Some(2));
    assert!(out_json(&o)["reason"]
        .as_str()
        .unwrap()
        .contains("did not finish within 1s"));
}

#[test]
fn exit_0_without_a_valid_report_is_error() {
    let (bin, scan) = (TempDir::new().unwrap(), tree());
    for body in [
        "exit 0",
        "echo '{}'; exit 0",
        "echo 'garbage'; exit 0",
        "echo '[]'; exit 0",
    ] {
        let pons = stub(bin.path(), body);
        let o = ponsbot(&pons, scan.path(), &[]);
        assert_eq!(o.status.code(), Some(2), "{body}");
        assert_eq!(out_json(&o)["status"], "error", "{body}");
    }
}

#[test]
fn a_pons_at_another_commit_is_refused() {
    let (bin, scan) = (TempDir::new().unwrap(), tree());
    let other = report("[]").replace(&pin_commit(), "1111111111111111111111111111111111111111");
    let pons = stub(bin.path(), &emit(&other, 0));
    let o = ponsbot(&pons, scan.path(), &[]);
    assert_eq!(o.status.code(), Some(2));
    assert!(out_json(&o)["reason"]
        .as_str()
        .unwrap()
        .contains("refusing to report"));
}

#[test]
fn a_directory_that_does_not_exist_is_error() {
    let bin = TempDir::new().unwrap();
    let pons = stub(bin.path(), &emit(&report("[]"), 0));
    let o = ponsbot(&pons, Path::new("/nonexistent/dir"), &[]);
    assert_eq!(o.status.code(), Some(2));
}

fn snapshot(dir: &Path) -> Vec<(String, Vec<u8>)> {
    let mut v: Vec<_> = walk(dir)
        .into_iter()
        .map(|p| {
            (
                p.strip_prefix(dir).unwrap().display().to_string(),
                fs::read(&p).unwrap_or_default(),
            )
        })
        .collect();
    v.sort();
    v
}
fn walk(d: &Path) -> Vec<PathBuf> {
    let mut out = vec![];
    for e in fs::read_dir(d).unwrap() {
        let p = e.unwrap().path();
        out.push(p.clone());
        if p.is_dir() {
            out.extend(walk(&p));
        }
    }
    out
}

#[test]
fn the_scanned_tree_is_left_untouched_and_pons_runs_with_the_documented_args() {
    let (bin, scan) = (TempDir::new().unwrap(), tree());
    let argsfile = bin.path().join("args");
    let body = format!(
        "echo \"$*\" > '{}'; pwd -P >> '{}'\n{}",
        argsfile.display(),
        argsfile.display(),
        emit(&report(&format!("[{WARN}]")), 0)
    );
    let pons = stub(bin.path(), &body);
    let before = snapshot(scan.path());
    let o = ponsbot(&pons, scan.path(), &[]);
    assert_eq!(o.status.code(), Some(0));
    assert_eq!(
        before,
        snapshot(scan.path()),
        "ponsbot wrote into the scanned tree"
    );
    let args = fs::read_to_string(&argsfile).unwrap();
    let mut lines = args.lines();
    assert_eq!(
        lines.next(),
        Some("scan --format json ."),
        "no exception flags and no --no-speculative"
    );
    assert_eq!(
        Path::new(lines.next().unwrap()),
        scan.path().canonicalize().unwrap()
    );
}

/// Runs only when `PONSBOT_REAL_PONS` names a pons built at the pinned commit.
#[test]
fn real_pons_end_to_end_when_available() {
    let Some(real) = std::env::var_os("PONSBOT_REAL_PONS") else {
        eprintln!("SKIP: PONSBOT_REAL_PONS unset; the real-binary path is unverified by this run");
        return;
    };
    let scan = TempDir::new().unwrap();
    fs::write(scan.path().join("a.js"), "if (true) { f(); }\n").unwrap();
    let before = snapshot(scan.path());
    let o = ponsbot(Path::new(&real), scan.path(), &[]);
    assert_eq!(
        o.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&o.stderr)
    );
    let v = out_json(&o);
    assert_eq!(v[0]["rule_id"], "constant-condition");
    assert_eq!(v[0]["evidence_class"], "HEURISTIC");
    assert_eq!(v[0]["file"], "a.js");
    assert_eq!(before, snapshot(scan.path()));
    // a tree with no analysable source: pons exits 2, which must be an error
    let empty = TempDir::new().unwrap();
    let o = ponsbot(Path::new(&real), empty.path(), &[]);
    assert_eq!(o.status.code(), Some(2));
    assert_eq!(out_json(&o)["status"], "error");
}

/// pons is spawned with the scanned directory as its cwd, so a relative
/// `PONS_BIN`, or a relative `PATH` entry, must be made absolute first.
fn relative_run(env: &[(&str, &str)], cwd: &Path, scan: &str) -> Output {
    let mut c = Command::new(env!("CARGO_BIN_EXE_ponsbot"));
    c.env_clear().env("PATH", "/usr/bin:/bin");
    for (k, v) in env {
        c.env(k, v);
    }
    c.current_dir(cwd)
        .args(["--format", "json", "scan", scan])
        .output()
        .unwrap()
}

#[test]
fn a_relative_pons_bin_is_resolved_before_the_cwd_changes() {
    let root = TempDir::new().unwrap();
    fs::create_dir(root.path().join("bin")).unwrap();
    fs::create_dir(root.path().join("tree")).unwrap();
    fs::write(root.path().join("tree/a.js"), "if (true) {}\n").unwrap();
    stub(
        &root.path().join("bin"),
        &emit(&report(&format!("[{WARN}]")), 0),
    );
    let o = relative_run(&[("PONS_BIN", "bin/pons")], root.path(), "tree");
    assert_eq!(
        o.status.code(),
        Some(0),
        "stdout: {} stderr: {}",
        String::from_utf8_lossy(&o.stdout),
        String::from_utf8_lossy(&o.stderr)
    );
    assert_eq!(out_json(&o)[0]["rule_id"], "constant-condition");
}

#[test]
fn a_relative_path_entry_is_resolved_before_the_cwd_changes() {
    let root = TempDir::new().unwrap();
    fs::create_dir(root.path().join("bin")).unwrap();
    fs::create_dir(root.path().join("tree")).unwrap();
    fs::write(root.path().join("tree/a.js"), "if (true) {}\n").unwrap();
    stub(
        &root.path().join("bin"),
        &emit(&report(&format!("[{WARN}]")), 0),
    );
    let o = relative_run(&[("PATH", "bin:/usr/bin:/bin")], root.path(), "tree");
    assert_eq!(
        o.status.code(),
        Some(0),
        "stdout: {} stderr: {}",
        String::from_utf8_lossy(&o.stdout),
        String::from_utf8_lossy(&o.stderr)
    );
    assert_eq!(out_json(&o)[0]["rule_id"], "constant-condition");
}
