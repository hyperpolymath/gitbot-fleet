// SPDX-License-Identifier: MPL-2.0
//! Envelope parity with the estate's direct panic-attack lanes (cutover design
//! section 5). `panicbot --format json scan` must never print a findings array
//! for a scan that did not really run: each failure mode below must exit
//! non-zero with an empty stdout (the report-back driver turns that into an
//! `error` row carrying stderr's first line), and only a real report may
//! produce an array.
//!
//! panic-attack is a stub shell script on PATH here; this proves panicbot's
//! handling, not panic-attack's analysis.

use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::process::{Command, Output};
use tempfile::TempDir;

fn stub(dir: &Path, body: &str) {
    let p = dir.join("panic-attack");
    fs::write(&p, format!("#!/usr/bin/env bash\n{body}\n")).unwrap();
    fs::set_permissions(&p, fs::Permissions::from_mode(0o755)).unwrap();
}

fn scan(bin_dir: &Path, tree: &Path) -> Output {
    let home = TempDir::new().unwrap();
    Command::new(env!("CARGO_BIN_EXE_panicbot"))
        .env_clear()
        .env("PATH", format!("{}:/usr/bin:/bin", bin_dir.display()))
        .env("HOME", home.path())
        .env("RUST_LOG", "off")
        .args(["--format", "json", "scan"])
        .arg(tree)
        .output()
        .unwrap()
}

fn tree_with_timeout(secs: Option<u64>) -> TempDir {
    let t = TempDir::new().unwrap();
    fs::write(t.path().join("a.rs"), "fn main() {}\n").unwrap();
    if let Some(s) = secs {
        let d = t.path().join(".machine_readable/bot_directives");
        fs::create_dir_all(&d).unwrap();
        fs::write(
            d.join("panicbot.a2ml"),
            format!("[config]\ntimeout-seconds = {s}\n"),
        )
        .unwrap();
    }
    t
}

const REAL: &str = r#"Running assail analysis on: .
{"program_path":"/t","language":"rust","statistics":{"total_lines":10},"weak_points":[{"category":"PanicPath","location":"a.rs:1","severity":"Medium","description":"unwrap"}]}"#;

/// The scan did not run: non-zero exit, no findings array on stdout, a reason on stderr.
fn assert_not_a_run(o: &Output, reason_contains: &str) {
    assert!(
        !o.status.success(),
        "exit {:?}, stdout: {}",
        o.status.code(),
        String::from_utf8_lossy(&o.stdout)
    );
    let stdout = String::from_utf8_lossy(&o.stdout);
    assert!(
        serde_json::from_str::<serde_json::Value>(&stdout)
            .map(|v| !v.is_array())
            .unwrap_or(true),
        "stdout must not be a findings array: {stdout}"
    );
    let stderr = String::from_utf8_lossy(&o.stderr);
    assert!(
        stderr.contains(reason_contains),
        "stderr lacks {reason_contains:?}: {stderr}"
    );
}

#[test]
fn not_installed_is_not_a_run() {
    let empty = TempDir::new().unwrap(); // no stub
    let o = scan(empty.path(), tree_with_timeout(None).path());
    assert_not_a_run(&o, "not found");
}

#[test]
fn timed_out_is_not_a_run_and_does_not_hang() {
    let bin = TempDir::new().unwrap();
    stub(bin.path(), "exec sleep 30");
    let t = std::time::Instant::now();
    let o = scan(bin.path(), tree_with_timeout(Some(1)).path());
    assert!(
        t.elapsed().as_secs() < 15,
        "the timeout must be enforced by panicbot itself"
    );
    assert_not_a_run(&o, "did not finish within 1s");
}

#[test]
fn exited_non_zero_is_not_a_run_even_with_a_valid_report_on_stdout() {
    let bin = TempDir::new().unwrap();
    stub(
        bin.path(),
        &format!("cat <<'J'\n{REAL}\nJ\necho 'panic-attack: boom' >&2\nexit 3"),
    );
    let o = scan(bin.path(), tree_with_timeout(None).path());
    assert_not_a_run(&o, "exited with");
}

#[test]
fn exit_0_without_a_valid_report_is_not_a_run() {
    for (body, why) in [
        ("exit 0", "without a report"),
        (
            "echo 'Running assail analysis on: .'; exit 0",
            "without a valid report",
        ),
        ("echo '{}'; exit 0", "without a valid report"),
        ("echo '[]'; exit 0", "without a valid report"),
        (
            r#"echo '{"program_path":"/t","language":"x","statistics":{"total_lines":0},"weak_points":[]}'; exit 0"#,
            "analysed nothing",
        ),
    ] {
        let bin = TempDir::new().unwrap();
        stub(bin.path(), body);
        let o = scan(bin.path(), tree_with_timeout(None).path());
        assert_not_a_run(&o, why);
    }
}

#[test]
fn a_real_report_is_a_run_with_its_findings() {
    let bin = TempDir::new().unwrap();
    stub(bin.path(), &format!("cat <<'J'\n{REAL}\nJ\nexit 0"));
    let o = scan(bin.path(), tree_with_timeout(None).path());
    let v: serde_json::Value = serde_json::from_slice(&o.stdout).expect("a findings array");
    assert!(v.is_array() && v.as_array().unwrap().len() == 1, "{v}");
}
