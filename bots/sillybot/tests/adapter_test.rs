// SPDX-License-Identifier: MPL-2.0
//! End-to-end tests of the sillybot binary with stub tools.
//!
//! The stubs stand in for docs-ingest and reconcile so these tests need
//! neither Zig toolchain. They check the exit-code and JSON contract, the
//! base handling, and that the scanned tree is left untouched.

use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

const BIN: &str = env!("CARGO_BIN_EXE_sillybot");

fn sh(dir: &Path, args: &[&str]) {
    let st = Command::new("git")
        .arg("-C")
        .arg(dir)
        .args(["-c", "user.name=t", "-c", "user.email=t@example.invalid"])
        .args(["-c", "commit.gpgsign=false"])
        .args(args)
        .output()
        .expect("git runs");
    assert!(st.status.success(), "git {args:?}: {st:?}");
}

/// A repo with README.adoc and src/a.rs, origin set to a GitHub URL.
fn repo() -> tempfile::TempDir {
    let d = tempfile::tempdir().unwrap();
    sh(d.path(), &["init", "-q", "-b", "main"]);
    fs::write(d.path().join("README.adoc"), "= T\n\nlink:gone.adoc[x]\n").unwrap();
    fs::create_dir(d.path().join("src")).unwrap();
    fs::write(d.path().join("src/a.rs"), "fn main() {}\n").unwrap();
    sh(d.path(), &["add", "."]);
    sh(d.path(), &["commit", "-q", "-m", "one"]);
    sh(
        d.path(),
        &[
            "remote",
            "add",
            "origin",
            "https://github.com/acme/widgets.git",
        ],
    );
    d
}

fn head(dir: &Path) -> String {
    let o = Command::new("git")
        .arg("-C")
        .arg(dir)
        .args(["rev-parse", "HEAD"])
        .output()
        .unwrap();
    String::from_utf8(o.stdout).unwrap().trim().to_string()
}

fn script(dir: &Path, name: &str, body: &str) -> PathBuf {
    let p = dir.join(name);
    fs::write(
        &p,
        format!("#!/usr/bin/env bash\nset -euo pipefail\n{body}\n"),
    )
    .unwrap();
    fs::set_permissions(&p, fs::Permissions::from_mode(0o755)).unwrap();
    p
}

/// Stub docs-ingest: checks its arguments, records them, prints a bundle.
fn stub_ingest(tools: &Path) -> PathBuf {
    let log = tools.join("ingest.log");
    script(
        tools,
        "docs-ingest",
        &format!(
            r#"echo "$@" >> '{}'
[[ "$1" == --repo && "$3" == --rev && "$5" == --root && "$7" == --files ]]
test -f "$6/README.adoc"
grep -qx README.adoc "$8"
echo '{{"schema":"reconcile-input/v0","repo":"'"$2"'","rev":"'"$4"'","files":[],"docs":[]}}'"#,
            log.display()
        ),
    )
}

const FINDINGS: &str = r#"{"findings":[
 {"class":"dead-internal-link","severity":"error","path":"README.adoc",
  "span":{"start":{"line":3,"col":1},"end":{"line":3,"col":18}},
  "message":"Link target 'gone.adoc' is not in the repository.",
  "introduced":true,"id":"f1","tool":"sillybot","evidence":[]},
 {"class":"dead-anchor","severity":"warning","path":"README.adoc",
  "span":{"start":{"line":1,"col":1},"end":{"line":1,"col":3}},
  "message":"old","introduced":false,"id":"f2","tool":"sillybot","evidence":[]}]}"#;

fn stub_reconcile(tools: &Path, body: &str) -> PathBuf {
    script(tools, "reconcile", body)
}

fn run(dir: &Path, ingest: Option<&Path>, reconcile: Option<&Path>, extra: &[&str]) -> Output {
    let mut c = Command::new(BIN);
    c.args(["--format", "json", "scan", "--path"])
        .arg(dir)
        .args(extra);
    c.env("PATH", "/usr/bin:/bin");
    match ingest {
        Some(p) => c.env("SILLYBOT_DOCS_INGEST", p),
        None => c.env("SILLYBOT_DOCS_INGEST", "/nonexistent/docs-ingest"),
    };
    match reconcile {
        Some(p) => c.env("SILLYBOT_RECONCILE", p),
        None => c.env("SILLYBOT_RECONCILE", "/nonexistent/reconcile"),
    };
    c.output().unwrap()
}

fn json(o: &Output) -> serde_json::Value {
    serde_json::from_slice(&o.stdout).unwrap_or_else(|e| panic!("{e}: {o:?}"))
}

/// Every file under `dir` with its contents, .git included.
fn snapshot(dir: &Path) -> Vec<(PathBuf, Vec<u8>)> {
    let mut out = Vec::new();
    let mut stack = vec![dir.to_path_buf()];
    while let Some(d) = stack.pop() {
        for e in fs::read_dir(&d).unwrap() {
            let p = e.unwrap().path();
            if p.is_dir() {
                stack.push(p);
            } else {
                out.push((p.clone(), fs::read(&p).unwrap()));
            }
        }
    }
    out.sort();
    out
}

#[test]
fn missing_tool_is_not_run() {
    let r = repo();
    let o = run(r.path(), None, None, &[]);
    assert_eq!(o.status.code(), Some(3));
    assert_eq!(json(&o)["status"], "not_run");
}

#[test]
fn ran_maps_findings_and_leaves_the_tree_alone() {
    let r = repo();
    let tools = tempfile::tempdir().unwrap();
    let ing = stub_ingest(tools.path());
    let rec = stub_reconcile(
        tools.path(),
        &format!("[[ \"$1\" == --head ]]\ncat <<'J'\n{FINDINGS}\nJ"),
    );
    let before = snapshot(r.path());
    let o = run(r.path(), Some(&ing), Some(&rec), &[]);
    assert_eq!(o.status.code(), Some(1), "{o:?}");
    let v = json(&o);
    assert_eq!(v[0]["rule_id"], "SB-dead-internal-link");
    assert_eq!(v[0]["severity"], "error");
    assert_eq!(v[0]["file"], "README.adoc");
    assert_eq!(v[0]["line"], 3);
    assert_eq!(v[1]["severity"], "info");
    assert_eq!(v[1]["reconcile_severity"], "warning");
    assert_eq!(before, snapshot(r.path()), "scanned tree changed");
    let log = fs::read_to_string(tools.path().join("ingest.log")).unwrap();
    assert!(log.contains("--repo acme/widgets"), "{log}");
    assert!(log.contains(&head(r.path())), "{log}");
}

#[test]
fn base_without_prose_change_is_not_run() {
    let r = repo();
    let base = head(r.path());
    fs::write(r.path().join("src/a.rs"), "fn main() { }\n").unwrap();
    sh(r.path(), &["commit", "-qam", "code only"]);
    let tools = tempfile::tempdir().unwrap();
    let ing = stub_ingest(tools.path());
    let rec = stub_reconcile(tools.path(), "exit 9");
    let o = run(r.path(), Some(&ing), Some(&rec), &["--base", &base]);
    assert_eq!(o.status.code(), Some(3), "{o:?}");
    assert_eq!(json(&o)["status"], "not_run");
}

#[test]
fn base_with_prose_change_passes_both_bundles() {
    let r = repo();
    let base = head(r.path());
    fs::write(r.path().join("README.adoc"), "= T\n\nchanged\n").unwrap();
    sh(r.path(), &["commit", "-qam", "docs"]);
    let tools = tempfile::tempdir().unwrap();
    let ing = stub_ingest(tools.path());
    let rec = stub_reconcile(
        tools.path(),
        "[[ \"$1\" == --head && \"$3\" == --base && \"$5\" == --pr && \"$6\" == 7 ]]\necho '{\"findings\":[]}'",
    );
    let o = run(
        r.path(),
        Some(&ing),
        Some(&rec),
        &["--base", &base, "--pr", "7"],
    );
    assert_eq!(o.status.code(), Some(0), "{o:?}");
    assert_eq!(json(&o), serde_json::json!([]));
    let log = fs::read_to_string(tools.path().join("ingest.log")).unwrap();
    assert!(log.contains(&base), "base not ingested: {log}");
}

#[test]
fn tool_failure_is_an_error_not_a_pass() {
    let r = repo();
    let tools = tempfile::tempdir().unwrap();
    let ing = stub_ingest(tools.path());
    let rec = stub_reconcile(tools.path(), "echo boom >&2; exit 2");
    let o = run(r.path(), Some(&ing), Some(&rec), &[]);
    assert_eq!(o.status.code(), Some(2));
    let v = json(&o);
    assert_eq!(v["status"], "error");
    assert!(v["reason"].as_str().unwrap().contains("boom"));
}

#[test]
fn unusable_output_is_an_error() {
    let r = repo();
    let tools = tempfile::tempdir().unwrap();
    let ing = stub_ingest(tools.path());
    for body in [
        "echo not-json",
        "echo '{}'",
        r#"echo '{"findings":[{"class":"x"}]}'"#,
    ] {
        let rec = stub_reconcile(tools.path(), body);
        let o = run(r.path(), Some(&ing), Some(&rec), &[]);
        assert_eq!(o.status.code(), Some(2), "{body}: {o:?}");
        assert_eq!(json(&o)["status"], "error");
    }
}

#[test]
fn bad_base_and_repo_are_errors() {
    let r = repo();
    let tools = tempfile::tempdir().unwrap();
    let ing = stub_ingest(tools.path());
    let rec = stub_reconcile(tools.path(), "echo '{\"findings\":[]}'");
    for extra in [
        &["--base", "--upload-pack=x"][..],
        &["--base", "nope"][..],
        &["--repo", "a/b/c"][..],
    ] {
        let o = run(r.path(), Some(&ing), Some(&rec), extra);
        assert_eq!(o.status.code(), Some(2), "{extra:?}: {o:?}");
    }
}

#[test]
fn timeout_is_an_error() {
    let r = repo();
    let tools = tempfile::tempdir().unwrap();
    let ing = stub_ingest(tools.path());
    let rec = stub_reconcile(tools.path(), "sleep 30");
    let start = std::time::Instant::now();
    let o = run(r.path(), Some(&ing), Some(&rec), &["--timeout", "1"]);
    assert_eq!(o.status.code(), Some(2));
    assert!(json(&o)["reason"].as_str().unwrap().contains("timed out"));
    assert!(start.elapsed().as_secs() < 10);
}
