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

/// A panic-attack that answers every subcommand the full suite runs:
/// `assail` prints REAL, `assay` writes `ASSAY_JSON` to its `-o` path,
/// `--version` and `diagnostics` answer.
fn full_stub_body(assay_json: &str) -> String {
    format!(
        r#"case "$1" in
  --version) echo "panic-attack 2.5.5" ;;
  diagnostics) exit 0 ;;
  assay)
    out=""
    while [ $# -gt 0 ]; do [ "$1" = "-o" ] && out="$2"; shift; done
    cat > "$out" <<'J'
{assay_json}
J
    ;;
  *) cat <<'J'
{REAL}
J
    ;;
esac
exit 0"#
    )
}

const ASSAY_NONE: &str = r#"{"candidates":[],"catalogue_entries":2,"notes":[]}"#;

#[test]
fn a_real_report_is_a_run_with_its_findings() {
    let bin = TempDir::new().unwrap();
    stub(bin.path(), &full_stub_body(ASSAY_NONE));
    let o = scan(bin.path(), tree_with_timeout(None).path());
    assert!(
        o.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&o.stderr)
    );
    let v: serde_json::Value = serde_json::from_slice(&o.stdout).expect("a findings array");
    assert!(v.is_array() && v.as_array().unwrap().len() == 1, "{v}");
}

#[test]
fn the_full_suite_runs_by_default_and_assay_findings_name_their_file() {
    let bin = TempDir::new().unwrap();
    let assay = r#"{"candidates":[{"id":"safe-path","proven_name":"SafePath","proof_backing":"proven: Operations.idr","status":"offered","confidence":0.9,"target_hits":[{"file":"a.rs","line":1,"snippet":"x"}]}],"catalogue_entries":2,"notes":[]}"#;
    stub(bin.path(), &full_stub_body(assay));
    let o = scan(bin.path(), tree_with_timeout(None).path());
    assert!(
        o.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&o.stderr)
    );
    let v: serde_json::Value = serde_json::from_slice(&o.stdout).expect("a findings array");
    let rules: Vec<&str> = v
        .as_array()
        .unwrap()
        .iter()
        .map(|f| f["rule_id"].as_str().unwrap())
        .collect();
    assert!(
        rules.contains(&"PA005") && rules.contains(&"PB-ASSAY-SAFE-PATH"),
        "{rules:?}"
    );
    for f in v.as_array().unwrap() {
        assert_eq!(f["file"], "a.rs", "{f}");
    }
    // The per-part table is on stderr: assail, assay and diagnostics ran; the rest say why not.
    let err = String::from_utf8_lossy(&o.stderr);
    assert!(
        err.contains("assay") && err.contains("not_applicable"),
        "{err}"
    );
}

#[test]
fn a_part_that_fails_makes_the_run_an_error_object_never_a_findings_array() {
    // assail works, assay writes no report: the failure is loud and named, and
    // the findings of the part that ran are carried, not dropped.
    let bin = TempDir::new().unwrap();
    stub(
        bin.path(),
        &format!("case \"$1\" in --version) echo x ;; diagnostics) exit 0 ;; assay) exit 0 ;; *) cat <<'J'\n{REAL}\nJ\n;; esac"),
    );
    let o = scan(bin.path(), tree_with_timeout(None).path());
    assert_eq!(
        o.status.code(),
        Some(3),
        "stderr: {}",
        String::from_utf8_lossy(&o.stderr)
    );
    let stderr = String::from_utf8_lossy(&o.stderr);
    let first = stderr.lines().find(|l| !l.trim().is_empty()).unwrap();
    assert!(
        first.contains("1 of 10") && first.contains("assay"),
        "{first}"
    );
    let v: serde_json::Value = serde_json::from_slice(&o.stdout).expect("an object");
    assert_eq!(v["status"], "error");
    assert!(v["reason"].as_str().unwrap().contains("assay"), "{v}");
    assert_eq!(v["findings"].as_array().unwrap().len(), 1);
    assert!(v["parts"]
        .as_array()
        .unwrap()
        .iter()
        .any(|p| p["command"] == "assay" && p["status"] == "error"));
}

fn tree_with_directive(content: &str) -> TempDir {
    let t = TempDir::new().unwrap();
    fs::write(t.path().join("a.rs"), "fn main() {}\n").unwrap();
    let d = t.path().join(".machine_readable/bot_directives");
    fs::create_dir_all(&d).unwrap();
    fs::write(d.join("panicbot.a2ml"), content).unwrap();
    t
}

#[test]
fn a_directive_that_denies_everything_is_not_run_never_a_clean_run() {
    let bin = TempDir::new().unwrap();
    stub(bin.path(), &full_stub_body(ASSAY_NONE));
    let o = scan(bin.path(), tree_with_directive("allow = []\n").path());
    assert_eq!(
        o.status.code(),
        Some(4),
        "stderr: {}",
        String::from_utf8_lossy(&o.stderr)
    );
    let v: serde_json::Value = serde_json::from_slice(&o.stdout).expect("an object");
    assert_eq!(v["status"], "not_run");
    assert!(v["reason"].as_str().unwrap().contains("denied"), "{v}");
}

#[test]
fn a_directive_file_keeps_its_scope_and_is_not_widened_to_assay() {
    // The shipped directive shape: static trio allowed. The stub has no assay
    // answer at all, so any assay call would turn the run into an error.
    let bin = TempDir::new().unwrap();
    stub(bin.path(), &format!("case \"$1\" in --version) echo x ;; diagnostics) exit 0 ;; assay) echo assay was called >&2; exit 9 ;; *) cat <<'J'\n{REAL}\nJ\n;; esac"));
    let tree = tree_with_directive(
        "allow = [\"assail\", \"adjudicate\", \"diagnostics\"]\ndeny = [\"attack\", \"assault\", \"ambush\", \"amuck\", \"abduct\", \"axial\"]\n",
    );
    let o = scan(bin.path(), tree.path());
    assert!(
        o.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&o.stderr)
    );
    let err = String::from_utf8_lossy(&o.stderr);
    assert!(!err.contains("assay was called"), "{err}");
    assert!(err.contains("denied"), "{err}");
}

#[test]
fn a_broken_directive_stops_the_run_and_never_falls_back_to_the_full_suite() {
    let bin = TempDir::new().unwrap();
    stub(bin.path(), &full_stub_body(ASSAY_NONE));
    let o = scan(
        bin.path(),
        tree_with_directive("allow = [ not toml\n").path(),
    );
    assert!(!o.status.success());
    assert!(
        o.stdout.is_empty(),
        "stdout: {}",
        String::from_utf8_lossy(&o.stdout)
    );
    assert!(String::from_utf8_lossy(&o.stderr).contains("bot directives unreadable"));
}

/// A stub that records the arguments and working directory of every call into
/// `calls.log` beside itself, and answers like `full_stub_body`.
fn recording_stub_body(assay_json: &str) -> String {
    format!(
        r#"echo "$PWD|$*" >> "$(dirname "$0")/calls.log"
case "$1" in
  --version) echo "panic-attack 2.5.5" ;;
  diagnostics) exit 0 ;;
  assay|assault|ambush)
    out=""
    a=("$@")
    for ((i=0;i<${{#a[@]}};i++)); do [ "${{a[$i]}}" = "-o" ] && out="${{a[$((i+1))]}}"; done
    case "$1" in
      assay) cat > "$out" <<'J'
{assay_json}
J
      ;;
      *) cat > "$out" <<'J'
{{"attack_results":[{{"axis":"cpu","success":true,"skipped":false}}],"total_crashes":0,"total_signatures":0}}
J
      ;;
    esac ;;
  adjudicate)
    out=""
    a=("$@")
    for ((i=0;i<${{#a[@]}};i++)); do [ "${{a[$i]}}" = "-o" ] && out="${{a[$((i+1))]}}"; done
    echo '{{"verdict":"pass","processed_reports":2,"failed_reports":0}}' > "$out" ;;
  *) cat <<'J'
{REAL}
J
    ;;
esac
exit 0"#
    )
}

#[test]
fn relative_scan_dir_and_target_reach_the_children_as_absolute_paths() {
    // `panicbot scan .` runs the children in a scratch directory. A relative
    // path would be read there (an empty tree) and report clean.
    let bin = TempDir::new().unwrap();
    stub(bin.path(), &recording_stub_body(ASSAY_NONE));
    let tree = tree_with_timeout(None);
    let prog = tree.path().join("prog");
    fs::write(&prog, "#!/bin/sh\nexit 0\n").unwrap();
    fs::set_permissions(&prog, fs::Permissions::from_mode(0o755)).unwrap();
    let home = TempDir::new().unwrap();
    let o = Command::new(env!("CARGO_BIN_EXE_panicbot"))
        .env_clear()
        .env("PATH", format!("{}:/usr/bin:/bin", bin.path().display()))
        .env("HOME", home.path())
        .env("RUST_LOG", "off")
        .current_dir(tree.path())
        .args(["--format", "json", "scan", ".", "--target", "prog"])
        .output()
        .unwrap();
    assert!(
        o.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&o.stderr)
    );
    let log = fs::read_to_string(bin.path().join("calls.log")).unwrap();
    let canon = tree.path().canonicalize().unwrap();
    let canon = canon.display().to_string();
    let mut seen = 0;
    for line in log.lines() {
        let (_, args) = line.split_once('|').unwrap();
        let words: Vec<&str> = args.split(' ').collect();
        match words[0] {
            "assay" => {
                seen += 1;
                assert_eq!(words[1], canon, "assay target: {line}");
            }
            "assault" | "ambush" => {
                seen += 1;
                assert_eq!(words[1], format!("{canon}/prog"), "program: {line}");
                let i = words.iter().position(|w| *w == "--source").unwrap();
                assert_eq!(words[i + 1], canon, "source: {line}");
            }
            _ => {}
        }
    }
    assert_eq!(seen, 3, "assay, assault, ambush were all called: {log}");
}

#[test]
fn a_hung_diagnostics_is_an_error_part_not_a_healthy_report() {
    let bin = TempDir::new().unwrap();
    stub(
        bin.path(),
        &format!(
            "case \"$1\" in --version) echo 'panic-attack 2.5.5' ;; diagnostics) exec sleep 30 ;; assay) out=\"\"; while [ $# -gt 0 ]; do [ \"$1\" = -o ] && out=\"$2\"; shift; done; echo '{ASSAY_NONE}' > \"$out\" ;; *) cat <<'J'\n{REAL}\nJ\n;; esac"
        ),
    );
    let t = std::time::Instant::now();
    let o = scan(bin.path(), tree_with_timeout(Some(2)).path());
    assert!(t.elapsed().as_secs() < 20, "diagnostics must be timed");
    assert_eq!(
        o.status.code(),
        Some(3),
        "stderr: {}",
        String::from_utf8_lossy(&o.stderr)
    );
    let v: serde_json::Value = serde_json::from_slice(&o.stdout).expect("an object");
    assert_eq!(v["status"], "error");
    assert!(v["parts"]
        .as_array()
        .unwrap()
        .iter()
        .any(|p| p["command"] == "diagnostics"
            && p["status"] == "error"
            && p["detail"].as_str().unwrap().contains("did not finish")));
}

#[test]
fn a_crashed_diagnostics_is_an_error_part() {
    let bin = TempDir::new().unwrap();
    stub(
        bin.path(),
        &format!(
            "case \"$1\" in --version) echo 'panic-attack 2.5.5' ;; diagnostics) kill -SEGV $$ ;; assay) out=\"\"; while [ $# -gt 0 ]; do [ \"$1\" = -o ] && out=\"$2\"; shift; done; echo '{ASSAY_NONE}' > \"$out\" ;; *) cat <<'J'\n{REAL}\nJ\n;; esac"
        ),
    );
    let o = scan(bin.path(), tree_with_timeout(None).path());
    assert_eq!(o.status.code(), Some(3));
    let v: serde_json::Value = serde_json::from_slice(&o.stdout).expect("an object");
    assert!(v["parts"]
        .as_array()
        .unwrap()
        .iter()
        .any(|p| p["command"] == "diagnostics"
            && p["status"] == "error"
            && p["detail"].as_str().unwrap().contains("signal")));
}

/// On a timeout the whole process group dies. Killing only panic-attack would
/// leave the backgrounded `sleep` holding the output pipes, so the scan would
/// block until it ended.
#[test]
fn timeout_kills_descendants_and_returns_within_the_timeout() {
    let bin = TempDir::new().unwrap();
    let pidfile = bin.path().join("child.pid");
    stub(
        bin.path(),
        &format!("sleep 25 &\necho $! > {}\nwait", pidfile.display()),
    );
    let t = std::time::Instant::now();
    let o = scan(bin.path(), tree_with_timeout(Some(1)).path());
    assert!(
        t.elapsed().as_secs() < 12,
        "a descendant holding the pipes must not outlast the timeout"
    );
    assert_not_a_run(&o, "did not finish within 1s");
    let pid = fs::read_to_string(&pidfile).unwrap().trim().to_string();
    let mut gone = false;
    for _ in 0..50 {
        let stat = fs::read_to_string(format!("/proc/{pid}/stat"));
        if stat.map(|s| s.contains(") Z ")).unwrap_or(true) {
            gone = true;
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(100));
    }
    assert!(
        gone,
        "the backgrounded descendant {pid} survived the timeout"
    );
}

/// A relative `PATH` entry must be made absolute before `assay` changes the
/// working directory to its scratch dir.
#[test]
fn a_relative_path_entry_is_resolved_before_the_cwd_changes() {
    let root = TempDir::new().unwrap();
    let bin = root.path().join("bin");
    fs::create_dir(&bin).unwrap();
    stub(&bin, &full_stub_body(ASSAY_NONE));
    let tree = tree_with_timeout(None);
    let home = TempDir::new().unwrap();
    let o = Command::new(env!("CARGO_BIN_EXE_panicbot"))
        .env_clear()
        .current_dir(root.path())
        .env("PATH", "bin:/usr/bin:/bin")
        .env("HOME", home.path())
        .env("RUST_LOG", "off")
        .args(["--format", "json", "scan"])
        .arg(tree.path())
        .output()
        .unwrap();
    assert!(
        o.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&o.stderr)
    );
    let v: serde_json::Value = serde_json::from_slice(&o.stdout).expect("a findings array");
    assert!(v.is_array(), "{v}");
}

fn scan_text(bin_dir: &Path, tree: &Path) -> Output {
    let home = TempDir::new().unwrap();
    Command::new(env!("CARGO_BIN_EXE_panicbot"))
        .env_clear()
        .env("PATH", format!("{}:/usr/bin:/bin", bin_dir.display()))
        .env("HOME", home.path())
        .env("RUST_LOG", "off")
        .args(["--format", "text", "scan"])
        .arg(tree)
        .output()
        .unwrap()
}

/// Text mode must not print "No issues found." for a run that errored or in
/// which nothing ran.
#[test]
fn text_mode_never_says_no_issues_for_an_incomplete_run() {
    // assail fails: a part error (exit 3).
    let bin = TempDir::new().unwrap();
    stub(bin.path(), "echo 'panic-attack: boom' >&2\nexit 3");
    let o = scan_text(bin.path(), tree_with_timeout(None).path());
    let out = String::from_utf8_lossy(&o.stdout);
    assert_eq!(o.status.code(), Some(3), "{out}");
    assert!(!out.contains("No issues found."), "{out}");
    assert!(out.contains("INCOMPLETE"), "{out}");

    // Nothing allowed to run (exit 4).
    let tree = tree_with_timeout(None);
    let d = tree.path().join(".machine_readable/bot_directives");
    fs::create_dir_all(&d).unwrap();
    fs::write(d.join("panicbot.a2ml"), "allow = [\"diagnostics\"]\n").unwrap();
    let bin = TempDir::new().unwrap();
    stub(bin.path(), "exit 0");
    let o = scan_text(bin.path(), tree.path());
    let out = String::from_utf8_lossy(&o.stdout);
    assert_eq!(o.status.code(), Some(4), "{out}");
    assert!(!out.contains("No issues found."), "{out}");
    assert!(out.contains("NOT RUN"), "{out}");
}

/// A clean, complete run still says so.
#[test]
fn text_mode_says_no_issues_for_a_clean_complete_run() {
    let bin = TempDir::new().unwrap();
    let clean = r#"{"program_path":"/t","language":"rust","statistics":{"total_lines":10},"weak_points":[]}"#;
    stub(bin.path(), &full_stub_body(ASSAY_NONE).replace(REAL, clean));
    let o = scan_text(bin.path(), tree_with_timeout(None).path());
    let out = String::from_utf8_lossy(&o.stdout);
    assert!(
        o.status.success(),
        "{out} {}",
        String::from_utf8_lossy(&o.stderr)
    );
    assert!(out.contains("No issues found."), "{out}");
}
