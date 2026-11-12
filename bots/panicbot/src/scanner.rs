// SPDX-License-Identifier: MPL-2.0
//! Scanner — subprocess wrapper for the `panic-attack` binary.
//!
//! Panicbot does NOT link against panic-attack as a Rust crate. Instead, it
//! invokes `panic-attack` as a subprocess with `--output-format json` and
//! parses stdout. This matches how Hypatia already integrates with it, and
//! keeps the coupling loose — panic-attack can be upgraded independently.
//!
//! Three entry points:
//! - [`run_assail`] — core static analysis (WeakPoints, taint matrix, stats)
//! - [`run_adjudicate`] — multi-report verdict across several scan outputs
//! - [`run_diagnostics`] — preflight health check (binary presence, version)

use anyhow::{bail, Context, Result};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

/// Report from `panic-attack assail <target> --output-format json`.
///
/// This is the primary scan output. Contains weak points (static analysis
/// findings), program statistics, taint matrix, and recommended attack axes.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AssailReport {
    /// Path to the scanned program/repository.
    pub program_path: PathBuf,
    /// Detected primary language.
    pub language: String,
    /// Detected frameworks.
    #[serde(default)]
    pub frameworks: Vec<String>,
    /// All detected weak points (the core findings).
    pub weak_points: Vec<WeakPoint>,
    /// Aggregate program statistics.
    #[serde(default)]
    pub statistics: serde_json::Value,
    /// Per-file statistics.
    #[serde(default)]
    pub file_statistics: Vec<serde_json::Value>,
    /// Recommended attack axes (for dynamic testing).
    #[serde(default)]
    pub recommended_attacks: Vec<String>,
    /// Dependency graph summary.
    #[serde(default)]
    pub dependency_graph: serde_json::Value,
    /// Taint matrix (source → sink tracking).
    #[serde(default)]
    pub taint_matrix: serde_json::Value,
}

/// A single weak point detected by panic-attack's static analysis.
///
/// Maps 1:1 to panic-attack's `WeakPoint` struct. The `category` field
/// determines which fleet category and triangle tier this finding maps to
/// (see `translator.rs`).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WeakPoint {
    /// Category of the weak point (e.g., "UnsafeCode", "CommandInjection").
    pub category: String,
    /// File location (e.g., "src/ffi.rs:42").
    pub location: Option<String>,
    /// Severity level (Low, Medium, High, Critical).
    pub severity: String,
    /// Human-readable description of the issue.
    pub description: String,
    /// Recommended attack axes for dynamic validation.
    #[serde(default)]
    pub recommended_attack: Vec<String>,
    /// Set by panic-attack's context-aware FP suppression engine.
    /// Suppressed items are excluded from fleet counts and CI gates.
    #[serde(default)]
    pub suppressed: bool,
    /// Test-vs-production classification of the finding's location
    /// (panic-attack v2.5.5+). `None` means the scanner predates the
    /// classification field; treat as "Production" for downstream
    /// routing decisions. `"test_only"` and `"doc"` indicate the
    /// finding is in test or documentation code respectively — usually
    /// the panic-attack engine has ALREADY set `suppressed: true` for
    /// these, but the metadata is preserved so the translator can emit
    /// audit-trail context (e.g. "PanicPath in tests/foo.rs — accepted
    /// by test_context rule").
    #[serde(default)]
    pub test_context: Option<String>,
}

/// Report from `panic-attack adjudicate` — cross-report verdict.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AdjudicateReport {
    /// Number of reports adjudicated.
    pub reports_analyzed: usize,
    /// Overall robustness score (0-100).
    pub robustness_score: f64,
    /// Critical issues that appeared across multiple reports.
    #[serde(default)]
    pub critical_issues: Vec<serde_json::Value>,
    /// Consolidated recommendations.
    #[serde(default)]
    pub recommendations: Vec<String>,
}

/// Report from `panic-attack migration-snapshot <target> --label <label>`.
///
/// Contains migration-specific metrics: deprecated/modern API counts,
/// health score, version bracket, config format, and optional build/bundle data.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MigrationSnapshotReport {
    /// Snapshot label (e.g., "before-v12-migration")
    pub label: String,
    /// ISO 8601 timestamp
    pub timestamp: String,
    /// Path to the scanned target
    pub target_path: PathBuf,
    /// Number of deprecated API calls (Js.*, Belt.*)
    pub deprecated_api_count: usize,
    /// Number of modern @rescript/core API calls
    pub modern_api_count: usize,
    /// Ratio of modern to total API calls (0.0 - 1.0)
    pub api_migration_ratio: f64,
    /// Overall migration health score (0.0 - 1.0)
    pub health_score: f64,
    /// Detected ReScript version bracket
    pub version_bracket: String,
    /// Configuration format (bsconfig, rescript_json, both, none)
    pub config_format: String,
    /// Build time in milliseconds (if --build-time was passed)
    #[serde(default)]
    pub build_time_ms: Option<u64>,
    /// Bundle size in bytes (if --bundle-size was passed)
    #[serde(default)]
    pub bundle_size_bytes: Option<u64>,
    /// Number of ReScript source files
    pub file_count: usize,
    /// Total lines of ReScript code
    pub rescript_lines: usize,
    /// Detailed deprecated pattern instances
    #[serde(default)]
    pub deprecated_patterns: Vec<serde_json::Value>,
}

/// Report from `panic-attack migration-diff <before> <after>`.
///
/// Compares two migration snapshots and produces deltas.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MigrationDiffReport {
    /// Label of the before snapshot
    pub before_label: String,
    /// Label of the after snapshot
    pub after_label: String,
    /// Change in health score
    pub health_delta: f64,
    /// Change in deprecated API count (negative = improvement)
    pub deprecated_delta: i64,
    /// Change in modern API count (positive = improvement)
    pub modern_delta: i64,
    /// Change in build time (negative = faster)
    #[serde(default)]
    pub build_time_delta_ms: Option<i64>,
    /// Change in bundle size (negative = smaller)
    #[serde(default)]
    pub bundle_size_delta_bytes: Option<i64>,
    /// Deprecated patterns that were removed
    #[serde(default)]
    pub patterns_removed: Vec<serde_json::Value>,
    /// New deprecated patterns introduced
    #[serde(default)]
    pub patterns_added: Vec<serde_json::Value>,
}

/// Report from `panic-attack diagnostics` — preflight health check.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiagnosticsReport {
    /// panic-attack version string.
    pub version: String,
    /// Whether the binary is functional.
    pub healthy: bool,
    /// Supported languages.
    #[serde(default)]
    pub supported_languages: Vec<String>,
    /// Any diagnostic messages.
    #[serde(default)]
    pub messages: Vec<String>,
}

/// Run `panic-attack assail <target> --output-format json`.
///
/// Returns the parsed AssailReport containing all detected weak points.
/// Handles both envelope formats: `{"assail_report": {...}}` and direct
/// `{"program_path": ..., "weak_points": [...]}`.
pub fn run_assail(target: &Path, timeout: Duration) -> Result<AssailReport> {
    // `--headless` selects CI-safe output (JSON to stdout, no interactive
    // prompts). Required for panic-attack >= 2.5: without it the default
    // `--report-view accordion` renders a human report, not parseable JSON.
    let run = run_panic_attack_raw(
        &["assail", &target.display().to_string(), "--headless"],
        timeout,
    )
    .context("Failed to run panic-attack assail")?;

    // A non-zero exit is an error, whatever stdout holds: the estate's direct
    // lane treats any non-zero `assail` exit as `error`, and a scan that died
    // part-way can still have printed a parseable prefix. (assail exits 0 with
    // findings; it exits non-zero on failure or, with --fail-on-incomplete,
    // on incomplete coverage.)
    if !run.status.success() {
        bail!(
            "panic-attack assail exited with {}: {}",
            run.status,
            first_diagnostic(&run.stderr)
        );
    }
    if run.stdout.trim().is_empty() {
        bail!(
            "panic-attack assail exited 0 without a report (empty output). stderr: {}",
            first_diagnostic(&run.stderr)
        );
    }
    parse_assail_output(&run.stdout)
}

/// Parse and VALIDATE the stdout of a successful `assail` run.
///
/// Exit 0 is not evidence that anything was analysed. Mirroring the estate's
/// direct lanes (standards `hypatia-scan-reusable.yml`, `deep-recon-reusable.yml`),
/// a report is accepted only when it has a `weak_points` array, every weak
/// point carries a known severity, and `statistics.total_lines` is a positive
/// number. Anything else is an error, never an empty `ran` (cutover design
/// section 5; a crashed or empty scanner must not summarise itself green).
pub fn parse_assail_output(raw: &str) -> Result<AssailReport> {
    // Even in --headless mode panic-attack prints a human banner line
    // (e.g. "Running assail analysis on: .") to stdout before the JSON
    // document. Slice from the first JSON delimiter so serde sees pure JSON.
    let json = extract_json(raw);

    let value: serde_json::Value = serde_json::from_str(json)
        .context("panic-attack assail exited 0 without a valid report (not JSON)")?;
    // Handles both envelope formats: `{"assail_report": {...}}` and flat.
    let report_value = value
        .get("assail_report")
        .filter(|v| v.is_object())
        .unwrap_or(&value);

    let weak_points = report_value
        .get("weak_points")
        .and_then(|w| w.as_array())
        .context("panic-attack assail exited 0 without a valid report (no weak_points array)")?;
    for (i, wp) in weak_points.iter().enumerate() {
        let sev = wp.get("severity").and_then(|s| s.as_str()).unwrap_or("");
        if !matches!(sev, "Low" | "Medium" | "High" | "Critical") {
            bail!("panic-attack assail exited 0 without a valid report (weak point {i} has severity {sev:?})");
        }
    }
    let total_lines = report_value
        .pointer("/statistics/total_lines")
        .and_then(|n| n.as_f64())
        .unwrap_or(0.0);
    if total_lines <= 0.0 {
        bail!(
            "panic-attack assail exited 0 but analysed nothing (statistics.total_lines is missing or not positive)"
        );
    }

    serde_json::from_value::<AssailReport>(report_value.clone())
        .context("panic-attack assail exited 0 without a valid report (unexpected shape)")
}

/// First nonblank line of captured stderr, at most 240 characters.
fn first_diagnostic(stderr: &str) -> String {
    stderr
        .lines()
        .map(str::trim)
        .find(|l| !l.is_empty())
        .map(|l| l.chars().take(240).collect())
        .unwrap_or_else(|| "no diagnostic".to_string())
}

/// Slice a captured stdout buffer down to its JSON payload.
///
/// panic-attack emits a human-readable banner line before the JSON document
/// (even with `--headless`), so a raw `serde_json::from_str` on the whole
/// buffer fails at "line 1 column 1". Return the substring starting at the
/// first `{` or `[`; if neither is present, return the input unchanged so the
/// caller still produces a meaningful parse error.
fn extract_json(raw: &str) -> &str {
    match raw.find(['{', '[']) {
        Some(idx) => &raw[idx..],
        None => raw,
    }
}

/// Run `panic-attack adjudicate` on multiple report files.
///
/// Takes paths to previously generated JSON report files and produces
/// a consolidated verdict.
pub fn run_adjudicate(reports: &[PathBuf], timeout: Duration) -> Result<AdjudicateReport> {
    let mut args = vec!["adjudicate".to_string()];
    for report in reports {
        args.push(report.display().to_string());
    }
    let arg_refs: Vec<&str> = args.iter().map(|s| s.as_str()).collect();

    let output =
        invoke_panic_attack(&arg_refs, timeout).context("Failed to run panic-attack adjudicate")?;

    serde_json::from_str::<AdjudicateReport>(&output)
        .context("Failed to parse panic-attack adjudicate output as JSON")
}

/// Run `panic-attack diagnostics` to check binary health.
///
/// Used as a preflight check to verify that panic-attack is installed,
/// functional, and reports its version.
pub fn run_diagnostics(timeout: Duration) -> Result<DiagnosticsReport> {
    // First check if the binary exists at all
    let binary = find_panic_attack_binary()?;

    // Try `--version` for a lightweight health check
    let version_output = Command::new(&binary)
        .arg("--version")
        .output()
        .context("Failed to execute panic-attack --version")?;

    let version = String::from_utf8_lossy(&version_output.stdout)
        .trim()
        .to_string();

    // If diagnostics subcommand exists, use it
    match invoke_panic_attack(&["diagnostics"], timeout) {
        Ok(output) => {
            if let Ok(report) = serde_json::from_str::<DiagnosticsReport>(&output) {
                return Ok(report);
            }
        }
        Err(_) => {
            // diagnostics subcommand may not exist in older versions
            tracing::debug!("panic-attack diagnostics subcommand not available, using fallback");
        }
    }

    // Fallback: construct a minimal diagnostics report
    Ok(DiagnosticsReport {
        version: if version.is_empty() {
            "unknown".to_string()
        } else {
            version
        },
        healthy: version_output.status.success(),
        supported_languages: Vec::new(),
        messages: vec!["Diagnostics constructed from --version check".to_string()],
    })
}

/// Run `panic-attack migration-snapshot <target> --label <label>`.
///
/// Captures a point-in-time migration health snapshot for a ReScript repository.
/// Optionally includes build time and bundle size measurements.
pub fn run_migration_snapshot(
    target: &Path,
    label: &str,
    build_time: bool,
    bundle_size: bool,
    timeout: Duration,
) -> Result<MigrationSnapshotReport> {
    let target_str = target.display().to_string();
    let mut args = vec!["migration-snapshot", &target_str, "--label", label];

    // Allocate owned strings for optional flags so references stay valid
    let build_flag = "--build-time".to_string();
    let bundle_flag = "--bundle-size".to_string();

    if build_time {
        args.push(&build_flag);
    }
    if bundle_size {
        args.push(&bundle_flag);
    }

    let output = invoke_panic_attack(&args, timeout)
        .context("Failed to run panic-attack migration-snapshot")?;

    serde_json::from_str::<MigrationSnapshotReport>(&output)
        .context("Failed to parse panic-attack migration-snapshot output as JSON")
}

/// Run `panic-attack migration-diff <before.json> <after.json>`.
///
/// Compares two migration snapshots and produces a delta report showing
/// health score changes, deprecated API removals, and build/bundle deltas.
pub fn run_migration_diff(
    before: &Path,
    after: &Path,
    timeout: Duration,
) -> Result<MigrationDiffReport> {
    let before_str = before.display().to_string();
    let after_str = after.display().to_string();
    let args = vec!["migration-diff", &before_str, &after_str];

    let output =
        invoke_panic_attack(&args, timeout).context("Failed to run panic-attack migration-diff")?;

    serde_json::from_str::<MigrationDiffReport>(&output)
        .context("Failed to parse panic-attack migration-diff output as JSON")
}

/// Locate the `panic-attack` binary on PATH.
///
/// Returns the path to the binary, or an error if not found.
fn find_panic_attack_binary() -> Result<PathBuf> {
    // Check PATH via `which`
    let which_output = Command::new("which")
        .arg("panic-attack")
        .output()
        .context("Failed to run 'which panic-attack'")?;

    if which_output.status.success() {
        let path = String::from_utf8_lossy(&which_output.stdout)
            .trim()
            .to_string();
        return Ok(PathBuf::from(path));
    }

    // Check common locations
    let common_paths = ["/usr/local/bin/panic-attack", "/usr/bin/panic-attack"];

    for path in &common_paths {
        if Path::new(path).exists() {
            return Ok(PathBuf::from(path));
        }
    }

    // Check if it's a cargo-installed binary
    if let Ok(home) = std::env::var("HOME") {
        let cargo_bin = PathBuf::from(&home).join(".cargo/bin/panic-attack");
        if cargo_bin.exists() {
            return Ok(cargo_bin);
        }
    }

    bail!(
        "panic-attack binary not found on PATH or in common locations. \
         Install with: cargo install panic-attacker"
    )
}

/// Captured result of one `panic-attack` run.
struct RawRun {
    status: std::process::ExitStatus,
    stdout: String,
    stderr: String,
}

/// Run `panic-attack <args> --output-format json` with an ENFORCED timeout.
///
/// Handles: binary not found (clear error), a timeout (the child is killed and
/// the call fails: a hung scanner must not hang the lane or read as a run).
/// The exit status is returned for the caller to judge.
fn run_panic_attack_raw(args: &[&str], timeout: Duration) -> Result<RawRun> {
    use std::io::Read;

    let binary = find_panic_attack_binary()?;

    tracing::debug!(
        "Invoking: {} {} --output-format json",
        binary.display(),
        args.join(" ")
    );

    let mut cmd = Command::new(&binary);
    cmd.args(args);
    cmd.arg("--output-format");
    cmd.arg("json");
    cmd.stdin(std::process::Stdio::null());
    cmd.stdout(std::process::Stdio::piped());
    cmd.stderr(std::process::Stdio::piped());

    let mut child = cmd
        .spawn()
        .with_context(|| format!("Failed to spawn panic-attack at {}", binary.display()))?;

    // Drain both pipes on threads so a large report cannot fill a pipe and
    // block the child while this thread waits on the deadline.
    let (Some(mut out_pipe), Some(mut err_pipe)) = (child.stdout.take(), child.stderr.take())
    else {
        let _ = child.kill();
        let _ = child.wait();
        anyhow::bail!("panic-attack stdout/stderr pipes were not captured");
    };
    let out_t = std::thread::spawn(move || {
        let mut b = Vec::new();
        let _ = out_pipe.read_to_end(&mut b);
        b
    });
    let err_t = std::thread::spawn(move || {
        let mut b = Vec::new();
        let _ = err_pipe.read_to_end(&mut b);
        b
    });

    let deadline = std::time::Instant::now() + timeout;
    let status = loop {
        match child
            .try_wait()
            .context("Failed to wait for panic-attack subprocess")?
        {
            Some(s) => break s,
            None if std::time::Instant::now() >= deadline => {
                let _ = child.kill();
                let _ = child.wait();
                bail!(
                    "panic-attack did not finish within {}s (timed out and killed)",
                    timeout.as_secs()
                );
            }
            None => std::thread::sleep(Duration::from_millis(20)),
        }
    };

    let stdout = String::from_utf8_lossy(&out_t.join().unwrap_or_default()).to_string();
    let stderr = String::from_utf8_lossy(&err_t.join().unwrap_or_default()).to_string();
    if !stderr.is_empty() {
        tracing::debug!("panic-attack stderr: {}", stderr.trim());
    }
    Ok(RawRun {
        status,
        stdout,
        stderr,
    })
}

/// Invoke `panic-attack` for the subcommands other than `assail`.
///
/// Non-zero exit with output is tolerated here (diagnostics and some report
/// commands exit non-zero while still printing a usable document); `assail`
/// does NOT use this path, it is strict (see [`run_assail`]).
fn invoke_panic_attack(args: &[&str], timeout: Duration) -> Result<String> {
    let run = run_panic_attack_raw(args, timeout)?;

    if run.stdout.trim().is_empty() {
        bail!(
            "panic-attack produced empty output (exit code: {}). stderr: {}",
            run.status,
            run.stderr.trim()
        );
    }
    if !run.status.success() {
        tracing::warn!(
            "panic-attack exited with code {} (output may still be valid)",
            run.status
        );
    }
    Ok(run.stdout)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extract_json_strips_leading_banner() {
        // panic-attack prints a banner line before the JSON, even headless.
        let raw = "Running assail analysis on: .\n{\"weak_points\": []}";
        assert_eq!(extract_json(raw), "{\"weak_points\": []}");
    }

    #[test]
    fn extract_json_passes_clean_json_through() {
        let raw = "{\"weak_points\": []}";
        assert_eq!(extract_json(raw), raw);
        // No JSON delimiter → returned unchanged for a meaningful parse error.
        assert_eq!(extract_json("totally not json"), "totally not json");
    }

    #[test]
    fn test_parse_assail_report_envelope() {
        let json = r#"{
            "assail_report": {
                "program_path": "/tmp/test",
                "language": "rust",
                "frameworks": [],
                "statistics": {"total_lines": 120},
                "weak_points": [
                    {
                        "category": "UnsafeCode",
                        "location": "src/lib.rs:10",
                        "severity": "High",
                        "description": "unsafe block in FFI boundary"
                    }
                ]
            }
        }"#;

        let report = parse_assail_output(json).unwrap();
        assert_eq!(report.weak_points.len(), 1);
        assert_eq!(report.weak_points[0].category, "UnsafeCode");
        assert_eq!(report.language, "rust");
    }

    #[test]
    fn test_parse_assail_report_flat() {
        let json = r#"{
            "program_path": "/tmp/test",
            "language": "python",
            "frameworks": ["Flask"],
            "weak_points": [
                {
                    "category": "CommandInjection",
                    "location": "app.py:42",
                    "severity": "Critical",
                    "description": "os.system() with user input",
                    "recommended_attack": ["Memory"]
                }
            ]
        }"#;

        let report: AssailReport = serde_json::from_str(json).unwrap();
        assert_eq!(report.weak_points.len(), 1);
        assert_eq!(report.weak_points[0].category, "CommandInjection");
        assert_eq!(report.weak_points[0].severity, "Critical");
    }

    #[test]
    fn test_parse_adjudicate_report() {
        let json = r#"{
            "reports_analyzed": 3,
            "robustness_score": 67.5,
            "critical_issues": [],
            "recommendations": ["Pin all GitHub Actions to SHA"]
        }"#;

        let report: AdjudicateReport = serde_json::from_str(json).unwrap();
        assert_eq!(report.reports_analyzed, 3);
        assert!((report.robustness_score - 67.5).abs() < f64::EPSILON);
        assert_eq!(report.recommendations.len(), 1);
    }

    #[test]
    fn test_parse_migration_snapshot_report() {
        let json = r#"{
            "label": "before-v12",
            "timestamp": "2026-03-01T10:00:00Z",
            "target_path": "/tmp/rescript-repo",
            "deprecated_api_count": 42,
            "modern_api_count": 18,
            "api_migration_ratio": 0.3,
            "health_score": 0.45,
            "version_bracket": "V11",
            "config_format": "bsconfig",
            "build_time_ms": 3200,
            "bundle_size_bytes": 524288,
            "file_count": 15,
            "rescript_lines": 2800,
            "deprecated_patterns": []
        }"#;

        let report: MigrationSnapshotReport = serde_json::from_str(json).unwrap();
        assert_eq!(report.label, "before-v12");
        assert_eq!(report.deprecated_api_count, 42);
        assert_eq!(report.modern_api_count, 18);
        assert!(report.health_score < 0.5);
        assert_eq!(report.version_bracket, "V11");
        assert_eq!(report.config_format, "bsconfig");
        assert_eq!(report.build_time_ms, Some(3200));
    }

    #[test]
    fn test_parse_migration_diff_report() {
        let json = r#"{
            "before_label": "before-v12",
            "after_label": "after-v12",
            "health_delta": 0.35,
            "deprecated_delta": -30,
            "modern_delta": 25,
            "build_time_delta_ms": -800,
            "bundle_size_delta_bytes": -102400,
            "patterns_removed": [],
            "patterns_added": []
        }"#;

        let report: MigrationDiffReport = serde_json::from_str(json).unwrap();
        assert_eq!(report.before_label, "before-v12");
        assert!(report.health_delta > 0.0);
        assert!(report.deprecated_delta < 0); // Improvement
        assert!(report.modern_delta > 0); // Improvement
    }

    #[test]
    fn test_migration_snapshot_optional_fields() {
        let json = r#"{
            "label": "quick-scan",
            "timestamp": "2026-03-01T10:00:00Z",
            "target_path": "/tmp/repo",
            "deprecated_api_count": 5,
            "modern_api_count": 50,
            "api_migration_ratio": 0.91,
            "health_score": 0.88,
            "version_bracket": "V12Current",
            "config_format": "rescript_json",
            "file_count": 8,
            "rescript_lines": 1200
        }"#;

        let report: MigrationSnapshotReport = serde_json::from_str(json).unwrap();
        assert!(report.build_time_ms.is_none());
        assert!(report.bundle_size_bytes.is_none());
        assert!(report.deprecated_patterns.is_empty());
    }

    #[test]
    fn test_weak_point_optional_fields() {
        let json = r#"{
            "category": "PanicPath",
            "severity": "Medium",
            "description": "unwrap() on Result"
        }"#;

        let wp: WeakPoint = serde_json::from_str(json).unwrap();
        assert!(wp.location.is_none());
        assert!(wp.recommended_attack.is_empty());
    }
}

#[cfg(test)]
mod parity_tests {
    //! Envelope parity with the estate's direct lanes (cutover design section 5):
    //! a report that is not exactly a real scan is an error, never an empty
    //! `ran`.
    use super::*;

    const GOOD: &str = r#"Running assail analysis on: .
{"program_path":"/t","language":"rust","statistics":{"total_lines":10},"weak_points":[
  {"category":"PanicPath","location":"a.rs:1","severity":"Medium","description":"unwrap"}]}"#;

    #[test]
    fn a_real_report_with_its_banner_parses() {
        assert_eq!(parse_assail_output(GOOD).unwrap().weak_points.len(), 1);
    }

    #[test]
    fn a_clean_real_scan_is_accepted_as_zero_findings() {
        let r = parse_assail_output(
            r#"{"program_path":"/t","language":"rust","statistics":{"total_lines":10},"weak_points":[]}"#,
        )
        .unwrap();
        assert!(r.weak_points.is_empty());
    }

    #[test]
    fn exit_0_without_a_valid_report_is_an_error() {
        for bad in [
            "",
            "Running assail analysis on: .\n",
            "not json at all",
            "{}",
            "[]",
            r#"{"statistics":{"total_lines":10}}"#,
            r#"{"program_path":"/t","language":"x","statistics":{"total_lines":10},"weak_points":"none"}"#,
        ] {
            assert!(
                parse_assail_output(bad).is_err(),
                "accepted as a report: {bad:?}"
            );
        }
    }

    #[test]
    fn a_scan_that_analysed_nothing_is_an_error_not_an_empty_ran() {
        for stats in [
            r#""statistics":{"total_lines":0}"#,
            r#""statistics":{}"#,
            r#""statistics":null"#,
            r#""other":1"#,
        ] {
            let json =
                format!(r#"{{"program_path":"/t","language":"x",{stats},"weak_points":[]}}"#);
            let e = parse_assail_output(&json).unwrap_err().to_string();
            assert!(e.contains("analysed nothing"), "{stats}: {e}");
        }
    }

    #[test]
    fn a_weak_point_with_an_unknown_severity_is_an_error() {
        let json = r#"{"program_path":"/t","language":"x","statistics":{"total_lines":5},
            "weak_points":[{"category":"c","severity":"Catastrophic","description":"d"}]}"#;
        assert!(parse_assail_output(json).is_err());
    }
}
