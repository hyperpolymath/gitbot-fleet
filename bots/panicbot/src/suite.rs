// SPDX-License-Identifier: MPL-2.0
//! Suite — runs the allowed panic-attack subcommands and reports each part.
//!
//! Owner ruling 2026-11-13: panicbot runs the **full suite** unless the
//! repository's bot directives say otherwise. "Full suite" is every class (a)
//! subcommand below. Each subcommand is one *part*; every part ends as `ran`,
//! `not_applicable` (with its reason), `denied` (by the directives) or `error`
//! (loudly). Nothing is skipped silently and nothing is a fake pass.
//!
//! ## Inventory of panic-attack 2.5.5 subcommands
//!
//! Source: `src/main.rs` of panic-attack, each subcommand run against
//! `rsr-template-repo` on 2026-11-13 unless the reason says otherwise.
//!
//! **Class (a)**: a scan or analysis that runs non-interactively and yields
//! findings or a verdict. This is the suite.
//!
//! | Subcommand | Input | Yields | Panicbot runs it |
//! |---|---|---|---|
//! | `assail` | repo directory | weak points | always (if allowed) |
//! | `assay` | repo directory | proven-library substitution candidates (file and line) | always (if allowed) |
//! | `diagnostics` | none | tool health | always (if allowed); preflight only, no findings |
//! | `assault` | built program | per-axis crashes and bug signatures | only with `--target` (see below) |
//! | `ambush` | built program | same, under ambient stressors | only with `--target` |
//! | `adjudicate` | assault/ambush/amuck/abduct reports | pass/warn/fail verdict | only when a dynamic part produced a report |
//! | `attack` | built program, one axis | one axis of `assault` | `not_applicable`: `assault` runs every axis |
//! | `amuck` | one file and an exec program | mutation outcomes | `not_applicable`: panicbot has no source for the file/exec pairing |
//! | `abduct` | one file and an exec program | isolated-run outcome | `not_applicable`: same |
//! | `axial` | an exec program or existing reports | observed reactions | `not_applicable`: same (it rejects an assail report) |
//!
//! **Class (b)**: interactive or UI, never run: `tui`, `gui` (need a terminal
//! or display), `groove` (starts a long-lived server).
//!
//! **Class (c)**: infrastructure, side effects, or no repository scan; not part
//! of the suite:
//!
//! | Subcommand | Why not class (a) |
//! |---|---|
//! | `assimilate` | rewrites the tree (stages proven modules); an action, not a scan |
//! | `notify`, `sweep-tracker`, `query`, `campaign` (`register-pr`, `dismiss`, `status`, `poll`) | issue/PR side effects or a store of past findings, not a scan |
//! | `verisim-push`, `assemblyline`, `image` | push to a database, or batch-scan many repos and write a store |
//! | `attest`, `verify` | check an attestation sidecar; no repository analysis |
//! | `temporal` (`list`, `diff`) | reads the local scan-history store; errors without one |
//! | `diff`, `report`, `panll`, `a2ml-export`, `a2ml-import`, `manifest`, `analyze`, `aggregate` | consume or convert reports or prover output panicbot does not hold |
//! | `fingerprint` | a BLAKE3 hash of the tree; yields neither findings nor a verdict |
//! | `migration-snapshot`, `migration-diff` | ReScript-migration *metrics* (their embedded assail report duplicates `assail`); no findings; `--build-time` runs `rescript build` |
//! | `describe-contract`, `completions`, `help` | describe the CLI |
//! | `bridge` (`triage`, `status`) | class (a) in kind (CVE triage with reachability) but **held out**: owner ruling D4 of 2026-11-13 keeps the bridge gate outside panicbot (CVE-mitigation lifecycle, a sanctioned direct use), it needs the `http` feature and OSV network access, and it errors on a repo with no supported lockfile. Revisit with the owner |
//!
//! ## Dynamic modes: where the target comes from
//!
//! `assault` and `ambush` execute a program. Panicbot does not build the
//! scanned repository, so there is no program unless the operator gives one:
//! `--target PATH` (a built executable inside the scanned directory). Without
//! it the dynamic parts report `not_applicable` with that reason. None of the
//! CI callers passes a target, so in CI the dynamic parts are `not_applicable`.
//!
//! The security gate (checked 2026-11-13 over gitbot-fleet
//! `fleet-report-back-reusable.yml` and standards `fork-governance.yml`): both
//! run on plain `pull_request` with `contents: read` and no secrets, which is
//! unprivileged. A reusable workflow cannot know its caller's trigger, though,
//! so [`dynamic_refusal`] makes panicbot refuse to execute repository code when
//! `GITHUB_EVENT_NAME` is `pull_request_target` or `workflow_run`.
//!
//! ## Rule ids
//!
//! `assail` findings keep panic-attack's canonical `PAxxx` ids. The other
//! subcommands have no canonical id, so their findings use panicbot-local ids
//! with a `PB-` prefix (`PB-ASSAY-<candidate>`, `PB-DYN-CRASH`,
//! `PB-DYN-SIGNATURE`, `PB-DYN-FAIL`, `PB-ADJUDICATE-VERDICT`) rather than a
//! second numbering inside `PA`. Every finding names a file (gitbot-fleet #668):
//! assay names the hit's file, dynamic findings and the verdict name the target.
//!
//! ## Budget
//!
//! Parts run one after another. Each invocation gets the smaller of the
//! per-command timeout and what is left of the suite budget
//! ([`PanicbotConfig::budget`], 240 s, under the driver's 300 s adapter limit).
//! A part that cannot start inside the budget is an `error`.

use crate::config::{PanicbotConfig, Subcommand};
use crate::scanner::{
    self, AdjudicateVerdict, AssailReport, AssayReport, DiagnosticsReport, DynamicReport, Scratch,
};
use crate::translator;
use anyhow::Result;
use gitbot_shared_context::bot::BotId;
use gitbot_shared_context::finding::{Finding, Severity};
use gitbot_shared_context::triangle::TriangleTier;
use serde::Serialize;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

/// Seconds each attack axis runs for in the dynamic modes.
pub const DYNAMIC_AXIS_SECONDS: u64 = 10;

/// How one part of the suite ended.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PartStatus {
    /// It ran to a usable result (with or without findings).
    Ran,
    /// It cannot apply here; the detail says why.
    NotApplicable,
    /// The repository's directives deny it.
    Denied,
    /// It failed. Always loud.
    Error,
}

/// The outcome of one subcommand.
#[derive(Debug, Clone, Serialize)]
pub struct Part {
    /// Which subcommand.
    pub command: Subcommand,
    /// How it ended.
    pub status: PartStatus,
    /// The reason (never empty unless it ran with nothing to add).
    pub detail: String,
    /// Findings it contributed after the severity filter.
    pub findings: usize,
    /// Wall time spent in it.
    pub wall_ms: u128,
}

/// Everything a suite run produced.
#[derive(Debug, Clone, Serialize)]
pub struct SuiteRun {
    /// One entry per subcommand, in run order.
    pub parts: Vec<Part>,
    /// The findings of every part that ran.
    #[serde(skip)]
    pub findings: Vec<Finding>,
    /// The assail report, when assail ran (statistics for callers).
    #[serde(skip)]
    pub assail: Option<AssailReport>,
    /// Total wall time.
    pub wall_ms: u128,
}

impl SuiteRun {
    /// The parts that ended in `error`.
    pub fn errored(&self) -> Vec<&Part> {
        self.parts
            .iter()
            .filter(|p| p.status == PartStatus::Error)
            .collect()
    }

    /// Whether any part that analyses the repository ran. A preflight alone
    /// (`diagnostics`) is not an analysis: a run where nothing else ran must
    /// not read as a clean scan.
    pub fn analysed(&self) -> bool {
        self.parts
            .iter()
            .any(|p| p.status == PartStatus::Ran && p.command != Subcommand::Diagnostics)
    }

    /// One line naming every failed part, or `None` when none failed.
    pub fn error_summary(&self) -> Option<String> {
        let errs = self.errored();
        if errs.is_empty() {
            return None;
        }
        let list: Vec<String> = errs
            .iter()
            .map(|p| format!("{}: {}", p.command.name(), p.detail))
            .collect();
        Some(format!(
            "{} of {} panic-attack part(s) failed: {}",
            errs.len(),
            self.parts.len(),
            list.join("; ")
        ))
    }

    /// One line per part, for logs and the text output.
    pub fn table(&self) -> Vec<String> {
        self.parts
            .iter()
            .map(|p| {
                let status = match p.status {
                    PartStatus::Ran => "ran",
                    PartStatus::NotApplicable => "not_applicable",
                    PartStatus::Denied => "denied",
                    PartStatus::Error => "ERROR",
                };
                format!(
                    "{:<12} {:<15} {:>4} finding(s) {:>6} ms  {}",
                    p.command.name(),
                    status,
                    p.findings,
                    p.wall_ms,
                    p.detail
                )
            })
            .collect()
    }
}

/// How a suite run is invoked.
#[derive(Debug, Clone, Default)]
pub struct SuiteOptions {
    /// A built program for the dynamic modes. None: they are `not_applicable`.
    pub target: Option<PathBuf>,
    /// The CI event name (`GITHUB_EVENT_NAME`), for the dynamic-mode gate.
    pub ci_event: Option<String>,
}

impl SuiteOptions {
    /// Options from the environment, with an optional explicit target.
    pub fn from_env(target: Option<PathBuf>) -> Self {
        Self {
            target,
            ci_event: std::env::var("GITHUB_EVENT_NAME").ok(),
        }
    }
}

/// The panic-attack side of the suite, as a trait so the orchestration can be
/// tested without the binary.
pub trait Engine {
    /// `panic-attack assail`.
    fn assail(&self, dir: &Path, timeout: Duration) -> Result<AssailReport>;
    /// `panic-attack assay`.
    fn assay(&self, dir: &Path, timeout: Duration) -> Result<AssayReport>;
    /// `panic-attack assault|ambush`, keeping the raw report at `keep`.
    fn dynamic(
        &self,
        mode: &str,
        program: &Path,
        source: &Path,
        timeout: Duration,
        keep: &Path,
    ) -> Result<DynamicReport>;
    /// `panic-attack adjudicate`.
    fn adjudicate(&self, reports: &[PathBuf], timeout: Duration) -> Result<AdjudicateVerdict>;
    /// The tool's health.
    fn diagnostics(&self, timeout: Duration) -> Result<DiagnosticsReport>;
}

/// The real panic-attack binary.
pub struct PanicAttack;

impl Engine for PanicAttack {
    fn assail(&self, dir: &Path, timeout: Duration) -> Result<AssailReport> {
        scanner::run_assail(dir, timeout)
    }
    fn assay(&self, dir: &Path, timeout: Duration) -> Result<AssayReport> {
        scanner::run_assay(dir, timeout)
    }
    fn dynamic(
        &self,
        mode: &str,
        program: &Path,
        source: &Path,
        timeout: Duration,
        keep: &Path,
    ) -> Result<DynamicReport> {
        scanner::run_dynamic(mode, program, source, DYNAMIC_AXIS_SECONDS, timeout, keep)
    }
    fn adjudicate(&self, reports: &[PathBuf], timeout: Duration) -> Result<AdjudicateVerdict> {
        scanner::run_adjudicate_verdict(reports, timeout)
    }
    fn diagnostics(&self, timeout: Duration) -> Result<DiagnosticsReport> {
        scanner::run_diagnostics(timeout)
    }
}

/// Why panicbot must not execute repository code right now, if it must not.
///
/// A reusable workflow cannot see its caller's trigger policy, and the
/// privileged triggers run with secrets or a write token. Refuse to execute
/// the scanned repository's code under them.
pub fn dynamic_refusal(opts: &SuiteOptions) -> Option<String> {
    match opts.ci_event.as_deref() {
        Some(ev @ ("pull_request_target" | "workflow_run")) => Some(format!(
            "refused to execute repository code under the privileged '{ev}' trigger"
        )),
        _ => None,
    }
}

/// Resolve `--target` to a canonical executable inside `dir`.
fn resolve_target(dir: &Path, target: &Path) -> std::result::Result<PathBuf, String> {
    let root = dir
        .canonicalize()
        .map_err(|e| format!("cannot resolve the scanned directory: {e}"))?;
    let joined = if target.is_absolute() {
        target.to_path_buf()
    } else {
        root.join(target)
    };
    let real = joined
        .canonicalize()
        .map_err(|e| format!("target {} is not usable: {e}", target.display()))?;
    if !real.starts_with(&root) {
        return Err(format!(
            "target {} is outside the scanned directory; refusing to execute it",
            target.display()
        ));
    }
    if !real.is_file() {
        return Err(format!("target {} is not a file", target.display()));
    }
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mode = real
            .metadata()
            .map_err(|e| format!("cannot stat target: {e}"))?
            .permissions()
            .mode();
        if mode & 0o111 == 0 {
            return Err(format!("target {} is not executable", target.display()));
        }
    }
    Ok(real)
}

fn severity_value(s: &Severity) -> u8 {
    match s {
        Severity::Error => 3,
        Severity::Warning => 2,
        Severity::Info => 1,
        Severity::Suggestion => 0,
    }
}

/// A repository-relative path for a finding: strips the scanned directory,
/// leaves anything else as given.
fn rel(dir: &Path, p: &Path) -> PathBuf {
    let canon = dir.canonicalize().ok();
    for base in std::iter::once(dir.to_path_buf()).chain(canon) {
        if let Ok(r) = p.strip_prefix(&base) {
            return r.to_path_buf();
        }
    }
    p.to_path_buf()
}

/// Findings for an `assay` report. Candidates already applied are not findings.
fn assay_findings(dir: &Path, report: &AssayReport) -> (Vec<Finding>, usize) {
    let mut out = Vec::new();
    let mut unlocated = 0;
    for c in &report.candidates {
        if c.status == "already-applied" {
            continue;
        }
        if c.target_hits.is_empty() {
            unlocated += 1;
            continue;
        }
        for hit in &c.target_hits {
            let f = Finding::new(
                BotId::Panicbot,
                &format!("PB-ASSAY-{}", c.id.to_uppercase()),
                Severity::Info,
                &format!(
                    "unproven pattern has a proven drop-in equivalent: {} ({}); status {}",
                    c.proven_name, c.proof_backing, c.status
                ),
            )
            .with_rule_name("Proven-library substitution candidate")
            .with_category("static-analysis/proven-substitution")
            .with_triangle_tier(TriangleTier::Substitute)
            .with_confidence(c.confidence)
            .with_file(rel(dir, &hit.file))
            .with_line(hit.line as usize)
            .with_metadata(serde_json::json!({
                "panic_attack_subcommand": "assay",
                "candidate": c.id,
                "status": c.status,
            }));
            out.push(f);
        }
    }
    (out, unlocated)
}

/// Findings for an `assault`/`ambush` report. They name the target program.
fn dynamic_findings(mode: &str, target_rel: &Path, report: &DynamicReport) -> Vec<Finding> {
    let mut out = Vec::new();
    let base = |rule: &str, sev: Severity, msg: String| {
        Finding::new(BotId::Panicbot, rule, sev, &msg)
            .with_category("dynamic-analysis")
            .with_triangle_tier(TriangleTier::Control)
            .with_confidence(0.9)
            .with_file(target_rel.to_path_buf())
            .with_metadata(serde_json::json!({ "panic_attack_subcommand": mode }))
    };
    for r in &report.attack_results {
        if r.skipped {
            continue;
        }
        if !r.crashes.is_empty() {
            out.push(
                base(
                    "PB-DYN-CRASH",
                    Severity::Error,
                    format!(
                        "program crashed under the {} {mode} ({} crash(es))",
                        r.axis,
                        r.crashes.len()
                    ),
                )
                .with_rule_name("Program crash under attack"),
            );
        }
        for s in &r.signatures_detected {
            let kind = s
                .get("signature_type")
                .and_then(|v| v.as_str())
                .unwrap_or("unknown");
            out.push(
                base(
                    "PB-DYN-SIGNATURE",
                    Severity::Warning,
                    format!("bug signature {kind} detected under the {} {mode}", r.axis),
                )
                .with_rule_name("Bug signature under attack"),
            );
        }
        if !r.success && r.crashes.is_empty() {
            out.push(
                base(
                    "PB-DYN-FAIL",
                    Severity::Warning,
                    format!(
                        "program failed under the {} {mode} (exit code {})",
                        r.axis,
                        r.exit_code
                            .map(|c| c.to_string())
                            .unwrap_or_else(|| "none".to_string())
                    ),
                )
                .with_rule_name("Program failure under attack"),
            );
        }
    }
    out
}

/// The finding for an adjudicated verdict, if it is not a pass.
fn verdict_finding(target_rel: &Path, v: &AdjudicateVerdict) -> Option<Finding> {
    let sev = match v.verdict.as_str() {
        "fail" => Severity::Warning,
        "warn" => Severity::Info,
        _ => return None,
    };
    let why: Vec<&str> = v
        .priorities
        .iter()
        .filter(|p| p.level != "info")
        .take(3)
        .map(|p| p.message.as_str())
        .collect();
    Some(
        Finding::new(
            BotId::Panicbot,
            "PB-ADJUDICATE-VERDICT",
            sev,
            &format!("adjudicated verdict {}: {}", v.verdict, why.join("; ")),
        )
        .with_rule_name("Adjudicated campaign verdict")
        .with_category("dynamic-analysis")
        .with_triangle_tier(TriangleTier::Control)
        .with_confidence(0.8)
        .with_file(target_rel.to_path_buf())
        .with_metadata(serde_json::json!({ "panic_attack_subcommand": "adjudicate" })),
    )
}

/// Run the suite against `dir` with the real panic-attack binary.
pub fn run_suite(dir: &Path, config: &PanicbotConfig, opts: &SuiteOptions) -> SuiteRun {
    run_suite_with(&PanicAttack, dir, config, opts)
}

/// Run the suite against `dir` with `engine`.
pub fn run_suite_with(
    engine: &dyn Engine,
    dir: &Path,
    config: &PanicbotConfig,
    opts: &SuiteOptions,
) -> SuiteRun {
    let start = Instant::now();
    let mut parts: Vec<Part> = Vec::new();
    let mut findings: Vec<Finding> = Vec::new();
    let mut assail_report: Option<AssailReport> = None;
    let mut dynamic_reports: Vec<PathBuf> = Vec::new();
    let mut target_rel: Option<PathBuf> = None;
    let mut scratch: Option<Scratch> = None;
    let min = config.min_severity.threshold();

    // The part's timeout: the per-command timeout, capped by what is left.
    let slot = |start: &Instant| -> Option<Duration> {
        let left = config.budget.checked_sub(start.elapsed())?;
        if left.is_zero() {
            None
        } else {
            Some(config.timeout.min(left))
        }
    };

    for cmd in Subcommand::ALL {
        let t0 = Instant::now();
        let mut push = |status: PartStatus, detail: String, n: usize| {
            parts.push(Part {
                command: cmd,
                status,
                detail,
                findings: n,
                wall_ms: t0.elapsed().as_millis(),
            });
        };

        if !config.allowed_commands.allows(cmd) {
            push(
                PartStatus::Denied,
                "denied by the repository's bot directives".into(),
                0,
            );
            continue;
        }

        match cmd {
            Subcommand::Assail | Subcommand::Assay | Subcommand::Diagnostics => {
                let Some(timeout) = slot(&start) else {
                    push(PartStatus::Error, budget_msg(config), 0);
                    continue;
                };
                match cmd {
                    Subcommand::Assail => match engine.assail(dir, timeout) {
                        Ok(report) => {
                            let fs = translator::translate_all(&report.weak_points, config);
                            let n = fs.len();
                            findings.extend(fs);
                            assail_report = Some(report);
                            push(PartStatus::Ran, String::new(), n);
                        }
                        Err(e) => push(PartStatus::Error, format!("{e:#}"), 0),
                    },
                    Subcommand::Assay => match engine.assay(dir, timeout) {
                        Ok(report) => {
                            let (fs, unlocated) = assay_findings(dir, &report);
                            let fs: Vec<Finding> = fs
                                .into_iter()
                                .filter(|f| severity_value(&f.severity) >= min)
                                .collect();
                            let n = fs.len();
                            findings.extend(fs);
                            let detail = if unlocated > 0 {
                                format!("{unlocated} candidate(s) without a located file not reported as findings")
                            } else {
                                String::new()
                            };
                            push(PartStatus::Ran, detail, n);
                        }
                        Err(e) => push(PartStatus::Error, format!("{e:#}"), 0),
                    },
                    _ => match engine.diagnostics(timeout) {
                        Ok(d) if d.healthy => push(PartStatus::Ran, d.version.clone(), 0),
                        Ok(d) => push(
                            PartStatus::Error,
                            format!(
                                "panic-attack reports itself unhealthy ({}): {}",
                                d.version,
                                d.messages.join("; ")
                            ),
                            0,
                        ),
                        Err(e) => push(PartStatus::Error, format!("{e:#}"), 0),
                    },
                }
            }
            Subcommand::Assault | Subcommand::Ambush => {
                let Some(target) = &opts.target else {
                    push(PartStatus::NotApplicable, no_target_msg(), 0);
                    continue;
                };
                if let Some(why) = dynamic_refusal(opts) {
                    push(PartStatus::Error, why, 0);
                    continue;
                }
                let program = match resolve_target(dir, target) {
                    Ok(p) => p,
                    Err(why) => {
                        push(PartStatus::Error, why, 0);
                        continue;
                    }
                };
                let Some(timeout) = slot(&start) else {
                    push(PartStatus::Error, budget_msg(config), 0);
                    continue;
                };
                if scratch.is_none() {
                    match Scratch::new() {
                        Ok(s) => scratch = Some(s),
                        Err(e) => {
                            push(PartStatus::Error, format!("{e:#}"), 0);
                            continue;
                        }
                    }
                }
                let Some(sc) = scratch.as_ref() else {
                    push(PartStatus::Error, "scratch directory unavailable".into(), 0);
                    continue;
                };
                let keep = sc.path().join(format!("{}.json", cmd.name()));
                let trel = rel(dir, &program);
                match engine.dynamic(cmd.name(), &program, dir, timeout, &keep) {
                    Ok(report) if report.attack_results.iter().all(|r| r.skipped) => {
                        // Every axis was skipped: nothing was attacked, so
                        // this is not a clean run. Not applicable, with why.
                        let reasons: Vec<&str> = report
                            .attack_results
                            .iter()
                            .filter_map(|r| r.skip_reason.as_deref())
                            .collect();
                        let why = if reasons.is_empty() {
                            "no reason given".to_string()
                        } else {
                            reasons.join("; ")
                        };
                        push(
                            PartStatus::NotApplicable,
                            format!(
                                "panic-attack skipped all {} axis result(s), nothing was attacked: {why}",
                                report.attack_results.len()
                            ),
                            0,
                        );
                    }
                    Ok(report) => {
                        let fs: Vec<Finding> = dynamic_findings(cmd.name(), &trel, &report)
                            .into_iter()
                            .filter(|f| severity_value(&f.severity) >= min)
                            .collect();
                        let n = fs.len();
                        findings.extend(fs);
                        dynamic_reports.push(keep);
                        target_rel = Some(trel);
                        let skipped = report.attack_results.iter().filter(|r| r.skipped).count();
                        let detail = if skipped > 0 {
                            format!("{skipped} axis result(s) skipped by panic-attack")
                        } else {
                            String::new()
                        };
                        push(PartStatus::Ran, detail, n);
                    }
                    Err(e) => push(PartStatus::Error, format!("{e:#}"), 0),
                }
            }
            Subcommand::Attack => push(
                PartStatus::NotApplicable,
                "covered by assault: assault runs the attack engine on every axis, \
                 so a single-axis attack would only repeat it"
                    .into(),
                0,
            ),
            Subcommand::Amuck | Subcommand::Abduct | Subcommand::Axial => push(
                PartStatus::NotApplicable,
                format!(
                    "{} needs one target file and an exec program (or reports it accepts) \
                     and panicbot has no source for either; not run",
                    cmd.name()
                ),
                0,
            ),
            Subcommand::Adjudicate => {
                if dynamic_reports.is_empty() {
                    push(
                        PartStatus::NotApplicable,
                        "no dynamic report to adjudicate: no dynamic part ran".into(),
                        0,
                    );
                    continue;
                }
                let Some(timeout) = slot(&start) else {
                    push(PartStatus::Error, budget_msg(config), 0);
                    continue;
                };
                match engine.adjudicate(&dynamic_reports, timeout) {
                    Ok(v) => {
                        let fs: Vec<Finding> = target_rel
                            .as_deref()
                            .and_then(|t| verdict_finding(t, &v))
                            .into_iter()
                            .filter(|f| severity_value(&f.severity) >= min)
                            .collect();
                        let n = fs.len();
                        findings.extend(fs);
                        push(PartStatus::Ran, format!("verdict {}", v.verdict), n);
                    }
                    Err(e) => push(PartStatus::Error, format!("{e:#}"), 0),
                }
            }
        }
    }

    SuiteRun {
        parts,
        findings,
        assail: assail_report,
        wall_ms: start.elapsed().as_millis(),
    }
}

fn budget_msg(config: &PanicbotConfig) -> String {
    format!(
        "the {}s suite budget was spent before this part could start",
        config.budget.as_secs()
    )
}

fn no_target_msg() -> String {
    "no target program: panicbot does not build the scanned repository and none was \
     given (--target PATH)"
        .into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::AllowedCommands;
    use crate::scanner::{AdjudicatePriority, AssayCandidate, AssayHit, AxisResult};
    use std::cell::RefCell;

    /// A scripted panic-attack: records calls, fails on request.
    struct Fake {
        calls: RefCell<Vec<String>>,
        timeouts: RefCell<Vec<Duration>>,
        keeps: RefCell<Vec<PathBuf>>,
        adjudicated: RefCell<Vec<PathBuf>>,
        fail: Vec<&'static str>,
        healthy: bool,
        crash: bool,
        all_skipped: bool,
        verdict: &'static str,
    }

    impl Fake {
        fn new() -> Self {
            Fake {
                calls: RefCell::new(Vec::new()),
                timeouts: RefCell::new(Vec::new()),
                keeps: RefCell::new(Vec::new()),
                adjudicated: RefCell::new(Vec::new()),
                fail: Vec::new(),
                healthy: true,
                crash: true,
                all_skipped: false,
                verdict: "fail",
            }
        }
        fn failing(names: &[&'static str]) -> Self {
            Fake {
                fail: names.to_vec(),
                ..Fake::new()
            }
        }
        fn called(&self) -> Vec<String> {
            self.calls.borrow().clone()
        }
        fn note(&self, what: &str, timeout: Duration) -> Result<()> {
            self.calls.borrow_mut().push(what.to_string());
            self.timeouts.borrow_mut().push(timeout);
            if self.fail.contains(&what) {
                anyhow::bail!("{what} blew up");
            }
            Ok(())
        }
    }

    impl Engine for Fake {
        fn assail(&self, _dir: &Path, timeout: Duration) -> Result<AssailReport> {
            self.note("assail", timeout)?;
            Ok(AssailReport {
                program_path: PathBuf::from("x"),
                language: "rust".into(),
                frameworks: vec![],
                weak_points: vec![scanner::WeakPoint {
                    category: "UnsafeCode".into(),
                    location: Some("src/ffi.rs:42".into()),
                    severity: "High".into(),
                    description: "unsafe".into(),
                    recommended_attack: vec![],
                    suppressed: false,
                    test_context: None,
                }],
                statistics: serde_json::Value::Null,
                file_statistics: vec![],
                recommended_attacks: vec![],
                dependency_graph: serde_json::Value::Null,
                taint_matrix: serde_json::Value::Null,
            })
        }
        fn assay(&self, dir: &Path, timeout: Duration) -> Result<AssayReport> {
            self.note("assay", timeout)?;
            let cand = |id: &str, status: &str, hits: Vec<AssayHit>| AssayCandidate {
                id: id.into(),
                proven_name: "SafePath".into(),
                proof_backing: "proven: Operations.idr".into(),
                status: status.into(),
                confidence: 0.9,
                target_hits: hits,
            };
            Ok(AssayReport {
                candidates: vec![
                    cand(
                        "safe-path",
                        "offered",
                        vec![AssayHit {
                            file: dir.join("src/p.rs"),
                            line: 7,
                        }],
                    ),
                    cand("done", "already-applied", vec![]),
                    cand("nowhere", "offered", vec![]),
                ],
                catalogue_entries: 3,
                notes: vec![],
            })
        }
        fn dynamic(
            &self,
            mode: &str,
            _program: &Path,
            _source: &Path,
            timeout: Duration,
            keep: &Path,
        ) -> Result<DynamicReport> {
            self.note(mode, timeout)?;
            self.keeps.borrow_mut().push(keep.to_path_buf());
            Ok(DynamicReport {
                attack_results: if self.all_skipped {
                    vec![AxisResult {
                        axis: "cpu".into(),
                        success: false,
                        skipped: true,
                        skip_reason: Some("no runnable program".into()),
                        exit_code: None,
                        crashes: vec![],
                        signatures_detected: vec![],
                    }]
                } else {
                    vec![
                        AxisResult {
                            axis: "cpu".into(),
                            success: !self.crash,
                            skipped: false,
                            skip_reason: None,
                            exit_code: None,
                            crashes: if self.crash {
                                vec![serde_json::json!({})]
                            } else {
                                vec![]
                            },
                            signatures_detected: vec![],
                        },
                        AxisResult {
                            axis: "time".into(),
                            success: false,
                            skipped: true,
                            skip_reason: Some("n/a".into()),
                            exit_code: None,
                            crashes: vec![],
                            signatures_detected: vec![],
                        },
                    ]
                },
                total_crashes: 1,
                total_signatures: 0,
            })
        }
        fn adjudicate(&self, reports: &[PathBuf], timeout: Duration) -> Result<AdjudicateVerdict> {
            self.note("adjudicate", timeout)?;
            self.adjudicated
                .borrow_mut()
                .extend(reports.iter().cloned());
            Ok(AdjudicateVerdict {
                verdict: self.verdict.into(),
                processed_reports: reports.len(),
                failed_reports: 0,
                notes: vec![],
                priorities: vec![AdjudicatePriority {
                    level: "high".into(),
                    message: "1 crashes detected".into(),
                }],
            })
        }
        fn diagnostics(&self, timeout: Duration) -> Result<DiagnosticsReport> {
            self.note("diagnostics", timeout)?;
            Ok(DiagnosticsReport {
                version: "panic-attack 2.5.5".into(),
                healthy: self.healthy,
                supported_languages: vec![],
                messages: vec!["bad".into()],
            })
        }
    }

    fn status_of(run: &SuiteRun, cmd: Subcommand) -> PartStatus {
        run.parts
            .iter()
            .find(|p| p.command == cmd)
            .map(|p| p.status)
            .expect("every subcommand has a part")
    }

    fn detail_of(run: &SuiteRun, cmd: Subcommand) -> String {
        run.parts
            .iter()
            .find(|p| p.command == cmd)
            .map(|p| p.detail.clone())
            .unwrap_or_default()
    }

    /// A directory holding an executable `prog` and a non-executable `data`.
    fn repo_with_program() -> tempfile::TempDir {
        use std::os::unix::fs::PermissionsExt;
        let d = tempfile::tempdir().unwrap();
        let prog = d.path().join("prog");
        std::fs::write(&prog, "#!/bin/sh\n").unwrap();
        std::fs::set_permissions(&prog, std::fs::Permissions::from_mode(0o755)).unwrap();
        std::fs::write(d.path().join("data"), "x").unwrap();
        d
    }

    fn opts(target: Option<&str>, event: Option<&str>) -> SuiteOptions {
        SuiteOptions {
            target: target.map(PathBuf::from),
            ci_event: event.map(String::from),
        }
    }

    #[test]
    fn default_config_runs_the_full_suite_and_reports_every_part() {
        let fake = Fake::new();
        let run = run_suite_with(
            &fake,
            Path::new("."),
            &PanicbotConfig::default(),
            &opts(None, None),
        );
        // The static class (a) parts run.
        assert_eq!(fake.called(), ["assail", "assay", "diagnostics"]);
        // Every class (a) subcommand has exactly one part, in suite order.
        let names: Vec<Subcommand> = run.parts.iter().map(|p| p.command).collect();
        assert_eq!(names, Subcommand::ALL);
        assert!(run.parts.iter().all(|p| p.status != PartStatus::Denied));
        assert_eq!(status_of(&run, Subcommand::Assail), PartStatus::Ran);
        assert_eq!(status_of(&run, Subcommand::Assay), PartStatus::Ran);
        // With no target the dynamic parts say so: never a silent skip, never a pass.
        for cmd in [Subcommand::Assault, Subcommand::Ambush] {
            assert_eq!(status_of(&run, cmd), PartStatus::NotApplicable);
            assert!(detail_of(&run, cmd).contains("no target program"));
        }
        for cmd in [
            Subcommand::Attack,
            Subcommand::Amuck,
            Subcommand::Abduct,
            Subcommand::Axial,
        ] {
            assert_eq!(status_of(&run, cmd), PartStatus::NotApplicable);
            assert!(!detail_of(&run, cmd).is_empty());
        }
        assert_eq!(
            status_of(&run, Subcommand::Adjudicate),
            PartStatus::NotApplicable
        );
        assert!(run.errored().is_empty());
        assert!(run.analysed());
        assert!(run.error_summary().is_none());
    }

    #[test]
    fn deny_removes_one_subcommand_and_never_calls_it() {
        let fake = Fake::new();
        let mut config = PanicbotConfig::default();
        config.allowed_commands.set(Subcommand::Assay, false);
        let run = run_suite_with(&fake, Path::new("."), &config, &opts(None, None));
        assert_eq!(fake.called(), ["assail", "diagnostics"]);
        assert_eq!(status_of(&run, Subcommand::Assay), PartStatus::Denied);
        assert!(detail_of(&run, Subcommand::Assay).contains("directives"));
        assert_eq!(status_of(&run, Subcommand::Assail), PartStatus::Ran);
    }

    #[test]
    fn explicit_allow_runs_only_what_it_names() {
        let fake = Fake::new();
        let mut config = PanicbotConfig {
            allowed_commands: AllowedCommands::none(),
            ..PanicbotConfig::default()
        };
        config.allowed_commands.set(Subcommand::Assay, true);
        let run = run_suite_with(&fake, Path::new("."), &config, &opts(None, None));
        assert_eq!(fake.called(), ["assay"]);
        assert_eq!(status_of(&run, Subcommand::Assail), PartStatus::Denied);
        assert_eq!(status_of(&run, Subcommand::Assay), PartStatus::Ran);
    }

    #[test]
    fn a_failing_part_is_an_error_and_does_not_drop_the_others() {
        let fake = Fake::failing(&["assay"]);
        let run = run_suite_with(
            &fake,
            Path::new("."),
            &PanicbotConfig::default(),
            &opts(None, None),
        );
        assert_eq!(status_of(&run, Subcommand::Assay), PartStatus::Error);
        assert!(detail_of(&run, Subcommand::Assay).contains("assay blew up"));
        // assail's finding is still there, and the failure is named in the summary.
        assert_eq!(
            run.findings.iter().filter(|f| f.rule_id == "PA004").count(),
            1
        );
        let summary = run.error_summary().expect("a failed part has a summary");
        assert!(summary.contains("assay: assay blew up"), "{summary}");
        assert!(summary.starts_with("1 of 10"), "{summary}");
    }

    #[test]
    fn a_failing_assail_is_an_error_not_an_empty_run() {
        let fake = Fake::failing(&["assail"]);
        let run = run_suite_with(
            &fake,
            Path::new("."),
            &PanicbotConfig::default(),
            &opts(None, None),
        );
        assert_eq!(status_of(&run, Subcommand::Assail), PartStatus::Error);
        assert!(run.findings.iter().all(|f| f.rule_id != "PA004"));
        assert!(run.error_summary().is_some());
    }

    #[test]
    fn an_unhealthy_tool_is_an_error() {
        let fake = Fake {
            healthy: false,
            ..Fake::new()
        };
        let run = run_suite_with(
            &fake,
            Path::new("."),
            &PanicbotConfig::default(),
            &opts(None, None),
        );
        assert_eq!(status_of(&run, Subcommand::Diagnostics), PartStatus::Error);
        assert!(detail_of(&run, Subcommand::Diagnostics).contains("unhealthy"));
    }

    #[test]
    fn a_preflight_alone_is_not_an_analysis() {
        let fake = Fake::new();
        let mut config = PanicbotConfig {
            allowed_commands: AllowedCommands::none(),
            ..PanicbotConfig::default()
        };
        config.allowed_commands.set(Subcommand::Diagnostics, true);
        let run = run_suite_with(&fake, Path::new("."), &config, &opts(None, None));
        assert_eq!(status_of(&run, Subcommand::Diagnostics), PartStatus::Ran);
        assert!(
            !run.analysed(),
            "diagnostics alone must not read as a clean scan"
        );
    }

    #[test]
    fn nothing_allowed_means_nothing_analysed() {
        let fake = Fake::new();
        let config = PanicbotConfig {
            allowed_commands: AllowedCommands::none(),
            ..PanicbotConfig::default()
        };
        let run = run_suite_with(&fake, Path::new("."), &config, &opts(None, None));
        assert!(fake.called().is_empty());
        assert!(!run.analysed());
        assert!(run.parts.iter().all(|p| p.status == PartStatus::Denied));
    }

    #[test]
    fn assay_findings_name_their_file_and_line() {
        let fake = Fake::new();
        let dir = PathBuf::from("/repo");
        let run = run_suite_with(&fake, &dir, &PanicbotConfig::default(), &opts(None, None));
        let f: Vec<&Finding> = run
            .findings
            .iter()
            .filter(|f| f.rule_id.starts_with("PB-ASSAY"))
            .collect();
        assert_eq!(
            f.len(),
            1,
            "already-applied and location-less candidates are not findings"
        );
        assert_eq!(f[0].rule_id, "PB-ASSAY-SAFE-PATH");
        assert_eq!(f[0].file.as_deref(), Some(Path::new("src/p.rs")));
        assert_eq!(f[0].line, Some(7));
        assert!(
            detail_of(&run, Subcommand::Assay).contains("1 candidate(s) without a located file")
        );
    }

    #[test]
    fn the_severity_filter_applies_to_every_part() {
        let fake = Fake::new();
        let config = PanicbotConfig {
            min_severity: crate::config::MinSeverity::High,
            ..PanicbotConfig::default()
        };
        let run = run_suite_with(&fake, Path::new("/repo"), &config, &opts(None, None));
        // assay findings are info: below High, dropped by the same filter assail uses.
        assert!(run
            .findings
            .iter()
            .all(|f| !f.rule_id.starts_with("PB-ASSAY")));
        assert_eq!(status_of(&run, Subcommand::Assay), PartStatus::Ran);
    }

    #[test]
    fn dynamic_modes_with_a_target_run_and_feed_adjudicate() {
        let repo = repo_with_program();
        let fake = Fake::new();
        let run = run_suite_with(
            &fake,
            repo.path(),
            &PanicbotConfig::default(),
            &opts(Some("prog"), Some("pull_request")),
        );
        assert_eq!(
            fake.called(),
            [
                "assail",
                "assay",
                "assault",
                "ambush",
                "adjudicate",
                "diagnostics"
            ]
        );
        assert_eq!(status_of(&run, Subcommand::Assault), PartStatus::Ran);
        assert_eq!(status_of(&run, Subcommand::Ambush), PartStatus::Ran);
        assert_eq!(status_of(&run, Subcommand::Adjudicate), PartStatus::Ran);
        // Both dynamic reports were handed to adjudicate.
        assert_eq!(fake.adjudicated.borrow().len(), 2);
        assert_eq!(*fake.adjudicated.borrow(), *fake.keeps.borrow());
        // The crash and the verdict name the target, relative to the repo.
        let crash: Vec<&Finding> = run
            .findings
            .iter()
            .filter(|f| f.rule_id == "PB-DYN-CRASH")
            .collect();
        assert_eq!(crash.len(), 2, "one per dynamic mode");
        for f in crash {
            assert_eq!(f.file.as_deref(), Some(Path::new("prog")));
            assert_eq!(f.severity, Severity::Error);
        }
        let verdict: Vec<&Finding> = run
            .findings
            .iter()
            .filter(|f| f.rule_id == "PB-ADJUDICATE-VERDICT")
            .collect();
        assert_eq!(verdict.len(), 1);
        assert_eq!(verdict[0].file.as_deref(), Some(Path::new("prog")));
        // A skipped axis is reported in the detail, not turned into a finding.
        assert!(detail_of(&run, Subcommand::Assault).contains("1 axis result(s) skipped"));
    }

    #[test]
    fn a_dynamic_report_with_every_axis_skipped_is_not_applicable_never_ran() {
        let repo = repo_with_program();
        let fake = Fake {
            all_skipped: true,
            ..Fake::new()
        };
        let run = run_suite_with(
            &fake,
            repo.path(),
            &PanicbotConfig::default(),
            &opts(Some("prog"), Some("pull_request")),
        );
        for cmd in [Subcommand::Assault, Subcommand::Ambush] {
            assert_eq!(status_of(&run, cmd), PartStatus::NotApplicable);
            let d = detail_of(&run, cmd);
            assert!(
                d.contains("nothing was attacked") && d.contains("no runnable program"),
                "{d}"
            );
        }
        // Nothing dynamic ran, so there is nothing to adjudicate.
        assert_eq!(
            status_of(&run, Subcommand::Adjudicate),
            PartStatus::NotApplicable
        );
        assert!(!fake.called().contains(&"adjudicate".to_string()));
        assert!(run.errored().is_empty());
    }

    #[test]
    fn a_passing_verdict_is_not_a_finding() {
        let repo = repo_with_program();
        let fake = Fake {
            verdict: "pass",
            crash: false,
            ..Fake::new()
        };
        let run = run_suite_with(
            &fake,
            repo.path(),
            &PanicbotConfig::default(),
            &opts(Some("prog"), None),
        );
        assert!(run
            .findings
            .iter()
            .all(|f| f.rule_id != "PB-ADJUDICATE-VERDICT"));
        assert!(detail_of(&run, Subcommand::Adjudicate).contains("verdict pass"));
    }

    #[test]
    fn privileged_triggers_refuse_to_execute_repository_code() {
        let repo = repo_with_program();
        for ev in ["pull_request_target", "workflow_run"] {
            let fake = Fake::new();
            let run = run_suite_with(
                &fake,
                repo.path(),
                &PanicbotConfig::default(),
                &opts(Some("prog"), Some(ev)),
            );
            assert!(!fake.called().contains(&"assault".to_string()), "{ev}");
            assert!(!fake.called().contains(&"ambush".to_string()), "{ev}");
            assert_eq!(status_of(&run, Subcommand::Assault), PartStatus::Error);
            assert!(detail_of(&run, Subcommand::Assault).contains(ev));
            // The static parts are unaffected.
            assert_eq!(status_of(&run, Subcommand::Assail), PartStatus::Ran);
        }
        assert!(dynamic_refusal(&opts(None, Some("pull_request"))).is_none());
        assert!(dynamic_refusal(&opts(None, None)).is_none());
    }

    #[test]
    fn a_target_that_is_missing_outside_or_not_executable_is_an_error() {
        let repo = repo_with_program();
        let outside = repo_with_program();
        let outside_prog = outside.path().join("prog");
        for (label, target) in [
            ("missing", "nope".to_string()),
            ("not executable", "data".to_string()),
            ("outside the repo", outside_prog.display().to_string()),
            (
                "traversal",
                format!(
                    "../{}/prog",
                    outside.path().file_name().unwrap().to_string_lossy()
                ),
            ),
        ] {
            let fake = Fake::new();
            let run = run_suite_with(
                &fake,
                repo.path(),
                &PanicbotConfig::default(),
                &opts(Some(&target), None),
            );
            assert_eq!(
                status_of(&run, Subcommand::Assault),
                PartStatus::Error,
                "{label}"
            );
            assert!(
                !fake.called().contains(&"assault".to_string()),
                "{label}: must not execute it"
            );
        }
    }

    #[test]
    fn each_part_gets_the_smaller_of_the_timeout_and_the_remaining_budget() {
        let fake = Fake::new();
        let config = PanicbotConfig {
            timeout: Duration::from_secs(300),
            budget: Duration::from_secs(100),
            ..PanicbotConfig::default()
        };
        let _ = run_suite_with(&fake, Path::new("."), &config, &opts(None, None));
        for t in fake.timeouts.borrow().iter() {
            assert!(*t <= Duration::from_secs(100), "{t:?} exceeds the budget");
        }
        let short = PanicbotConfig {
            timeout: Duration::from_secs(5),
            ..PanicbotConfig::default()
        };
        let fake2 = Fake::new();
        let _ = run_suite_with(&fake2, Path::new("."), &short, &opts(None, None));
        for t in fake2.timeouts.borrow().iter() {
            assert!(
                *t <= Duration::from_secs(5),
                "{t:?} exceeds the per-command timeout"
            );
        }
    }

    #[test]
    fn a_spent_budget_is_an_error_per_part_not_a_silent_skip() {
        let fake = Fake::new();
        let config = PanicbotConfig {
            budget: Duration::ZERO,
            ..PanicbotConfig::default()
        };
        let run = run_suite_with(&fake, Path::new("."), &config, &opts(None, None));
        assert!(fake.called().is_empty());
        for cmd in [
            Subcommand::Assail,
            Subcommand::Assay,
            Subcommand::Diagnostics,
        ] {
            assert_eq!(status_of(&run, cmd), PartStatus::Error);
            assert!(detail_of(&run, cmd).contains("budget"));
        }
        assert!(!run.analysed());
    }

    #[test]
    fn every_finding_names_a_file() {
        let repo = repo_with_program();
        let fake = Fake::new();
        let run = run_suite_with(
            &fake,
            repo.path(),
            &PanicbotConfig::default(),
            &opts(Some("prog"), None),
        );
        assert!(!run.findings.is_empty());
        for f in &run.findings {
            assert!(
                f.file.is_some(),
                "{} has no file (gitbot-fleet #668)",
                f.rule_id
            );
        }
    }
}
