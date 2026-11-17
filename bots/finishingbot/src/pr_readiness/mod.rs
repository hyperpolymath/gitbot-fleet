// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
//! Pull-request readiness: is this pull request ready for the owner to review
//! and merge?
//!
//! Each unmet condition is one [`Finding`]; a pull request with no finding and
//! every source read is `ready`. The conditions, in rule-id order:
//!
//! | Rule    | Condition                                                         |
//! |---------|-------------------------------------------------------------------|
//! | PRR-001 | unresolved review conversations (count, a link to each thread)    |
//! | PRR-002 | a reviewer whose latest review is CHANGES_REQUESTED               |
//! | PRR-003 | head branch behind its base                                       |
//! | PRR-004 | merge conflict                                                    |
//! | PRR-005 | check runs or commit statuses failing, timed out, cancelled, ...  |
//! | PRR-006 | check runs or commit statuses still pending                       |
//! | PRR-007 | a required status context that nothing on the head provides      |
//! | PRR-008 | the pull request is a draft (informational)                       |
//!
//! Auto-merge is reported in the verdict, not as a finding: it is a setting,
//! not an unmet condition.
//!
//! The report is ADVISORY. Nothing here decides whether a merge may happen:
//! the verdict never changes finishing-bot's exit status, and the verdict grid
//! row it feeds is advisory.
//!
//! This module is pure: [`evaluate`] reads a [`Snapshot`] (the GitHub REST and
//! GraphQL response shapes, only the fields read) and makes no request. The
//! requests live in [`github`], so the logic is tested on recorded responses.
//!
//! A pull-request finding names no file: it is a fact about the change as a
//! whole, and the grid grades a path-less finding as part of every change,
//! which is exactly the scope these findings have.

pub mod github;

use crate::analyzers::{Finding, Severity};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// At most this many entries are listed in one finding's message; the rest
/// are counted.
const LIST_CAP: usize = 25;

/// Check-run conclusions that mean the check did not pass.
const FAILING_CONCLUSIONS: &[&str] = &[
    "failure",
    "timed_out",
    "cancelled",
    "action_required",
    "startup_failure",
];

// ── inputs: GitHub response shapes, only the fields read ─────────────────────

/// `GET /repos/{o}/{r}/pulls/{n}`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PullRequest {
    pub number: u64,
    #[serde(default)]
    pub html_url: String,
    #[serde(default)]
    pub state: String,
    #[serde(default)]
    pub draft: bool,
    /// `None` while GitHub is still computing mergeability.
    #[serde(default)]
    pub mergeable: Option<bool>,
    #[serde(default)]
    pub mergeable_state: Option<String>,
    #[serde(default)]
    pub auto_merge: Option<AutoMerge>,
    #[serde(default)]
    pub head: GitRef,
    #[serde(default)]
    pub base: GitRef,
}

/// The `auto_merge` object of a pull request (present only when enabled).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct AutoMerge {
    #[serde(default)]
    pub merge_method: Option<String>,
}

/// `head` / `base` of a pull request.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GitRef {
    #[serde(default)]
    pub sha: String,
    #[serde(rename = "ref", default)]
    pub ref_name: String,
}

/// `GET /repos/{o}/{r}/compare/{base}...{head}`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Compare {
    #[serde(default)]
    pub behind_by: Option<u64>,
}

/// One element of `GET /repos/{o}/{r}/commits/{sha}/check-runs` `.check_runs`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CheckRun {
    #[serde(default)]
    pub id: u64,
    pub name: String,
    #[serde(default)]
    pub status: String,
    #[serde(default)]
    pub conclusion: Option<String>,
    #[serde(default)]
    pub html_url: Option<String>,
    #[serde(default)]
    pub details_url: Option<String>,
    #[serde(default)]
    pub app: Option<App>,
}

/// The GitHub App that created a check run.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct App {
    #[serde(default)]
    pub id: Option<u64>,
    #[serde(default)]
    pub slug: Option<String>,
}

/// `GET /repos/{o}/{r}/commits/{sha}/status` (the combined status).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CombinedStatus {
    #[serde(default)]
    pub statuses: Vec<CommitStatus>,
}

/// One commit status (the latest per context, as the combined status gives).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CommitStatus {
    pub context: String,
    pub state: String,
    #[serde(default)]
    pub target_url: Option<String>,
}

/// One element of `GET /repos/{o}/{r}/rules/branches/{branch}`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Rule {
    #[serde(rename = "type")]
    pub kind: String,
    #[serde(default)]
    pub parameters: Option<serde_json::Value>,
}

/// `GET /repos/{o}/{r}/branches/{branch}`: classic branch protection.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Branch {
    #[serde(default)]
    pub protection: Option<Protection>,
}

/// `protection` of a branch.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Protection {
    #[serde(default)]
    pub required_status_checks: Option<ProtectionChecks>,
}

/// `protection.required_status_checks` of a branch.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ProtectionChecks {
    #[serde(default)]
    pub enforcement_level: Option<String>,
    #[serde(default)]
    pub contexts: Vec<String>,
    #[serde(default)]
    pub checks: Vec<RequiredCheck>,
}

/// A required check, as both rulesets and classic protection name it.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RequiredCheck {
    pub context: String,
    #[serde(default)]
    pub integration_id: Option<u64>,
}

/// One element of `GET /repos/{o}/{r}/pulls/{n}/reviews`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Review {
    #[serde(default)]
    pub id: u64,
    #[serde(default)]
    pub user: Option<User>,
    pub state: String,
    #[serde(default)]
    pub submitted_at: Option<String>,
    #[serde(default)]
    pub html_url: Option<String>,
}

/// A GitHub user (login only).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct User {
    pub login: String,
}

/// One node of GraphQL `pullRequest.reviewThreads.nodes`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ReviewThread {
    #[serde(rename = "isResolved")]
    pub is_resolved: bool,
    #[serde(rename = "isOutdated", default)]
    pub is_outdated: bool,
    #[serde(default)]
    pub path: Option<String>,
    #[serde(default)]
    pub line: Option<u64>,
    #[serde(default)]
    pub comments: ThreadComments,
}

/// `comments(first: 1)` of a review thread: the first comment's URL is the
/// thread's link.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ThreadComments {
    #[serde(default)]
    pub nodes: Vec<ThreadComment>,
}

/// One comment of a review thread (its URL only).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ThreadComment {
    #[serde(default)]
    pub url: Option<String>,
}

/// Everything one evaluation reads. A source that is `None` could not be read
/// (the fetcher says why in `unreadable`); a pull request with an unread
/// source is never `ready`, because the missing source could hold the reason
/// it is not.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Snapshot {
    pub pull: PullRequest,
    #[serde(default)]
    pub compare: Option<Compare>,
    #[serde(default)]
    pub check_runs: Option<Vec<CheckRun>>,
    #[serde(default)]
    pub status: Option<CombinedStatus>,
    #[serde(default)]
    pub rules: Option<Vec<Rule>>,
    #[serde(default)]
    pub branch: Option<Branch>,
    #[serde(default)]
    pub reviews: Option<Vec<Review>>,
    #[serde(default)]
    pub review_threads: Option<Vec<ReviewThread>>,
    /// "source: why" for every source that could not be read.
    #[serde(default)]
    pub unreadable: Vec<String>,
}

/// Evaluation options.
#[derive(Debug, Clone, Default)]
pub struct EvalOptions {
    /// The GitHub Actions run doing this evaluation. Its own check runs are
    /// necessarily still in progress, so they are not counted as pending (nor
    /// as failing); they still count as PROVIDING a required context.
    pub exclude_run_id: Option<u64>,
}

// ── output ───────────────────────────────────────────────────────────────────

/// The verdict on one pull request.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Verdict {
    /// Every source read, nothing unmet.
    Ready,
    /// At least one condition unmet (see the findings).
    NotReady,
    /// Nothing unmet in what was read, but a source could not be read.
    Unknown,
    /// The evaluation did not run (see `skipped`).
    #[default]
    Skipped,
}

impl Verdict {
    fn words(self) -> &'static str {
        match self {
            Verdict::Ready => "ready",
            Verdict::NotReady => "not ready",
            Verdict::Unknown => "readiness unknown",
            Verdict::Skipped => "readiness not evaluated",
        }
    }
}

/// The readiness report. Serialised beside the audit analyzers, so it carries
/// the analyzer shape (`findings`, `files_checked`, `duration_ms`) the fleet
/// report-back driver reads, plus the verdict.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PrReadiness {
    pub findings: Vec<Finding>,
    /// Always 0: readiness reads no file. Kept for the analyzer shape.
    pub files_checked: usize,
    pub duration_ms: u64,
    pub verdict: Verdict,
    /// One line per unmet condition, in rule order.
    pub reasons: Vec<String>,
    pub pr: Option<u64>,
    pub url: Option<String>,
    pub head_sha: Option<String>,
    pub base: Option<String>,
    pub draft: Option<bool>,
    pub auto_merge: Option<bool>,
    pub auto_merge_method: Option<String>,
    /// Required status contexts on the base branch (rulesets and classic
    /// protection together).
    pub required_contexts: Vec<String>,
    /// "source: why" for every source that could not be read.
    pub unreadable: Vec<String>,
    /// Why the evaluation did not run at all.
    pub skipped: Option<String>,
    /// One line: the verdict, its reasons, draft and auto-merge.
    pub summary: String,
}

impl PrReadiness {
    /// A report for an evaluation that could not run.
    pub fn skipped(pr: Option<u64>, why: &str) -> Self {
        let summary = match pr {
            Some(n) => format!("PR #{n}: readiness not evaluated: {why}"),
            None => format!("readiness not evaluated: {why}"),
        };
        Self {
            pr,
            skipped: Some(why.to_string()),
            summary,
            ..Self::default()
        }
    }
}

// ── policy read from the base branch ─────────────────────────────────────────

/// What the base branch's rules require, from rulesets and classic protection.
#[derive(Debug, Default)]
struct Policy {
    /// context -> integration ids that may provide it (empty: any provider).
    contexts: BTreeMap<String, Vec<u64>>,
    /// The head must be up to date with the base.
    strict: bool,
    /// Every conversation must be resolved before merging.
    thread_resolution: bool,
    /// Approving reviews required (the most any rule asks for).
    approvals: u64,
}

impl Policy {
    fn read(s: &Snapshot) -> Self {
        let mut p = Policy::default();
        let add = |ctx: &str, id: Option<u64>, p: &mut Policy| {
            let ids = p.contexts.entry(ctx.to_string()).or_default();
            if let Some(i) = id {
                ids.push(i);
            }
        };
        for rule in s.rules.iter().flatten() {
            let Some(params) = rule.parameters.as_ref() else {
                continue;
            };
            match rule.kind.as_str() {
                "required_status_checks" => {
                    if params
                        .get("strict_required_status_checks_policy")
                        .and_then(|v| v.as_bool())
                        == Some(true)
                    {
                        p.strict = true;
                    }
                    let checks: Vec<RequiredCheck> = params
                        .get("required_status_checks")
                        .cloned()
                        .and_then(|v| serde_json::from_value(v).ok())
                        .unwrap_or_default();
                    for c in checks {
                        add(&c.context, c.integration_id, &mut p);
                    }
                }
                "pull_request" => {
                    if params
                        .get("required_review_thread_resolution")
                        .and_then(|v| v.as_bool())
                        == Some(true)
                    {
                        p.thread_resolution = true;
                    }
                    if let Some(n) = params
                        .get("required_approving_review_count")
                        .and_then(|v| v.as_u64())
                    {
                        p.approvals = p.approvals.max(n);
                    }
                }
                _ => {}
            }
        }
        let classic = s
            .branch
            .as_ref()
            .and_then(|b| b.protection.as_ref())
            .and_then(|pr| pr.required_status_checks.as_ref());
        if let Some(rsc) = classic {
            if rsc.enforcement_level.as_deref() != Some("off") {
                for c in &rsc.contexts {
                    add(c, None, &mut p);
                }
                for c in &rsc.checks {
                    add(&c.context, c.integration_id, &mut p);
                }
            }
        }
        // A context whose rules name providing apps may be provided by any of
        // those apps; one no rule binds to an app may be provided by anyone.
        p
    }
}

// ── evaluation ───────────────────────────────────────────────────────────────

fn list(items: &[String]) -> String {
    if items.len() <= LIST_CAP {
        items.join("; ")
    } else {
        format!(
            "{}; and {} more",
            items[..LIST_CAP].join("; "),
            items.len() - LIST_CAP
        )
    }
}

fn short(sha: &str) -> &str {
    sha.get(..7).unwrap_or(sha)
}

/// The run id in an Actions check-run URL (`.../actions/runs/<id>/job/...`).
fn run_id_of(url: &str) -> Option<u64> {
    let rest = url.split("/actions/runs/").nth(1)?;
    rest.split(|c: char| !c.is_ascii_digit())
        .next()?
        .parse()
        .ok()
}

fn is_own_run(cr: &CheckRun, opts: &EvalOptions) -> bool {
    let Some(own) = opts.exclude_run_id else {
        return false;
    };
    [cr.details_url.as_deref(), cr.html_url.as_deref()]
        .into_iter()
        .flatten()
        .any(|u| run_id_of(u) == Some(own))
}

/// The latest check run per (app, name): a re-run supersedes the attempt it
/// replaces, as on the pull request's merge box.
fn latest_check_runs(runs: &[CheckRun]) -> Vec<&CheckRun> {
    let mut latest: BTreeMap<(String, String), &CheckRun> = BTreeMap::new();
    for cr in runs {
        let app = cr
            .app
            .as_ref()
            .and_then(|a| a.slug.clone())
            .unwrap_or_default();
        let key = (app, cr.name.clone());
        match latest.get(&key) {
            Some(prev) if prev.id >= cr.id => {}
            _ => {
                latest.insert(key, cr);
            }
        }
    }
    latest.into_values().collect()
}

fn link_of(cr: &CheckRun) -> String {
    match cr.html_url.as_deref().or(cr.details_url.as_deref()) {
        Some(u) => format!("{} <{}>", cr.name, u),
        None => cr.name.clone(),
    }
}

fn finding(id: &str, name: &str, severity: Severity, message: String, fix: &str) -> Finding {
    Finding::new(id, name, severity, &message).with_suggestion(fix)
}

/// Evaluate one pull request. Pure: reads only `s`.
pub fn evaluate(s: &Snapshot, opts: &EvalOptions) -> PrReadiness {
    let pr = &s.pull;
    let policy = Policy::read(s);
    let mut findings = Vec::new();

    // PRR-001: unresolved review conversations.
    if let Some(threads) = &s.review_threads {
        let open: Vec<&ReviewThread> = threads.iter().filter(|t| !t.is_resolved).collect();
        if !open.is_empty() {
            let outdated = open.iter().filter(|t| t.is_outdated).count();
            let links: Vec<String> = open
                .iter()
                .map(|t| {
                    let at = match (&t.path, t.line) {
                        (Some(p), Some(l)) => format!("{p}:{l}"),
                        (Some(p), None) => p.clone(),
                        _ => "conversation".to_string(),
                    };
                    match t.comments.nodes.first().and_then(|c| c.url.as_deref()) {
                        Some(u) => format!("{at} <{u}>"),
                        None => at,
                    }
                })
                .collect();
            let required = if policy.thread_resolution {
                "; the base branch requires every conversation resolved before merging"
            } else {
                ""
            };
            let outdated_note = if outdated > 0 {
                format!(" ({outdated} on outdated code)")
            } else {
                String::new()
            };
            findings.push(finding(
                "PRR-001",
                "Unresolved review conversations",
                if policy.thread_resolution {
                    Severity::Error
                } else {
                    Severity::Warning
                },
                format!(
                    "{} unresolved review conversation(s){outdated_note}{required}: {}",
                    open.len(),
                    list(&links)
                ),
                "Answer or resolve each conversation.",
            ));
        }
    }

    // PRR-002: a reviewer whose latest review requests changes. COMMENTED and
    // PENDING reviews do not change a reviewer's standing; DISMISSED clears it.
    if let Some(reviews) = &s.reviews {
        let mut ordered: Vec<&Review> = reviews
            .iter()
            .filter(|r| r.state != "COMMENTED" && r.state != "PENDING")
            .collect();
        ordered.sort_by(|a, b| {
            (a.submitted_at.as_deref().unwrap_or(""), a.id)
                .cmp(&(b.submitted_at.as_deref().unwrap_or(""), b.id))
        });
        let mut standing: BTreeMap<&str, &Review> = BTreeMap::new();
        for r in ordered {
            if let Some(u) = &r.user {
                standing.insert(u.login.as_str(), r);
            }
        }
        let blocking: Vec<String> = standing
            .iter()
            .filter(|(_, r)| r.state == "CHANGES_REQUESTED")
            .map(|(login, r)| match &r.html_url {
                Some(u) => format!("{login} <{u}>"),
                None => (*login).to_string(),
            })
            .collect();
        if !blocking.is_empty() {
            findings.push(finding(
                "PRR-002",
                "Changes requested",
                if policy.approvals > 0 {
                    Severity::Error
                } else {
                    Severity::Warning
                },
                format!(
                    "{} reviewer(s) whose latest review requests changes: {}",
                    blocking.len(),
                    list(&blocking)
                ),
                "Address the requested changes and ask for a re-review, or have the review dismissed.",
            ));
        }
    }

    // PRR-003: behind the base branch.
    if let Some(behind) = s.compare.as_ref().and_then(|c| c.behind_by) {
        if behind > 0 {
            let strict = if policy.strict {
                "; the base branch requires the head to be up to date before merging"
            } else {
                ""
            };
            findings.push(finding(
                "PRR-003",
                "Behind base branch",
                if policy.strict {
                    Severity::Error
                } else {
                    Severity::Warning
                },
                format!(
                    "head {} is {behind} commit(s) behind {}{strict}",
                    short(&pr.head.sha),
                    pr.base.ref_name
                ),
                "Update the branch from its base (merge or rebase), then let the checks run again.",
            ));
        }
    }

    // PRR-004: merge conflict.
    if pr.mergeable == Some(false) || pr.mergeable_state.as_deref() == Some("dirty") {
        findings.push(finding(
            "PRR-004",
            "Merge conflict",
            Severity::Error,
            format!(
                "head {} does not merge cleanly into {}",
                short(&pr.head.sha),
                pr.base.ref_name
            ),
            "Resolve the conflicts against the base branch.",
        ));
    }

    // PRR-005 / PRR-006: checks failing, and checks still pending.
    let mut failing = Vec::new();
    let mut pending = Vec::new();
    if let Some(runs) = &s.check_runs {
        for cr in latest_check_runs(runs) {
            if is_own_run(cr, opts) {
                continue;
            }
            let req = if policy.contexts.contains_key(&cr.name) {
                " (required)"
            } else {
                ""
            };
            if cr.status != "completed" {
                pending.push(format!("{}{req} [{}]", link_of(cr), cr.status));
            } else if let Some(c) = cr.conclusion.as_deref() {
                if FAILING_CONCLUSIONS.contains(&c) {
                    failing.push(format!("{}{req} [{c}]", link_of(cr)));
                }
            }
        }
    }
    if let Some(st) = &s.status {
        for cs in &st.statuses {
            let req = if policy.contexts.contains_key(&cs.context) {
                " (required)"
            } else {
                ""
            };
            let name = match &cs.target_url {
                Some(u) if !u.is_empty() => format!("{} <{u}>", cs.context),
                _ => cs.context.clone(),
            };
            match cs.state.as_str() {
                "failure" | "error" => failing.push(format!("{name}{req} [{}]", cs.state)),
                "pending" => pending.push(format!("{name}{req} [pending]")),
                _ => {}
            }
        }
    }
    if !failing.is_empty() {
        findings.push(finding(
            "PRR-005",
            "Checks failing",
            Severity::Error,
            format!(
                "{} check(s) on head {} did not pass: {}",
                failing.len(),
                short(&pr.head.sha),
                list(&failing)
            ),
            "Fix the cause and re-run; a cancelled or timed-out check needs a re-run to count.",
        ));
    }
    if !pending.is_empty() {
        findings.push(finding(
            "PRR-006",
            "Checks pending",
            Severity::Info,
            format!(
                "{} check(s) on head {} had not finished when readiness was read: {}",
                pending.len(),
                short(&pr.head.sha),
                list(&pending)
            ),
            "Wait for the checks to finish; a check queued for a long time may need a runner or a re-run.",
        ));
    }

    // PRR-007: a required context that nothing on the head provides. A check
    // run provides it when its name matches (and, if the rule names providing
    // apps, its app is one of them); a commit status provides it by context.
    // Presence is enough here: a provided-but-failing context is PRR-005.
    if s.check_runs.is_some() || s.status.is_some() {
        let runs = s.check_runs.as_deref().unwrap_or(&[]);
        let statuses = s.status.as_ref().map(|c| &c.statuses[..]).unwrap_or(&[]);
        let missing: Vec<String> = policy
            .contexts
            .iter()
            .filter(|(ctx, ids)| {
                let by_run = runs.iter().any(|cr| {
                    &cr.name == *ctx
                        && (ids.is_empty()
                            || cr
                                .app
                                .as_ref()
                                .and_then(|a| a.id)
                                .is_none_or(|id| ids.contains(&id)))
                });
                let by_status = statuses.iter().any(|cs| &cs.context == *ctx);
                !(by_run || by_status)
            })
            .map(|(ctx, _)| format!("`{ctx}`"))
            .collect();
        // A job waiting on `needs:` (or a called workflow's jobs) has no check
        // run until it starts, so while anything on the head is unfinished a
        // missing context may still appear: say "not yet", at warning. Only
        // once every check has finished is the absence a settled fact.
        let unfinished = runs.iter().any(|cr| cr.status != "completed")
            || statuses.iter().any(|cs| cs.state == "pending");
        if !missing.is_empty() && unfinished {
            findings.push(finding(
                "PRR-007",
                "Required status context not provided yet",
                Severity::Warning,
                format!(
                    "{} required status context(s) on {} have no check run or commit status on head {} yet: {}. \
                     Checks were still running, so a job that has not started can still produce them",
                    missing.len(),
                    pr.base.ref_name,
                    short(&pr.head.sha),
                    list(&missing)
                ),
                "Re-read once the checks finish; if a context is still missing then, nothing on this pull request produces it.",
            ));
        } else if !missing.is_empty() {
            findings.push(finding(
                "PRR-007",
                "Required status context not provided",
                Severity::Error,
                format!(
                    "{} required status context(s) on {} have no check run or commit status on head {}: {}. \
                     Every check has finished, so nothing on the pull request produces them, and it cannot merge whatever reviewers approve",
                    missing.len(),
                    pr.base.ref_name,
                    short(&pr.head.sha),
                    list(&missing)
                ),
                "Make a workflow on this pull request produce the exact context name, or have the owner correct the rule that requires it.",
            ));
        }
    }

    // PRR-008: draft (informational, but a draft cannot be merged).
    if pr.draft {
        findings.push(finding(
            "PRR-008",
            "Draft",
            Severity::Info,
            format!("pull request #{} is a draft", pr.number),
            "Mark it ready for review when the work is done.",
        ));
    }

    let reasons: Vec<String> = findings
        .iter()
        .map(|f| match f.id.as_str() {
            "PRR-007" => format!("{}: {}", f.name, f.message.split(". ").next().unwrap_or("")),
            _ => f.name.to_lowercase(),
        })
        .collect();
    let verdict = if !findings.is_empty() {
        Verdict::NotReady
    } else if !s.unreadable.is_empty() {
        Verdict::Unknown
    } else {
        Verdict::Ready
    };
    let mut summary = format!("PR #{} {}", pr.number, verdict.words());
    if !findings.is_empty() {
        let short_reasons: Vec<String> = findings.iter().map(reason_word).collect();
        summary.push_str(&format!(": {}", short_reasons.join("; ")));
    }
    if !s.unreadable.is_empty() {
        summary.push_str(&format!("; could not read: {}", s.unreadable.join("; ")));
    }
    let auto = match &pr.auto_merge {
        Some(a) => format!(
            "on ({})",
            a.merge_method.as_deref().unwrap_or("method unknown")
        ),
        None => "off".to_string(),
    };
    summary.push_str(&format!(
        "; draft: {}; auto-merge: {auto}",
        if pr.draft { "yes" } else { "no" }
    ));

    PrReadiness {
        findings,
        files_checked: 0,
        duration_ms: 0,
        verdict,
        reasons,
        pr: Some(pr.number),
        url: Some(pr.html_url.clone()).filter(|u| !u.is_empty()),
        head_sha: Some(pr.head.sha.clone()).filter(|u| !u.is_empty()),
        base: Some(pr.base.ref_name.clone()).filter(|u| !u.is_empty()),
        draft: Some(pr.draft),
        auto_merge: Some(pr.auto_merge.is_some()),
        auto_merge_method: pr.auto_merge.as_ref().and_then(|a| a.merge_method.clone()),
        required_contexts: policy.contexts.keys().cloned().collect(),
        unreadable: s.unreadable.clone(),
        skipped: None,
        summary,
    }
}

/// A short phrase for one finding, for the one-line summary.
fn reason_word(f: &Finding) -> String {
    let n = f
        .message
        .split(|c: char| !c.is_ascii_digit())
        .next()
        .unwrap_or("");
    match f.id.as_str() {
        "PRR-001" => format!("{n} unresolved conversation(s)"),
        "PRR-002" => format!("changes requested by {n} reviewer(s)"),
        "PRR-003" => "behind base".to_string(),
        "PRR-004" => "merge conflict".to_string(),
        "PRR-005" => format!("{n} check(s) failing"),
        "PRR-006" => format!("{n} check(s) pending"),
        "PRR-007" => {
            let names: Vec<&str> = f.message.split('`').skip(1).step_by(2).collect();
            format!("required context(s) not provided: {}", names.join(", "))
        }
        "PRR-008" => "draft".to_string(),
        _ => f.name.to_lowercase(),
    }
}

#[cfg(test)]
mod tests {
    //! Fixtures in `tests/fixtures/pr_readiness/` are GitHub responses recorded
    //! on 2026-11-17 and trimmed to the fields this module reads; each file
    //! says where it came from in its `_source` key. Expected results come
    //! from the hub's prototype run of the same day
    //! (pr-readiness-2026-11-17.txt).
    use super::*;

    fn load(name: &str) -> Snapshot {
        let path = format!(
            "{}/tests/fixtures/pr_readiness/{name}",
            env!("CARGO_MANIFEST_DIR")
        );
        let text = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{path}: {e}"));
        serde_json::from_str(&text).unwrap_or_else(|e| panic!("{path}: {e}"))
    }

    fn ids(r: &PrReadiness) -> Vec<&str> {
        r.findings.iter().map(|f| f.id.as_str()).collect()
    }

    #[test]
    fn rsr_template_435_is_missing_ci_gate_and_nothing_else() {
        let r = evaluate(&load("rsr-template-repo-435.json"), &EvalOptions::default());
        assert_eq!(ids(&r), vec!["PRR-007"]);
        assert_eq!(r.verdict, Verdict::NotReady);
        assert_eq!(r.required_contexts, vec!["CI/gate".to_string()]);
        let f = &r.findings[0];
        assert_eq!(f.severity, Severity::Error);
        assert!(f.message.contains("`CI/gate`"), "{}", f.message);
        assert!(f.file.is_none());
        assert_eq!(r.auto_merge, Some(true));
        assert_eq!(r.auto_merge_method.as_deref(), Some("squash"));
        assert!(
            r.summary
                .contains("required context(s) not provided: CI/gate"),
            "{}",
            r.summary
        );
        assert!(
            r.summary.contains("auto-merge: on (squash)"),
            "{}",
            r.summary
        );
    }

    #[test]
    fn a_clean_pull_request_yields_no_finding_and_is_ready() {
        // Negative control: #435 with the one thing it lacks supplied.
        let mut s = load("rsr-template-repo-435.json");
        s.check_runs.as_mut().unwrap().push(CheckRun {
            id: 1,
            name: "CI/gate".into(),
            status: "completed".into(),
            conclusion: Some("success".into()),
            ..CheckRun::default()
        });
        let r = evaluate(&s, &EvalOptions::default());
        assert!(r.findings.is_empty(), "{:?}", ids(&r));
        assert_eq!(r.verdict, Verdict::Ready);
        assert!(r.reasons.is_empty());
    }

    #[test]
    fn a_missing_context_while_checks_run_is_not_yet_settled() {
        // gitbot-fleet#693 on 2026-11-17: `CI/gate` (a `needs:` job) had no
        // check run while 47 checks were still queued. That is "not yet",
        // a warning, not the settled error #435 gets once all checks finished.
        let mut s = load("rsr-template-repo-435.json");
        s.check_runs.as_mut().unwrap().push(CheckRun {
            id: 3,
            name: "lint".into(),
            status: "queued".into(),
            ..CheckRun::default()
        });
        let r = evaluate(&s, &EvalOptions::default());
        let f = r.findings.iter().find(|f| f.id == "PRR-007").unwrap();
        assert_eq!(f.severity, Severity::Warning);
        assert!(f.message.contains("yet: `CI/gate`"), "{}", f.message);
        assert_eq!(r.verdict, Verdict::NotReady);
        // Settled case for contrast: every check finished.
        let settled = evaluate(&load("rsr-template-repo-435.json"), &EvalOptions::default());
        assert_eq!(settled.findings[0].severity, Severity::Error);
        assert!(settled.findings[0]
            .message
            .contains("Every check has finished"));
    }

    #[test]
    fn a_commit_status_also_provides_a_required_context() {
        let mut s = load("rsr-template-repo-435.json");
        s.status.as_mut().unwrap().statuses.push(CommitStatus {
            context: "CI/gate".into(),
            state: "success".into(),
            target_url: None,
        });
        assert!(evaluate(&s, &EvalOptions::default()).findings.is_empty());
    }

    #[test]
    fn a_check_run_from_another_app_does_not_provide_an_app_bound_context() {
        let mut s = load("rsr-template-repo-435.json");
        s.rules = Some(vec![Rule {
            kind: "required_status_checks".into(),
            parameters: Some(serde_json::json!({
                "required_status_checks": [{"context": "app-bound / check", "integration_id": 15368}]
            })),
        }]);
        s.check_runs.as_mut().unwrap().push(CheckRun {
            id: 2,
            name: "app-bound / check".into(),
            status: "completed".into(),
            conclusion: Some("success".into()),
            app: Some(App {
                id: Some(1),
                slug: Some("someone-else".into()),
            }),
            ..CheckRun::default()
        });
        assert_eq!(ids(&evaluate(&s, &EvalOptions::default())), vec!["PRR-007"]);
        s.check_runs.as_mut().unwrap().last_mut().unwrap().app = Some(App {
            id: Some(15368),
            slug: Some("github-actions".into()),
        });
        assert!(evaluate(&s, &EvalOptions::default()).findings.is_empty());
    }

    #[test]
    fn standards_1619_has_twenty_unresolved_conversations_in_one_finding() {
        let r = evaluate(&load("standards-1619.json"), &EvalOptions::default());
        let f: Vec<&Finding> = r.findings.iter().filter(|f| f.id == "PRR-001").collect();
        assert_eq!(f.len(), 1);
        assert!(
            f[0].message
                .starts_with("20 unresolved review conversation(s)"),
            "{}",
            f[0].message
        );
        assert_eq!(f[0].message.matches("<https://github.com/").count(), 20);
        // standards' base requires conversation resolution, so this blocks.
        assert_eq!(f[0].severity, Severity::Error);
        // #1619 was also 14 commits behind on 2026-11-17, auto-merge on.
        assert!(ids(&r).contains(&"PRR-003"));
        assert_eq!(r.auto_merge, Some(true));
        assert_eq!(r.verdict, Verdict::NotReady);
        assert!(
            r.summary.contains("20 unresolved conversation(s)"),
            "{}",
            r.summary
        );
    }

    #[test]
    fn exactly_one_unresolved_thread_yields_exactly_one_finding() {
        let mut s = load("rsr-template-repo-435.json");
        s.check_runs.as_mut().unwrap().push(CheckRun {
            id: 1,
            name: "CI/gate".into(),
            status: "completed".into(),
            conclusion: Some("success".into()),
            ..CheckRun::default()
        });
        let thread = |resolved: bool| ReviewThread {
            is_resolved: resolved,
            path: Some("README.adoc".into()),
            line: Some(3),
            comments: ThreadComments {
                nodes: vec![ThreadComment {
                    url: Some("https://github.com/o/r/pull/1#discussion_r1".into()),
                }],
            },
            ..ReviewThread::default()
        };
        s.review_threads = Some(vec![thread(true), thread(false), thread(true)]);
        let r = evaluate(&s, &EvalOptions::default());
        assert_eq!(ids(&r), vec!["PRR-001"]);
        assert!(r.findings[0].message.starts_with("1 unresolved"));
        assert!(r.findings[0]
            .message
            .contains("README.adoc:3 <https://github.com/o/r/pull/1#discussion_r1>"));
        // rsr-template-repo requires conversation resolution too.
        assert_eq!(r.findings[0].severity, Severity::Error);
    }

    #[test]
    fn latest_review_decides_changes_requested() {
        let mut s = load("rsr-template-repo-435.json");
        s.check_runs = Some(vec![]);
        s.rules = Some(vec![]);
        let rv = |id, who: &str, state: &str, at: &str| Review {
            id,
            user: Some(User { login: who.into() }),
            state: state.into(),
            submitted_at: Some(at.into()),
            html_url: None,
        };
        // alice requested changes then approved; bob requested changes then
        // only commented; carol's request was dismissed.
        s.reviews = Some(vec![
            rv(1, "alice", "CHANGES_REQUESTED", "2026-11-01T00:00:00Z"),
            rv(2, "alice", "APPROVED", "2026-11-02T00:00:00Z"),
            rv(3, "bob", "CHANGES_REQUESTED", "2026-11-01T00:00:00Z"),
            rv(4, "bob", "COMMENTED", "2026-11-03T00:00:00Z"),
            rv(5, "carol", "CHANGES_REQUESTED", "2026-11-01T00:00:00Z"),
            rv(6, "carol", "DISMISSED", "2026-11-04T00:00:00Z"),
        ]);
        let r = evaluate(&s, &EvalOptions::default());
        assert_eq!(ids(&r), vec!["PRR-002"]);
        assert!(r.findings[0].message.starts_with("1 reviewer(s)"));
        assert!(r.findings[0].message.contains("bob"));
        assert!(!r.findings[0].message.contains("alice"));
        // No approvals are required on that base, so it is advisory.
        assert_eq!(r.findings[0].severity, Severity::Warning);
    }

    #[test]
    fn failing_and_pending_checks_are_separate_and_reruns_supersede() {
        let mut s = load("rsr-template-repo-435.json");
        s.rules = Some(vec![]);
        let cr = |id, name: &str, status: &str, concl: Option<&str>, url: &str| CheckRun {
            id,
            name: name.into(),
            status: status.into(),
            conclusion: concl.map(Into::into),
            details_url: Some(url.into()),
            ..CheckRun::default()
        };
        s.check_runs = Some(vec![
            cr(
                10,
                "lint",
                "completed",
                Some("cancelled"),
                "https://github.com/o/r/actions/runs/7/job/1",
            ),
            cr(
                11,
                "lint",
                "completed",
                Some("success"),
                "https://github.com/o/r/actions/runs/8/job/2",
            ),
            cr(
                12,
                "test",
                "completed",
                Some("timed_out"),
                "https://github.com/o/r/actions/runs/8/job/3",
            ),
            cr(
                13,
                "build",
                "queued",
                None,
                "https://github.com/o/r/actions/runs/8/job/4",
            ),
            cr(
                14,
                "Fleet report-backs",
                "in_progress",
                None,
                "https://github.com/o/r/actions/runs/99/job/5",
            ),
        ]);
        s.status = Some(CombinedStatus {
            statuses: vec![CommitStatus {
                context: "ci/legacy".into(),
                state: "error".into(),
                target_url: None,
            }],
        });
        let r = evaluate(
            &s,
            &EvalOptions {
                exclude_run_id: Some(99),
            },
        );
        assert_eq!(ids(&r), vec!["PRR-005", "PRR-006"]);
        assert!(
            r.findings[0].message.starts_with("2 check(s)"),
            "{}",
            r.findings[0].message
        );
        assert!(r.findings[0].message.contains("test"));
        assert!(r.findings[0].message.contains("ci/legacy [error]"));
        assert!(
            !r.findings[0].message.contains("lint"),
            "the re-run superseded the cancelled lint"
        );
        assert!(
            r.findings[1].message.starts_with("1 check(s)"),
            "own run 99 is excluded"
        );
        assert_eq!(r.findings[1].severity, Severity::Info);
        // Without the exclusion, the evaluating run itself reads as pending.
        let r = evaluate(&s, &EvalOptions::default());
        let pending = r.findings.iter().find(|f| f.id == "PRR-006").unwrap();
        assert!(pending.message.starts_with("2 check(s)"));
    }

    #[test]
    fn behind_conflict_and_draft() {
        let mut s = load("rsr-template-repo-435.json");
        s.check_runs = Some(vec![]);
        s.rules = Some(vec![]);
        s.compare = Some(Compare { behind_by: Some(3) });
        s.pull.mergeable = Some(false);
        s.pull.mergeable_state = Some("dirty".into());
        s.pull.draft = true;
        s.pull.auto_merge = None;
        let r = evaluate(&s, &EvalOptions::default());
        assert_eq!(ids(&r), vec!["PRR-003", "PRR-004", "PRR-008"]);
        // No strict up-to-date rule now, so behind is a warning.
        assert_eq!(r.findings[0].severity, Severity::Warning);
        assert_eq!(r.findings[2].severity, Severity::Info);
        assert!(
            r.summary.ends_with("draft: yes; auto-merge: off"),
            "{}",
            r.summary
        );
    }

    #[test]
    fn mergeability_still_being_computed_is_not_a_conflict() {
        let mut s = load("rsr-template-repo-435.json");
        s.pull.mergeable = None;
        s.pull.mergeable_state = Some("unknown".into());
        assert!(!ids(&evaluate(&s, &EvalOptions::default())).contains(&"PRR-004"));
    }

    #[test]
    fn an_unread_source_makes_a_clean_result_unknown_not_ready() {
        let mut s = load("rsr-template-repo-435.json");
        s.check_runs.as_mut().unwrap().push(CheckRun {
            id: 1,
            name: "CI/gate".into(),
            status: "completed".into(),
            conclusion: Some("success".into()),
            ..CheckRun::default()
        });
        s.review_threads = None;
        s.unreadable = vec!["review threads: GraphQL needs a token".into()];
        let r = evaluate(&s, &EvalOptions::default());
        assert!(r.findings.is_empty());
        assert_eq!(r.verdict, Verdict::Unknown);
        assert!(
            r.summary.contains("could not read: review threads"),
            "{}",
            r.summary
        );
    }

    #[test]
    fn classic_protection_contexts_count_unless_enforcement_is_off() {
        let mut s = load("rsr-template-repo-435.json");
        s.rules = Some(vec![]);
        s.branch = Some(Branch {
            protection: Some(Protection {
                required_status_checks: Some(ProtectionChecks {
                    enforcement_level: Some("non_admins".into()),
                    contexts: vec!["legacy/ci".into()],
                    checks: vec![],
                }),
            }),
        });
        assert_eq!(ids(&evaluate(&s, &EvalOptions::default())), vec!["PRR-007"]);
        s.branch
            .as_mut()
            .unwrap()
            .protection
            .as_mut()
            .unwrap()
            .required_status_checks
            .as_mut()
            .unwrap()
            .enforcement_level = Some("off".into());
        assert!(evaluate(&s, &EvalOptions::default()).findings.is_empty());
    }

    #[test]
    fn long_lists_are_capped_and_counted() {
        let items: Vec<String> = (0..30).map(|i| i.to_string()).collect();
        assert!(list(&items).ends_with("; and 5 more"));
        assert_eq!(list(&items[..2]), "0; 1");
    }

    #[test]
    fn run_id_is_read_from_actions_urls_only() {
        assert_eq!(
            run_id_of("https://github.com/o/r/actions/runs/44124672727/job/1"),
            Some(44124672727)
        );
        assert_eq!(run_id_of("https://sonarcloud.io/dashboard?id=x"), None);
    }

    #[test]
    fn skipped_report_says_why() {
        let r = PrReadiness::skipped(Some(5), "no token");
        assert_eq!(r.verdict, Verdict::Skipped);
        assert_eq!(r.summary, "PR #5: readiness not evaluated: no token");
        assert!(r.findings.is_empty());
    }
}
