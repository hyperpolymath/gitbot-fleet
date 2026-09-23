// SPDX-License-Identifier: MPL-2.0

//! A repository's canon conformance, assembled from the three steps before it.
//!
//! Step one reads what a criterion asks for from its own description, step two
//! decides whether the criterion applies to this repository at all, and step
//! three says what was found. This turns those into one report: what was asked,
//! what applies, what was found, and what to do about it.
//!
//! The report is deliberately *quiet about the unapplicable*. A gated criterion
//! a repository does not declare is counted as `na` and left out of the
//! findings, because reporting it would be reporting a requirement that does
//! not exist -- the same mistake as reading `template_ref` as a requirement,
//! one level up.

use std::collections::BTreeMap;

use anyhow::Result;
use serde::Serialize;

use super::Canon;
use super::profile::{GateTable, Profile};
use super::verdict::{Deprecation, GroupVerdict, Severity, Verdict};

/// What a repository would be told, if it asked.
#[derive(Debug, Serialize)]
pub struct CanonReport {
    /// What was checked: `owner/repo`, or a path.
    pub subject: String,
    /// The criteria the canon holds, before any filtering.
    pub criteria: usize,
    /// Criteria scored against this repository: applicable, and asking about files.
    pub scored: usize,
    /// Gated criteria the repository does not declare a capability for. `na`,
    /// and excluded from the denominator rather than counted against it.
    pub not_applicable: usize,
    /// Criteria that apply but ask a content question rather than naming files.
    /// A human answers these, or hypatia's rules do; this report cannot.
    pub not_file_questions: usize,
    /// How the scored criteria came out.
    pub counts: Counts,
    /// Everything not satisfied, worst first. Empty is the good outcome.
    pub findings: Vec<Finding>,
    /// The locations the canon has retired, so a report can explain a
    /// "deprecated location" verdict without the reader having to find it.
    pub retired_locations: Vec<RetiredLocation>,
    /// Capabilities the repository declared, if it has a profile.
    pub declared_capabilities: Vec<String>,
}

#[derive(Debug, Default, Serialize, PartialEq, Eq)]
pub struct Counts {
    pub satisfied: usize,
    pub relocated: usize,
    pub deprecated: usize,
    pub missing: usize,
}

impl Counts {
    fn record(&mut self, severity: Severity) {
        match severity {
            Severity::Satisfied => self.satisfied += 1,
            Severity::Relocated => self.relocated += 1,
            Severity::Deprecated => self.deprecated += 1,
            Severity::Missing => self.missing += 1,
        }
    }
}

#[derive(Debug, Serialize)]
pub struct RetiredLocation {
    pub location: String,
    pub since: String,
    pub stated_by: String,
}

/// One criterion the repository does not satisfy, and why.
#[derive(Debug, Serialize)]
pub struct Finding {
    pub id: String,
    pub name: String,
    pub tier: String,
    pub severity: Severity,
    /// Where the canon's own template satisfies it, when that differs from
    /// what was found. A note, never a fault.
    pub canon_keeps_it_at: Option<String>,
    /// What the description asks for.
    pub expected: Vec<String>,
    /// What the repository has instead, by that name.
    pub found: Vec<String>,
    /// Copies of the same file under a retired location.
    pub deprecated_copies: Vec<String>,
    /// One line per unsatisfied group, ready to print.
    pub details: Vec<String>,
}

/// How bad a finding has to be before the command fails.
///
/// Advisory by default. The canon designates hypatia's `rsr-conformance` as the
/// single normative checker, so a gate here would be a second opinion claiming
/// authority it does not have -- but a repository that *wants* the exit code can
/// ask for one.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FailOn {
    Nothing,
    Relocated,
    Deprecated,
    Missing,
}

impl FailOn {
    /// The severity that fails, if any.
    pub fn threshold(self) -> Option<Severity> {
        match self {
            Self::Nothing => None,
            Self::Relocated => Some(Severity::Relocated),
            Self::Deprecated => Some(Severity::Deprecated),
            Self::Missing => Some(Severity::Missing),
        }
    }

    pub fn parse(value: &str) -> Result<Self> {
        match value {
            "none" | "nothing" => Ok(Self::Nothing),
            "relocated" => Ok(Self::Relocated),
            "deprecated" => Ok(Self::Deprecated),
            "missing" => Ok(Self::Missing),
            other => anyhow::bail!(
                "unknown --fail-on value {other:?}; expected one of none, relocated, deprecated, \
                 missing"
            ),
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Self::Nothing => "none",
            Self::Relocated => "relocated",
            Self::Deprecated => "deprecated",
            Self::Missing => "missing",
        }
    }

    /// Does this report contain a finding at or above the threshold?
    pub fn breached_by(self, report: &CanonReport) -> bool {
        let Some(threshold) = self.threshold() else {
            return false;
        };
        report
            .findings
            .iter()
            .any(|finding| finding.severity >= threshold)
    }
}

impl CanonReport {
    /// Check one repository's file list against the canon.
    ///
    /// `profile_source` is the text of the repository's
    /// `.machine_readable/rsr-profile.a2ml`, or `None` when it has none -- which
    /// means it declares no capabilities, not that it is non-compliant.
    pub fn build(
        subject: &str,
        canon: &Canon,
        gates: &GateTable,
        files: &[String],
        profile_source: Option<&str>,
    ) -> Result<Self> {
        let profile = match profile_source {
            Some(source) => Profile::parse(source, gates)?,
            None => Profile::default(),
        };
        let deprecations = Deprecation::from_canon(canon)?;

        let mut report = Self {
            subject: subject.to_string(),
            criteria: canon.criterion_count(),
            scored: 0,
            not_applicable: 0,
            not_file_questions: 0,
            counts: Counts::default(),
            findings: Vec::new(),
            retired_locations: deprecations
                .iter()
                .map(|deprecation| RetiredLocation {
                    location: deprecation.location.clone(),
                    since: deprecation.since.clone(),
                    stated_by: deprecation.stated_by.clone(),
                })
                .collect(),
            declared_capabilities: profile.declared().cloned().collect(),
        };

        for criterion in canon.criteria() {
            if !profile.is_applicable(criterion) {
                report.not_applicable += 1;
                continue;
            }

            let Some(verdict) = Verdict::of(criterion, files, &deprecations) else {
                report.not_file_questions += 1;
                continue;
            };

            report.scored += 1;
            let severity = verdict.severity();
            report.counts.record(severity);

            if severity.is_finding() {
                report.findings.push(Finding::of(criterion, &verdict));
            }
        }

        // Worst first, and stable, so equal severities stay in canon order --
        // which is category order, and the order a reader expects.
        report
            .findings
            .sort_by_key(|finding| std::cmp::Reverse(finding.severity));

        Ok(report)
    }

    /// The report as a person reads it.
    pub fn render(&self) -> String {
        let mut out = String::new();

        out.push_str(&format!("Canon conformance — {}\n", self.subject));
        for location in &self.retired_locations {
            out.push_str(&format!(
                "  retired: {} (since {}, per criterion {})\n",
                location.location, location.since, location.stated_by
            ));
        }
        out.push('\n');

        out.push_str(&format!(
            "  scored {} of {} criteria ({} na — capability not declared, {} ask content \
             questions, not files)\n",
            self.scored, self.criteria, self.not_applicable, self.not_file_questions
        ));
        out.push_str(&format!(
            "  at path {}   elsewhere {}   deprecated location {}   absent {}\n",
            self.counts.satisfied,
            self.counts.relocated,
            self.counts.deprecated,
            self.counts.missing
        ));

        if !self.declared_capabilities.is_empty() {
            out.push_str(&format!(
                "  declared: {}\n",
                self.declared_capabilities.join(", ")
            ));
        }

        out.push('\n');
        if self.findings.is_empty() {
            out.push_str("  nothing to act on.\n");
            return out;
        }

        for finding in &self.findings {
            out.push_str(&format!(
                "  {:<7} {:<6} {:<19} {}\n",
                finding.id,
                finding.tier,
                finding.severity.as_str(),
                finding.name
            ));
            for detail in &finding.details {
                out.push_str(&format!("            {detail}\n"));
            }
            if let Some(note) = &finding.canon_keeps_it_at {
                out.push_str(&format!("            canon keeps it at {note}\n"));
            }
            if !finding.deprecated_copies.is_empty() {
                out.push_str(&format!(
                    "            stale copy: {}\n",
                    finding.deprecated_copies.join(", ")
                ));
            }
        }

        out
    }

    /// How many findings there are, by severity.
    pub fn findings_by_severity(&self) -> BTreeMap<&'static str, usize> {
        let mut counts = BTreeMap::new();
        for finding in &self.findings {
            *counts.entry(finding.severity.as_str()).or_insert(0) += 1;
        }
        counts
    }
}

impl Finding {
    fn of(criterion: &super::Criterion, verdict: &Verdict) -> Self {
        let mut expected = Vec::new();
        let mut found = Vec::new();
        let mut details = Vec::new();
        let mut canon_keeps_it_at = None;

        for group in &verdict.groups {
            match group {
                GroupVerdict::AtPath { .. } => {}
                GroupVerdict::Deprecated {
                    found: path,
                    location,
                    since,
                } => {
                    found.push(path.clone());
                    details.push(format!(
                        "present, but {path} is under {location}, retired {since}"
                    ));
                }
                GroupVerdict::Elsewhere {
                    expected: alternatives,
                    found: elsewhere,
                    deprecated_copies,
                } => {
                    expected.extend(alternatives.iter().cloned());
                    found.extend(elsewhere.iter().cloned());
                    if let Some(note) =
                        super::verdict::canon_location_note(&verdict.template_ref, &elsewhere[0])
                    {
                        canon_keeps_it_at = Some(note);
                    }
                    let mut line = format!(
                        "present at {} — not {}",
                        elsewhere.join(", "),
                        alternatives.join(" or ")
                    );
                    if !deprecated_copies.is_empty() {
                        line.push_str(&format!(
                            "; also a stale copy at {}",
                            deprecated_copies.join(", ")
                        ));
                    }
                    details.push(line);
                }
                GroupVerdict::Absent {
                    expected: alternatives,
                    near_misses,
                } => {
                    expected.extend(alternatives.iter().cloned());
                    let mut line = format!("absent — expected {}", alternatives.join(" or "));
                    if !near_misses.is_empty() {
                        line.push_str(&format!(
                            "; there is a {} where the canon asks for one ending in {}",
                            near_misses.join(", "),
                            extension_of(&alternatives[0])
                        ));
                    }
                    details.push(line);
                }
            }
        }

        Self {
            id: criterion.id.clone(),
            name: criterion.name.clone(),
            tier: criterion.tier.to_string(),
            severity: verdict.severity(),
            canon_keeps_it_at,
            expected,
            found,
            deprecated_copies: verdict.deprecated_copies().into_iter().cloned().collect(),
            details,
        }
    }
}

/// The extension a criterion asked for, for a line about near misses.
fn extension_of(path: &str) -> String {
    match path.rsplit_once('.') {
        Some((_, extension)) => format!(".{extension}"),
        None => path.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::canon::verdict::Severity;

    fn canon() -> Canon {
        Canon::vendored().expect("the canon parses")
    }

    fn gates() -> GateTable {
        GateTable::vendored().expect("the gate table parses")
    }

    fn files(paths: &[&str]) -> Vec<String> {
        paths.iter().map(|path| path.to_string()).collect()
    }

    /// Everything the canon asks for in one of the named locations.
    const COMPLIANT: &[&str] = &[
        "Justfile",
        ".editorconfig",
        ".pre-commit-config.yaml",
        ".tool-versions",
        "README.adoc",
        "LICENSE",
        "LICENSES/MPL-2.0.txt",
        "SECURITY.md",
        "CODE_OF_CONDUCT.md",
        "CONTRIBUTING.md",
        ".gitignore",
        ".gitattributes",
        ".well-known/security.txt",
        "0-AI-MANIFEST.a2ml",
        ".machine_readable/rsr-profile.a2ml",
        ".machine_readable/descriptiles/STATE.a2ml",
        ".machine_readable/descriptiles/META.a2ml",
        ".machine_readable/descriptiles/ECOSYSTEM.a2ml",
        ".machine_readable/descriptiles/AGENTIC.a2ml",
        ".machine_readable/descriptiles/NEUROSYM.a2ml",
        ".machine_readable/descriptiles/PLAYBOOK.a2ml",
        ".machine_readable/descriptiles/anchors/ANCHOR.a2ml",
    ];

    #[test]
    fn a_compliant_repository_has_nothing_to_act_on() {
        let report =
            CanonReport::build("acme/widgets", &canon(), &gates(), &files(COMPLIANT), None)
                .expect("the report builds");

        assert_eq!(report.findings.len(), 0, "{:#?}", report.findings);
        assert_eq!(report.counts.missing, 0);
        assert_eq!(report.counts.satisfied, report.scored);
        assert!(report.render().contains("nothing to act on."));
    }

    #[test]
    fn the_unapplicable_are_counted_and_not_reported() {
        let report =
            CanonReport::build("acme/widgets", &canon(), &gates(), &files(COMPLIANT), None)
                .expect("the report builds");

        // The vendored canon: 78 criteria, 30 gated on a capability.
        assert_eq!(report.criteria, 78);
        assert_eq!(report.not_applicable, 30);
        assert_eq!(report.scored, 22);
        assert_eq!(report.not_file_questions, 26);
        assert_eq!(
            report.findings.len() + report.counts.satisfied,
            report.scored
        );
        assert!(
            report.findings.iter().all(|finding| ![
                // governance-tier, docs-site and web-ui criteria the pilot's
                // repositories did not declare
                "2.1.7", "2.1.8", "2.1.9", "3.1.9", "9.1.4", "11.1.2"
            ]
            .contains(&finding.id.as_str())),
            "a criterion the repository cannot satisfy from lack of a capability must not be \
             reported at all"
        );
    }

    #[test]
    fn declaring_a_capability_brings_its_criteria_into_scope() {
        let profile = "[rsr-profile]\nversion = \"1.0.0\"\ncapabilities = [\"governance-tier\"]\n";

        let without =
            CanonReport::build("acme/widgets", &canon(), &gates(), &files(COMPLIANT), None)
                .expect("builds");
        let with = CanonReport::build(
            "acme/widgets",
            &canon(),
            &gates(),
            &files(COMPLIANT),
            Some(profile),
        )
        .expect("builds");

        // Ten criteria gate on governance-tier; six of them ask about files
        // and four ask content questions, so declaring the capability moves ten
        // out of `na` and adds six to `scored`.
        assert_eq!(without.not_applicable - with.not_applicable, 10);
        assert_eq!(without.scored + 6, with.scored);
        assert_eq!(without.not_file_questions + 4, with.not_file_questions);
        assert_eq!(
            with.declared_capabilities,
            vec!["governance-tier".to_string()]
        );
        assert!(
            with.findings.iter().any(|finding| finding.id == "2.1.7"),
            "MAINTAINERS.adoc is now asked for, and this repository does not have it"
        );
    }

    #[test]
    fn a_retired_leftover_is_reported_with_the_deprecation_that_retired_it() {
        let report = CanonReport::build(
            "acme/widgets",
            &canon(),
            &gates(),
            &files(&[".machine_readable/6a2/STATE.a2ml"]),
            None,
        )
        .expect("builds");

        let finding = report
            .findings
            .iter()
            .find(|finding| finding.id == "3.1.2")
            .expect("3.1.2 asks for STATE.a2ml");

        assert_eq!(finding.severity, Severity::Deprecated);
        assert!(
            finding.details[0].contains(".machine_readable/6a2/")
                && finding.details[0].contains("2026-06-30"),
            "{:?}",
            finding.details
        );
        assert_eq!(
            report.retired_locations.len(),
            1,
            "the report can explain the word it just used"
        );
        assert!(report.render().contains("retired: .machine_readable/6a2/"));
    }

    #[test]
    fn findings_are_worst_first_and_stable_within_a_severity() {
        let report = CanonReport::build(
            "acme/widgets",
            &canon(),
            &gates(),
            &files(&[".machine_readable/6a2/STATE.a2ml"]),
            None,
        )
        .expect("builds");

        let severities: Vec<Severity> = report.findings.iter().map(|f| f.severity).collect();
        let mut sorted = severities.clone();
        sorted.sort_by_key(|severity| std::cmp::Reverse(*severity));
        assert_eq!(severities, sorted, "findings must read worst first");
    }

    #[test]
    fn a_near_miss_is_described_by_what_the_canon_asked_for() {
        let report = CanonReport::build(
            "acme/widgets",
            &canon(),
            &gates(),
            &files(&["CODE_OF_CONDUCT.adoc"]),
            None,
        )
        .expect("builds");

        let finding = report
            .findings
            .iter()
            .find(|finding| finding.id == "2.1.4")
            .expect("2.1.4 asks for CODE_OF_CONDUCT.md");

        assert_eq!(finding.severity, Severity::Missing);
        assert!(
            finding.details[0].contains("CODE_OF_CONDUCT.adoc")
                && finding.details[0].contains(".md"),
            "{:?}",
            finding.details
        );
    }

    #[test]
    fn fail_on_is_advisory_until_asked_otherwise() {
        let report = CanonReport::build(
            "acme/widgets",
            &canon(),
            &gates(),
            &files(&[".machine_readable/6a2/STATE.a2ml"]),
            None,
        )
        .expect("builds");
        assert!(report.counts.deprecated > 0 && report.counts.missing > 0);

        // The default: report, do not fail.
        assert!(!FailOn::Nothing.breached_by(&report));
        // Opt-in thresholds, in order of how much they tolerate.
        assert!(FailOn::Relocated.breached_by(&report));
        assert!(FailOn::Deprecated.breached_by(&report));
        assert!(FailOn::Missing.breached_by(&report));

        let satisfiable =
            CanonReport::build("acme/widgets", &canon(), &gates(), &files(COMPLIANT), None)
                .expect("builds");
        assert!(!FailOn::Relocated.breached_by(&satisfiable));
        assert!(!FailOn::Missing.breached_by(&satisfiable));
    }

    #[test]
    fn the_severity_of_a_finding_is_the_worst_group_in_it() {
        // 2.1.10 wants .gitignore and .gitattributes. A repository with a stale
        // .gitignore and no .gitattributes at all is missing, not deprecated:
        // the missing group is the thing to fix first.
        let report = CanonReport::build(
            "acme/widgets",
            &canon(),
            &gates(),
            &files(&[".machine_readable/6a2/.gitignore"]),
            None,
        )
        .expect("builds");

        let finding = report
            .findings
            .iter()
            .find(|finding| finding.id == "2.1.10")
            .expect("2.1.10 asks for two files");
        assert_eq!(finding.severity, Severity::Missing);
        assert_eq!(finding.details.len(), 2, "{:?}", finding.details);
    }

    #[test]
    fn the_report_serialises_to_json() {
        let report = CanonReport::build(
            "acme/widgets",
            &canon(),
            &gates(),
            &files(&[".machine_readable/6a2/STATE.a2ml"]),
            None,
        )
        .expect("builds");

        let value = serde_json::to_value(&report).expect("serialises");
        assert_eq!(value["subject"], "acme/widgets");
        assert_eq!(value["criteria"], 78);
        assert!(value["counts"]["deprecated"].as_u64().unwrap() > 0);
        assert!(
            value["findings"]
                .as_array()
                .unwrap()
                .iter()
                .any(|finding| { finding["severity"] == "deprecated" && finding["id"] == "3.1.2" })
        );
        assert_eq!(value["retired_locations"][0]["since"], "2026-06-30");
    }

    #[test]
    fn an_unparseable_profile_is_an_error_not_an_empty_one() {
        // A repository with a broken profile declares nothing, which would
        // silently shrink the report to the universal criteria -- a clean
        // scorecard for a repository whose declaration could not be read.
        let error = CanonReport::build(
            "acme/widgets",
            &canon(),
            &gates(),
            &files(COMPLIANT),
            Some("[rsr-profile]\ncapabilites = [\"rust\"]\n"),
        )
        .expect_err("a misspelt key must not pass as a profile");
        assert!(format!("{error:#}").contains("capabilites"), "{error:#}");
    }

    #[test]
    fn fail_on_rejects_a_value_it_does_not_know() {
        assert_eq!(FailOn::parse("missing").expect("known"), FailOn::Missing);
        assert_eq!(FailOn::parse("none").expect("known"), FailOn::Nothing);
        let error = FailOn::parse("critical").expect_err("not a severity");
        assert!(format!("{error:#}").contains("critical"), "{error:#}");
    }
}
