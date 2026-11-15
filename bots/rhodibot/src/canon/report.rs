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

use anyhow::{Context, Result};
use serde::Serialize;

use super::Canon;
use super::overlay::{Overlay, find_chora_deed};
use super::profile::{GateTable, Profile};
use super::verdict::{Deprecation, GroupVerdict, Severity, Verdict};
use crate::deed::{Deed, DocFormat};

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
    /// Checks rhodibot adds to the canon's (the chora deed), counted in `scored`.
    pub extra_checks: usize,
}

#[derive(Debug, Default, Serialize, PartialEq, Eq)]
pub struct Counts {
    pub satisfied: usize,
    pub relocated: usize,
    pub deprecated: usize,
    pub missing: usize,
    pub manual: usize,
}

impl Counts {
    fn record(&mut self, severity: Severity) {
        match severity {
            Severity::Satisfied => self.satisfied += 1,
            Severity::Relocated => self.relocated += 1,
            Severity::Deprecated => self.deprecated += 1,
            Severity::Missing => self.missing += 1,
            Severity::Manual => self.manual += 1,
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

/// Everything read from one repository, before any check runs.
#[derive(Debug, Default)]
pub struct RepoInput {
    /// What the repository is called: the root file name its chora deed is
    /// expected under when it has none.
    pub name: String,
    /// The repository's tracked files.
    pub files: Vec<String>,
    /// The text of the root `*_chora.deed`, when there is one.
    pub chora_deed: Option<String>,
    /// The text of a legacy `rsr-profile.a2ml`. A read fallback, consulted
    /// only when the chora deed carries no `(rsr-profile ...)` clause.
    pub legacy_profile: Option<String>,
}

impl CanonReport {
    /// Check one repository's file list against the canon, with no chora deed.
    ///
    /// Kept for callers that only have a file list. `profile_source` is the
    /// text of a legacy profile, or `None`.
    pub fn build(
        subject: &str,
        canon: &Canon,
        gates: &GateTable,
        files: &[String],
        profile_source: Option<&str>,
    ) -> Result<Self> {
        Self::build_with(
            subject,
            canon,
            gates,
            &RepoInput {
                name: subject
                    .rsplit(['/', '\\'])
                    .next()
                    .unwrap_or(subject)
                    .to_string(),
                files: files.to_vec(),
                chora_deed: None,
                legacy_profile: profile_source.map(str::to_string),
            },
        )
    }

    /// Check one repository against the canon, as overlaid by [`overlay`].
    ///
    /// The profile comes from the chora deed's `(rsr-profile ...)` clause; a
    /// legacy `rsr-profile.a2ml` is read only when the deed has none. No
    /// profile at all means it declares no capabilities, not that it is
    /// non-compliant.
    pub fn build_with(
        subject: &str,
        canon: &Canon,
        gates: &GateTable,
        input: &RepoInput,
    ) -> Result<Self> {
        let deed = input
            .chora_deed
            .as_deref()
            .map(Deed::parse)
            .transpose()
            .context("the chora deed does not read as an s-expression")?;
        let format = DocFormat::from_deed(deed.as_ref())?;
        let chora_name =
            find_chora_deed(&input.files)?.unwrap_or_else(|| format!("{}_chora.deed", input.name));
        let overlay = Overlay::new(&chora_name, deed.as_ref(), format);

        let profile = match deed.as_ref().map(Deed::rsr_profile).transpose()?.flatten() {
            Some(clause) => Profile::from_clause(clause, gates)?,
            None => match input.legacy_profile.as_deref() {
                Some(source) => Profile::parse(source, gates)?,
                None => Profile::default(),
            },
        };
        let deprecations = Deprecation::from_canon(canon)?;

        // The files, plus one virtual path per clause of the chora deed.
        let mut files = input.files.clone();
        files.extend(overlay.virtual_files());
        let files = files.as_slice();

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
            extra_checks: 0,
        };

        for criterion in canon.criteria() {
            if !profile.is_applicable(criterion) {
                report.not_applicable += 1;
                continue;
            }

            if Overlay::is_suspended(&criterion.id) {
                report.not_file_questions += 1;
                continue;
            }

            if Overlay::is_manual_only(&criterion.id) {
                report.counts.record(Severity::Manual);
                report.findings.push(Finding::manual(criterion));
                continue;
            }

            let verdict = match overlay.replacement(criterion) {
                Some(replacement) => {
                    Verdict::of_replacement(criterion, &replacement, files, &deprecations)
                }
                None => match Verdict::of(criterion, files, &deprecations) {
                    Some(verdict) => verdict,
                    None => {
                        report.not_file_questions += 1;
                        continue;
                    }
                },
            };

            report.record(criterion, &verdict);
        }

        for (criterion, replacement) in overlay.extra_checks() {
            let verdict = Verdict::of_replacement(&criterion, &replacement, files, &deprecations);
            report.extra_checks += 1;
            report.record(&criterion, &verdict);
        }

        // Worst first, and stable, so equal severities stay in canon order --
        // which is category order, and the order a reader expects.
        report
            .findings
            .sort_by_key(|finding| std::cmp::Reverse(finding.severity));

        Ok(report)
    }

    fn record(&mut self, criterion: &super::Criterion, verdict: &Verdict) {
        self.scored += 1;
        let severity = verdict.severity();
        self.counts.record(severity);
        if severity.is_finding() {
            self.findings.push(Finding::of(criterion, verdict));
        }
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
        if self.extra_checks > 0 {
            out.push_str(&format!(
                "  (scored includes {} rhodibot check beyond the canon: the chora deed)\n",
                self.extra_checks
            ));
        }
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
    /// A criterion rhodibot cannot decide from files: visible, not graded.
    fn manual(criterion: &super::Criterion) -> Self {
        // The path the criterion is about, so a consumer can place the finding:
        // the first path its description names, else the canon's own template
        // path. Never empty, or a grid would grade it against every change.
        let about = super::requirement::requirement_from(&criterion.desc)
            .and_then(|requirement| requirement.paths().next().cloned())
            .unwrap_or_else(|| criterion.template_ref.clone());
        Self {
            id: criterion.id.clone(),
            name: criterion.name.clone(),
            tier: criterion.tier.to_string(),
            severity: Severity::Manual,
            canon_keeps_it_at: None,
            expected: vec![about],
            found: Vec::new(),
            deprecated_copies: Vec::new(),
            details: vec![format!(
                "manual check: not verified by rhodibot ({})",
                criterion.desc
            )],
        }
    }

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
                    expected.extend(alternatives.iter().map(|path| describe(path)));
                    found.extend(elsewhere.iter().cloned());
                    if let Some(note) =
                        super::verdict::canon_location_note(&verdict.template_ref, &elsewhere[0])
                    {
                        canon_keeps_it_at = Some(note);
                    }
                    let mut line = format!(
                        "present at {} — not {}",
                        elsewhere.join(", "),
                        describe_all(alternatives, " or ")
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
                    expected.extend(alternatives.iter().map(|path| describe(path)));
                    let mut line =
                        format!("absent — expected {}", describe_all(alternatives, " or "));
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

/// A path as a person reads it: a virtual `<deed>#<clause>` path is the clause.
fn describe(path: &str) -> String {
    match path.split_once('#') {
        Some((deed, clause)) => format!("the ({clause} ...) clause of {deed}"),
        None => path.to_string(),
    }
}

fn describe_all(paths: &[String], separator: &str) -> String {
    paths
        .iter()
        .map(|path| describe(path))
        .collect::<Vec<_>>()
        .join(separator)
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
        "CODE_OF_CONDUCT.adoc",
        "CONTRIBUTING.adoc",
        ".gitignore",
        ".gitattributes",
        ".well-known/security.txt",
        "widgets_chora.deed",
        // 3.1.1 asks for the descriptiles directory to exist.
        ".machine_readable/descriptiles/STATE.deed",
    ];

    /// The chora deed of a repository that carries every fact the canon asks for.
    const DEED: &str = "(repo-deed :schema-version \"1.0.0\" (manifest :version \"1\") (status :phase active) \
        (meta :version \"1\") (ecosystem :project \"w\") (agentic :version \"1\") (neurosym :version \"1\") \
        (playbook :version \"1\") (anchor :id \"a\") (rsr-profile :capabilities ()))";

    fn input(paths: &[&str], deed: Option<&str>, legacy: Option<&str>) -> RepoInput {
        RepoInput {
            name: "widgets".to_string(),
            files: files(paths),
            chora_deed: deed.map(str::to_string),
            legacy_profile: legacy.map(str::to_string),
        }
    }

    fn compliant() -> RepoInput {
        input(COMPLIANT, Some(DEED), None)
    }

    #[test]
    fn a_compliant_repository_has_nothing_to_act_on() {
        let report = CanonReport::build_with("acme/widgets", &canon(), &gates(), &compliant())
            .expect("the report builds");

        assert_eq!(report.findings.len(), 0, "{:#?}", report.findings);
        assert_eq!(report.counts.missing, 0);
        assert_eq!(report.counts.satisfied, report.scored);
        assert!(report.render().contains("nothing to act on."));
    }

    #[test]
    fn the_unapplicable_are_counted_and_not_reported() {
        let report = CanonReport::build_with("acme/widgets", &canon(), &gates(), &compliant())
            .expect("the report builds");

        // The vendored canon: 78 criteria, 30 gated on a capability.
        assert_eq!(report.criteria, 78);
        assert_eq!(report.not_applicable, 30);
        assert_eq!(report.scored, 23, "22 canon criteria and the chora deed");
        assert_eq!(report.extra_checks, 1);
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

        let without = CanonReport::build_with("acme/widgets", &canon(), &gates(), &compliant())
            .expect("builds");
        // The legacy file is only a fallback: here the deed has no profile clause.
        let deed_without_profile = DEED.replace(" (rsr-profile :capabilities ())", "");
        let with = CanonReport::build_with(
            "acme/widgets",
            &canon(),
            &gates(),
            &input(COMPLIANT, Some(&deed_without_profile), Some(profile)),
        )
        .expect("builds");

        // Ten criteria gate on governance-tier; five of them ask about files, four
        // ask content questions, and 3.1.9 (CLADE) is suspended by the overlay
        // because its deed form has not landed. Declaring the capability moves
        // ten out of `na` and adds five to `scored`.
        assert_eq!(without.not_applicable - with.not_applicable, 10);
        assert_eq!(without.scored + 5, with.scored);
        assert_eq!(without.not_file_questions + 5, with.not_file_questions);
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
    fn security_follows_the_same_adoc_or_md_toggle() {
        for file in ["SECURITY.adoc", "SECURITY.md"] {
            let report =
                CanonReport::build("acme/widgets", &canon(), &gates(), &files(&[file]), None)
                    .expect("builds");
            assert!(
                report.findings.iter().all(|finding| finding.id != "2.1.3"),
                "{file} satisfies 2.1.3"
            );
        }
        let missing =
            CanonReport::build("acme/widgets", &canon(), &gates(), &files(&[]), None).unwrap();
        let finding = missing.findings.iter().find(|f| f.id == "2.1.3").unwrap();
        assert_eq!(finding.expected[0], "SECURITY.adoc");
    }

    fn finding<'a>(report: &'a CanonReport, id: &str) -> Option<&'a Finding> {
        report.findings.iter().find(|finding| finding.id == id)
    }

    fn report_for(paths: &[&str], deed: Option<&str>, legacy: Option<&str>) -> CanonReport {
        CanonReport::build_with(
            "acme/widgets",
            &canon(),
            &gates(),
            &input(paths, deed, legacy),
        )
        .expect("builds")
    }

    #[test]
    fn adoc_community_documents_are_not_missing() {
        let report = report_for(&["CODE_OF_CONDUCT.adoc", "CONTRIBUTING.adoc"], None, None);
        assert!(
            finding(&report, "2.1.4").is_none(),
            "{:#?}",
            report.findings
        );
        assert!(
            finding(&report, "2.1.5").is_none(),
            "{:#?}",
            report.findings
        );
    }

    #[test]
    fn md_community_documents_are_not_missing() {
        let report = report_for(&["CODE_OF_CONDUCT.md", "CONTRIBUTING.md"], None, None);
        assert!(
            finding(&report, "2.1.4").is_none(),
            "{:#?}",
            report.findings
        );
        assert!(
            finding(&report, "2.1.5").is_none(),
            "{:#?}",
            report.findings
        );
    }

    #[test]
    fn both_forms_present_is_fine_and_neither_is_required_alongside_the_other() {
        let report = report_for(
            &[
                "CODE_OF_CONDUCT.adoc",
                "CODE_OF_CONDUCT.md",
                "CONTRIBUTING.adoc",
                "CONTRIBUTING.md",
            ],
            None,
            None,
        );
        assert!(finding(&report, "2.1.4").is_none() && finding(&report, "2.1.5").is_none());
    }

    #[test]
    fn a_missing_document_names_adoc_first_by_default() {
        let report = report_for(&[], None, None);
        let missing = finding(&report, "2.1.5").expect("CONTRIBUTING is missing");
        assert_eq!(missing.severity, Severity::Missing);
        assert_eq!(missing.expected[0], "CONTRIBUTING.adoc");
        assert!(missing.expected.contains(&"CONTRIBUTING.md".to_string()));
    }

    #[test]
    fn the_bot_directive_makes_md_the_named_form_but_adoc_still_satisfies() {
        let deed = "(repo-deed (bot-directives (bot :name rhodibot \
                    (config :community-doc-format md))))";

        let missing = report_for(&[], Some(deed), None);
        let finding_missing = finding(&missing, "2.1.4").expect("CODE_OF_CONDUCT is missing");
        assert_eq!(finding_missing.expected[0], "CODE_OF_CONDUCT.md");

        // The override changes the wording, never whether the other form counts.
        let adoc_present = report_for(
            &["CODE_OF_CONDUCT.adoc", "CONTRIBUTING.adoc"],
            Some(deed),
            None,
        );
        assert!(finding(&adoc_present, "2.1.4").is_none());
        assert!(finding(&adoc_present, "2.1.5").is_none());

        let md_present = report_for(&["CODE_OF_CONDUCT.md", "CONTRIBUTING.md"], Some(deed), None);
        assert!(finding(&md_present, "2.1.4").is_none());
    }

    #[test]
    fn a_bad_doc_format_directive_is_an_error_not_a_default() {
        let deed = "(repo-deed (bot-directives (bot :name rhodibot \
                    (config :community-doc-format rst))))";
        assert!(
            CanonReport::build_with(
                "acme/widgets",
                &canon(),
                &gates(),
                &input(&[], Some(deed), None)
            )
            .is_err()
        );
    }

    #[test]
    fn nothing_the_report_expects_is_an_a2ml_file() {
        // A bare repository is missing everything; none of it is an A2ML file.
        let report = report_for(&[], None, None);
        for finding in &report.findings {
            for expected in &finding.expected {
                assert!(
                    !expected.ends_with(".a2ml"),
                    "{} still expects {expected}",
                    finding.id
                );
            }
            for detail in &finding.details {
                assert!(
                    !detail.contains(".a2ml"),
                    "{} still mentions an a2ml in {detail:?}",
                    finding.id
                );
            }
        }
        let ids: Vec<&str> = report.findings.iter().map(|f| f.id.as_str()).collect();
        for id in [
            "2.3.1", "3.1.2", "3.1.3", "3.1.4", "3.1.5", "3.1.6", "3.1.7", "3.1.8", "3.2.2",
        ] {
            assert!(
                ids.contains(&id),
                "{id} should ask for its deed form: {ids:?}"
            );
        }
    }

    #[test]
    fn the_chora_deed_is_required_and_named_after_the_repository() {
        let report = report_for(&[], None, None);
        let missing = finding(&report, super::super::overlay::CHORA_DEED_ID)
            .expect("a repository with no chora deed is told so");
        assert_eq!(missing.severity, Severity::Missing);
        assert_eq!(missing.expected, vec!["widgets_chora.deed".to_string()]);
    }

    #[test]
    fn a_deed_clause_satisfies_what_an_a2ml_file_used_to() {
        let report = report_for(&["widgets_chora.deed"], Some(DEED), None);
        for id in [
            "2.3.1", "3.1.2", "3.1.3", "3.1.4", "3.1.5", "3.1.6", "3.1.7", "3.1.8", "3.2.2",
        ] {
            assert!(
                finding(&report, id).is_none(),
                "{id}: {:#?}",
                report.findings
            );
        }
        assert!(finding(&report, super::super::overlay::CHORA_DEED_ID).is_none());
    }

    #[test]
    fn a_descriptile_deed_satisfies_state_meta_ecosystem_and_agentic() {
        let report = report_for(
            &[
                "widgets_chora.deed",
                ".machine_readable/descriptiles/STATE.deed",
                ".machine_readable/descriptiles/META.deed",
                ".machine_readable/descriptiles/ECOSYSTEM.deed",
                ".machine_readable/descriptiles/AGENTIC.deed",
            ],
            Some("(repo-deed)"),
            None,
        );
        for id in ["3.1.2", "3.1.3", "3.1.4", "3.1.5"] {
            assert!(
                finding(&report, id).is_none(),
                "{id}: {:#?}",
                report.findings
            );
        }
    }

    #[test]
    fn a_legacy_a2ml_is_read_as_migrate_never_as_missing_and_never_recommended() {
        let report = report_for(
            &[
                "widgets_chora.deed",
                ".machine_readable/descriptiles/STATE.a2ml",
            ],
            Some("(repo-deed)"),
            None,
        );
        let state = finding(&report, "3.1.2").expect("present only in the legacy form");
        assert_eq!(state.severity, Severity::Relocated, "{state:?}");
        // The form it is told to move to is the deed.
        assert!(
            state.expected.iter().all(|path| !path.ends_with(".a2ml")),
            "{state:?}"
        );
        assert!(
            state
                .expected
                .iter()
                .any(|path| path.contains("STATE.deed"))
        );
        assert!(
            state.canon_keeps_it_at.is_none(),
            "the canon's own a2ml location must not be quoted: {state:?}"
        );
    }

    #[test]
    fn a_legacy_profile_is_read_only_when_the_deed_declares_none() {
        let legacy = "[rsr-profile]\nversion = \"1.0.0\"\ncapabilities = [\"governance-tier\"]\n";
        // No deed profile: the fallback applies.
        let fallback = report_for(&["widgets_chora.deed"], Some("(repo-deed)"), Some(legacy));
        assert_eq!(
            fallback.declared_capabilities,
            vec!["governance-tier".to_string()]
        );
        // A deed profile wins over the legacy file.
        let deed = "(repo-deed (rsr-profile :capabilities ()))";
        let first = report_for(&["widgets_chora.deed"], Some(deed), Some(legacy));
        assert!(first.declared_capabilities.is_empty());
    }

    #[test]
    fn the_deed_profile_declares_capabilities() {
        let deed = "(repo-deed (rsr-profile :capabilities (\"governance-tier\")))";
        let report = report_for(&["widgets_chora.deed"], Some(deed), None);
        assert_eq!(
            report.declared_capabilities,
            vec!["governance-tier".to_string()]
        );
    }

    #[test]
    fn a_misspelt_deed_capability_is_refused() {
        let deed = "(repo-deed (rsr-profile :capabilities (\"governence-tier\")))";
        assert!(
            CanonReport::build_with(
                "acme/widgets",
                &canon(),
                &gates(),
                &input(&["widgets_chora.deed"], Some(deed), None)
            )
            .is_err()
        );
    }

    #[test]
    fn an_unreadable_deed_is_an_error_not_an_absent_one() {
        assert!(
            CanonReport::build_with(
                "acme/widgets",
                &canon(),
                &gates(),
                &input(&["widgets_chora.deed"], Some("(repo-deed"), None)
            )
            .is_err()
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

        let satisfiable = CanonReport::build_with("acme/widgets", &canon(), &gates(), &compliant())
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

#[cfg(test)]
mod manual_tests {
    use super::*;

    #[test]
    fn a_deployment_question_is_a_visible_manual_info_not_a_missing_file() {
        let profile = "[rsr-profile]\nversion = \"1.0.0\"\ncapabilities = [\"docs-site\"]\n";
        let report = CanonReport::build(
            "acme/widgets",
            &Canon::vendored().unwrap(),
            &GateTable::vendored().unwrap(),
            // The site is served from www/, so .well-known/ is not at the root.
            &["www/.well-known/security.txt".to_string()],
            Some(profile),
        )
        .unwrap();
        let finding = report
            .findings
            .iter()
            .find(|f| f.id == "9.1.4")
            .expect("still visible");
        assert_eq!(finding.severity, Severity::Manual);
        assert!(finding.details[0].contains("manual check: not verified by rhodibot"));
        // Every manual finding carries a non-empty path, or a grid would grade
        // it against every change.
        for manual in report
            .findings
            .iter()
            .filter(|f| f.severity == Severity::Manual)
        {
            let path = manual.expected.first().map(String::as_str).unwrap_or("");
            assert!(!path.is_empty() && path != "-", "{} has no path", manual.id);
        }
        assert_eq!(finding.expected, vec![".well-known/".to_string()]);
        assert!(report.counts.manual >= 1);
        // Never trips a --fail-on threshold, even the lowest.
        assert!(!FailOn::Relocated.breached_by(&report) || report.counts.relocated > 0);
    }

    #[test]
    fn a_manual_detect_file_question_is_still_graded() {
        // 1.2.4 is detect = manual but names a file: it stays a real finding.
        let report = CanonReport::build(
            "acme/widgets",
            &Canon::vendored().unwrap(),
            &GateTable::vendored().unwrap(),
            &[],
            None,
        )
        .unwrap();
        let tv = report.findings.iter().find(|f| f.id == "1.2.4").unwrap();
        assert_eq!(tv.severity, Severity::Missing);
    }
}
