// SPDX-License-Identifier: MPL-2.0

//! The RSR criteria, read from the canon instead of remembered here.
//!
//! # Why this module exists
//!
//! Rhodibot's compliance rules used to be a hand-written table compiled into
//! [`crate::rsr`]: paths, points and severities decided once, by hand, and
//! edited by hand ever after. The canon (`standards/0-canon/rsr/
//! rsr-criteria-v2.a2ml`) is the single source of truth for those criteria, and
//! it says so explicitly:
//!
//! > This file is the sole normative source for RSR v2.0 criteria. Every other
//! > artefact — the prose checklist, the one normative checker's rule table,
//! > and the badge thresholds — is GENERATED from this file.
//!
//! So the rule table belongs downstream of the canon, not beside it. This module
//! reads the canon's A2ML record dialect into typed data, validates it, and
//! refuses to load a copy that has drifted from its pin.
//!
//! # What this module is not
//!
//! RSR v2.0 designates exactly one *normative* checker — hypatia's
//! `rsr-conformance` rule family — and rhodibot is not it. The canon retires
//! most of what came before (`rsr-audit.sh` demoted to a non-normative
//! reference, `rsr-check.scm` retired with the `.scm` era,
//! `rsr-compliance-checklist.k9.ncl` retired) and names `rsr-certifier`
//! explicitly as "product, not the spec's oracle".
//!
//! Rhodibot is therefore a consumer: it evaluates what it can see from a
//! repository tree, reports against canon criterion ids rather than inventing
//! its own, and stays advisory. Nothing here should be read as authority.
//!
//! # Fail-closed, on purpose
//!
//! Parsing is validated rather than trusted. A canon copy that loses a category,
//! repeats a criterion id, invents a tier, or whose weights stop adding up to
//! the total the canon declares for itself is an error, not a smaller rule set.
//! The failure mode to avoid is the quiet one: a rule set that silently shrinks
//! and reports every repository as compliant.

pub mod requirement;

use std::collections::HashSet;
use std::fmt;
use std::path::Path;

use anyhow::{Context, Result, bail, ensure};
use serde::Deserialize;
use sha2::{Digest, Sha256};

/// The vendored canon, embedded at compile time.
///
/// Embedded rather than read at run time so that the rule set a binary applies
/// is fixed when it is built: a deployment cannot end up evaluating against
/// whatever the filesystem happens to contain.
pub const VENDORED_CRITERIA: &str = include_str!("../canon/rsr-criteria-v2.a2ml");

/// The pin describing which canon revision [`VENDORED_CRITERIA`] came from.
const VENDORED_PIN: &str = include_str!("../canon/pin.toml");

/// SHA-256 of a canon source, hex encoded.
pub fn digest_of(source: &str) -> String {
    hex::encode(Sha256::digest(source.as_bytes()))
}

/// The lowest tier at which a criterion is required.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Tier {
    Bronze,
    Silver,
    Gold,
    Rhodium,
}

impl Tier {
    /// Parse a tier name as the canon spells it.
    pub fn parse(value: &str) -> Result<Self> {
        match value {
            "bronze" => Ok(Self::Bronze),
            "silver" => Ok(Self::Silver),
            "gold" => Ok(Self::Gold),
            "rhodium" => Ok(Self::Rhodium),
            other => bail!("unknown tier {other:?} (expected bronze, silver, gold or rhodium)"),
        }
    }

    /// The canon's spelling, for reports.
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Bronze => "bronze",
            Self::Silver => "silver",
            Self::Gold => "gold",
            Self::Rhodium => "rhodium",
        }
    }
}

impl fmt::Display for Tier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

/// One criterion from the canon.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Criterion {
    /// Stable dotted id (`category.group.item`), stable across minor releases.
    pub id: String,
    /// Short slug.
    pub name: String,
    /// What the criterion requires, in the canon's own words.
    pub desc: String,
    /// Lowest tier at which it is required.
    pub tier: Tier,
    /// `universal`, or a capability the repository must declare.
    pub gate: String,
    /// The hypatia rule that detects it, or `manual`.
    pub detect: String,
    /// Where the template satisfies it, or `-`.
    pub template_ref: String,
}

impl Criterion {
    /// Whether the canon admits there is no automated check for this yet.
    pub fn is_manual(&self) -> bool {
        self.detect == "manual"
    }

    /// The capability this criterion is gated on, or `None` when universal.
    pub fn capability(&self) -> Option<&str> {
        (self.gate != "universal").then_some(self.gate.as_str())
    }
}

/// A weighted category of criteria.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Category {
    pub id: u32,
    pub key: String,
    pub name: String,
    pub weight: u32,
    pub criteria: Vec<Criterion>,
}

/// Tier thresholds, as percentages of the applicable weighted set.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Tiers {
    pub bronze: u32,
    pub silver: u32,
    pub gold: u32,
    pub rhodium: u32,
}

/// The canon, parsed and validated.
#[derive(Debug, Clone)]
pub struct Canon {
    pub version: String,
    pub status: String,
    pub tiers: Tiers,
    pub categories: Vec<Category>,
    /// The total the canon declares for its own category weights, if it says.
    pub declared_weight_sum: Option<u32>,
    /// SHA-256 of the exact source this was parsed from.
    pub digest: String,
}

impl Canon {
    /// Parse and validate canon source.
    pub fn parse(source: &str) -> Result<Self> {
        let mut builder = Builder::default();
        let mut section = String::new();
        let mut category: Option<Category> = None;
        let mut in_list = false;
        let mut skipping_list = false;
        let mut line_no = 0;

        for raw in source.lines() {
            line_no += 1;
            let line = strip_comment(raw).trim().to_string();
            if line.is_empty() {
                continue;
            }

            // Inside an open `criteria = [` list, only records and the closing
            // bracket are valid. Anything else -- a section header among them --
            // means the list was never closed, which is what a truncated or
            // mis-edited canon looks like. Swallowing it would drop criteria
            // silently, which is the failure this module exists to prevent.
            if in_list {
                if line == "]" {
                    in_list = false;
                    continue;
                }
                ensure!(
                    line.starts_with('{'),
                    "line {line_no}: expected a criteria record or `]`, found {line:?}"
                );
                let target = category.as_mut().with_context(|| {
                    format!("line {line_no}: a criteria record outside any category")
                })?;
                target.criteria.push(parse_record(&line, line_no)?);
                continue;
            }

            // Prose lists (`[oracle]` has `retired = [ "rsr-audit.sh ...", ... ]`)
            // are skipped rather than parsed: nothing here consumes them, and
            // refusing to read the canon over one would make this consumer
            // brittle for no gain. Their contents are not assignments, so they
            // cannot be read as any.
            if skipping_list {
                if line == "]" {
                    skipping_list = false;
                }
                continue;
            }

            if line == "[[category]]" {
                if let Some(previous) = category.take() {
                    builder.categories.push(previous);
                }
                category = Some(Category {
                    id: 0,
                    key: String::new(),
                    name: String::new(),
                    weight: 0,
                    criteria: Vec::new(),
                });
                section = "category".to_string();
                continue;
            }

            if line.starts_with('[') && line.ends_with(']') {
                section = line.trim_matches(['[', ']']).trim().to_string();
                continue;
            }

            let Some((key, value)) = split_assignment(&line) else {
                bail!("line {line_no}: expected `key = value`, found {line:?}");
            };

            // A multi-line list other than a category's criteria: skip it.
            if value == "[" && !(section == "category" && key == "criteria") {
                ensure!(
                    section != "category",
                    "line {line_no}: unknown category field {key:?}; \
                     this parser is older than the canon it is reading"
                );
                skipping_list = true;
                continue;
            }

            match section.as_str() {
                "meta" => match key {
                    "version" => builder.version = unquote(value, line_no)?,
                    "status" => builder.status = unquote(value, line_no)?,
                    // The rest of [meta] is prose about the spec. A new key
                    // there is harmless to a consumer, so it is ignored rather
                    // than treated as a reason to refuse the file.
                    _ => {}
                },
                // The four thresholds are numbers; the same section also
                // carries prose (`scale = "X F E D C B A maps onto ..."`), so
                // only the known keys are parsed. That all four are present and
                // in order is checked once, at the end.
                "tiers" => {
                    if matches!(key, "bronze" | "silver" | "gold" | "rhodium") {
                        let parsed: u32 = value
                            .parse()
                            .with_context(|| format!("line {line_no}: {key} is not a number"))?;
                        match key {
                            "bronze" => builder.tiers.bronze = parsed,
                            "silver" => builder.tiers.silver = parsed,
                            "gold" => builder.tiers.gold = parsed,
                            _ => builder.tiers.rhodium = parsed,
                        }
                    }
                }
                "weights-check" => {
                    if key == "sum-declared" {
                        builder.declared_weight_sum =
                            Some(value.parse().with_context(|| {
                                format!("line {line_no}: {key} is not a number")
                            })?);
                    }
                }
                "category" => {
                    let target = category.as_mut().with_context(|| {
                        format!("line {line_no}: a category field before any [[category]]")
                    })?;
                    match key {
                        "id" => {
                            target.id = value.parse().with_context(|| {
                                format!("line {line_no}: category id is not a number")
                            })?
                        }
                        "key" => target.key = unquote(value, line_no)?,
                        "name" => target.name = unquote(value, line_no)?,
                        "weight" => {
                            target.weight = value.parse().with_context(|| {
                                format!("line {line_no}: category weight is not a number")
                            })?
                        }
                        "criteria" => {
                            ensure!(
                                value == "[",
                                "line {line_no}: expected `criteria = [`, found {value:?}"
                            );
                            in_list = true;
                        }
                        // A field this parser does not know about, inside the
                        // structural part of the canon, means the parser and the
                        // canon disagree about what a category is. Refusing is
                        // the point: a typo such as `weigth = 12` would
                        // otherwise drop a weight and quietly change every score.
                        other => bail!(
                            "line {line_no}: unknown category field {other:?}; \
                             this parser is older than the canon it is reading"
                        ),
                    }
                }
                // [scoring], [oracle] and [versioning] are prose for humans and
                // for hypatia; nothing here consumes them yet.
                _ => {}
            }
        }

        ensure!(
            !in_list,
            "the canon ends inside a `criteria = [` list: the closing `]` is missing"
        );

        if let Some(last) = category.take() {
            builder.categories.push(last);
        }

        builder.finish(digest_of(source))
    }

    /// The canon vendored in this repository.
    pub fn vendored() -> Result<Self> {
        Self::parse(VENDORED_CRITERIA)
    }

    /// Parse a canon file from disk.
    pub fn load(path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();
        let source = std::fs::read_to_string(path)
            .with_context(|| format!("reading canon at {}", path.display()))?;
        Self::parse(&source)
    }

    /// Every criterion, in canon order.
    pub fn criteria(&self) -> impl Iterator<Item = &Criterion> {
        self.categories.iter().flat_map(|c| c.criteria.iter())
    }

    /// How many criteria the canon defines.
    pub fn criterion_count(&self) -> usize {
        self.criteria().count()
    }

    /// The sum of category weights as written.
    pub fn weight_sum(&self) -> u32 {
        self.categories.iter().map(|c| c.weight).sum()
    }

    /// Criteria a repository must satisfy at the given tier or below, for the
    /// criteria that are universal. Capability-gated criteria are excluded:
    /// they apply only where the repository declares the capability.
    pub fn universal_criteria_up_to(&self, tier: Tier) -> impl Iterator<Item = &Criterion> {
        self.criteria()
            .filter(move |c| c.tier <= tier && c.capability().is_none())
    }
}

/// A pin: which canon revision a vendored copy came from.
#[derive(Debug, Clone, Deserialize)]
pub struct Pin {
    pub source: PinSource,
}

#[derive(Debug, Clone, Deserialize)]
pub struct PinSource {
    pub repo: String,
    pub path: String,
    pub canon_version: String,
    pub criteria_version: String,
    pub released: String,
    pub sha256: String,
    /// How many categories the pinned revision has.
    pub categories: usize,
    /// How many criteria it has.
    pub criteria: usize,
    /// The total its category weights sum to.
    pub weight_sum: u32,
}

impl Pin {
    /// Parse a pin file.
    pub fn parse(source: &str) -> Result<Self> {
        toml::from_str(source).context("parsing the canon pin")
    }

    /// The pin shipped beside the vendored canon.
    pub fn vendored() -> Result<Self> {
        Self::parse(VENDORED_PIN)
    }

    /// Check a parsed canon against the shape this pin describes.
    ///
    /// This complements [`Self::verify`], which checks the source digest. The
    /// counts catch a hand-edited file whose digest was updated along with it,
    /// or a parse that silently lost records. A rule set that shrinks is the
    /// failure that matters here, so the size of the rule set is pinned too.
    pub fn verify_counts(&self, canon: &Canon) -> Result<()> {
        let source = &self.source;
        ensure!(
            canon.version == source.criteria_version,
            "pin names criteria version {}, this copy declares {}",
            source.criteria_version,
            canon.version
        );
        ensure!(
            canon.categories.len() == source.categories,
            "pinned canon has {} categories, this copy has {}",
            source.categories,
            canon.categories.len()
        );
        ensure!(
            canon.criterion_count() == source.criteria,
            "pinned canon has {} criteria, this copy has {}",
            source.criteria,
            canon.criterion_count()
        );
        ensure!(
            canon.weight_sum() == source.weight_sum,
            "pinned canon weights sum to {}, this copy sums to {}",
            source.weight_sum,
            canon.weight_sum()
        );
        Ok(())
    }

    /// Check a canon source's SHA-256 digest against this pin.
    pub fn verify(&self, source: &str) -> Result<()> {
        let actual = digest_of(source);
        let expected = self.source.sha256.to_lowercase();
        if actual != expected {
            bail!(
                "vendored canon does not match its pin: {} says {} at {}, this copy hashes {}. \
                 Either re-pin (copy {}, update sha256 in canon/pin.toml) or restore the copy.",
                self.source.path,
                expected,
                self.source.released,
                actual,
                self.source.path
            );
        }
        Ok(())
    }
}

impl fmt::Display for Pin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}@{} ({})",
            self.source.repo, self.source.path, self.source.sha256
        )
    }
}

#[derive(Default)]
struct Builder {
    version: String,
    status: String,
    tiers: Tiers,
    categories: Vec<Category>,
    declared_weight_sum: Option<u32>,
}

impl Builder {
    /// Validate the accumulated sections and build a canon.
    fn finish(self, digest: String) -> Result<Canon> {
        ensure!(
            !self.version.is_empty(),
            "the canon declares no [meta] version; a rule set without a version cannot be pinned"
        );
        ensure!(
            !self.categories.is_empty(),
            "the canon declares no [[category]] blocks; refusing to apply an empty rule set"
        );

        let mut seen_keys = HashSet::new();
        let mut seen_ids = HashSet::new();

        for (index, category) in self.categories.iter().enumerate() {
            let expected_category_id = index as u32 + 1;
            ensure!(
                category.id == expected_category_id,
                "category ids must run consecutively from 1; expected {expected_category_id}, found {}",
                category.id
            );

            ensure!(
                !category.key.is_empty() && !category.name.is_empty(),
                "category {} has no key or name",
                category.id
            );
            ensure!(
                seen_keys.insert(category.key.clone()),
                "category key {:?} appears twice",
                category.key
            );
            ensure!(
                category.weight > 0,
                "category {} has a zero weight; every weight must be positive",
                category.id
            );
            ensure!(
                !category.criteria.is_empty(),
                "category {} ({}) declares no criteria",
                category.id,
                category.key
            );

            for criterion in &category.criteria {
                let mut parts = criterion.id.split('.');
                let category_part = parts.next().unwrap_or_default();
                ensure!(
                    parts.next().is_some() && parts.next().is_some() && parts.next().is_none(),
                    "criterion id {:?} is not `category.group.item`",
                    criterion.id
                );
                let declared_category: u32 = category_part.parse().with_context(|| {
                    format!("criterion id {:?} has a non-numeric category", criterion.id)
                })?;
                ensure!(
                    declared_category == category.id,
                    "criterion {:?} sits in category {}",
                    criterion.id,
                    category.id
                );
                ensure!(
                    seen_ids.insert(criterion.id.clone()),
                    "criterion id {:?} appears twice",
                    criterion.id
                );
                ensure!(
                    !criterion.name.is_empty()
                        && !criterion.desc.is_empty()
                        && !criterion.gate.is_empty()
                        && !criterion.detect.is_empty()
                        && !criterion.template_ref.is_empty(),
                    "criterion {:?} is missing a field",
                    criterion.id
                );
            }
        }

        // Every threshold must be stated, and the ladder must ascend. A tier
        // left at zero because its key was misspelt would make everything pass.
        let tiers = self.tiers;
        ensure!(
            tiers.bronze > 0
                && tiers.bronze <= tiers.silver
                && tiers.silver <= tiers.gold
                && tiers.gold <= tiers.rhodium
                && tiers.rhodium <= 100,
            "tier thresholds are missing or out of order: bronze={} silver={} gold={} rhodium={}",
            tiers.bronze,
            tiers.silver,
            tiers.gold,
            tiers.rhodium
        );

        // The canon checks its own arithmetic in [weights-check]. That section
        // is required, not optional: it sits at the end of the file, so a
        // truncated copy loses it along with whatever was cut -- which is
        // exactly the case where a missing check would go unnoticed. Refusing a
        // canon that does not state its own total means truncation cannot hide.
        let declared = self.declared_weight_sum.with_context(|| {
            "the canon states no [weights-check] sum-declared total, so its own weights cannot be \
             checked; refusing to apply a rule set that cannot be verified"
        })?;

        let weight_sum: u32 = self.categories.iter().map(|c| c.weight).sum();
        ensure!(
            declared == weight_sum,
            "the canon declares its category weights sum to {declared}, but they sum to {weight_sum}; \
             a mismatch means this copy lost or gained a category"
        );

        Ok(Canon {
            version: self.version,
            status: self.status,
            tiers: self.tiers,
            categories: self.categories,
            declared_weight_sum: self.declared_weight_sum,
            digest,
        })
    }
}

/// Parse `{ field = "value", ... }` into a criterion.
fn parse_record(line: &str, line_no: usize) -> Result<Criterion> {
    // List entries are comma-separated, so the closing brace is normally
    // followed by one. The brace that matters is the last on the line: several
    // `desc` values carry braces of their own
    // (`.well-known/{security.txt,ai.txt,humans.txt}`).
    let record = line.trim().trim_end_matches(',').trim();
    let inner = record
        .strip_prefix('{')
        .and_then(|rest| rest.strip_suffix('}'))
        .with_context(|| {
            format!("line {line_no}: a criteria record must be one `{{ ... }}` line")
        })?;

    let (mut id, mut name, mut desc, mut tier, mut gate, mut detect, mut template_ref) =
        (None, None, None, None, None, None, None);

    for field in split_fields(inner) {
        let field = field.trim();
        if field.is_empty() {
            bail!("line {line_no}: empty field in a criteria record");
        }
        let (key, value) = split_assignment(field)
            .with_context(|| format!("line {line_no}: field {field:?} is not `key = value`"))?;
        let value = unquote(value, line_no)?;
        match key {
            "id" => id = Some(value),
            "name" => name = Some(value),
            "desc" => desc = Some(value),
            "tier" => tier = Some(Tier::parse(&value).with_context(|| format!("line {line_no}"))?),
            "gate" => gate = Some(value),
            "detect" => detect = Some(value),
            "template_ref" => template_ref = Some(value),
            other => bail!("line {line_no}: unknown criterion field {other:?}"),
        }
    }

    let missing = [
        ("id", id.is_none()),
        ("name", name.is_none()),
        ("desc", desc.is_none()),
        ("tier", tier.is_none()),
        ("gate", gate.is_none()),
        ("detect", detect.is_none()),
        ("template_ref", template_ref.is_none()),
    ]
    .iter()
    .filter(|(_, absent)| *absent)
    .map(|(name, _)| *name)
    .collect::<Vec<_>>();
    ensure!(
        missing.is_empty(),
        "line {line_no}: criteria record is missing {missing:?}"
    );

    Ok(Criterion {
        id: id.unwrap_or_default(),
        name: name.unwrap_or_default(),
        desc: desc.unwrap_or_default(),
        tier: tier.unwrap_or(Tier::Bronze),
        gate: gate.unwrap_or_default(),
        detect: detect.unwrap_or_default(),
        template_ref: template_ref.unwrap_or_default(),
    })
}

/// Drop a `#` comment, ignoring a `#` inside a quoted value.
fn strip_comment(line: &str) -> &str {
    let mut in_string = false;
    for (index, ch) in line.char_indices() {
        match ch {
            '"' => in_string = !in_string,
            '#' if !in_string => return &line[..index],
            _ => {}
        }
    }
    line
}

/// Split at the first `=` and trim whitespace from the key and value.
fn split_assignment(line: &str) -> Option<(&str, &str)> {
    let (key, value) = line.split_once('=')?;
    Some((key.trim(), value.trim()))
}

/// Remove the surrounding quotes from a quoted value.
fn unquote(value: &str, line_no: usize) -> Result<String> {
    value
        .strip_prefix('"')
        .and_then(|rest| rest.strip_suffix('"'))
        .map(str::to_string)
        .with_context(|| format!("line {line_no}: expected a quoted value, found {value:?}"))
}

/// Split a record body on commas that are not inside a quoted value.
///
/// The canon's `desc` fields contain commas (`.well-known/{security.txt,ai.txt,
/// humans.txt}` is one value), so splitting naively loses fields -- which is how
/// a "missing tier" appears where the canon is perfectly well formed.
fn split_fields(inner: &str) -> Vec<&str> {
    let mut fields = Vec::new();
    let mut start = 0;
    let mut in_string = false;
    for (index, ch) in inner.char_indices() {
        match ch {
            '"' => in_string = !in_string,
            ',' if !in_string => {
                fields.push(&inner[start..index]);
                start = index + 1;
            }
            _ => {}
        }
    }
    fields.push(&inner[start..]);
    fields
}

#[cfg(test)]
mod tests {
    use super::*;

    fn vendored() -> Canon {
        Canon::vendored().expect("the vendored canon parses")
    }

    #[test]
    fn parses_the_vendored_canon() {
        let canon = vendored();
        assert_eq!(canon.categories.len(), 11, "the canon has 11 categories");
        assert_eq!(canon.criterion_count(), 74);
        assert_eq!(canon.weight_sum(), 88);
        assert_eq!(canon.declared_weight_sum, Some(88));
        assert_eq!(canon.tiers.bronze, 75);
        assert_eq!(canon.tiers.rhodium, 100);
        assert_eq!(canon.status, "draft");
    }

    #[test]
    fn the_vendored_canon_matches_its_pin() {
        let pin = Pin::vendored().expect("pin parses");
        pin.verify(VENDORED_CRITERIA).expect("pin verifies");
        pin.verify_counts(&vendored()).expect("shape verifies");
        assert_eq!(pin.source.repo, "hyperpolymath/standards");
        assert_eq!(pin.source.sha256, digest_of(VENDORED_CRITERIA));
        assert_eq!(pin.source.categories, 11);
        assert_eq!(pin.source.criteria, 74);
        assert_eq!(pin.source.weight_sum, 88);
    }

    #[test]
    fn criteria_are_grouped_under_their_category() {
        let canon = vendored();
        for category in &canon.categories {
            for criterion in &category.criteria {
                let prefix: u32 = criterion
                    .id
                    .split('.')
                    .next()
                    .and_then(|p| p.parse().ok())
                    .expect("numeric prefix");
                assert_eq!(
                    prefix, category.id,
                    "{} is in the wrong category",
                    criterion.id
                );
            }
        }
    }

    #[test]
    fn the_canon_is_honest_about_what_it_cannot_detect() {
        // Not a rule about the canon so much as a guard on the pilot: if this
        // number moves, the automated coverage story changes with it.
        let canon = vendored();
        let manual = canon.criteria().filter(|c| c.is_manual()).count();
        assert_eq!(manual, 19, "criteria with no automated detection");
        let gated = canon
            .criteria()
            .filter(|c| c.capability().is_some())
            .count();
        assert_eq!(gated, 26, "criteria gated on a declared capability");
    }

    // ---- fail-closed ---------------------------------------------------------

    #[test]
    fn a_duplicate_criterion_id_is_rejected() {
        let source = VENDORED_CRITERIA.replacen("{ id = \"1.1.3\"", "{ id = \"1.1.1\"", 1);
        let error = Canon::parse(&source).expect_err("duplicates must not parse");
        assert!(format!("{error:#}").contains("appears twice"), "{error:#}");
    }

    #[test]
    fn a_criterion_in_the_wrong_category_is_rejected() {
        let source = VENDORED_CRITERIA.replacen("{ id = \"2.1.1\"", "{ id = \"7.1.1\"", 1);
        let error = Canon::parse(&source).expect_err("a misfiled criterion must not parse");
        assert!(
            format!("{error:#}").contains("sits in category"),
            "{error:#}"
        );
    }

    #[test]
    fn an_unknown_tier_is_rejected() {
        let source = VENDORED_CRITERIA.replacen("tier = \"bronze\"", "tier = \"platinum\"", 1);
        let error = Canon::parse(&source).expect_err("an invented tier must not parse");
        assert!(format!("{error:#}").contains("unknown tier"), "{error:#}");
    }

    #[test]
    fn weights_that_disagree_with_the_declared_total_are_rejected() {
        let source = VENDORED_CRITERIA.replacen("sum-declared = 88", "sum-declared = 100", 1);
        let error =
            Canon::parse(&source).expect_err("arithmetic that no longer holds must not parse");
        assert!(format!("{error:#}").contains("sum to"), "{error:#}");
    }

    #[test]
    fn a_lost_category_is_caught_by_the_arithmetic() {
        // Drop category 11 entirely, as a truncated copy would. The remaining
        // weights no longer match the declared total, so the file is refused
        // rather than quietly evaluated as a smaller rule set.
        // The canon holds 11 `[[category]]` headers, so splitting on them gives
        // 12 pieces: the preamble plus 11 categories. Keeping 11 drops the last.
        let truncated: String = VENDORED_CRITERIA
            .split("[[category]]")
            .take(11)
            .collect::<Vec<_>>()
            .join("[[category]]");
        let error = Canon::parse(&truncated).expect_err("a truncated canon must not parse");
        let message = format!("{error:#}");
        assert!(
            message.contains("weights-check"),
            "truncation must be refused for the right reason: {message}"
        );
    }

    #[test]
    fn an_unterminated_criteria_list_is_rejected() {
        // Written out rather than mutated from the vendored file so the test
        // does not depend on that file's formatting. The first category's list
        // is never closed; the next header therefore arrives with it still open.
        let source = "\
[meta]
version = \"2.0.0\"

[[category]]
id = 1
key = \"first\"
name = \"First\"
weight = 1
criteria = [
  { id = \"1.1.1\", name = \"n\", desc = \"d\", tier = \"bronze\", gate = \"universal\", detect = \"manual\", template_ref = \"-\" }

[[category]]
id = 2
key = \"second\"
name = \"Second\"
weight = 1
criteria = [
  { id = \"2.1.1\", name = \"n\", desc = \"d\", tier = \"bronze\", gate = \"universal\", detect = \"manual\", template_ref = \"-\" }
]
";
        let error = Canon::parse(source).expect_err("an unterminated list must not parse");
        assert!(
            format!("{error:#}").contains("expected a criteria record or `]`"),
            "{error:#}"
        );
    }

    #[test]
    fn an_unknown_category_field_is_rejected() {
        let source = VENDORED_CRITERIA.replacen("weight = 12", "weigth = 12", 1);
        let error = Canon::parse(&source).expect_err("a misspelt field must not parse");
        assert!(
            format!("{error:#}").contains("unknown category field"),
            "{error:#}"
        );
    }

    #[test]
    fn an_empty_rule_set_is_rejected() {
        let error = Canon::parse("[meta]\nversion = \"2.0.0\"\n").expect_err("no categories");
        assert!(
            format!("{error:#}").contains("no [[category]]"),
            "{error:#}"
        );
    }

    #[test]
    fn a_canon_without_a_version_is_rejected() {
        let source = VENDORED_CRITERIA.replacen("version = \"2.0.0-draft\"", "", 1);
        let error = Canon::parse(&source).expect_err("a versionless canon must not parse");
        assert!(
            format!("{error:#}").contains("no [meta] version"),
            "{error:#}"
        );
    }

    #[test]
    fn a_canon_that_parses_but_is_smaller_than_the_pin_is_caught() {
        // The second line of defence, for a copy that is internally consistent
        // -- it declares its own weights correctly -- but is not the revision
        // the pin describes. A digest comparison alone would also catch this;
        // the counts catch a copy whose digest was updated by hand along with
        // its contents, which is how a rule set quietly loses a category.
        let source = "\
[meta]
version = \"2.0.0-draft\"

[tiers]
bronze = 75
silver = 90
gold = 100
rhodium = 100

[[category]]
id = 1
key = \"only\"
name = \"Only\"
weight = 1
criteria = [
  { id = \"1.1.1\", name = \"n\", desc = \"d\", tier = \"bronze\", gate = \"universal\", detect = \"manual\", template_ref = \"-\" }
]

[weights-check]
sum-declared = 1
";
        let canon = Canon::parse(source).expect("the small canon is internally consistent");
        let pin = Pin::vendored().expect("pin parses");
        let error = pin
            .verify_counts(&canon)
            .expect_err("a smaller rule set must not pass the pin");
        assert!(format!("{error:#}").contains("categories"), "{error:#}");
    }

    #[test]
    fn a_mismatched_pin_is_reported_with_both_hashes() {
        let pin = Pin::vendored().expect("pin parses");
        let error = pin
            .verify(&format!("{VENDORED_CRITERIA}\n# edited"))
            .expect_err("an edited canon must fail its pin");
        let message = format!("{error:#}");
        assert!(message.contains("does not match its pin"), "{message}");
        assert!(message.contains(&pin.source.sha256), "{message}");
        assert!(
            message.contains(&digest_of(&format!("{VENDORED_CRITERIA}\n# edited"))),
            "{message}"
        );
    }

    #[test]
    fn commas_inside_a_description_do_not_split_the_record() {
        let canon = vendored();
        let wellknown = canon
            .criteria()
            .find(|c| c.id == "2.2.1")
            .expect("2.2.1 exists");
        assert_eq!(
            wellknown.desc,
            ".well-known/{security.txt,ai.txt,humans.txt}"
        );
        assert_eq!(wellknown.tier, Tier::Silver);
        assert_eq!(wellknown.gate, "universal");
    }

    #[test]
    fn comments_do_not_truncate_quoted_hashes() {
        assert_eq!(
            strip_comment("sha256 = \"abc#def\"  # trailing"),
            "sha256 = \"abc#def\"  "
        );
        assert_eq!(strip_comment("weight = 12  # twelve"), "weight = 12  ");
    }

    #[test]
    fn universal_criteria_below_gold_are_counted_as_the_canon_states() {
        let canon = vendored();
        let bronze_universal = canon.universal_criteria_up_to(Tier::Bronze).count();
        assert_eq!(
            bronze_universal, 17,
            "universal criteria required at bronze"
        );
    }
}
