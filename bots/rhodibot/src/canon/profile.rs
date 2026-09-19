// SPDX-License-Identifier: MPL-2.0

//! Which capabilities a repository declares, and whether a criterion applies.
//!
//! # The rule
//!
//! A criterion's `gate` is either `"universal"` -- every repository is scored
//! against it -- or the name of a capability. A gated criterion is scored only
//! when the repository's `.machine_readable/rsr-profile.a2ml` declares that
//! capability; otherwise it is `na` and leaves the denominator rather than
//! counting against the repository.
//!
//! This is not a nicety. Of the canon's 74 criteria, 26 are gated. Asked as
//! universal questions they demand files a repository has no reason to carry:
//! an FFI seam from a docs site, a container from a library. The pilot's
//! finding was that a check must ask the question the canon asks.
//!
//! # Effective set
//!
//! Per `template-capability-gates.toml`, a profile's effective set is
//!
//! ```text
//! (capabilities | preset expansion) + add - remove
//! ```
//!
//! # Fail loudly
//!
//! A misspelt capability is refused, never ignored. `capabilities = ["russt"]`
//! collapsing to an empty set would switch off a whole gate of checks and
//! report the repository as clean -- the exact failure this module exists to
//! prevent. The same goes for an unknown preset, a misspelt key, a preset that
//! expands to nothing, and removing a capability that was never declared.

use std::collections::{BTreeMap, BTreeSet};

use anyhow::{Context, Result, ensure};

use super::{Canon, Criterion, VENDORED_GATES, split_assignment, strip_comment, unquote};

/// The canon's gate table: the capabilities that exist, and what each preset
/// expands to.
///
/// `[baseline]`, `[gates]`, `[carrier]` and `[canon]` in the same file map
/// module paths to capabilities. Those answer a different question -- "is this
/// file legitimately here?" -- and are read by the classification step, not
/// here.
#[derive(Debug)]
pub struct GateTable {
    version: String,
    known: BTreeSet<String>,
    presets: BTreeMap<String, Vec<String>>,
}

impl GateTable {
    /// Parse the gate table.
    pub fn parse(source: &str) -> Result<Self> {
        let mut version = None;
        let mut known: Option<Vec<String>> = None;
        let mut presets: BTreeMap<String, Vec<String>> = BTreeMap::new();

        for field in field_list(source)? {
            match (field.section.as_str(), field.key.as_str()) {
                ("meta", "version") => version = Some(unquote(&field.value, field.line_no)?),
                ("capabilities", "known") => {
                    ensure!(
                        known.is_none(),
                        "line {}: `known` is declared twice",
                        field.line_no
                    );
                    known = Some(parse_string_array(&field.value, field.line_no)?);
                }
                ("presets", name) => {
                    let expanded = parse_string_array(&field.value, field.line_no)?;
                    ensure!(
                        presets.insert(name.to_string(), expanded).is_none(),
                        "line {}: preset {name:?} is declared twice",
                        field.line_no
                    );
                }
                _ => {}
            }
        }

        let version = version.context("the gate table states no [meta] version")?;
        let known = known.context("the gate table declares no [capabilities] known list")?;
        ensure!(!known.is_empty(), "the gate table's `known` list is empty");

        let mut set = BTreeSet::new();
        for name in known {
            ensure!(
                set.insert(name.clone()),
                "capability {name:?} is listed twice in `known`"
            );
        }

        let table = Self {
            version,
            known: set,
            presets,
        };

        for (name, expanded) in &table.presets {
            ensure!(!expanded.is_empty(), "preset {name:?} expands to nothing");
            for capability in expanded {
                ensure!(
                    table.knows(capability),
                    "preset {name:?} expands to {capability:?}, which is not a known capability"
                );
            }
        }

        Ok(table)
    }

    /// The gate table vendored beside the criteria, as pinned by `pin.toml`.
    pub fn vendored() -> Result<Self> {
        Self::parse(VENDORED_GATES)
    }

    pub fn version(&self) -> &str {
        &self.version
    }

    pub fn knows(&self, capability: &str) -> bool {
        self.known.contains(capability)
    }

    pub fn known(&self) -> impl Iterator<Item = &String> {
        self.known.iter()
    }

    pub fn known_count(&self) -> usize {
        self.known.len()
    }

    pub fn preset_count(&self) -> usize {
        self.presets.len()
    }

    pub fn preset(&self, name: &str) -> Option<&[String]> {
        self.presets.get(name).map(Vec::as_slice)
    }

    /// Criteria whose gate names a capability this table does not define.
    ///
    /// Such a criterion can never become applicable: no profile can declare a
    /// capability the vocabulary has no word for. `deno` was added to the table
    /// for exactly this reason -- criterion 4.3.1 gates on it.
    pub fn unknown_gates<'a>(&'a self, canon: &'a Canon) -> Vec<(&'a str, &'a str)> {
        canon
            .criteria()
            .filter_map(|criterion| {
                let capability = criterion.capability()?;
                (!self.knows(capability)).then_some((criterion.id.as_str(), capability))
            })
            .collect()
    }
}

/// What a repository declares about itself in
/// `.machine_readable/rsr-profile.a2ml`.
#[derive(Debug)]
pub struct Profile {
    declared: BTreeSet<String>,
    role: Option<String>,
}

/// The keys `[rsr-profile]` may carry.
///
/// Anything else is refused. `capabilites = ["rust"]` would otherwise parse as
/// a profile declaring nothing, and a repository would be reported clean
/// against a gate of checks that never ran.
const PROFILE_KEYS: &[&str] = &[
    "version",
    "spec",
    "declares-against",
    "role",
    "capabilities",
    "preset",
    "add",
    "remove",
];

impl Profile {
    /// Parse a profile, resolving its preset and validating every capability
    /// against the gate table.
    pub fn parse(source: &str, gates: &GateTable) -> Result<Self> {
        let mut declared_direct: Option<Vec<String>> = None;
        let mut preset: Option<String> = None;
        let mut add: Vec<String> = Vec::new();
        let mut remove: Vec<String> = Vec::new();
        let mut role = None;
        let mut saw_section = false;

        for field in field_list(source)? {
            let in_profile = field.section == "rsr-profile" || field.section.is_empty();
            if !in_profile {
                // [canon] carries the hash binding, [notes] carries prose.
                continue;
            }
            if field.section == "rsr-profile" {
                saw_section = true;
            }

            ensure!(
                PROFILE_KEYS.contains(&field.key.as_str()),
                "line {}: unknown key {:?} in [rsr-profile]; a misspelt key would declare no \
                 capabilities and silently switch off every gated check",
                field.line_no,
                field.key
            );

            match field.key.as_str() {
                "capabilities" => {
                    declared_direct = Some(parse_string_array(&field.value, field.line_no)?);
                }
                "preset" => preset = Some(unquote(&field.value, field.line_no)?),
                "add" => add = parse_string_array(&field.value, field.line_no)?,
                "remove" => remove = parse_string_array(&field.value, field.line_no)?,
                "role" => role = Some(unquote(&field.value, field.line_no)?),
                // version, spec and declares-against record which canon the
                // profile was written against; the hash binding that makes
                // that checkable lives in [canon].
                _ => {}
            }
        }

        ensure!(
            saw_section || declared_direct.is_some() || preset.is_some(),
            "no [rsr-profile] section, capabilities or preset: this is not a profile"
        );

        let mut declared = BTreeSet::new();

        if let Some(name) = &preset {
            let base = gates
                .preset(name)
                .with_context(|| format!("the gate table defines no preset {name:?}"))?;
            declared.extend(base.iter().cloned());
        }

        if let Some(direct) = &declared_direct {
            declared.extend(direct.iter().cloned());
        }

        for capability in &add {
            declared.insert(capability.clone());
        }

        for capability in &remove {
            ensure!(
                declared.remove(capability),
                "line: capability {capability:?} is removed but never declared; removing \
                 something that is not there is a typo, not a no-op"
            );
        }

        for capability in &declared {
            ensure!(
                gates.knows(capability),
                "capability {capability:?} is not in the gate table's `known` list; a misspelt \
                 capability would switch its gated checks off and report the repository as clean"
            );
        }

        Ok(Self { declared, role })
    }

    pub fn declares(&self, capability: &str) -> bool {
        self.declared.contains(capability)
    }

    pub fn declared(&self) -> impl Iterator<Item = &String> {
        self.declared.iter()
    }

    pub fn capability_count(&self) -> usize {
        self.declared.len()
    }

    pub fn role(&self) -> Option<&str> {
        self.role.as_deref()
    }

    /// Whether the canon scores this criterion against this repository.
    pub fn applicability(&self, criterion: &Criterion) -> Applicability {
        match criterion.capability() {
            None => Applicability::Universal,
            Some(capability) if self.declares(capability) => {
                Applicability::Declared(capability.to_string())
            }
            Some(capability) => Applicability::NotDeclared(capability.to_string()),
        }
    }

    pub fn is_applicable(&self, criterion: &Criterion) -> bool {
        self.applicability(criterion).is_applicable()
    }
}

/// A repository's profile-less state: nothing declared, so only the universal
/// criteria are scored.
impl Default for Profile {
    fn default() -> Self {
        Self {
            declared: BTreeSet::new(),
            role: None,
        }
    }
}

/// Why a criterion is or is not scored against a repository.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Applicability {
    /// Names no capability: every repository is scored against it.
    Universal,
    /// Gated on a capability this repository declares.
    Declared(String),
    /// Gated on a capability this repository does not declare. Reported `na`
    /// and excluded from the denominator -- not a finding.
    NotDeclared(String),
}

impl Applicability {
    pub fn is_applicable(&self) -> bool {
        !matches!(self, Self::NotDeclared(_))
    }

    pub fn capability(&self) -> Option<&str> {
        match self {
            Self::Universal => None,
            Self::Declared(capability) | Self::NotDeclared(capability) => Some(capability),
        }
    }

    /// The word a report uses. `na` is the canon's term for a criterion that
    /// does not apply.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Universal => "universal",
            Self::Declared(_) => "declared",
            Self::NotDeclared(_) => "na",
        }
    }
}

/// One `key = value` line of a table, with its section and where it was found.
struct Field {
    section: String,
    key: String,
    /// The value with comments stripped, and with any list continuation lines
    /// joined onto it.
    value: String,
    line_no: usize,
}

/// Read a TOML-ish table line by line.
///
/// Only what this module needs: section headers, `key = value`, string values,
/// string arrays, and arrays that run over several lines -- `known` does, and
/// so does the spine's own capability list. Anything else in the file is
/// skipped rather than refused: the gate table is the canon's, and this is a
/// consumer of it, not its parser of record.
fn field_list(source: &str) -> Result<Vec<Field>> {
    let lines: Vec<&str> = source.lines().collect();
    let mut fields = Vec::new();
    let mut section = String::new();
    let mut index = 0;

    while index < lines.len() {
        let line_no = index + 1;
        let line = strip_comment(lines[index]).trim();

        if line.is_empty() {
            index += 1;
            continue;
        }

        if line.starts_with('[') && line.ends_with(']') {
            section = line.trim_matches(['[', ']']).trim().to_string();
            index += 1;
            continue;
        }

        if let Some((key, value)) = split_assignment(line) {
            let mut value = value.to_string();

            if value.starts_with('[') && !brackets_balanced(&value) {
                loop {
                    index += 1;
                    ensure!(
                        index < lines.len(),
                        "line {line_no}: {key:?} has no closing `]`"
                    );
                    value.push(' ');
                    value.push_str(strip_comment(lines[index]).trim());
                    if brackets_balanced(&value) {
                        break;
                    }
                }
            }

            fields.push(Field {
                section: section.clone(),
                key: key.to_string(),
                value,
                line_no,
            });
        }

        index += 1;
    }

    Ok(fields)
}

fn brackets_balanced(text: &str) -> bool {
    let mut depth = 0i32;
    let mut in_string = false;
    for ch in text.chars() {
        match ch {
            '"' => in_string = !in_string,
            '[' if !in_string => depth += 1,
            ']' if !in_string => depth -= 1,
            _ => {}
        }
    }
    depth <= 0
}

/// Parse `[ "a", "b", ]` into its quoted strings. A trailing comma is fine;
/// anything unquoted is refused.
fn parse_string_array(value: &str, line_no: usize) -> Result<Vec<String>> {
    let inner = value
        .strip_prefix('[')
        .and_then(|rest| rest.strip_suffix(']'))
        .with_context(|| format!("line {line_no}: expected a [ ... ] list, found {value:?}"))?;

    let mut names = Vec::new();
    for piece in inner.split(',') {
        let piece = piece.trim();
        if piece.is_empty() {
            continue;
        }
        names.push(unquote(piece, line_no)?);
    }
    Ok(names)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::canon::VENDORED_CRITERIA;

    fn gates() -> GateTable {
        GateTable::vendored().expect("the vendored gate table parses")
    }

    fn canon() -> Canon {
        Canon::vendored().expect("the vendored canon parses")
    }

    /// The capability list the spine declares, verbatim from
    /// `rsr-template-repo/.machine_readable/rsr-profile.a2ml`.
    const SPINE_PROFILE: &str = "\
[rsr-profile]
version = \"1.0.0\"
spec = \"rsr-criteria-v2\"
declares-against = \"2.0.0-draft\"
role = \"spine\"
capabilities = [
  \"zig\",                # src/interface/ffi/build.zig + Zig FFI layer
  \"idris2\",             # src/interface/abi.ipkg + src/interface/Abi/
  \"bash\",               # scripts/*.sh automation
  \"cli\",                # src/ command-line entry scaffolding
  \"library\",            # consumed/published as a library surface
  \"ffi\",                # C-ABI FFI seam (Zig), src/interface/ffi/
  \"abi\",                # formally specified ABI
  \"formal-proofs\",      # verification/proofs/
  \"docs-site\",          # .github/workflows/pages.yml (casket/Pages)
  \"container\",          # build/container/ + .devcontainer/Containerfile
  \"reproducible-build\", # build/guix.scm
  \"benchmarks\",         # benches/
  \"governance-tier\",    # AUDIT/AFFIRMATION/GOVERNANCE/MAINTAINERS
]

[canon]
version = \"2.0.4\"
";

    #[test]
    fn the_vendored_gate_table_has_the_expected_shape() {
        let table = gates();
        assert_eq!(table.version(), "0.2.0");
        assert_eq!(
            table.known_count(),
            28,
            "the capability vocabulary changed; profiles declaring the new words would be \
             refused, and criteria gated on them could never apply"
        );
        assert_eq!(table.preset_count(), 11);
        assert!(
            table.knows("deno"),
            "criterion 4.3.1 gates on `deno`; without it in `known` that criterion can never \
             become applicable"
        );
    }

    #[test]
    fn every_gate_the_canon_uses_is_a_known_capability() {
        let table = gates();
        let canon = canon();
        let unknown = table.unknown_gates(&canon);
        assert!(
            unknown.is_empty(),
            "these criteria gate on capabilities the table does not define: {unknown:?}"
        );

        // The check has to be able to fail, or it proves nothing. Doctor the
        // *canon* -- gating a criterion on a word the vocabulary does not have
        // -- because that is the drift the check exists to catch: a canon that
        // gates on a capability nobody can declare.
        let drifted =
            Canon::parse(&VENDORED_CRITERIA.replacen("gate = \"rust\"", "gate = \"rust-2024\"", 1))
                .expect("a canon gating on an unknown capability still parses");
        assert_eq!(
            table.unknown_gates(&drifted),
            vec![("5.2.1", "rust-2024")],
            "the canon's rust-gated criterion must be found"
        );
    }

    #[test]
    fn the_spine_profile_parses_to_its_declared_set() {
        let profile = Profile::parse(SPINE_PROFILE, &gates()).expect("the spine's profile parses");
        assert_eq!(profile.capability_count(), 13);
        assert_eq!(profile.role(), Some("spine"));
        assert!(profile.declares("zig") && profile.declares("governance-tier"));
        assert!(
            !profile.declares("rust"),
            "the spine carries no Rust; its [notes] say so, and scoring it against the Rust \
             gate is the mistake the gate table's [carrier] section exists to correct"
        );
    }

    #[test]
    fn a_preset_expands_to_its_base_capabilities() {
        let profile = Profile::parse(
            "[rsr-profile]\nrole = \"spine\"\npreset = \"rust-cli\"\n",
            &gates(),
        )
        .expect("a preset parses");
        let declared: Vec<&str> = profile.declared().map(String::as_str).collect();
        assert_eq!(declared, ["cli", "library", "rust"]);
    }

    #[test]
    fn add_and_remove_adjust_a_preset() {
        let profile = Profile::parse(
            "[rsr-profile]\npreset = \"rust-cli\"\nadd = [\"container\",]\nremove = [\"library\"]\n",
            &gates(),
        )
        .expect("preset + add - remove parses");
        let declared: Vec<&str> = profile.declared().map(String::as_str).collect();
        assert_eq!(declared, ["cli", "container", "rust"]);
    }

    #[test]
    fn a_preset_and_a_direct_list_are_combined() {
        let profile = Profile::parse(
            "[rsr-profile]\npreset = \"docs-site\"\ncapabilities = [\"bash\"]\n",
            &gates(),
        )
        .expect("preset plus a direct list parses");
        let declared: Vec<&str> = profile.declared().map(String::as_str).collect();
        assert_eq!(declared, ["bash", "docs-site"]);
    }

    #[test]
    fn a_misspelt_capability_is_refused() {
        let error = Profile::parse("[rsr-profile]\ncapabilities = [\"russt\"]\n", &gates())
            .expect_err("a capability that is not in the vocabulary must be refused");
        let message = format!("{error:#}");
        assert!(
            message.contains("russt") && message.contains("clean"),
            "the refusal must name the token and say why it matters: {message}"
        );
    }

    #[test]
    fn an_unknown_preset_is_refused() {
        let error = Profile::parse("[rsr-profile]\npreset = \"rust-clli\"\n", &gates())
            .expect_err("a preset that does not exist must be refused");
        assert!(format!("{error:#}").contains("rust-clli"), "{error:#}");
    }

    #[test]
    fn a_preset_naming_an_unknown_capability_is_refused() {
        // A hand-written table, so the test does not depend on which presets
        // the canon happens to carry.
        let text = "\
[meta]
version = \"0.0.0\"

[capabilities]
known = [\"rust\"]

[presets]
rust-cli = [\"rust\", \"cli\"]
";
        let error = GateTable::parse(text).expect_err("a preset may not invent a capability");
        let message = format!("{error:#}");
        assert!(
            message.contains("rust-cli") && message.contains("cli"),
            "{message}"
        );
    }

    #[test]
    fn a_misspelt_key_is_refused() {
        let error = Profile::parse("[rsr-profile]\ncapabilites = [\"rust\"]\n", &gates())
            .expect_err("a misspelt key must not parse as a profile declaring nothing");
        let message = format!("{error:#}");
        assert!(
            message.contains("capabilites") && message.contains("unknown key"),
            "the refusal must name the key it did not recognise: {message}"
        );
    }

    #[test]
    fn removing_a_capability_that_was_never_declared_is_refused() {
        let error = Profile::parse(
            "[rsr-profile]\ncapabilities = [\"bash\"]\nremove = [\"rust\"]\n",
            &gates(),
        )
        .expect_err("removing something absent is a typo, not a no-op");
        assert!(format!("{error:#}").contains("never declared"), "{error:#}");
    }

    #[test]
    fn a_file_that_is_not_a_profile_is_refused() {
        let error = Profile::parse("[notes]\n# prose only\n", &gates())
            .expect_err("a file with no profile content must not parse as an empty one");
        assert!(format!("{error:#}").contains("not a profile"), "{error:#}");
    }

    #[test]
    fn a_gated_criterion_applies_only_when_its_capability_is_declared() {
        let canon = canon();
        let rust_gated = canon
            .criteria()
            .find(|criterion| criterion.capability() == Some("rust"))
            .expect("the canon gates at least one criterion on rust");
        let universal = canon
            .criteria()
            .find(|criterion| criterion.capability().is_none())
            .expect("the canon has universal criteria");

        let declared = Profile::parse("[rsr-profile]\ncapabilities = [\"rust\"]\n", &gates())
            .expect("a profile declaring rust");
        assert_eq!(
            declared.applicability(rust_gated),
            Applicability::Declared("rust".to_string())
        );
        assert!(declared.is_applicable(rust_gated));

        // No profile at all: the repository has declared nothing, so every
        // gated criterion is `na` -- not a finding.
        let undeclared = Profile::default();
        assert_eq!(
            undeclared.applicability(rust_gated),
            Applicability::NotDeclared("rust".to_string())
        );
        assert_eq!(undeclared.applicability(rust_gated).as_str(), "na");
        assert!(!undeclared.is_applicable(rust_gated));

        // A universal criterion is scored either way.
        assert_eq!(
            undeclared.applicability(universal),
            Applicability::Universal
        );
        assert_eq!(declared.applicability(universal), Applicability::Universal);
        assert!(undeclared.is_applicable(universal));
    }
}
