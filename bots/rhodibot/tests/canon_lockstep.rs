// SPDX-License-Identifier: MPL-2.0

//! The canon lockstep, exercised from outside the crate.
//!
//! The unit tests in `src/canon.rs` check the parser against the canon. These
//! check the *binding*: that the copy in the repository is the revision the pin
//! names, that the copy a binary would apply is the same file, and that the rule
//! set has the shape the pilot's numbers are quoted from.
//!
//! The last test is the one to update deliberately rather than accidentally: it
//! records what the rule set contains, so a canon change that shrinks it fails
//! here instead of producing a quieter scorecard somewhere downstream.

use std::path::PathBuf;

use rhodibot::canon::profile::{Applicability, GateTable, Profile};
use rhodibot::canon::requirement::requirement_from;
use rhodibot::canon::verdict::{Deprecation, GroupVerdict, Severity, Verdict};
use rhodibot::canon::{Canon, Pin, Tier, VENDORED_CRITERIA, VENDORED_GATES, digest_of};

/// The vendored canon as it sits on disk.
fn canon_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("canon/rsr-criteria-v2.a2ml")
}

#[test]
fn the_file_on_disk_is_the_copy_compiled_into_the_binary() {
    let on_disk = std::fs::read_to_string(canon_path()).expect("the vendored canon is readable");

    // If these differ, a binary would apply one rule set and `Canon::load`
    // another -- the two would agree on the digest and disagree on the rules.
    assert_eq!(
        digest_of(&on_disk),
        digest_of(VENDORED_CRITERIA),
        "the vendored file and the embedded copy have diverged"
    );

    let from_disk = Canon::load(canon_path()).expect("the file parses");
    let embedded = Canon::vendored().expect("the embedded copy parses");
    assert_eq!(from_disk.criterion_count(), embedded.criterion_count());
    assert_eq!(from_disk.weight_sum(), embedded.weight_sum());
}

#[test]
fn the_pin_describes_the_vendored_copy() {
    let pin = Pin::vendored().expect("the pin parses");
    pin.verify(VENDORED_CRITERIA).expect("digest matches");
    pin.verify_counts(&Canon::vendored().expect("parses"))
        .expect("shape matches");
}

#[test]
fn the_pin_refuses_a_copy_that_changed() {
    let pin = Pin::vendored().expect("the pin parses");
    // One keystroke: an apostrophe in a description is enough to change what
    // every repository is measured against.
    let edited = VENDORED_CRITERIA.replacen(".gitignore and .gitattributes present", "x", 1);

    let error = pin
        .verify(&edited)
        .expect_err("an edited canon must be refused");
    assert!(format!("{error:#}").contains("does not match its pin"));
}

#[test]
fn the_rule_set_the_pilot_is_quoted_from() {
    let canon = Canon::vendored().expect("the canon parses");

    assert_eq!(canon.categories.len(), 11, "weighted categories");
    assert_eq!(canon.criterion_count(), 78, "criteria in total");
    assert_eq!(
        canon.weight_sum(),
        88,
        "category weights, as the canon states"
    );

    let bronze = canon.criteria().filter(|c| c.tier == Tier::Bronze).count();
    let gold = canon.criteria().filter(|c| c.tier == Tier::Gold).count();
    let rhodium = canon.criteria().filter(|c| c.tier == Tier::Rhodium).count();
    assert_eq!((bronze, gold, rhodium), (18, 30, 4));

    // The honest half of the coverage story: the canon says outright that these
    // have no automated detection. A tool that reported them as passing would
    // be inventing results.
    assert_eq!(canon.criteria().filter(|c| c.is_manual()).count(), 19);

    // Capability-gated criteria apply only where a repository declares the
    // capability, so they are excluded from a universal denominator rather than
    // counted as failures.
    let gated = canon
        .criteria()
        .filter(|c| c.capability().is_some())
        .count();
    assert_eq!(gated, 30);

    // What a repository with no declared capabilities is measured on at bronze.
    assert_eq!(canon.universal_criteria_up_to(Tier::Bronze).count(), 17);
}

#[test]
fn requirements_are_derivable_from_descriptions() {
    use rhodibot::canon::requirement::requirement_from;

    let canon = Canon::vendored().expect("the canon parses");
    let mut derived = Vec::new();

    for criterion in canon.criteria() {
        if let Some(requirement) = requirement_from(&criterion.desc) {
            derived.push(format!(
                "  {:<7} {:<8} {}",
                criterion.id,
                criterion.tier,
                requirement.paths().cloned().collect::<Vec<_>>().join(" | ")
            ));
        }
    }

    for line in &derived {
        println!("{line}");
    }
    println!(
        "filed presence requirements: {} of {} criteria",
        derived.len(),
        canon.criterion_count()
    );

    assert!(
        derived.len() > 20 && derived.len() < canon.criterion_count(),
        "some criteria name files and some do not; {} derived looks wrong",
        derived.len()
    );
}

/// The vendored gate table as it sits on disk.
fn gates_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("canon/template-capability-gates.toml")
}

#[test]
fn the_gate_table_on_disk_is_the_copy_compiled_into_the_binary() {
    let on_disk =
        std::fs::read_to_string(gates_path()).expect("the vendored gate table is readable");

    // Same failure as the criteria: a binary would ask one vocabulary and the
    // file would answer another.
    assert_eq!(
        digest_of(&on_disk),
        digest_of(VENDORED_GATES),
        "the vendored gate table and the embedded copy have diverged"
    );

    let from_disk = GateTable::parse(&on_disk).expect("the file parses");
    let embedded = GateTable::vendored().expect("the embedded copy parses");
    assert_eq!(from_disk.known_count(), embedded.known_count());
    assert_eq!(from_disk.preset_count(), embedded.preset_count());
    assert_eq!(from_disk.version(), embedded.version());
}

#[test]
fn the_pin_describes_the_vendored_gate_table() {
    let pin = Pin::vendored().expect("the pin parses");
    pin.verify_gates(VENDORED_GATES).expect("digest matches");
    pin.verify_gate_counts(&GateTable::vendored().expect("parses"))
        .expect("shape matches");
}

#[test]
fn the_gate_pin_refuses_a_table_that_changed() {
    let pin = Pin::vendored().expect("the pin parses");
    // Removing one capability is enough: a profile declaring it would stop
    // parsing, and criteria gated on it could never apply again.
    let edited = VENDORED_GATES.replacen("\"plugin\",", "", 1);

    let error = pin
        .verify_gates(&edited)
        .expect_err("an edited gate table must be refused");
    assert!(format!("{error:#}").contains("does not match its pin"));
}

#[test]
fn capability_gates_split_the_file_presence_criteria() {
    let canon = Canon::vendored().expect("the canon parses");
    let profile = Profile::default();

    let mut scored = Vec::new();
    let mut inapplicable = Vec::new();

    for criterion in canon.criteria() {
        if requirement_from(&criterion.desc).is_none() {
            continue;
        }
        match profile.applicability(criterion) {
            Applicability::Universal => scored.push(criterion.id.clone()),
            Applicability::NotDeclared(capability) => {
                inapplicable.push((criterion.id.clone(), capability));
            }
            Applicability::Declared(capability) => {
                unreachable!("nothing is declared, so {capability} cannot be")
            }
        }
    }

    println!("with no profile -- the pilot's five repositories:");
    println!("  scored     {}", scored.join(" "));
    for (id, capability) in &inapplicable {
        println!("  na         {id} (needs {capability})");
    }
    println!(
        "scored {}  na {}  of {} derived",
        scored.len(),
        inapplicable.len(),
        scored.len() + inapplicable.len()
    );

    assert_eq!(
        scored.len() + inapplicable.len(),
        33,
        "the number of criteria whose description names files changed"
    );
    assert!(
        !inapplicable.is_empty(),
        "the canon gates some file-presence criteria; a check that scored them anyway would \
         demand files these repositories have no reason to carry"
    );
}

/// The classifier against a real repository the advisory pilot went through by
/// hand. The paths below are nesy-solver's at `main` on 2026-09-19, read from
/// the GitHub tree API -- not invented for the test.
///
/// The pilot recorded three findings for this repository, all of them
/// descriptive files left under the location the canon retired. This asserts
/// the classifier reaches the same three, and that it does not invent a fourth
/// where the repository is mid-move.
#[test]
fn a_real_repository_classifies_the_way_the_pilot_recorded_it() {
    let canon = Canon::vendored().expect("the canon parses");
    let deprecations = Deprecation::from_canon(&canon).expect("the canon's retirement is readable");
    let profile = Profile::default();

    let files: Vec<String> = [
        // The move's destination.
        ".machine_readable/STATE.a2ml",
        ".machine_readable/META.a2ml",
        ".machine_readable/ECOSYSTEM.a2ml",
        // The move's source, still populated.
        ".machine_readable/6a2/STATE.a2ml",
        ".machine_readable/6a2/META.a2ml",
        ".machine_readable/6a2/ECOSYSTEM.a2ml",
        ".machine_readable/6a2/AGENTIC.a2ml",
        ".machine_readable/6a2/NEUROSYM.a2ml",
        ".machine_readable/6a2/PLAYBOOK.a2ml",
        // Everything else the eleven pilot criteria ask about.
        ".editorconfig",
        ".pre-commit-config.yaml",
        ".well-known/security.txt",
        ".well-known/ai.txt",
        ".well-known/humans.txt",
        "0-AI-MANIFEST.a2ml",
        "Justfile",
        ".machine_readable/anchors/ANCHOR.a2ml",
    ]
    .iter()
    .map(|path| path.to_string())
    .collect();

    let verdict_of = |id: &str| {
        let criterion = canon
            .criteria()
            .find(|criterion| criterion.id == id)
            .unwrap_or_else(|| panic!("the canon has a criterion {id}"));
        assert!(profile.is_applicable(criterion), "{id} is not universal");
        Verdict::of(criterion, &files, &deprecations).unwrap_or_else(|| panic!("{id} names files"))
    };

    // The three the pilot recorded: present, but only under the retired path.
    for id in ["3.1.5", "3.1.6", "3.1.7"] {
        let verdict = verdict_of(id);
        assert_eq!(verdict.severity(), Severity::Deprecated, "{id}");
        match &verdict.groups[0] {
            GroupVerdict::Deprecated {
                found, location, ..
            } => {
                assert_eq!(location, ".machine_readable/6a2/");
                assert!(found.starts_with(".machine_readable/6a2/"), "{found}");
            }
            other => panic!("{id}: expected Deprecated, got {other:?}"),
        }
    }

    // The three that moved out of the retired directory but not yet into the
    // canon's: relocated, with the leftover named rather than counted as the
    // repository's location.
    for id in ["3.1.2", "3.1.3", "3.1.4"] {
        let verdict = verdict_of(id);
        assert_eq!(verdict.severity(), Severity::Relocated, "{id}");
        assert_eq!(
            verdict.deprecated_copies().len(),
            1,
            "{id} left a copy behind"
        );
    }

    // The pilot's own false positive: 1.2.2 is satisfied by a root
    // .pre-commit-config.yaml even though the canon's template keeps one in ci/.
    assert_eq!(verdict_of("1.2.2").severity(), Severity::Satisfied);

    // 1.2.4 is genuinely absent here.
    assert_eq!(verdict_of("1.2.4").severity(), Severity::Missing);

    // The eleven criteria the pilot asked, three ways.
    let mut counts = std::collections::BTreeMap::new();
    for id in [
        "1.1.4", "1.2.2", "1.2.4", "2.3.1", "3.1.2", "3.1.3", "3.1.4", "3.1.5", "3.1.6", "3.1.7",
        "3.1.8",
    ] {
        *counts.entry(verdict_of(id).severity()).or_insert(0) += 1;
    }
    println!("nesy-solver, the pilot's eleven: {counts:?}");
    assert_eq!(
        (
            counts.get(&Severity::Satisfied).copied().unwrap_or(0),
            counts.get(&Severity::Relocated).copied().unwrap_or(0),
            counts.get(&Severity::Deprecated).copied().unwrap_or(0),
            counts.get(&Severity::Missing).copied().unwrap_or(0)
        ),
        // satisfied: .editorconfig, the root .pre-commit-config.yaml, the
        // AI manifest. relocated: the three that left 6a2/ for
        // .machine_readable/, and ANCHOR.a2ml at .machine_readable/anchors/.
        // deprecated: the three still only under 6a2/. missing: .tool-versions.
        (3, 4, 3, 1),
        "the pilot's eleven, reclassified by the canon's own words"
    );
}
