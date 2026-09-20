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

use rhodibot::canon::{Canon, Pin, Tier, VENDORED_CRITERIA, digest_of};

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
    assert_eq!(canon.criterion_count(), 74, "criteria in total");
    assert_eq!(
        canon.weight_sum(),
        88,
        "category weights, as the canon states"
    );

    let bronze = canon.criteria().filter(|c| c.tier == Tier::Bronze).count();
    let gold = canon.criteria().filter(|c| c.tier == Tier::Gold).count();
    let rhodium = canon.criteria().filter(|c| c.tier == Tier::Rhodium).count();
    assert_eq!((bronze, gold, rhodium), (17, 29, 4));

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
    assert_eq!(gated, 26);

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
        derived.len() < canon.criterion_count(),
        "some criteria name no file at all"
    );
    assert_eq!(
        derived.len(),
        31,
        "file-presence requirements derived from criterion descriptions"
    );
}
