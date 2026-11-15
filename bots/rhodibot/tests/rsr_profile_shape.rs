// SPDX-License-Identifier: MPL-2.0

//! The `(rsr-profile ...)` clause as it really is in the estate: a verbatim
//! copy of the clause from `metadatastician/standards`' own chora deed
//! (`tests/fixtures/standards-rsr-profile.fragment`), with its comments,
//! nested `(notes ...)` and `:omitted-rationale`.

use rhodibot::canon::profile::{GateTable, Profile};
use rhodibot::deed::Deed;

#[test]
fn the_real_clause_parses_and_resolves_against_the_gate_table() {
    let fragment = include_str!("fixtures/standards-rsr-profile.fragment");
    let deed = Deed::parse(&format!("(repo-deed\n{fragment}\n)")).expect("the deed parses");
    let clause = deed.rsr_profile().expect("reads").expect("present");
    assert_eq!(clause.role.as_deref(), Some("canon"));
    assert_eq!(clause.capabilities.as_ref().map(Vec::len), Some(5));

    let profile = Profile::from_clause(clause, &GateTable::vendored().unwrap()).expect("resolves");
    for capability in [
        "bash",
        "docs-site",
        "governance-tier",
        "reproducible-build",
        "rust",
    ] {
        assert!(profile.declares(capability), "{capability}");
    }
    assert_eq!(profile.capability_count(), 5);
}
