// SPDX-License-Identifier: MPL-2.0
//! Rule IDs must be panic-attack's, not a second numbering.
//!
//! `canon/canonical-rule-ids.json` is panic-attack's published rule table
//! (`data/canonical-rule-ids.json`), copied verbatim and pinned by digest in
//! `canon/pin.toml` (checked by `tests/panic-attack-rule-ids-test.sh`). Hypatia deduplicates findings by these IDs, so a
//! translator that numbers categories differently files every finding under
//! the wrong rule.

use panicbot::translator::category_mapping;
use serde_json::Value;

const TABLE: &str = include_str!("../canon/canonical-rule-ids.json");

/// Categories panic-attack defines that panicbot does not translate yet.
/// They fall through to the generic PA000 finding. Mapping one is a policy
/// call (tier, confidence, fixability) — and `LicensePolicy` findings are
/// flag-only under estate policy — so each entry here is deliberate.
const NOT_YET_MAPPED: &[&str] = &[
    "UnboundedAllocation",
    "LicensePolicy",
    "Advisory",
    "BannedDep",
    "UnknownSource",
];

fn rules() -> Vec<(String, String)> {
    let table: Value = serde_json::from_str(TABLE).expect("vendored table is JSON");
    table["rules"]
        .as_array()
        .expect("rules array")
        .iter()
        .map(|r| {
            (
                r["category"].as_str().expect("category").to_string(),
                r["rule_id"].as_str().expect("rule_id").to_string(),
            )
        })
        .collect()
}

#[test]
fn every_mapped_category_uses_the_canonical_rule_id() {
    for (category, rule_id) in rules() {
        match category_mapping(&category) {
            Some(m) => assert_eq!(m.rule_id, rule_id, "{category}"),
            None => assert!(
                NOT_YET_MAPPED.contains(&category.as_str()),
                "{category} ({rule_id}) is in panic-attack's table but neither \
                 mapped nor listed in NOT_YET_MAPPED"
            ),
        }
    }
}

#[test]
fn not_yet_mapped_names_only_real_unmapped_categories() {
    let categories: Vec<String> = rules().into_iter().map(|(c, _)| c).collect();
    for name in NOT_YET_MAPPED {
        assert!(
            categories.iter().any(|c| c == name),
            "{name} is not a panic-attack category"
        );
        assert!(
            category_mapping(name).is_none(),
            "{name} is mapped; drop it from NOT_YET_MAPPED"
        );
    }
}
