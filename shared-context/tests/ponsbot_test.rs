// SPDX-License-Identifier: MPL-2.0
//! `BotId::Ponsbot` is known to the fleet but not yet dispatched by it.

use gitbot_shared_context::{BotId, Tier};

#[test]
fn ponsbot_round_trips_through_its_name() {
    assert_eq!(BotId::Ponsbot.to_string(), "ponsbot");
    assert_eq!(BotId::from_str("ponsbot"), Some(BotId::Ponsbot));
    assert_eq!(BotId::from_str("Pons-Bot"), Some(BotId::Ponsbot));
    assert_eq!(BotId::Ponsbot.tier(), Tier::Verifier);
}

#[test]
fn ponsbot_is_deliberately_not_in_all() {
    // The adapter has the report-back `scan` mode only; registering it in all()
    // would make verifier-tier completion wait on a bot the coordinator cannot
    // run. Add it here and to all() together with its fleet dispatch.
    assert!(!BotId::all().contains(&BotId::Ponsbot));
    assert!(!Tier::Verifier.bots().contains(&BotId::Ponsbot));
}
