// SPDX-License-Identifier: MPL-2.0
//! Ponsbot: a thin adapter that runs `pons` (pons asinorum) as a pinned
//! subprocess and reports its findings to the gitbot-fleet report-back driver.
//!
//! pons stays an external engine (owner ruling D0, 2026-11-12); nothing of its
//! analysis lives here. The adapter only
//! - finds and runs the pons binary, never writing into the scanned tree,
//! - maps pons' exit codes to a status that is never a pass ([`scanner`]),
//! - carries each finding's tier and evidence class ([`translator`]).

pub mod cli;
pub mod pin;
pub mod scanner;
pub mod translator;
