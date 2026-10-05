// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Double-loop feedback: proof-history → tactic-selection reweighting,
//! successful-proof → corpus-delta pipeline.
//!
//! This module is the *local* half of the double loop — consulting the
//! `tactic_outcomes` store to reweight ECHIDNA's ML-produced suggestions —
//! plus the corpus-delta trigger that feeds successful proofs back to
//! `echidna/training_data/` for the retrainer (Package 5 / `just corpus-refresh`).

pub mod corpus_delta;
pub mod reranker;

pub use corpus_delta::{CorpusDelta, DeltaRow, DeltaSource, RefreshStatus};
pub use reranker::Reranker;
