// SPDX-License-Identifier: PMPL-1.0-or-later
//! Panicbot — Targeted Audit Bot for gitbot-fleet
//!
//! Panicbot wraps the `panic-attack` CLI tool as a subprocess, translates its
//! `WeakPoint` findings into gitbot-fleet `Finding` structs, and produces
//! per-repo A2ML debt registers for unfixable issues.
//!
//! ## Architecture
//!
//! ```text
//! panic-attack assail <repo> --output-format json
//!       │
//!       ▼
//!   scanner.rs (subprocess wrapper, JSON parsing)
//!       │
//!       ▼
//!   translator.rs (WeakPoint → Finding, confidence/tier mapping)
//!       │
//!       ├── fixable → FindingSet (fleet pipeline → Hypatia → auto-fix)
//!       │
//!       └── unfixable → a2ml_writer.rs → .panicbot/PANICBOT-FINDINGS.a2ml
//! ```
//!
//! ## Scheduling
//!
//! | Mode | Frequency | Scope |
//! |------|-----------|-------|
//! | Per-PR | Every push | Single repo (via hypatia-scan.yml) |
//! | On-demand | Manual | Single repo (fleet-coordinator.sh) |
//! | Weekly sweep | Cron | All repos (panicbot-sweep.yml) |
//!
//! ## Bot Modes
//!
//! - **Advisor** (default): Report findings, don't block.
//! - **Auditor**: Full suite including adjudicate and diagnostics.
//! - **Guardian**: Block PRs on critical findings.

pub mod a2ml_writer;
pub mod cli;
pub mod config;
pub mod directives;
pub mod fleet;
pub mod scanner;
pub mod translator;
