// SPDX-License-Identifier: MPL-2.0
//! Sillybot: a thin adapter that runs two pinned external tools and reports
//! docs-consistency findings to the gitbot-fleet report-back driver.
//!
//! - docudactyl `docs-ingest` turns a repository revision into a
//!   reconcile-input/v0 bundle,
//! - recon-silly-ation `reconcile` turns one or two bundles into findings/v0.
//!
//! Nothing of either tool's logic lives here (sillybot spec v0, owner ruling
//! DP5). The adapter only
//! - exports the revisions it scans into a private temporary directory with
//!   `git archive`, so it never reads a dirty work tree and never writes into
//!   the scanned tree ([`runner`]),
//! - maps the tools' failures to a status that is never a pass,
//! - maps findings/v0 to the fleet finding shape ([`translator`]).
//!
//! Known gap: neither tool stamps its source commit in its output, so the
//! pins in `canon/pin.toml` cannot be checked at run time ([`pin`]).

pub mod cli;
pub mod pin;
pub mod runner;
pub mod translator;
