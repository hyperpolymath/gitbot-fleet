// SPDX-License-Identifier: PMPL-1.0-or-later

//! Rhodibot - RSR Compliance Bot library
//!
//! Provides the RSR compliance checking engine, GitHub API client,
//! and webhook handling for the rhodibot GitHub App.
//!
//! # Security
//!
//! All webhook inputs are validated via the [`sanitize`] module before
//! being used in API calls or markdown output. Tokens are never logged.

pub mod config;
pub mod fleet;
pub mod github;
pub mod rsr;
pub mod sanitize;
pub mod webhook;
