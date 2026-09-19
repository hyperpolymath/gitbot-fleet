// SPDX-License-Identifier: MPL-2.0

//! Rhodibot - RSR Compliance Bot library
//!
//! Provides the RSR compliance checking engine, GitHub REST and GraphQL
//! clients, GitHub App authentication, and webhook handling.
//!
//! # Security
//!
//! All webhook inputs are validated via the [`sanitize`] module before
//! being used in API calls or markdown output. Tokens are never logged.

#![forbid(unsafe_code)]
pub mod app_auth;
pub mod canon;
pub mod config;
pub mod fleet;
pub mod github;
pub mod graphql;
pub mod rsr;
pub mod sanitize;
pub mod webhook;
