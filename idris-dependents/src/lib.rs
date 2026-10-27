// SPDX-License-Identifier: MPL-2.0
//! idris-dependents — find people outside your own accounts whose Idris2
//! projects depend on your packages, using GitHub code search (GET only).
//!
//! The parsing and classification rules are pure functions so they can be
//! tested without the network; [`scan::run`] does the search walk through the
//! [`scan::Io`] trait.

pub mod catalogue;
pub mod ipkg;
pub mod refs;
pub mod scan;
