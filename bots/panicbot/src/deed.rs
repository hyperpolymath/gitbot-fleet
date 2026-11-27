// SPDX-License-Identifier: MPL-2.0
//! The repo deed (`<repo>_chora.deed`) reader.
//!
//! This module was panicbot's own strict reader; it now lives, unchanged in
//! behaviour, in the shared std-only crate `deed-reader/` (gitbot-deed-reader)
//! so every bot reads the deed one way. One tightening came with the move: a
//! bare CR (not part of CRLF) is an error, as deed.abnf `line-end` requires.
//! `deed_lint` in standards stays the conformance gate.

pub use deed_reader::{parse, Clause, DeedError, Item, Result, Value};
