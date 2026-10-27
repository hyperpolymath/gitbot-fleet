// SPDX-License-Identifier: MPL-2.0
//! Property: printing a random well-formed `.ipkg` and parsing it back
//! recovers exactly the package name and dependency list that were printed,
//! whatever comments, line endings and comma styles surround them.

mod common;

use idris_dependents::ipkg::{depends, package_name};
use proptest::prelude::*;

proptest! {
    #![proptest_config(ProptestConfig::with_cases(2000))]

    #[test]
    fn depends_round_trip(p in common::ipkg()) {
        prop_assert_eq!(depends(&p.text), p.deps, "file:\n{}", p.text);
        prop_assert_eq!(package_name(&p.text), Some(p.name), "file:\n{}", p.text);
    }

    #[test]
    fn lexer_is_total(s in "\\PC{0,200}") {
        // Arbitrary input never panics, whatever it contains.
        let _ = depends(&s);
        let _ = package_name(&s);
    }
}
