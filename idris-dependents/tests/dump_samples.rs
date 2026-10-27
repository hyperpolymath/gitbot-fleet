// SPDX-License-Identifier: MPL-2.0
//! Writes generated cases to $DUMP_IPKG_DIR (used to check the generator's power
//! and to feed the Idris2 oracle by hand). Does nothing when the variable is unset.

mod common;

use proptest::strategy::{Strategy, ValueTree};
use proptest::test_runner::TestRunner;

#[test]
fn dump_samples() {
    let Ok(dir) = std::env::var("DUMP_IPKG_DIR") else {
        return;
    };
    let n: usize = std::env::var("DUMP_IPKG_N")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(300);
    std::fs::create_dir_all(&dir).unwrap();
    let mut runner = TestRunner::deterministic();
    for i in 0..n {
        let p = common::ipkg().new_tree(&mut runner).unwrap().current();
        std::fs::write(format!("{dir}/case{i:04}.ipkg"), &p.text).unwrap();
        std::fs::write(
            format!("{dir}/case{i:04}.json"),
            serde_json::json!({"package": p.name, "depends": p.deps}).to_string(),
        )
        .unwrap();
    }
}
