// SPDX-License-Identifier: PMPL-1.0-or-later
#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // Fuzz Finding struct creation from arbitrary data
    if let Ok(s) = std::str::from_utf8(data) {
        // Simulate finding creation with fuzzed input
        let parts: Vec<&str> = s.split('|').collect();
        if parts.len() >= 4 {
            // Create findings with fuzzed data
            use finishingbot::analyzers::{Finding, Severity};
            let _finding = Finding::new(
                parts[0],
                parts[1],
                Severity::Info,
                parts[2],
            );
        }
    }
});
