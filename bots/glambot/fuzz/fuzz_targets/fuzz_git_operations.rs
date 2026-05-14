// SPDX-License-Identifier: PMPL-1.0-or-later
#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // Fuzz git command parsing/operations
    if let Ok(s) = std::str::from_utf8(data) {
        let _ = s.parse::<String>();
        // Add actual vext git operation fuzzing here
        // Example: vext_core::parse_git_ref(s);
    }
});
