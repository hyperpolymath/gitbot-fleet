// SPDX-License-Identifier: PMPL-1.0-or-later
#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // Fuzz command parsing
    if let Ok(s) = std::str::from_utf8(data) {
        // Add actual vext command parsing fuzzing here
        // Example: vext_core::parse_command(s);
        let _ = s.len();
    }
});
