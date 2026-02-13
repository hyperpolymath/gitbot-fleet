// SPDX-License-Identifier: PMPL-1.0-or-later
#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // Try to parse arbitrary data as YAML config
    if let Ok(s) = std::str::from_utf8(data) {
        let _ = serde_yaml::from_str::<serde_yaml::Value>(s);
    }
});
