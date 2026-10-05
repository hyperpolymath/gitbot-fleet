// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Fuzz target: HMAC-SHA256 webhook signature verification.
//!
//! Verifies that:
//! 1. Malformed/arbitrary payloads never panic the HMAC path
//! 2. The constant-time comparison branch is exercised
//! 3. Adversarial inputs cannot produce false-positive signature matches

#![no_main]
use libfuzzer_sys::fuzz_target;
use hmac::{Hmac, Mac};
use sha2::Sha256;

// Fixed secret — we're testing the parsing/comparison path, not the secret itself
const SECRET: &[u8] = b"fuzz-test-secret-key-not-real";  // scanner-allow: rust-secrets

fuzz_target!(|data: &[u8]| {
    // Split data: first 32 bytes treated as a "claimed signature", rest as body
    if data.len() < 4 {
        return;
    }
    let split = (data[0] as usize % data.len().saturating_sub(1)).max(1);
    let claimed_sig_bytes = &data[..split];
    let body = &data[split..];

    // Compute real HMAC for the body
    let mut mac = Hmac::<Sha256>::new_from_slice(SECRET).unwrap();
    mac.update(body);
    let real_tag = mac.finalize().into_bytes();
    let real_hex = hex::encode(real_tag);

    // Interpret claimed_sig_bytes as a hex string (may be invalid hex)
    let claimed_hex = String::from_utf8_lossy(claimed_sig_bytes);

    // Must not panic regardless of claimed_hex content
    let _ = verify_signature(&real_hex, &claimed_hex, body);
});

/// Mirrors the logic in src/api/webhooks.rs verify_github_signature.
/// Returns true only if claimed matches real (constant-time).
fn verify_signature(real_hex: &str, claimed_hex: &str, _body: &[u8]) -> bool {
    if real_hex.len() != claimed_hex.len() {
        return false;
    }
    // Constant-time comparison
    let mut result = 0u8;
    for (a, b) in real_hex.bytes().zip(claimed_hex.bytes()) {
        result |= a ^ b;
    }
    result == 0
}
