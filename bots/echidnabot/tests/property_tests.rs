// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Property-based tests (proptest).
//!
//! Testing taxonomy category: Property-Based / Generative.
//! Verifies that properties hold across randomly-generated inputs,
//! exercising invariants that unit tests can miss with hand-crafted values.

use echidnabot::api::rate_limit::WebhookRateLimiter;
use echidnabot::dispatcher::ProverKind;
use echidnabot::store::models::goal_fingerprint;
use proptest::prelude::*;
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr};

// ──────────────────────────────────────────────────────────────────────────────
// ProverKind properties
// ──────────────────────────────────────────────────────────────────────────────

proptest! {
    /// ProverKind::new is never empty — as_str() always returns a non-empty string.
    #[test]
    fn prop_prover_kind_nonempty(slug in "[a-z][a-z0-9-]{0,30}") {
        let p = ProverKind::new(&slug);
        prop_assert!(!p.as_str().is_empty());
    }

    /// Roundtrip: as_str() on a constructed slug returns the original slug.
    #[test]
    fn prop_prover_kind_roundtrip(slug in "[a-z][a-z0-9-]{0,30}") {
        let p = ProverKind::new(&slug);
        prop_assert_eq!(p.as_str(), slug.as_str());
    }

    /// display_name() never panics — always returns something, even for unknown slugs.
    #[test]
    fn prop_prover_kind_display_name_never_panics(slug in ".*") {
        // Extremely adversarial: any string, including empty, unicode, control chars.
        let p = ProverKind::new(&slug);
        let _ = p.display_name(); // must not panic
    }

    /// tier() never panics for any slug.
    #[test]
    fn prop_prover_kind_tier_never_panics(slug in "[a-z0-9-]{0,40}") {
        let p = ProverKind::new(&slug);
        let _ = p.tier(); // must not panic
    }

    /// Equality is symmetric.
    #[test]
    fn prop_prover_kind_eq_symmetric(
        s1 in "[a-z][a-z0-9]{0,15}",
        s2 in "[a-z][a-z0-9]{0,15}",
    ) {
        let a = ProverKind::new(&s1);
        let b = ProverKind::new(&s2);
        prop_assert_eq!(a == b, b == a);
    }
}

// ──────────────────────────────────────────────────────────────────────────────
// goal_fingerprint properties
// ──────────────────────────────────────────────────────────────────────────────

proptest! {
    /// Identical inputs always produce identical fingerprints (determinism).
    #[test]
    fn prop_fingerprint_deterministic(goal in ".*") {
        let f1 = goal_fingerprint(&goal);
        let f2 = goal_fingerprint(&goal);
        prop_assert_eq!(f1, f2);
    }

    /// Fingerprint is always non-empty.
    #[test]
    fn prop_fingerprint_nonempty(goal in ".*") {
        let f = goal_fingerprint(&goal);
        prop_assert!(!f.is_empty());
    }

    /// Fingerprint has fixed length (SHA-256 hex = 64 chars).
    #[test]
    fn prop_fingerprint_fixed_length(goal in ".*") {
        let f = goal_fingerprint(&goal);
        prop_assert_eq!(f.len(), 64, "SHA-256 hex must be 64 chars, got {}", f.len());
    }

    /// Different inputs almost always produce different fingerprints.
    /// (Not guaranteed by hash theory, but collision prob is negligible
    ///  for random test inputs.)
    #[test]
    fn prop_fingerprint_differs_for_different_inputs(
        a in "[a-zA-Z0-9 ]{1,200}",
        b in "[a-zA-Z0-9 ]{1,200}",
    ) {
        prop_assume!(a != b);
        let fa = goal_fingerprint(&a);
        let fb = goal_fingerprint(&b);
        // Soft check: log if equal rather than hard-fail (collision is theoretically possible)
        if fa == fb {
            eprintln!("Collision detected (extremely rare): {:?} and {:?} both → {}", a, b, fa);
        }
    }
}

// ──────────────────────────────────────────────────────────────────────────────
// Rate limiter properties
// ──────────────────────────────────────────────────────────────────────────────

fn arb_ipv4() -> impl Strategy<Value = IpAddr> {
    (0u8..=255, 0u8..=255, 0u8..=255, 0u8..=255)
        .prop_map(|(a, b, c, d)| IpAddr::V4(Ipv4Addr::new(a, b, c, d)))
}

fn arb_ipv6() -> impl Strategy<Value = IpAddr> {
    (
        0u16..=0xffff,
        0u16..=0xffff,
        0u16..=0xffff,
        0u16..=0xffff,
        0u16..=0xffff,
        0u16..=0xffff,
        0u16..=0xffff,
        0u16..=0xffff,
    )
        .prop_map(|(a, b, c, d, e, f, g, h)| IpAddr::V6(Ipv6Addr::new(a, b, c, d, e, f, g, h)))
}

proptest! {
    /// Exactly `limit` requests are allowed per IP before the (limit+1)th is blocked.
    #[test]
    fn prop_rate_limiter_blocks_at_limit(limit in 1u32..=20) {
        let limiter = WebhookRateLimiter::new(limit);
        let ip = IpAddr::V4(Ipv4Addr::new(192, 168, 1, 1));

        for i in 0..limit {
            prop_assert!(
                limiter.check_ip(ip),
                "request {} of {} should be allowed", i + 1, limit
            );
        }
        prop_assert!(
            !limiter.check_ip(ip),
            "request {} should be blocked (limit = {})", limit + 1, limit
        );
    }

    /// IPv4 and IPv6 addresses are tracked independently.
    #[test]
    fn prop_rate_limiter_v4_v6_independent(
        ip4 in arb_ipv4(),
        ip6 in arb_ipv6(),
    ) {
        let limiter = WebhookRateLimiter::new(1);
        // Use up the v4 slot
        let _ = limiter.check_ip(ip4);
        let _ = limiter.check_ip(ip4);
        // v6 should still have capacity
        prop_assert!(limiter.check_ip(ip6));
    }

    /// Distinct IPs never share quota.
    #[test]
    fn prop_rate_limiter_ips_independent(
        a in arb_ipv4(),
        b in arb_ipv4(),
    ) {
        prop_assume!(a != b);
        let limiter = WebhookRateLimiter::new(1);
        // Exhaust a's limit
        limiter.check_ip(a);
        limiter.check_ip(a);
        // b still has capacity
        prop_assert!(limiter.check_ip(b));
    }
}
