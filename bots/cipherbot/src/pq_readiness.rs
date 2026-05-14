// SPDX-License-Identifier: PMPL-1.0-or-later
//! Post-Quantum Readiness Assessment — scores repositories for PQ preparedness.
//!
//! Score ranges:
//! - **0-20**: Uses broken crypto (MD5, SHA1, DES) — CRITICAL
//! - **20-40**: Uses classical-only crypto (RSA, ECDH, Ed25519) — HIGH
//! - **40-60**: Uses some PQ-ready algorithms but not consistently — MEDIUM
//! - **60-80**: Hybrid classical+PQ in most places — GOOD
//! - **80-100**: Full PQ readiness with FIPS compliance — EXCELLENT

use crate::analyzers::{CryptoStatus, CryptoUsage};
use gitbot_shared_context::finding::{Finding, Severity};
use crate::analyzers::CIPHERBOT_ID;
use serde::{Deserialize, Serialize};

/// Post-quantum readiness rating.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PqRating {
    /// 0-20: Uses broken crypto — CRITICAL
    Critical,
    /// 20-40: Classical-only crypto — HIGH risk
    High,
    /// 40-60: Partially PQ-ready — MEDIUM risk
    Medium,
    /// 60-80: Mostly hybrid classical+PQ — GOOD
    Good,
    /// 80-100: Full PQ readiness — EXCELLENT
    Excellent,
}

impl PqRating {
    /// Get the rating from a numeric score.
    pub fn from_score(score: u32) -> Self {
        match score {
            0..=20 => PqRating::Critical,
            21..=40 => PqRating::High,
            41..=60 => PqRating::Medium,
            61..=80 => PqRating::Good,
            _ => PqRating::Excellent,
        }
    }

    /// Get a human-readable label.
    pub fn label(&self) -> &str {
        match self {
            PqRating::Critical => "CRITICAL - Broken crypto in use",
            PqRating::High => "HIGH - Classical-only, not PQ-safe",
            PqRating::Medium => "MEDIUM - Partially PQ-ready",
            PqRating::Good => "GOOD - Mostly hybrid PQ",
            PqRating::Excellent => "EXCELLENT - Full PQ readiness",
        }
    }
}

/// Post-quantum readiness assessment result.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PqAssessment {
    /// Overall PQ readiness score (0-100).
    pub score: u32,
    /// Rating derived from score.
    pub rating: PqRating,
    /// Total cryptographic usages found.
    pub total_usages: usize,
    /// Count of broken/rejected algorithms.
    pub rejected_count: usize,
    /// Count of classical-only (warned) algorithms.
    pub warned_count: usize,
    /// Count of acceptable algorithms.
    pub accepted_count: usize,
    /// Count of preferred (PQ-ready) algorithms.
    pub preferred_count: usize,
    /// Migration recommendations.
    pub migrations: Vec<MigrationRecommendation>,
}

/// A recommended migration from one algorithm to another.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MigrationRecommendation {
    /// Current algorithm being used.
    pub from: String,
    /// Recommended replacement.
    pub to: String,
    /// Priority (1 = highest, 5 = lowest).
    pub priority: u8,
    /// Reason for migration.
    pub reason: String,
}

/// Standard migration paths from classical to post-quantum.
static MIGRATION_PATHS: &[(&str, &str, u8, &str)] = &[
    ("MD5", "SHAKE3-512 (FIPS 202)", 1, "MD5 is broken — immediate replacement required"),
    ("SHA-1", "SHAKE3-512 (FIPS 202)", 1, "SHA-1 is broken — immediate replacement required"),
    ("DES/3DES", "XChaCha20-Poly1305 (256-bit)", 1, "DES/3DES is broken — immediate replacement"),
    ("RC4", "XChaCha20-Poly1305 (256-bit)", 1, "RC4 is broken — immediate replacement"),
    ("AES-ECB", "XChaCha20-Poly1305 or AES-256-GCM", 1, "ECB mode reveals patterns"),
    ("SHA-256", "SHAKE3-512 (FIPS 202)", 2, "Not post-quantum safe"),
    ("SHA-384", "SHAKE3-512 (FIPS 202)", 3, "SHAKE3-512 preferred for PQ readiness"),
    ("AES-CBC", "XChaCha20-Poly1305 or AES-256-GCM", 2, "No authenticated encryption"),
    ("AES-GCM-128", "XChaCha20-Poly1305 (256-bit)", 3, "128-bit may lack PQ safety margin"),
    ("RSA-1024", "Kyber-1024 + SHAKE256-KDF (FIPS 203)", 1, "RSA-1024 is factorable"),
    ("DH-1024", "Kyber-1024 + SHAKE256-KDF (FIPS 203)", 1, "Vulnerable to Logjam"),
    ("RSA-2048", "Kyber-1024 + SHAKE256-KDF hybrid", 2, "Not post-quantum safe"),
    ("ECDH-P256", "Kyber-1024 + X448 hybrid", 2, "Not post-quantum safe"),
    ("Ed25519", "Ed448 + Dilithium5 hybrid (FIPS 204)", 3, "Classical only"),
    ("RSA-SHA1", "Dilithium5 + Ed448 hybrid", 1, "SHA-1 is broken"),
    ("DSA", "Dilithium5 + Ed448 hybrid", 1, "DSA is deprecated"),
    ("bcrypt", "Argon2id (512 MiB, 8 iter, 4 lanes)", 3, "Limited to 72 bytes"),
    ("scrypt", "Argon2id (512 MiB, 8 iter, 4 lanes)", 4, "Acceptable but Argon2id preferred"),
    ("PBKDF2", "Argon2id (512 MiB, 8 iter, 4 lanes)", 3, "No memory-hardness"),
];

/// Assess post-quantum readiness from a set of crypto usages.
pub fn assess(usages: &[CryptoUsage]) -> PqAssessment {
    if usages.is_empty() {
        return PqAssessment {
            score: 100,
            rating: PqRating::Excellent,
            total_usages: 0,
            rejected_count: 0,
            warned_count: 0,
            accepted_count: 0,
            preferred_count: 0,
            migrations: Vec::new(),
        };
    }

    let mut rejected = 0u32;
    let mut warned = 0u32;
    let mut accepted = 0u32;
    let mut preferred = 0u32;
    let mut seen_algorithms: std::collections::HashSet<String> = std::collections::HashSet::new();

    for usage in usages {
        match usage.status {
            CryptoStatus::Reject => rejected += 1,
            CryptoStatus::Warn => warned += 1,
            CryptoStatus::Accept | CryptoStatus::Fallback => accepted += 1,
            CryptoStatus::Prefer => preferred += 1,
        }
        seen_algorithms.insert(usage.algorithm.clone());
    }

    let total = usages.len() as u32;

    // Score calculation:
    // - Each reject: -20 points (from base 50)
    // - Each warn: -5 points
    // - Each accept: +2 points
    // - Each prefer: +10 points
    // Clamped to 0-100
    let base_score: i32 = 50;
    let score_adjust = -(rejected as i32 * 20)
        - (warned as i32 * 5)
        + (accepted as i32 * 2)
        + (preferred as i32 * 10);
    let raw_score = (base_score + score_adjust).clamp(0, 100) as u32;

    // Generate migration recommendations
    let mut migrations = Vec::new();
    for (from, to, priority, reason) in MIGRATION_PATHS {
        if seen_algorithms.contains(*from) {
            migrations.push(MigrationRecommendation {
                from: from.to_string(),
                to: to.to_string(),
                priority: *priority,
                reason: reason.to_string(),
            });
        }
    }
    migrations.sort_by_key(|m| m.priority);

    PqAssessment {
        score: raw_score,
        rating: PqRating::from_score(raw_score),
        total_usages: total as usize,
        rejected_count: rejected as usize,
        warned_count: warned as usize,
        accepted_count: accepted as usize,
        preferred_count: preferred as usize,
        migrations,
    }
}

/// Convert a PQ assessment into a fleet finding for reporting.
pub fn assessment_to_finding(assessment: &PqAssessment) -> Finding {
    let severity = match assessment.rating {
        PqRating::Critical => Severity::Error,
        PqRating::High => Severity::Error,
        PqRating::Medium => Severity::Warning,
        PqRating::Good => Severity::Info,
        PqRating::Excellent => Severity::Suggestion,
    };

    let migration_summary = if assessment.migrations.is_empty() {
        "No migrations needed.".to_string()
    } else {
        let items: Vec<String> = assessment
            .migrations
            .iter()
            .take(5)
            .map(|m| format!("{} -> {} (P{})", m.from, m.to, m.priority))
            .collect();
        format!("Top migrations: {}", items.join("; "))
    };

    Finding::new(
        CIPHERBOT_ID,
        "CIPHER-PQ-READINESS",
        severity,
        &format!(
            "Post-Quantum Readiness Score: {}/100 ({}). {} usages analyzed: {} rejected, {} warned, {} accepted, {} preferred. {}",
            assessment.score,
            assessment.rating.label(),
            assessment.total_usages,
            assessment.rejected_count,
            assessment.warned_count,
            assessment.accepted_count,
            assessment.preferred_count,
            migration_summary,
        ),
    )
    .with_category("crypto/pq-vulnerable")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn make_usage(algorithm: &str, status: CryptoStatus) -> CryptoUsage {
        CryptoUsage {
            algorithm: algorithm.to_string(),
            status,
            file: PathBuf::from("test.rs"),
            line: 1,
            matched_text: "test".to_string(),
            category: "crypto/test".to_string(),
            message: "test".to_string(),
            suggestion: None,
        }
    }

    #[test]
    fn test_empty_usages_excellent() {
        let assessment = assess(&[]);
        assert_eq!(assessment.score, 100);
        assert_eq!(assessment.rating, PqRating::Excellent);
    }

    #[test]
    fn test_all_preferred_excellent() {
        let usages = vec![
            make_usage("SHAKE3-512", CryptoStatus::Prefer),
            make_usage("Kyber-1024", CryptoStatus::Prefer),
            make_usage("Dilithium5", CryptoStatus::Prefer),
            make_usage("XChaCha20-Poly1305", CryptoStatus::Prefer),
            make_usage("ChaCha20-DRBG", CryptoStatus::Prefer),
            make_usage("Argon2id-PQ", CryptoStatus::Prefer),
        ];
        let assessment = assess(&usages);
        // base 50 + 6*10 = 110, clamped to 100
        assert!(assessment.score >= 80, "Score {} should be >= 80", assessment.score);
        assert_eq!(assessment.rating, PqRating::Excellent);
    }

    #[test]
    fn test_all_rejected_critical() {
        let usages = vec![
            make_usage("MD5", CryptoStatus::Reject),
            make_usage("SHA-1", CryptoStatus::Reject),
            make_usage("DES/3DES", CryptoStatus::Reject),
        ];
        let assessment = assess(&usages);
        assert!(assessment.score <= 20, "Score {} should be <= 20", assessment.score);
        assert_eq!(assessment.rating, PqRating::Critical);
    }

    #[test]
    fn test_classical_only_high() {
        let usages = vec![
            make_usage("Ed25519", CryptoStatus::Warn),
            make_usage("RSA-2048", CryptoStatus::Warn),
            make_usage("ECDH-P256", CryptoStatus::Warn),
        ];
        let assessment = assess(&usages);
        assert!(assessment.score <= 40, "Score {} should be <= 40", assessment.score);
    }

    #[test]
    fn test_mixed_medium() {
        let usages = vec![
            make_usage("SHAKE3-512", CryptoStatus::Prefer),
            make_usage("Ed25519", CryptoStatus::Warn),
            make_usage("BLAKE3", CryptoStatus::Accept),
        ];
        let assessment = assess(&usages);
        assert!(
            assessment.score > 20 && assessment.score <= 80,
            "Mixed score {} should be between 20 and 80",
            assessment.score
        );
    }

    #[test]
    fn test_migration_recommendations() {
        let usages = vec![
            make_usage("MD5", CryptoStatus::Reject),
            make_usage("Ed25519", CryptoStatus::Warn),
        ];
        let assessment = assess(&usages);
        assert!(!assessment.migrations.is_empty(), "Should have migration recommendations");
        // MD5 migration should be priority 1
        assert_eq!(assessment.migrations[0].priority, 1);
    }

    #[test]
    fn test_pq_rating_from_score() {
        assert_eq!(PqRating::from_score(0), PqRating::Critical);
        assert_eq!(PqRating::from_score(20), PqRating::Critical);
        assert_eq!(PqRating::from_score(21), PqRating::High);
        assert_eq!(PqRating::from_score(40), PqRating::High);
        assert_eq!(PqRating::from_score(50), PqRating::Medium);
        assert_eq!(PqRating::from_score(70), PqRating::Good);
        assert_eq!(PqRating::from_score(90), PqRating::Excellent);
        assert_eq!(PqRating::from_score(100), PqRating::Excellent);
    }

    #[test]
    fn test_assessment_to_finding() {
        let assessment = PqAssessment {
            score: 30,
            rating: PqRating::High,
            total_usages: 5,
            rejected_count: 1,
            warned_count: 3,
            accepted_count: 1,
            preferred_count: 0,
            migrations: vec![MigrationRecommendation {
                from: "MD5".to_string(),
                to: "SHAKE3-512".to_string(),
                priority: 1,
                reason: "Broken".to_string(),
            }],
        };
        let finding = assessment_to_finding(&assessment);
        assert_eq!(finding.severity, Severity::Error);
        assert_eq!(finding.category, "crypto/pq-vulnerable");
    }
}
