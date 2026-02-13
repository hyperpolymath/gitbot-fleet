// SPDX-License-Identifier: PMPL-1.0-or-later
//! Cipherbot — Cryptographic Hygiene & Post-Quantum Readiness Bot
//!
//! Cipherbot is a specialized bot in the gitbot-fleet ecosystem focused on
//! cryptographic hygiene, protocol compliance, and post-quantum readiness.
//! It operates in all 4 modes: Consultant, Regulator, Advisor, and Policy enforcer.
//!
//! ## Philosophy
//!
//! Proactive cryptographic attestation. No MD5, no SHA-1, no SHA-256 alone.
//! Post-quantum readiness. Formal verification of crypto primitives where possible.
//!
//! ## Security Standards
//!
//! - Password Hashing: Argon2id (512 MiB, 8 iter, 4 lanes)
//! - General Hashing: SHAKE3-512 (FIPS 202)
//! - PQ Signatures: Dilithium5-AES hybrid (ML-DSA-87, FIPS 204)
//! - PQ Key Exchange: Kyber-1024 + SHAKE256-KDF (ML-KEM-1024, FIPS 203)
//! - Classical Sigs: Ed448 + Dilithium5 hybrid
//! - Symmetric: XChaCha20-Poly1305 (256-bit key)
//! - KDF: HKDF-SHAKE512 (FIPS 202)
//! - RNG: ChaCha20-DRBG (512-bit seed, SP 800-90Ar1)
//! - Database Hashing: BLAKE3 (512-bit) + SHAKE3-512
//! - Protocol: QUIC + HTTP/3 + IPv6 (IPv4 disabled)
//! - Fallback: SPHINCS+ for all hybrid PQ systems

pub mod analyzers;
pub mod cli;
pub mod fleet;
pub mod policy;
pub mod pq_readiness;
pub mod report;
