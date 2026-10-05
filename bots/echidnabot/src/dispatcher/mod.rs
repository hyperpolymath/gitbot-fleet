// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Prover dispatcher - communicates with ECHIDNA Core

pub mod echidna_client;

pub use echidna_client::EchidnaClient;

use serde::{Deserialize, Serialize};

use crate::trust::{axiom_tracker::AxiomReport, confidence::ConfidenceReport};

/// Proof verification result from ECHIDNA, enriched with trust-bridge data.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProofResult {
    pub status: ProofStatus,
    pub message: String,
    pub prover_output: String,
    pub duration_ms: u64,
    pub artifacts: Vec<String>,
    /// Trust confidence level assessed from prover kind and certificate presence.
    /// None when the result was synthesised without calling ECHIDNA (e.g. error
    /// fall-through or REST path with no output).
    #[serde(default)]
    pub confidence: Option<ConfidenceReport>,
    /// Axiom usage flags scanned from prover output.
    #[serde(default)]
    pub axioms: Option<AxiomReport>,
}

/// Proof verification status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum ProofStatus {
    Verified,
    Failed,
    Timeout,
    Error,
    Unknown,
}

/// Prover slug referencing ECHIDNA's full 113-prover backend set.
///
/// # Migration from enum to slug-based addressing (ADR-CART-003, 2026-04-25)
///
/// This replaces the 12-variant enum mirror with slug-based addressing.
/// Any prover slug from echidna's full 113-prover set is now valid:
/// - Tier 1 classicals (agda, coq, lean, isabelle, z3, cvc5, ...)
/// - Tier 2 classicals (metamath, hol-light, mizar, ...)
/// - Tier 3 classicals (pvs, acl2, hol4, ...)
/// - HP-ecosystem TypeDiscipline (linear-agda, affine-coq, phantom-lean, ...)
/// - Constraint & SAT solvers (glpk, scip, minizinc, cadical, kissat, ...)
/// - Proof checkers (tamarin, proverif, dreal, ...)
/// - And all 113 others supported upstream in echidna.
///
/// Benefits:
/// - echidnabot no longer needs to maintain a parallel enum
/// - Advisor/Consultant/Regulator modes automatically support all 113 provers
/// - No compile-time drift — runtime slug resolution from echidna's dispatcher
/// - Backwards compatible: classic 12 provers serialize as before (lowercase)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ProverSlug(String);

impl ProverSlug {
    /// Create a new prover slug from a string
    pub fn new(slug: impl Into<String>) -> Self {
        ProverSlug(slug.into().to_lowercase())
    }

    /// Get the slug as a string reference
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Detect prover from file extension (classic 12 only; others return None)
    pub fn from_extension(ext: &str) -> Option<Self> {
        let ext = ext.to_lowercase();
        let ext = if ext.starts_with('.') {
            ext
        } else {
            format!(".{}", ext)
        };

        CLASSIC_PROVERS.iter().find_map(|(slug, _)| {
            let prover = ProverSlug::new(*slug);
            if prover.file_extensions().contains(&ext.as_str()) {
                Some(prover)
            } else {
                None
            }
        })
    }

    /// Human-readable name for classic provers (classic 12), others return slug
    pub fn display_name(&self) -> &str {
        CLASSIC_PROVERS
            .iter()
            .find(|(slug, _)| slug.to_lowercase() == self.0)
            .map(|(_, name)| *name)
            .unwrap_or(self.0.as_str())
    }

    /// Get tier for classic provers (1–3); others return 0
    pub fn tier(&self) -> u8 {
        match self.0.as_str() {
            "agda" | "coq" | "lean" | "isabelle" | "z3" | "cvc5" => 1,
            "metamath" | "hol-light" | "mizar" => 2,
            "pvs" | "acl2" | "hol4" => 3,
            _ => 0, // Unknown or HP-ecosystem; defer to echidna
        }
    }

    /// Get file extensions for classic provers
    pub fn file_extensions(&self) -> &[&str] {
        CLASSIC_PROVERS
            .iter()
            .find(|(slug, _)| slug.to_lowercase() == self.0)
            .map(|(_, _)| {
                // Return extensions from the tuple's list
                const AGDA_EXTS: &[&str] = &[".agda", ".lagda", ".lagda.md"];
                const COQ_EXTS: &[&str] = &[".v"];
                const LEAN_EXTS: &[&str] = &[".lean"];
                const ISABELLE_EXTS: &[&str] = &[".thy"];
                const Z3_EXTS: &[&str] = &[".smt2", ".z3"];
                const CVC5_EXTS: &[&str] = &[".smt2", ".cvc5"];
                const METAMATH_EXTS: &[&str] = &[".mm"];
                const HOLLIGHT_EXTS: &[&str] = &[".ml"];
                const MIZAR_EXTS: &[&str] = &[".miz"];
                const PVS_EXTS: &[&str] = &[".pvs"];
                const ACL2_EXTS: &[&str] = &[".lisp", ".acl2"];
                const HOL4_EXTS: &[&str] = &[".sml"];

                match self.0.as_str() {
                    "agda" => AGDA_EXTS,
                    "coq" => COQ_EXTS,
                    "lean" => LEAN_EXTS,
                    "isabelle" => ISABELLE_EXTS,
                    "z3" => Z3_EXTS,
                    "cvc5" => CVC5_EXTS,
                    "metamath" => METAMATH_EXTS,
                    "hol-light" => HOLLIGHT_EXTS,
                    "mizar" => MIZAR_EXTS,
                    "pvs" => PVS_EXTS,
                    "acl2" => ACL2_EXTS,
                    "hol4" => HOL4_EXTS,
                    _ => &[],
                }
            })
            .unwrap_or(&[])
    }

    /// All classic prover slugs (12) — known statically
    pub fn classic_all() -> impl Iterator<Item = Self> {
        CLASSIC_PROVERS
            .iter()
            .map(|(slug, _)| ProverSlug::new(*slug))
    }

    /// All known provers (currently classic 12; supports 113 via slug resolution)
    pub fn all() -> impl Iterator<Item = Self> {
        Self::classic_all()
    }
}

impl std::fmt::Display for ProverSlug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::str::FromStr for ProverSlug {
    type Err = std::convert::Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(ProverSlug::new(s))
    }
}

// Mapping of classic 12 provers: (slug, display_name, extensions)
const CLASSIC_PROVERS: &[(&str, &str)] = &[
    ("agda", "Agda"),
    ("coq", "Coq"),
    ("lean", "Lean 4"),
    ("isabelle", "Isabelle/HOL"),
    ("z3", "Z3"),
    ("cvc5", "CVC5"),
    ("metamath", "Metamath"),
    ("hol-light", "HOL Light"),
    ("mizar", "Mizar"),
    ("pvs", "PVS"),
    ("acl2", "ACL2"),
    ("hol4", "HOL4"),
];

// Type alias for backwards compatibility
pub type ProverKind = ProverSlug;

/// Tactic suggestion from ECHIDNA's Julia ML component
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TacticSuggestion {
    pub tactic: String,
    pub confidence: f64,
    pub explanation: Option<String>,
}
