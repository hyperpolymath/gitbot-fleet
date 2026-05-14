// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Prover dispatcher - communicates with ECHIDNA Core

pub mod echidna_client;

pub use echidna_client::EchidnaClient;

use serde::{Deserialize, Serialize};

/// Proof verification result from ECHIDNA
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProofResult {
    pub status: ProofStatus,
    pub message: String,
    pub prover_output: String,
    pub duration_ms: u64,
    pub artifacts: Vec<String>,
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

/// Prover kind matching ECHIDNA's supported backends.
///
/// # DRIFT NOTICE (2026-04-17)
///
/// This local mirror carries **12 variants**, covering only the classic
/// Tier 1–3 backends. Upstream `echidna::provers::ProverKind` (see
/// `verification-ecosystem/echidna/src/rust/provers/mod.rs`) now carries
/// **89 variants** as of commit `8f573f1`: the classic 49 solver backends
/// plus the 40-variant HP-ecosystem TypeDiscipline family (linear, affine,
/// phantom, modal, tropical, epistemic, choreographic, …).
///
/// The mirror is intentionally NOT extended here today. Compile-time effect
/// is zero — this enum stands on its own, and `from_extension` / `tier` /
/// `display_name` / `file_extensions` are all total over its 12 variants.
/// Runtime effect IS present: proofs tagged with an HP-ecosystem discipline
/// that reaches echidnabot will be routed as "unrecognised" by
/// `from_extension`, even though the upstream echidna dispatcher would
/// handle them fine.
///
/// Phase-2 decision (tracked in AI-WORK-todo.md): either (a) extend this
/// mirror to the full 89 variants with a code-generation tool that reads
/// echidna's enum at build time, or (b) switch echidnabot to a path-dep on
/// echidna so `ProverKind` is shared by construction. Option (b) is cleaner
/// but requires breaking the current self-contained deployment. Option (a)
/// is a stepping stone.
///
/// See `standards/testing-and-benchmarking/TESTING-TAXONOMY.adoc` § Part VI
/// CR-1 "Mirror-enum drift test" and CR-2 "Foreign-enum exhaustive-match
/// lint" for the general pattern and the proposed estate-wide fix.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ProverKind {
    // Tier 1 (complete in ECHIDNA)
    Agda,
    Coq,
    Lean,
    Isabelle,
    Z3,
    Cvc5,
    // Tier 2 (complete in ECHIDNA)
    Metamath,
    HolLight,
    Mizar,
    // Tier 3 (stubs in ECHIDNA)
    Pvs,
    Acl2,
    Hol4,
    // Intentional gap vs echidna upstream — see DRIFT NOTICE above.
    // NOT added here: FStar, Dafny, Why3, TLAPS, Twelf, Nuprl, Minlog,
    // Imandra, Vampire, EProver, SPASS, AltErgo, GLPK, SCIP, MiniZinc,
    // Chuffed, ORTools, TypedWasm, SPIN, CBMC, SeaHorn, CaDiCaL, Kissat,
    // MiniSat, NuSMV, TLC, Alloy, Prism, UPPAAL, FramaC, Viper, Tamarin,
    // ProVerif, KeY, DReal, ABC, Idris2 (the 37 classic backends beyond
    // Tier 1–3) + the 40 HP-ecosystem TypeDiscipline variants.
}

impl ProverKind {
    /// Get file extensions associated with this prover
    pub fn file_extensions(&self) -> &[&str] {
        match self {
            Self::Agda => &[".agda", ".lagda", ".lagda.md"],
            Self::Coq => &[".v"],
            Self::Lean => &[".lean"],
            Self::Isabelle => &[".thy"],
            Self::Z3 => &[".smt2", ".z3"],
            Self::Cvc5 => &[".smt2", ".cvc5"],
            Self::Metamath => &[".mm"],
            Self::HolLight => &[".ml"],
            Self::Mizar => &[".miz"],
            Self::Pvs => &[".pvs"],
            Self::Acl2 => &[".lisp", ".acl2"],
            Self::Hol4 => &[".sml"],
        }
    }

    /// Get the tier (1 = complete, 2 = complete, 3 = stub)
    pub fn tier(&self) -> u8 {
        match self {
            Self::Agda | Self::Coq | Self::Lean | Self::Isabelle | Self::Z3 | Self::Cvc5 => 1,
            Self::Metamath | Self::HolLight | Self::Mizar => 2,
            Self::Pvs | Self::Acl2 | Self::Hol4 => 3,
        }
    }

    /// Human-readable name
    pub fn display_name(&self) -> &str {
        match self {
            Self::Agda => "Agda",
            Self::Coq => "Coq",
            Self::Lean => "Lean 4",
            Self::Isabelle => "Isabelle/HOL",
            Self::Z3 => "Z3",
            Self::Cvc5 => "CVC5",
            Self::Metamath => "Metamath",
            Self::HolLight => "HOL Light",
            Self::Mizar => "Mizar",
            Self::Pvs => "PVS",
            Self::Acl2 => "ACL2",
            Self::Hol4 => "HOL4",
        }
    }

    /// Detect prover from file extension
    pub fn from_extension(ext: &str) -> Option<Self> {
        let ext = ext.to_lowercase();
        let ext = if ext.starts_with('.') { ext } else { format!(".{}", ext) };

        Self::all().find(|prover| {
            prover.file_extensions().iter().any(|e| e.to_lowercase() == ext)
        })
    }

    /// All prover variants
    pub fn all() -> impl Iterator<Item = Self> {
        [
            Self::Agda,
            Self::Coq,
            Self::Lean,
            Self::Isabelle,
            Self::Z3,
            Self::Cvc5,
            Self::Metamath,
            Self::HolLight,
            Self::Mizar,
            Self::Pvs,
            Self::Acl2,
            Self::Hol4,
        ]
        .into_iter()
    }
}

/// Tactic suggestion from ECHIDNA's Julia ML component
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TacticSuggestion {
    pub tactic: String,
    pub confidence: f64,
    pub explanation: Option<String>,
}
