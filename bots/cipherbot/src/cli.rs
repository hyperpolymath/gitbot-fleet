// SPDX-License-Identifier: PMPL-1.0-or-later
//! CLI Interface — clap-based command-line argument parsing for cipherbot.
//!
//! Subcommands:
//! - `scan <dir>` — Full crypto hygiene scan
//! - `analyze <file>` — Single file analysis
//! - `report <dir>` — Generate SARIF report
//! - `fleet <dir>` — Run as fleet member
//! - `pq-readiness <dir>` — Post-quantum readiness assessment
//! - `audit-deps <dir>` — Dependency crypto audit only

use clap::{Parser, Subcommand};
use std::path::PathBuf;

/// Cipherbot — Cryptographic Hygiene & Post-Quantum Readiness Bot
///
/// Proactive cryptographic attestation for the gitbot-fleet ecosystem.
/// No MD5, no SHA-1, no SHA-256 alone. Post-quantum readiness.
#[derive(Parser, Debug)]
#[command(name = "cipherbot")]
#[command(version)]
#[command(about = "Cryptographic Hygiene & Post-Quantum Readiness Bot")]
#[command(long_about = "Cipherbot scans codebases for deprecated, weak, or non-post-quantum-safe \
    cryptographic algorithms and provides migration recommendations.")]
pub struct Cli {
    /// Subcommand to run.
    #[command(subcommand)]
    pub command: Command,

    /// Verbosity level (-v, -vv, -vvv).
    #[arg(short, long, action = clap::ArgAction::Count, global = true)]
    pub verbose: u8,

    /// Output format (text, json).
    #[arg(short, long, default_value = "text", global = true)]
    pub format: OutputFormat,
}

/// Output format for results.
#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum OutputFormat {
    /// Human-readable text output.
    Text,
    /// JSON output for programmatic consumption.
    Json,
}

/// Available subcommands.
#[derive(Subcommand, Debug)]
pub enum Command {
    /// Full crypto hygiene scan of a directory.
    Scan {
        /// Directory to scan.
        #[arg(default_value = ".")]
        dir: PathBuf,
    },

    /// Analyze a single file for crypto issues.
    Analyze {
        /// File to analyze.
        file: PathBuf,
    },

    /// Generate a SARIF report for a directory.
    Report {
        /// Directory to scan.
        #[arg(default_value = ".")]
        dir: PathBuf,

        /// Output file (default: stdout).
        #[arg(short, long)]
        output: Option<PathBuf>,
    },

    /// Run as a gitbot-fleet member.
    Fleet {
        /// Directory to scan.
        #[arg(default_value = ".")]
        dir: PathBuf,

        /// Bot operating mode.
        #[arg(short, long, default_value = "advisor")]
        mode: String,
    },

    /// Post-quantum readiness assessment.
    PqReadiness {
        /// Directory to assess.
        #[arg(default_value = ".")]
        dir: PathBuf,
    },

    /// Dependency crypto audit only.
    AuditDeps {
        /// Directory containing manifests.
        #[arg(default_value = ".")]
        dir: PathBuf,
    },
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::CommandFactory;

    #[test]
    fn test_cli_parses() {
        // Verify the CLI structure is valid
        Cli::command().debug_assert();
    }
}
