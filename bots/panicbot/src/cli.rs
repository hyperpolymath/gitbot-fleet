// SPDX-License-Identifier: PMPL-1.0-or-later
//! CLI Interface — clap-based command-line argument parsing for panicbot.
//!
//! Subcommands:
//! - `scan <dir>` — Run panic-attack assail and translate findings
//! - `fleet <dir>` — Run as a gitbot-fleet member
//! - `report <dir>` — Generate A2ML debt register

use clap::{Parser, Subcommand};
use std::path::PathBuf;

/// Panicbot — Targeted Audit Bot for gitbot-fleet
///
/// Wraps panic-attack static analysis, translates WeakPoint findings into
/// fleet-compatible Finding structs, and produces A2ML debt registers for
/// unfixable issues.
#[derive(Parser, Debug)]
#[command(name = "panicbot")]
#[command(version)]
#[command(about = "Targeted audit bot wrapping panic-attack static analysis")]
#[command(long_about = "Panicbot invokes panic-attack as a subprocess, translates its findings \
    into gitbot-fleet Finding structs, classifies them as fixable or unfixable, and writes \
    unfixable findings to per-repo A2ML debt registers.")]
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
    /// Run panic-attack assail on a directory and translate findings.
    Scan {
        /// Directory or binary to scan.
        #[arg(default_value = ".")]
        dir: PathBuf,
    },

    /// Run as a gitbot-fleet member.
    Fleet {
        /// Repository directory to scan.
        #[arg(default_value = ".")]
        dir: PathBuf,

        /// Bot operating mode (advisor, auditor, guardian).
        #[arg(short, long, default_value = "advisor")]
        mode: String,
    },

    /// Generate A2ML debt register for a directory.
    Report {
        /// Repository directory to scan.
        #[arg(default_value = ".")]
        dir: PathBuf,

        /// Output file path. Default: <repo>/.panicbot/PANICBOT-FINDINGS.a2ml.
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::CommandFactory;

    #[test]
    fn test_cli_parses() {
        Cli::command().debug_assert();
    }
}
