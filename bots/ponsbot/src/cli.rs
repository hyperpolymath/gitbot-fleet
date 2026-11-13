// SPDX-License-Identifier: MPL-2.0
//! CLI: `ponsbot [--format text|json] scan <dir>`.

use clap::{Parser, Subcommand};
use std::path::PathBuf;

/// Ponsbot: gitbot-fleet adapter for pons asinorum.
#[derive(Parser, Debug)]
#[command(name = "ponsbot", version)]
#[command(about = "Runs pons asinorum at a pinned commit and reports its findings to gitbot-fleet")]
#[command(
    long_about = "Ponsbot runs the `pons` scanner as a subprocess, never writes into the \
    scanned tree, and keeps each finding's tier and evidence class. Exit codes: 0 ran, no \
    blocking finding; 1 ran, at least one blocking finding; 2 error (pons failed, timed out, \
    or its report was unusable); 3 not run (pons is not installed). On 2 and 3 the JSON \
    output is an object {status, reason}, never a findings array."
)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Command,

    /// Output format (text, json).
    #[arg(short, long, default_value = "text", global = true)]
    pub format: OutputFormat,
}

/// Output format for results.
#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum OutputFormat {
    Text,
    Json,
}

#[derive(Subcommand, Debug)]
pub enum Command {
    /// Scan a directory with pons and print the findings.
    Scan {
        /// Repository directory to scan.
        #[arg(default_value = ".")]
        dir: PathBuf,

        /// Seconds pons may run before ponsbot reports an error.
        #[arg(long, default_value_t = 300)]
        timeout: u64,
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
