// SPDX-License-Identifier: MPL-2.0
//! CLI: `sillybot [--format text|json] scan --path DIR [--base REV] [--repo OWNER/NAME]`.

use clap::{Parser, Subcommand};
use std::path::PathBuf;

/// Sillybot: gitbot-fleet adapter for docs consistency checks.
#[derive(Parser, Debug)]
#[command(name = "sillybot", version)]
#[command(
    about = "Runs docudactyl docs-ingest and recon-silly-ation reconcile at pinned commits and reports docs findings to gitbot-fleet"
)]
#[command(
    long_about = "Sillybot exports HEAD (and --base, if given) of a git repository into a \
    private temporary directory, runs docs-ingest and reconcile on it, and prints the \
    findings. It never writes into the scanned tree. Report-only. Exit codes: 0 ran, no \
    blocking finding; 1 ran, at least one blocking finding; 2 error; 3 not run (a tool is \
    not installed, or no prose file changed against --base). On 2 and 3 the JSON output is \
    an object {status, reason}, never a findings array. Tools are found through \
    SILLYBOT_DOCS_INGEST and SILLYBOT_RECONCILE, else on PATH as docs-ingest and reconcile."
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
    /// Scan a git repository's HEAD and print the docs findings.
    Scan {
        /// Repository directory to scan (a git work tree).
        #[arg(long, default_value = ".")]
        path: PathBuf,

        /// Base revision. Findings already present at the base are reported
        /// as inherited (info), and the scan is skipped when no prose file
        /// changed.
        #[arg(long)]
        base: Option<String>,

        /// Repository name OWNER/NAME. Defaults to the GitHub origin remote.
        #[arg(long)]
        repo: Option<String>,

        /// Pull request number passed through to reconcile.
        #[arg(long, default_value_t = 0)]
        pr: u64,

        /// Seconds the whole scan may run before sillybot reports an error.
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
