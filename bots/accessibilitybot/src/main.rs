// SPDX-License-Identifier: PMPL-1.0-or-later
//! Accessibilitybot CLI - WCAG 2.3 AAA Accessibility Compliance Bot
//!
//! Part of the gitbot-fleet ecosystem.

use accessibilitybot::fleet::WcagLevel;
use accessibilitybot::report::{generate_report, OutputFormat};
use accessibilitybot::scanner;
use clap::{Parser, Subcommand, ValueEnum};
use std::path::PathBuf;
use tracing_subscriber::EnvFilter;

/// WCAG 2.3 AAA Accessibility Compliance Bot for gitbot-fleet
#[derive(Parser)]
#[command(name = "accessibilitybot")]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Run all WCAG checks on a directory
    Check {
        /// Directory to scan
        dir: PathBuf,

        /// WCAG conformance level
        #[arg(long, default_value = "aaa")]
        level: WcagLevelArg,

        /// Output format
        #[arg(long, default_value = "text")]
        format: FormatArg,

        /// Output file (stdout if not specified)
        #[arg(long)]
        output: Option<PathBuf>,

        /// Enable verbose logging
        #[arg(long, short)]
        verbose: bool,
    },

    /// Analyze a single file
    Analyze {
        /// File to analyze
        file: PathBuf,

        /// WCAG conformance level
        #[arg(long, default_value = "aaa")]
        level: WcagLevelArg,

        /// Output format
        #[arg(long, default_value = "text")]
        format: FormatArg,

        /// Enable verbose logging
        #[arg(long, short)]
        verbose: bool,
    },

    /// Generate a SARIF report for a directory
    Report {
        /// Directory to scan
        dir: PathBuf,

        /// WCAG conformance level
        #[arg(long, default_value = "aaa")]
        level: WcagLevelArg,

        /// Output file (stdout if not specified)
        #[arg(long)]
        output: Option<PathBuf>,

        /// Enable verbose logging
        #[arg(long, short)]
        verbose: bool,
    },

    /// Run as a fleet member (machine-readable output)
    Fleet {
        /// Directory to scan
        dir: PathBuf,

        /// WCAG conformance level
        #[arg(long, default_value = "aaa")]
        level: WcagLevelArg,

        /// Enable verbose logging
        #[arg(long, short)]
        verbose: bool,
    },
}

/// WCAG conformance level CLI argument
#[derive(Debug, Clone, Copy, ValueEnum)]
enum WcagLevelArg {
    /// Level A - minimum
    A,
    /// Level AA - standard
    Aa,
    /// Level AAA - enhanced
    Aaa,
}

impl From<WcagLevelArg> for WcagLevel {
    fn from(arg: WcagLevelArg) -> Self {
        match arg {
            WcagLevelArg::A => WcagLevel::A,
            WcagLevelArg::Aa => WcagLevel::AA,
            WcagLevelArg::Aaa => WcagLevel::AAA,
        }
    }
}

/// Output format CLI argument
#[derive(Debug, Clone, Copy, ValueEnum)]
enum FormatArg {
    /// Human-readable text
    Text,
    /// Structured JSON
    Json,
    /// SARIF for IDE/CI
    Sarif,
}

impl From<FormatArg> for OutputFormat {
    fn from(arg: FormatArg) -> Self {
        match arg {
            FormatArg::Text => OutputFormat::Text,
            FormatArg::Json => OutputFormat::Json,
            FormatArg::Sarif => OutputFormat::Sarif,
        }
    }
}

fn init_logging(verbose: bool) {
    let filter = if verbose {
        EnvFilter::new("accessibilitybot=debug")
    } else {
        EnvFilter::new("accessibilitybot=warn")
    };

    tracing_subscriber::fmt()
        .with_env_filter(filter)
        .with_target(false)
        .init();
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Check { dir, level, format, output, verbose } => {
            init_logging(verbose);
            let findings = scanner::scan_directory(&dir, level.into())?;
            let report = generate_report(&findings, format.into());
            write_output(&report, output.as_deref())?;

            if findings.has_errors() {
                std::process::exit(1);
            }
        }

        Commands::Analyze { file, level, format, verbose } => {
            init_logging(verbose);
            let findings = scanner::scan_file(&file, level.into())?;
            let report = generate_report(&findings, format.into());
            println!("{}", report);

            if findings.has_errors() {
                std::process::exit(1);
            }
        }

        Commands::Report { dir, level, output, verbose } => {
            init_logging(verbose);
            let findings = scanner::scan_directory(&dir, level.into())?;
            let report = generate_report(&findings, OutputFormat::Sarif);
            write_output(&report, output.as_deref())?;
        }

        Commands::Fleet { dir, level, verbose } => {
            init_logging(verbose);
            let findings = scanner::scan_directory(&dir, level.into())?;
            let report = generate_report(&findings, OutputFormat::Json);
            println!("{}", report);

            if findings.blocks_release() {
                std::process::exit(1);
            }
        }
    }

    Ok(())
}

/// Write output to file or stdout
fn write_output(content: &str, path: Option<&std::path::Path>) -> anyhow::Result<()> {
    match path {
        Some(p) => {
            std::fs::write(p, content)?;
            eprintln!("Report written to {}", p.display());
        }
        None => {
            println!("{}", content);
        }
    }
    Ok(())
}
