// SPDX-License-Identifier: MPL-2.0
//! Panicbot — Targeted Audit Bot for gitbot-fleet
//!
//! Entry point for the panicbot CLI. Dispatches to the appropriate
//! subcommand handler based on command-line arguments.
//!
//! Subcommands:
//! - `scan <dir>` — Run the allowed panic-attack suite and show findings
//! - `fleet <dir>` — Run as a fleet member (produces FindingSet + A2ML)
//! - `report <dir>` — Generate A2ML debt register only

use anyhow::{Context, Result};
use clap::Parser;
use tracing_subscriber::EnvFilter;

use panicbot::a2ml_writer;
use panicbot::cli::{Cli, Command, OutputFormat};
use panicbot::directives;
use panicbot::fleet;
use panicbot::scanner;
use panicbot::suite::{self, SuiteOptions};
use panicbot::translator;

fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize tracing with verbosity-based filter
    let filter = match cli.verbose {
        0 => "warn",
        1 => "info",
        2 => "debug",
        _ => "trace",
    };
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new(filter)),
        )
        .init();

    match cli.command {
        Command::Scan {
            dir,
            target,
            budget_seconds,
        } => cmd_scan(&dir, target, budget_seconds, cli.format),
        Command::Fleet { dir, mode, target } => cmd_fleet(&dir, &mode, target, cli.format),
        Command::Report { dir, output } => cmd_report(&dir, output.as_deref(), cli.format),
    }
}

/// Exit status when at least one panic-attack part failed. The findings of the
/// parts that ran are still printed; the report-back driver reads this status
/// as an `error` row carrying the failure list.
const EXIT_PART_ERROR: i32 = 3;
/// Exit status when no part analysed anything (all denied by the directives or
/// not applicable). The driver reads it as `not_run`, never as a clean `ran`.
const EXIT_NOTHING_RAN: i32 = 4;

const NOTHING_RAN: &str =
    "no part analysed the repository (all denied by its bot directives or not applicable)";

/// Scan a directory with the allowed panic-attack suite and display translated
/// findings.
fn cmd_scan(
    dir: &std::path::Path,
    target: Option<std::path::PathBuf>,
    budget_seconds: Option<u64>,
    format: OutputFormat,
) -> Result<()> {
    tracing::info!("Scanning: {}", dir.display());

    // A directive file that exists but does not parse stops the run: falling
    // back to the default (now the full suite) would widen the repository.
    let mut config =
        directives::load_directives(dir).context("bot directives unreadable, nothing run")?;
    if let Some(secs) = budget_seconds {
        config.budget = std::time::Duration::from_secs(secs);
    }

    let run = suite::run_suite(dir, &config, &SuiteOptions::from_env(target));
    let findings = run.findings.clone();

    // Failure summary FIRST on stderr: the report-back driver quotes the first
    // line as the row's reason.
    let part_error = run.error_summary();
    if let Some(summary) = &part_error {
        eprintln!("panicbot: {summary}");
    }
    for note in &config.notes {
        eprintln!("panicbot: directive note: {note}");
    }
    for line in run.table() {
        eprintln!("panicbot: {line}");
    }
    eprintln!("panicbot: suite wall time {} ms", run.wall_ms);

    match format {
        OutputFormat::Text => {
            // "No issues found." only for a run that really analysed the repo.
            // A part error or an empty run is stated as such (and exits 3/4).
            if let Some(summary) = &part_error {
                println!("Scan INCOMPLETE: {summary}");
                if !findings.is_empty() {
                    println!(
                        "({} finding(s) from the parts that ran follow)\n",
                        findings.len()
                    );
                }
            } else if !run.analysed() {
                println!("Scan NOT RUN: {NOTHING_RAN}");
            }
            if findings.is_empty() {
                if part_error.is_none() && run.analysed() {
                    println!("No issues found.");
                }
            } else {
                println!("Found {} issue(s):\n", findings.len());
                for finding in &findings {
                    let location = finding.location_string().unwrap_or_default();
                    println!(
                        "  {} [{}] {} {}",
                        finding.severity.icon(),
                        finding.rule_id,
                        location,
                        finding.message,
                    );
                    if let Some(ref suggestion) = finding.suggestion {
                        println!("    Suggestion: {}", suggestion);
                    }
                }

                let (fixable, unfixable) = translator::classify_fixability(&findings);
                println!();
                println!(
                    "Summary: {} fixable, {} unfixable",
                    fixable.len(),
                    unfixable.len()
                );
            }
        }
        OutputFormat::Json => {
            // A findings ARRAY means every allowed part that applies ran. A run
            // that did not is an object naming its status and reason (the same
            // form ponsbot uses), never an array: the report-back driver turns
            // it into an `error` or `not_run` row, so nothing reads as clean.
            let value = if let Some(summary) = &part_error {
                serde_json::json!({
                    "status": "error",
                    "reason": summary,
                    "findings": findings,
                    "parts": run.parts,
                })
            } else if !run.analysed() {
                serde_json::json!({
                    "status": "not_run",
                    "reason": NOTHING_RAN,
                    "findings": findings,
                    "parts": run.parts,
                })
            } else {
                serde_json::to_value(&findings).context("Failed to serialize findings")?
            };
            let json =
                serde_json::to_string_pretty(&value).context("Failed to serialize findings")?;
            println!("{}", json);
        }
    }

    if part_error.is_some() {
        std::process::exit(EXIT_PART_ERROR);
    }
    if !run.analysed() {
        eprintln!("panicbot: {NOTHING_RAN}");
        std::process::exit(EXIT_NOTHING_RAN);
    }
    // Exit with error code if there are blocking issues
    if findings.iter().any(|f| f.severity.blocks_release()) {
        std::process::exit(1);
    }

    Ok(())
}

/// Run as a fleet member.
fn cmd_fleet(
    dir: &std::path::Path,
    mode_str: &str,
    target: Option<std::path::PathBuf>,
    format: OutputFormat,
) -> Result<()> {
    let mode = fleet::BotMode::parse(mode_str).unwrap_or_else(|| {
        tracing::warn!("Unknown mode '{}', defaulting to advisor", mode_str);
        fleet::BotMode::Advisor
    });

    tracing::info!("Running as fleet member in {:?} mode", mode);

    let (finding_set, execution, run) =
        fleet::run_fleet_suite(dir, mode, &SuiteOptions::from_env(target));

    match format {
        OutputFormat::Text => {
            println!("Panicbot Fleet Scan Complete");
            println!("============================");
            println!("Mode: {:?}", mode);
            println!("Findings: {}", finding_set.len());
            println!("Errors: {}", finding_set.errors().len());
            println!("Warnings: {}", finding_set.warnings().len());
            println!();
            if let Some(run) = &run {
                for line in run.table() {
                    println!("  {line}");
                }
                println!();
            }

            for finding in finding_set.into_iter() {
                let location = finding.location_string().unwrap_or_default();
                println!(
                    "  {} [{}] {} {}",
                    finding.severity.icon(),
                    finding.rule_id,
                    location,
                    finding.message,
                );
            }
        }
        OutputFormat::Json => {
            let output = serde_json::json!({
                "bot_info": fleet::bot_info(),
                "execution": execution,
                "findings": finding_set,
                "parts": run.as_ref().map(|r| &r.parts),
            });
            let json = serde_json::to_string_pretty(&output)
                .context("Failed to serialize fleet output")?;
            println!("{}", json);
        }
    }

    Ok(())
}

/// Generate A2ML debt register.
fn cmd_report(
    dir: &std::path::Path,
    output: Option<&std::path::Path>,
    _format: OutputFormat,
) -> Result<()> {
    tracing::info!("Generating A2ML report for: {}", dir.display());

    let config =
        directives::load_directives(dir).context("bot directives unreadable, nothing run")?;
    let report =
        scanner::run_assail(dir, config.timeout).context("Failed to run panic-attack assail")?;

    let findings = translator::translate_all(&report.weak_points, &config);
    let (fixable, unfixable) = translator::classify_fixability(&findings);

    let repo_name = dir
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_else(|| "unknown".to_string());

    let pa_version = scanner::run_diagnostics(config.timeout)
        .map(|d| d.version)
        .unwrap_or_else(|_| "unknown".to_string());

    let a2ml_doc = a2ml_writer::generate_a2ml(
        &fixable,
        &unfixable,
        &format!("hyperpolymath/{}", repo_name),
        &pa_version,
    );

    if let Some(output_path) = output {
        // Write to specified path
        if let Some(parent) = output_path.parent() {
            std::fs::create_dir_all(parent)
                .with_context(|| format!("Failed to create {}", parent.display()))?;
        }
        let json =
            serde_json::to_string_pretty(&a2ml_doc).context("Failed to serialize A2ML document")?;
        std::fs::write(output_path, &json)
            .with_context(|| format!("Failed to write {}", output_path.display()))?;
        println!("A2ML report written to {}", output_path.display());
    } else {
        // Write to default location
        let written = a2ml_writer::write_a2ml(dir, &a2ml_doc)?;
        println!("A2ML report written to {}", written.display());
    }

    println!(
        "Summary: {} total ({} fixable, {} unfixable)",
        findings.len(),
        fixable.len(),
        unfixable.len()
    );

    Ok(())
}
