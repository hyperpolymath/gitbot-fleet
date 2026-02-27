// SPDX-License-Identifier: PMPL-1.0-or-later
//! Panicbot — Targeted Audit Bot for gitbot-fleet
//!
//! Entry point for the panicbot CLI. Dispatches to the appropriate
//! subcommand handler based on command-line arguments.
//!
//! Subcommands:
//! - `scan <dir>` — Run panic-attack assail and show findings
//! - `fleet <dir>` — Run as a fleet member (produces FindingSet + A2ML)
//! - `report <dir>` — Generate A2ML debt register only

use anyhow::{Context, Result};
use clap::Parser;
use tracing_subscriber::EnvFilter;

use panicbot::cli::{Cli, Command, OutputFormat};
use panicbot::fleet;
use panicbot::scanner;
use panicbot::translator;
use panicbot::directives;
use panicbot::a2ml_writer;

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
        Command::Scan { dir } => cmd_scan(&dir, cli.format),
        Command::Fleet { dir, mode } => cmd_fleet(&dir, &mode, cli.format),
        Command::Report { dir, output } => cmd_report(&dir, output.as_deref(), cli.format),
    }
}

/// Scan a directory with panic-attack and display translated findings.
fn cmd_scan(dir: &std::path::Path, format: OutputFormat) -> Result<()> {
    tracing::info!("Scanning: {}", dir.display());

    let config = directives::load_directives(dir).unwrap_or_default();
    let report = scanner::run_assail(dir, config.timeout)
        .context("Failed to run panic-attack assail")?;

    let findings = translator::translate_all(&report.weak_points, &config);

    match format {
        OutputFormat::Text => {
            if findings.is_empty() {
                println!("No issues found.");
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
            let json = serde_json::to_string_pretty(&findings)
                .context("Failed to serialize findings")?;
            println!("{}", json);
        }
    }

    // Exit with error code if there are blocking issues
    if findings.iter().any(|f| f.severity.blocks_release()) {
        std::process::exit(1);
    }

    Ok(())
}

/// Run as a fleet member.
fn cmd_fleet(dir: &std::path::Path, mode_str: &str, format: OutputFormat) -> Result<()> {
    let mode = fleet::BotMode::parse(mode_str).unwrap_or_else(|| {
        tracing::warn!("Unknown mode '{}', defaulting to advisor", mode_str);
        fleet::BotMode::Advisor
    });

    tracing::info!("Running as fleet member in {:?} mode", mode);

    let (finding_set, execution) = fleet::run_fleet_scan(dir, mode);

    match format {
        OutputFormat::Text => {
            println!("Panicbot Fleet Scan Complete");
            println!("============================");
            println!("Mode: {:?}", mode);
            println!("Findings: {}", finding_set.len());
            println!("Errors: {}", finding_set.errors().len());
            println!("Warnings: {}", finding_set.warnings().len());
            println!();

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

    let config = directives::load_directives(dir).unwrap_or_default();
    let report = scanner::run_assail(dir, config.timeout)
        .context("Failed to run panic-attack assail")?;

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
        let json = serde_json::to_string_pretty(&a2ml_doc)
            .context("Failed to serialize A2ML document")?;
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
