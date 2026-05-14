// SPDX-License-Identifier: PMPL-1.0-or-later
//! Cipherbot — Cryptographic Hygiene & Post-Quantum Readiness Bot
//!
//! Entry point for the cipherbot CLI. Dispatches to the appropriate
//! subcommand handler based on command-line arguments.

use anyhow::{Context, Result};
use clap::Parser;
use tracing_subscriber::EnvFilter;

use cipherbot::analyzers::{self, Analyzer};
use cipherbot::cli::{Cli, Command, OutputFormat};
use cipherbot::fleet;
use cipherbot::pq_readiness;
use cipherbot::report;

fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize tracing
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
        Command::Analyze { file } => cmd_analyze(&file, cli.format),
        Command::Report { dir, output } => cmd_report(&dir, output.as_deref(), cli.format),
        Command::Fleet { dir, mode } => cmd_fleet(&dir, &mode, cli.format),
        Command::PqReadiness { dir } => cmd_pq_readiness(&dir, cli.format),
        Command::AuditDeps { dir } => cmd_audit_deps(&dir, cli.format),
    }
}

/// Full crypto hygiene scan.
fn cmd_scan(dir: &std::path::Path, format: OutputFormat) -> Result<()> {
    tracing::info!("Scanning directory: {}", dir.display());

    let findings = analyzers::run_all_analyzers(dir);

    match format {
        OutputFormat::Text => {
            if findings.is_empty() {
                println!("No cryptographic issues found.");
            } else {
                println!("Found {} cryptographic issue(s):\n", findings.len());
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

/// Single file analysis.
fn cmd_analyze(file: &std::path::Path, format: OutputFormat) -> Result<()> {
    tracing::info!("Analyzing file: {}", file.display());

    let content =
        std::fs::read_to_string(file).with_context(|| format!("Failed to read {}", file.display()))?;

    let all_analyzers: Vec<Box<dyn analyzers::Analyzer>> = vec![
        Box::new(analyzers::hashing::HashingAnalyzer),
        Box::new(analyzers::symmetric::SymmetricAnalyzer),
        Box::new(analyzers::key_exchange::KeyExchangeAnalyzer),
        Box::new(analyzers::signatures::SignatureAnalyzer),
        Box::new(analyzers::password::PasswordAnalyzer),
        Box::new(analyzers::protocol::ProtocolAnalyzer),
        Box::new(analyzers::rng::RngAnalyzer),
        Box::new(analyzers::deps::DependencyAnalyzer),
        Box::new(analyzers::config::ConfigAnalyzer),
        Box::new(analyzers::dns::DnsAnalyzer),
        Box::new(analyzers::infra::InfraAnalyzer),
    ];

    let mut all_usages = Vec::new();
    for analyzer in &all_analyzers {
        all_usages.extend(analyzer.analyze_content(file, &content));
    }

    let findings: Vec<_> = all_usages.iter().map(|u| u.to_finding()).collect();

    match format {
        OutputFormat::Text => {
            if findings.is_empty() {
                println!("No cryptographic issues found in {}.", file.display());
            } else {
                println!(
                    "Found {} issue(s) in {}:\n",
                    findings.len(),
                    file.display()
                );
                for finding in &findings {
                    println!(
                        "  {} [{}] L{}: {}",
                        finding.severity.icon(),
                        finding.rule_id,
                        finding.line.unwrap_or(0),
                        finding.message,
                    );
                    if let Some(ref suggestion) = finding.suggestion {
                        println!("    Suggestion: {}", suggestion);
                    }
                }
            }
        }
        OutputFormat::Json => {
            let json = serde_json::to_string_pretty(&findings)
                .context("Failed to serialize findings")?;
            println!("{}", json);
        }
    }

    Ok(())
}

/// Generate SARIF report.
fn cmd_report(
    dir: &std::path::Path,
    output: Option<&std::path::Path>,
    _format: OutputFormat,
) -> Result<()> {
    tracing::info!("Generating SARIF report for: {}", dir.display());

    let findings = analyzers::run_all_analyzers(dir);
    let sarif = report::generate_sarif(&findings);
    let json = serde_json::to_string_pretty(&sarif).context("Failed to serialize SARIF report")?;

    if let Some(output_path) = output {
        std::fs::write(output_path, &json)
            .with_context(|| format!("Failed to write report to {}", output_path.display()))?;
        println!("SARIF report written to {}", output_path.display());
    } else {
        println!("{}", json);
    }

    Ok(())
}

/// Run as fleet member.
fn cmd_fleet(dir: &std::path::Path, mode_str: &str, format: OutputFormat) -> Result<()> {
    let mode = fleet::BotMode::parse(mode_str)
        .unwrap_or_else(|| {
            tracing::warn!("Unknown mode '{}', defaulting to advisor", mode_str);
            fleet::BotMode::Advisor
        });

    tracing::info!("Running as fleet member in {:?} mode", mode);

    let (finding_set, execution) = fleet::run_fleet_scan(dir, mode);

    match format {
        OutputFormat::Text => {
            println!("Cipherbot Fleet Scan Complete");
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

/// Post-quantum readiness assessment.
fn cmd_pq_readiness(dir: &std::path::Path, format: OutputFormat) -> Result<()> {
    tracing::info!("Assessing PQ readiness for: {}", dir.display());

    let all_analyzers: Vec<Box<dyn analyzers::Analyzer>> = vec![
        Box::new(analyzers::hashing::HashingAnalyzer),
        Box::new(analyzers::symmetric::SymmetricAnalyzer),
        Box::new(analyzers::key_exchange::KeyExchangeAnalyzer),
        Box::new(analyzers::signatures::SignatureAnalyzer),
        Box::new(analyzers::password::PasswordAnalyzer),
        Box::new(analyzers::protocol::ProtocolAnalyzer),
        Box::new(analyzers::rng::RngAnalyzer),
    ];

    let mut all_usages = Vec::new();
    for analyzer in &all_analyzers {
        all_usages.extend(analyzer.analyze_directory(dir));
    }

    let assessment = pq_readiness::assess(&all_usages);

    match format {
        OutputFormat::Text => {
            println!("Post-Quantum Readiness Assessment");
            println!("==================================");
            println!("Score: {}/100", assessment.score);
            println!("Rating: {}", assessment.rating.label());
            println!();
            println!("Breakdown:");
            println!("  Total usages: {}", assessment.total_usages);
            println!("  Rejected (broken): {}", assessment.rejected_count);
            println!("  Warned (classical): {}", assessment.warned_count);
            println!("  Accepted: {}", assessment.accepted_count);
            println!("  Preferred (PQ-ready): {}", assessment.preferred_count);

            if !assessment.migrations.is_empty() {
                println!();
                println!("Migration Roadmap:");
                for migration in &assessment.migrations {
                    println!(
                        "  [P{}] {} -> {}",
                        migration.priority, migration.from, migration.to
                    );
                    println!("       Reason: {}", migration.reason);
                }
            }
        }
        OutputFormat::Json => {
            let json = serde_json::to_string_pretty(&assessment)
                .context("Failed to serialize assessment")?;
            println!("{}", json);
        }
    }

    Ok(())
}

/// Dependency crypto audit only.
fn cmd_audit_deps(dir: &std::path::Path, format: OutputFormat) -> Result<()> {
    tracing::info!("Auditing dependencies in: {}", dir.display());

    let analyzer = analyzers::deps::DependencyAnalyzer;
    let usages = analyzer.analyze_directory(dir);
    let findings: Vec<_> = usages.iter().map(|u| u.to_finding()).collect();

    match format {
        OutputFormat::Text => {
            if findings.is_empty() {
                println!("No problematic crypto dependencies found.");
            } else {
                println!("Found {} dependency issue(s):\n", findings.len());
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
            }
        }
        OutputFormat::Json => {
            let json = serde_json::to_string_pretty(&findings)
                .context("Failed to serialize findings")?;
            println!("{}", json);
        }
    }

    Ok(())
}
