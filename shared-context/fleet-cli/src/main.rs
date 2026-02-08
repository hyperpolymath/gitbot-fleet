// SPDX-License-Identifier: PMPL-1.0-or-later
//! Unified Fleet CLI - Single entry point for all gitbot-fleet operations

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use colored::*;
use gitbot_shared_context::{BotId, Context as FleetContext, ReportFormat};
use std::path::PathBuf;
use tracing::{info, warn};

#[derive(Parser)]
#[command(name = "fleet")]
#[command(about = "Unified CLI for gitbot-fleet coordination", long_about = None)]
#[command(version)]
struct Cli {
    /// Path to repository
    #[arg(short, long, default_value = ".")]
    repo: PathBuf,

    /// Verbose logging
    #[arg(short, long)]
    verbose: bool,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Run all fleet bots on a repository
    Run {
        /// Specific bots to run (default: all)
        #[arg(short, long)]
        bots: Vec<String>,

        /// Skip bot if it fails
        #[arg(long)]
        continue_on_error: bool,
    },

    /// Show fleet status
    Status {
        /// Show detailed information
        #[arg(short, long)]
        detailed: bool,
    },

    /// List all available bots
    List {
        /// Filter by tier (0=Engine, 1=Verifier, 2=Finisher, 3=Executor)
        #[arg(short, long)]
        tier: Option<u8>,
    },

    /// Show findings from fleet analysis
    Findings {
        /// Filter by bot
        #[arg(short, long)]
        bot: Option<String>,

        /// Filter by severity (error, warning, info, suggestion)
        #[arg(short, long)]
        severity: Option<String>,

        /// Export as JSON
        #[arg(long)]
        json: bool,
    },

    /// Generate fleet report
    Report {
        /// Output file (default: stdout)
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Format (markdown, json, html)
        #[arg(short, long, default_value = "markdown")]
        format: String,
    },

    /// Check fleet health status
    Health {
        /// Export as JSON
        #[arg(long)]
        json: bool,

        /// Watch mode (continuous monitoring)
        #[arg(short, long)]
        watch: bool,

        /// Watch interval in seconds
        #[arg(long, default_value = "30")]
        interval: u64,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize tracing
    let level = if cli.verbose { "debug" } else { "info" };
    tracing_subscriber::fmt()
        .with_env_filter(level)
        .with_target(false)
        .init();

    // Get repository name
    let repo_name = cli
        .repo
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("unknown");

    match cli.command {
        Commands::Run {
            bots,
            continue_on_error,
        } => run_bots(&cli.repo, repo_name, bots, continue_on_error).await,

        Commands::Status { detailed } => show_status(&cli.repo, repo_name, detailed).await,

        Commands::List { tier } => {
            list_bots(tier);
            Ok(())
        }

        Commands::Findings {
            bot,
            severity,
            json,
        } => show_findings(&cli.repo, repo_name, bot, severity, json).await,

        Commands::Report { output, format } => {
            generate_report(&cli.repo, repo_name, output, format).await
        }

        Commands::Health {
            json,
            watch,
            interval,
        } => check_health(&cli.repo, repo_name, json, watch, interval).await,
    }
}

async fn run_bots(
    repo_path: &PathBuf,
    repo_name: &str,
    bot_names: Vec<String>,
    continue_on_error: bool,
) -> Result<()> {
    println!(
        "{} {}",
        "Running fleet on:".bold(),
        repo_name.bright_blue()
    );

    let mut ctx = FleetContext::new(repo_name, repo_path);
    ctx.register_all_bots();

    let bots_to_run = if bot_names.is_empty() {
        BotId::all()
    } else {
        bot_names
            .iter()
            .filter_map(|name| parse_bot_id(name))
            .collect()
    };

    let mut success_count = 0;
    let mut error_count = 0;

    for bot in bots_to_run {
        print!("  {} {:?}... ", "→".bright_green(), bot);

        match ctx.start_bot(bot) {
            Ok(_) => {
                println!("{}", "started".green());

                // Simulate bot execution (would call actual bot here)
                ctx.complete_bot(bot, 0, 0, 0)?;
                success_count += 1;
            }
            Err(e) => {
                println!("{}: {}", "failed".red(), e);
                error_count += 1;

                if !continue_on_error {
                    return Err(e.into());
                }
            }
        }
    }

    println!();
    println!(
        "{} {} succeeded, {} failed",
        "Summary:".bold(),
        success_count.to_string().green(),
        error_count.to_string().red()
    );

    Ok(())
}

async fn show_status(repo_path: &PathBuf, repo_name: &str, detailed: bool) -> Result<()> {
    println!("{} {}", "Fleet Status for:".bold(), repo_name.bright_blue());
    println!();

    let ctx = FleetContext::new(repo_name, repo_path);

    if detailed {
        println!("  {} {}", "Session ID:".bold(), ctx.session_id);
        println!("  {} {}", "Started:".bold(), ctx.started_at);
    }

    println!(
        "  {} {} registered",
        "Bots:".bold(),
        ctx.executions.len()
    );
    println!(
        "  {} {}",
        "Findings:".bold(),
        ctx.findings.findings.len()
    );

    Ok(())
}

fn list_bots(tier_filter: Option<u8>) {
    println!("{}", "Available Fleet Bots:".bold());
    println!();

    for bot in BotId::all() {
        let tier = bot.tier();
        if let Some(filter) = tier_filter {
            if tier as u8 != filter {
                continue;
            }
        }

        println!(
            "  {} {:?} (Tier {}: {})",
            "•".bright_cyan(),
            bot,
            tier as u8,
            tier_name(tier)
        );
    }
}

async fn show_findings(
    _repo_path: &PathBuf,
    repo_name: &str,
    bot_filter: Option<String>,
    _severity_filter: Option<String>,
    as_json: bool,
) -> Result<()> {
    if as_json {
        println!("{{\"repo\": \"{}\", \"findings\": []}}", repo_name);
    } else {
        println!("{} {}", "Findings for:".bold(), repo_name.bright_blue());
        println!("  {} No findings yet", "ℹ️".bright_blue());
    }

    Ok(())
}

async fn generate_report(
    repo_path: &PathBuf,
    repo_name: &str,
    output: Option<PathBuf>,
    format: String,
) -> Result<()> {
    println!(
        "{} {} report for {}",
        "Generating".bold(),
        format,
        repo_name.bright_blue()
    );

    let ctx = FleetContext::new(repo_name, repo_path);

    let report_format = match format.to_lowercase().as_str() {
        "json" => ReportFormat::Json,
        "html" => ReportFormat::Html,
        _ => ReportFormat::Markdown,
    };

    let report = ctx.generate_report(report_format);

    if let Some(path) = output {
        std::fs::write(&path, &report)?;
        println!("{} Report written to {}", "✓".green(), path.display());
    } else {
        println!("\n{}", report);
    }

    Ok(())
}

fn parse_bot_id(name: &str) -> Option<BotId> {
    match name.to_lowercase().as_str() {
        "rhodibot" => Some(BotId::Rhodibot),
        "echidnabot" => Some(BotId::Echidnabot),
        "sustainabot" => Some(BotId::Sustainabot),
        "glambot" => Some(BotId::Glambot),
        "seambot" => Some(BotId::Seambot),
        "finishbot" => Some(BotId::Finishbot),
        "robot-repo-automaton" | "robot" => Some(BotId::RobotRepoAutomaton),
        "hypatia" => Some(BotId::Hypatia),
        "accessibilitybot" | "accessibility-bot" => Some(BotId::Accessibilitybot),
        "cipherbot" | "cipher-bot" => Some(BotId::Cipherbot),
        _ => None,
    }
}

fn tier_name(tier: gitbot_shared_context::Tier) -> &'static str {
    use gitbot_shared_context::Tier;
    match tier {
        Tier::Engine => "Engine",
        Tier::Verifier => "Verifier",
        Tier::Finisher => "Finisher",
        Tier::Specialist => "Specialist",
        Tier::Executor => "Executor",
        Tier::Custom => "Custom",
    }
}

async fn check_health(
    repo_path: &PathBuf,
    repo_name: &str,
    as_json: bool,
    watch: bool,
    interval: u64,
) -> Result<()> {
    use tokio::time::{sleep, Duration};

    if watch {
        println!(
            "{} {}",
            "Monitoring fleet health for:".bold(),
            repo_name.bright_blue()
        );
        println!("Press Ctrl+C to stop");
        println!();

        loop {
            let ctx = FleetContext::new(repo_name, repo_path);
            let health = ctx.health_check();

            if as_json {
                println!("{}", health.to_json());
            } else {
                health.print();
            }

            sleep(Duration::from_secs(interval)).await;
        }
    } else {
        let ctx = FleetContext::new(repo_name, repo_path);
        let health = ctx.health_check();

        if as_json {
            println!("{}", health.to_json());
        } else {
            health.print();
        }
    }

    Ok(())
}
