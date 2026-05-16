// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! finishing-bot CLI - Release Readiness Validator

mod analyzers;
mod config;
mod error;
#[cfg(feature = "fleet")]
mod fleet;

use clap::{Parser, Subcommand};
use std::path::PathBuf;
use std::process::ExitCode;
use tracing_subscriber::fmt::format::FmtSpan;
use tracing_subscriber::{fmt, EnvFilter};
use tracing_subscriber::prelude::*;

/// finishing-bot: Release Readiness Validator
///
/// Ensures repositories are production-ready by validating licenses,
/// removing placeholders, verifying claims, and checking release artifacts.
#[derive(Parser)]
#[command(name = "finishing-bot")]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Path to repository (defaults to current directory)
    #[arg(short, long, default_value = ".")]
    path: PathBuf,

    /// Path to configuration file
    #[arg(short, long)]
    config: Option<PathBuf>,

    /// Enable dry-run mode (no changes)
    #[arg(long)]
    dry_run: bool,

    /// Enable auto-fix mode
    #[arg(long)]
    fix: bool,

    /// Log level (trace, debug, info, warn, error)
    #[arg(long, default_value = "info")]
    log_level: String,

    /// Output format (pretty, json)
    #[arg(long, default_value = "pretty")]
    format: String,

    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand)]
enum Command {
    /// Run full release readiness audit
    Audit {
        /// Fail on any warnings (strict mode)
        #[arg(long)]
        strict: bool,
    },

    /// Apply automatic fixes
    Fix {
        /// Only fix specific category (license, placeholder)
        #[arg(long)]
        only: Option<String>,
    },

    /// Check license compliance only
    License {
        /// List allowed licenses
        #[arg(long)]
        list_allowed: bool,
    },

    /// Check placeholders only
    Placeholders {
        /// Action to take: flag, remove, comment
        #[arg(long, default_value = "flag")]
        action: String,
    },

    /// Verify documentation claims
    Claims,

    /// Check release readiness
    Release,

    /// Initialize configuration file
    Init {
        /// Output format (yaml, toml)
        #[arg(long, default_value = "yaml")]
        format: String,
    },

    /// Show current configuration
    Show,

    /// Run as part of gitbot-fleet (requires 'fleet' feature)
    #[cfg(feature = "fleet")]
    Fleet {
        /// Path to repository to analyze
        #[arg(long, short = 'r', value_name = "PATH")]
        repo: PathBuf,
        /// Path to shared context file
        #[arg(long, value_name = "PATH")]
        context: Option<PathBuf>,
        /// Session ID to join (creates new if not provided)
        #[arg(long, value_name = "ID")]
        session: Option<String>,
        /// Output context to file after execution
        #[arg(long, value_name = "PATH")]
        output: Option<PathBuf>,
    },
}

fn main() -> ExitCode {
    let cli = Cli::parse();

    // Initialize logging
    init_logging(&cli.log_level);

    // Load configuration
    let config_path = cli
        .config
        .clone()
        .unwrap_or_else(config::default_config_path);

    let mut config = match config::load_config(&config_path) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error loading config: {}", e);
            return ExitCode::FAILURE;
        }
    };

    // Apply CLI overrides
    if cli.dry_run {
        config.dry_run = true;
    }
    if cli.fix {
        config.auto_fix = true;
    }

    // Handle commands
    match cli.command {
        Some(Command::Init { format }) => {
            handle_init(&config_path, &format)
        }
        Some(Command::Show) => {
            handle_show(&config)
        }
        Some(Command::Audit { strict }) => {
            handle_audit(&cli.path, &config, strict, &cli.format)
        }
        Some(Command::Fix { only }) => {
            handle_fix(&cli.path, &config, only.as_deref())
        }
        Some(Command::License { list_allowed }) => {
            handle_license(&cli.path, &config, list_allowed)
        }
        Some(Command::Placeholders { action }) => {
            handle_placeholders(&cli.path, &config, &action)
        }
        Some(Command::Claims) => {
            handle_claims(&cli.path, &config)
        }
        Some(Command::Release) => {
            handle_release(&cli.path, &config)
        }
        #[cfg(feature = "fleet")]
        Some(Command::Fleet {
            repo,
            context,
            session,
            output,
        }) => {
            handle_fleet_mode(repo, context, session, output, config)
        }
        None => {
            // Default to audit
            handle_audit(&cli.path, &config, false, &cli.format)
        }
    }
}

#[cfg(feature = "fleet")]
fn handle_fleet_mode(
    repo: PathBuf,
    context_path: Option<PathBuf>,
    session_id: Option<String>,
    output_path: Option<PathBuf>,
    config: config::Config,
) -> ExitCode {
    use gitbot_shared_context::{Context, ContextStorage};
    use std::path::Path;
    use tracing::{error, info};

    // Load or create context
    let mut ctx = if let Some(ref path) = context_path {
        // Parse session ID if provided
        let session_uuid = session_id
            .as_deref()
            .and_then(|s| uuid::Uuid::parse_str(s).ok());

        if let Some(ref sid) = session_uuid {
            match ContextStorage::new(path.parent().unwrap_or(Path::new("."))).load_context(sid) {
                Ok(ctx) => {
                    info!("Loaded existing context from {}", path.display());
                    ctx
                }
                Err(e) => {
                    info!("Creating new context (load failed: {})", e);
                    let repo_name = repo
                        .file_name()
                        .and_then(|s| s.to_str())
                        .unwrap_or("unknown");
                    Context::new(repo_name, &repo)
                }
            }
        } else {
            let repo_name = repo
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or("unknown");
            Context::new(repo_name, &repo)
        }
    } else {
        let repo_name = repo
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown");
        Context::new(repo_name, &repo)
    };

    // Run finishing-bot in fleet mode
    let result = fleet::run_in_context(&mut ctx, &config);

    // Print fleet summary
    let summary = ctx.summary();
    summary.print();

    // Save context if output path specified
    if let Some(ref path) = output_path {
        let storage = ContextStorage::new(path.parent().unwrap_or(Path::new(".")));
        if let Err(e) = storage.save_context(&ctx) {
            error!("Failed to save context: {}", e);
        } else {
            info!("Saved context to {}", path.display());
        }
    }

    // Return exit code based on release readiness
    if result.should_block_release() {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn init_logging(level: &str) {
    let filter = EnvFilter::try_new(level).unwrap_or_else(|_| EnvFilter::new("info"));

    let layer = fmt::layer()
        .with_target(true)
        .with_span_events(FmtSpan::CLOSE)
        .with_ansi(true);

    tracing_subscriber::registry()
        .with(layer.with_filter(filter))
        .init();
}

fn handle_init(config_path: &PathBuf, format: &str) -> ExitCode {
    let path = if format == "toml" {
        config_path.with_extension("toml")
    } else {
        config_path.clone()
    };

    match config::write_default_config(&path) {
        Ok(()) => {
            println!("Created configuration file: {}", path.display());
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("Error creating config: {}", e);
            ExitCode::FAILURE
        }
    }
}

fn handle_show(config: &config::Config) -> ExitCode {
    println!("\nCurrent Configuration:");
    println!("======================\n");

    println!("Licenses:");
    println!("  Allowed: {}", config.licenses.allowed.join(", "));
    println!("  Strict: {}", config.licenses.strict);
    println!("  Require SPDX headers: {}", config.licenses.require_spdx_headers);
    println!();

    println!("Placeholders:");
    println!("  Patterns: {}", config.placeholders.patterns.join(", "));
    println!("  Action: {:?}", config.placeholders.action);
    println!("  Max allowed: {}", config.placeholders.max_allowed);
    println!();

    println!("Claims:");
    println!("  Verify docs: {}", config.claims.verify_docs);
    println!("  Verify tests: {}", config.claims.verify_tests);
    println!("  Require README: {}", config.claims.require_readme);
    println!("  Require CHANGELOG: {}", config.claims.require_changelog);
    println!();

    println!("Release:");
    println!("  Require hashes: {}", config.release.require_hashes);
    println!("  Require signatures: {}", config.release.require_signatures);
    println!();

    println!("General:");
    println!("  Dry run: {}", config.dry_run);
    println!("  Auto fix: {}", config.auto_fix);
    println!("  Exclude: {}", config.exclude.join(", "));

    ExitCode::SUCCESS
}

fn handle_audit(path: &PathBuf, config: &config::Config, strict: bool, format: &str) -> ExitCode {
    use crate::analyzers::{
        claims::ClaimsAnalyzer, license::LicenseAnalyzer, placeholder::PlaceholderAnalyzer,
        release::ReleaseAnalyzer, scm_files::ScmFilesAnalyzer, testing::TestingAnalyzer,
        tooling::ToolingAnalyzer, v1_readiness::V1ReadinessAnalyzer, Analyzer, AuditResult,
    };

    let mut result = AuditResult::default();

    // Run all analyzers
    let license = LicenseAnalyzer::default();
    result.license = match license.analyze(path, config) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("License analysis error: {}", e);
            return ExitCode::FAILURE;
        }
    };

    let placeholder = PlaceholderAnalyzer::default();
    result.placeholder = match placeholder.analyze(path, config) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Placeholder analysis error: {}", e);
            return ExitCode::FAILURE;
        }
    };

    let claims = ClaimsAnalyzer::default();
    result.claims = match claims.analyze(path, config) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Claims analysis error: {}", e);
            return ExitCode::FAILURE;
        }
    };

    let release = ReleaseAnalyzer::default();
    result.release = match release.analyze(path, config) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Release analysis error: {}", e);
            return ExitCode::FAILURE;
        }
    };

    // V1 preparation analyzers
    let scm_files = ScmFilesAnalyzer::default();
    result.scm_files = match scm_files.analyze(path, config) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("SCM files analysis error: {}", e);
            return ExitCode::FAILURE;
        }
    };

    let testing = TestingAnalyzer::default();
    result.testing = match testing.analyze(path, config) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Testing analysis error: {}", e);
            return ExitCode::FAILURE;
        }
    };

    let tooling = ToolingAnalyzer::default();
    result.tooling = match tooling.analyze(path, config) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Tooling analysis error: {}", e);
            return ExitCode::FAILURE;
        }
    };

    let v1_readiness = V1ReadinessAnalyzer::default();
    result.v1_readiness = match v1_readiness.analyze(path, config) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("V1 readiness analysis error: {}", e);
            return ExitCode::FAILURE;
        }
    };

    // Output results
    if format == "json" {
        match serde_json::to_string_pretty(&result) {
            Ok(json) => println!("{}", json),
            Err(e) => eprintln!("JSON serialization error: {}", e),
        }
    } else {
        print_summary(&result);
    }

    // Determine exit code
    if result.should_block_release() {
        ExitCode::FAILURE
    } else if strict && result.total_warnings() > 0 {
        ExitCode::from(2)
    } else {
        ExitCode::SUCCESS
    }
}

fn handle_fix(path: &PathBuf, config: &config::Config, only: Option<&str>) -> ExitCode {
    use crate::analyzers::{
        license::LicenseAnalyzer, placeholder::PlaceholderAnalyzer, Analyzer,
    };

    if only.is_none() || only == Some("license") {
        let license = LicenseAnalyzer::default();
        if let Ok(result) = license.analyze(path, config) {
            let fixed = license.fix(path, config, &result.findings);
            match fixed {
                Ok(f) => {
                    for msg in f {
                        println!("{}", msg);
                    }
                }
                Err(e) => eprintln!("Error fixing license issues: {}", e),
            }
        }
    }

    if only.is_none() || only == Some("placeholder") {
        let placeholder = PlaceholderAnalyzer::default();
        if let Ok(result) = placeholder.analyze(path, config) {
            let fixed = placeholder.fix(path, config, &result.findings);
            match fixed {
                Ok(f) => {
                    for msg in f {
                        println!("{}", msg);
                    }
                }
                Err(e) => eprintln!("Error fixing placeholder issues: {}", e),
            }
        }
    }

    ExitCode::SUCCESS
}

fn handle_license(path: &PathBuf, config: &config::Config, list_allowed: bool) -> ExitCode {
    if list_allowed {
        println!("Allowed licenses:");
        for license in &config.licenses.allowed {
            println!("  - {}", license);
        }
        return ExitCode::SUCCESS;
    }

    use crate::analyzers::{license::LicenseAnalyzer, Analyzer};

    let analyzer = LicenseAnalyzer::default();
    match analyzer.analyze(path, config) {
        Ok(result) => {
            print_analysis_result("LICENSE", &result);
            if result.has_errors() {
                ExitCode::FAILURE
            } else {
                ExitCode::SUCCESS
            }
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            ExitCode::FAILURE
        }
    }
}

fn handle_placeholders(path: &PathBuf, config: &config::Config, action: &str) -> ExitCode {
    use crate::analyzers::{placeholder::PlaceholderAnalyzer, Analyzer};
    use crate::config::PlaceholderAction;

    let mut config = config.clone();
    config.placeholders.action = match action {
        "remove" => PlaceholderAction::Remove,
        "comment" => PlaceholderAction::Comment,
        _ => PlaceholderAction::Flag,
    };

    let analyzer = PlaceholderAnalyzer::default();
    match analyzer.analyze(path, &config) {
        Ok(result) => {
            print_analysis_result("PLACEHOLDERS", &result);
            if result.has_errors() {
                ExitCode::FAILURE
            } else {
                ExitCode::SUCCESS
            }
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            ExitCode::FAILURE
        }
    }
}

fn handle_claims(path: &PathBuf, config: &config::Config) -> ExitCode {
    use crate::analyzers::{claims::ClaimsAnalyzer, Analyzer};

    let analyzer = ClaimsAnalyzer::default();
    match analyzer.analyze(path, config) {
        Ok(result) => {
            print_analysis_result("CLAIMS", &result);
            if result.has_errors() {
                ExitCode::FAILURE
            } else {
                ExitCode::SUCCESS
            }
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            ExitCode::FAILURE
        }
    }
}

fn handle_release(path: &PathBuf, config: &config::Config) -> ExitCode {
    use crate::analyzers::{release::ReleaseAnalyzer, Analyzer};

    let analyzer = ReleaseAnalyzer::default();
    match analyzer.analyze(path, config) {
        Ok(result) => {
            print_analysis_result("RELEASE", &result);
            if result.has_errors() {
                ExitCode::FAILURE
            } else {
                ExitCode::SUCCESS
            }
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            ExitCode::FAILURE
        }
    }
}

fn print_analysis_result(category: &str, result: &analyzers::AnalysisResult) {
    use crate::analyzers::Severity;

    println!();
    println!("╔════════════════════════════════════════════════════════════════╗");
    println!("║  {} ANALYSIS", category);
    println!("╠════════════════════════════════════════════════════════════════╣");
    println!(
        "║  Files checked: {:4}                                           ║",
        result.files_checked
    );
    println!(
        "║  Findings:      {:4} ({} errors, {} warnings)               ║",
        result.findings.len(),
        result.errors().len(),
        result.warnings().len()
    );
    println!("╚════════════════════════════════════════════════════════════════╝");
    println!();

    if result.findings.is_empty() {
        println!("✅ No issues found!");
        return;
    }

    for finding in &result.findings {
        let icon = match finding.severity {
            Severity::Error => "❌",
            Severity::Warning => "⚠️ ",
            Severity::Info => "ℹ️ ",
            Severity::Suggestion => "💡",
        };

        println!("{} [{}] {}", icon, finding.id, finding.name);
        println!("   {}", finding.message);

        if let Some(ref file) = finding.file {
            let loc = match (finding.line, finding.column) {
                (Some(l), Some(c)) => format!("{}:{}:{}", file.display(), l, c),
                (Some(l), None) => format!("{}:{}", file.display(), l),
                _ => file.display().to_string(),
            };
            println!("   📁 {}", loc);
        }

        if let Some(ref suggestion) = finding.suggestion {
            println!("   💡 {}", suggestion);
        }

        println!();
    }
}

fn print_summary(result: &analyzers::AuditResult) {
    use crate::analyzers::Severity;

    println!();
    println!("╔════════════════════════════════════════════════════════════════╗");
    println!("║              FINISHING-BOT RELEASE AUDIT SUMMARY               ║");
    println!("╠════════════════════════════════════════════════════════════════╣");
    println!(
        "║  License:        {:4} findings  ({:3} errors, {:3} warnings)     ║",
        result.license.findings.len(),
        result.license.errors().len(),
        result.license.warnings().len()
    );
    println!(
        "║  Placeholders:   {:4} findings  ({:3} errors, {:3} warnings)     ║",
        result.placeholder.findings.len(),
        result.placeholder.errors().len(),
        result.placeholder.warnings().len()
    );
    println!(
        "║  Claims:         {:4} findings  ({:3} errors, {:3} warnings)     ║",
        result.claims.findings.len(),
        result.claims.errors().len(),
        result.claims.warnings().len()
    );
    println!(
        "║  Release:        {:4} findings  ({:3} errors, {:3} warnings)     ║",
        result.release.findings.len(),
        result.release.errors().len(),
        result.release.warnings().len()
    );
    println!(
        "║  SCM Files:      {:4} findings  ({:3} errors, {:3} warnings)     ║",
        result.scm_files.findings.len(),
        result.scm_files.errors().len(),
        result.scm_files.warnings().len()
    );
    println!(
        "║  Testing:        {:4} findings  ({:3} errors, {:3} warnings)     ║",
        result.testing.findings.len(),
        result.testing.errors().len(),
        result.testing.warnings().len()
    );
    println!(
        "║  Tooling:        {:4} findings  ({:3} errors, {:3} warnings)     ║",
        result.tooling.findings.len(),
        result.tooling.errors().len(),
        result.tooling.warnings().len()
    );
    println!(
        "║  V1 Readiness:   {:4} findings  ({:3} errors, {:3} warnings)     ║",
        result.v1_readiness.findings.len(),
        result.v1_readiness.errors().len(),
        result.v1_readiness.warnings().len()
    );
    println!("╠════════════════════════════════════════════════════════════════╣");
    println!(
        "║  TOTAL:          {:4} findings  ({:3} errors, {:3} warnings)     ║",
        result.total_findings(),
        result.total_errors(),
        result.total_warnings()
    );
    println!("╠════════════════════════════════════════════════════════════════╣");

    if result.should_block_release() {
        println!("║  STATUS: ❌ NOT READY FOR RELEASE                              ║");
    } else if result.total_warnings() > 0 {
        println!("║  STATUS: ⚠️  READY WITH WARNINGS                                ║");
    } else {
        println!("║  STATUS: ✅ READY FOR RELEASE                                  ║");
    }

    println!("╚════════════════════════════════════════════════════════════════╝");
    println!();

    // Print detailed findings
    for finding in result.all_findings() {
        let icon = match finding.severity {
            Severity::Error => "❌",
            Severity::Warning => "⚠️ ",
            Severity::Info => "ℹ️ ",
            Severity::Suggestion => "💡",
        };

        println!("{} [{}] {}", icon, finding.id, finding.name);
        println!("   {}", finding.message);

        if let Some(ref file) = finding.file {
            println!("   📁 {}", file.display());
        }

        if let Some(ref suggestion) = finding.suggestion {
            println!("   💡 {}", suggestion);
        }

        println!();
    }
}
