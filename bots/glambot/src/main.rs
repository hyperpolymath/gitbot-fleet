// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! glambot CLI - Presentation Quality Enforcer

mod analyzers;
mod config;
mod error;

use clap::{Parser, Subcommand};
use std::path::PathBuf;
use std::process::ExitCode;
use tracing_subscriber::{fmt, EnvFilter};

/// glambot: Presentation Quality Enforcer
///
/// Ensures repositories meet high standards for visual polish, accessibility,
/// SEO, and machine-readability.
#[derive(Parser)]
#[command(name = "glambot")]
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
    /// Run full presentation quality audit
    Audit {
        /// Fail on any warnings (strict mode)
        #[arg(long)]
        strict: bool,
    },

    /// Apply automatic fixes
    Fix {
        /// Only fix specific category (visual, accessibility, seo, machine)
        #[arg(long)]
        only: Option<String>,
    },

    /// Check visual polish only
    Visual,

    /// Check accessibility (WCAG) only
    Accessibility,

    /// Check SEO only
    Seo,

    /// Check machine-readability only
    Machine,

    /// Initialize configuration file
    Init {
        /// Output format (yaml, toml)
        #[arg(long, default_value = "yaml")]
        format: String,
    },

    /// Show current configuration
    Show,
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
        Some(Command::Init { format }) => handle_init(&config_path, &format),
        Some(Command::Show) => handle_show(&config),
        Some(Command::Audit { strict }) => handle_audit(&cli.path, &config, strict, &cli.format),
        Some(Command::Fix { only }) => handle_fix(&cli.path, &config, only.as_deref()),
        Some(Command::Visual) => handle_visual(&cli.path, &config),
        Some(Command::Accessibility) => handle_accessibility(&cli.path, &config),
        Some(Command::Seo) => handle_seo(&cli.path, &config),
        Some(Command::Machine) => handle_machine(&cli.path, &config),
        None => {
            // Default to audit
            handle_audit(&cli.path, &config, false, &cli.format)
        }
    }
}

fn init_logging(level: &str) {
    let filter = EnvFilter::try_new(level).unwrap_or_else(|_| EnvFilter::new("info"));

    fmt()
        .with_target(true)
        .with_ansi(true)
        .with_env_filter(filter)
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

    println!("Visual:");
    println!("  Line length: {}", config.visual.max_line_length);
    println!("  Enforce badges: {}", config.visual.enforce_badges);
    println!();

    println!("Accessibility:");
    println!("  WCAG level: {:?}", config.accessibility.wcag_level);
    println!("  Require alt text: {}", config.accessibility.require_alt_text);
    println!();

    println!("SEO:");
    println!("  Require meta tags: {}", config.seo.require_meta_tags);
    println!("  Require OpenGraph: {}", config.seo.require_opengraph);
    println!();

    println!("Machine:");
    println!("  Validate JSON: {}", config.machine.validate_json);
    println!("  Require schema.org: {}", config.machine.require_schema_org);
    println!();

    println!("General:");
    println!("  Dry run: {}", config.dry_run);
    println!("  Auto fix: {}", config.auto_fix);

    ExitCode::SUCCESS
}

fn handle_audit(path: &PathBuf, config: &config::Config, strict: bool, format: &str) -> ExitCode {
    use crate::analyzers::{
        accessibility::AccessibilityAnalyzer, git_seo_integration::GitSeoAnalyzer,
        machine::MachineAnalyzer, seo::SeoAnalyzer, visual::VisualAnalyzer, Analyzer,
        AuditResult,
    };

    let mut result = AuditResult::default();

    // Run all analyzers
    let visual = VisualAnalyzer::default();
    result.visual = match visual.analyze(path, config) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Visual analysis error: {}", e);
            return ExitCode::FAILURE;
        }
    };

    let accessibility = AccessibilityAnalyzer::default();
    result.accessibility = match accessibility.analyze(path, config) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Accessibility analysis error: {}", e);
            return ExitCode::FAILURE;
        }
    };

    let seo = SeoAnalyzer::default();
    result.seo = match seo.analyze(path, config) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("SEO analysis error: {}", e);
            return ExitCode::FAILURE;
        }
    };

    let machine = MachineAnalyzer::default();
    result.machine = match machine.analyze(path, config) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Machine-readability analysis error: {}", e);
            return ExitCode::FAILURE;
        }
    };

    // Run Git-SEO integration if enabled
    if config.seo.enable_git_seo.unwrap_or(true) {
        let git_seo = GitSeoAnalyzer::default();
        result.git_seo = match git_seo.analyze(path, config) {
            Ok(r) => r,
            Err(e) => {
                eprintln!("Git-SEO analysis error: {}", e);
                // Don't fail if git-seo is unavailable, just skip
                Default::default()
            }
        };
    }

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

fn handle_fix(path: &PathBuf, config: &config::Config, _only: Option<&str>) -> ExitCode {
    println!("Auto-fix not yet implemented for glambot");
    println!("Path: {}", path.display());
    println!("Config: dry_run={}", config.dry_run);
    ExitCode::SUCCESS
}

fn handle_visual(path: &PathBuf, config: &config::Config) -> ExitCode {
    use crate::analyzers::{visual::VisualAnalyzer, Analyzer};

    let analyzer = VisualAnalyzer::default();
    match analyzer.analyze(path, config) {
        Ok(result) => {
            print_analysis_result("VISUAL POLISH", &result);
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

fn handle_accessibility(path: &PathBuf, config: &config::Config) -> ExitCode {
    use crate::analyzers::{accessibility::AccessibilityAnalyzer, Analyzer};

    let analyzer = AccessibilityAnalyzer::default();
    match analyzer.analyze(path, config) {
        Ok(result) => {
            print_analysis_result("ACCESSIBILITY (WCAG)", &result);
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

fn handle_seo(path: &PathBuf, config: &config::Config) -> ExitCode {
    use crate::analyzers::{seo::SeoAnalyzer, Analyzer};

    let analyzer = SeoAnalyzer::default();
    match analyzer.analyze(path, config) {
        Ok(result) => {
            print_analysis_result("SEO", &result);
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

fn handle_machine(path: &PathBuf, config: &config::Config) -> ExitCode {
    use crate::analyzers::{machine::MachineAnalyzer, Analyzer};

    let analyzer = MachineAnalyzer::default();
    match analyzer.analyze(path, config) {
        Ok(result) => {
            print_analysis_result("MACHINE-READABILITY", &result);
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
    println!("║                GLAMBOT PRESENTATION AUDIT SUMMARY              ║");
    println!("╠════════════════════════════════════════════════════════════════╣");
    println!(
        "║  Visual:         {:4} findings  ({:3} errors, {:3} warnings)     ║",
        result.visual.findings.len(),
        result.visual.errors().len(),
        result.visual.warnings().len()
    );
    println!(
        "║  Accessibility:  {:4} findings  ({:3} errors, {:3} warnings)     ║",
        result.accessibility.findings.len(),
        result.accessibility.errors().len(),
        result.accessibility.warnings().len()
    );
    println!(
        "║  SEO:            {:4} findings  ({:3} errors, {:3} warnings)     ║",
        result.seo.findings.len(),
        result.seo.errors().len(),
        result.seo.warnings().len()
    );
    println!(
        "║  Machine:        {:4} findings  ({:3} errors, {:3} warnings)     ║",
        result.machine.findings.len(),
        result.machine.errors().len(),
        result.machine.warnings().len()
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
