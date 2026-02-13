// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! # SustainaBot CLI
//!
//! Ecological and economic code analysis tool.
//! Built with Eclexia principles - proving resource-aware design works.

use anyhow::Result;
use clap::{Parser, Subcommand};
use std::fs;
use std::path::PathBuf;
use sustainabot_analysis::analyze_file;
use tracing::info;
use walkdir::WalkDir;

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Parser)]
#[command(name = "sustainabot")]
#[command(about = "Ecological & Economic Code Analysis", long_about = None)]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Enable verbose logging
    #[arg(short, long, global = true)]
    verbose: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Analyze a single file
    Analyze {
        /// File to analyze
        file: PathBuf,

        /// Output format (text, json, sarif)
        #[arg(short, long, default_value = "text")]
        format: String,

        /// Write output to file instead of stdout
        #[arg(short, long)]
        output: Option<PathBuf>,
    },

    /// Analyze a directory recursively
    Check {
        /// Directory to check
        path: PathBuf,

        /// Minimum eco score threshold (0-100)
        #[arg(long, default_value = "50")]
        eco_threshold: f64,

        /// Output format (text, json, sarif)
        #[arg(short, long, default_value = "text")]
        format: String,

        /// Write output to file instead of stdout
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Include security-sustainability correlation (requires panic-attack feature)
        #[arg(long)]
        security: bool,

        /// Directory containing Eclexia policy files (.ecl)
        #[arg(long)]
        policy_dir: Option<PathBuf>,
    },

    /// Generate a full report for a directory (alias for check with defaults)
    Report {
        /// Directory to analyze
        path: PathBuf,

        /// Output format (text, json, sarif)
        #[arg(short, long, default_value = "sarif")]
        format: String,

        /// Write output to file instead of stdout
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Minimum eco score threshold (0-100)
        #[arg(long, default_value = "50")]
        eco_threshold: f64,

        /// Include security-sustainability correlation (requires panic-attack feature)
        #[arg(long)]
        security: bool,

        /// Directory containing Eclexia policy files (.ecl)
        #[arg(long)]
        policy_dir: Option<PathBuf>,
    },

    /// Run as a gitbot-fleet member
    Fleet {
        /// Repository path to analyze
        path: PathBuf,

        /// Path to shared context JSON file
        #[arg(short, long)]
        context: Option<PathBuf>,
    },

    /// Show analysis of sustainabot itself (dogfooding!)
    SelfAnalyze,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    // Set up logging
    let log_level = if cli.verbose { "debug" } else { "info" };
    tracing_subscriber::fmt()
        .with_env_filter(log_level)
        .init();

    match cli.command {
        Commands::Analyze {
            file,
            format,
            output,
        } => {
            info!("Analyzing file: {}", file.display());
            let results = analyze_file(&file)?;
            emit_output(&results, &format, output.as_deref())?;
        }

        Commands::Check {
            path,
            eco_threshold,
            format,
            output,
            security,
            policy_dir,
        }
        | Commands::Report {
            path,
            format,
            output,
            eco_threshold,
            security,
            policy_dir,
        } => {
            info!("Checking directory: {}", path.display());

            let mut all_results = collect_directory_results(&path)?;

            // Security-sustainability correlation
            if security {
                run_security_correlation(&path, &mut all_results);
            }

            // Policy evaluation
            if let Some(ref pdir) = policy_dir {
                run_policy_evaluation(pdir, &mut all_results);
            }

            // Emit formatted output
            match format.as_str() {
                "sarif" | "json" => {
                    emit_output(&all_results, &format, output.as_deref())?;
                }
                "text" => {
                    println!(
                        "Checking directory: {} (eco threshold: {})\n",
                        path.display(),
                        eco_threshold
                    );

                    let mut files_below_threshold = 0u32;
                    for result in &all_results {
                        if result.health.eco_score.0 < eco_threshold {
                            files_below_threshold += 1;
                            println!(
                                "  BELOW THRESHOLD: {} :: {} (eco: {:.1}, threshold: {})",
                                result.location.file,
                                result.location.name.as_deref().unwrap_or("<anon>"),
                                result.health.eco_score.0,
                                eco_threshold
                            );
                        }
                    }

                    print_summary(&all_results, eco_threshold, files_below_threshold);

                    if let Some(ref out_path) = output {
                        // Also write text summary to file
                        let text = format_results_text(&all_results);
                        fs::write(out_path, text)?;
                        println!("\nOutput written to: {}", out_path.display());
                    }

                    if files_below_threshold > 0 {
                        std::process::exit(1);
                    }
                }
                _ => {
                    eprintln!("Unsupported format: {}", format);
                }
            }
        }

        Commands::Fleet { path, context } => {
            info!("Running fleet analysis: {}", path.display());
            sustainabot_fleet::run_fleet_analysis(
                &path,
                context.as_deref(),
            )?;
        }

        Commands::SelfAnalyze => {
            println!("SustainaBot Self-Analysis (Dogfooding!)");
            println!("==========================================\n");
            println!("Analyzing sustainabot's own resource usage...\n");

            let analyzer_src = PathBuf::from("crates/sustainabot-analysis/src/analyzer.rs");
            if analyzer_src.exists() {
                let results = analyze_file(&analyzer_src)?;
                print_results_text(&results);

                println!("\nMeta-Analysis:");
                println!("This analyzer used minimal resources to analyze itself.");
                println!("Eclexia-inspired design: explicit resource tracking from day 1.");
            } else {
                println!("Run from sustainabot repository root.");
            }
        }
    }

    Ok(())
}

/// Collect analysis results from all supported files in a directory
fn collect_directory_results(
    path: &std::path::Path,
) -> Result<Vec<sustainabot_metrics::AnalysisResult>> {
    let mut all_results = Vec::new();

    for entry in WalkDir::new(path)
        .follow_links(false)
        .into_iter()
        .filter_entry(|e| {
            let name = e.file_name().to_str().unwrap_or("");
            !matches!(
                name,
                "target" | "node_modules" | ".git" | "dist" | "build" | ".cache"
            )
        })
        .filter_map(|e| e.ok())
    {
        let entry_path = entry.path();
        if !entry_path.is_file() {
            continue;
        }

        let ext = entry_path
            .extension()
            .and_then(|e| e.to_str())
            .unwrap_or("");
        if !matches!(ext, "rs" | "js" | "py") {
            continue;
        }

        match analyze_file(entry_path) {
            Ok(results) => {
                all_results.extend(results);
            }
            Err(e) => {
                info!("Skipping {}: {}", entry_path.display(), e);
            }
        }
    }

    Ok(all_results)
}

/// Emit analysis results in the requested format
fn emit_output(
    results: &[sustainabot_metrics::AnalysisResult],
    format: &str,
    output: Option<&std::path::Path>,
) -> Result<()> {
    let text = match format {
        "sarif" => sustainabot_sarif::to_sarif_json(results, VERSION)?,
        "json" => serde_json::to_string_pretty(results)?,
        "text" => {
            print_results_text(results);
            return Ok(());
        }
        other => {
            eprintln!("Unsupported format: {}", other);
            return Ok(());
        }
    };

    match output {
        Some(path) => {
            fs::write(path, &text)?;
            eprintln!("Output written to: {}", path.display());
        }
        None => {
            println!("{}", text);
        }
    }

    Ok(())
}

fn print_summary(
    all_results: &[sustainabot_metrics::AnalysisResult],
    eco_threshold: f64,
    files_below_threshold: u32,
) {
    let total_files = all_results
        .iter()
        .map(|r| r.location.file.as_str())
        .collect::<std::collections::HashSet<_>>()
        .len();

    println!("\n--- Summary ---");
    println!("Files analyzed:        {}", total_files);
    println!("Functions found:       {}", all_results.len());
    println!("Below threshold:       {}", files_below_threshold);

    if !all_results.is_empty() {
        let avg_eco: f64 =
            all_results.iter().map(|r| r.health.eco_score.0).sum::<f64>() / all_results.len() as f64;
        let avg_overall: f64 =
            all_results.iter().map(|r| r.health.overall).sum::<f64>() / all_results.len() as f64;
        let total_energy: f64 = all_results.iter().map(|r| r.resources.energy.0).sum();
        let total_carbon: f64 = all_results.iter().map(|r| r.resources.carbon.0).sum();

        println!("Avg eco score:         {:.1}/100", avg_eco);
        println!("Avg overall health:    {:.1}/100", avg_overall);
        println!("Total est. energy:     {:.2} J", total_energy);
        println!("Total est. carbon:     {:.4} gCO2e", total_carbon);
    }

    if files_below_threshold > 0 {
        println!(
            "\nResult: FAIL ({} functions below eco threshold {})",
            files_below_threshold, eco_threshold
        );
    } else {
        println!(
            "\nResult: PASS (all functions meet eco threshold {})",
            eco_threshold
        );
    }
}

fn print_results_text(results: &[sustainabot_metrics::AnalysisResult]) {
    for result in results {
        println!(
            "\nFunction: {}",
            result.location.name.as_deref().unwrap_or("<anonymous>")
        );
        println!(
            "   Location: {}:{}:{}",
            result.location.file, result.location.line, result.location.column
        );
        println!("\n   Resources:");
        println!("     Energy:   {:.2} J", result.resources.energy.0);
        println!("     Time:     {:.2} ms", result.resources.duration.0);
        println!("     Carbon:   {:.4} gCO2e", result.resources.carbon.0);
        println!("     Memory:   {} bytes", result.resources.memory.0);

        println!("\n   Health Index:");
        println!("     Eco:      {:.1}/100", result.health.eco_score.0);
        println!("     Econ:     {:.1}/100", result.health.econ_score.0);
        println!("     Quality:  {:.1}/100", result.health.quality_score);
        println!("     Overall:  {:.1}/100", result.health.overall);

        if !result.recommendations.is_empty() {
            println!("\n   Recommendations:");
            for rec in &result.recommendations {
                println!("     - {}", rec);
            }
        }
    }

    println!("\nAnalysis complete");
}

/// Run Eclexia policy evaluation
fn run_policy_evaluation(
    policy_dir: &std::path::Path,
    results: &mut Vec<sustainabot_metrics::AnalysisResult>,
) {
    match sustainabot_eclexia::evaluate_policies(policy_dir, results) {
        Ok(decisions) => {
            let warns = decisions
                .iter()
                .filter(|d| d.outcome != sustainabot_eclexia::PolicyOutcome::Pass)
                .count();
            eprintln!(
                "Policy evaluation: {} policies, {} warnings/failures",
                decisions.len(),
                warns
            );
            for d in &decisions {
                if d.outcome != sustainabot_eclexia::PolicyOutcome::Pass {
                    eprintln!("  {:?}: {} - {}", d.outcome, d.policy_name, d.message);
                }
            }
            // Convert policy decisions to analysis results for SARIF output
            let policy_results = sustainabot_eclexia::decisions_to_results(&decisions);
            results.extend(policy_results);
        }
        Err(e) => {
            eprintln!("Policy evaluation failed: {}", e);
        }
    }
}

/// Run security-sustainability correlation if the feature is available
fn run_security_correlation(
    path: &std::path::Path,
    _results: &mut Vec<sustainabot_metrics::AnalysisResult>,
) {
    // Check for .bot_directives/panic-attack.scm
    let directive = sustainabot_analysis::directives::check_directive(path, "panic-attack");

    match directive {
        Some(ref d) if !d.allow => {
            eprintln!(
                "Security scan denied by .bot_directives/panic-attack.scm: {}",
                d.notes.as_deref().unwrap_or("no reason given")
            );
            return;
        }
        None => {
            eprintln!(
                "Warning: No .bot_directives/panic-attack.scm found in {}. \
                 Running security scan anyway.",
                path.display()
            );
        }
        _ => {}
    }

    #[cfg(feature = "security")]
    {
        match sustainabot_analysis::security::correlate(path, results) {
            Ok(correlation) => {
                eprintln!(
                    "Security scan: {} findings, composite score: {:.1}",
                    correlation.security_findings.len(),
                    correlation.composite_score
                );
                results.extend(correlation.security_findings);
            }
            Err(e) => {
                eprintln!("Security scan failed: {}", e);
            }
        }
    }

    #[cfg(not(feature = "security"))]
    {
        eprintln!(
            "Security correlation unavailable: build with --features security"
        );
    }
}

fn format_results_text(results: &[sustainabot_metrics::AnalysisResult]) -> String {
    let mut out = String::new();

    for result in results {
        out.push_str(&format!(
            "\nFunction: {}\n",
            result.location.name.as_deref().unwrap_or("<anonymous>")
        ));
        out.push_str(&format!(
            "   Location: {}:{}:{}\n",
            result.location.file, result.location.line, result.location.column
        ));
        out.push_str(&format!("   Energy: {:.2} J\n", result.resources.energy.0));
        out.push_str(&format!(
            "   Carbon: {:.4} gCO2e\n",
            result.resources.carbon.0
        ));
        out.push_str(&format!(
            "   Eco: {:.1}/100  Overall: {:.1}/100\n",
            result.health.eco_score.0, result.health.overall
        ));
    }

    out
}
