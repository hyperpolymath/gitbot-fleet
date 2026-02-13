// SPDX-License-Identifier: PMPL-1.0-or-later

//! Seambot - Seam Hygiene Auditor
//!
//! Tracks, enforces, and detects drift in architectural boundaries (seams).
//!
//! Seambot is a governor and auditor, not a designer. It ensures that
//! declared seams remain explicit, stable, and correctly exercised over time.

use anyhow::Result;
use clap::{Parser, Subcommand};
use std::path::PathBuf;
use tracing::info;

mod checks;
mod fleet;
mod forge;
mod github;
mod hidden_channels;
mod register;
mod report;
mod seam;
mod security;

use forge::{create_forge_client, ForgeType};
use github::{GitHubAppConfig, GitHubClient};
use report::{OutputFormat, Reporter};

/// Seam hygiene auditor for architectural boundaries
#[derive(Parser, Debug)]
#[command(name = "seambot")]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Path to the repository root
    #[arg(short, long, default_value = ".")]
    path: PathBuf,

    /// Output format
    #[arg(short, long, default_value = "text")]
    format: OutputFormat,

    /// Verbose output
    #[arg(short, long)]
    verbose: bool,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Check seam hygiene (all checks)
    Check {
        /// Fail on warnings
        #[arg(long)]
        strict: bool,

        /// Output file (defaults to stdout)
        #[arg(short, long)]
        output: Option<PathBuf>,
    },

    /// Verify seam register completeness
    Register {
        /// Path to seam register (defaults to spec/seams/seam-register.json)
        #[arg(long)]
        register: Option<PathBuf>,
    },

    /// Check for seam drift between declared and observed interfaces
    Drift {
        /// Compare against a baseline file
        #[arg(long)]
        baseline: Option<PathBuf>,

        /// Update baseline after check
        #[arg(long)]
        update_baseline: bool,
    },

    /// Validate conformance examples
    Conformance {
        /// Specific seam to check (checks all if omitted)
        #[arg(long)]
        seam: Option<String>,
    },

    /// Generate a seam status report
    Report {
        /// Output file
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Include full details
        #[arg(long)]
        full: bool,
    },

    /// Initialize seam infrastructure in a repository
    Init {
        /// Force overwrite existing files
        #[arg(long)]
        force: bool,
    },

    /// Validate stage freeze has seam-freeze stamp
    FreezeCheck {
        /// Stage identifier (e.g., f1, f2)
        #[arg(long)]
        stage: String,
    },

    /// Detect hidden channels across seam boundaries
    HiddenChannels {
        /// Output file
        #[arg(short, long)]
        output: Option<PathBuf>,
    },

    /// GitHub App integration commands
    #[command(subcommand)]
    Github(GitHubCommands),

    /// Multi-forge integration commands (GitHub, GitLab, Bitbucket)
    #[command(subcommand)]
    Forge(ForgeCommands),
}

/// Forge type selection for multi-forge commands
#[derive(Debug, Clone, Copy, clap::ValueEnum)]
enum ForgeSelection {
    /// GitHub (default)
    Github,
    /// GitLab
    Gitlab,
    /// Bitbucket
    Bitbucket,
}

impl ForgeSelection {
    fn to_forge_type(self) -> ForgeType {
        match self {
            ForgeSelection::Github => ForgeType::GitHub,
            ForgeSelection::Gitlab => ForgeType::GitLab,
            ForgeSelection::Bitbucket => ForgeType::Bitbucket,
        }
    }
}

/// Auto-detect forge type from a git remote URL
fn detect_forge_from_url(url: &str) -> ForgeType {
    if url.contains("gitlab.com") || url.contains("gitlab") {
        ForgeType::GitLab
    } else if url.contains("bitbucket.org") || url.contains("bitbucket") {
        ForgeType::Bitbucket
    } else {
        ForgeType::GitHub
    }
}

/// Determine the forge type: explicit flag, auto-detect from git remote, or default
fn resolve_forge_type(explicit: Option<ForgeSelection>, repo_path: &std::path::Path) -> ForgeType {
    if let Some(selection) = explicit {
        return selection.to_forge_type();
    }

    // Try auto-detect from git remote
    if let Ok(output) = std::process::Command::new("git")
        .args(["remote", "get-url", "origin"])
        .current_dir(repo_path)
        .output()
    {
        if output.status.success() {
            let url = String::from_utf8_lossy(&output.stdout);
            return detect_forge_from_url(url.trim());
        }
    }

    ForgeType::GitHub
}

#[derive(Subcommand, Debug)]
enum GitHubCommands {
    /// Create a GitHub Check Run with seam check results
    CheckRun {
        /// Repository owner (e.g., "hyperpolymath")
        #[arg(long, env = "GITHUB_REPOSITORY_OWNER")]
        owner: String,

        /// Repository name (e.g., "seambot")
        #[arg(long, env = "GITHUB_REPOSITORY_NAME")]
        repo: String,

        /// Commit SHA to report on
        #[arg(long, env = "GITHUB_SHA")]
        sha: String,

        /// GitHub App ID
        #[arg(long, env = "GITHUB_APP_ID")]
        app_id: u64,

        /// Path to GitHub App private key PEM file
        #[arg(long, env = "GITHUB_APP_PRIVATE_KEY_PATH")]
        private_key: PathBuf,

        /// GitHub App installation ID
        #[arg(long, env = "GITHUB_APP_INSTALLATION_ID")]
        installation_id: u64,

        /// GitHub API base URL (for GitHub Enterprise)
        #[arg(long, env = "GITHUB_API_URL")]
        api_url: Option<String>,

        /// Fail on warnings
        #[arg(long)]
        strict: bool,
    },

    /// Post seam check results as a PR comment
    PrComment {
        /// Repository owner
        #[arg(long, env = "GITHUB_REPOSITORY_OWNER")]
        owner: String,

        /// Repository name
        #[arg(long, env = "GITHUB_REPOSITORY_NAME")]
        repo: String,

        /// Pull request number
        #[arg(long)]
        pr: u64,

        /// GitHub App ID
        #[arg(long, env = "GITHUB_APP_ID")]
        app_id: u64,

        /// Path to GitHub App private key PEM file
        #[arg(long, env = "GITHUB_APP_PRIVATE_KEY_PATH")]
        private_key: PathBuf,

        /// GitHub App installation ID
        #[arg(long, env = "GITHUB_APP_INSTALLATION_ID")]
        installation_id: u64,

        /// GitHub API base URL (for GitHub Enterprise)
        #[arg(long, env = "GITHUB_API_URL")]
        api_url: Option<String>,
    },

    /// Verify a GitHub webhook signature
    VerifyWebhook {
        /// Webhook secret
        #[arg(long, env = "GITHUB_WEBHOOK_SECRET")]
        secret: String,

        /// X-Hub-Signature-256 header value
        #[arg(long)]
        signature: String,

        /// Path to payload file (or - for stdin)
        #[arg(long)]
        payload: PathBuf,
    },
}

#[derive(Subcommand, Debug)]
enum ForgeCommands {
    /// Create a status check on a commit via the multi-forge abstraction
    CheckStatus {
        /// Repository owner (e.g., "hyperpolymath")
        #[arg(long, env = "FORGE_OWNER")]
        owner: String,

        /// Repository name (e.g., "seambot")
        #[arg(long, env = "FORGE_REPO")]
        repo: String,

        /// Commit SHA to report on
        #[arg(long, env = "FORGE_COMMIT_SHA")]
        sha: String,

        /// Forge type (auto-detected from git remote if not specified)
        #[arg(long, env = "FORGE_TYPE")]
        forge: Option<ForgeSelection>,

        /// API token for the forge
        #[arg(long, env = "FORGE_TOKEN")]
        token: String,

        /// Fail on warnings
        #[arg(long)]
        strict: bool,
    },

    /// Post a comment on a merge/pull request via the multi-forge abstraction
    Comment {
        /// Repository owner
        #[arg(long, env = "FORGE_OWNER")]
        owner: String,

        /// Repository name
        #[arg(long, env = "FORGE_REPO")]
        repo: String,

        /// Pull/Merge request number
        #[arg(long)]
        pr: u64,

        /// Forge type (auto-detected from git remote if not specified)
        #[arg(long, env = "FORGE_TYPE")]
        forge: Option<ForgeSelection>,

        /// API token for the forge
        #[arg(long, env = "FORGE_TOKEN")]
        token: String,
    },

    /// Create an issue with seam check findings via the multi-forge abstraction
    Issue {
        /// Repository owner
        #[arg(long, env = "FORGE_OWNER")]
        owner: String,

        /// Repository name
        #[arg(long, env = "FORGE_REPO")]
        repo: String,

        /// Forge type (auto-detected from git remote if not specified)
        #[arg(long, env = "FORGE_TYPE")]
        forge: Option<ForgeSelection>,

        /// API token for the forge
        #[arg(long, env = "FORGE_TOKEN")]
        token: String,

        /// Issue title
        #[arg(long, default_value = "Seambot: Seam hygiene findings")]
        title: String,

        /// Fail on warnings
        #[arg(long)]
        strict: bool,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize logging
    let log_level = if cli.verbose { "debug" } else { "info" };
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| format!("seambot={}", log_level).into()),
        )
        .init();

    info!("Seambot v{}", env!("CARGO_PKG_VERSION"));

    let repo_path = cli.path.canonicalize()?;
    info!("Analyzing repository: {}", repo_path.display());

    match cli.command {
        Commands::Check { strict, output } => {
            let result = checks::run_all_checks(&repo_path).await?;
            let reporter = Reporter::new(cli.format);
            reporter.output(&result, output.as_deref())?;

            if result.has_errors() || (strict && result.has_warnings()) {
                std::process::exit(1);
            }
        }

        Commands::Register { register } => {
            let register_path = register.unwrap_or_else(|| {
                repo_path.join("spec/seams/seam-register.json")
            });
            let result = checks::check_register(&repo_path, &register_path).await?;
            let reporter = Reporter::new(cli.format);
            reporter.output(&result, None)?;

            if result.has_errors() {
                std::process::exit(1);
            }
        }

        Commands::Drift { baseline, update_baseline } => {
            let result = checks::check_drift(&repo_path, baseline.as_deref()).await?;
            let reporter = Reporter::new(cli.format);
            reporter.output(&result, None)?;

            if update_baseline {
                checks::update_drift_baseline(&repo_path).await?;
                info!("Updated drift baseline");
            }

            if result.has_errors() {
                std::process::exit(1);
            }
        }

        Commands::Conformance { seam } => {
            let result = checks::check_conformance(&repo_path, seam.as_deref()).await?;
            let reporter = Reporter::new(cli.format);
            reporter.output(&result, None)?;

            if result.has_errors() {
                std::process::exit(1);
            }
        }

        Commands::Report { output, full } => {
            let result = checks::generate_report(&repo_path, full).await?;
            let reporter = Reporter::new(cli.format);
            reporter.output(&result, output.as_deref())?;
        }

        Commands::Init { force } => {
            register::init_seam_infrastructure(&repo_path, force).await?;
            info!("Initialized seam infrastructure");
        }

        Commands::FreezeCheck { stage } => {
            let result = checks::check_freeze_stamp(&repo_path, &stage).await?;
            let reporter = Reporter::new(cli.format);
            reporter.output(&result, None)?;

            if result.has_errors() {
                std::process::exit(1);
            }
        }

        Commands::HiddenChannels { output } => {
            // Load seam register
            let register_path = repo_path.join("spec/seams/seam-register.json");
            let register = register::load_register(&register_path)?;

            // Detect hidden channels
            let channels = hidden_channels::detect_hidden_channels(&register, &repo_path)?;

            // Publish findings to fleet shared context
            let mut ctx = fleet::load_or_create_context(&repo_path)?;
            fleet::publish_findings(&mut ctx, &register, &channels, 0, 0)?;
            info!("Published {} findings to fleet shared context", channels.len());

            // Format output
            if channels.is_empty() {
                info!("No hidden channels detected");
            } else {
                println!("Found {} hidden channels:\n", channels.len());
                for channel in &channels {
                    let severity = match channel.severity {
                        hidden_channels::Severity::Critical => "CRITICAL",
                        hidden_channels::Severity::High => "HIGH",
                        hidden_channels::Severity::Medium => "MEDIUM",
                        hidden_channels::Severity::Low => "LOW",
                    };
                    let channel_type = match channel.channel_type {
                        hidden_channels::ChannelType::UndeclaredImport => "Undeclared Import",
                        hidden_channels::ChannelType::GlobalState => "Global State",
                        hidden_channels::ChannelType::FilesystemCoupling => "Filesystem Coupling",
                        hidden_channels::ChannelType::DatabaseCoupling => "Database Coupling",
                        hidden_channels::ChannelType::NetworkCoupling => "Network Coupling",
                    };
                    println!("[{}] {}: {} -> {}",
                        severity, channel_type, channel.source_seam, channel.target_seam);
                    println!("  Evidence: {}\n", channel.evidence);
                }

                // Save to file if requested
                if let Some(output_path) = output {
                    let json = serde_json::to_string_pretty(&channels)?;
                    std::fs::write(&output_path, json)?;
                    info!("Saved report to {}", output_path.display());
                }

                std::process::exit(1);
            }
        }

        Commands::Github(github_cmd) => {
            match github_cmd {
                GitHubCommands::CheckRun {
                    owner,
                    repo,
                    sha,
                    app_id,
                    private_key,
                    installation_id,
                    api_url,
                    strict,
                } => {
                    // Run seam checks
                    let result = checks::run_all_checks(&repo_path).await?;

                    // Configure GitHub client
                    let mut config = GitHubAppConfig::new(
                        app_id,
                        private_key.display().to_string(),
                        installation_id,
                    );
                    if let Some(url) = api_url {
                        config = config.with_enterprise_url(url);
                    }

                    let mut client = GitHubClient::new(config)?;

                    // Create check run
                    let check_run = client
                        .create_check_run(&owner, &repo, &sha, "seambot")
                        .await?;

                    info!("Created check run: {}", check_run.html_url);

                    // Update with results
                    let updated = client
                        .update_check_run(&owner, &repo, check_run.id, &result)
                        .await?;

                    info!(
                        "Check run completed: {} (conclusion: {:?})",
                        updated.html_url, updated.conclusion
                    );

                    // Also output locally
                    let reporter = Reporter::new(cli.format);
                    reporter.output(&result, None)?;

                    if result.has_errors() || (strict && result.has_warnings()) {
                        std::process::exit(1);
                    }
                }

                GitHubCommands::PrComment {
                    owner,
                    repo,
                    pr,
                    app_id,
                    private_key,
                    installation_id,
                    api_url,
                } => {
                    // Run seam checks
                    let result = checks::run_all_checks(&repo_path).await?;

                    // Configure GitHub client
                    let mut config = GitHubAppConfig::new(
                        app_id,
                        private_key.display().to_string(),
                        installation_id,
                    );
                    if let Some(url) = api_url {
                        config = config.with_enterprise_url(url);
                    }

                    let mut client = GitHubClient::new(config)?;

                    // Post comment
                    let comment = client
                        .post_pr_comment(&owner, &repo, pr, &result)
                        .await?;

                    info!("Posted comment: {}", comment.html_url);

                    // Also output locally
                    let reporter = Reporter::new(cli.format);
                    reporter.output(&result, None)?;
                }

                GitHubCommands::VerifyWebhook {
                    secret,
                    signature,
                    payload,
                } => {
                    let payload_bytes = std::fs::read(&payload)?;
                    let valid = github::verify_webhook_signature(
                        &payload_bytes,
                        &signature,
                        &secret,
                    );

                    if valid {
                        info!("Webhook signature is valid");
                        println!("valid");
                    } else {
                        info!("Webhook signature is INVALID");
                        println!("invalid");
                        std::process::exit(1);
                    }
                }
            }
        }

        Commands::Forge(forge_cmd) => {
            match forge_cmd {
                ForgeCommands::CheckStatus {
                    owner,
                    repo,
                    sha,
                    forge,
                    token,
                    strict,
                } => {
                    let forge_type = resolve_forge_type(forge, &repo_path);
                    info!("Using forge: {:?}", forge_type);

                    // Run seam checks
                    let result = checks::run_all_checks(&repo_path).await?;

                    // Create forge client and post status
                    let client = create_forge_client(forge_type, &token);

                    let conclusion = if result.has_errors() {
                        forge::Conclusion::Failure
                    } else if result.has_warnings() {
                        forge::Conclusion::Neutral
                    } else {
                        forge::Conclusion::Success
                    };

                    let status = forge::CheckStatus {
                        name: "seambot".to_string(),
                        conclusion,
                        title: format!("Seam hygiene: {:?}", result.status),
                        summary: format!(
                            "{} errors, {} warnings, {} info across {} seams",
                            result.summary.errors,
                            result.summary.warnings,
                            result.summary.info,
                            result.summary.total_seams,
                        ),
                    };

                    let check_response = client
                        .update_check(&owner, &repo, &sha, status)
                        .await?;

                    info!("Posted check status (ID: {}): {}", check_response.id, check_response.status);

                    // Also output locally
                    let reporter = Reporter::new(cli.format);
                    reporter.output(&result, None)?;

                    if result.has_errors() || (strict && result.has_warnings()) {
                        std::process::exit(1);
                    }
                }

                ForgeCommands::Comment {
                    owner,
                    repo,
                    pr,
                    forge,
                    token,
                } => {
                    let forge_type = resolve_forge_type(forge, &repo_path);
                    info!("Using forge: {:?}", forge_type);

                    // Run seam checks
                    let result = checks::run_all_checks(&repo_path).await?;

                    // Build comment body
                    let body = format!(
                        "## Seambot Check Results\n\n\
                         | Metric | Value |\n\
                         |--------|-------|\n\
                         | Status | {:?} |\n\
                         | Seams Checked | {}/{} |\n\
                         | Errors | {} |\n\
                         | Warnings | {} |\n\
                         | Info | {} |\n\n\
                         ---\n*Generated by [seambot](https://github.com/hyperpolymath/seambot)*",
                        result.status,
                        result.summary.checked_seams,
                        result.summary.total_seams,
                        result.summary.errors,
                        result.summary.warnings,
                        result.summary.info,
                    );

                    let client = create_forge_client(forge_type, &token);
                    let comment = client
                        .add_comment(&owner, &repo, pr, &body)
                        .await?;

                    info!("Posted comment (ID: {}): {}", comment.id, comment.url);

                    // Also output locally
                    let reporter = Reporter::new(cli.format);
                    reporter.output(&result, None)?;
                }

                ForgeCommands::Issue {
                    owner,
                    repo,
                    forge,
                    token,
                    title,
                    strict,
                } => {
                    let forge_type = resolve_forge_type(forge, &repo_path);
                    info!("Using forge: {:?}", forge_type);

                    // Run seam checks
                    let result = checks::run_all_checks(&repo_path).await?;

                    // Build issue body
                    let body = format!(
                        "## Seam Hygiene Audit Report\n\n\
                         | Metric | Value |\n\
                         |--------|-------|\n\
                         | Status | {:?} |\n\
                         | Seams Checked | {}/{} |\n\
                         | Errors | {} |\n\
                         | Warnings | {} |\n\
                         | Info | {} |\n\n\
                         ---\n*Generated by [seambot](https://github.com/hyperpolymath/seambot)*",
                        result.status,
                        result.summary.checked_seams,
                        result.summary.total_seams,
                        result.summary.errors,
                        result.summary.warnings,
                        result.summary.info,
                    );

                    let labels: Vec<&str> = vec!["seambot", "architecture"];
                    let client = create_forge_client(forge_type, &token);
                    let issue = client
                        .create_issue(&owner, &repo, &title, &body, &labels)
                        .await?;

                    info!("Created issue #{} (ID: {}): {}", issue.number, issue.id, issue.url);

                    // Also output locally
                    let reporter = Reporter::new(cli.format);
                    reporter.output(&result, None)?;

                    if result.has_errors() || (strict && result.has_warnings()) {
                        std::process::exit(1);
                    }
                }
            }
        }
    }

    Ok(())
}
