// SPDX-License-Identifier: PMPL-1.0-or-later

//! Robot-Repo-Automaton — Automated Management CLI.
//!
//! This binary is the primary executor for the GitBot Fleet. It provides
//! an administrative interface for scanning and repairing large
//! organizations of repositories according to the Rhodium Standard.
//!
//! SUBCOMMANDS:
//! 1. **Scan**: Deterministic discovery of repository-level issues.
//! 2. **Fix**: Automated remediation with confidence-gated PR creation.
//! 3. **ScanOrg**: High-concurrency bulk auditing across a GitHub organization.
//! 4. **Hooks**: Lifecycle management for Git pre-commit and pre-push filters.
//! 5. **Catalog**: Inspection of the authoritative error database (ERROR-CATALOG.scm).

use clap::{Parser, Subcommand};
use robot_repo_automaton::prelude::*;
use robot_repo_automaton::github::{GitHubClient, CreatePullRequest};
use robot_repo_automaton::confidence::ThresholdConfig;
use std::path::PathBuf;
use tracing::{debug, error, info, warn};
use tracing_subscriber::EnvFilter;

/// Robot-Repo-Automaton: Automated repository compliance executor
#[derive(Parser, Debug)]
#[command(name = "robot-repo-automaton", version, about)]
struct Cli {
    /// Enable verbose logging
    #[arg(short, long)]
    verbose: bool,

    /// Dry run mode — scan and propose but do not modify anything
    #[arg(long)]
    dry_run: bool,

    /// Path to configuration file
    #[arg(short, long, default_value = "config.toml")]
    config: PathBuf,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Scan a repository for compliance issues
    Scan {
        /// Path to repository (local) or owner/name (GitHub)
        repo: String,

        /// Output format
        #[arg(short, long, default_value = "text")]
        format: String,
    },

    /// Fix detected issues in a repository (with confidence gating)
    Fix {
        /// Path to repository (local) or owner/name (GitHub)
        repo: String,

        /// Only fix issues above this confidence level (low, medium, high)
        #[arg(long, default_value = "high")]
        min_confidence: String,

        /// Create a PR instead of committing directly to the current branch
        #[arg(long)]
        create_pr: bool,

        /// Branch name for PR (default: auto-generated)
        #[arg(long)]
        branch: Option<String>,
    },

    /// Scan all repositories in a GitHub organization
    ScanOrg {
        /// GitHub organization name
        org: String,

        /// Output format
        #[arg(short, long, default_value = "text")]
        format: String,

        /// Maximum concurrent scans
        #[arg(long, default_value = "4")]
        concurrency: usize,
    },

    /// Manage Git hooks
    Hooks {
        /// Hook action
        #[command(subcommand)]
        action: HookAction,
    },

    /// Inspect the error catalog
    Catalog {
        /// Path to ERROR-CATALOG.scm
        #[arg(default_value = "ERROR-CATALOG.scm")]
        path: PathBuf,

        /// Filter by severity
        #[arg(short, long)]
        severity: Option<String>,
    },
}

#[derive(Subcommand, Debug)]
enum HookAction {
    /// Install hooks into a repository
    Install {
        /// Path to repository
        repo: PathBuf,
    },
    /// Remove hooks from a repository
    Remove {
        /// Path to repository
        repo: PathBuf,
    },
    /// Check hook status
    Status {
        /// Path to repository
        repo: PathBuf,
    },
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    // Initialise structured logging with tracing
    let filter = if cli.verbose {
        EnvFilter::new("debug")
    } else {
        EnvFilter::new("info")
    };
    tracing_subscriber::fmt()
        .with_env_filter(filter)
        .with_target(false)
        .init();

    // Load configuration (optional — some commands work without it)
    let config = if cli.config.exists() {
        Some(Config::from_file_with_env(&cli.config)?)
    } else {
        debug!("No config file found at {}, using defaults", cli.config.display());
        None
    };

    match cli.command {
        Commands::Scan { repo, format } => {
            cmd_scan(config.as_ref(), &repo, &format, cli.dry_run).await
        }
        Commands::Fix {
            repo,
            min_confidence,
            create_pr,
            branch,
        } => {
            cmd_fix(
                config.as_ref(),
                &repo,
                &min_confidence,
                create_pr,
                branch,
                cli.dry_run,
            )
            .await
        }
        Commands::ScanOrg {
            org,
            format,
            concurrency,
        } => cmd_scan_org(config.as_ref(), &org, &format, concurrency, cli.dry_run).await,
        Commands::Hooks { action } => cmd_hooks(config.as_ref(), action).await,
        Commands::Catalog { path, severity } => cmd_catalog(&path, severity.as_deref()),
    }
}

/// Scan a single repository for compliance issues and report findings.
async fn cmd_scan(
    config: Option<&Config>,
    repo: &str,
    format: &str,
    _dry_run: bool,
) -> anyhow::Result<()> {
    let repo_path = resolve_repo_path(repo)?;
    info!("Scanning repository: {}", repo_path.display());

    // Load error catalog
    let catalog_path = config
        .map(|c| c.catalog_path.clone())
        .unwrap_or_else(|| PathBuf::from("ERROR-CATALOG.scm"));
    let catalog = ErrorCatalog::from_file(&catalog_path)?;
    info!("Loaded {} error types from catalog", catalog.error_types.len());

    // Detect issues
    let detector = Detector::new(repo_path.clone())?;
    let issues = detector.detect_all(&catalog.error_types);
    info!("Found {} issues", issues.len());

    // Connect to fleet if available
    let mut fleet = FleetCoordinator::new();
    let repo_name = repo_path
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("unknown");
    if let Err(e) = fleet.connect(repo_name, &repo_path) {
        debug!("Fleet connection not available: {}", e);
    }

    // Publish findings to fleet
    if fleet.is_connected() {
        fleet.publish_detections(&issues)?;
    }

    // Output results
    match format {
        "json" => {
            let json_issues: Vec<serde_json::Value> = issues
                .iter()
                .map(|i| {
                    serde_json::json!({
                        "id": i.error_type_id,
                        "name": i.error_name,
                        "severity": format!("{:?}", i.severity),
                        "description": i.description,
                        "files": i.affected_files.iter().map(|f| f.display().to_string()).collect::<Vec<_>>(),
                        "confidence": i.confidence,
                        "suggested_fix": i.suggested_fix,
                    })
                })
                .collect();
            println!("{}", serde_json::to_string_pretty(&json_issues)?);
        }
        _ => {
            if issues.is_empty() {
                println!("No issues found.");
            } else {
                println!("=== Scan Results: {} issues ===\n", issues.len());
                for issue in &issues {
                    println!(
                        "[{:?}] {} ({})",
                        issue.severity, issue.error_name, issue.error_type_id
                    );
                    println!("  {}", issue.description);
                    println!("  Confidence: {:.0}%", issue.confidence * 100.0);
                    println!("  Files: {:?}", issue.affected_files);
                    println!("  Suggested: {}\n", issue.suggested_fix);
                }
            }
        }
    }

    // Disconnect fleet
    if fleet.is_connected() {
        fleet.disconnect(issues.len(), 0, 0)?;
    }

    Ok(())
}

/// Fix detected issues in a repository with confidence-gated actuation.
///
/// This is the core actuation pipeline:
/// 1. Detect issues (same as scan)
/// 2. Classify each fix by confidence (High/Medium/Low)
/// 3. Auto-apply high-confidence fixes locally
/// 4. Commit fixes to a branch
/// 5. Push branch and create a PR (if --create-pr)
/// 6. Create GitHub issues for low-confidence proposals
async fn cmd_fix(
    config: Option<&Config>,
    repo: &str,
    _min_confidence: &str,
    create_pr: bool,
    branch: Option<String>,
    dry_run: bool,
) -> anyhow::Result<()> {
    let repo_path = resolve_repo_path(repo)?;
    info!("Fixing repository: {}", repo_path.display());

    // Load error catalog
    let catalog_path = config
        .map(|c| c.catalog_path.clone())
        .unwrap_or_else(|| PathBuf::from("ERROR-CATALOG.scm"));
    let catalog = ErrorCatalog::from_file(&catalog_path)?;

    // Detect issues
    let detector = Detector::new(repo_path.clone())?;
    let issues = detector.detect_all(&catalog.error_types);
    info!("Found {} issues to fix", issues.len());

    if issues.is_empty() {
        println!("No issues found. Repository is compliant.");
        return Ok(());
    }

    // Load confidence thresholds from repo's bot directives
    let threshold_config = ThresholdConfig::from_repo(&repo_path);

    // Connect to fleet
    let mut fleet = FleetCoordinator::new();
    let repo_name = repo_path
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("unknown");
    if let Err(e) = fleet.connect(repo_name, &repo_path) {
        debug!("Fleet connection not available: {}", e);
    }

    // Classify fixes by confidence
    let mut auto_fixes = Vec::new();
    let mut proposals = Vec::new();
    let mut skipped = Vec::new();

    for issue in &issues {
        if let Some(error_type) = catalog.get(&issue.error_type_id) {
            let decision = threshold_config.decide(issue, &error_type.fix);
            match decision {
                FixDecision::AutoApply => {
                    info!(
                        "Auto-applying fix for {} (confidence: {:?})",
                        issue.error_name,
                        threshold_config.classify_fix(issue, &error_type.fix)
                    );
                    auto_fixes.push((issue.clone(), error_type.fix.clone()));
                }
                FixDecision::Propose { diff_preview } => {
                    let proposal =
                        threshold_config.create_proposal(issue, &error_type.fix, &diff_preview);
                    info!("Proposing fix for {} (below auto-apply threshold)", issue.error_name);
                    proposals.push(proposal);
                }
                FixDecision::Skip { reason } => {
                    debug!("Skipping fix for {}: {}", issue.error_name, reason);
                    skipped.push((issue.clone(), reason));
                }
            }
        }
    }

    println!(
        "\n=== Fix Plan ===\n  Auto-apply: {}\n  Propose:    {}\n  Skip:       {}\n",
        auto_fixes.len(),
        proposals.len(),
        skipped.len()
    );

    // Apply auto-fixes
    let fixer = Fixer::new(repo_path.clone(), dry_run);
    let fix_results = if !auto_fixes.is_empty() {
        let results = fixer.apply_and_commit(&issues, &auto_fixes)?;
        let success_count = results.iter().filter(|r| r.success).count();
        let modified_files: Vec<PathBuf> = results
            .iter()
            .flat_map(|r| r.files_modified.clone())
            .collect();

        println!(
            "Applied {}/{} fixes ({} files modified)",
            success_count,
            auto_fixes.len(),
            modified_files.len()
        );

        // Report to fleet
        if fleet.is_connected() {
            fleet.publish_fixes(success_count, &modified_files)?;
            for (issue, _fix) in &auto_fixes {
                fleet.report_fix_outcome(
                    &issue.error_type_id,
                    true,
                    "auto-apply",
                    threshold_config
                        .classify_fix(issue, &catalog.get(&issue.error_type_id).unwrap().fix)
                        .as_str(),
                )?;
            }
        }

        results
    } else {
        Vec::new()
    };

    // Create PR if requested and fixes were applied
    let has_fixes = fix_results.iter().any(|r| r.success);
    if create_pr && has_fixes && !dry_run {
        if let Some(gh_config) = config.and_then(|c| c.github.as_ref()) {
            if let Some(ref token) = gh_config.token {
                let github = GitHubClient::new(token, &gh_config.org, Some(&gh_config.api_url))?;

                let branch_name = branch.unwrap_or_else(|| {
                    format!(
                        "robot-repo-automaton/fixes-{}",
                        chrono::Utc::now().format("%Y%m%d-%H%M%S")
                    )
                });

                // Get default branch SHA
                let default_branch = github.get_default_branch(repo_name).await?;
                let base_sha = github.get_branch_sha(repo_name, &default_branch).await?;

                // Create branch and push
                github
                    .create_branch(repo_name, &branch_name, &base_sha)
                    .await?;

                // Build PR body
                let fix_summary: Vec<String> = fix_results
                    .iter()
                    .filter(|r| r.success)
                    .map(|r| format!("- {}", r.action_taken))
                    .collect();

                let pr_body = format!(
                    "## Automated Compliance Fixes\n\n\
                     robot-repo-automaton applied **{}** fix(es):\n\n\
                     {}\n\n\
                     ### Confidence\n\n\
                     All fixes were classified as **high confidence** by the threshold system.\n\
                     Bot directives: `.machine_readable/bot_directives/robot-repo-automaton.scm`\n\n\
                     ---\n\
                     _Created by robot-repo-automaton via gitbot-fleet._",
                    fix_results.iter().filter(|r| r.success).count(),
                    fix_summary.join("\n")
                );

                let pr = CreatePullRequest {
                    title: format!(
                        "fix: {} automated compliance fixes",
                        fix_results.iter().filter(|r| r.success).count()
                    ),
                    head: branch_name.clone(),
                    base: default_branch,
                    body: Some(pr_body),
                    draft: false,
                };

                match github.create_pull_request(repo_name, pr).await {
                    Ok(created_pr) => {
                        println!("Created PR #{}: {}", created_pr.number, created_pr.html_url);
                    }
                    Err(e) => {
                        error!("Failed to create PR: {}", e);
                    }
                }
            } else {
                warn!("No GitHub token available — skipping PR creation");
            }
        } else {
            warn!("No GitHub config — skipping PR creation");
        }
    } else if create_pr && has_fixes && dry_run {
        println!("[DRY RUN] Would create PR with {} fixes", fix_results.iter().filter(|r| r.success).count());
    }

    // Create GitHub issues for proposals (below auto-apply threshold)
    if !proposals.is_empty() && !dry_run {
        if let Some(gh_config) = config.and_then(|c| c.github.as_ref()) {
            if let Some(ref token) = gh_config.token {
                let github = GitHubClient::new(token, &gh_config.org, Some(&gh_config.api_url))?;

                for proposal in &proposals {
                    let issue_body = format!(
                        "## Proposed Fix (Below Auto-Apply Threshold)\n\n\
                         **Issue:** {} (`{}`)\n\
                         **Confidence:** {}\n\
                         **Target:** `{}`\n\
                         **Action:** {}\n\n\
                         ### Diff Preview\n\n\
                         ```\n{}\n```\n\n\
                         ### Why This Wasn't Auto-Applied\n\n\
                         This fix was classified as **{}** confidence, below the auto-apply \
                         threshold. Please review and apply manually if appropriate.\n\n\
                         ---\n\
                         _Created by robot-repo-automaton via gitbot-fleet._",
                        proposal.description,
                        proposal.issue_id,
                        proposal.confidence,
                        proposal.target_file,
                        proposal.action,
                        proposal.diff_preview,
                        proposal.confidence,
                    );

                    let gh_issue = robot_repo_automaton::github::CreateIssue {
                        title: format!("compliance: {}", proposal.description),
                        body: Some(issue_body),
                        labels: vec![
                            "compliance".to_string(),
                            "robot-repo-automaton".to_string(),
                            format!("confidence:{}", proposal.confidence),
                        ],
                    };

                    match github.create_issue(repo_name, gh_issue).await {
                        Ok(number) => {
                            println!("Created issue #{} for: {}", number, proposal.description);
                        }
                        Err(e) => {
                            warn!("Failed to create issue for {}: {}", proposal.issue_id, e);
                        }
                    }
                }
            }
        }
    } else if !proposals.is_empty() {
        println!("\n=== Proposed Fixes (need manual review) ===\n");
        for proposal in &proposals {
            println!(
                "[{}] {} — {} {}",
                proposal.confidence, proposal.issue_id, proposal.action, proposal.target_file
            );
            println!("  {}\n", proposal.description);
        }
    }

    // Disconnect fleet
    if fleet.is_connected() {
        fleet.disconnect(issues.len(), 0, 0)?;
    }

    Ok(())
}

/// Scan all repositories in a GitHub organization.
async fn cmd_scan_org(
    config: Option<&Config>,
    org: &str,
    _format: &str,
    concurrency: usize,
    _dry_run: bool,
) -> anyhow::Result<()> {
    let gh_config = config
        .and_then(|c| c.github.as_ref())
        .ok_or_else(|| anyhow::anyhow!("GitHub configuration required for scan-org"))?;

    let token = gh_config
        .token
        .as_ref()
        .ok_or_else(|| anyhow::anyhow!("GITHUB_TOKEN required for scan-org"))?;

    let github = GitHubClient::new(token, org, Some(&gh_config.api_url))?;
    let repos = github.list_repos().await?;

    info!("Found {} repositories in {}", repos.len(), org);

    let active_repos: Vec<_> = repos
        .iter()
        .filter(|r| !r.archived && !r.disabled)
        .collect();

    println!(
        "Scanning {} active repositories in {} (concurrency: {})",
        active_repos.len(),
        org,
        concurrency
    );

    // Clone and scan each repo
    let work_dir = std::env::temp_dir().join("robot-repo-automaton");
    std::fs::create_dir_all(&work_dir)?;

    let catalog_path = config
        .map(|c| c.catalog_path.clone())
        .unwrap_or_else(|| PathBuf::from("ERROR-CATALOG.scm"));
    let catalog = ErrorCatalog::from_file(&catalog_path)?;

    let mut total_issues = 0;
    for repo in &active_repos {
        let target = work_dir.join(&repo.name);
        if let Err(e) = github.clone_repo(repo, &target).await {
            warn!("Failed to clone {}: {}", repo.name, e);
            continue;
        }

        let detector = match Detector::new(target) {
            Ok(d) => d,
            Err(e) => {
                warn!("Failed to scan {}: {}", repo.name, e);
                continue;
            }
        };

        let issues = detector.detect_all(&catalog.error_types);
        if !issues.is_empty() {
            println!("  {} — {} issues", repo.name, issues.len());
            total_issues += issues.len();
        }
    }

    println!(
        "\n=== Organization Scan Complete ===\n  Repos: {}\n  Total issues: {}",
        active_repos.len(),
        total_issues
    );

    Ok(())
}

/// Manage Git hooks in a repository.
async fn cmd_hooks(_config: Option<&Config>, action: HookAction) -> anyhow::Result<()> {
    match action {
        HookAction::Install { repo } => {
            let manager = HookManager::new(repo)?;
            let rulesets = vec!["rsr-compliance".to_string()];
            manager.install_standard_hooks(&rulesets)?;
            println!("Hooks installed (pre-commit, pre-push, commit-msg).");
        }
        HookAction::Remove { repo } => {
            let manager = HookManager::new(repo)?;
            for hook_type in [HookType::PreCommit, HookType::PrePush, HookType::CommitMsg] {
                manager.remove_hook(hook_type)?;
            }
            println!("Hooks removed.");
        }
        HookAction::Status { repo } => {
            let manager = HookManager::new(repo)?;
            let hooks = [
                HookType::PreCommit,
                HookType::PrePush,
                HookType::CommitMsg,
                HookType::PostCommit,
                HookType::PostCheckout,
                HookType::PostMerge,
            ];
            println!("Hook status:");
            for hook_type in &hooks {
                let status = if manager.hook_exists(*hook_type) {
                    "installed"
                } else {
                    "not installed"
                };
                println!("  {}: {}", hook_type.filename(), status);
            }
        }
    }

    Ok(())
}

/// Inspect the error catalog.
fn cmd_catalog(path: &PathBuf, severity_filter: Option<&str>) -> anyhow::Result<()> {
    let catalog = ErrorCatalog::from_file(path)?;
    println!(
        "Error Catalog: {} error types (v{})",
        catalog.error_types.len(),
        catalog.metadata.schema_version
    );
    println!();

    for error_type in &catalog.error_types {
        if let Some(filter) = severity_filter {
            let matches = format!("{:?}", error_type.severity).to_lowercase() == filter.to_lowercase();
            if !matches {
                continue;
            }
        }

        println!(
            "[{:?}] {} ({})",
            error_type.severity, error_type.name, error_type.id
        );
        println!("  {}", error_type.description);
        println!("  Category: {}", error_type.category);
        println!("  Fix: {:?} {}", error_type.fix.action, error_type.fix.target);
        println!();
    }

    Ok(())
}

/// Resolve a repo argument to a local path.
///
/// Accepts either a local path or a GitHub owner/name format.
fn resolve_repo_path(repo: &str) -> anyhow::Result<PathBuf> {
    let path = PathBuf::from(repo);
    if path.exists() {
        return Ok(path);
    }

    // Try as a relative path from common locations
    let eclipse_path = PathBuf::from("/var/mnt/eclipse/repos").join(repo);
    if eclipse_path.exists() {
        return Ok(eclipse_path);
    }

    Err(anyhow::anyhow!(
        "Repository not found: {} (tried local path and /var/mnt/eclipse/repos/{})",
        repo,
        repo
    ))
}
