// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! finishing-bot: Release Readiness Validator
//!
//! This crate provides comprehensive release readiness validation for repositories,
//! ensuring they are production-ready by:
//!
//! - Validating licenses and SPDX headers
//! - Detecting and flagging/removing placeholders (TODO, FIXME, etc.)
//! - Verifying documentation claims and completeness
//! - Checking release artifacts and security configurations
//!
//! Part of the Hyperpolymath Gitbot Fleet.

#![forbid(unsafe_code)]
pub mod analyzers;
pub mod config;
pub mod error;

pub use analyzers::{
    claims::ClaimsAnalyzer, license::LicenseAnalyzer, placeholder::PlaceholderAnalyzer,
    release::ReleaseAnalyzer, scm_files::ScmFilesAnalyzer, testing::TestingAnalyzer,
    tooling::ToolingAnalyzer, v1_readiness::V1ReadinessAnalyzer, AnalysisResult, Analyzer,
    AuditResult, Finding, Severity,
};
pub use config::Config;
pub use error::{FinishingError, Result};

use std::path::Path;
use tracing::{debug, error, info, warn};

/// Run a full audit on a repository
pub fn audit(path: &Path, config: &Config) -> Result<AuditResult> {
    info!(path = %path.display(), "Starting release readiness audit");

    let mut result = AuditResult::default();

    // Run license analysis
    info!("Running license validation...");
    let license_analyzer = LicenseAnalyzer::default();
    result.license = license_analyzer.analyze(path, config)?;
    info!(
        findings = result.license.findings.len(),
        files = result.license.files_checked,
        "License analysis complete"
    );

    // Run placeholder analysis
    info!("Running placeholder detection...");
    let placeholder_analyzer = PlaceholderAnalyzer::default();
    result.placeholder = placeholder_analyzer.analyze(path, config)?;
    info!(
        findings = result.placeholder.findings.len(),
        files = result.placeholder.files_checked,
        "Placeholder analysis complete"
    );

    // Run claims verification
    info!("Running claim verification...");
    let claims_analyzer = ClaimsAnalyzer::default();
    result.claims = claims_analyzer.analyze(path, config)?;
    info!(
        findings = result.claims.findings.len(),
        files = result.claims.files_checked,
        "Claim verification complete"
    );

    // Run release readiness checks
    info!("Running release readiness checks...");
    let release_analyzer = ReleaseAnalyzer::default();
    result.release = release_analyzer.analyze(path, config)?;
    info!(
        findings = result.release.findings.len(),
        files = result.release.files_checked,
        "Release readiness checks complete"
    );

    // Log summary
    if result.should_block_release() {
        error!(
            errors = result.total_errors(),
            warnings = result.total_warnings(),
            "Audit completed with blocking errors"
        );
    } else if result.total_warnings() > 0 {
        warn!(
            warnings = result.total_warnings(),
            "Audit completed with warnings"
        );
    } else {
        info!("Audit completed successfully - repository is release-ready");
    }

    Ok(result)
}

/// Apply automatic fixes for fixable issues
pub fn fix(path: &Path, config: &Config, audit_result: &AuditResult) -> Result<Vec<String>> {
    let mut all_fixed = Vec::new();

    if config.dry_run {
        info!("Dry run mode - no changes will be made");
    }

    // Fix license issues
    let license_analyzer = LicenseAnalyzer::default();
    let fixed = license_analyzer.fix(path, config, &audit_result.license.findings)?;
    all_fixed.extend(fixed);

    // Fix placeholder issues
    let placeholder_analyzer = PlaceholderAnalyzer::default();
    let fixed = placeholder_analyzer.fix(path, config, &audit_result.placeholder.findings)?;
    all_fixed.extend(fixed);

    // Claims and release issues generally require manual intervention
    debug!("Claims and release issues require manual review");

    if all_fixed.is_empty() {
        info!("No automatic fixes applied");
    } else {
        info!(count = all_fixed.len(), "Applied automatic fixes");
    }

    Ok(all_fixed)
}

/// Print a summary of audit results
pub fn print_summary(result: &AuditResult) {
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

    // Print detailed findings by category
    if !result.license.findings.is_empty() {
        print_findings_section("LICENSE", &result.license.findings);
    }
    if !result.placeholder.findings.is_empty() {
        print_findings_section("PLACEHOLDERS", &result.placeholder.findings);
    }
    if !result.claims.findings.is_empty() {
        print_findings_section("CLAIMS", &result.claims.findings);
    }
    if !result.release.findings.is_empty() {
        print_findings_section("RELEASE", &result.release.findings);
    }
}

fn print_findings_section(category: &str, findings: &[Finding]) {
    println!("┌─ {} ─────────────────────────────────────────────────────", category);

    for finding in findings {
        let severity_icon = match finding.severity {
            Severity::Error => "❌",
            Severity::Warning => "⚠️ ",
            Severity::Info => "ℹ️ ",
            Severity::Suggestion => "💡",
        };

        println!("│");
        println!(
            "│ {} [{}] {}",
            severity_icon, finding.id, finding.name
        );
        println!("│    {}", finding.message);

        if let Some(ref file) = finding.file {
            let location = match (finding.line, finding.column) {
                (Some(l), Some(c)) => format!("{}:{}:{}", file.display(), l, c),
                (Some(l), None) => format!("{}:{}", file.display(), l),
                _ => file.display().to_string(),
            };
            println!("│    📁 {}", location);
        }

        if let Some(ref suggestion) = finding.suggestion {
            println!("│    💡 {}", suggestion);
        }

        if finding.fixable {
            println!("│    🔧 Auto-fixable");
        }
    }

    println!("└─────────────────────────────────────────────────────────────────");
    println!();
}
