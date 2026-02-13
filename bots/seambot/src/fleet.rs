// SPDX-License-Identifier: PMPL-1.0-or-later

//! Gitbot fleet integration for seambot
//!
//! Publishes seam hygiene findings to the shared context layer for consumption
//! by other bots in the fleet (glambot, finishbot, etc.).

use anyhow::Result;
use gitbot_shared_context::{BotId, Context, Finding, Severity};
use std::path::Path;

use crate::hidden_channels::{ChannelType, HiddenChannel};
use crate::seam::SeamRegister;

/// Publish seam analysis findings to the fleet shared context
pub fn publish_findings(
    ctx: &mut Context,
    register: &SeamRegister,
    hidden_channels: &[HiddenChannel],
    drift_count: usize,
    conformance_failures: usize,
) -> Result<()> {
    // Publish hidden channel findings
    for channel in hidden_channels {
        let finding_id = format!(
            "SEAM-HIDDEN-{:?}-{}-{}",
            channel.channel_type, channel.source_seam, channel.target_seam
        );

        let description = format!(
            "Hidden {} channel from '{}' to '{}': {}",
            channel_type_name(channel.channel_type),
            channel.source_seam,
            channel.target_seam,
            channel.evidence
        );

        ctx.add_finding(Finding::new(
            BotId::Seambot,
            &finding_id,
            map_severity(channel.severity),
            &description,
        ));
    }

    // Publish seam drift findings
    if drift_count > 0 {
        ctx.add_finding(Finding::new(
            BotId::Seambot,
            "SEAM-DRIFT",
            Severity::Warning,
            &format!("{} seam interface(s) have drifted from baseline", drift_count),
        ));
    }

    // Publish conformance failures
    if conformance_failures > 0 {
        ctx.add_finding(Finding::new(
            BotId::Seambot,
            "SEAM-CONFORMANCE",
            Severity::Error,
            &format!("{} seam(s) lack valid conformance examples", conformance_failures),
        ));
    }

    // Publish register completeness status
    let completeness = calculate_register_completeness(register);
    if completeness < 100.0 {
        ctx.add_finding(Finding::new(
            BotId::Seambot,
            "SEAM-INCOMPLETE",
            Severity::Warning,
            &format!("Seam register is {:.1}% complete", completeness),
        ));
    }

    Ok(())
}

/// Calculate seam register completeness percentage
fn calculate_register_completeness(register: &SeamRegister) -> f64 {
    if register.seams.is_empty() {
        return 0.0;
    }

    let mut complete_fields = 0;
    let total_fields = register.seams.len() * 5; // name, boundary_path, interface, examples, dependencies

    for seam in &register.seams {
        if !seam.name.is_empty() {
            complete_fields += 1;
        }
        if !seam.boundary_path.is_empty() {
            complete_fields += 1;
        }
        if !seam.description.is_empty() {
            complete_fields += 1;
        }
        if !seam.conformance_paths.is_empty() {
            complete_fields += 1;
        }
        if !seam.side_a.is_empty() || !seam.side_b.is_empty() {
            complete_fields += 1;
        }
    }

    (complete_fields as f64 / total_fields as f64) * 100.0
}

/// Map seambot ChannelType to fleet-compatible string
fn channel_type_name(ct: ChannelType) -> &'static str {
    match ct {
        ChannelType::UndeclaredImport => "undeclared-import",
        ChannelType::GlobalState => "global-state",
        ChannelType::FilesystemCoupling => "filesystem",
        ChannelType::DatabaseCoupling => "database",
        ChannelType::NetworkCoupling => "network",
    }
}

/// Map seambot Severity to fleet Severity
fn map_severity(s: crate::hidden_channels::Severity) -> Severity {
    use crate::hidden_channels::Severity as SeamSeverity;

    match s {
        SeamSeverity::Critical => Severity::Error,
        SeamSeverity::High => Severity::Error,
        SeamSeverity::Medium => Severity::Warning,
        SeamSeverity::Low => Severity::Info,
    }
}

/// Load existing fleet context for a repository or create new one
pub fn load_or_create_context(repo_path: &Path) -> Result<Context> {
    let repo_name = repo_path
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("unknown");

    Ok(Context::new(
        repo_name,
        repo_path.to_str().unwrap_or(""),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hidden_channels::ChannelType;
    use crate::seam::{RegisterMetadata, SeamType};

    /// Helper to create an empty SeamRegister for testing
    fn empty_register() -> SeamRegister {
        SeamRegister {
            version: "1.0".to_string(),
            repository: "test".to_string(),
            seams: vec![],
            cross_repo_seams: vec![],
            metadata: RegisterMetadata {
                updated_at: String::new(),
                updated_by: String::new(),
                commit_hash: None,
            },
        }
    }

    #[test]
    fn test_publish_hidden_channel_findings() {
        let mut ctx = Context::new("test-repo", "/tmp/test");
        let register = empty_register();

        let channels = vec![
            HiddenChannel::new(
                ChannelType::DatabaseCoupling,
                "auth-service",
                "user-service",
                "Shared users table",
                crate::hidden_channels::Severity::High,
            ),
        ];

        publish_findings(&mut ctx, &register, &channels, 0, 0).unwrap();

        let findings = ctx.findings_from(BotId::Seambot);
        // 1 hidden channel finding + 1 incomplete register finding (0% complete)
        assert!(findings.len() >= 1);
        assert!(findings.iter().any(|f| f.rule_id.contains("SEAM-HIDDEN")));
    }

    #[test]
    fn test_publish_drift_finding() {
        let mut ctx = Context::new("test-repo", "/tmp/test");
        let register = empty_register();

        publish_findings(&mut ctx, &register, &[], 3, 0).unwrap();

        let findings = ctx.findings_from(BotId::Seambot);
        assert!(findings.len() >= 1);
        assert!(findings.iter().any(|f| f.rule_id.contains("SEAM-DRIFT")));
    }

    #[test]
    fn test_publish_conformance_finding() {
        let mut ctx = Context::new("test-repo", "/tmp/test");
        let register = empty_register();

        publish_findings(&mut ctx, &register, &[], 0, 2).unwrap();

        let findings = ctx.findings_from(BotId::Seambot);
        assert!(findings.len() >= 1);
        assert!(findings.iter().any(|f| f.rule_id.contains("SEAM-CONFORMANCE")));
    }

    #[test]
    fn test_register_completeness() {
        let mut register = empty_register();

        // Empty register
        assert_eq!(calculate_register_completeness(&register), 0.0);

        // Add incomplete seam
        register.seams.push(crate::seam::Seam {
            id: "test".to_string(),
            name: "test-seam".to_string(),
            description: String::new(),
            side_a: vec![],
            side_b: vec![],
            seam_type: SeamType::Module,
            invariants: vec![],
            introduced_at: None,
            frozen: false,
            ring: 0,
            checklist_path: None,
            conformance_paths: vec![],
            boundary_path: "src/test".to_string(),
            declared_dependencies: vec![],
        });

        // Should be 40% complete (2 of 5 fields)
        assert!((calculate_register_completeness(&register) - 40.0).abs() < 0.1);
    }

    #[test]
    fn test_channel_type_name() {
        assert_eq!(channel_type_name(ChannelType::UndeclaredImport), "undeclared-import");
        assert_eq!(channel_type_name(ChannelType::GlobalState), "global-state");
        assert_eq!(channel_type_name(ChannelType::FilesystemCoupling), "filesystem");
        assert_eq!(channel_type_name(ChannelType::DatabaseCoupling), "database");
        assert_eq!(channel_type_name(ChannelType::NetworkCoupling), "network");
    }

    #[test]
    fn test_severity_mapping() {
        use crate::hidden_channels::Severity as SeamSeverity;

        assert!(matches!(map_severity(SeamSeverity::Critical), Severity::Error));
        assert!(matches!(map_severity(SeamSeverity::High), Severity::Error));
        assert!(matches!(map_severity(SeamSeverity::Medium), Severity::Warning));
        assert!(matches!(map_severity(SeamSeverity::Low), Severity::Info));
    }
}
