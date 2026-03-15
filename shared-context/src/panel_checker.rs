// SPDX-License-Identifier: PMPL-1.0-or-later
//! Panel checker — invokes the PCC binary and parses results.
//!
//! This module bridges the gitbot-fleet coordination layer with the
//! Panel Contract Compiler. Bots call `run_pcc_verify` to get structured
//! panel verification results that can be converted to fleet findings.

use crate::bot::BotId;
use crate::finding::{Finding, Severity};
use crate::panel::{PanelId, PanelManifest, PanelPhase, WiringCheck, WiringStatus};
use serde::Deserialize;
use std::path::{Path, PathBuf};
use std::process::Command;

// =============================================================================
// PCC JSON Output Types
// =============================================================================

/// PCC JSON output for a single panel (matches PCC reporter.rs output).
///
/// When PCC runs `panel verify --json`, it emits one of these per panel.
/// The structure mirrors the PCC reporter's serialization format so that
/// deserialization is zero-config.
#[derive(Debug, Clone, Deserialize)]
pub struct PccPanelResult {
    /// PascalCase panel identifier (e.g. "CloudGuard").
    pub panel_id: String,
    /// Overall status: "complete", "incomplete", "error".
    pub status: String,
    /// Lifecycle state: "releasable", "viable", "wired", "draft".
    pub state: Option<String>,
    /// Whether the panel is visible in the panel bar.
    pub visible: Option<bool>,
    /// Whether the panel meets release criteria.
    pub releasable: Option<bool>,
    /// Human-readable description of the next unmet requirement.
    pub next_requirement: Option<String>,
    /// Obligation counts summary.
    pub summary: PccSummary,
    /// The obligation ID that blocks the most downstream obligations.
    pub primary_bottleneck: Option<String>,
    /// Individual obligation check results.
    pub obligations: Vec<PccObligation>,
}

/// Summary counts from PCC verification.
#[derive(Debug, Clone, Deserialize)]
pub struct PccSummary {
    /// Total number of obligations checked.
    pub total: usize,
    /// Obligations that passed verification.
    pub satisfied: usize,
    /// Obligations that failed verification.
    pub unsatisfied: usize,
    /// Obligations blocked by upstream failures.
    pub blocked: usize,
}

/// A single obligation check result from PCC.
///
/// Each obligation represents one contract requirement for a panel
/// (e.g. "registry entry exists", "model slice wired", "test bundle present").
#[derive(Debug, Clone, Deserialize)]
pub struct PccObligation {
    /// Unique obligation identifier (e.g. "registry:CloudGuard").
    pub id: String,
    /// Obligation kind: "registry_entry", "model_slice", "msg_namespace",
    /// "view_route", "test_bundle", "panel_wired", "completion_state",
    /// "contractile_health".
    pub kind: String,
    /// Result status: "satisfied", "unsatisfied", "blocked".
    pub status: String,
    /// Failure classification: "root" (direct failure) or "derived"
    /// (blocked by upstream). None when satisfied.
    pub failure_class: Option<String>,
    /// Repair strategy: "safe" (auto-repairable), "manual", "blocked".
    pub repairability: String,
    /// Human-readable description of the check result.
    pub message: String,
    /// Source file relevant to this obligation.
    pub file: Option<String>,
    /// Expected content or pattern that was checked for.
    pub expected: Option<String>,
    /// Obligation IDs that this obligation depends on.
    pub depends_on: Vec<String>,
    /// Number of downstream obligations blocked by this one.
    pub blocked_downstream_count: usize,
}

// =============================================================================
// PCC Binary Discovery
// =============================================================================

/// Find the PCC binary, checking common locations.
///
/// Looks for the `panll` binary in both release and debug build directories
/// under `tools/pcc/target/` within the given repository root.
pub fn find_pcc_binary(repo_root: &Path) -> Option<PathBuf> {
    let candidates = [
        repo_root.join("tools/pcc/target/release/panll"),
        repo_root.join("tools/pcc/target/debug/panll"),
    ];
    candidates.into_iter().find(|p| p.exists())
}

// =============================================================================
// PCC Invocation
// =============================================================================

/// Run PCC verify against a repo and parse the JSON output.
///
/// Executes the PCC binary with `panel verify --json` and deserializes
/// the stdout into structured results. Handles both single-object and
/// array output formats from PCC.
///
/// # Arguments
///
/// * `repo_root` - Path to the repository containing `tools/pcc/`.
/// * `panel_filter` - Optional panel name to verify a single panel.
///
/// # Errors
///
/// Returns `Err` if the PCC binary is not found, fails to execute,
/// or produces unparseable output.
pub fn run_pcc_verify(
    repo_root: &Path,
    panel_filter: Option<&str>,
) -> Result<Vec<PccPanelResult>, String> {
    let pcc_binary = find_pcc_binary(repo_root).ok_or_else(|| {
        "PCC binary not found at tools/pcc/target/{release,debug}/panll".to_string()
    })?;

    let mut cmd = Command::new(&pcc_binary);
    cmd.arg("panel")
        .arg("verify")
        .arg("--json")
        .arg("--repo-root")
        .arg(repo_root);

    if let Some(panel) = panel_filter {
        cmd.arg("--panel").arg(panel);
    }

    let output = cmd
        .output()
        .map_err(|e| format!("Failed to execute PCC: {}", e))?;

    let stdout = String::from_utf8_lossy(&output.stdout);

    // PCC outputs either a single object or an array depending on
    // whether --panel was specified. Try both formats.
    if let Ok(single) = serde_json::from_str::<PccPanelResult>(&stdout) {
        Ok(vec![single])
    } else if let Ok(multi) = serde_json::from_str::<Vec<PccPanelResult>>(&stdout) {
        Ok(multi)
    } else {
        Err(format!(
            "Failed to parse PCC output: {}",
            &stdout[..stdout.len().min(200)]
        ))
    }
}

// =============================================================================
// Conversion: PCC Results → Fleet Findings
// =============================================================================

/// Convert PCC results into fleet findings for a specific bot.
///
/// This is the bridge: PCC obligations become fleet findings that
/// bots can report, aggregate, and act on through the safety triangle.
/// Only unsatisfied/blocked obligations produce findings — satisfied
/// obligations are silently accepted.
///
/// # Rule ID Mapping
///
/// PCC obligation kinds map to fleet panel rule IDs:
///
/// | PCC Kind            | Fleet Rule | Description              |
/// |---------------------|-----------|--------------------------|
/// | `registry_entry`    | PANEL-021 | Registry entry missing   |
/// | `model_slice`       | PANEL-022 | Model include missing    |
/// | `msg_namespace`     | PANEL-023 | Msg routing missing      |
/// | `view_route`        | PANEL-024 | View dispatch missing    |
/// | `test_bundle`       | PANEL-040 | Test bundle missing      |
/// | `panel_wired`       | PANEL-020 | Panel wiring incomplete  |
/// | `completion_state`  | PANEL-042 | Clade unregistered       |
/// | `contractile_health`| PANEL-050 | Contractile health fail  |
pub fn pcc_results_to_findings(results: &[PccPanelResult], bot: BotId) -> Vec<Finding> {
    let mut findings = Vec::new();

    for result in results {
        for obligation in &result.obligations {
            if obligation.status == "satisfied" {
                continue; // Only report failures
            }

            let severity = match obligation.failure_class.as_deref() {
                Some("root") => Severity::Error,
                Some("derived") => Severity::Warning,
                _ => Severity::Info,
            };

            let rule_id = match obligation.kind.as_str() {
                "registry_entry" => "PANEL-021",
                "model_slice" => "PANEL-022",
                "msg_namespace" => "PANEL-023",
                "view_route" => "PANEL-024",
                "test_bundle" => "PANEL-040",
                "panel_wired" => "PANEL-020",
                "completion_state" => "PANEL-042",
                "contractile_health" => "PANEL-050",
                _ => "PANEL-000",
            };

            let mut finding = Finding::new(
                bot,
                rule_id,
                severity,
                &format!("[{}] {}", result.panel_id, obligation.message),
            )
            .with_category(&format!("panel/{}", obligation.kind));

            if let Some(ref file) = obligation.file {
                finding = finding.with_file(PathBuf::from(file));
            }

            if obligation.repairability == "safe" {
                finding = finding
                    .fixable()
                    .with_suggestion(&format!(
                        "Run: panll panel repair --panel {} --apply",
                        result.panel_id
                    ))
                    .with_confidence(0.95);
            }

            if obligation.blocked_downstream_count > 0 {
                finding = finding.with_metadata(serde_json::json!({
                    "blocked_downstream": obligation.blocked_downstream_count,
                    "is_bottleneck": true,
                    "panel_state": result.state,
                }));
            }

            findings.push(finding);
        }
    }

    findings
}

// =============================================================================
// Conversion: PCC Results → Panel Manifests
// =============================================================================

/// Convert a PCC result into a PanelManifest for the shared-context panel system.
///
/// Maps PCC lifecycle states to fleet panel phases and populates the
/// wiring status from individual obligation results. This allows bots
/// that consume PanelManifest (e.g. finishbot checking `can_advance()`)
/// to work with live PCC data.
pub fn pcc_result_to_manifest(result: &PccPanelResult) -> PanelManifest {
    let id = PanelId::new(&result.panel_id)
        .unwrap_or_else(|_| PanelId(result.panel_id.clone()));

    let phase = match result.state.as_deref() {
        Some("releasable") => PanelPhase::Active,
        Some("viable") => PanelPhase::Provisioned,
        Some("wired") => PanelPhase::Wired,
        Some("draft") => PanelPhase::Minted,
        _ => PanelPhase::Minted,
    };

    let mut wiring = WiringStatus::default();
    for obligation in &result.obligations {
        let check = if obligation.status == "satisfied" {
            WiringCheck::connected_with_note("PCC verified")
        } else {
            WiringCheck::disconnected()
        };

        match obligation.kind.as_str() {
            "registry_entry" => wiring.panel_registry = check,
            "model_slice" => wiring.model_include = check,
            "msg_namespace" => wiring.msg_routing = check,
            "view_route" => wiring.view_dispatch = check,
            _ => {}
        }
    }

    // Set switcher, update, and rust registration from the aggregate
    // "panel_wired" obligation when present.
    if result
        .obligations
        .iter()
        .any(|o| o.kind == "panel_wired" && o.status == "satisfied")
    {
        wiring.panel_switcher_model = WiringCheck::connected_with_note("PCC verified");
        wiring.update_handler = WiringCheck::connected_with_note("PCC verified");
        wiring.rust_registration = WiringCheck::connected_with_note("PCC verified");
    }

    PanelManifest {
        id,
        short_name: result.panel_id.clone(),
        description: String::new(),
        icon: String::new(),
        has_backend: false,
        phase,
        clade_id: None,
        isolation: None,
        wiring,
        validations: Vec::new(),
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_pcc_result() {
        let json = r#"{
            "panel_id": "CloudGuard",
            "status": "complete",
            "state": "releasable",
            "visible": true,
            "releasable": true,
            "next_requirement": null,
            "summary": {"total": 8, "satisfied": 8, "unsatisfied": 0, "blocked": 0},
            "primary_bottleneck": null,
            "obligations": []
        }"#;
        let result: PccPanelResult = serde_json::from_str(json).unwrap();
        assert_eq!(result.panel_id, "CloudGuard");
        assert_eq!(result.state.as_deref(), Some("releasable"));
        assert_eq!(result.summary.total, 8);
    }

    #[test]
    fn test_parse_pcc_obligation() {
        let json = r#"{
            "id": "registry:CloudGuard",
            "kind": "registry_entry",
            "status": "unsatisfied",
            "failure_class": "root",
            "repairability": "safe",
            "message": "Not found in PanelRegistry.res",
            "file": "src/modules/PanelRegistry.res",
            "expected": "id: PanelCloudGuard",
            "depends_on": ["contract:CloudGuard"],
            "blocked_downstream_count": 3
        }"#;
        let obligation: PccObligation = serde_json::from_str(json).unwrap();
        assert_eq!(obligation.kind, "registry_entry");
        assert_eq!(obligation.status, "unsatisfied");
        assert_eq!(obligation.blocked_downstream_count, 3);
    }

    #[test]
    fn test_pcc_result_to_manifest_releasable() {
        let result = PccPanelResult {
            panel_id: "TestPanel".to_string(),
            status: "complete".to_string(),
            state: Some("releasable".to_string()),
            visible: Some(true),
            releasable: Some(true),
            next_requirement: None,
            summary: PccSummary {
                total: 8,
                satisfied: 8,
                unsatisfied: 0,
                blocked: 0,
            },
            primary_bottleneck: None,
            obligations: vec![],
        };
        let manifest = pcc_result_to_manifest(&result);
        assert_eq!(manifest.phase, PanelPhase::Active);
    }

    #[test]
    fn test_pcc_result_to_manifest_draft() {
        let result = PccPanelResult {
            panel_id: "DraftPanel".to_string(),
            status: "incomplete".to_string(),
            state: Some("draft".to_string()),
            visible: Some(false),
            releasable: Some(false),
            next_requirement: Some("Wire panel".to_string()),
            summary: PccSummary {
                total: 8,
                satisfied: 1,
                unsatisfied: 4,
                blocked: 3,
            },
            primary_bottleneck: Some("registry:DraftPanel".to_string()),
            obligations: vec![],
        };
        let manifest = pcc_result_to_manifest(&result);
        assert_eq!(manifest.phase, PanelPhase::Minted);
    }

    #[test]
    fn test_pcc_result_to_manifest_wiring_from_obligations() {
        let result = PccPanelResult {
            panel_id: "WiredPanel".to_string(),
            status: "complete".to_string(),
            state: Some("wired".to_string()),
            visible: Some(true),
            releasable: Some(false),
            next_requirement: None,
            summary: PccSummary {
                total: 4,
                satisfied: 4,
                unsatisfied: 0,
                blocked: 0,
            },
            primary_bottleneck: None,
            obligations: vec![
                PccObligation {
                    id: "registry:WiredPanel".to_string(),
                    kind: "registry_entry".to_string(),
                    status: "satisfied".to_string(),
                    failure_class: None,
                    repairability: "safe".to_string(),
                    message: "Found in PanelRegistry.res".to_string(),
                    file: Some("src/modules/PanelRegistry.res".to_string()),
                    expected: None,
                    depends_on: vec![],
                    blocked_downstream_count: 0,
                },
                PccObligation {
                    id: "model:WiredPanel".to_string(),
                    kind: "model_slice".to_string(),
                    status: "satisfied".to_string(),
                    failure_class: None,
                    repairability: "safe".to_string(),
                    message: "Model include found".to_string(),
                    file: Some("src/Model.res".to_string()),
                    expected: None,
                    depends_on: vec![],
                    blocked_downstream_count: 0,
                },
            ],
        };
        let manifest = pcc_result_to_manifest(&result);
        assert!(manifest.wiring.panel_registry.is_connected());
        assert!(manifest.wiring.model_include.is_connected());
    }

    #[test]
    fn test_pcc_results_to_findings_empty_on_all_satisfied() {
        let result = PccPanelResult {
            panel_id: "TestPanel".to_string(),
            status: "complete".to_string(),
            state: Some("releasable".to_string()),
            visible: Some(true),
            releasable: Some(true),
            next_requirement: None,
            summary: PccSummary {
                total: 8,
                satisfied: 8,
                unsatisfied: 0,
                blocked: 0,
            },
            primary_bottleneck: None,
            obligations: vec![PccObligation {
                id: "contract:TestPanel".to_string(),
                kind: "contract_exists".to_string(),
                status: "satisfied".to_string(),
                failure_class: None,
                repairability: "safe".to_string(),
                message: "OK".to_string(),
                file: None,
                expected: None,
                depends_on: vec![],
                blocked_downstream_count: 0,
            }],
        };
        let findings = pcc_results_to_findings(&[result], BotId::Seambot);
        assert!(findings.is_empty()); // All satisfied = no findings
    }

    #[test]
    fn test_pcc_results_to_findings_reports_failures() {
        let result = PccPanelResult {
            panel_id: "BrokenPanel".to_string(),
            status: "incomplete".to_string(),
            state: Some("draft".to_string()),
            visible: Some(false),
            releasable: Some(false),
            next_requirement: Some("Wire panel".to_string()),
            summary: PccSummary {
                total: 8,
                satisfied: 1,
                unsatisfied: 4,
                blocked: 3,
            },
            primary_bottleneck: Some("registry:BrokenPanel".to_string()),
            obligations: vec![PccObligation {
                id: "registry:BrokenPanel".to_string(),
                kind: "registry_entry".to_string(),
                status: "unsatisfied".to_string(),
                failure_class: Some("root".to_string()),
                repairability: "safe".to_string(),
                message: "Not found in PanelRegistry.res".to_string(),
                file: Some("src/modules/PanelRegistry.res".to_string()),
                expected: Some("id: PanelBrokenPanel".to_string()),
                depends_on: vec!["contract:BrokenPanel".to_string()],
                blocked_downstream_count: 3,
            }],
        };
        let findings = pcc_results_to_findings(&[result], BotId::Seambot);
        assert_eq!(findings.len(), 1);
        assert_eq!(findings[0].severity, Severity::Error);
        assert!(findings[0].fixable);
        assert!(findings[0].confidence.unwrap() >= 0.95);
    }

    #[test]
    fn test_pcc_results_to_findings_derived_is_warning() {
        let result = PccPanelResult {
            panel_id: "BlockedPanel".to_string(),
            status: "incomplete".to_string(),
            state: Some("draft".to_string()),
            visible: Some(false),
            releasable: Some(false),
            next_requirement: None,
            summary: PccSummary {
                total: 2,
                satisfied: 0,
                unsatisfied: 1,
                blocked: 1,
            },
            primary_bottleneck: None,
            obligations: vec![PccObligation {
                id: "model:BlockedPanel".to_string(),
                kind: "model_slice".to_string(),
                status: "blocked".to_string(),
                failure_class: Some("derived".to_string()),
                repairability: "blocked".to_string(),
                message: "Blocked by upstream failure".to_string(),
                file: None,
                expected: None,
                depends_on: vec!["registry:BlockedPanel".to_string()],
                blocked_downstream_count: 0,
            }],
        };
        let findings = pcc_results_to_findings(&[result], BotId::Seambot);
        assert_eq!(findings.len(), 1);
        assert_eq!(findings[0].severity, Severity::Warning);
        assert_eq!(findings[0].rule_id, "PANEL-022");
    }

    #[test]
    fn test_pcc_results_to_findings_rule_id_mapping() {
        // Verify that each PCC obligation kind maps to the correct fleet rule ID
        let kinds_and_rules = [
            ("registry_entry", "PANEL-021"),
            ("model_slice", "PANEL-022"),
            ("msg_namespace", "PANEL-023"),
            ("view_route", "PANEL-024"),
            ("test_bundle", "PANEL-040"),
            ("panel_wired", "PANEL-020"),
            ("completion_state", "PANEL-042"),
            ("contractile_health", "PANEL-050"),
            ("unknown_kind", "PANEL-000"),
        ];

        for (kind, expected_rule) in kinds_and_rules {
            let result = PccPanelResult {
                panel_id: "RuleTest".to_string(),
                status: "incomplete".to_string(),
                state: None,
                visible: None,
                releasable: None,
                next_requirement: None,
                summary: PccSummary {
                    total: 1,
                    satisfied: 0,
                    unsatisfied: 1,
                    blocked: 0,
                },
                primary_bottleneck: None,
                obligations: vec![PccObligation {
                    id: format!("test:{}", kind),
                    kind: kind.to_string(),
                    status: "unsatisfied".to_string(),
                    failure_class: Some("root".to_string()),
                    repairability: "manual".to_string(),
                    message: format!("Testing {} mapping", kind),
                    file: None,
                    expected: None,
                    depends_on: vec![],
                    blocked_downstream_count: 0,
                }],
            };
            let findings = pcc_results_to_findings(&[result], BotId::Seambot);
            assert_eq!(
                findings[0].rule_id, expected_rule,
                "Kind '{}' should map to '{}'",
                kind, expected_rule
            );
        }
    }
}
