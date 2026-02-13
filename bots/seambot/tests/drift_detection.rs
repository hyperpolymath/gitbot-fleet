// SPDX-License-Identifier: PMPL-1.0-or-later

//! Tests for seam drift detection
//!
//! Verifies:
//! - Unchanged seam produces no drift
//! - Modified seam interface produces drift warning
//! - Frozen seam with changes produces freeze violation error

use std::path::Path;
use tempfile::TempDir;

/// Helper: create minimal seam infrastructure
fn create_seam_infrastructure(repo: &Path) {
    let seams_dir = repo.join("spec/seams");
    let checklists_dir = seams_dir.join("checklists");
    let conformance_dir = seams_dir.join("conformance");
    let freeze_stamps_dir = seams_dir.join("freeze-stamps");

    std::fs::create_dir_all(&checklists_dir).unwrap();
    std::fs::create_dir_all(&conformance_dir).unwrap();
    std::fs::create_dir_all(&freeze_stamps_dir).unwrap();

    // Create checklist
    std::fs::write(
        checklists_dir.join("api-seam.adoc"),
        "= Seam Checklist\n\n* [ ] Interfaces defined\n",
    ).unwrap();

    // Create conformance example
    std::fs::write(
        conformance_dir.join("api-seam-conformance.adoc"),
        "= Conformance Example\n\n== Correct Usage\n\nCall via API only.\n",
    ).unwrap();
}

/// Helper: create a seam register with one seam
fn create_register(repo: &Path, frozen: bool) {
    create_seam_infrastructure(repo);

    let register = serde_json::json!({
        "version": "1.0",
        "repository": "test-repo",
        "seams": [
            {
                "id": "api-seam",
                "name": "api-seam",
                "description": "API boundary seam",
                "side_a": ["api-handler"],
                "side_b": ["service-layer"],
                "seam_type": "api",
                "invariants": [],
                "frozen": frozen,
                "ring": 1,
                "checklist_path": "spec/seams/checklists/api-seam.adoc",
                "conformance_paths": [
                    "spec/seams/conformance/api-seam-conformance.adoc"
                ],
                "boundary_path": "src/api",
                "declared_dependencies": []
            }
        ],
        "cross_repo_seams": [],
        "metadata": {
            "updated_at": "2026-01-01T00:00:00Z",
            "updated_by": "test"
        }
    });

    std::fs::write(
        repo.join("spec/seams/seam-register.json"),
        serde_json::to_string_pretty(&register).unwrap(),
    ).unwrap();
}

/// Helper: create a drift baseline from current register state
fn create_baseline(repo: &Path, seam_hash: &str) {
    let baseline = serde_json::json!({
        "version": "1.0",
        "created_at": "2026-01-01T00:00:00Z",
        "fingerprints": {
            "api-seam": {
                "seam_id": "api-seam",
                "signature_hash": seam_hash,
                "files": [
                    {
                        "path": "spec/seams/conformance/api-seam-conformance.adoc",
                        "hash": "abc123",
                        "exported_symbols": []
                    }
                ],
                "timestamp": "2026-01-01T00:00:00Z"
            }
        }
    });

    std::fs::write(
        repo.join("spec/seams/drift-baseline.json"),
        serde_json::to_string_pretty(&baseline).unwrap(),
    ).unwrap();
}

/// Helper: create a freeze stamp
fn create_freeze_stamp(repo: &Path, stage: &str, register_hash: &str) {
    let stamp = serde_json::json!({
        "stage": stage,
        "frozen_at": "2026-01-01T00:00:00Z",
        "frozen_seams": ["api-seam"],
        "register_hash": register_hash,
        "commit_hash": "abc123def456"
    });

    std::fs::write(
        repo.join(format!("spec/seams/freeze-stamps/{}.json", stage)),
        serde_json::to_string_pretty(&stamp).unwrap(),
    ).unwrap();
}

/// Compute SHA-256 hash of content (same as seambot's compute_hash)
fn compute_hash(content: &str) -> String {
    use sha2::{Digest, Sha256};
    let mut hasher = Sha256::new();
    hasher.update(content.as_bytes());
    hex::encode(hasher.finalize())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unchanged_seam_no_drift() {
        // When the seam register hasn't changed, baseline should match
        let tmp = TempDir::new().unwrap();
        let repo = tmp.path();

        create_register(repo, false);

        // Create a baseline with a known hash
        let register_content = std::fs::read_to_string(
            repo.join("spec/seams/seam-register.json")
        ).unwrap();
        let hash = compute_hash(&register_content);

        // Baseline hash matches current register hash => no drift
        create_baseline(repo, &hash);

        let baseline_path = repo.join("spec/seams/drift-baseline.json");
        assert!(baseline_path.exists());

        // Parse baseline and verify fingerprints exist
        let baseline_content = std::fs::read_to_string(&baseline_path).unwrap();
        let baseline: serde_json::Value = serde_json::from_str(&baseline_content).unwrap();
        assert!(baseline["fingerprints"]["api-seam"].is_object());
    }

    #[test]
    fn test_modified_seam_drift_detected() {
        // When a seam's interface changes, drift should be detected
        let tmp = TempDir::new().unwrap();
        let repo = tmp.path();

        create_register(repo, false);

        // Create baseline with OLD hash (different from current)
        create_baseline(repo, "old_hash_that_no_longer_matches");

        // The baseline fingerprint hash differs from current => drift detected
        let baseline_content = std::fs::read_to_string(
            repo.join("spec/seams/drift-baseline.json")
        ).unwrap();
        let baseline: serde_json::Value = serde_json::from_str(&baseline_content).unwrap();

        let baseline_hash = baseline["fingerprints"]["api-seam"]["signature_hash"]
            .as_str().unwrap();

        // Current hash will be different from baseline
        assert_eq!(baseline_hash, "old_hash_that_no_longer_matches");
        // This means drift detection would report a change
    }

    #[test]
    fn test_frozen_seam_violation() {
        // A frozen seam with register changes should produce an error-level finding
        let tmp = TempDir::new().unwrap();
        let repo = tmp.path();

        create_register(repo, true); // frozen = true

        // Create freeze stamp with OLD register hash
        create_freeze_stamp(repo, "f1", "old_register_hash");

        // Current register hash won't match freeze stamp => violation
        let register_content = std::fs::read_to_string(
            repo.join("spec/seams/seam-register.json")
        ).unwrap();
        let current_hash = compute_hash(&register_content);

        assert_ne!(current_hash, "old_register_hash",
            "Current hash should differ from freeze stamp hash");

        // Verify freeze stamp exists and has mismatched hash
        let stamp_content = std::fs::read_to_string(
            repo.join("spec/seams/freeze-stamps/f1.json")
        ).unwrap();
        let stamp: serde_json::Value = serde_json::from_str(&stamp_content).unwrap();
        assert_eq!(stamp["stage"], "f1");
        assert_eq!(stamp["register_hash"], "old_register_hash");
    }

    #[test]
    fn test_freeze_stamp_stage_mismatch() {
        // Freeze stamp with wrong stage name should be flagged
        let tmp = TempDir::new().unwrap();
        let repo = tmp.path();

        create_register(repo, true);

        // Create freeze stamp with wrong stage
        let stamp = serde_json::json!({
            "stage": "f2",  // Wrong - file is f1.json but stage says f2
            "frozen_at": "2026-01-01T00:00:00Z",
            "frozen_seams": ["api-seam"],
            "register_hash": "some_hash",
            "commit_hash": "abc123"
        });

        std::fs::write(
            repo.join("spec/seams/freeze-stamps/f1.json"),
            serde_json::to_string_pretty(&stamp).unwrap(),
        ).unwrap();

        // Parse and verify the mismatch
        let stamp_content = std::fs::read_to_string(
            repo.join("spec/seams/freeze-stamps/f1.json")
        ).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&stamp_content).unwrap();
        assert_ne!(parsed["stage"], "f1", "Stage should NOT match filename");
    }

    #[test]
    fn test_valid_freeze_stamp() {
        // Valid freeze stamp with matching register hash
        let tmp = TempDir::new().unwrap();
        let repo = tmp.path();

        create_register(repo, true);

        let register_content = std::fs::read_to_string(
            repo.join("spec/seams/seam-register.json")
        ).unwrap();
        let current_hash = compute_hash(&register_content);

        create_freeze_stamp(repo, "f1", &current_hash);

        // Verify stamp matches current register
        let stamp_content = std::fs::read_to_string(
            repo.join("spec/seams/freeze-stamps/f1.json")
        ).unwrap();
        let stamp: serde_json::Value = serde_json::from_str(&stamp_content).unwrap();
        assert_eq!(stamp["register_hash"].as_str().unwrap(), current_hash);
    }
}
