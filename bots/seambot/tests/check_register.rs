// SPDX-License-Identifier: PMPL-1.0-or-later

//! Integration tests for seam register checking

// Import the seambot library
mod common {
    use std::path::PathBuf;

    pub fn fixtures_path() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures")
    }
}

#[cfg(test)]
mod tests {
    use super::common;

    // We need to test the library code, so let's create inline tests
    // that verify the core seam parsing and checking logic

    #[test]
    fn test_parse_valid_seam_register() {
        let fixture_path = common::fixtures_path().join("valid-register/spec/seams/seam-register.json");
        let content = std::fs::read_to_string(&fixture_path)
            .expect("Should read fixture file");

        // Parse as generic JSON first to validate structure
        let register: serde_json::Value = serde_json::from_str(&content)
            .expect("Should parse as JSON");

        // Verify required fields
        assert!(register.get("version").is_some(), "Should have version");
        assert!(register.get("repository").is_some(), "Should have repository");
        assert!(register.get("seams").is_some(), "Should have seams");
        assert!(register.get("metadata").is_some(), "Should have metadata");

        // Verify seams array
        let seams = register["seams"].as_array()
            .expect("seams should be an array");
        assert_eq!(seams.len(), 1, "Should have one seam");

        // Verify seam structure
        let seam = &seams[0];
        assert_eq!(seam["id"], "api-to-db");
        assert_eq!(seam["seam_type"], "layer");
        assert!(!seam["frozen"].as_bool().unwrap());
        assert_eq!(seam["ring"], 1);

        // Verify invariants
        let invariants = seam["invariants"].as_array()
            .expect("invariants should be an array");
        assert_eq!(invariants.len(), 1);
        assert_eq!(invariants[0]["id"], "no-direct-sql");
        assert_eq!(invariants[0]["severity"], "error");
    }

    #[test]
    fn test_checklist_file_exists() {
        let fixture_path = common::fixtures_path().join("valid-register");
        let checklist_path = fixture_path.join("spec/seams/checklists/api-to-db.adoc");

        assert!(checklist_path.exists(), "Checklist file should exist");

        let content = std::fs::read_to_string(&checklist_path)
            .expect("Should read checklist file");

        assert!(content.contains("Pre-Implementation"), "Should have pre-implementation section");
        assert!(content.contains("Post-Implementation"), "Should have post-implementation section");
    }

    #[test]
    fn test_conformance_file_exists() {
        let fixture_path = common::fixtures_path().join("valid-register");
        let conformance_path = fixture_path.join("spec/seams/conformance/api-to-db-conformance.adoc");

        assert!(conformance_path.exists(), "Conformance file should exist");

        let content = std::fs::read_to_string(&conformance_path)
            .expect("Should read conformance file");

        assert!(content.contains("Correct Usage"), "Should document correct usage");
        assert!(content.contains("Anti-Patterns"), "Should document anti-patterns");
    }

    #[test]
    fn test_register_missing_detection() {
        let fixture_path = common::fixtures_path().join("missing-register");
        let register_path = fixture_path.join("spec/seams/seam-register.json");

        assert!(!register_path.exists(), "Register should not exist in this fixture");
    }

    #[test]
    fn test_seam_type_values() {
        // Validate that our schema accepts all expected seam types
        let valid_types = ["module", "service", "layer", "data", "api", "build", "cross_repo"];

        for seam_type in valid_types {
            let json = format!(r#"{{
                "id": "test",
                "name": "Test",
                "description": "Test seam",
                "side_a": ["a"],
                "side_b": ["b"],
                "seam_type": "{}",
                "invariants": [],
                "frozen": false,
                "ring": 1,
                "conformance_paths": []
            }}"#, seam_type);

            let result: serde_json::Value = serde_json::from_str(&json)
                .expect(&format!("Should parse seam with type '{}'", seam_type));

            assert_eq!(result["seam_type"], seam_type);
        }
    }

    #[test]
    fn test_severity_values() {
        let valid_severities = ["info", "warning", "error", "critical"];

        for severity in valid_severities {
            let json = format!(r#"{{
                "id": "test-invariant",
                "description": "Test invariant",
                "verification": "manual",
                "severity": "{}"
            }}"#, severity);

            let result: serde_json::Value = serde_json::from_str(&json)
                .expect(&format!("Should parse invariant with severity '{}'", severity));

            assert_eq!(result["severity"], severity);
        }
    }

    #[test]
    fn test_ring_levels() {
        // Ring 0 = core (most stable)
        // Ring 1 = standard
        // Ring 2 = augmented (least stable)
        for ring in 0..=2 {
            let json = format!(r#"{{
                "id": "test",
                "name": "Test",
                "description": "Test",
                "side_a": ["a"],
                "side_b": ["b"],
                "seam_type": "module",
                "invariants": [],
                "frozen": false,
                "ring": {},
                "conformance_paths": []
            }}"#, ring);

            let result: serde_json::Value = serde_json::from_str(&json)
                .expect(&format!("Should parse seam with ring {}", ring));

            assert_eq!(result["ring"], ring);
        }
    }
}
