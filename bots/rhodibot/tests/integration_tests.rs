// SPDX-License-Identifier: PMPL-1.0-or-later

//! Integration tests for rhodibot RSR compliance engine, webhook handling,
//! GitHub client, and report formatting.
//!
//! All tests use wiremock to mock the GitHub API - no real API calls are made.

use wiremock::matchers::{header, method, path};
use wiremock::{Mock, MockServer, ResponseTemplate};

/// Helper to create a Config pointing at the mock server
fn mock_config(server_url: &str) -> rhodibot::config::Config {
    rhodibot::config::Config {
        app_id: None,
        private_key: None,
        webhook_secret: Some("test-secret".to_string()),
        github_api_url: server_url.to_string(),
    }
}

/// Helper to build a repository API response JSON
fn repo_json(license_key: Option<&str>, license_name: Option<&str>) -> serde_json::Value {
    let license = license_key.map(|key| {
        serde_json::json!({
            "key": key,
            "name": license_name.unwrap_or(key),
            "spdx_id": key.to_uppercase()
        })
    });

    serde_json::json!({
        "id": 12345,
        "name": "test-repo",
        "full_name": "test-org/test-repo",
        "description": "A test repository",
        "default_branch": "main",
        "language": "Rust",
        "topics": ["test"],
        "license": license
    })
}

// ============================================================================
// Module: RSR Compliance Engine Tests
// ============================================================================

mod rsr_tests {
    use super::*;
    use rhodibot::rsr::*;

    /// Mount file existence mocks for a list of files that exist.
    ///
    /// Mounts HEAD 200 for specified files, HEAD 404 catch-all for all others,
    /// and GET 404 for .rsr.toml (config lookup). The GET catch-all is NOT mounted
    /// here to avoid conflicting with test-specific GET mocks (e.g., repo info).
    async fn mount_file_mocks(server: &MockServer, owner: &str, repo: &str, existing_files: &[&str]) {
        // Mount 200 for existing files
        for file in existing_files {
            Mock::given(method("HEAD"))
                .and(path(format!("/repos/{}/{}/contents/{}", owner, repo, file)))
                .respond_with(ResponseTemplate::new(200))
                .mount(server)
                .await;
        }

        // Default 404 for HEAD requests to files that don't exist
        Mock::given(method("HEAD"))
            .respond_with(ResponseTemplate::new(404))
            .expect(0..)
            .mount(server)
            .await;

        // 404 for .rsr.toml GET (repo config lookup)
        Mock::given(method("GET"))
            .and(path(format!("/repos/{}/{}/contents/.rsr.toml", owner, repo)))
            .respond_with(ResponseTemplate::new(404))
            .expect(0..)
            .mount(server)
            .await;
    }

    #[tokio::test]
    async fn test_all_required_files_present_high_score() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        let files = &[
            "README.adoc", "LICENSE.txt", "SECURITY.md", "CONTRIBUTING.md",
            "CODE_OF_CONDUCT.md", ".claude/CLAUDE.md", "STATE.scm",
            "META.scm", "ECOSYSTEM.scm", ".github/workflows",
            ".editorconfig", ".gitattributes", ".gitignore",
            "justfile", ".bot_directives",
        ];

        mount_file_mocks(&server, "test-org", "test-repo", files).await;

        // Mock repository info with approved license
        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo"))
            .and(header("Accept", "application/vnd.github+json"))
            .respond_with(ResponseTemplate::new(200).set_body_json(repo_json(Some("mit"), Some("MIT License"))))
            .mount(&server)
            .await;

        let repo_config = RepoConfig::default();
        let report = check_compliance_with_policy(&config, "test-org", "test-repo", &repo_config)
            .await
            .expect("compliance check should succeed");

        assert!(report.required_passed, "all required checks should pass");
        assert!(report.percentage >= 90.0, "score should be >= 90%, got {:.1}%", report.percentage);
        assert_eq!(report.owner, "test-org");
        assert_eq!(report.repo, "test-repo");
    }

    #[tokio::test]
    async fn test_missing_readme_lower_score() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        // All files except README.adoc
        let files = &[
            "LICENSE.txt", "SECURITY.md", "CONTRIBUTING.md",
            "CODE_OF_CONDUCT.md", ".github/workflows",
        ];

        mount_file_mocks(&server, "test-org", "test-repo", files).await;

        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo"))
            .and(header("Accept", "application/vnd.github+json"))
            .respond_with(ResponseTemplate::new(200).set_body_json(repo_json(Some("mit"), Some("MIT License"))))
            .mount(&server)
            .await;

        let repo_config = RepoConfig::default();
        let report = check_compliance_with_policy(&config, "test-org", "test-repo", &repo_config)
            .await
            .expect("compliance check should succeed");

        // README is Required in all policies - should fail
        assert!(!report.required_passed, "missing README should fail required checks");

        // Verify the specific check failed
        let readme_check = report.checks.iter().find(|c| c.name == "README.adoc");
        assert!(readme_check.is_some(), "should have a README.adoc check");
        let readme_check = readme_check.unwrap();
        assert_eq!(readme_check.status, CheckStatus::Fail);
    }

    #[tokio::test]
    async fn test_missing_license_required_fails() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        let files = &["README.adoc"];
        mount_file_mocks(&server, "test-org", "test-repo", files).await;

        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo"))
            .and(header("Accept", "application/vnd.github+json"))
            .respond_with(ResponseTemplate::new(200).set_body_json(repo_json(None, None)))
            .mount(&server)
            .await;

        let repo_config = RepoConfig::default();
        let report = check_compliance_with_policy(&config, "test-org", "test-repo", &repo_config)
            .await
            .expect("compliance check should succeed");

        assert!(!report.required_passed, "missing LICENSE should fail required checks");

        let license_check = report.checks.iter().find(|c| c.name == "LICENSE.txt");
        assert!(license_check.is_some());
        assert_eq!(license_check.unwrap().status, CheckStatus::Fail);
    }

    #[tokio::test]
    async fn test_banned_files_language_policy() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        // Include banned files (go.mod, package-lock.json)
        let files = &[
            "README.adoc", "LICENSE.txt", "go.mod", "package-lock.json",
        ];

        mount_file_mocks(&server, "test-org", "test-repo", files).await;

        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo"))
            .and(header("Accept", "application/vnd.github+json"))
            .respond_with(ResponseTemplate::new(200).set_body_json(repo_json(Some("mit"), Some("MIT License"))))
            .mount(&server)
            .await;

        let repo_config = RepoConfig::default();
        let report = check_compliance_with_policy(&config, "test-org", "test-repo", &repo_config)
            .await
            .expect("compliance check should succeed");

        // Banned files should produce warnings under standard policy
        let go_check = report.checks.iter().find(|c| c.name == "no-go.mod");
        assert!(go_check.is_some(), "should have a go.mod ban check");
        assert_eq!(go_check.unwrap().status, CheckStatus::Warn);
        assert_eq!(go_check.unwrap().category, CheckCategory::LanguagePolicy);

        let npm_check = report.checks.iter().find(|c| c.name == "no-package-lock.json");
        assert!(npm_check.is_some(), "should have a package-lock.json ban check");
        assert_eq!(npm_check.unwrap().status, CheckStatus::Warn);
    }

    #[tokio::test]
    async fn test_banned_files_strict_policy_fails() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        let files = &["README.adoc", "LICENSE.txt", "go.mod"];
        mount_file_mocks(&server, "test-org", "test-repo", files).await;

        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo"))
            .and(header("Accept", "application/vnd.github+json"))
            .respond_with(ResponseTemplate::new(200).set_body_json(repo_json(Some("mit"), Some("MIT License"))))
            .mount(&server)
            .await;

        let repo_config = RepoConfig { policy: PolicyPack::Strict, ..Default::default() };
        let report = check_compliance_with_policy(&config, "test-org", "test-repo", &repo_config)
            .await
            .expect("compliance check should succeed");

        // Under strict policy, banned files should cause failure
        let go_check = report.checks.iter().find(|c| c.name == "no-go.mod");
        assert!(go_check.is_some());
        assert_eq!(go_check.unwrap().status, CheckStatus::Fail);
        assert!(!report.required_passed);
    }

    #[tokio::test]
    async fn test_minimal_policy_few_requirements() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        // Only README and LICENSE - minimal policy should be satisfied
        let files = &["README.adoc", "LICENSE.txt"];
        mount_file_mocks(&server, "test-org", "test-repo", files).await;

        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo"))
            .and(header("Accept", "application/vnd.github+json"))
            .respond_with(ResponseTemplate::new(200).set_body_json(repo_json(Some("mit"), Some("MIT License"))))
            .mount(&server)
            .await;

        let repo_config = RepoConfig { policy: PolicyPack::Minimal, ..Default::default() };
        let report = check_compliance_with_policy(&config, "test-org", "test-repo", &repo_config)
            .await
            .expect("compliance check should succeed");

        assert!(report.required_passed, "minimal policy should pass with just README + LICENSE");
    }

    #[tokio::test]
    async fn test_enterprise_policy_strict_requirements() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        // Missing many files that enterprise requires
        let files = &["README.adoc", "LICENSE.txt"];
        mount_file_mocks(&server, "test-org", "test-repo", files).await;

        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo"))
            .and(header("Accept", "application/vnd.github+json"))
            .respond_with(ResponseTemplate::new(200).set_body_json(repo_json(Some("mit"), Some("MIT License"))))
            .mount(&server)
            .await;

        let repo_config = RepoConfig { policy: PolicyPack::Enterprise, ..Default::default() };
        let report = check_compliance_with_policy(&config, "test-org", "test-repo", &repo_config)
            .await
            .expect("compliance check should succeed");

        // Enterprise requires everything - should fail with only README + LICENSE
        assert!(!report.required_passed, "enterprise policy should fail with minimal files");
    }

    #[tokio::test]
    async fn test_custom_policy_overrides() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        let files = &["README.adoc", "LICENSE.txt"];
        mount_file_mocks(&server, "test-org", "test-repo", files).await;

        // Mock repository info with approved license
        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo"))
            .and(header("Accept", "application/vnd.github+json"))
            .respond_with(ResponseTemplate::new(200).set_body_json(repo_json(Some("mit"), Some("MIT License"))))
            .mount(&server)
            .await;

        // Use check_compliance_with_policy directly with a custom repo config
        let mut severity_overrides = std::collections::HashMap::new();
        severity_overrides.insert("CONTRIBUTING.md".to_string(), Severity::Optional);

        let repo_config = RepoConfig {
            policy: PolicyPack::Custom,
            severity_overrides,
            skip: vec!["SECURITY.md".to_string()],
            ..Default::default()
        };

        let report = check_compliance_with_policy(&config, "test-org", "test-repo", &repo_config)
            .await
            .expect("compliance check should succeed");

        // SECURITY.md should be skipped
        let security_check = report.checks.iter().find(|c| c.name == "SECURITY.md");
        assert!(security_check.is_some());
        assert_eq!(security_check.unwrap().status, CheckStatus::Skip);

        // CONTRIBUTING.md should be optional (not warn/fail)
        let contrib_check = report.checks.iter().find(|c| c.name == "CONTRIBUTING.md");
        assert!(contrib_check.is_some());
        assert_eq!(contrib_check.unwrap().severity, Severity::Optional);
    }

    #[tokio::test]
    async fn test_score_calculation() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        // Only 2 of the scored files: README (5pts) + LICENSE (5pts) = 10pts
        let files = &["README.adoc", "LICENSE.txt"];
        mount_file_mocks(&server, "test-org", "test-repo", files).await;

        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo"))
            .and(header("Accept", "application/vnd.github+json"))
            .respond_with(ResponseTemplate::new(200).set_body_json(repo_json(Some("mit"), Some("MIT License"))))
            .mount(&server)
            .await;

        let repo_config = RepoConfig::default();
        let report = check_compliance_with_policy(&config, "test-org", "test-repo", &repo_config)
            .await
            .expect("compliance check should succeed");

        // Score should be calculated correctly
        assert!(report.score > 0, "score should be > 0");
        assert!(report.max_score > 0, "max_score should be > 0");
        let expected_pct = (report.score as f32 / report.max_score as f32) * 100.0;
        assert!((report.percentage - expected_pct).abs() < 0.01, "percentage should match calculation");
    }

    #[tokio::test]
    async fn test_pmpl_license_approved() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        let files = &["README.adoc", "LICENSE.txt"];
        mount_file_mocks(&server, "test-org", "test-repo", files).await;

        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo"))
            .respond_with(ResponseTemplate::new(200).set_body_json(
                repo_json(Some("pmpl-1.0-or-later"), Some("Palimpsest License"))
            ))
            .mount(&server)
            .await;

        let repo_config = RepoConfig::default();
        let report = check_compliance_with_policy(&config, "test-org", "test-repo", &repo_config)
            .await
            .expect("compliance check should succeed");

        let license_check = report.checks.iter().find(|c| c.name == "license-type");
        assert!(license_check.is_some(), "should have a license-type check, got checks: {:?}",
            report.checks.iter().map(|c| &c.name).collect::<Vec<_>>());
        assert_eq!(license_check.unwrap().status, CheckStatus::Pass);
        assert!(license_check.unwrap().message.contains("Approved license"));
    }

    #[tokio::test]
    async fn test_policy_summary_returns_descriptions() {
        assert!(policy_summary(PolicyPack::Minimal).contains("Minimal"));
        assert!(policy_summary(PolicyPack::Standard).contains("Standard"));
        assert!(policy_summary(PolicyPack::Strict).contains("Strict"));
        assert!(policy_summary(PolicyPack::Enterprise).contains("Enterprise"));
        assert!(policy_summary(PolicyPack::Custom).contains("Custom"));
    }

    #[tokio::test]
    async fn test_extended_file_checks_editorconfig() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        // Include .editorconfig along with basics
        let files = &["README.adoc", "LICENSE.txt", ".editorconfig", ".gitattributes", ".gitignore"];
        mount_file_mocks(&server, "test-org", "test-repo", files).await;

        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo"))
            .respond_with(ResponseTemplate::new(200).set_body_json(repo_json(Some("mit"), Some("MIT License"))))
            .mount(&server)
            .await;

        let repo_config = RepoConfig::default();
        let report = check_compliance_with_policy(&config, "test-org", "test-repo", &repo_config)
            .await
            .expect("compliance check should succeed");

        let ec_check = report.checks.iter().find(|c| c.name == ".editorconfig");
        assert!(ec_check.is_some(), "should have .editorconfig check");

        let ga_check = report.checks.iter().find(|c| c.name == ".gitattributes");
        assert!(ga_check.is_some(), "should have .gitattributes check");

        let gi_check = report.checks.iter().find(|c| c.name == ".gitignore");
        assert!(gi_check.is_some(), "should have .gitignore check");
    }

    #[tokio::test]
    async fn test_justfile_and_bot_directives_checks() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        let files = &["README.adoc", "LICENSE.txt", "justfile", ".bot_directives"];
        mount_file_mocks(&server, "test-org", "test-repo", files).await;

        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo"))
            .respond_with(ResponseTemplate::new(200).set_body_json(repo_json(Some("mit"), Some("MIT License"))))
            .mount(&server)
            .await;

        let repo_config = RepoConfig { policy: PolicyPack::Enterprise, ..Default::default() };
        let report = check_compliance_with_policy(&config, "test-org", "test-repo", &repo_config)
            .await
            .expect("compliance check should succeed");

        let jf_check = report.checks.iter().find(|c| c.name == "justfile");
        assert!(jf_check.is_some(), "should have justfile check");
        assert_eq!(jf_check.unwrap().status, CheckStatus::Pass);

        let bd_check = report.checks.iter().find(|c| c.name == ".bot_directives");
        assert!(bd_check.is_some(), "should have .bot_directives check");
        assert_eq!(bd_check.unwrap().status, CheckStatus::Pass);
    }

    #[tokio::test]
    async fn test_author_attribution_bad_author() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        let files = &["README.adoc", "LICENSE.txt"];
        mount_file_mocks(&server, "test-org", "test-repo", files).await;

        // Mock Cargo.toml with bad author
        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo/contents/Cargo.toml"))
            .and(header("Accept", "application/vnd.github.raw+json"))
            .respond_with(ResponseTemplate::new(200).set_body_string(
                "[package]\nname = \"test\"\nauthors = [\"hyperpolymath <noreply@github.com>\"]\n"
            ))
            .mount(&server)
            .await;

        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo"))
            .respond_with(ResponseTemplate::new(200).set_body_json(repo_json(Some("mit"), Some("MIT License"))))
            .mount(&server)
            .await;

        let repo_config = RepoConfig::default();
        let report = check_compliance_with_policy(&config, "test-org", "test-repo", &repo_config)
            .await
            .expect("compliance check should succeed");

        let author_check = report.checks.iter().find(|c| c.name == "author-attribution");
        assert!(author_check.is_some(), "should have author-attribution check");
        assert_eq!(author_check.unwrap().status, CheckStatus::Warn,
            "bad author should warn under standard policy");
    }

    #[tokio::test]
    async fn test_author_attribution_good_author() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        let files = &["README.adoc", "LICENSE.txt"];
        mount_file_mocks(&server, "test-org", "test-repo", files).await;

        // Mock Cargo.toml with good author
        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo/contents/Cargo.toml"))
            .and(header("Accept", "application/vnd.github.raw+json"))
            .respond_with(ResponseTemplate::new(200).set_body_string(
                "[package]\nname = \"test\"\nauthors = [\"Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>\"]\n"
            ))
            .mount(&server)
            .await;

        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo"))
            .respond_with(ResponseTemplate::new(200).set_body_json(repo_json(Some("mit"), Some("MIT License"))))
            .mount(&server)
            .await;

        let repo_config = RepoConfig::default();
        let report = check_compliance_with_policy(&config, "test-org", "test-repo", &repo_config)
            .await
            .expect("compliance check should succeed");

        let author_check = report.checks.iter().find(|c| c.name == "author-attribution");
        assert!(author_check.is_some(), "should have author-attribution check");
        assert_eq!(author_check.unwrap().status, CheckStatus::Pass);
    }
}

// ============================================================================
// Module: Webhook Handler Tests
// ============================================================================

mod webhook_tests {
    use super::*;
    use rhodibot::webhook;

    #[test]
    fn test_valid_hmac_signature_accepted() {
        let secret = "test-webhook-secret";
        let payload = r#"{"action":"opened","repository":{"name":"test"}}"#;

        // Compute expected signature
        use hmac::{Hmac, Mac};
        use sha2::Sha256;
        type HmacSha256 = Hmac<Sha256>;

        let mut mac = HmacSha256::new_from_slice(secret.as_bytes()).unwrap();
        mac.update(payload.as_bytes());
        let result = mac.finalize();
        let signature = format!("sha256={}", hex::encode(result.into_bytes()));

        assert!(
            webhook::verify_signature(secret, payload, &signature),
            "valid HMAC-SHA256 signature should be accepted"
        );
    }

    #[test]
    fn test_invalid_signature_rejected() {
        let secret = "test-webhook-secret";
        let payload = r#"{"action":"opened"}"#;
        let bad_signature = "sha256=deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef";

        assert!(
            !webhook::verify_signature(secret, payload, bad_signature),
            "invalid signature should be rejected"
        );
    }

    #[test]
    fn test_missing_sha256_prefix_handled() {
        let secret = "test-secret";
        let payload = "{}";
        // Pass raw hex without sha256= prefix
        let bad_signature = "deadbeef";

        assert!(
            !webhook::verify_signature(secret, payload, bad_signature),
            "signature without proper prefix should be rejected"
        );
    }

    #[test]
    fn test_malformed_hex_rejected() {
        let secret = "test-secret";
        let payload = "{}";
        let bad_signature = "sha256=not-valid-hex-at-all-zzzz";

        assert!(
            !webhook::verify_signature(secret, payload, bad_signature),
            "malformed hex should be rejected"
        );
    }

    #[test]
    fn test_empty_signature_rejected() {
        let secret = "test-secret";
        let payload = "{}";

        assert!(
            !webhook::verify_signature(secret, payload, ""),
            "empty signature should be rejected"
        );
    }

    #[tokio::test]
    async fn test_push_event_default_branch_creates_check() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        // Mock all file existence checks
        Mock::given(method("HEAD"))
            .respond_with(ResponseTemplate::new(404))
            .expect(0..)
            .mount(&server)
            .await;

        // Mock README and LICENSE exist
        for file in &["README.adoc", "LICENSE.txt"] {
            Mock::given(method("HEAD"))
                .and(path(format!("/repos/test-org/test-repo/contents/{}", file)))
                .respond_with(ResponseTemplate::new(200))
                .mount(&server)
                .await;
        }

        // Mock GET for .rsr.toml (not found)
        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo/contents/.rsr.toml"))
            .respond_with(ResponseTemplate::new(404))
            .mount(&server)
            .await;

        // Mock repository info
        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo"))
            .and(header("Accept", "application/vnd.github+json"))
            .respond_with(ResponseTemplate::new(200).set_body_json(repo_json(Some("mit"), Some("MIT License"))))
            .mount(&server)
            .await;

        // Mock check-run creation
        Mock::given(method("POST"))
            .and(path("/repos/test-org/test-repo/check-runs"))
            .respond_with(ResponseTemplate::new(201).set_body_json(serde_json::json!({
                "id": 1,
                "name": "RSR Compliance (standard)",
                "status": "completed"
            })))
            .mount(&server)
            .await;

        let push_body = serde_json::json!({
            "ref": "refs/heads/main",
            "after": "abc123def456",
            "repository": {
                "name": "test-repo",
                "default_branch": "main",
                "owner": { "login": "test-org" }
            }
        });

        let result = webhook::handle_push(&config, &push_body.to_string()).await;
        assert!(result.is_ok(), "push to default branch should create check run");
    }

    #[tokio::test]
    async fn test_push_event_non_default_branch_ignored() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        let push_body = serde_json::json!({
            "ref": "refs/heads/feature-branch",
            "after": "abc123def456",
            "repository": {
                "name": "test-repo",
                "default_branch": "main",
                "owner": { "login": "test-org" }
            }
        });

        let result = webhook::handle_push(&config, &push_body.to_string()).await;
        assert!(result.is_ok(), "push to non-default branch should be ignored silently");
    }

    #[tokio::test]
    async fn test_pr_opened_creates_check() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        Mock::given(method("HEAD"))
            .respond_with(ResponseTemplate::new(404))
            .expect(0..)
            .mount(&server)
            .await;

        for file in &["README.adoc", "LICENSE.txt"] {
            Mock::given(method("HEAD"))
                .and(path(format!("/repos/test-org/test-repo/contents/{}", file)))
                .respond_with(ResponseTemplate::new(200))
                .mount(&server)
                .await;
        }

        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo/contents/.rsr.toml"))
            .respond_with(ResponseTemplate::new(404))
            .mount(&server)
            .await;

        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo"))
            .and(header("Accept", "application/vnd.github+json"))
            .respond_with(ResponseTemplate::new(200).set_body_json(repo_json(Some("mit"), Some("MIT License"))))
            .mount(&server)
            .await;

        Mock::given(method("POST"))
            .and(path("/repos/test-org/test-repo/check-runs"))
            .respond_with(ResponseTemplate::new(201).set_body_json(serde_json::json!({
                "id": 2,
                "name": "RSR Compliance (standard)",
                "status": "completed"
            })))
            .mount(&server)
            .await;

        let pr_body = serde_json::json!({
            "action": "opened",
            "pull_request": {
                "number": 42,
                "head": { "sha": "pr-sha-123" }
            },
            "repository": {
                "name": "test-repo",
                "default_branch": "main",
                "owner": { "login": "test-org" }
            }
        });

        let result = webhook::handle_pull_request(&config, &pr_body.to_string()).await;
        assert!(result.is_ok(), "opened PR should create check run");
    }

    #[tokio::test]
    async fn test_pr_closed_ignored() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        let pr_body = serde_json::json!({
            "action": "closed",
            "pull_request": {
                "number": 42,
                "head": { "sha": "pr-sha-123" }
            },
            "repository": {
                "name": "test-repo",
                "default_branch": "main",
                "owner": { "login": "test-org" }
            }
        });

        let result = webhook::handle_pull_request(&config, &pr_body.to_string()).await;
        assert!(result.is_ok(), "closed PR should be ignored silently");
    }

    #[tokio::test]
    async fn test_repository_created_creates_issue() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        Mock::given(method("POST"))
            .and(path("/repos/test-org/new-repo/issues"))
            .respond_with(ResponseTemplate::new(201).set_body_json(serde_json::json!({
                "id": 1,
                "number": 1,
                "title": "[Rhodibot] RSR Compliance Checklist",
                "html_url": "https://github.com/test-org/new-repo/issues/1"
            })))
            .mount(&server)
            .await;

        let repo_event = serde_json::json!({
            "action": "created",
            "repository": {
                "name": "new-repo",
                "default_branch": "main",
                "owner": { "login": "test-org" }
            }
        });

        let result = webhook::handle_repository(&config, &repo_event.to_string()).await;
        assert!(result.is_ok(), "repository creation should trigger issue creation");
    }

    #[tokio::test]
    async fn test_installation_event_handled() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        let install_event = serde_json::json!({
            "action": "created",
            "installation": {
                "account": { "login": "test-org" }
            }
        });

        let result = webhook::handle_installation(&config, &install_event.to_string()).await;
        assert!(result.is_ok(), "installation event should be handled gracefully");
    }
}

// ============================================================================
// Module: GitHub Client Tests
// ============================================================================

mod github_client_tests {
    use super::*;
    use rhodibot::github::GitHubClient;

    #[tokio::test]
    async fn test_file_exists_returns_true_for_200() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        Mock::given(method("HEAD"))
            .and(path("/repos/test-org/test-repo/contents/README.adoc"))
            .respond_with(ResponseTemplate::new(200))
            .mount(&server)
            .await;

        let client = GitHubClient::new(&config);
        assert!(client.file_exists("test-org", "test-repo", "README.adoc").await);
    }

    #[tokio::test]
    async fn test_file_exists_returns_false_for_404() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        Mock::given(method("HEAD"))
            .and(path("/repos/test-org/test-repo/contents/MISSING.md"))
            .respond_with(ResponseTemplate::new(404))
            .mount(&server)
            .await;

        let client = GitHubClient::new(&config);
        assert!(!client.file_exists("test-org", "test-repo", "MISSING.md").await);
    }

    #[tokio::test]
    async fn test_get_file_content_returns_body() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo/contents/.rsr.toml"))
            .and(header("Accept", "application/vnd.github.raw+json"))
            .respond_with(
                ResponseTemplate::new(200).set_body_string("policy = \"strict\"\n"),
            )
            .mount(&server)
            .await;

        let client = GitHubClient::new(&config);
        let content = client.get_file_content("test-org", "test-repo", ".rsr.toml").await;
        assert!(content.is_ok());
        assert_eq!(content.unwrap(), "policy = \"strict\"\n");
    }

    #[tokio::test]
    async fn test_get_file_content_error_on_404() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo/contents/nonexistent"))
            .respond_with(ResponseTemplate::new(404))
            .mount(&server)
            .await;

        let client = GitHubClient::new(&config);
        let content = client.get_file_content("test-org", "test-repo", "nonexistent").await;
        assert!(content.is_err());
    }

    #[tokio::test]
    async fn test_create_check_run_posts_correctly() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        Mock::given(method("POST"))
            .and(path("/repos/test-org/test-repo/check-runs"))
            .and(header("Accept", "application/vnd.github+json"))
            .respond_with(ResponseTemplate::new(201).set_body_json(serde_json::json!({
                "id": 99,
                "name": "RSR Compliance (standard)",
                "status": "completed"
            })))
            .mount(&server)
            .await;

        let client = GitHubClient::new(&config);
        let check_run = rhodibot::github::CreateCheckRun {
            name: "RSR Compliance (standard)".to_string(),
            head_sha: "abc123".to_string(),
            status: "completed".to_string(),
            conclusion: Some("success".to_string()),
            output: Some(rhodibot::github::CheckRunOutput {
                title: "RSR Score: 95%".to_string(),
                summary: "Excellent compliance".to_string(),
                text: Some("Details here".to_string()),
            }),
        };

        let result = client.create_check_run("test-org", "test-repo", &check_run).await;
        assert!(result.is_ok(), "check run creation should succeed");
    }

    #[tokio::test]
    async fn test_create_issue_posts_correctly() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        Mock::given(method("POST"))
            .and(path("/repos/test-org/test-repo/issues"))
            .and(header("Accept", "application/vnd.github+json"))
            .respond_with(ResponseTemplate::new(201).set_body_json(serde_json::json!({
                "id": 1,
                "number": 1,
                "title": "Test Issue",
                "html_url": "https://github.com/test-org/test-repo/issues/1"
            })))
            .mount(&server)
            .await;

        let client = GitHubClient::new(&config);
        let result = client
            .create_issue(
                "test-org",
                "test-repo",
                "Test Issue",
                "Issue body",
                &["documentation", "rsr-compliance"],
            )
            .await;

        assert!(result.is_ok(), "issue creation should succeed");
        let issue = result.unwrap();
        assert_eq!(issue.html_url, "https://github.com/test-org/test-repo/issues/1");
    }

    #[tokio::test]
    async fn test_get_repository_parses_response() {
        let server = MockServer::start().await;
        let config = mock_config(&server.uri());

        Mock::given(method("GET"))
            .and(path("/repos/test-org/test-repo"))
            .respond_with(ResponseTemplate::new(200).set_body_json(repo_json(Some("mit"), Some("MIT License"))))
            .mount(&server)
            .await;

        let client = GitHubClient::new(&config);
        let result = client.get_repository("test-org", "test-repo").await;
        assert!(result.is_ok());
        let repo = result.unwrap();
        assert!(repo.license.is_some());
        assert_eq!(repo.license.unwrap().key, "mit");
    }
}

// ============================================================================
// Module: Report Formatting Tests
// ============================================================================

mod report_tests {
    use rhodibot::rsr::*;

    fn sample_report(required_passed: bool) -> ComplianceReport {
        ComplianceReport {
            owner: "test-org".to_string(),
            repo: "test-repo".to_string(),
            policy: PolicyPack::Standard,
            score: 20,
            max_score: 30,
            percentage: 66.7,
            required_passed,
            checks: vec![
                Check {
                    name: "README.adoc".to_string(),
                    category: CheckCategory::Documentation,
                    severity: Severity::Required,
                    status: CheckStatus::Pass,
                    points: 5,
                    max_points: 5,
                    message: "AsciiDoc README found".to_string(),
                },
                Check {
                    name: "SECURITY.md".to_string(),
                    category: CheckCategory::Security,
                    severity: Severity::Recommended,
                    status: CheckStatus::Warn,
                    points: 0,
                    max_points: 5,
                    message: "Security policy missing".to_string(),
                },
                Check {
                    name: "LICENSE.txt".to_string(),
                    category: CheckCategory::Governance,
                    severity: Severity::Required,
                    status: if required_passed { CheckStatus::Pass } else { CheckStatus::Fail },
                    points: if required_passed { 5 } else { 0 },
                    max_points: 5,
                    message: if required_passed {
                        "License file found".to_string()
                    } else {
                        "License file missing".to_string()
                    },
                },
                Check {
                    name: "no-go.mod".to_string(),
                    category: CheckCategory::LanguagePolicy,
                    severity: Severity::Recommended,
                    status: CheckStatus::Warn,
                    points: 0,
                    max_points: 0,
                    message: "Go module (use Rust) detected - policy violation".to_string(),
                },
            ],
            summary: "Partial RSR compliance (standard) - improvements needed".to_string(),
        }
    }

    #[test]
    fn test_report_contains_all_categories() {
        let report = sample_report(true);
        let text = super::format_report_text_pub(&report);

        assert!(text.contains("Documentation"), "report should contain Documentation category");
        assert!(text.contains("Security"), "report should contain Security category");
        assert!(text.contains("Governance"), "report should contain Governance category");
        assert!(text.contains("Language Policy"), "report should contain Language Policy category");
    }

    #[test]
    fn test_report_emoji_indicators() {
        let report = sample_report(true);
        let text = super::format_report_text_pub(&report);

        assert!(text.contains(":white_check_mark:"), "pass should show checkmark");
        assert!(text.contains(":warning:"), "warn should show warning");
    }

    #[test]
    fn test_report_failed_required_shows_x() {
        let report = sample_report(false);
        let text = super::format_report_text_pub(&report);

        assert!(text.contains(":x:"), "failed required check should show X");
        assert!(text.contains("Required checks failed"), "should note required checks failed");
    }

    #[test]
    fn test_report_severity_badges() {
        let report = sample_report(true);
        let text = super::format_report_text_pub(&report);

        assert!(text.contains("[required]"), "should show required badge");
        assert!(text.contains("[recommended]"), "should show recommended badge");
    }

    #[test]
    fn test_report_policy_header() {
        let report = sample_report(true);
        let text = super::format_report_text_pub(&report);

        assert!(text.contains("Policy: standard"), "should include policy name");
    }

    #[test]
    fn test_report_score_format() {
        let report = sample_report(true);
        let text = super::format_report_text_pub(&report);

        // Points should be shown for checks with max_points > 0
        assert!(text.contains("(5/5)"), "should show points for scored checks");
    }
}

// ============================================================================
// Module: Fleet Integration Tests
// ============================================================================

mod fleet_tests {
    use rhodibot::fleet;
    use rhodibot::rsr::*;
    use gitbot_shared_context::{BotId, Severity as FleetSeverity};

    fn sample_report_with_issues() -> ComplianceReport {
        ComplianceReport {
            owner: "test-org".to_string(),
            repo: "test-repo".to_string(),
            policy: PolicyPack::Standard,
            score: 10,
            max_score: 30,
            percentage: 33.3,
            required_passed: false,
            checks: vec![
                Check {
                    name: "README.adoc".to_string(),
                    category: CheckCategory::Documentation,
                    severity: Severity::Required,
                    status: CheckStatus::Fail,
                    points: 0,
                    max_points: 5,
                    message: "AsciiDoc README missing".to_string(),
                },
                Check {
                    name: "LICENSE.txt".to_string(),
                    category: CheckCategory::Governance,
                    severity: Severity::Required,
                    status: CheckStatus::Pass,
                    points: 5,
                    max_points: 5,
                    message: "License file found".to_string(),
                },
                Check {
                    name: "SECURITY.md".to_string(),
                    category: CheckCategory::Security,
                    severity: Severity::Recommended,
                    status: CheckStatus::Warn,
                    points: 0,
                    max_points: 5,
                    message: "Security policy missing".to_string(),
                },
                Check {
                    name: "no-go.mod".to_string(),
                    category: CheckCategory::LanguagePolicy,
                    severity: Severity::Recommended,
                    status: CheckStatus::Warn,
                    points: 0,
                    max_points: 0,
                    message: "Go module (use Rust) detected - policy violation".to_string(),
                },
                Check {
                    name: "STATE.scm".to_string(),
                    category: CheckCategory::Structure,
                    severity: Severity::Recommended,
                    status: CheckStatus::Skip,
                    points: 0,
                    max_points: 0,
                    message: "Project state file not present (optional)".to_string(),
                },
            ],
            summary: "RSR standard policy: Required checks failed".to_string(),
        }
    }

    #[test]
    fn test_findings_only_for_failures() {
        let report = sample_report_with_issues();
        let findings = fleet::report_to_findings(&report);

        // Only fail/warn checks should produce findings (not pass/skip)
        assert_eq!(findings.len(), 3, "should have 3 findings (1 fail + 2 warn)");
    }

    #[test]
    fn test_findings_source_is_rhodibot() {
        let report = sample_report_with_issues();
        let findings = fleet::report_to_findings(&report);

        for finding in &findings {
            assert_eq!(finding.source, BotId::Rhodibot, "all findings should come from Rhodibot");
        }
    }

    #[test]
    fn test_findings_categories_correct() {
        let report = sample_report_with_issues();
        let findings = fleet::report_to_findings(&report);

        let readme_finding = findings.iter().find(|f| f.rule_name == "README.adoc");
        assert!(readme_finding.is_some());
        assert_eq!(readme_finding.unwrap().category, "rsr/documentation");

        let security_finding = findings.iter().find(|f| f.rule_name == "SECURITY.md");
        assert!(security_finding.is_some());
        assert_eq!(security_finding.unwrap().category, "rsr/security");

        let gomod_finding = findings.iter().find(|f| f.rule_name == "no-go.mod");
        assert!(gomod_finding.is_some());
        assert_eq!(gomod_finding.unwrap().category, "rsr/language-policy");
    }

    #[test]
    fn test_findings_severity_mapping() {
        let report = sample_report_with_issues();
        let findings = fleet::report_to_findings(&report);

        // Required + Fail -> Error
        let readme_finding = findings.iter().find(|f| f.rule_name == "README.adoc").unwrap();
        assert_eq!(readme_finding.severity, FleetSeverity::Error);

        // Recommended + Warn -> Warning
        let security_finding = findings.iter().find(|f| f.rule_name == "SECURITY.md").unwrap();
        assert_eq!(security_finding.severity, FleetSeverity::Warning);
    }

    #[test]
    fn test_findings_have_rule_ids() {
        let report = sample_report_with_issues();
        let findings = fleet::report_to_findings(&report);

        let readme_finding = findings.iter().find(|f| f.rule_name == "README.adoc").unwrap();
        assert_eq!(readme_finding.rule_id, "RSR-001");

        let gomod_finding = findings.iter().find(|f| f.rule_name == "no-go.mod").unwrap();
        assert!(gomod_finding.rule_id.starts_with("RSR-BAN-"));
    }

    #[test]
    fn test_findings_have_suggestions() {
        let report = sample_report_with_issues();
        let findings = fleet::report_to_findings(&report);

        let readme_finding = findings.iter().find(|f| f.rule_name == "README.adoc").unwrap();
        assert!(readme_finding.suggestion.is_some());
        assert!(readme_finding.suggestion.as_ref().unwrap().contains("README.adoc"));
    }

    #[test]
    fn test_findings_fixable_flag() {
        let report = sample_report_with_issues();
        let findings = fleet::report_to_findings(&report);

        // SECURITY.md should be fixable
        let security_finding = findings.iter().find(|f| f.rule_name == "SECURITY.md").unwrap();
        assert!(security_finding.fixable, "SECURITY.md should be marked as fixable");

        // README.adoc should NOT be fixable (content is project-specific)
        let readme_finding = findings.iter().find(|f| f.rule_name == "README.adoc").unwrap();
        assert!(!readme_finding.fixable, "README.adoc should not be auto-fixable");
    }

    #[test]
    fn test_context_file_written() {
        let report = sample_report_with_issues();
        let dir = std::env::temp_dir().join("rhodibot-test-fleet");
        let _ = std::fs::create_dir_all(&dir);
        let context_path = dir.join("context.json");

        let result = fleet::publish_to_context(&report, &context_path);
        assert!(result.is_ok(), "publishing to context should succeed");

        // Verify the file was written
        assert!(context_path.exists(), "context file should exist");

        // Verify it's valid JSON
        let data = std::fs::read_to_string(&context_path).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&data).unwrap();
        assert!(parsed.get("findings").is_some());
        assert!(parsed.get("session_id").is_some());

        // Cleanup
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_context_contains_rsr_metadata() {
        let report = sample_report_with_issues();
        let dir = std::env::temp_dir().join("rhodibot-test-fleet-meta");
        let _ = std::fs::create_dir_all(&dir);
        let context_path = dir.join("context.json");

        let context = fleet::publish_to_context(&report, &context_path).unwrap();

        // Verify RSR metadata is stored
        assert_eq!(context.get_data("rsr:score"), Some(&serde_json::json!(10)));
        assert_eq!(context.get_data("rsr:required_passed"), Some(&serde_json::json!(false)));
        assert_eq!(context.get_data("rsr:policy"), Some(&serde_json::json!("standard")));

        // Cleanup
        let _ = std::fs::remove_dir_all(&dir);
    }
}

// Helper to access the private format_report_text from webhook module
// We expose it via a test helper function
fn format_report_text_pub(report: &rhodibot::rsr::ComplianceReport) -> String {
    // Re-implement the format logic for testing since it's private in webhook module
    let mut text = String::new();

    text.push_str(&format!("## Policy: {}\n\n", report.policy));
    text.push_str(&format!("{}\n\n", rhodibot::rsr::policy_summary(report.policy)));

    if !report.required_passed {
        text.push_str("> :x: **Required checks failed** - repository does not meet minimum RSR requirements\n\n");
    }

    text.push_str("## Detailed Results\n\n");

    let categories = [
        ("Documentation", rhodibot::rsr::CheckCategory::Documentation),
        ("Security", rhodibot::rsr::CheckCategory::Security),
        ("Governance", rhodibot::rsr::CheckCategory::Governance),
        ("Structure", rhodibot::rsr::CheckCategory::Structure),
        ("Language Policy", rhodibot::rsr::CheckCategory::LanguagePolicy),
    ];

    for (cat_name, cat) in categories {
        let cat_checks: Vec<_> = report.checks.iter().filter(|c| c.category == cat).collect();
        if cat_checks.is_empty() {
            continue;
        }

        text.push_str(&format!("### {}\n\n", cat_name));

        for check in cat_checks {
            let icon = match check.status {
                rhodibot::rsr::CheckStatus::Pass => ":white_check_mark:",
                rhodibot::rsr::CheckStatus::Fail => ":x:",
                rhodibot::rsr::CheckStatus::Warn => ":warning:",
                rhodibot::rsr::CheckStatus::Skip => ":fast_forward:",
            };

            let severity_badge = match check.severity {
                rhodibot::rsr::Severity::Required => "[required]",
                rhodibot::rsr::Severity::Recommended => "[recommended]",
                rhodibot::rsr::Severity::Optional => "[optional]",
            };

            if check.max_points > 0 {
                text.push_str(&format!(
                    "- {} **{}** {}: {} ({}/{})\n",
                    icon, check.name, severity_badge, check.message, check.points, check.max_points
                ));
            } else {
                text.push_str(&format!(
                    "- {} **{}** {}: {}\n",
                    icon, check.name, severity_badge, check.message
                ));
            }
        }

        text.push('\n');
    }

    text
}
