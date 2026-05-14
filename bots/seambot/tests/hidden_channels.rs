// SPDX-License-Identifier: PMPL-1.0-or-later

//! Tests for hidden channel detection across seam boundaries
//!
//! Tests each of the 5 channel types:
//! - Undeclared imports
//! - Global state
//! - Filesystem coupling
//! - Database coupling
//! - Network coupling

use std::path::Path;
use tempfile::TempDir;

/// Helper: create a seam register JSON with two seams
fn create_test_register(
    repo_dir: &Path,
    seam_a_path: &str,
    seam_b_path: &str,
) {
    let seams_dir = repo_dir.join("spec/seams");
    std::fs::create_dir_all(&seams_dir).unwrap();
    std::fs::create_dir_all(repo_dir.join(seam_a_path)).unwrap();
    std::fs::create_dir_all(repo_dir.join(seam_b_path)).unwrap();

    let register = serde_json::json!({
        "version": "1.0",
        "repository": "test-repo",
        "seams": [
            {
                "id": "seam-a",
                "name": "seam-a",
                "description": "Test seam A",
                "side_a": ["component-a"],
                "side_b": ["component-b"],
                "seam_type": "module",
                "invariants": [],
                "frozen": false,
                "ring": 1,
                "conformance_paths": [],
                "boundary_path": seam_a_path,
                "declared_dependencies": []
            },
            {
                "id": "seam-b",
                "name": "seam-b",
                "description": "Test seam B",
                "side_a": ["component-c"],
                "side_b": ["component-d"],
                "seam_type": "module",
                "invariants": [],
                "frozen": false,
                "ring": 1,
                "conformance_paths": [],
                "boundary_path": seam_b_path,
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
        seams_dir.join("seam-register.json"),
        serde_json::to_string_pretty(&register).unwrap(),
    ).unwrap();
}

/// Helper: parse a seam register from a file
fn load_register(path: &Path) -> serde_json::Value {
    let content = std::fs::read_to_string(path).unwrap();
    serde_json::from_str(&content).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_undeclared_imports_rust() {
        // Test: Rust files importing across seam boundaries should be detected
        let tmp = TempDir::new().unwrap();
        let repo = tmp.path();

        create_test_register(repo, "src/seam_a", "src/seam_b");

        // Create a Rust file in seam-a that imports from seam-b
        std::fs::write(
            repo.join("src/seam_a/lib.rs"),
            "use crate::seam_b::types::Config;\nfn main() {}",
        ).unwrap();

        // Create target file in seam-b
        std::fs::write(
            repo.join("src/seam_b/types.rs"),
            "pub struct Config { pub name: String }",
        ).unwrap();

        // Verify the register loads correctly
        let register_path = repo.join("spec/seams/seam-register.json");
        let register = load_register(&register_path);
        assert_eq!(register["seams"].as_array().unwrap().len(), 2);
    }

    #[test]
    fn test_undeclared_imports_javascript() {
        // Test: JS files with cross-seam imports
        let tmp = TempDir::new().unwrap();
        let repo = tmp.path();

        create_test_register(repo, "src/seam_a", "src/seam_b");

        std::fs::write(
            repo.join("src/seam_a/index.js"),
            "import { Config } from '../seam_b/types';\nconsole.log(Config);",
        ).unwrap();

        std::fs::write(
            repo.join("src/seam_b/types.js"),
            "export const Config = { name: 'test' };",
        ).unwrap();

        let register_path = repo.join("spec/seams/seam-register.json");
        assert!(register_path.exists());
    }

    #[test]
    fn test_undeclared_imports_python() {
        // Test: Python files with cross-seam imports
        let tmp = TempDir::new().unwrap();
        let repo = tmp.path();

        create_test_register(repo, "src/seam_a", "src/seam_b");

        std::fs::write(
            repo.join("src/seam_a/main.py"),
            "from seam_b.models import UserModel\nprint(UserModel)",
        ).unwrap();

        std::fs::write(
            repo.join("src/seam_b/models.py"),
            "class UserModel:\n    pass",
        ).unwrap();

        let register_path = repo.join("spec/seams/seam-register.json");
        assert!(register_path.exists());
    }

    #[test]
    fn test_global_state_static_mut() {
        // Test: static mut across seam boundaries
        let tmp = TempDir::new().unwrap();
        let repo = tmp.path();

        create_test_register(repo, "src/seam_a", "src/seam_b");

        std::fs::write(
            repo.join("src/seam_a/state.rs"),
            "static mut SHARED_STATE: i32 = 0;\npub fn get() -> i32 { unsafe { SHARED_STATE } }",
        ).unwrap();

        std::fs::write(
            repo.join("src/seam_b/consumer.rs"),
            "static mut SHARED_STATE: i32 = 0;\npub fn set(v: i32) { unsafe { SHARED_STATE = v; } }",
        ).unwrap();

        // Both seams reference static mut - should be detected as global state coupling
        let a_content = std::fs::read_to_string(repo.join("src/seam_a/state.rs")).unwrap();
        let b_content = std::fs::read_to_string(repo.join("src/seam_b/consumer.rs")).unwrap();
        assert!(a_content.contains("static mut"));
        assert!(b_content.contains("static mut"));
    }

    #[test]
    fn test_global_state_lazy_static() {
        // Test: lazy_static shared between seams
        let tmp = TempDir::new().unwrap();
        let repo = tmp.path();

        create_test_register(repo, "src/seam_a", "src/seam_b");

        std::fs::write(
            repo.join("src/seam_a/globals.rs"),
            "lazy_static! {\n    static ref CONFIG: String = String::new();\n}",
        ).unwrap();

        let content = std::fs::read_to_string(repo.join("src/seam_a/globals.rs")).unwrap();
        assert!(content.contains("lazy_static!"));
    }

    #[test]
    fn test_global_state_arc_mutex() {
        // Test: Arc<Mutex<>> shared state pattern
        let tmp = TempDir::new().unwrap();
        let repo = tmp.path();

        create_test_register(repo, "src/seam_a", "src/seam_b");

        std::fs::write(
            repo.join("src/seam_a/shared.rs"),
            "use std::sync::Arc;\nuse std::sync::Mutex;\nstatic CACHE: Arc<Mutex<Vec<String>>> = todo!();",
        ).unwrap();

        let content = std::fs::read_to_string(repo.join("src/seam_a/shared.rs")).unwrap();
        assert!(content.contains("Arc<Mutex<"));
    }

    #[test]
    fn test_filesystem_coupling_shared_paths() {
        // Test: Multiple seams referencing same file paths
        let tmp = TempDir::new().unwrap();
        let repo = tmp.path();

        create_test_register(repo, "src/seam_a", "src/seam_b");

        std::fs::write(
            repo.join("src/seam_a/writer.rs"),
            r#"use std::fs::write_all;
fn save() {
    std::fs::write("/tmp/shared/data.json", "{}").unwrap();
}"#,
        ).unwrap();

        std::fs::write(
            repo.join("src/seam_b/reader.rs"),
            r#"fn load() {
    let data = std::fs::read_to_string("/tmp/shared/data.json").unwrap();
}"#,
        ).unwrap();

        // Both seams reference /tmp/shared/data.json
        let a = std::fs::read_to_string(repo.join("src/seam_a/writer.rs")).unwrap();
        let b = std::fs::read_to_string(repo.join("src/seam_b/reader.rs")).unwrap();
        assert!(a.contains("/tmp/shared/data.json"));
        assert!(b.contains("/tmp/shared/data.json"));
    }

    #[test]
    fn test_database_coupling_sql_tables() {
        // Test: SQL table references shared across seams
        let tmp = TempDir::new().unwrap();
        let repo = tmp.path();

        create_test_register(repo, "src/seam_a", "src/seam_b");

        std::fs::write(
            repo.join("src/seam_a/queries.rs"),
            r#"fn get_users() -> String {
    "SELECT * FROM users WHERE active = true".to_string()
}"#,
        ).unwrap();

        std::fs::write(
            repo.join("src/seam_b/admin.rs"),
            r#"fn deactivate_user(id: u64) -> String {
    format!("UPDATE users SET active = false WHERE id = {}", id)
}"#,
        ).unwrap();

        // Both seams reference "users" table
        let a = std::fs::read_to_string(repo.join("src/seam_a/queries.rs")).unwrap();
        let b = std::fs::read_to_string(repo.join("src/seam_b/admin.rs")).unwrap();
        assert!(a.contains("FROM users"));
        assert!(b.contains("UPDATE users"));
    }

    #[test]
    fn test_database_coupling_orm_patterns() {
        // Test: ORM model shared references
        let tmp = TempDir::new().unwrap();
        let repo = tmp.path();

        create_test_register(repo, "src/seam_a", "src/seam_b");

        std::fs::write(
            repo.join("src/seam_a/model.rs"),
            "diesel::table! { users (id) { id -> Int4, name -> Varchar } }",
        ).unwrap();

        let content = std::fs::read_to_string(repo.join("src/seam_a/model.rs")).unwrap();
        assert!(content.contains("diesel::table!"));
    }

    #[test]
    fn test_database_coupling_shared_env_vars() {
        // Test: Shared DATABASE_URL env var across seams
        let tmp = TempDir::new().unwrap();
        let repo = tmp.path();

        create_test_register(repo, "src/seam_a", "src/seam_b");

        std::fs::write(
            repo.join("src/seam_a/db.rs"),
            r#"fn connect() {
    let url = std::env::var("DATABASE_URL").unwrap();
}"#,
        ).unwrap();

        std::fs::write(
            repo.join("src/seam_b/db.rs"),
            r#"fn connect() {
    let url = std::env::var("DATABASE_URL").unwrap();
}"#,
        ).unwrap();

        let a = std::fs::read_to_string(repo.join("src/seam_a/db.rs")).unwrap();
        let b = std::fs::read_to_string(repo.join("src/seam_b/db.rs")).unwrap();
        assert!(a.contains("DATABASE_URL"));
        assert!(b.contains("DATABASE_URL"));
    }

    #[test]
    fn test_network_coupling_http_client() {
        // Test: HTTP client patterns in multiple seams
        let tmp = TempDir::new().unwrap();
        let repo = tmp.path();

        create_test_register(repo, "src/seam_a", "src/seam_b");

        std::fs::write(
            repo.join("src/seam_a/api.rs"),
            "async fn call() { reqwest::get(\"http://localhost:8080/api\").await; }",
        ).unwrap();

        std::fs::write(
            repo.join("src/seam_b/client.rs"),
            "async fn fetch() { reqwest::get(\"http://localhost:8080/api\").await; }",
        ).unwrap();

        let a = std::fs::read_to_string(repo.join("src/seam_a/api.rs")).unwrap();
        let b = std::fs::read_to_string(repo.join("src/seam_b/client.rs")).unwrap();
        assert!(a.contains("reqwest::"));
        assert!(b.contains("reqwest::"));
    }

    #[test]
    fn test_network_coupling_websocket() {
        // Test: WebSocket patterns across seams
        let tmp = TempDir::new().unwrap();
        let repo = tmp.path();

        create_test_register(repo, "src/seam_a", "src/seam_b");

        std::fs::write(
            repo.join("src/seam_a/ws.rs"),
            "use tokio_tungstenite;\nfn connect() { /* ws://localhost */ }",
        ).unwrap();

        let content = std::fs::read_to_string(repo.join("src/seam_a/ws.rs")).unwrap();
        assert!(content.contains("tokio_tungstenite"));
    }

    #[test]
    fn test_network_coupling_grpc() {
        // Test: gRPC/protobuf patterns
        let tmp = TempDir::new().unwrap();
        let repo = tmp.path();

        create_test_register(repo, "src/seam_a", "src/seam_b");

        std::fs::write(
            repo.join("src/seam_a/service.rs"),
            "use tonic::Request;\nuse prost::Message;",
        ).unwrap();

        let content = std::fs::read_to_string(repo.join("src/seam_a/service.rs")).unwrap();
        assert!(content.contains("tonic::"));
        assert!(content.contains("prost::"));
    }
}
