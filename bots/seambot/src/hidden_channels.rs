// SPDX-License-Identifier: PMPL-1.0-or-later

//! Hidden channel detection for seam boundaries
//!
//! Detects undeclared coupling between seams through:
//! - Undeclared imports across seam boundaries
//! - Shared global state
//! - Filesystem coupling (shared files/directories)
//! - Database coupling (shared tables/schemas)
//! - Network calls across boundaries

use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

use crate::seam::SeamRegister;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HiddenChannel {
    pub channel_type: ChannelType,
    pub source_seam: String,
    pub target_seam: String,
    pub evidence: String,
    pub severity: Severity,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ChannelType {
    UndeclaredImport,
    GlobalState,
    FilesystemCoupling,
    DatabaseCoupling,
    NetworkCoupling,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Severity {
    Critical,  // Violates seam integrity
    High,      // Bypasses declared interfaces
    Medium,    // Implicit coupling
    Low,       // Potential issue
}

impl HiddenChannel {
    pub fn new(
        channel_type: ChannelType,
        source_seam: impl Into<String>,
        target_seam: impl Into<String>,
        evidence: impl Into<String>,
        severity: Severity,
    ) -> Self {
        Self {
            channel_type,
            source_seam: source_seam.into(),
            target_seam: target_seam.into(),
            evidence: evidence.into(),
            severity,
        }
    }
}

/// Detect all hidden channels in a repository
pub fn detect_hidden_channels(
    register: &SeamRegister,
    repo_path: &Path,
) -> Result<Vec<HiddenChannel>> {
    let mut channels = Vec::new();

    // Detect undeclared imports
    channels.extend(detect_undeclared_imports(register, repo_path)?);

    // Detect shared global state
    channels.extend(detect_global_state(register, repo_path)?);

    // Detect filesystem coupling
    channels.extend(detect_filesystem_coupling(register, repo_path)?);

    // Detect database coupling
    channels.extend(detect_database_coupling(register, repo_path)?);

    // Detect network coupling
    channels.extend(detect_network_coupling(register, repo_path)?);

    Ok(channels)
}

/// Detect undeclared imports across seam boundaries
fn detect_undeclared_imports(
    register: &SeamRegister,
    repo_path: &Path,
) -> Result<Vec<HiddenChannel>> {
    let mut channels = Vec::new();

    // Build a map of file paths to seams
    let file_seam_map = build_file_seam_map(register, repo_path);

    // Scan all source files for imports
    for entry in WalkDir::new(repo_path)
        .follow_links(false)
        .into_iter()
        .filter_entry(|e| !is_ignored(e.file_name().to_str().unwrap_or("")))
        .filter_map(|e| e.ok())
    {
        let entry_path = entry.path();

        if !is_source_file(entry_path) {
            continue;
        }

        if let Some(source_seam) = file_seam_map.get(entry_path) {
            if let Ok(imports) = extract_imports(entry_path) {
                for import_path in imports {
                    // Resolve import to actual file
                    if let Some(target_file) = resolve_import(&import_path, entry_path, repo_path) {
                        if let Some(target_seam) = file_seam_map.get(&target_file) {
                            // Check if import crosses seam boundary
                            if source_seam != target_seam {
                                // Check if this import is declared in the seam interface
                                if !is_declared_dependency(register, source_seam, target_seam) {
                                    channels.push(HiddenChannel::new(
                                        ChannelType::UndeclaredImport,
                                        source_seam,
                                        target_seam,
                                        format!("Import from {} to {}", entry_path.display(), target_file.display()),
                                        Severity::High,
                                    ));
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    Ok(channels)
}

/// Detect shared global state between seams
fn detect_global_state(
    register: &SeamRegister,
    repo_path: &Path,
) -> Result<Vec<HiddenChannel>> {
    let mut channels = Vec::new();
    let file_seam_map = build_file_seam_map(register, repo_path);

    // Patterns indicating global state
    let global_patterns = [
        "static mut ",
        "lazy_static!",
        "once_cell",
        "global ",
        "GLOBAL_",
        "Arc<Mutex<",
        "Arc<RwLock<",
    ];

    let mut global_vars: HashMap<String, HashSet<String>> = HashMap::new();

    for entry in WalkDir::new(repo_path)
        .follow_links(false)
        .into_iter()
        .filter_entry(|e| !is_ignored(e.file_name().to_str().unwrap_or("")))
        .filter_map(|e| e.ok())
    {
        let entry_path = entry.path();

        if !is_source_file(entry_path) {
            continue;
        }

        if let Some(seam_name) = file_seam_map.get(entry_path) {
            if let Ok(content) = std::fs::read_to_string(entry_path) {
                for pattern in &global_patterns {
                    if content.contains(pattern) {
                        // Extract variable name (simplified)
                        for line in content.lines() {
                            if line.contains(pattern) {
                                let var_name = extract_var_name(line);
                                global_vars
                                    .entry(var_name)
                                    .or_default()
                                    .insert(seam_name.clone());
                            }
                        }
                    }
                }
            }
        }
    }

    // Find globals shared across seams
    for (var_name, seams) in &global_vars {
        if seams.len() > 1 {
            let seams_vec: Vec<_> = seams.iter().cloned().collect();
            for i in 0..seams_vec.len() {
                for j in (i + 1)..seams_vec.len() {
                    channels.push(HiddenChannel::new(
                        ChannelType::GlobalState,
                        &seams_vec[i],
                        &seams_vec[j],
                        format!("Shared global variable: {}", var_name),
                        Severity::Critical,
                    ));
                }
            }
        }
    }

    Ok(channels)
}

/// Detect filesystem coupling between seams
fn detect_filesystem_coupling(
    register: &SeamRegister,
    repo_path: &Path,
) -> Result<Vec<HiddenChannel>> {
    let mut channels = Vec::new();
    let file_seam_map = build_file_seam_map(register, repo_path);

    // Patterns indicating filesystem operations
    let fs_patterns = [
        "std::fs::",
        "tokio::fs::",
        "File::open",
        "File::create",
        "read_to_string",
        "write_all",
    ];

    let mut fs_paths: HashMap<String, HashSet<String>> = HashMap::new();

    for entry in WalkDir::new(repo_path)
        .follow_links(false)
        .into_iter()
        .filter_entry(|e| !is_ignored(e.file_name().to_str().unwrap_or("")))
        .filter_map(|e| e.ok())
    {
        let entry_path = entry.path();

        if !is_source_file(entry_path) {
            continue;
        }

        if let Some(seam_name) = file_seam_map.get(entry_path) {
            if let Ok(content) = std::fs::read_to_string(entry_path) {
                for pattern in &fs_patterns {
                    if content.contains(pattern) {
                        // Extract file paths (simplified - would need better parsing)
                        for line in content.lines() {
                            if line.contains(pattern) {
                                if let Some(path) = extract_path_literal(line) {
                                    fs_paths
                                        .entry(path)
                                        .or_default()
                                        .insert(seam_name.clone());
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Find filesystem paths accessed by multiple seams
    for (path, seams) in &fs_paths {
        if seams.len() > 1 {
            let seams_vec: Vec<_> = seams.iter().cloned().collect();
            for i in 0..seams_vec.len() {
                for j in (i + 1)..seams_vec.len() {
                    channels.push(HiddenChannel::new(
                        ChannelType::FilesystemCoupling,
                        &seams_vec[i],
                        &seams_vec[j],
                        format!("Shared file access: {}", path),
                        Severity::Medium,
                    ));
                }
            }
        }
    }

    Ok(channels)
}

/// Detect database coupling between seams
///
/// Scans source files for SQL table references, ORM model usage, and
/// shared database connection patterns. When multiple seams access the
/// same table or database resource, this indicates undeclared coupling.
fn detect_database_coupling(
    register: &SeamRegister,
    repo_path: &Path,
) -> Result<Vec<HiddenChannel>> {
    let mut channels = Vec::new();
    let file_seam_map = build_file_seam_map(register, repo_path);

    // SQL patterns that reference table names
    let sql_table_re = regex::Regex::new(
        r"(?i)(?:FROM|INTO|UPDATE|JOIN|CREATE\s+TABLE|ALTER\s+TABLE|DROP\s+TABLE|INSERT\s+INTO|DELETE\s+FROM)\s+[`\x22]?(\w+)"
    )?;

    // ORM patterns indicating model/table binding
    let orm_patterns = [
        "diesel::table!",
        "sqlx::query",
        "sea_orm::Entity",
        "#[table_name",
        "ActiveModel",
        "Schema::create_table",
        "migration::Migration",
        "models.Model",
        "Base.metadata",
        "sequelize.define",
        "mongoose.model",
        "Ecto.Schema",
        "has_many",
        "belongs_to",
    ];

    // Connection string env vars indicating shared database
    let db_env_patterns = [
        "DATABASE_URL",
        "DB_HOST",
        "DB_NAME",
        "MONGO_URI",
        "REDIS_URL",
        "POSTGRES_",
        "MYSQL_",
    ];

    // Map: table/model name → set of seams that reference it
    let mut table_seam_map: HashMap<String, HashSet<String>> = HashMap::new();
    // Map: db env var → set of seams that reference it
    let mut db_env_seam_map: HashMap<String, HashSet<String>> = HashMap::new();

    for entry in WalkDir::new(repo_path)
        .follow_links(false)
        .into_iter()
        .filter_entry(|e| !is_ignored(e.file_name().to_str().unwrap_or("")))
        .filter_map(|e| e.ok())
    {
        let entry_path = entry.path();

        if !is_source_file(entry_path) {
            continue;
        }

        if let Some(seam_name) = file_seam_map.get(entry_path) {
            if let Ok(content) = std::fs::read_to_string(entry_path) {
                // Extract SQL table names
                for caps in sql_table_re.captures_iter(&content) {
                    if let Some(table_name) = caps.get(1) {
                        let name = table_name.as_str().to_lowercase();
                        // Skip common SQL keywords that might match
                        if !matches!(name.as_str(), "set" | "where" | "values" | "select" | "as" | "on") {
                            table_seam_map
                                .entry(name)
                                .or_default()
                                .insert(seam_name.clone());
                        }
                    }
                }

                // Check for ORM patterns
                for pattern in &orm_patterns {
                    for line in content.lines() {
                        if line.contains(pattern) {
                            let model_name = extract_var_name(line);
                            if !model_name.is_empty() {
                                table_seam_map
                                    .entry(model_name)
                                    .or_default()
                                    .insert(seam_name.clone());
                            }
                        }
                    }
                }

                // Check for shared database connection env vars
                for pattern in &db_env_patterns {
                    if content.contains(pattern) {
                        db_env_seam_map
                            .entry(pattern.to_string())
                            .or_default()
                            .insert(seam_name.clone());
                    }
                }
            }
        }
    }

    // Also check migration directories for shared ownership
    let migration_dirs = ["migrations", "db/migrate", "alembic", "priv/repo/migrations"];
    for dir_name in &migration_dirs {
        let migration_path = repo_path.join(dir_name);
        if migration_path.exists() {
            // Check which seams have migration files
            for entry in WalkDir::new(&migration_path)
                .max_depth(2)
                .into_iter()
                .filter_map(|e| e.ok())
            {
                if entry.path().is_file() {
                    if let Ok(content) = std::fs::read_to_string(entry.path()) {
                        for caps in sql_table_re.captures_iter(&content) {
                            if let Some(table_name) = caps.get(1) {
                                let name = table_name.as_str().to_lowercase();
                                if !matches!(name.as_str(), "set" | "where" | "values" | "select" | "as" | "on") {
                                    table_seam_map
                                        .entry(name)
                                        .or_default()
                                        .insert(format!("migration:{}", dir_name));
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Flag tables accessed by multiple seams
    for (table_name, seams) in &table_seam_map {
        if seams.len() > 1 {
            let seams_vec: Vec<_> = seams.iter().cloned().collect();
            for i in 0..seams_vec.len() {
                for j in (i + 1)..seams_vec.len() {
                    channels.push(HiddenChannel::new(
                        ChannelType::DatabaseCoupling,
                        &seams_vec[i],
                        &seams_vec[j],
                        format!("Shared database table: {}", table_name),
                        Severity::High,
                    ));
                }
            }
        }
    }

    // Flag shared database env vars across seams
    for (env_var, seams) in &db_env_seam_map {
        if seams.len() > 1 {
            let seams_vec: Vec<_> = seams.iter().cloned().collect();
            for i in 0..seams_vec.len() {
                for j in (i + 1)..seams_vec.len() {
                    channels.push(HiddenChannel::new(
                        ChannelType::DatabaseCoupling,
                        &seams_vec[i],
                        &seams_vec[j],
                        format!("Shared database connection via {}", env_var),
                        Severity::Medium,
                    ));
                }
            }
        }
    }

    Ok(channels)
}

/// Detect network coupling between seams
///
/// Scans source files for HTTP client calls, gRPC references, WebSocket
/// connections, and service endpoint patterns. When multiple seams communicate
/// over the network without declared interfaces, this indicates hidden coupling.
fn detect_network_coupling(
    register: &SeamRegister,
    repo_path: &Path,
) -> Result<Vec<HiddenChannel>> {
    let mut channels = Vec::new();
    let file_seam_map = build_file_seam_map(register, repo_path);

    // HTTP client patterns by language
    let http_patterns = [
        // Rust
        "reqwest::", "hyper::", "surf::", "ureq::", "isahc::",
        "http::Request", "http::Client",
        // JavaScript/TypeScript
        "fetch(", "axios.", "http.request", "https.request",
        "XMLHttpRequest",
        // Python
        "requests.", "urllib.", "aiohttp.", "httpx.",
    ];

    // gRPC / protocol buffer patterns
    let grpc_patterns = [
        "tonic::", "grpc.", "grpcio.", "proto::",
        ".proto", "protobuf::", "prost::",
    ];

    // WebSocket patterns
    let ws_patterns = [
        "WebSocket", "ws://", "wss://",
        "tokio_tungstenite", "async_tungstenite",
        "websocket::", "socket.io",
    ];

    // URL/endpoint patterns
    let url_re = regex::Regex::new(
        r#"(?:https?://|localhost:)\S+"#
    )?;

    // Service endpoint env var patterns
    let endpoint_env_patterns = [
        "_URL", "_ENDPOINT", "_HOST", "_PORT",
        "_API_BASE", "_SERVICE_ADDR", "_GRPC_ADDR",
    ];

    // Map: endpoint/URL → set of seams that reference it
    let mut endpoint_seam_map: HashMap<String, HashSet<String>> = HashMap::new();
    // Map: protocol type → set of seams using it
    let mut protocol_seam_map: HashMap<String, HashSet<String>> = HashMap::new();

    for entry in WalkDir::new(repo_path)
        .follow_links(false)
        .into_iter()
        .filter_entry(|e| !is_ignored(e.file_name().to_str().unwrap_or("")))
        .filter_map(|e| e.ok())
    {
        let entry_path = entry.path();

        if !is_source_file(entry_path) {
            continue;
        }

        if let Some(seam_name) = file_seam_map.get(entry_path) {
            if let Ok(content) = std::fs::read_to_string(entry_path) {
                // Check for HTTP client usage
                for pattern in &http_patterns {
                    if content.contains(pattern) {
                        protocol_seam_map
                            .entry("http".to_string())
                            .or_default()
                            .insert(seam_name.clone());
                    }
                }

                // Check for gRPC usage
                for pattern in &grpc_patterns {
                    if content.contains(pattern) {
                        protocol_seam_map
                            .entry("grpc".to_string())
                            .or_default()
                            .insert(seam_name.clone());
                    }
                }

                // Check for WebSocket usage
                for pattern in &ws_patterns {
                    if content.contains(pattern) {
                        protocol_seam_map
                            .entry("websocket".to_string())
                            .or_default()
                            .insert(seam_name.clone());
                    }
                }

                // Extract URL literals
                for caps in url_re.find_iter(&content) {
                    let url = caps.as_str();
                    // Normalize: strip trailing punctuation and quotes
                    let url_clean = url.trim_end_matches(|c: char| {
                        c == '"' || c == '\'' || c == ')' || c == ';' || c == ',' || c == '>'
                    });
                    endpoint_seam_map
                        .entry(url_clean.to_string())
                        .or_default()
                        .insert(seam_name.clone());
                }

                // Check for service endpoint env vars
                for line in content.lines() {
                    for pattern in &endpoint_env_patterns {
                        if line.contains(pattern) {
                            // Extract the env var name
                            let trimmed = line.trim();
                            let env_key = trimmed
                                .split(|c: char| !c.is_alphanumeric() && c != '_')
                                .find(|word| word.ends_with(pattern))
                                .unwrap_or(pattern)
                                .to_string();
                            if !env_key.is_empty() {
                                endpoint_seam_map
                                    .entry(env_key)
                                    .or_default()
                                    .insert(seam_name.clone());
                            }
                        }
                    }
                }
            }
        }
    }

    // Flag endpoints accessed by multiple seams
    for (endpoint, seams) in &endpoint_seam_map {
        if seams.len() > 1 {
            let seams_vec: Vec<_> = seams.iter().cloned().collect();
            for i in 0..seams_vec.len() {
                for j in (i + 1)..seams_vec.len() {
                    channels.push(HiddenChannel::new(
                        ChannelType::NetworkCoupling,
                        &seams_vec[i],
                        &seams_vec[j],
                        format!("Shared network endpoint: {}", endpoint),
                        Severity::Medium,
                    ));
                }
            }
        }
    }

    // Flag cross-seam network protocol usage (seams communicating over network)
    for (protocol, seams) in &protocol_seam_map {
        if seams.len() > 1 {
            let seams_vec: Vec<_> = seams.iter().cloned().collect();
            for i in 0..seams_vec.len() {
                for j in (i + 1)..seams_vec.len() {
                    // Only flag if the seams are NOT declared as having a network dependency
                    if !is_declared_dependency(register, &seams_vec[i], &seams_vec[j])
                        && !is_declared_dependency(register, &seams_vec[j], &seams_vec[i])
                    {
                        channels.push(HiddenChannel::new(
                            ChannelType::NetworkCoupling,
                            &seams_vec[i],
                            &seams_vec[j],
                            format!("Undeclared {} communication between seams", protocol),
                            Severity::High,
                        ));
                    }
                }
            }
        }
    }

    Ok(channels)
}

// Helper functions

fn build_file_seam_map(register: &SeamRegister, repo_path: &Path) -> HashMap<PathBuf, String> {
    let mut map = HashMap::new();

    for seam in &register.seams {
        let seam_path = repo_path.join(&seam.boundary_path);
        for entry in WalkDir::new(&seam_path)
            .follow_links(false)
            .into_iter()
            .filter_map(|e| e.ok())
        {
            if entry.path().is_file() {
                map.insert(entry.path().to_path_buf(), seam.name.clone());
            }
        }
    }

    map
}

fn is_ignored(name: &str) -> bool {
    matches!(
        name,
        "target" | "node_modules" | ".git" | "dist" | "build" | ".cache"
    )
}

fn is_source_file(path: &Path) -> bool {
    if let Some(ext) = path.extension() {
        matches!(
            ext.to_str().unwrap_or(""),
            "rs" | "py" | "js" | "ts" | "go" | "java" | "kt" | "swift"
        )
    } else {
        false
    }
}

fn extract_imports(path: &Path) -> Result<Vec<String>> {
    let content = std::fs::read_to_string(path)?;
    let mut imports = Vec::new();

    // Rust imports
    if path.extension().and_then(|e| e.to_str()) == Some("rs") {
        for line in content.lines() {
            let trimmed = line.trim();
            if trimmed.starts_with("use ") || trimmed.starts_with("mod ") {
                if let Some(import) = extract_rust_import(trimmed) {
                    imports.push(import);
                }
            }
        }
    }

    // Python imports
    if path.extension().and_then(|e| e.to_str()) == Some("py") {
        for line in content.lines() {
            let trimmed = line.trim();
            if trimmed.starts_with("import ") || trimmed.starts_with("from ") {
                if let Some(import) = extract_python_import(trimmed) {
                    imports.push(import);
                }
            }
        }
    }

    // JavaScript/TypeScript imports
    if matches!(
        path.extension().and_then(|e| e.to_str()),
        Some("js") | Some("ts") | Some("jsx") | Some("tsx")
    ) {
        for line in content.lines() {
            let trimmed = line.trim();
            if trimmed.starts_with("import ") || trimmed.contains("require(") {
                if let Some(import) = extract_js_import(trimmed) {
                    imports.push(import);
                }
            }
        }
    }

    Ok(imports)
}

fn extract_rust_import(line: &str) -> Option<String> {
    // Simplified: extract module path from "use path::to::module"
    line.split_whitespace()
        .nth(1)?
        .trim_end_matches(';')
        .split("::")
        .next()
        .map(|s| s.to_string())
}

fn extract_python_import(line: &str) -> Option<String> {
    // Simplified: extract module from "import module" or "from module import"
    if line.starts_with("import ") {
        line.split_whitespace().nth(1)?.split('.').next().map(|s| s.to_string())
    } else if line.starts_with("from ") {
        line.split_whitespace().nth(1)?.split('.').next().map(|s| s.to_string())
    } else {
        None
    }
}

fn extract_js_import(line: &str) -> Option<String> {
    // Simplified: extract path from "import ... from 'path'" or "require('path')"
    if line.contains("from") {
        line.split("from").nth(1)?
            .trim()
            .trim_matches(|c| c == '\'' || c == '"' || c == ';')
            .to_string()
            .into()
    } else if line.contains("require(") {
        line.split("require(").nth(1)?
            .split(')').next()?
            .trim()
            .trim_matches(|c| c == '\'' || c == '"')
            .to_string()
            .into()
    } else {
        None
    }
}

fn resolve_import(import: &str, source_file: &Path, repo_path: &Path) -> Option<PathBuf> {
    // Simplified: try common patterns
    let source_dir = source_file.parent()?;

    // Relative import
    if import.starts_with('.') {
        let resolved = source_dir.join(import);
        if resolved.exists() {
            return Some(resolved);
        }
    }

    // Absolute from repo root
    let from_root = repo_path.join(import);
    if from_root.exists() {
        return Some(from_root);
    }

    None
}

fn is_declared_dependency(register: &SeamRegister, source: &str, target: &str) -> bool {
    // Check if target is in source's declared dependencies
    if let Some(seam) = register.seams.iter().find(|s| s.name == source) {
        seam.declared_dependencies.contains(&target.to_string())
    } else {
        false
    }
}

fn extract_var_name(line: &str) -> String {
    // Simplified: extract variable name from declaration
    line.split_whitespace()
        .last()
        .unwrap_or("unknown")
        .trim_matches(|c: char| !c.is_alphanumeric() && c != '_')
        .to_string()
}

fn extract_path_literal(line: &str) -> Option<String> {
    // Simplified: extract string literals that look like paths
    for word in line.split(['"', '\'']) {
        if word.contains('/') || word.contains('\\') {
            return Some(word.to_string());
        }
    }
    None
}
