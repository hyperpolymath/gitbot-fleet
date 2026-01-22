// SPDX-License-Identifier: PMPL-1.0
//! Context storage backends

use crate::context::Context;
use crate::state::{RepoState, SessionState};
use crate::{ContextError, Result};
use std::path::{Path, PathBuf};
use tracing::{debug, info};

/// Storage backend for contexts and state
pub struct ContextStorage {
    /// Base directory for storage
    base_path: PathBuf,
}

impl ContextStorage {
    /// Create storage with base path
    pub fn new(base_path: impl Into<PathBuf>) -> Self {
        Self {
            base_path: base_path.into(),
        }
    }

    /// Create storage in default location (~/.gitbot-fleet)
    pub fn default() -> Result<Self> {
        let home = std::env::var("HOME").map_err(|_| {
            ContextError::InvalidState("HOME environment variable not set".to_string())
        })?;
        let base = PathBuf::from(home).join(".gitbot-fleet");
        std::fs::create_dir_all(&base)?;
        Ok(Self::new(base))
    }

    /// Get sessions directory
    fn sessions_dir(&self) -> PathBuf {
        self.base_path.join("sessions")
    }

    /// Get repos directory
    fn repos_dir(&self) -> PathBuf {
        self.base_path.join("repos")
    }

    /// Save a context
    pub fn save_context(&self, ctx: &Context) -> Result<PathBuf> {
        let sessions_dir = self.sessions_dir();
        std::fs::create_dir_all(&sessions_dir)?;

        let filename = format!("{}.json", ctx.session_id);
        let path = sessions_dir.join(&filename);

        let json = serde_json::to_string_pretty(ctx)?;
        std::fs::write(&path, json)?;

        debug!(path = %path.display(), "Saved context");
        Ok(path)
    }

    /// Load a context by session ID
    pub fn load_context(&self, session_id: &uuid::Uuid) -> Result<Context> {
        let filename = format!("{}.json", session_id);
        let path = self.sessions_dir().join(&filename);

        if !path.exists() {
            return Err(ContextError::NotFound(format!(
                "Session {} not found",
                session_id
            )));
        }

        let json = std::fs::read_to_string(&path)?;
        let ctx: Context = serde_json::from_str(&json)?;

        debug!(path = %path.display(), "Loaded context");
        Ok(ctx)
    }

    /// List all session IDs
    pub fn list_sessions(&self) -> Result<Vec<uuid::Uuid>> {
        let sessions_dir = self.sessions_dir();
        if !sessions_dir.exists() {
            return Ok(Vec::new());
        }

        let mut sessions = Vec::new();
        for entry in std::fs::read_dir(&sessions_dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.extension().map(|e| e == "json").unwrap_or(false) {
                if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                    if let Ok(id) = uuid::Uuid::parse_str(stem) {
                        sessions.push(id);
                    }
                }
            }
        }

        Ok(sessions)
    }

    /// Save repository state
    pub fn save_repo_state(&self, state: &RepoState) -> Result<PathBuf> {
        let repos_dir = self.repos_dir();
        std::fs::create_dir_all(&repos_dir)?;

        // Use sanitized repo name as filename
        let safe_name = state.name.replace(['/', '\\', ':'], "_");
        let filename = format!("{}.json", safe_name);
        let path = repos_dir.join(&filename);

        let json = serde_json::to_string_pretty(state)?;
        std::fs::write(&path, json)?;

        debug!(path = %path.display(), repo = %state.name, "Saved repo state");
        Ok(path)
    }

    /// Load repository state
    pub fn load_repo_state(&self, repo_name: &str) -> Result<RepoState> {
        let safe_name = repo_name.replace(['/', '\\', ':'], "_");
        let filename = format!("{}.json", safe_name);
        let path = self.repos_dir().join(&filename);

        if !path.exists() {
            return Err(ContextError::NotFound(format!("Repo {} not found", repo_name)));
        }

        let json = std::fs::read_to_string(&path)?;
        let state: RepoState = serde_json::from_str(&json)?;

        debug!(path = %path.display(), "Loaded repo state");
        Ok(state)
    }

    /// Get or create repository state
    pub fn get_or_create_repo_state(&self, repo_name: &str, repo_path: PathBuf) -> Result<RepoState> {
        match self.load_repo_state(repo_name) {
            Ok(state) => Ok(state),
            Err(ContextError::NotFound(_)) => Ok(RepoState::new(repo_name, repo_path)),
            Err(e) => Err(e),
        }
    }

    /// List all tracked repositories
    pub fn list_repos(&self) -> Result<Vec<String>> {
        let repos_dir = self.repos_dir();
        if !repos_dir.exists() {
            return Ok(Vec::new());
        }

        let mut repos = Vec::new();
        for entry in std::fs::read_dir(&repos_dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.extension().map(|e| e == "json").unwrap_or(false) {
                if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                    repos.push(stem.to_string());
                }
            }
        }

        Ok(repos)
    }

    /// Clean up old sessions (keep last N)
    pub fn cleanup_sessions(&self, keep: usize) -> Result<usize> {
        let sessions_dir = self.sessions_dir();
        if !sessions_dir.exists() {
            return Ok(0);
        }

        // Get all session files with modification times
        let mut sessions: Vec<(PathBuf, std::time::SystemTime)> = Vec::new();
        for entry in std::fs::read_dir(&sessions_dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.extension().map(|e| e == "json").unwrap_or(false) {
                if let Ok(metadata) = entry.metadata() {
                    if let Ok(modified) = metadata.modified() {
                        sessions.push((path, modified));
                    }
                }
            }
        }

        // Sort by modification time (newest first)
        sessions.sort_by(|a, b| b.1.cmp(&a.1));

        // Delete sessions beyond keep limit
        let mut deleted = 0;
        for (path, _) in sessions.into_iter().skip(keep) {
            if std::fs::remove_file(&path).is_ok() {
                deleted += 1;
            }
        }

        if deleted > 0 {
            info!(deleted, keep, "Cleaned up old sessions");
        }

        Ok(deleted)
    }

    /// Export context to a specific path (for sharing)
    pub fn export_context(&self, ctx: &Context, path: &Path) -> Result<()> {
        let json = serde_json::to_string_pretty(ctx)?;
        std::fs::write(path, json)?;
        Ok(())
    }

    /// Import context from a specific path
    pub fn import_context(&self, path: &Path) -> Result<Context> {
        let json = std::fs::read_to_string(path)?;
        let ctx: Context = serde_json::from_str(&json)?;
        Ok(ctx)
    }
}

/// In-memory storage for testing
#[cfg(test)]
pub struct MemoryStorage {
    contexts: std::collections::HashMap<uuid::Uuid, Context>,
    repos: std::collections::HashMap<String, RepoState>,
}

#[cfg(test)]
impl MemoryStorage {
    pub fn new() -> Self {
        Self {
            contexts: std::collections::HashMap::new(),
            repos: std::collections::HashMap::new(),
        }
    }

    pub fn save_context(&mut self, ctx: Context) {
        self.contexts.insert(ctx.session_id, ctx);
    }

    pub fn load_context(&self, id: &uuid::Uuid) -> Option<&Context> {
        self.contexts.get(id)
    }

    pub fn save_repo_state(&mut self, state: RepoState) {
        self.repos.insert(state.name.clone(), state);
    }

    pub fn load_repo_state(&self, name: &str) -> Option<&RepoState> {
        self.repos.get(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_context_storage() {
        let temp = TempDir::new().unwrap();
        let storage = ContextStorage::new(temp.path());

        let ctx = Context::new("test-repo", "/tmp/test");
        let path = storage.save_context(&ctx).unwrap();
        assert!(path.exists());

        let loaded = storage.load_context(&ctx.session_id).unwrap();
        assert_eq!(loaded.repo_name, "test-repo");
    }

    #[test]
    fn test_repo_state_storage() {
        let temp = TempDir::new().unwrap();
        let storage = ContextStorage::new(temp.path());

        let state = RepoState::new("my-repo", PathBuf::from("/tmp/my-repo"));
        storage.save_repo_state(&state).unwrap();

        let loaded = storage.load_repo_state("my-repo").unwrap();
        assert_eq!(loaded.name, "my-repo");
    }
}
