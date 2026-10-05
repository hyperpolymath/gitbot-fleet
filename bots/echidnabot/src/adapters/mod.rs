// SPDX-License-Identifier: MPL-2.0
// Owner: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! Platform adapters for GitHub, GitLab, Bitbucket, Codeberg/Forgejo

use serde::{Deserialize, Serialize};

pub mod bitbucket;
pub mod codeberg;
pub mod github;
pub mod gitlab;

use async_trait::async_trait;

use crate::error::Result;

/// Unique identifier for a repository
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RepoId {
    pub platform: Platform,
    pub owner: String,
    pub name: String,
}

impl RepoId {
    pub fn new(platform: Platform, owner: impl Into<String>, name: impl Into<String>) -> Self {
        Self {
            platform,
            owner: owner.into(),
            name: name.into(),
        }
    }

    pub fn full_name(&self) -> String {
        format!("{}/{}", self.owner, self.name)
    }
}

/// Platform enum
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Platform {
    GitHub,
    GitLab,
    Bitbucket,
    Codeberg,
}

/// Check run identifier
#[derive(Debug, Clone)]
pub struct CheckRunId(pub String);

/// Comment identifier
#[derive(Debug, Clone)]
pub struct CommentId(pub String);

/// Issue identifier
#[derive(Debug, Clone)]
pub struct IssueId(pub String);

/// Pull request identifier
#[derive(Debug, Clone)]
pub struct PrId(pub String);

/// Check run status
#[derive(Debug, Clone)]
pub enum CheckStatus {
    Queued,
    InProgress,
    Completed {
        conclusion: CheckConclusion,
        summary: String,
    },
}

/// Check run conclusion
#[derive(Debug, Clone)]
pub enum CheckConclusion {
    Success,
    Failure,
    Neutral,
    Cancelled,
    Skipped,
    TimedOut,
    ActionRequired,
}

/// Check run to create
#[derive(Debug, Clone)]
pub struct CheckRun {
    pub name: String,
    pub head_sha: String,
    pub status: CheckStatus,
    pub details_url: Option<String>,
}

/// Issue to create
#[derive(Debug, Clone)]
pub struct NewIssue {
    pub title: String,
    pub body: String,
    pub labels: Vec<String>,
}

/// Location anchor for an inline PR review comment.
///
/// Used by Consultant mode to attach failure notes directly to the
/// offending line in the diff rather than posting a general PR comment.
/// When the file or line is unknown, `line` defaults to 1.
#[derive(Debug, Clone)]
pub struct ReviewCommentLocation {
    /// The commit SHA to anchor the comment to (must be in the PR's history).
    pub commit_sha: String,
    /// Path of the file to comment on, relative to the repo root.
    pub path: String,
    /// Line number (1-based) on the RIGHT side of the diff. Defaults to 1
    /// when the prover output does not contain a parseable location.
    pub line: u32,
}

/// Build the right `PlatformAdapter` for a given platform.
///
/// Single source of truth for adapter construction — used by both
/// `main.rs::report_to_platform` (Phase 3) and
/// `api/webhooks.rs::handle_consultant_mention` (Phase 6).
///
/// Falls back to a tokenless GitHub client when no token is configured —
/// downstream call sites tolerate auth-failure as a warning, not a panic.
/// Codeberg uses the Forgejo/Gitea-compatible adapter (scaffold, issue #62).
pub fn build_adapter(
    config: &crate::Config,
    platform: Platform,
) -> crate::error::Result<Box<dyn PlatformAdapter>> {
    use crate::adapters::{
        bitbucket::BitbucketAdapter, codeberg::CodebergAdapter, github::GitHubAdapter,
        gitlab::GitLabAdapter,
    };
    match platform {
        Platform::GitHub => {
            let token = config
                .github
                .as_ref()
                .and_then(|g| g.token.clone())
                .unwrap_or_default();
            Ok(Box::new(GitHubAdapter::new(&token)?))
        }
        Platform::GitLab => Ok(Box::new(GitLabAdapter::new(
            config.gitlab.as_ref().map(|g| g.url.as_str()),
        ))),
        Platform::Bitbucket => Ok(Box::new(BitbucketAdapter::new(None))),
        Platform::Codeberg => Ok(Box::new(CodebergAdapter::new(
            config.codeberg.as_ref().map(|c| c.url.as_str()),
        ))),
    }
}

/// Platform adapter trait
///
/// Abstracts operations across GitHub, GitLab, Bitbucket
#[async_trait]
pub trait PlatformAdapter: Send + Sync {
    /// Clone a repository to a local path
    async fn clone_repo(&self, repo: &RepoId, commit: &str) -> Result<tempfile::TempDir>;

    /// Create a check run (GitHub) or pipeline status (GitLab)
    async fn create_check_run(&self, repo: &RepoId, check: CheckRun) -> Result<CheckRunId>;

    /// Update a check run status
    async fn update_check_run(&self, id: CheckRunId, status: CheckStatus) -> Result<()>;

    /// Create a comment on a PR/MR
    async fn create_comment(&self, repo: &RepoId, pr: PrId, body: &str) -> Result<CommentId>;

    /// Create an issue
    async fn create_issue(&self, repo: &RepoId, issue: NewIssue) -> Result<IssueId>;

    /// Get the default branch name
    async fn get_default_branch(&self, repo: &RepoId) -> Result<String>;

    /// Fetch a single file's contents from the target repo via platform API.
    ///
    /// Returns `Ok(None)` when the file does not exist (not an error —
    /// callers like the directive resolver use absence as a signal to
    /// fall through the cascade). `Err` is reserved for actual API
    /// failures (auth, rate limit, network).
    ///
    /// `branch` may be `None` to use the default branch.
    async fn get_file_contents(
        &self,
        repo: &RepoId,
        branch: Option<&str>,
        path: &str,
    ) -> Result<Option<String>>;

    /// Post an inline review comment on a specific line in the PR diff.
    ///
    /// Used by Consultant mode to anchor failure notes to the offending
    /// proof line. Returns `Err` when the file/line is not in the diff or
    /// when the platform does not support inline review comments — callers
    /// should fall back to `create_comment` on error.
    async fn create_review_comment(
        &self,
        repo: &RepoId,
        pr: PrId,
        body: &str,
        location: ReviewCommentLocation,
    ) -> Result<CommentId>;
}

/// Clone and check out the requested revision. The returned owner removes the
/// checkout on drop, including cancellation and errors during proof processing.
/// All Git operations share a two-minute deadline and run without terminal
/// prompts. Dropping an in-flight operation also terminates its Git child.
pub async fn clone_revision(url: &str, commit: &str) -> Result<tempfile::TempDir> {
    let deadline = tokio::time::Instant::now() + std::time::Duration::from_secs(120);
    let temp_dir = tempfile::tempdir()?;
    let clone = git_status(
        tokio::process::Command::new("git")
            .args(["clone", "--no-checkout", "--depth", "1", "--", url])
            .arg(temp_dir.path()),
        "clone",
        deadline,
    )
    .await?;
    if !clone.success() {
        return Err(crate::Error::Internal(format!(
            "Failed to clone {}",
            "requested repository"
        )));
    }
    let fetch = git_status(
        tokio::process::Command::new("git")
            .current_dir(temp_dir.path())
            .args(["fetch", "--depth", "1", "--", "origin", commit]),
        "fetch",
        deadline,
    )
    .await?;
    if !fetch.success() {
        return Err(crate::Error::Internal(format!(
            "Failed to fetch requested revision for {}",
            "requested repository"
        )));
    }
    let checkout = git_status(
        tokio::process::Command::new("git")
            .current_dir(temp_dir.path())
            .args(["checkout", "--detach", "FETCH_HEAD"]),
        "checkout",
        deadline,
    )
    .await?;
    if !checkout.success() {
        return Err(crate::Error::Internal(format!(
            "Failed to check out requested revision for {}",
            "requested repository"
        )));
    }
    Ok(temp_dir)
}

/// Wait for a Git child within the checkout's shared deadline. A timeout kills
/// and reaps the direct child before the checkout owner can clean up its files.
async fn git_status(
    command: &mut tokio::process::Command,
    operation: &str,
    deadline: tokio::time::Instant,
) -> Result<std::process::ExitStatus> {
    let mut child = command
        .env("GIT_TERMINAL_PROMPT", "0")
        .stdin(std::process::Stdio::null())
        .kill_on_drop(true)
        .spawn()?;
    match tokio::time::timeout_at(deadline, child.wait()).await {
        Ok(status) => Ok(status?),
        Err(_) => {
            child.kill().await?;
            Err(crate::Error::Internal(format!("Git {operation} timed out")))
        }
    }
}

#[cfg(all(test, target_os = "linux"))]
mod git_process_tests {
    use super::*;
    use std::path::Path;
    use std::time::Duration;
    use tokio::time::{sleep, Instant};

    async fn assert_child_reaped(pid_file: &Path) {
        let pid: u32 = std::fs::read_to_string(pid_file)
            .unwrap()
            .trim()
            .parse()
            .unwrap();
        let proc_path = std::path::PathBuf::from(format!("/proc/{pid}"));
        let deadline = Instant::now() + Duration::from_secs(2);
        while proc_path.exists() && Instant::now() < deadline {
            sleep(Duration::from_millis(10)).await;
        }
        assert!(
            !proc_path.exists(),
            "Git child {pid} survived its operation"
        );
    }

    #[tokio::test]
    async fn git_deadline_kills_and_reaps_the_child_without_prompts() {
        let dir = tempfile::tempdir().unwrap();
        let pid = dir.path().join("pid");
        let prompt = dir.path().join("prompt");
        let result = git_status(
            tokio::process::Command::new("/bin/sh")
                .args([
                    "-c",
                    "echo \"$GIT_TERMINAL_PROMPT\" > \"$1\"; echo $$ > \"$2\"; exec sleep 30",
                    "contract",
                ])
                .arg(&prompt)
                .arg(&pid),
            "contract",
            Instant::now() + Duration::from_millis(200),
        )
        .await;
        assert!(
            matches!(result, Err(crate::Error::Internal(message)) if message.contains("timed out"))
        );
        assert_eq!(std::fs::read_to_string(prompt).unwrap().trim(), "0");
        assert_child_reaped(&pid).await;
    }

    #[tokio::test]
    async fn cancelling_checkout_terminates_its_child() {
        let dir = tempfile::tempdir().unwrap();
        let pid = dir.path().join("pid");
        let child_pid = pid.clone();
        let task = tokio::spawn(async move {
            git_status(
                tokio::process::Command::new("/bin/sh")
                    .args(["-c", "echo $$ > \"$1\"; exec sleep 30", "contract"])
                    .arg(child_pid),
                "contract",
                Instant::now() + Duration::from_secs(10),
            )
            .await
        });
        let deadline = Instant::now() + Duration::from_secs(2);
        while !pid.exists() && Instant::now() < deadline {
            sleep(Duration::from_millis(10)).await;
        }
        task.abort();
        assert!(task.await.unwrap_err().is_cancelled());
        assert_child_reaped(&pid).await;
    }
}
