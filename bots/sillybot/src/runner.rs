// SPDX-License-Identifier: MPL-2.0
//! Run docs-ingest and reconcile on a git repository, never writing into it.
//!
//! Each revision is exported with `git archive` into a private temporary
//! directory, ingested there, and the bundles are handed to reconcile. The
//! scanned tree is only read by `git`, with hooks and fsmonitor switched off.
//! Every failure maps to `Error` or `NotRun`; nothing here turns a failed run
//! into an empty findings list.

use std::ffi::OsString;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

use serde_json::Value;

/// What happened.
#[derive(Debug)]
pub enum Outcome {
    /// Both tools ran. The value is reconcile's findings/v0 document.
    Ran(Value),
    /// The scan failed. The reason is for the report.
    Error(String),
    /// The scan was not run (a tool is missing, or no prose changed).
    NotRun(String),
}

/// Inputs for one scan.
#[derive(Debug, Clone)]
pub struct Request {
    pub path: PathBuf,
    pub base: Option<String>,
    pub repo: Option<String>,
    pub pr: u64,
    pub timeout: Duration,
}

/// The prose formats docs-ingest parses (docudactyl `formatOf`).
pub fn is_prose(path: &str) -> bool {
    let lower = path.to_ascii_lowercase();
    [".adoc", ".asciidoc", ".asc", ".md", ".markdown"]
        .iter()
        .any(|e| lower.ends_with(e))
}

/// OWNER/NAME with GitHub's name characters only.
pub fn valid_repo(s: &str) -> bool {
    let mut parts = s.split('/');
    let ok = |p: Option<&str>| {
        p.is_some_and(|p| {
            !p.is_empty()
                && p != "."
                && p != ".."
                && p.bytes()
                    .all(|b| b.is_ascii_alphanumeric() || matches!(b, b'-' | b'_' | b'.'))
        })
    };
    ok(parts.next()) && ok(parts.next()) && parts.next().is_none()
}

/// OWNER/NAME from a remote URL: the last two path segments, `.git` dropped.
pub fn repo_from_url(url: &str) -> Option<String> {
    let url = url.trim().trim_end_matches('/');
    let url = url.strip_suffix(".git").unwrap_or(url);
    let tail = url.rsplit(['/', ':']).take(2).collect::<Vec<_>>();
    if tail.len() != 2 {
        return None;
    }
    let r = format!("{}/{}", tail[1], tail[0]);
    valid_repo(&r).then_some(r)
}

/// A tool from its environment variable, else from PATH.
fn find_tool(env: &str, name: &str) -> Option<PathBuf> {
    if let Some(p) = std::env::var_os(env).filter(|p| !p.is_empty()) {
        let p = PathBuf::from(p);
        return p.is_file().then_some(p);
    }
    std::env::split_paths(&std::env::var_os("PATH")?)
        .map(|d| d.join(name))
        .find(|p| p.is_file())
}

struct Clock {
    deadline: Instant,
}

impl Clock {
    fn left(&self) -> Result<Duration, String> {
        self.deadline
            .checked_duration_since(Instant::now())
            .filter(|d| !d.is_zero())
            .ok_or_else(|| "timed out".to_string())
    }
}

/// Run a command with stdout and stderr in files under `tmp`, killing it at
/// the deadline. Returns stdout on exit 0, else an error with stderr's tail.
fn run(mut cmd: Command, tmp: &Path, tag: &str, clock: &Clock) -> Result<Vec<u8>, String> {
    let out_p = tmp.join(format!("{tag}.out"));
    let err_p = tmp.join(format!("{tag}.err"));
    let out = File::create(&out_p).map_err(|e| format!("{tag}: {e}"))?;
    let err = File::create(&err_p).map_err(|e| format!("{tag}: {e}"))?;
    cmd.stdin(Stdio::null())
        .stdout(Stdio::from(out))
        .stderr(Stdio::from(err));
    let mut child = cmd
        .spawn()
        .map_err(|e| format!("{tag}: cannot start: {e}"))?;
    let status = loop {
        match child.try_wait() {
            Ok(Some(s)) => break s,
            Ok(None) => {}
            Err(e) => return Err(format!("{tag}: {e}")),
        }
        if clock.left().is_err() {
            let _ = child.kill();
            let _ = child.wait();
            return Err(format!("{tag}: timed out"));
        }
        std::thread::sleep(Duration::from_millis(20));
    };
    if status.success() {
        std::fs::read(&out_p).map_err(|e| format!("{tag}: {e}"))
    } else {
        let stderr = std::fs::read_to_string(&err_p).unwrap_or_default();
        let tail: String = stderr
            .lines()
            .rev()
            .take(3)
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .collect::<Vec<_>>()
            .join(" | ");
        Err(format!("{tag}: exited with {status}: {tail}"))
    }
}

/// `git` with repository hooks and fsmonitor switched off.
fn git(repo: &Path) -> Command {
    let mut c = Command::new("git");
    c.arg("-C")
        .arg(repo)
        .args([
            "-c",
            "core.fsmonitor=false",
            "-c",
            "core.hooksPath=/dev/null",
        ])
        .env("GIT_TERMINAL_PROMPT", "0");
    c
}

fn git_text(repo: &Path, args: &[&str], tmp: &Path, clock: &Clock) -> Result<String, String> {
    let mut c = git(repo);
    c.args(args);
    let out = run(c, tmp, "git", clock)?;
    String::from_utf8(out).map_err(|_| "git: output is not UTF-8".to_string())
}

/// Resolve a revision to its full commit id.
fn resolve(repo: &Path, rev: &str, tmp: &Path, clock: &Clock) -> Result<String, String> {
    if rev.is_empty() || rev.starts_with('-') || rev.chars().any(char::is_control) {
        return Err(format!("bad revision {rev:?}"));
    }
    let spec = format!("{rev}^{{commit}}");
    let sha = git_text(
        repo,
        &[
            "rev-parse",
            "--verify",
            "--quiet",
            "--end-of-options",
            &spec,
        ],
        tmp,
        clock,
    )
    .map_err(|_| format!("revision {rev:?} is not a commit in this repository"))?;
    let sha = sha.trim().to_string();
    if crate::pin::is_full_commit(&sha) {
        Ok(sha)
    } else {
        Err(format!(
            "revision {rev:?} did not resolve to a SHA-1 commit id"
        ))
    }
}

/// NUL-separated names from git, refusing names docs-ingest's newline lists
/// cannot carry.
fn nul_names(raw: &str) -> Result<Vec<String>, String> {
    let names: Vec<String> = raw
        .split('\0')
        .filter(|n| !n.is_empty())
        .map(str::to_string)
        .collect();
    if names.iter().any(|n| n.contains('\n') || n.contains('\r')) {
        return Err("a file name contains a line break; docs-ingest cannot list it".into());
    }
    Ok(names)
}

/// Export `sha` to `tmp/<tag>/`, write its file list, and run docs-ingest.
/// Returns the bundle's path.
fn ingest(
    req_path: &Path,
    ingest_bin: &Path,
    repo: &str,
    sha: &str,
    tag: &str,
    tmp: &Path,
    clock: &Clock,
) -> Result<PathBuf, String> {
    let tree = tmp.join(tag);
    std::fs::create_dir(&tree).map_err(|e| format!("{tag}: {e}"))?;
    let tar = tmp.join(format!("{tag}.tar"));
    let mut c = git(req_path);
    c.args(["archive", "--format=tar", "-o"]).arg(&tar).arg(sha);
    run(c, tmp, &format!("{tag}-archive"), clock)?;
    let mut c = Command::new("tar");
    c.arg("-xf").arg(&tar).arg("-C").arg(&tree);
    run(c, tmp, &format!("{tag}-untar"), clock)?;

    let names = nul_names(&git_text(
        req_path,
        &["ls-tree", "-r", "-z", "--name-only", sha],
        tmp,
        clock,
    )?)?;
    let list = tmp.join(format!("{tag}.files"));
    std::fs::write(&list, names.join("\n") + "\n").map_err(|e| format!("{tag}: {e}"))?;

    let mut c = Command::new(ingest_bin);
    c.args(["--repo", repo, "--rev", sha, "--root"])
        .arg(&tree)
        .arg("--files")
        .arg(&list);
    let bundle = run(c, tmp, &format!("{tag}-ingest"), clock)?;
    let out = tmp.join(format!("{tag}.json"));
    std::fs::write(&out, bundle).map_err(|e| format!("{tag}: {e}"))?;
    Ok(out)
}

/// Run one scan.
pub fn scan(req: &Request) -> Outcome {
    let clock = Clock {
        deadline: Instant::now() + req.timeout,
    };
    let Some(ingest_bin) = find_tool("SILLYBOT_DOCS_INGEST", "docs-ingest") else {
        return Outcome::NotRun("docs-ingest is not installed (set SILLYBOT_DOCS_INGEST)".into());
    };
    let Some(reconcile_bin) = find_tool("SILLYBOT_RECONCILE", "reconcile") else {
        return Outcome::NotRun("reconcile is not installed (set SILLYBOT_RECONCILE)".into());
    };
    match scan_with(req, &ingest_bin, &reconcile_bin, &clock) {
        Ok(o) => o,
        Err(e) => Outcome::Error(e),
    }
}

fn scan_with(
    req: &Request,
    ingest_bin: &Path,
    reconcile_bin: &Path,
    clock: &Clock,
) -> Result<Outcome, String> {
    let tmpdir = tempfile::Builder::new()
        .prefix("sillybot-")
        .tempdir()
        .map_err(|e| format!("cannot make a temporary directory: {e}"))?;
    let tmp = tmpdir.path();

    let repo = match &req.repo {
        Some(r) if valid_repo(r) => r.clone(),
        Some(r) => return Err(format!("--repo {r:?} is not OWNER/NAME")),
        None => git_text(
            &req.path,
            &["config", "--get", "remote.origin.url"],
            tmp,
            clock,
        )
        .ok()
        .and_then(|u| repo_from_url(&u))
        .ok_or("no --repo given and the origin remote does not name OWNER/NAME")?,
    };

    let head = resolve(&req.path, "HEAD", tmp, clock)?;
    let base = match &req.base {
        Some(b) => Some(resolve(&req.path, b, tmp, clock)?),
        None => None,
    };

    if let Some(b) = &base {
        let changed = nul_names(&git_text(
            &req.path,
            &["diff", "--no-renames", "--name-only", "-z", b, &head],
            tmp,
            clock,
        )?)?;
        if !changed.iter().any(|n| is_prose(n)) {
            return Ok(Outcome::NotRun(format!(
                "no prose file changed between {} and {}",
                &b[..12],
                &head[..12]
            )));
        }
    }

    let head_json = ingest(&req.path, ingest_bin, &repo, &head, "head", tmp, clock)?;
    let base_json = match &base {
        Some(b) => Some(ingest(&req.path, ingest_bin, &repo, b, "base", tmp, clock)?),
        None => None,
    };

    let mut c = Command::new(reconcile_bin);
    c.arg("--head").arg(&head_json);
    if let Some(b) = &base_json {
        c.arg("--base").arg(b);
    }
    c.arg("--pr").arg(OsString::from(req.pr.to_string()));
    let out = run(c, tmp, "reconcile", clock)?;
    let doc: Value =
        serde_json::from_slice(&out).map_err(|e| format!("reconcile: output is not JSON: {e}"))?;
    Ok(Outcome::Ran(doc))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prose_matches_docs_ingest_formats() {
        for p in ["README.adoc", "a/b.md", "x.MARKDOWN", "y.asc", "z.asciidoc"] {
            assert!(is_prose(p), "{p}");
        }
        for p in ["src/main.rs", "README", "md", "a.mdx"] {
            assert!(!is_prose(p), "{p}");
        }
    }

    #[test]
    fn repo_names_from_remotes() {
        assert_eq!(
            repo_from_url("https://github.com/hyperpolymath/docudactyl.git").as_deref(),
            Some("hyperpolymath/docudactyl")
        );
        assert_eq!(
            repo_from_url("git@github.com:hyperpolymath/gitbot-fleet").as_deref(),
            Some("hyperpolymath/gitbot-fleet")
        );
        assert_eq!(repo_from_url("nothing"), None);
        assert!(!valid_repo("a/b/c"));
        assert!(!valid_repo("../b"));
        assert!(!valid_repo("a b/c"));
    }

    #[test]
    fn names_with_line_breaks_are_refused() {
        assert_eq!(nul_names("a.md\0b.md\0").unwrap(), vec!["a.md", "b.md"]);
        assert!(nul_names("a\nb.md\0").is_err());
    }
}
