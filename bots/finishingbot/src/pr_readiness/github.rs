// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
//! Read a [`Snapshot`] of one pull request from GitHub.
//!
//! REST for everything except review-thread resolution, which only GraphQL
//! exposes (`reviewThreads { isResolved }`, readable by a workflow's
//! `GITHUB_TOKEN`). Read-only: every request is a GET, or a GraphQL query.
//!
//! The token comes from `GH_TOKEN`, else `GITHUB_TOKEN`; without one the REST
//! reads are attempted unauthenticated (public repositories, low rate limit)
//! and the review threads are recorded as unreadable. A source that fails is
//! recorded in `Snapshot::unreadable` with the reason, never dropped: the
//! verdict can then be `not_ready` or `unknown`, never `ready`.

use super::{
    Branch, CheckRun, CombinedStatus, Compare, PullRequest, Review, ReviewThread, Rule, Snapshot,
};
use serde::de::DeserializeOwned;
use serde_json::{json, Value};
use std::time::Duration;

/// Pages followed per list endpoint, at 100 items a page.
const MAX_PAGES: usize = 20;

const THREADS_QUERY: &str = "query($owner:String!,$name:String!,$number:Int!,$after:String){\
repository(owner:$owner,name:$name){pullRequest(number:$number){\
reviewThreads(first:100,after:$after){pageInfo{hasNextPage endCursor}\
nodes{isResolved isOutdated path line comments(first:1){nodes{url}}}}}}}";

/// `owner/name`, each part letters, digits, `.`, `_` or `-`.
pub fn valid_repo_slug(s: &str) -> bool {
    let mut parts = s.split('/');
    let ok = |p: Option<&str>| {
        p.is_some_and(|p| {
            !p.is_empty()
                && p.len() <= 100
                && p != "."
                && p != ".."
                && p.chars()
                    .all(|c| c.is_ascii_alphanumeric() || matches!(c, '.' | '_' | '-'))
        })
    };
    ok(parts.next()) && ok(parts.next()) && parts.next().is_none()
}

/// Percent-encode a branch name for a URL path (slashes kept: the branch
/// endpoints take them literally).
fn encode_branch(b: &str) -> String {
    b.bytes()
        .map(|c| {
            if c.is_ascii_alphanumeric() || matches!(c, b'.' | b'_' | b'-' | b'/') {
                (c as char).to_string()
            } else {
                format!("%{c:02X}")
            }
        })
        .collect()
}

/// The `rel="next"` URL of a `Link` header.
fn next_link(link: &str) -> Option<String> {
    link.split(',').find_map(|part| {
        let (url, rel) = part.split_once(';')?;
        if rel.trim() == "rel=\"next\"" {
            Some(
                url.trim()
                    .trim_start_matches('<')
                    .trim_end_matches('>')
                    .to_string(),
            )
        } else {
            None
        }
    })
}

/// The value of a source that was read, or `None` with the reason recorded.
fn keep<T>(unreadable: &mut Vec<String>, what: &str, r: Result<T, String>) -> Option<T> {
    match r {
        Ok(v) => Some(v),
        Err(e) => {
            // Bounded: an error body can be long, and the reason rides into the
            // one-line summary and the envelope.
            let e: String = e.chars().take(200).collect();
            unreadable.push(format!("{what}: {e}"));
            None
        }
    }
}

/// A read-only GitHub client.
pub struct Client {
    http: reqwest::Client,
    api: String,
    graphql: String,
    token: Option<String>,
}

impl Client {
    /// Configure from the environment: `GH_TOKEN` / `GITHUB_TOKEN`,
    /// `GITHUB_API_URL`, `GITHUB_GRAPHQL_URL` (GitHub Actions sets the last two).
    pub fn from_env() -> Result<Self, String> {
        let token = ["GH_TOKEN", "GITHUB_TOKEN"]
            .iter()
            .filter_map(|k| std::env::var(k).ok())
            .find(|t| !t.trim().is_empty());
        let api = std::env::var("GITHUB_API_URL")
            .unwrap_or_else(|_| "https://api.github.com".into())
            .trim_end_matches('/')
            .to_string();
        let graphql =
            std::env::var("GITHUB_GRAPHQL_URL").unwrap_or_else(|_| format!("{api}/graphql"));
        let mut builder = reqwest::Client::builder()
            .user_agent(concat!("finishing-bot/", env!("CARGO_PKG_VERSION")))
            .timeout(Duration::from_secs(30));
        // Extra trust roots from the standard SSL_CERT_FILE (a TLS-terminating
        // egress proxy); unset on GitHub-hosted runners, so nothing changes there.
        if let Some(path) = std::env::var_os("SSL_CERT_FILE") {
            let pem = std::fs::read(&path)
                .map_err(|e| format!("SSL_CERT_FILE {}: {e}", path.to_string_lossy()))?;
            for cert in reqwest::Certificate::from_pem_bundle(&pem)
                .map_err(|e| format!("SSL_CERT_FILE {}: {e}", path.to_string_lossy()))?
            {
                builder = builder.add_root_certificate(cert);
            }
        }
        let http = builder.build().map_err(|e| format!("HTTP client: {e}"))?;
        Ok(Self {
            http,
            api,
            graphql,
            token,
        })
    }

    fn get(&self, url: &str) -> reqwest::RequestBuilder {
        let r = self
            .http
            .get(url)
            .header("Accept", "application/vnd.github+json")
            .header("X-GitHub-Api-Version", "2022-11-28");
        match &self.token {
            Some(t) => r.bearer_auth(t),
            None => r,
        }
    }

    /// GET one URL; the body and the next-page URL. The token is only ever
    /// sent to this client's own API base.
    async fn get_page(&self, url: &str) -> Result<(Value, Option<String>), String> {
        if !url.starts_with(&self.api) {
            return Err(format!("refusing to follow a URL outside {}", self.api));
        }
        let resp = self
            .get(url)
            .send()
            .await
            .map_err(|e| format!("request failed: {e}"))?;
        let status = resp.status();
        let next = resp
            .headers()
            .get("link")
            .and_then(|v| v.to_str().ok())
            .and_then(next_link);
        let body: Value = resp
            .json()
            .await
            .map_err(|e| format!("HTTP {status}: unreadable body: {e}"))?;
        if !status.is_success() {
            let msg = body.get("message").and_then(Value::as_str).unwrap_or("");
            return Err(format!("HTTP {}: {msg}", status.as_u16()));
        }
        Ok((body, next))
    }

    async fn one<T: DeserializeOwned>(&self, path: &str) -> Result<T, String> {
        let (body, _) = self.get_page(&format!("{}{path}", self.api)).await?;
        serde_json::from_value(body).map_err(|e| format!("unexpected shape: {e}"))
    }

    /// Every page of a list endpoint; `key` names the array inside an object
    /// page (check runs), `None` for a bare array page.
    async fn all<T: DeserializeOwned>(
        &self,
        path: &str,
        key: Option<&str>,
    ) -> Result<Vec<T>, String> {
        let sep = if path.contains('?') { '&' } else { '?' };
        let mut url = Some(format!("{}{path}{sep}per_page=100", self.api));
        let mut out = Vec::new();
        for _ in 0..MAX_PAGES {
            let Some(u) = url.take() else { break };
            let (body, next) = self.get_page(&u).await?;
            let items = match key {
                Some(k) => body.get(k).cloned().unwrap_or(Value::Null),
                None => body,
            };
            let page: Vec<T> =
                serde_json::from_value(items).map_err(|e| format!("unexpected shape: {e}"))?;
            out.extend(page);
            url = next;
        }
        if url.is_some() {
            return Err(format!("more than {MAX_PAGES} pages; not all read"));
        }
        Ok(out)
    }

    async fn review_threads(&self, repo: &str, pr: u64) -> Result<Vec<ReviewThread>, String> {
        let Some(token) = &self.token else {
            return Err("GraphQL needs a token (GH_TOKEN or GITHUB_TOKEN)".into());
        };
        let (owner, name) = repo.split_once('/').ok_or("bad repository slug")?;
        let mut after: Option<String> = None;
        let mut out = Vec::new();
        for _ in 0..MAX_PAGES {
            let body = json!({
                "query": THREADS_QUERY,
                "variables": {"owner": owner, "name": name, "number": pr, "after": after},
            });
            let resp = self
                .http
                .post(&self.graphql)
                .bearer_auth(token)
                .json(&body)
                .send()
                .await
                .map_err(|e| format!("request failed: {e}"))?;
            let status = resp.status();
            let v: Value = resp
                .json()
                .await
                .map_err(|e| format!("HTTP {status}: unreadable body: {e}"))?;
            if let Some(err) = v.pointer("/errors/0/message").and_then(Value::as_str) {
                return Err(format!("GraphQL: {err}"));
            }
            if !status.is_success() {
                let msg = v.get("message").and_then(Value::as_str).unwrap_or("");
                return Err(format!("HTTP {}: {msg}", status.as_u16()));
            }
            let threads = v
                .pointer("/data/repository/pullRequest/reviewThreads")
                .ok_or("GraphQL: no reviewThreads in the response")?;
            let nodes: Vec<ReviewThread> =
                serde_json::from_value(threads.get("nodes").cloned().unwrap_or(Value::Null))
                    .map_err(|e| format!("unexpected shape: {e}"))?;
            out.extend(nodes);
            if threads
                .pointer("/pageInfo/hasNextPage")
                .and_then(Value::as_bool)
                != Some(true)
            {
                return Ok(out);
            }
            after = threads
                .pointer("/pageInfo/endCursor")
                .and_then(Value::as_str)
                .map(String::from);
        }
        Err(format!("more than {MAX_PAGES} pages; not all read"))
    }

    /// Read every source for `repo` (`owner/name`) pull request `pr`. Fails
    /// only when the pull request itself cannot be read; any other source
    /// that fails is recorded in `unreadable`.
    pub async fn snapshot(&self, repo: &str, pr: u64) -> Result<Snapshot, String> {
        if !valid_repo_slug(repo) {
            return Err(format!("not an owner/name repository slug: {repo:?}"));
        }
        let base = format!("/repos/{repo}");
        let pull: PullRequest = self
            .one(&format!("{base}/pulls/{pr}"))
            .await
            .map_err(|e| format!("pull request {repo}#{pr}: {e}"))?;
        let sha = pull.head.sha.clone();
        if sha.is_empty() || !sha.chars().all(|c| c.is_ascii_hexdigit()) {
            return Err(format!("pull request {repo}#{pr}: no head commit"));
        }
        let branch = encode_branch(&pull.base.ref_name);
        let mut unreadable = Vec::new();
        let compare: Option<Compare> = keep(
            &mut unreadable,
            "compare",
            self.one(&format!("{base}/compare/{branch}...{sha}")).await,
        );
        let check_runs: Option<Vec<CheckRun>> = keep(
            &mut unreadable,
            "check runs",
            self.all(
                &format!("{base}/commits/{sha}/check-runs"),
                Some("check_runs"),
            )
            .await,
        );
        let status: Option<CombinedStatus> = keep(
            &mut unreadable,
            "commit statuses",
            self.one(&format!("{base}/commits/{sha}/status?per_page=100"))
                .await,
        );
        let rules: Option<Vec<Rule>> = keep(
            &mut unreadable,
            "branch rules",
            self.all(&format!("{base}/rules/branches/{branch}"), None)
                .await,
        );
        let branch_info: Option<Branch> = keep(
            &mut unreadable,
            "branch protection",
            self.one(&format!("{base}/branches/{branch}")).await,
        );
        let reviews: Option<Vec<Review>> = keep(
            &mut unreadable,
            "reviews",
            self.all(&format!("{base}/pulls/{pr}/reviews"), None).await,
        );
        let review_threads = keep(
            &mut unreadable,
            "review threads",
            self.review_threads(repo, pr).await,
        );
        Ok(Snapshot {
            pull,
            compare,
            check_runs,
            status,
            rules,
            branch: branch_info,
            reviews,
            review_threads,
            unreadable,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn repository_slugs_are_validated() {
        assert!(valid_repo_slug("hyperpolymath/rsr-template-repo"));
        assert!(valid_repo_slug("metadatastician/standards"));
        for bad in ["", "a", "a/b/c", "../x", "a/..", "a/b c", "a/b?x=1", "/b"] {
            assert!(!valid_repo_slug(bad), "{bad:?}");
        }
    }

    #[test]
    fn branch_names_are_encoded_for_paths() {
        assert_eq!(encode_branch("main"), "main");
        assert_eq!(encode_branch("release/1.x"), "release/1.x");
        assert_eq!(encode_branch("a b#c"), "a%20b%23c");
    }

    #[test]
    fn the_next_page_is_read_from_the_link_header() {
        let h = r#"<https://api.github.com/x?page=2>; rel="next", <https://api.github.com/x?page=5>; rel="last""#;
        assert_eq!(
            next_link(h).as_deref(),
            Some("https://api.github.com/x?page=2")
        );
        assert_eq!(
            next_link(r#"<https://api.github.com/x?page=1>; rel="prev""#),
            None
        );
    }
}
