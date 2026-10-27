// SPDX-License-Identifier: MPL-2.0
//! The GitHub code-search walk. All I/O goes through [`Io`], whose only
//! network verb is `get`: the audit cannot write to GitHub by construction.

use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::time::Duration;

use anyhow::{bail, Context, Result};
use serde::Serialize;

use crate::catalogue::{classify, is_distinctive, search_stems, Confidence};
use crate::{ipkg, refs};

pub const API: &str = "https://api.github.com";
/// Authenticated code search allows 10 requests a minute.
pub const SEARCH_GAP: Duration = Duration::from_millis(6500);
/// GitHub code search never returns more than this many results per query.
const SEARCH_CAP: u64 = 1000;

pub struct Response {
    pub status: u16,
    /// Header names in lower case.
    pub headers: BTreeMap<String, String>,
    pub body: String,
}

impl Response {
    fn into_body(self, url: &str) -> Result<String> {
        if !(200..300).contains(&self.status) {
            bail!("HTTP {} for {url}", self.status);
        }
        Ok(self.body)
    }
}

/// Side effects the scan needs. `raw` asks for file contents instead of JSON.
pub trait Io {
    fn get(&mut self, url: &str, raw: bool) -> Result<Response>;
    fn sleep(&mut self, d: Duration);
    fn now_secs(&self) -> u64;
    fn log(&mut self, msg: &str);
}

pub struct Options {
    pub owners: Vec<String>,
    /// Accounts whose repositories are yours and are never reported.
    pub self_accounts: Vec<String>,
    /// Skip cataloguing and use these package names.
    pub packages: Option<Vec<String>>,
}

#[derive(Debug, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Row {
    pub owner: String,
    pub repo: String,
    pub deps: Vec<String>,
    pub pack_refs: Vec<refs::PackRef>,
    pub sub_refs: Vec<String>,
    pub files: Vec<String>,
    pub confidence: Confidence,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Report {
    pub catalogue_size: usize,
    pub rows: Vec<Row>,
    /// Queries whose results GitHub truncated or marked incomplete.
    pub incomplete_queries: Vec<String>,
    /// Raw file URLs unavailable with HTTP 404, 410, or 451.
    pub skipped_urls: Vec<String>,
}

#[derive(Clone, Copy, PartialEq)]
enum Kind {
    Ipkg,
    Pack,
    Submodule,
}

struct Hit {
    url: String,
    html_url: String,
    full_name: String,
    owner: String,
}

#[derive(Default)]
struct Acc {
    owner: String,
    deps: BTreeSet<String>,
    pack_refs: Vec<refs::PackRef>,
    sub_refs: Vec<String>,
    files: Vec<String>,
}

struct Scanner<'a, I: Io> {
    io: &'a mut I,
    incomplete: Vec<String>,
    skipped_urls: Vec<String>,
}

impl<I: Io> Scanner<'_, I> {
    fn get(&mut self, url: &str, raw: bool) -> Result<Response> {
        for _ in 0..=5 {
            let res = self.io.get(url, raw)?;
            let h = |k: &str| res.headers.get(k).map(String::as_str);
            let limited = res.status == 429
                || (res.status == 403
                    && (h("retry-after").is_some() || h("x-ratelimit-remaining") == Some("0")));
            if !limited {
                return Ok(res);
            }
            let retry_after = h("retry-after").and_then(|v| v.parse::<u64>().ok());
            let reset = h("x-ratelimit-reset").and_then(|v| v.parse::<u64>().ok());
            let wait = match (retry_after, reset) {
                (Some(s), _) if s > 0 => s,
                (_, Some(r)) => r.saturating_sub(self.io.now_secs()).max(1) + 1,
                _ => 60,
            };
            self.io.log(&format!("rate-limited; waiting {wait}s"));
            self.io.sleep(Duration::from_secs(wait));
        }
        bail!("still rate-limited after 5 retries: {url}")
    }

    fn get_file(&mut self, url: &str) -> Result<Option<String>> {
        let res = self.get(url, true)?;
        if matches!(res.status, 404 | 410 | 451) {
            self.io.log(&format!(
                "  skipping unavailable file {url}: HTTP {}",
                res.status
            ));
            self.skipped_urls.push(url.to_string());
            return Ok(None);
        }
        res.into_body(url).map(Some)
    }

    fn search_all(&mut self, q: &str) -> Result<Vec<Hit>> {
        let mut items = Vec::new();
        for page in 1..=10 {
            let url = format!(
                "{API}/search/code?q={}&per_page=100&page={page}",
                urlencode(q)
            );
            let body: serde_json::Value =
                serde_json::from_str(&self.get(&url, false)?.into_body(&url)?)
                    .with_context(|| format!("search response for {q}"))?;
            self.io.sleep(SEARCH_GAP);
            let total = body["total_count"].as_u64().unwrap_or(0);
            if page == 1 && total > SEARCH_CAP {
                self.io.log(&format!(
                    "  note: {total} results for `{q}`; GitHub returns at most {SEARCH_CAP}"
                ));
                self.incomplete.push(q.to_string());
            }
            if body["incomplete_results"].as_bool() == Some(true) {
                self.io.log(&format!(
                    "  note: GitHub marked results incomplete for: {q}"
                ));
                if !self.incomplete.iter().any(|x| x == q) {
                    self.incomplete.push(q.to_string());
                }
            }
            let page_items = body["items"].as_array().cloned().unwrap_or_default();
            let n = page_items.len();
            items.extend(page_items.iter().filter_map(|it| {
                Some(Hit {
                    url: it["url"].as_str()?.to_string(),
                    html_url: it["html_url"].as_str()?.to_string(),
                    full_name: it["repository"]["full_name"].as_str()?.to_string(),
                    owner: it["repository"]["owner"]["login"].as_str()?.to_string(),
                })
            }));
            if n < 100 || items.len() as u64 >= total {
                break;
            }
        }
        Ok(items)
    }
}

/// Percent-encode a query string component (RFC 3986 unreserved kept).
fn urlencode(s: &str) -> String {
    s.bytes()
        .map(|b| match b {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => {
                (b as char).to_string()
            }
            _ => format!("%{b:02X}"),
        })
        .collect()
}

pub fn run<I: Io>(opts: &Options, io: &mut I) -> Result<Report> {
    let self_set: HashSet<String> = opts
        .self_accounts
        .iter()
        .map(|s| s.to_ascii_lowercase())
        .collect();
    let mut sc = Scanner {
        io,
        incomplete: Vec::new(),
        skipped_urls: Vec::new(),
    };

    // 1. Catalogue of your package names (or use --packages to skip this step).
    let catalogue = match &opts.packages {
        Some(p) => p.clone(),
        None => {
            let mut names = BTreeSet::new();
            for o in &opts.owners {
                sc.io.log(&format!("cataloguing .ipkg packages under {o}…"));
                for it in sc.search_all(&format!("package extension:ipkg user:{o}"))? {
                    let Some(text) = sc.get_file(&it.url)? else {
                        continue;
                    };
                    if let Some(n) = ipkg::package_name(&text) {
                        names.insert(n);
                    }
                }
            }
            names.into_iter().collect()
        }
    };
    let distinctive: HashSet<&str> = catalogue
        .iter()
        .map(String::as_str)
        .filter(|n| is_distinctive(n))
        .collect();
    let stems = search_stems(&catalogue);
    sc.io.log(&format!(
        "{} distinctive package names; stems: {}",
        distinctive.len(),
        stems.join(", ")
    ));

    // 2. Package-name stems in .ipkg files, plus pack.toml / .gitmodules pointing at you.
    let mut queries: Vec<(String, Kind)> = stems
        .iter()
        .map(|s| (format!("{s} extension:ipkg"), Kind::Ipkg))
        .collect();
    for o in &opts.owners {
        queries.push((format!("{o} filename:pack.toml"), Kind::Pack));
        queries.push((format!("{o} filename:.gitmodules"), Kind::Submodule));
    }

    let mut hits: BTreeMap<String, Acc> = BTreeMap::new();
    let mut seen_files = HashSet::new();
    for (q, kind) in &queries {
        sc.io.log(&format!("search: {q}"));
        for it in sc.search_all(q)? {
            if self_set.contains(&it.owner.to_ascii_lowercase())
                || !seen_files.insert(it.html_url.clone())
            {
                continue;
            }
            let Some(text) = sc.get_file(&it.url)? else {
                continue;
            };
            let acc = hits.entry(it.full_name.clone()).or_default();
            acc.owner = it.owner.clone();
            let found = match kind {
                Kind::Ipkg => {
                    let matched: Vec<String> = ipkg::depends(&text)
                        .into_iter()
                        .filter(|d| distinctive.contains(d.as_str()))
                        .collect();
                    let found = !matched.is_empty();
                    acc.deps.extend(matched);
                    found
                }
                Kind::Pack => match refs::pack_toml_refs(&text, &opts.owners) {
                    Ok(r) => {
                        let found = !r.is_empty();
                        acc.pack_refs.extend(r);
                        found
                    }
                    Err(e) => {
                        sc.io.log(&format!(
                            "  skipping unparseable pack.toml {}: {e}",
                            it.html_url
                        ));
                        false
                    }
                },
                Kind::Submodule => {
                    let r = refs::gitmodules_refs(&text, &opts.owners);
                    let found = !r.is_empty();
                    acc.sub_refs.extend(r);
                    found
                }
            };
            if found {
                acc.files.push(it.html_url);
            }
        }
    }

    // 3. Classify; most confident first, then by repository name.
    let mut rows: Vec<Row> = hits
        .into_iter()
        .filter(|(_, a)| !a.files.is_empty())
        .filter_map(|(repo, a)| {
            let deps: Vec<String> = a.deps.into_iter().collect();
            let confidence = classify(&deps, a.pack_refs.len(), a.sub_refs.len())?;
            Some(Row {
                owner: a.owner,
                repo,
                deps,
                pack_refs: a.pack_refs,
                sub_refs: a.sub_refs,
                files: a.files,
                confidence,
            })
        })
        .collect();
    rows.sort_by(|a, b| {
        a.confidence
            .cmp(&b.confidence)
            .then_with(|| a.repo.cmp(&b.repo))
    });
    Ok(Report {
        catalogue_size: distinctive.len(),
        rows,
        incomplete_queries: sc.incomplete,
        skipped_urls: sc.skipped_urls,
    })
}
