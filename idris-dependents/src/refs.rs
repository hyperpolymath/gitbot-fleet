// SPDX-License-Identifier: MPL-2.0
//! References to GitHub owners from `pack.toml` and `.gitmodules`.

use serde::Serialize;

/// A `[custom.<collection>.<name>]` entry in `pack.toml` whose `url` points at
/// one of the watched owners.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PackRef {
    pub name: String,
    pub url: String,
}

/// The repository path of a GitHub URL (`scheme://[user@]host[:port]/path` or
/// scp-style `[user@]host:path`), lower-cased, when the host is GitHub itself.
fn github_path(url: &str) -> Option<String> {
    let url = url.trim().to_ascii_lowercase();
    let (authority, path) = match url.find("://") {
        Some(i) => {
            let rest = &url[i + 3..];
            // The authority ends at the first `/`, `?` or `#` (RFC 3986), so
            // `https://evil.example#@github.com/...` has host `evil.example`.
            let end = rest.find(['/', '?', '#'])?;
            if !rest[end..].starts_with('/') {
                return None;
            }
            (&rest[..end], &rest[end + 1..])
        }
        None => {
            let colon = url.find(':')?;
            if url[..colon].contains('/') {
                return None;
            }
            (&url[..colon], url[colon + 1..].trim_start_matches('/'))
        }
    };
    let host = authority.rsplit('@').next()?.split(':').next()?;
    matches!(host, "github.com" | "www.github.com").then(|| path.to_string())
}

/// Match a watched owner in a GitHub repository URL (HTTPS, SSH or scp form).
/// GitHub must be the URL's host, not merely appear in its path, and the owner
/// must be followed by a repository name. Matching is ASCII case-insensitive.
pub fn points_at(url: &str, owners: &[String]) -> bool {
    github_path(url).is_some_and(|path| {
        // A query or fragment is not part of the repository path.
        let path = path.split(['?', '#']).next().unwrap_or("");
        let mut parts = path.split('/');
        let (Some(owner), Some(repo)) = (parts.next(), parts.next()) else {
            return false;
        };
        !repo.is_empty() && owners.iter().any(|o| o.eq_ignore_ascii_case(owner))
    })
}

/// Resolve a submodule URL relative to its superproject (`../x` and `./x`, as
/// git does against the superproject's remote), taking the remote to be
/// `https://github.com/<full_name>`. Absolute URLs are returned unchanged.
/// Like git, `../` may climb past `owner/repo` into the host part, so
/// `../../../github.com/o/r` from `a/b` is `https://github.com/o/r`; climbing
/// past the host yields `None` (git's result there is never a GitHub URL).
fn resolve_submodule_url(url: &str, superproject: Option<&str>) -> Option<String> {
    if !(url.starts_with("../") || url.starts_with("./")) {
        return Some(url.to_string());
    }
    let mut base: Vec<&str> = std::iter::once("github.com")
        .chain(superproject?.split('/').filter(|s| !s.is_empty()))
        .collect();
    let mut rest = url;
    loop {
        if let Some(r) = rest.strip_prefix("../") {
            base.pop()?;
            rest = r;
        } else if let Some(r) = rest.strip_prefix("./") {
            rest = r;
        } else {
            break;
        }
    }
    base.push(rest);
    Some(format!("https://{}", base.join("/")))
}

/// `pack.toml` custom entries pointing at `owners`, in file order.
/// Uses a real TOML parser, so quoting styles and `#` inside strings are
/// handled the way pack itself handles them.
/// Missing custom tables or entries without matching string URLs are ignored.
/// Returns a TOML parse error if the document is invalid.
pub fn pack_toml_refs(text: &str, owners: &[String]) -> Result<Vec<PackRef>, toml::de::Error> {
    let doc: toml::Table = text.parse()?;
    let mut refs = Vec::new();
    let Some(custom) = doc.get("custom").and_then(|v| v.as_table()) else {
        return Ok(refs);
    };
    for collection in custom.values().filter_map(|v| v.as_table()) {
        for (name, entry) in collection {
            if let Some(url) = entry.get("url").and_then(|u| u.as_str()) {
                if points_at(url, owners) {
                    refs.push(PackRef {
                        name: name.clone(),
                        url: url.to_string(),
                    });
                }
            }
        }
    }
    Ok(refs)
}

/// Extract URLs matching `owners` from trimmed `url =` lines, in input order.
/// Double-quoted values end at the next quote; unquoted values end at `#` or
/// `;`. Sections and escapes are not interpreted, and duplicates are retained.
/// Relative URLs cannot be resolved without the superproject, so they are
/// skipped; see [`gitmodules_refs_in`].
pub fn gitmodules_refs(text: &str, owners: &[String]) -> Vec<String> {
    gitmodules_refs_in(text, owners, None)
}

/// Like [`gitmodules_refs`], resolving relative submodule URLs against the
/// GitHub repository `superproject` (`owner/repo`). Keys are matched
/// case-insensitively, as git config does; relative matches are returned
/// resolved to absolute URLs.
pub fn gitmodules_refs_in(
    text: &str,
    owners: &[String],
    superproject: Option<&str>,
) -> Vec<String> {
    text.lines()
        .filter_map(|line| {
            let line = line.trim();
            let key = line.get(..3)?;
            if !key.eq_ignore_ascii_case("url") {
                return None;
            }
            let value = line[3..].trim_start().strip_prefix('=')?.trim();
            let value = match value.strip_prefix('"') {
                Some(q) => q.split('"').next().unwrap_or(""),
                None => value.split(['#', ';']).next().unwrap_or("").trim(),
            };
            let url = resolve_submodule_url(value, superproject)?;
            points_at(&url, owners).then_some(url)
        })
        .collect()
}
