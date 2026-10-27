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

/// Match a watched owner in a GitHub-style repository reference, including
/// HTTPS, SSH, and scp forms. Owner matching is ASCII case-insensitive and
/// requires a following `/`.
///
/// This is a textual check, not full URL validation: `github.com` may follow
/// `/`, `@`, or `.` (or start the input), so subdomains and embedded paths can match.
pub fn points_at(url: &str, owners: &[String]) -> bool {
    let url = url.to_ascii_lowercase();
    let mut from = 0;
    while let Some(k) = url[from..].find("github.com") {
        let at = from + k;
        let host_start = at == 0 || matches!(url.as_bytes()[at - 1], b'/' | b'@' | b'.');
        let rest = &url[at + "github.com".len()..];
        if host_start && (rest.starts_with('/') || rest.starts_with(':')) {
            let path = &rest[1..];
            if owners.iter().any(|o| {
                let o = o.to_ascii_lowercase();
                path.strip_prefix(o.as_str())
                    .is_some_and(|r| r.starts_with('/'))
            }) {
                return true;
            }
        }
        from = at + 1;
    }
    false
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
pub fn gitmodules_refs(text: &str, owners: &[String]) -> Vec<String> {
    text.lines()
        .filter_map(|line| {
            let line = line.trim();
            let value = line
                .strip_prefix("url")?
                .trim_start()
                .strip_prefix('=')?
                .trim();
            let value = match value.strip_prefix('"') {
                Some(q) => q.split('"').next().unwrap_or(""),
                None => value.split(['#', ';']).next().unwrap_or("").trim(),
            };
            points_at(value, owners).then(|| value.to_string())
        })
        .collect()
}
