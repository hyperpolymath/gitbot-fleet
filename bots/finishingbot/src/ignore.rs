// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! Scoped exemptions from the repository's `.hypatia-ignore`.
//!
//! The estate already has one documented, reviewed place for "this path is
//! exempt from that rule, and here is why": `.hypatia-ignore`, which the RSR
//! template ships and scaffolded repositories inherit. finishing-bot honours
//! the same file with the same grammar rather than growing a second ignore
//! mechanism, and rather than special-casing any repository by name.
//!
//! Grammar (one entry per line; `#` starts a comment):
//!
//! ```text
//! <module>/<rule>:<path-fragment>   rule-scoped, e.g. finishing_bot/PH:build/just/
//! <module>/*:<path-fragment>        any rule of that module
//! <path-fragment>                   any rule, any module
//! ```
//!
//! A path fragment is a substring of the repo-relative path. This bot's module
//! is `finishing_bot`; entries for other modules (`structural_drift/...`) are
//! not ours and are ignored. A `<rule>` is an exact finding id (`PH-002`) or a
//! family prefix (`PH` matches `PH-001`, `PH-002`, ...).

use std::path::Path;

/// The module name under which finishing-bot's rules are addressed.
pub const MODULE: &str = "finishing_bot";

#[derive(Debug, Default, Clone)]
pub struct IgnoreFile {
    entries: Vec<Entry>,
}

#[derive(Debug, Clone)]
struct Entry {
    /// `None` = any rule.
    rule: Option<String>,
    fragment: String,
}

/// Drop a trailing `# reason`: a `#` that starts the line or follows whitespace.
fn strip_inline_comment(line: &str) -> &str {
    let mut prev_ws = true;
    for (i, c) in line.char_indices() {
        if c == '#' && prev_ws {
            return &line[..i];
        }
        prev_ws = c.is_whitespace();
    }
    line
}

impl IgnoreFile {
    /// Load `<root>/.hypatia-ignore`; an absent or unreadable file exempts nothing.
    pub fn load(root: &Path) -> Self {
        std::fs::read_to_string(root.join(".hypatia-ignore"))
            .map(|t| Self::parse(&t))
            .unwrap_or_default()
    }

    pub fn parse(text: &str) -> Self {
        let mut entries = Vec::new();
        for line in text.lines() {
            let line = strip_inline_comment(line).trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
            let (scope, fragment) = match line.split_once(':') {
                Some((scope, fragment))
                    if scope.contains('/') && !scope.contains(char::is_whitespace) =>
                {
                    (Some(scope), fragment)
                }
                _ => (None, line),
            };
            let fragment = fragment.trim();
            if fragment.is_empty() {
                continue;
            }
            let rule = match scope {
                None => None,
                Some(scope) => {
                    let (module, rule) = scope.split_once('/').unwrap_or((scope, "*"));
                    if module != MODULE {
                        continue; // another module's entry
                    }
                    if rule == "*" {
                        None
                    } else {
                        Some(rule.to_string())
                    }
                }
            };
            entries.push(Entry {
                rule,
                fragment: fragment.to_string(),
            });
        }
        Self { entries }
    }

    /// Does an entry exempt finding `rule_id` at repo-relative path `rel`?
    pub fn suppresses(&self, rule_id: &str, rel: &str) -> bool {
        let rel = rel.strip_prefix("./").unwrap_or(rel);
        self.entries.iter().any(|e| {
            rel.contains(&e.fragment)
                && e.rule.as_deref().is_none_or(|r| {
                    rule_id == r
                        || rule_id
                            .strip_prefix(r)
                            .is_some_and(|rest| rest.starts_with('-'))
                })
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const FILE: &str = "\
# comment
structural_drift/SD022:build/docs-seed/
finishing_bot/PH:build/just/
finishing_bot/PH-002:Justfile
finishing_bot/*:scripts/check-
generated/
";

    #[test]
    fn rule_family_and_exact_id_and_wildcard_and_bare_entries() {
        let f = IgnoreFile::parse(FILE);
        assert!(f.suppresses("PH-001", "build/just/a.just")); // family PH
        assert!(f.suppresses("PH-003", "./build/just/a.just")); // ./ tolerated
        assert!(f.suppresses("PH-002", "Justfile")); // exact id
        assert!(f.suppresses("REL-003", "scripts/check-x.sh")); // module wildcard
        assert!(f.suppresses("CLM-006", "generated/out.md")); // bare: any rule
    }

    #[test]
    fn non_matching_rule_path_or_module_is_not_suppressed() {
        let f = IgnoreFile::parse(FILE);
        assert!(!f.suppresses("PH-001", "src/main.rs")); // other path
        assert!(!f.suppresses("PH-003", "Justfile")); // exact id is PH-002 only
        assert!(!f.suppresses("REL-003", "build/just/a.just")); // family is PH only
        assert!(!f.suppresses("SD022", "build/docs-seed/x")); // another module's entry
        assert!(!f.suppresses("PHX-001", "build/just/a.just")); // prefix needs '-' boundary
    }

    #[test]
    fn inline_reason_comment_is_not_part_of_the_path() {
        let f = IgnoreFile::parse("finishing_bot/PH:build/just/  # scanner names tokens\ngenerated/ # reason\nkeep#hash/\n");
        assert!(f.suppresses("PH-001", "build/just/a.just"));
        assert!(f.suppresses("CLM-001", "generated/x"));
        assert!(f.suppresses("CLM-001", "keep#hash/x"));
        assert!(!f.suppresses("PH-001", "reason"));
    }

    #[test]
    fn absent_file_exempts_nothing() {
        let d = tempfile::TempDir::new().unwrap();
        assert!(!IgnoreFile::load(d.path()).suppresses("PH-001", "build/just/a.just"));
    }
}
