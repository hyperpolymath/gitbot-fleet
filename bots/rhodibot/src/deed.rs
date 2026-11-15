// SPDX-License-Identifier: MPL-2.0

//! A minimal, read-only reader for the repository's chora deed
//! (`<repo>_chora.deed`).
//!
//! Repo facts that used to live in `.a2ml` files -- manifest, state, meta,
//! ecosystem, agentic, neurosym, playbook, anchor, the RSR profile and the
//! per-bot directives -- are clauses of the chora deed. Rhodibot asks three
//! things of it: *which top-level clauses are present*, *what the
//! `(rsr-profile ...)` clause declares*, and *what the `(bot-directives ...)`
//! clause says about rhodibot*.
//!
//! This is **not** the deed linter. Conformance to the normative grammar
//! (`1-formats/deed/spec/abnf/deed.abnf` in metadatastician/standards) is
//! `deed_lint.js`'s job and runs in standards' CI; rhodibot only needs the
//! s-expression structure, so this reads parentheses, strings, comments and
//! atoms and nothing else. A file that does not balance is refused rather than
//! guessed at.

use std::collections::BTreeSet;

use anyhow::{Result, bail, ensure};

/// One node of the s-expression tree.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    /// A bare token: a symbol, a `:keyword`, a number, `#t`/`#f`, `#u8`.
    Atom(String),
    /// A double-quoted string, escapes resolved.
    Str(String),
    /// A parenthesised list.
    List(Vec<Node>),
}

impl Node {
    /// The text of an atom or string, `None` for a list.
    pub fn text(&self) -> Option<&str> {
        match self {
            Self::Atom(text) | Self::Str(text) => Some(text),
            Self::List(_) => None,
        }
    }

    fn items(&self) -> &[Node] {
        match self {
            Self::List(items) => items,
            _ => &[],
        }
    }

    /// The clause name: the first item when it is an atom.
    fn head(&self) -> Option<&str> {
        match self.items().first() {
            Some(Self::Atom(name)) => Some(name),
            _ => None,
        }
    }

    /// The members after the head.
    fn members(&self) -> &[Node] {
        self.items().get(1..).unwrap_or(&[])
    }

    /// The value following `:key` among this clause's own members.
    fn field(&self, key: &str) -> Option<&Node> {
        let members = self.members();
        members
            .iter()
            .position(|node| matches!(node, Node::Atom(atom) if atom == key))
            .and_then(|index| members.get(index + 1))
    }

    /// Direct child clauses with this name.
    fn children<'a>(&'a self, name: &str) -> impl Iterator<Item = &'a Node> {
        self.members()
            .iter()
            .filter(move |node| node.head() == Some(name))
    }
}

/// A parsed chora deed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Deed {
    root: Node,
}

impl Deed {
    /// Parse a deed. The file must hold exactly one top-level form.
    pub fn parse(source: &str) -> Result<Self> {
        let mut parser = Parser {
            chars: source.chars().collect(),
            at: 0,
        };
        parser.skip_trivia();
        ensure!(
            parser.peek() == Some('('),
            "a deed starts with an opening parenthesis"
        );
        let root = parser.node()?;
        parser.skip_trivia();
        ensure!(
            parser.peek().is_none(),
            "text after the closing parenthesis of the deed: a deed is one form"
        );
        Ok(Self { root })
    }

    /// The names of the top-level clauses, such as `meta` and `anchor`.
    pub fn clauses(&self) -> BTreeSet<String> {
        self.root
            .members()
            .iter()
            .filter_map(Node::head)
            .map(str::to_string)
            .collect()
    }

    /// The first top-level clause with this name.
    pub fn clause<'a>(&'a self, name: &str) -> Option<&'a Node> {
        self.root.children(name).next()
    }

    /// What the `(rsr-profile ...)` clause declares, if there is one.
    pub fn rsr_profile(&self) -> Result<Option<ProfileClause>> {
        let Some(clause) = self.clause("rsr-profile") else {
            return Ok(None);
        };
        const KEYS: &[&str] = &[
            ":preset",
            ":capabilities",
            ":add",
            ":remove",
            ":role",
            ":schema-version",
            ":version",
        ];
        let members = clause.members();
        for node in members {
            if let Node::Atom(atom) = node
                && atom.starts_with(':')
            {
                ensure!(
                    KEYS.contains(&atom.as_str()),
                    "unknown key {atom} in (rsr-profile ...); a misspelt key would declare no \
                     capabilities and silently switch off every gated check"
                );
            }
        }
        let list = |key: &str| -> Result<Vec<String>> {
            match clause.field(key) {
                None => Ok(Vec::new()),
                Some(Node::List(items)) => items
                    .iter()
                    .map(|item| {
                        item.text().map(str::to_string).ok_or_else(|| {
                            anyhow::anyhow!("(rsr-profile {key} ...) holds a nested list")
                        })
                    })
                    .collect(),
                Some(_) => bail!("(rsr-profile {key} ...) must be a list"),
            }
        };
        let word = |key: &str| clause.field(key).and_then(Node::text).map(str::to_string);
        Ok(Some(ProfileClause {
            preset: word(":preset"),
            capabilities: clause
                .field(":capabilities")
                .map(|_| list(":capabilities"))
                .transpose()?,
            add: list(":add")?,
            remove: list(":remove")?,
            role: word(":role"),
        }))
    }

    /// What `(bot-directives (bot :name <bot> ...))` says about one bot.
    pub fn bot_directive<'a>(&'a self, bot: &str) -> Option<BotDirective<'a>> {
        let directives = self.clause("bot-directives")?;
        directives
            .children("bot")
            .find(|node| node.field(":name").and_then(Node::text) == Some(bot))
            .map(|node| BotDirective { node })
    }
}

/// The `(rsr-profile ...)` clause, before it is checked against the gate table.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ProfileClause {
    pub preset: Option<String>,
    pub capabilities: Option<Vec<String>>,
    pub add: Vec<String>,
    pub remove: Vec<String>,
    pub role: Option<String>,
}

/// One `(bot :name ...)` entry of the `(bot-directives ...)` clause.
#[derive(Debug, Clone, Copy)]
pub struct BotDirective<'a> {
    node: &'a Node,
}

impl<'a> BotDirective<'a> {
    /// A symbol or string from the bot's `(config ...)`.
    pub fn config(&self, key: &str) -> Option<&'a str> {
        self.node
            .children("config")
            .find_map(|config| config.field(key))
            .and_then(Node::text)
    }
}

/// Which spelling of a community document a repository uses.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum DocFormat {
    /// `.adoc`: what the estate almost always uses, and the default.
    #[default]
    Adoc,
    /// `.md`: a per-repo exception, declared through the bot directives.
    Md,
}

impl DocFormat {
    pub fn extension(self) -> &'static str {
        match self {
            Self::Adoc => "adoc",
            Self::Md => "md",
        }
    }

    pub fn other(self) -> Self {
        match self {
            Self::Adoc => Self::Md,
            Self::Md => Self::Adoc,
        }
    }

    /// The `(config ...)` key rhodibot reads from its `(bot ...)` directive.
    pub const KEY: &'static str = ":community-doc-format";

    /// The repository's choice, from the deed. Absent means `.adoc`; a value
    /// other than `adoc` or `md` is refused, because quietly falling back to
    /// the default would hide a typo in a directive.
    pub fn from_deed(deed: Option<&Deed>) -> Result<Self> {
        let Some(value) = deed
            .and_then(|deed| deed.bot_directive("rhodibot"))
            .and_then(|directive| directive.config(Self::KEY))
        else {
            return Ok(Self::default());
        };
        match value {
            "adoc" => Ok(Self::Adoc),
            "md" => Ok(Self::Md),
            other => bail!(
                "(bot :name rhodibot (config {} {other})): expected adoc or md",
                Self::KEY
            ),
        }
    }
}

struct Parser {
    chars: Vec<char>,
    at: usize,
}

impl Parser {
    fn peek(&self) -> Option<char> {
        self.chars.get(self.at).copied()
    }

    /// Whitespace and `;` comments.
    fn skip_trivia(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.at += 1;
            } else if c == ';' {
                while let Some(c) = self.peek() {
                    if c == '\n' {
                        break;
                    }
                    self.at += 1;
                }
            } else {
                break;
            }
        }
    }

    fn node(&mut self) -> Result<Node> {
        self.skip_trivia();
        match self.peek() {
            None => bail!("the deed ends inside an open form"),
            Some('(') => {
                self.at += 1;
                let mut items = Vec::new();
                loop {
                    self.skip_trivia();
                    match self.peek() {
                        None => bail!("unbalanced parentheses: a form is never closed"),
                        Some(')') => {
                            self.at += 1;
                            return Ok(Node::List(items));
                        }
                        Some(_) => items.push(self.node()?),
                    }
                }
            }
            Some(')') => bail!("unbalanced parentheses: a closing parenthesis with no opener"),
            Some('"') => self.string(),
            Some(_) => Ok(Node::Atom(self.atom())),
        }
    }

    fn string(&mut self) -> Result<Node> {
        self.at += 1;
        let mut text = String::new();
        while let Some(c) = self.peek() {
            self.at += 1;
            match c {
                '"' => return Ok(Node::Str(text)),
                '\\' => {
                    if let Some(escaped) = self.peek() {
                        self.at += 1;
                        text.push(escaped);
                    }
                }
                other => text.push(other),
            }
        }
        bail!("a string is never closed")
    }

    /// A run of characters up to whitespace, a parenthesis, a quote or a comment.
    fn atom(&mut self) -> String {
        let start = self.at;
        while let Some(c) = self.peek() {
            if c.is_whitespace() || matches!(c, '(' | ')' | '"' | ';') {
                break;
            }
            self.at += 1;
        }
        self.chars[start..self.at].iter().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const DEED: &str = r#"
;; SPDX-License-Identifier: MPL-2.0
(repo-deed
  :schema-version "1.0.0"
  :repo-uuid #u8"gv-clade-index:github.com/o/r"   ; trailing comment
  (meta :version "0.1.0")
  (anchor :id "x")
  (rsr-profile :preset library :add (rust) :remove ())
  (bot-directives
    (bot :name panicbot :allow (assail))
    (bot :name rhodibot (config :community-doc-format md))))
"#;

    #[test]
    fn it_lists_top_level_clauses_only() {
        let deed = Deed::parse(DEED).expect("parses");
        let clauses = deed.clauses();
        assert!(clauses.contains("meta") && clauses.contains("anchor"));
        assert!(clauses.contains("rsr-profile") && clauses.contains("bot-directives"));
        // `bot` and `config` are nested, not top-level.
        assert!(!clauses.contains("bot") && !clauses.contains("config"));
    }

    #[test]
    fn it_reads_the_profile_clause() {
        let deed = Deed::parse(DEED).expect("parses");
        let profile = deed.rsr_profile().expect("reads").expect("present");
        assert_eq!(profile.preset.as_deref(), Some("library"));
        assert_eq!(profile.add, vec!["rust".to_string()]);
        assert!(profile.remove.is_empty());
        assert_eq!(profile.capabilities, None);
    }

    #[test]
    fn a_misspelt_profile_key_is_refused() {
        let deed = Deed::parse("(repo-deed (rsr-profile :capabilites (rust)))").expect("parses");
        assert!(deed.rsr_profile().is_err());
    }

    #[test]
    fn the_doc_format_defaults_to_adoc() {
        assert_eq!(DocFormat::from_deed(None).unwrap(), DocFormat::Adoc);
        let deed = Deed::parse("(repo-deed (bot-directives (bot :name rhodibot)))").unwrap();
        assert_eq!(DocFormat::from_deed(Some(&deed)).unwrap(), DocFormat::Adoc);
    }

    #[test]
    fn the_directive_selects_md_for_rhodibot_only() {
        let deed = Deed::parse(DEED).expect("parses");
        assert_eq!(DocFormat::from_deed(Some(&deed)).unwrap(), DocFormat::Md);
        // Another bot's config is not rhodibot's.
        let other = Deed::parse(
            "(repo-deed (bot-directives (bot :name panicbot (config :community-doc-format md))))",
        )
        .unwrap();
        assert_eq!(DocFormat::from_deed(Some(&other)).unwrap(), DocFormat::Adoc);
    }

    #[test]
    fn a_bad_format_value_is_refused() {
        let deed = Deed::parse(
            "(repo-deed (bot-directives (bot :name rhodibot (config :community-doc-format rst))))",
        )
        .unwrap();
        assert!(DocFormat::from_deed(Some(&deed)).is_err());
    }

    #[test]
    fn unbalanced_input_is_refused() {
        assert!(Deed::parse("(repo-deed (meta").is_err());
        assert!(Deed::parse("(repo-deed))").is_err());
        assert!(Deed::parse("repo-deed").is_err());
        assert!(Deed::parse("(repo-deed :x \"open)").is_err());
    }

    #[test]
    fn parentheses_inside_strings_and_comments_do_not_count() {
        let deed = Deed::parse("(repo-deed ; ) (\n (meta :note \"a ) b ( c\"))").unwrap();
        assert!(deed.clauses().contains("meta"));
    }
}
