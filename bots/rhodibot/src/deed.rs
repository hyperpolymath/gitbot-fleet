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
        ensure!(
            self.root.children("rsr-profile").count() <= 1,
            "more than one (rsr-profile ...) clause; a chora deed has exactly one"
        );
        let Some(clause) = self.clause("rsr-profile") else {
            return Ok(None);
        };
        let keys = RSR_PROFILE_KEYS;
        let members = clause.members();
        let mut seen_clauses = BTreeSet::new();
        let mut layout = Vec::new();
        for (index, node) in members.iter().enumerate() {
            if let Node::Atom(atom) = node
                && atom.starts_with(':')
            {
                ensure!(
                    keys.contains(&atom.as_str()),
                    "unknown key {atom} in (rsr-profile ...); a misspelt key would declare no \
                     capabilities and silently switch off every gated check"
                );
            }
            // A list right after a :keyword is that field's value. Any other
            // list is a sub-clause, and J39 refuses one it does not know
            // rather than skipping it.
            let is_value = index > 0
                && matches!(&members[index - 1], Node::Atom(atom) if atom.starts_with(':'));
            if let Node::List(_) = node
                && !is_value
            {
                let head = node.head().unwrap_or("");
                ensure!(
                    RSR_PROFILE_CLAUSES.contains(&head),
                    "unknown clause ({head} ...) in (rsr-profile ...); J39 refuses a sub-clause \
                     the standards module does not define"
                );
                ensure!(
                    seen_clauses.insert(head.to_string()),
                    "(rsr-profile ...) carries ({head} ...) once"
                );
                if head == "layout" {
                    layout = layout_bindings(node)?;
                }
            }
        }
        let list = |key: &str| -> Result<Vec<String>> {
            match clause.field(key) {
                None => Ok(Vec::new()),
                Some(Node::List(items)) => items
                    .iter()
                    .map(|item| match item {
                        Node::Str(text) => Ok(text.clone()),
                        _ => bail!(
                            "(rsr-profile {key} ...) holds a non-string; capabilities, :add \
                             and :remove are lists of strings and symbols are refused"
                        ),
                    })
                    .collect(),
                Some(_) => bail!("(rsr-profile {key} ...) must be a list"),
            }
        };
        let word = |key: &str| clause.field(key).and_then(Node::text).map(str::to_string);
        ensure!(
            clause.field(":capabilities").is_some() || clause.field(":preset").is_some(),
            "(rsr-profile ...) needs :capabilities or :preset"
        );
        Ok(Some(ProfileClause {
            preset: word(":preset"),
            capabilities: clause
                .field(":capabilities")
                .map(|_| list(":capabilities"))
                .transpose()?,
            add: list(":add")?,
            remove: list(":remove")?,
            role: word(":role"),
            layout,
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

/// The members of `(rsr-profile (layout ...))`: one or more
/// `(module :gate "<row>" :at ("<path>" ...))` (standards rsr-profile.abnf
/// v1.1.0, owner ruling 2026-11-16). This reader checks the shape and keys;
/// whether a gate names a real row, and what each path must be, is standards'
/// `check-rsr-profile.sh` and `deed_lint.js --rsr-profile`.
fn layout_bindings(layout: &Node) -> Result<Vec<LayoutBinding>> {
    let modules = layout.members();
    ensure!(
        !modules.is_empty(),
        "(rsr-profile (layout ...)) binds at least one (module ...)"
    );
    let mut out: Vec<LayoutBinding> = Vec::new();
    for module in modules {
        ensure!(
            module.head() == Some("module"),
            "(rsr-profile (layout ...)) holds (module ...) clauses only (J39)"
        );
        let members = module.members();
        let mut index = 0;
        let (mut gate, mut at) = (None, None);
        while index < members.len() {
            let key = match &members[index] {
                Node::Atom(atom) if RSR_PROFILE_LAYOUT_KEYS.contains(&atom.as_str()) => atom,
                other => bail!(
                    "(rsr-profile (layout (module ...))) holds :gate and :at only, not {other:?} \
                     (J39)"
                ),
            };
            let value = members.get(index + 1);
            match (key.as_str(), value) {
                (":gate", Some(Node::Str(text))) if !text.is_empty() => {
                    ensure!(gate.is_none(), "(layout (module ...)) carries :gate once");
                    gate = Some(text.clone());
                }
                (":at", Some(Node::List(items)))
                    if !items.is_empty() && items.iter().all(|i| matches!(i, Node::Str(_))) =>
                {
                    ensure!(at.is_none(), "(layout (module ...)) carries :at once");
                    at = Some(
                        items
                            .iter()
                            .filter_map(Node::text)
                            .map(str::to_string)
                            .collect::<Vec<_>>(),
                    );
                }
                (":gate", _) => bail!("(layout (module :gate ...)) must be a non-empty string"),
                _ => bail!("(layout (module :at ...)) must be a non-empty list of strings"),
            }
            index += 2;
        }
        let gate = gate.ok_or_else(|| anyhow::anyhow!("(layout (module ...)) is missing :gate"))?;
        let at = at.ok_or_else(|| anyhow::anyhow!("(layout (module ...)) is missing :at"))?;
        ensure!(
            out.iter().all(|binding| binding.gate != gate),
            "(rsr-profile (layout ...)) binds :gate \"{gate}\" once"
        );
        out.push(LayoutBinding { gate, at });
    }
    Ok(out)
}

/// The top-level keys of `(rsr-profile ...)`, as the standards module defines
/// them (`bun 1-formats/deed/tools/deed_lint.js --rsr-profile-keys`, standards
/// #1605, #1630). `(notes ...)` and `(layout ...)` are its sub-clauses
/// ([`RSR_PROFILE_CLAUSES`]); `(notes ...)` holds prose, so it is not scanned
/// here. Any other key or sub-clause fails closed.
pub const RSR_PROFILE_KEYS: &[&str] = &[
    ":version",
    ":spec",
    ":declares-against",
    ":role",
    ":preset",
    ":capabilities",
    ":add",
    ":remove",
];

/// The sub-clauses of `(rsr-profile ...)`, in the order
/// `deed_lint.js --rsr-profile-keys` prints them after the keys.
pub const RSR_PROFILE_CLAUSES: &[&str] = &["notes", "layout"];

/// The keys of a `(layout (module ...))` binding.
pub const RSR_PROFILE_LAYOUT_KEYS: &[&str] = &[":gate", ":at"];

/// One `(module :gate ... :at ...)` of `(rsr-profile (layout ...))`: where this
/// repository keeps the module a gate-table row asks for.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LayoutBinding {
    pub gate: String,
    pub at: Vec<String>,
}

/// The `(rsr-profile ...)` clause, before it is checked against the gate table.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ProfileClause {
    pub preset: Option<String>,
    pub capabilities: Option<Vec<String>>,
    pub add: Vec<String>,
    pub remove: Vec<String>,
    pub role: Option<String>,
    /// The `(layout ...)` bindings, in order; empty when there is none.
    pub layout: Vec<LayoutBinding>,
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
  (rsr-profile :preset library :add ("rust") :remove ())
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

#[cfg(test)]
mod profile_shape_tests {
    use super::*;

    #[test]
    fn the_real_string_shape_reads() {
        let deed = Deed::parse(
            "(repo-deed (rsr-profile :version \"1.0.0\" :spec \"rsr-criteria-v2\" \
             :declares-against \"2.1.1\" :role \"spine\" :capabilities (\"zig\" \"bash\") \
             :add (\"rust\") :remove (\"bash\")))",
        )
        .unwrap();
        let p = deed.rsr_profile().unwrap().unwrap();
        assert_eq!(p.role.as_deref(), Some("spine"));
        assert_eq!(p.capabilities, Some(vec!["zig".into(), "bash".into()]));
        assert_eq!(p.add, vec!["rust".to_string()]);
    }

    #[test]
    fn two_clauses_or_an_empty_one_is_a_defect() {
        let two =
            Deed::parse("(repo-deed (rsr-profile :preset a) (rsr-profile :preset b))").unwrap();
        assert!(two.rsr_profile().is_err());
        let empty = Deed::parse("(repo-deed (rsr-profile :role \"x\"))").unwrap();
        assert!(empty.rsr_profile().is_err());
    }
}

#[cfg(test)]
mod module_alignment_tests {
    use super::*;

    /// The set standards' module reports (`deed_lint.js --rsr-profile-keys`).
    #[test]
    fn the_key_set_is_the_modules() {
        assert_eq!(
            RSR_PROFILE_KEYS,
            [
                ":version",
                ":spec",
                ":declares-against",
                ":role",
                ":preset",
                ":capabilities",
                ":add",
                ":remove"
            ]
        );
    }

    #[test]
    fn keys_that_belong_to_other_clauses_are_refused() {
        // :policy belongs to (anchor ...) and (egress ...); :omitted-rationale
        // belongs inside (notes ...).
        for key in [":policy", ":omitted-rationale", ":schema-version"] {
            let deed = Deed::parse(&format!(
                "(repo-deed (rsr-profile :capabilities (\"rust\") {key} \"x\"))"
            ))
            .unwrap();
            assert!(deed.rsr_profile().is_err(), "{key}");
        }
    }

    #[test]
    fn omitted_rationale_inside_notes_is_fine() {
        let deed = Deed::parse(
            "(repo-deed (rsr-profile :capabilities (\"rust\") (notes :omitted-rationale \"why\")))",
        )
        .unwrap();
        assert!(deed.rsr_profile().unwrap().is_some());
    }

    /// The sub-clauses standards' module reports after the keys.
    #[test]
    fn the_clause_set_is_the_modules() {
        assert_eq!(RSR_PROFILE_CLAUSES, ["notes", "layout"]);
        assert_eq!(RSR_PROFILE_LAYOUT_KEYS, [":gate", ":at"]);
    }

    #[test]
    fn an_unknown_sub_clause_is_refused_not_skipped() {
        let deed =
            Deed::parse("(repo-deed (rsr-profile :capabilities (\"rust\") (extras :x \"y\")))")
                .unwrap();
        let err = deed.rsr_profile().unwrap_err().to_string();
        assert!(err.contains("unknown clause (extras"), "{err}");
    }

    #[test]
    fn a_list_value_after_a_key_is_not_a_sub_clause() {
        // ("rust") follows :capabilities, so it is that field's value.
        let deed = Deed::parse("(repo-deed (rsr-profile :capabilities (\"rust\")))").unwrap();
        assert!(deed.rsr_profile().unwrap().is_some());
    }

    #[test]
    fn the_template_layout_binding_is_read() {
        let deed = Deed::parse(
            "(repo-deed (rsr-profile :version \"1.0.0\" :role \"spine\" \
             :capabilities (\"abi\") \
             (layout (module :gate \"abi.ipkg\" :at (\"src/interface/abi.ipkg\"))) \
             (notes :omitted-rationale \"x\")))",
        )
        .unwrap();
        let clause = deed.rsr_profile().unwrap().unwrap();
        assert_eq!(
            clause.layout,
            vec![LayoutBinding {
                gate: "abi.ipkg".into(),
                at: vec!["src/interface/abi.ipkg".into()]
            }]
        );
    }

    #[test]
    fn malformed_layouts_are_refused() {
        for body in [
            "(layout)",
            "(layout :gate \"abi.ipkg\")",
            "(layout (path :gate \"abi.ipkg\" :at (\"a/abi.ipkg\")))",
            "(layout (module :gate \"abi.ipkg\" :at (\"a/abi.ipkg\") :why \"x\"))",
            "(layout (module :at (\"a/abi.ipkg\")))",
            "(layout (module :gate \"abi.ipkg\"))",
            "(layout (module :gate \"abi.ipkg\" :at ()))",
            "(layout (module :gate \"abi.ipkg\" :at \"a/abi.ipkg\"))",
            "(layout (module :gate abi :at (\"a/abi.ipkg\")))",
            "(layout (module :gate \"abi.ipkg\" :at (\"a\")) (module :gate \"abi.ipkg\" :at (\"b\")))",
            "(layout (module :gate \"a\" :at (\"a\"))) (layout (module :gate \"b\" :at (\"b\")))",
        ] {
            let deed = Deed::parse(&format!(
                "(repo-deed (rsr-profile :capabilities (\"abi\") {body}))"
            ))
            .unwrap();
            assert!(deed.rsr_profile().is_err(), "{body}");
        }
    }

    #[test]
    fn symbols_in_the_capability_lists_are_refused() {
        for text in [
            "(repo-deed (rsr-profile :capabilities (rust)))",
            "(repo-deed (rsr-profile :preset \"p\" :add (rust)))",
            "(repo-deed (rsr-profile :preset \"p\" :remove (rust)))",
        ] {
            let deed = Deed::parse(text).unwrap();
            assert!(deed.rsr_profile().is_err(), "{text}");
        }
    }
}
