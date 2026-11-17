// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! A small, strict, read-only reader for the repo deed (`<repo>_chora.deed`).
//!
//! Repo facts that used to live in the retired STATE/META/ECOSYSTEM/... `.scm`
//! and `.a2ml` files are clauses of the one chora deed at the repository root
//! (owner rulings D312, D313). Bots here only READ deeds; they never write,
//! extend or hand-convert one.
//!
//! The grammar is `metadatastician/standards` `1-formats/deed/spec/abnf/deed.abnf`:
//!
//! - `(` is the only bracket; `;` starts a comment that runs to the line end;
//! - a clause is `(head item...)`, an item is a `:keyword value` field or a
//!   nested clause, separated by spaces, line ends (LF or CRLF) or comments
//!   (a tab, or a CR not followed by LF, is an error);
//! - values: `"strings"` (escapes `\"` `\\` `\n` `\t` only), bare symbols,
//!   integers, `#t` / `#f`, the `#u5"..."` / `#u8"..."` / `#uuid"..."`
//!   literals, `'quoted` symbols and lists, and `( lists )`;
//! - the whole input must be consumed.
//!
//! Lookup follows the conventions of the estate's shell reader
//! (`scripts/deed-field.sh` in the RSR template and kyX): [`find_chora`] is
//! `--find` (exactly one `*_chora.deed` in the directory), a clause PATH is
//! slash-separated below the root (`status`, `agentic/integrity`), and a KEY is
//! matched only at the top level of that clause, never inside a nested one.
//!
//! This is a reader, not the validator: `deed_lint.js` in standards stays the
//! conformance gate (header, `:schema-version`, line length, UUID shape,
//! filename dispatch). The reader's job is to never guess: every malformed
//! input is an `Err` with a line number, never a silent default.
//!
//! Extracted from panicbot's `src/deed.rs` (2026-11-13) so that finishingbot
//! and glambot read the deed the same way. panicbot's `deed` module now
//! re-exports this crate; rhodibot keeps its own reader until #687 lands.

use std::fmt;
use std::path::{Path, PathBuf};

/// One parenthesised clause.
#[derive(Debug, Clone, PartialEq)]
pub struct Clause {
    /// The symbol after `(`.
    pub head: String,
    /// Fields and nested clauses, in order.
    pub items: Vec<Item>,
    /// 1-based line of the `(`.
    pub line: usize,
}

/// An item inside a clause.
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    /// `:key value`.
    Field {
        key: String,
        value: Value,
        line: usize,
    },
    /// A nested clause.
    Clause(Clause),
}

/// A field value.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Str(String),
    Sym(String),
    Int(i64),
    Bool(bool),
    /// `#u5"..."`, `#u8"..."` or `#uuid"..."`: carried as its text, unused here.
    Literal(String),
    Quoted(Box<Value>),
    List(Vec<Value>),
}

impl Value {
    fn describe(&self) -> &'static str {
        match self {
            Value::Str(_) => "a string",
            Value::Sym(_) => "a symbol",
            Value::Int(_) => "an integer",
            Value::Bool(_) => "a boolean",
            Value::Literal(_) => "a literal",
            Value::Quoted(_) => "a quoted value",
            Value::List(_) => "a list",
        }
    }
}

/// A deed parse or shape error, with the 1-based line it was found on.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeedError {
    pub line: usize,
    pub message: String,
}

impl fmt::Display for DeedError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line {}: {}", self.line, self.message)
    }
}

impl std::error::Error for DeedError {}

/// Shorthand for results in this module.
pub type Result<T> = std::result::Result<T, DeedError>;

impl Clause {
    /// The nested clauses with this head.
    pub fn clauses<'a>(&'a self, head: &'a str) -> impl Iterator<Item = &'a Clause> + 'a {
        self.items.iter().filter_map(move |i| match i {
            Item::Clause(c) if c.head == head => Some(c),
            _ => None,
        })
    }

    /// The value of `:key` at the top level of this clause (never inside a
    /// nested clause), as `deed-field.sh DEED PATH KEY` reads it. The first
    /// occurrence wins.
    pub fn field(&self, key: &str) -> Option<&Value> {
        self.items.iter().find_map(|i| match i {
            Item::Field { key: k, value, .. } if k == key => Some(value),
            _ => None,
        })
    }

    /// The clause at a slash-separated `path` below this one (`status`,
    /// `agentic/integrity`), taking the first clause with each head. An empty
    /// path is this clause itself, as in `deed-field.sh`.
    pub fn clause_at(&self, path: &str) -> Option<&Clause> {
        let mut at = self;
        for head in path.split('/').filter(|s| !s.is_empty()) {
            at = at.items.iter().find_map(|i| match i {
                Item::Clause(c) if c.head == head => Some(c),
                _ => None,
            })?;
        }
        Some(at)
    }

    /// Whether the clause at `path` exists (`deed-field.sh --has`).
    pub fn has(&self, path: &str) -> bool {
        self.clause_at(path).is_some()
    }

    /// The heads of the top-level clauses, in order.
    pub fn clause_heads(&self) -> Vec<&str> {
        self.items
            .iter()
            .filter_map(|i| match i {
                Item::Clause(c) => Some(c.head.as_str()),
                _ => None,
            })
            .collect()
    }
}

/// The suffix that names a repo deed (D312: one per repository, at its root).
pub const CHORA_SUFFIX: &str = "_chora.deed";

/// Why the chora deed of a directory could not be found or read.
#[derive(Debug)]
pub enum ChoraError {
    /// More than one `*_chora.deed` at the root: D312 allows exactly one.
    Multiple(Vec<PathBuf>),
    /// The file exists but could not be read.
    Io(PathBuf, std::io::Error),
    /// The file is not a deed this reader can parse.
    Parse(PathBuf, DeedError),
    /// It parses, but its doc-head is not `repo-deed` (deed.abnf filename
    /// dispatch: `*_chora.deed` is a `repo-deed`).
    WrongHead(PathBuf, String),
}

impl fmt::Display for ChoraError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ChoraError::Multiple(ps) => {
                let names: Vec<String> = ps.iter().map(|p| p.display().to_string()).collect();
                write!(
                    f,
                    "more than one chora deed (D312 allows exactly one): {}",
                    names.join(", ")
                )
            }
            ChoraError::Io(p, e) => write!(f, "{}: {e}", p.display()),
            ChoraError::Parse(p, e) => write!(f, "{}: {e}", p.display()),
            ChoraError::WrongHead(p, h) => write!(
                f,
                "{}: doc-head is '{h}', but a *_chora.deed must be a repo-deed",
                p.display()
            ),
        }
    }
}

impl std::error::Error for ChoraError {}

/// True when `name` is a chora deed file name: a non-empty stem of ASCII
/// letters, digits, `-`, `.` or `_` (deed.abnf `stem`), then `_chora.deed`.
pub fn is_chora_name(name: &str) -> bool {
    match name.strip_suffix(CHORA_SUFFIX) {
        Some(stem) => {
            !stem.is_empty()
                && stem
                    .chars()
                    .all(|c| c.is_ascii_alphanumeric() || "-._".contains(c))
        }
        None => false,
    }
}

/// The one `*_chora.deed` directly in `dir` (`deed-field.sh --find`):
/// `Ok(None)` when there is none, `Err(Multiple)` when there are several.
/// Only regular files count; a directory or a dangling link with the name
/// does not.
pub fn find_chora(dir: &Path) -> std::result::Result<Option<PathBuf>, ChoraError> {
    let entries = match std::fs::read_dir(dir) {
        Ok(e) => e,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(e) => return Err(ChoraError::Io(dir.to_path_buf(), e)),
    };
    let mut found: Vec<PathBuf> = entries
        .filter_map(|e| e.ok())
        .filter(|e| e.file_name().to_str().is_some_and(is_chora_name))
        .map(|e| e.path())
        .filter(|p| p.is_file())
        .collect();
    found.sort();
    match found.len() {
        0 => Ok(None),
        1 => Ok(found.pop()),
        _ => Err(ChoraError::Multiple(found)),
    }
}

/// A parsed chora deed and where it was read from.
#[derive(Debug, Clone)]
pub struct Chora {
    pub path: PathBuf,
    pub root: Clause,
}

/// Find, read and parse the chora deed of the repository at `dir`.
/// `Ok(None)` when the repository has none.
pub fn load_chora(dir: &Path) -> std::result::Result<Option<Chora>, ChoraError> {
    let Some(path) = find_chora(dir)? else {
        return Ok(None);
    };
    let text = std::fs::read_to_string(&path).map_err(|e| ChoraError::Io(path.clone(), e))?;
    let root = parse(&text).map_err(|e| ChoraError::Parse(path.clone(), e))?;
    if root.head != "repo-deed" {
        let head = root.head.clone();
        return Err(ChoraError::WrongHead(path, head));
    }
    Ok(Some(Chora { path, root }))
}

impl Value {
    /// The value as a bare symbol, or an error naming `what`.
    pub fn as_symbol(&self, what: &str, line: usize) -> Result<&str> {
        match self {
            Value::Sym(s) => Ok(s),
            other => Err(DeedError {
                line,
                message: format!("{what} must be a symbol, found {}", other.describe()),
            }),
        }
    }

    /// The value as a string, or an error naming `what`.
    pub fn as_string(&self, what: &str, line: usize) -> Result<&str> {
        match self {
            Value::Str(s) => Ok(s),
            other => Err(DeedError {
                line,
                message: format!("{what} must be a string, found {}", other.describe()),
            }),
        }
    }

    /// The value as a list of bare symbols, or an error naming `what`.
    pub fn as_symbol_list(&self, what: &str, line: usize) -> Result<Vec<&str>> {
        match self {
            Value::List(vs) => vs.iter().map(|v| v.as_symbol(what, line)).collect(),
            other => Err(DeedError {
                line,
                message: format!("{what} must be a list, found {}", other.describe()),
            }),
        }
    }
}

/// Parse a whole deed: one top-level clause, nothing after it.
pub fn parse(text: &str) -> Result<Clause> {
    let mut p = Parser {
        s: text.chars().collect(),
        i: 0,
        line: 1,
    };
    p.skip_sep()?;
    if p.peek() != Some('(') {
        return Err(p.err("a deed starts with '('"));
    }
    let root = p.clause()?;
    p.skip_sep()?;
    if p.peek().is_some() {
        return Err(p.err("trailing content after the closing ')'"));
    }
    Ok(root)
}

struct Parser {
    s: Vec<char>,
    i: usize,
    line: usize,
}

fn is_sym_start(c: char) -> bool {
    c.is_ascii_alphabetic()
}

fn is_sym_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || ".*/<>=!?+-_".contains(c)
}

impl Parser {
    fn err(&self, message: &str) -> DeedError {
        DeedError {
            line: self.line,
            message: message.to_string(),
        }
    }

    fn peek(&self) -> Option<char> {
        self.s.get(self.i).copied()
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.i += 1;
        if c == '\n' {
            self.line += 1;
        }
        Some(c)
    }

    /// Skip spaces, line ends and comments. Returns whether anything was
    /// consumed. A tab, or a CR that does not start a CRLF, is an error
    /// (deed.abnf `line-end = CRLF / LF`).
    fn skip_sep(&mut self) -> Result<bool> {
        let start = self.i;
        loop {
            match self.peek() {
                Some(' ' | '\n') => {
                    self.bump();
                }
                Some('\r') => {
                    if self.s.get(self.i + 1) != Some(&'\n') {
                        return Err(self.err("a bare CR is not a line end (use LF or CRLF)"));
                    }
                    self.bump();
                }
                Some('\t') => return Err(self.err("a tab is not a valid separator (use spaces)")),
                Some(';') => {
                    while let Some(c) = self.peek() {
                        if c == '\n' {
                            break;
                        }
                        self.bump();
                    }
                }
                _ => break,
            }
        }
        Ok(self.i > start)
    }

    fn symbol(&mut self) -> Result<String> {
        let mut out = String::new();
        match self.peek() {
            Some(c) if is_sym_start(c) => {
                out.push(c);
                self.bump();
            }
            _ => return Err(self.err("expected a symbol")),
        }
        while let Some(c) = self.peek() {
            if is_sym_char(c) {
                out.push(c);
                self.bump();
            } else {
                break;
            }
        }
        Ok(out)
    }

    fn clause(&mut self) -> Result<Clause> {
        let line = self.line;
        self.bump(); // '('
        let head = self.symbol()?;
        let mut items = Vec::new();
        loop {
            let sep = self.skip_sep()?;
            match self.peek() {
                None => {
                    return Err(DeedError {
                        line,
                        message: format!("clause '{head}' is never closed"),
                    })
                }
                Some(')') => {
                    self.bump();
                    return Ok(Clause { head, items, line });
                }
                Some(_) if !sep => return Err(self.err("expected a space between items")),
                Some('(') => items.push(Item::Clause(self.clause()?)),
                Some(':') => {
                    let line = self.line;
                    self.bump();
                    let key = self.symbol()?;
                    if !self.skip_sep()? {
                        return Err(self.err("expected a space between a keyword and its value"));
                    }
                    let value = self.value()?;
                    items.push(Item::Field { key, value, line });
                }
                Some(c) if head == "bot" && is_sym_start(c) => {
                    let name = self.symbol()?;
                    return Err(self.err(&format!(
                        "bare symbol '{name}' in a (bot) clause: the canonical spelling is (bot :name {name} ...)"
                    )));
                }
                Some(c) => {
                    return Err(self.err(&format!(
                        "unexpected '{c}' in clause '{head}' (expected a :field or a clause)"
                    )))
                }
            }
        }
    }

    fn value(&mut self) -> Result<Value> {
        match self.peek() {
            None => Err(self.err("expected a value, found the end of the file")),
            Some('"') => Ok(Value::Str(self.string()?)),
            Some('(') => self.list(),
            Some('\'') => {
                self.bump();
                match self.peek() {
                    Some('(') => Ok(Value::Quoted(Box::new(self.list()?))),
                    Some(c) if is_sym_start(c) => {
                        Ok(Value::Quoted(Box::new(Value::Sym(self.symbol()?))))
                    }
                    _ => Err(self.err("only a symbol or a list can be quoted")),
                }
            }
            Some('#') => self.hash_literal(),
            Some(c) if c.is_ascii_digit() || c == '-' => self.integer(),
            Some(c) if is_sym_start(c) => Ok(Value::Sym(self.symbol()?)),
            Some(c) => Err(self.err(&format!("unexpected '{c}' where a value was expected"))),
        }
    }

    fn list(&mut self) -> Result<Value> {
        let line = self.line;
        self.bump(); // '('
        let mut out = Vec::new();
        loop {
            let sep = self.skip_sep()?;
            match self.peek() {
                None => {
                    return Err(DeedError {
                        line,
                        message: "list is never closed".into(),
                    })
                }
                Some(')') => {
                    self.bump();
                    return Ok(Value::List(out));
                }
                Some(_) if !sep && !out.is_empty() => {
                    return Err(self.err("expected a space between list values"))
                }
                Some(_) => out.push(self.value()?),
            }
        }
    }

    fn string(&mut self) -> Result<String> {
        self.bump(); // opening quote
        let mut out = String::new();
        loop {
            match self.bump() {
                None => return Err(self.err("string is never closed")),
                Some('"') => return Ok(out),
                Some('\\') => match self.bump() {
                    Some('"') => out.push('"'),
                    Some('\\') => out.push('\\'),
                    Some('n') => out.push('\n'),
                    Some('t') => out.push('\t'),
                    _ => {
                        return Err(self.err("invalid escape (only \\\" \\\\ \\n \\t are allowed)"))
                    }
                },
                Some(c) if (c as u32) < 0x20 || c as u32 == 0x7f => {
                    return Err(self.err("a control character in a string"))
                }
                Some(c) => out.push(c),
            }
        }
    }

    fn integer(&mut self) -> Result<Value> {
        let mut text = String::new();
        if self.peek() == Some('-') {
            text.push('-');
            self.bump();
        }
        let digits_from = text.len();
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                text.push(c);
                self.bump();
            } else {
                break;
            }
        }
        if text.len() == digits_from {
            return Err(self.err("expected digits after '-'"));
        }
        if self.peek().is_some_and(is_sym_char) {
            return Err(self.err("an integer must end at a separator"));
        }
        text.parse::<i64>()
            .map(Value::Int)
            .map_err(|_| self.err("integer out of range"))
    }

    fn hash_literal(&mut self) -> Result<Value> {
        self.bump(); // '#'
        let mut tag = String::new();
        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() {
                tag.push(c);
                self.bump();
            } else {
                break;
            }
        }
        match tag.as_str() {
            "t" | "f" => {
                if self.peek().is_some_and(is_sym_char) {
                    return Err(self.err("a boolean must end at a separator"));
                }
                Ok(Value::Bool(tag == "t"))
            }
            "u5" | "u8" | "uuid" => {
                if self.peek() != Some('"') {
                    return Err(self.err(&format!("#{tag} must be followed by a string")));
                }
                Ok(Value::Literal(format!("#{tag}\"{}\"", self.string()?)))
            }
            _ => Err(self.err(&format!("unknown literal '#{tag}'"))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reads_fields_clauses_comments_and_every_value_kind() {
        let c = parse(
            ";; SPDX-License-Identifier: MPL-2.0\n; note\n(repo-deed\n  :schema-version \"1.0.0\" ; tail\n  :id #u5\"a/b\"\n  :n -12\n  :on #t\n  :off #f\n  :sym thing.v1\n  :q 'sym\n  :ql '(a b)\n  :l (\n    a \"b\" 3)\n  (inner :k \"a\\\"b\\\\c\\nd\\te\"))\n",
        )
        .unwrap();
        assert_eq!(c.head, "repo-deed");
        let inner = c.clauses("inner").next().unwrap();
        match &inner.items[0] {
            Item::Field { value, .. } => assert_eq!(value, &Value::Str("a\"b\\c\nd\te".into())),
            other => panic!("{other:?}"),
        }
        assert_eq!(c.items.len(), 10);
    }

    #[test]
    fn a_bare_symbol_is_an_error_everywhere_and_a_bot_clause_names_the_canonical_form() {
        assert!(parse("(x (bot :name panicbot :a b))").is_ok());
        let e = parse("(x (bot panicbot :a b))").unwrap_err();
        assert!(e.message.contains("(bot :name panicbot ...)"), "{e}");
        assert!(parse("(x (other panicbot))").is_err());
    }

    #[test]
    fn malformed_input_is_an_error_with_a_line() {
        for (label, text) in [
            ("unclosed", "(a\n :b c"),
            ("trailing", "(a) (b)"),
            ("tab", "(a\n\t:b c)"),
            ("bad escape", "(a :b \"\\u0041\")"),
            ("unclosed string", "(a :b \"x)"),
            ("no value", "(a :b)"),
            ("no space", "(a :b c(d))"),
            ("toml", "key = \"v\""),
            ("bare in clause", "(a b)"),
            ("bad literal", "(a :b #x)"),
            ("bad bool", "(a :b #true)"),
            ("quoted string", "(a :b '\"x\")"),
            ("empty", ""),
        ] {
            assert!(parse(text).is_err(), "{label} should not parse");
        }
        let e = parse("(a\n\n :b c\n :d").unwrap_err();
        assert!(e.line >= 3, "{e}");
    }

    #[test]
    fn a_bare_cr_is_an_error_and_crlf_is_a_line_end() {
        assert!(parse("(a\r\n :b c)\r\n").is_ok());
        let e = parse("(a\r :b c)").unwrap_err();
        assert!(e.message.contains("bare CR"), "{e}");
    }

    #[test]
    fn field_reads_the_top_level_only_and_clause_at_walks_a_path() {
        let c = parse(
            "(repo-deed :canonical-name \"x\"\n (status :phase alpha (history (entry :phase old)))\n (agentic (integrity :level 2)))",
        )
        .unwrap();
        assert_eq!(c.field("canonical-name"), Some(&Value::Str("x".into())));
        let status = c.clause_at("status").unwrap();
        assert_eq!(status.field("phase"), Some(&Value::Sym("alpha".into())));
        // A nested clause's field is never the clause's own.
        assert_eq!(c.clause_at("status/history").unwrap().field("phase"), None);
        assert_eq!(
            c.clause_at("agentic/integrity").unwrap().field("level"),
            Some(&Value::Int(2))
        );
        assert!(c.has("agentic") && !c.has("meta") && !c.has("agentic/missing"));
        assert_eq!(c.clause_at("").unwrap().head, "repo-deed");
        assert_eq!(c.clause_heads(), vec!["status", "agentic"]);
    }

    #[test]
    fn chora_names_follow_the_abnf_stem() {
        for ok in [
            "gitbot-fleet_chora.deed",
            "a.b_c_chora.deed",
            "x1_chora.deed",
        ] {
            assert!(is_chora_name(ok), "{ok}");
        }
        for bad in [
            "_chora.deed",
            "x_chora.deed.bak",
            "x y_chora.deed",
            "STATE.deed",
            "x_praxis.deed",
        ] {
            assert!(!is_chora_name(bad), "{bad}");
        }
    }

    fn tmp(tag: &str) -> PathBuf {
        let d = std::env::temp_dir().join(format!("deed-reader-{tag}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&d);
        std::fs::create_dir_all(&d).unwrap();
        d
    }

    #[test]
    fn find_and_load_chora_cover_none_one_many_bad_and_wrong_head() {
        let d = tmp("none");
        assert!(find_chora(&d).unwrap().is_none());
        assert!(load_chora(&d).unwrap().is_none());
        // A directory with the name does not count.
        std::fs::create_dir_all(d.join("dir_chora.deed")).unwrap();
        assert!(find_chora(&d).unwrap().is_none());

        let d = tmp("one");
        std::fs::write(d.join("r_chora.deed"), ";; SPDX-License-Identifier: MPL-2.0\n(repo-deed :canonical-name \"r\" (status :phase beta))\n").unwrap();
        let c = load_chora(&d).unwrap().unwrap();
        assert!(c.path.ends_with("r_chora.deed") && c.root.has("status"));

        std::fs::write(d.join("s_chora.deed"), "(repo-deed)").unwrap();
        assert!(matches!(find_chora(&d), Err(ChoraError::Multiple(v)) if v.len() == 2));

        let d = tmp("bad");
        std::fs::write(d.join("r_chora.deed"), "(repo-deed\n :x").unwrap();
        assert!(matches!(load_chora(&d), Err(ChoraError::Parse(_, e)) if e.line == 2));

        let d = tmp("head");
        std::fs::write(d.join("r_chora.deed"), "(praxis-deed :x \"y\")").unwrap();
        let e = load_chora(&d).unwrap_err();
        assert!(e.to_string().contains("must be a repo-deed"), "{e}");
    }
}
