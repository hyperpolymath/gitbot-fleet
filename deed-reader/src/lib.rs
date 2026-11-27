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
//! - the whole input must be consumed;
//! - [`parse_deed`] (used by [`load_chora`]) also requires the SPDX header and a
//!   non-empty top-level form; [`parse`] is the same form grammar without those
//!   two document-level rules.
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
    /// An integer outside `i64` (deed.abnf `integer` is unbounded digits),
    /// kept as its token text, sign included, so it never invalidates a deed.
    BigInt(String),
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
            Value::Int(_) | Value::BigInt(_) => "an integer",
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
                // deed.abnf: estate_chora.deed is an estate-file, tested before repo-file.
                && stem != "estate"
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
    pick_chora(dir, entries.map(|e| e.map(|e| (e.file_name(), e.path()))))
}

/// The chora deed among directory `entries` (name, path). An entry that
/// failed to read is an error, never skipped: `read_dir` may fail while
/// advancing, and skipping would turn an unreadable or ambiguous deed set into
/// a false "none" or "one".
fn pick_chora(
    dir: &Path,
    entries: impl Iterator<Item = std::io::Result<(std::ffi::OsString, PathBuf)>>,
) -> std::result::Result<Option<PathBuf>, ChoraError> {
    let mut found: Vec<PathBuf> = Vec::new();
    for entry in entries {
        let (name, path) = entry.map_err(|e| ChoraError::Io(dir.to_path_buf(), e))?;
        if !name.to_str().is_some_and(is_chora_name) {
            continue;
        }
        // `Path::is_file` turns every metadata error into `false`, which would
        // drop an unreadable candidate silently. Only a dangling link
        // (`NotFound`) is intentionally ignored; any other error surfaces.
        match std::fs::metadata(&path) {
            Ok(m) if m.is_file() => found.push(path),
            Ok(_) => {}
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            Err(e) => return Err(ChoraError::Io(path, e)),
        }
    }
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
    let root = parse_deed(&text).map_err(|e| ChoraError::Parse(path.clone(), e))?;
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

/// How deeply clauses and lists may nest. Real deeds nest a handful of levels;
/// the limit exists so a hostile or broken deed is a parse error, not a stack
/// overflow, which aborts the whole process and cannot be caught.
pub const MAX_DEPTH: usize = 64;

/// Parse a deed's form: one top-level clause, nothing after it. Lexically
/// strict (comments, separators), but it does not demand the document-level
/// rules of [`parse_deed`], so a consumer reading a hand-written fixture or a
/// file the linter has yet to see is not refused for a missing header.
pub fn parse(text: &str) -> Result<Clause> {
    parse_with(text, false)
}

/// Parse a whole deed as deed.abnf defines it: the mandatory SPDX header
/// (`header = 1*spdx-line`), then one form with at least one field or clause,
/// nothing after it. This is what [`load_chora`] applies to a chora deed.
pub fn parse_deed(text: &str) -> Result<Clause> {
    parse_with(text, true)
}

fn parse_with(text: &str, document: bool) -> Result<Clause> {
    let mut p = Parser {
        s: text.chars().collect(),
        i: 0,
        line: 1,
        depth: 0,
    };
    if document {
        p.header()?;
    }
    p.skip_sep()?;
    if p.peek() != Some('(') {
        return Err(p.err("a deed starts with '('"));
    }
    let root = p.clause()?;
    // deed.abnf `form` is `"(" doc-head 1*(token-sep (field / clause))`: the
    // top-level form needs an item, though a nested clause may be empty.
    if document && root.items.is_empty() {
        return Err(DeedError {
            line: root.line,
            message: format!(
                "the top-level form '{}' needs at least one field or clause",
                root.head
            ),
        });
    }
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
    depth: usize,
}

/// deed.abnf `text-char`: printable ASCII, space, or any non-ASCII scalar.
/// Control characters (C0, DEL) are not text.
fn is_text_char(c: char) -> bool {
    !((c as u32) < 0x20 || c as u32 == 0x7f)
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

    /// Enter one level of nesting, refusing past [`MAX_DEPTH`].
    fn descend(&mut self) -> Result<()> {
        if self.depth >= MAX_DEPTH {
            return Err(self.err(&format!(
                "clauses and lists nest deeper than {MAX_DEPTH} levels"
            )));
        }
        self.depth += 1;
        Ok(())
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

    /// Consume one comment, from its `;` through its line end (deed.abnf
    /// `comment = ";" *text-char line-end`): the content is text characters
    /// only, the line end is LF or CRLF, and the end of the file is not one.
    /// Leaves the LF (or the CR of a CRLF) for the caller.
    fn comment(&mut self) -> Result<()> {
        self.bump(); // ';'
        loop {
            match self.peek() {
                None => {
                    return Err(self.err(
                        "a comment reaches the end of the file without a line ending (end it with LF or CRLF)",
                    ))
                }
                Some('\n') => return Ok(()),
                Some('\r') => {
                    if self.s.get(self.i + 1) != Some(&'\n') {
                        return Err(self.err("a bare CR is not a line end (use LF or CRLF)"));
                    }
                    return Ok(());
                }
                Some(c) if !is_text_char(c) => {
                    return Err(self.err("a control character in a comment"))
                }
                Some(_) => {
                    self.bump();
                }
            }
        }
    }

    /// The mandatory header (deed.abnf `header = 1*spdx-line`): at the very
    /// start, one or more `;; SPDX-...` lines, each ended by a line end.
    fn header(&mut self) -> Result<()> {
        let mut lines = 0;
        while self.starts_with(";; SPDX-") {
            // `1*text-char` after the prefix: it may not end the line.
            if matches!(self.s.get(self.i + 8), None | Some('\n' | '\r')) {
                return Err(self.err("an SPDX header line has no content after 'SPDX-'"));
            }
            self.comment()?;
            lines += 1;
            // comment() leaves the CR of a CRLF or the LF: take the line end.
            if self.peek() == Some('\r') {
                self.bump();
            }
            self.bump();
        }
        if lines == 0 {
            return Err(self.err(
                "a deed starts with an SPDX header: a line beginning ';; SPDX-' (deed.abnf header)",
            ));
        }
        Ok(())
    }

    fn starts_with(&self, lit: &str) -> bool {
        lit.chars()
            .enumerate()
            .all(|(k, c)| self.s.get(self.i + k) == Some(&c))
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
                    // A comment runs to the line end, which is LF or CRLF: a CR
                    // inside it that does not start a CRLF is a bare CR, an
                    // error here as everywhere else.
                    self.comment()?;
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
        self.descend()?;
        let clause = self.clause_body(line);
        self.depth -= 1;
        clause
    }

    fn clause_body(&mut self, line: usize) -> Result<Clause> {
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
        self.descend()?;
        let list = self.list_body(line);
        self.depth -= 1;
        list
    }

    fn list_body(&mut self, line: usize) -> Result<Value> {
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
        // deed.abnf `integer` is unbounded digits: past i64 keep the token.
        Ok(text.parse::<i64>().map_or(Value::BigInt(text), Value::Int))
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

    /// The fixtures below are bodies; every deed starts with the SPDX header
    /// (it is one line, so reported line numbers are one more than the body's).
    fn parse(body: &str) -> Result<Clause> {
        parse_deed(&format!(";; SPDX-License-Identifier: MPL-2.0\n{body}"))
    }

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

    /// `depth` clauses nested inside one another: `(a (a (a)))`.
    fn nested_clauses(depth: usize) -> String {
        "(a".to_string() + &" (a".repeat(depth - 1) + &")".repeat(depth)
    }

    #[test]
    fn deep_nesting_is_a_parse_error_not_a_stack_overflow() {
        // Far deeper than any stack: without the limit this aborts the test
        // process instead of failing it.
        let e = parse(&nested_clauses(100_000)).unwrap_err();
        assert!(e.message.contains("nest deeper than 64"), "{e}");
        let lists = format!("(a :v {}{})", "(".repeat(100_000), ")".repeat(100_000));
        let e = parse(&lists).unwrap_err();
        assert!(e.message.contains("nest deeper than 64"), "{e}");
        let quoted = format!("(a :v '{}{})", "(".repeat(100_000), ")".repeat(100_000));
        assert!(parse(&quoted).is_err());
    }

    #[test]
    fn a_bare_cr_inside_a_comment_is_an_error_but_crlf_and_lf_end_one() {
        let e = parse("(repo-deed :x y ; comment\rstill comment\n)").unwrap_err();
        assert!(e.message.contains("bare CR"), "{e}");
        assert_eq!(e.line, 2);
        assert!(parse("(repo-deed ; comment\r\n :x y)").is_ok());
        assert!(parse("(repo-deed ; comment\n :x y)").is_ok());
        assert!(parse("(repo-deed :x y)\r\n").is_ok());
    }

    #[test]
    fn nesting_up_to_the_limit_still_reads() {
        assert!(parse(&nested_clauses(MAX_DEPTH)).is_ok());
        assert!(parse(&nested_clauses(MAX_DEPTH + 1)).is_err());
        // The root clause is one level, so a list may nest MAX_DEPTH - 1 deep.
        let n = MAX_DEPTH - 1;
        let list = format!("(a :v {}{})", "(".repeat(n), ")".repeat(n));
        assert!(parse(&list).is_ok());
        let list = format!("(a :v {}{})", "(".repeat(n + 1), ")".repeat(n + 1));
        assert!(parse(&list).is_err());
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
            ("empty top-level form", "(a)"),
        ] {
            assert!(parse(text).is_err(), "{label} should not parse");
        }
        let e = parse("(a\n\n :b c\n :d").unwrap_err();
        assert!(e.line >= 4, "{e}");
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

        std::fs::write(d.join("s_chora.deed"), "(repo-deed :x y)").unwrap();
        assert!(matches!(find_chora(&d), Err(ChoraError::Multiple(v)) if v.len() == 2));

        let d = tmp("bad");
        std::fs::write(d.join("r_chora.deed"), format!("{SPDX}(repo-deed\n :x")).unwrap();
        assert!(matches!(load_chora(&d), Err(ChoraError::Parse(_, e)) if e.line == 2 + 1));

        let d = tmp("head");
        std::fs::write(
            d.join("r_chora.deed"),
            format!("{SPDX}(praxis-deed :x \"y\")"),
        )
        .unwrap();
        let e = load_chora(&d).unwrap_err();
        assert!(e.to_string().contains("must be a repo-deed"), "{e}");
    }

    // --- Grammar strictness (review of #689) -------------------------------

    const SPDX: &str = ";; SPDX-License-Identifier: MPL-2.0\n";

    #[test]
    fn estate_chora_deed_is_not_a_repo_deed_name() {
        // deed.abnf: estate-file is tested before repo-file; stem "estate" is excluded.
        assert!(!is_chora_name("estate_chora.deed"));
        assert!(is_chora_name("estate2_chora.deed"));
        assert!(is_chora_name("my-estate_chora.deed"));
        let d = tmp("estate");
        std::fs::write(
            d.join("estate_chora.deed"),
            format!("{SPDX}(estate-deed :schema-version \"1.0.0\")\n"),
        )
        .unwrap();
        assert!(find_chora(&d).unwrap().is_none());
        std::fs::write(
            d.join("r_chora.deed"),
            format!("{SPDX}(repo-deed :schema-version \"1.0.0\")\n"),
        )
        .unwrap();
        let c = load_chora(&d).unwrap().unwrap();
        assert!(c.path.ends_with("r_chora.deed"));
    }

    #[test]
    fn a_comment_must_end_in_a_line_end() {
        let ok = format!("{SPDX}(repo-deed :schema-version \"1.0.0\")\n; trailing\n");
        assert!(parse_deed(&ok).is_ok());
        let e = parse_deed(&format!(
            "{SPDX}(repo-deed :schema-version \"1.0.0\")\n; trailing"
        ))
        .unwrap_err();
        assert!(e.message.contains("end of the file"), "{e}");
        // An SPDX line is a comment too and needs its line end.
        assert!(parse_deed(";; SPDX-License-Identifier: MPL-2.0").is_err());
    }

    #[test]
    fn a_comment_holds_text_chars_only() {
        let doc = |c: &str| format!("{SPDX}(repo-deed :schema-version \"1.0.0\" ; {c}\n)\n");
        assert!(parse_deed(&doc("plain ascii and utf-8: \u{a9} \u{4e2d}")).is_ok());
        for bad in ["bad\tcomment", "del\u{7f}", "nul\u{0}", "esc\u{1b}"] {
            let e = parse_deed(&doc(bad)).unwrap_err();
            assert!(e.message.contains("control character"), "{bad:?}: {e}");
        }
    }

    #[test]
    fn a_top_level_form_needs_at_least_one_item_but_a_nested_clause_may_be_empty() {
        let e = parse_deed(&format!("{SPDX}(repo-deed)\n")).unwrap_err();
        assert!(e.message.contains("at least one"), "{e}");
        assert!(parse_deed(&format!("{SPDX}(repo-deed ; only a comment\n)\n")).is_err());
        assert!(parse_deed(&format!("{SPDX}(repo-deed (status))\n")).is_ok());
    }

    #[test]
    fn plain_parse_is_the_form_only_but_lexically_strict() {
        assert!(super::parse("(repo-deed)").is_ok());
        assert!(super::parse("(repo-deed :a b)\n; no line end").is_err());
        assert!(super::parse("(repo-deed :a b ; tab\there\n)").is_err());
    }

    #[test]
    fn the_spdx_header_is_mandatory() {
        let body = "(repo-deed :schema-version \"1.0.0\")\n";
        let e = parse_deed(body).unwrap_err();
        assert!(e.message.contains("SPDX"), "{e}");
        assert!(parse_deed(&format!("; just a note\n{body}")).is_err());
        assert!(parse_deed(&format!(";; not spdx\n{body}")).is_err());
        assert!(parse_deed(&format!("\n{SPDX}{body}")).is_err());
        assert!(parse_deed(&format!("{SPDX}{body}")).is_ok());
        assert!(parse_deed(&format!("{SPDX}{SPDX}; more\n{body}")).is_ok());
        assert!(parse_deed(&format!(";; SPDX-License-Identifier: MPL-2.0\r\n{body}")).is_ok());
        assert!(parse_deed(&format!(";; SPDX-\n{body}")).is_err());
    }

    #[test]
    fn an_integer_beyond_i64_is_kept_as_its_digits() {
        let c = parse_deed(&format!(
            "{SPDX}(repo-deed :schema-version \"1.0.0\" :big 123456789012345678901234567890 :neg -99999999999999999999 :n 7)\n"
        ))
        .unwrap();
        assert_eq!(
            c.field("big"),
            Some(&Value::BigInt("123456789012345678901234567890".into()))
        );
        assert_eq!(
            c.field("neg"),
            Some(&Value::BigInt("-99999999999999999999".into()))
        );
        assert_eq!(c.field("n"), Some(&Value::Int(7)));
        assert_eq!(
            c.field("big")
                .unwrap()
                .as_symbol("x", 1)
                .unwrap_err()
                .message,
            "x must be a symbol, found an integer"
        );
    }

    #[test]
    fn an_error_while_iterating_the_directory_is_an_error_not_a_skipped_entry() {
        let ok = |n: &str| Ok((std::ffi::OsString::from(n), PathBuf::from(n)));
        let boom = || Err(std::io::Error::other("transient"));
        let none: Vec<std::io::Result<(std::ffi::OsString, PathBuf)>> = vec![ok("a.txt")];
        assert!(pick_chora(Path::new("d"), none.into_iter())
            .unwrap()
            .is_none());
        let failed = vec![ok("r_chora.deed"), boom()];
        assert!(matches!(
            pick_chora(Path::new("d"), failed.into_iter()),
            Err(ChoraError::Io(p, _)) if p == Path::new("d")
        ));
    }

    #[test]
    fn a_metadata_error_on_a_candidate_is_an_error_not_a_skipped_entry() {
        let tmp = std::env::temp_dir().join(format!("deed-meta-{}", std::process::id()));
        std::fs::create_dir_all(&tmp).unwrap();
        let plain = tmp.join("plain");
        std::fs::write(&plain, "x").unwrap();
        let good = tmp.join("a_chora.deed");
        std::fs::write(&good, "x").unwrap();
        // A path "inside" a regular file: metadata fails with an error that
        // is not NotFound, so it must surface instead of being skipped.
        let bad = plain.join("b_chora.deed");
        let entries = vec![
            Ok((std::ffi::OsString::from("a_chora.deed"), good)),
            Ok((std::ffi::OsString::from("b_chora.deed"), bad)),
        ];
        assert!(matches!(
            pick_chora(&tmp, entries.into_iter()),
            Err(ChoraError::Io(p, _)) if p.ends_with("b_chora.deed")
        ));
        // A dangling path is still ignored.
        let dangling = vec![Ok((
            std::ffi::OsString::from("c_chora.deed"),
            tmp.join("missing_chora.deed"),
        ))];
        assert!(pick_chora(&tmp, dangling.into_iter()).unwrap().is_none());
        std::fs::remove_dir_all(&tmp).unwrap();
    }
}
