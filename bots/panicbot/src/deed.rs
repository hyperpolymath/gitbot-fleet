// SPDX-License-Identifier: MPL-2.0
//! A small, strict reader for the repo deed (`<repo>_chora.deed`).
//!
//! No other crate in gitbot-fleet reads deeds (checked 2026-11-13: no
//! `chora`/`deed` reader in shared-context or any bot), so this module reads
//! just enough of the grammar (`standards/1-formats/deed/spec/abnf/deed.abnf`)
//! to find one clause and its fields:
//!
//! - `(` is the only bracket; `;` starts a comment that runs to the line end;
//! - a clause is `(head item...)`, an item is a `:keyword value` field or a
//!   nested clause, separated by spaces or line ends (a tab is an error);
//! - values: `"strings"` (escapes `\"` `\\` `\n` `\t` only), bare symbols,
//!   integers, `#t` / `#f`, the `#u5"..."` / `#u8"..."` / `#uuid"..."`
//!   literals, `'quoted` symbols and lists, and `( lists )`;
//! - the whole input must be consumed.
//!
//! There is one spelling of a bot entry: `(bot :name panicbot ...)`. A bare
//! symbol is not a field or a clause, so the grammar rejects `(bot panicbot ...)`
//! and so does this reader, with an error naming the canonical spelling.
//!
//! This is a reader, not the validator: `deed_lint` in standards stays the
//! conformance gate (header, `:schema-version`, line length, UUID shape). The
//! reader's job is to never guess: every malformed input is an `Err` with a
//! line number, never a silent default.

use std::fmt;

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
    /// consumed. A tab is an error.
    fn skip_sep(&mut self) -> Result<bool> {
        let start = self.i;
        loop {
            match self.peek() {
                Some(' ' | '\n') => {
                    self.bump();
                }
                Some('\r') => {
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
}
