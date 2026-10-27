// SPDX-License-Identifier: MPL-2.0
//! `.ipkg` reading, modelled on Idris2's own package lexer and parser
//! (`src/Parser/Lexer/Package.idr`, `src/Parser/Lexer/Common.idr` and
//! `src/Idris/Package.idr` on Idris2 `main`).
//!
//! The lexer follows Idris2's token rules so that, on every file Idris2
//! accepts, the dependency list agrees with `idris2 --dump-ipkg-json`
//! (checked by `tests/oracle.rs`). It is deliberately *lenient* where Idris2
//! would reject the whole file: an unexpected character is skipped and an
//! unterminated comment or string runs to end of input. A search tool should
//! still see the `depends` of a file that has one stray character.

/// One token of an `.ipkg` file. Comments and whitespace are dropped.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tok {
    /// An identifier; `dotted` when it has `.`-separated segments (a module
    /// name such as `Proven.SafeMath`), which can never be a package name.
    Ident {
        text: String,
        dotted: bool,
    },
    Equals,
    Sep,
    Dot,
    Lte,
    Gte,
    Lt,
    Gt,
    EqOp,
    AndOp,
    Str(String),
    Int(String),
}

/// Whether a character can start an identifier: ASCII letters, `_`, or any
/// character above U+00A0.
fn is_ident_start(c: char) -> bool {
    c == '_' || c.is_ascii_alphabetic() || c > '\u{a0}'
}

/// Whether a character can follow an identifier's first character, including
/// ASCII digits, hyphens, and apostrophes.
fn is_ident_trailing(c: char) -> bool {
    c == '-' || c == '\'' || c == '_' || c.is_ascii_alphanumeric() || c > '\u{a0}'
}

/// Tokenize `src`. Returns the tokens and the number of characters skipped
/// because no Idris2 token rule matches them.
/// Comments and whitespace are omitted. An unterminated block comment consumes
/// the remaining input; an unterminated string's opening quote is counted as
/// skipped, and tokenization continues after it. String contents retain escapes.
pub fn lex(src: &str) -> (Vec<Tok>, usize) {
    let s: Vec<char> = src.chars().collect();
    let at = |j: usize| s.get(j).copied();
    let mut toks = Vec::new();
    let mut skipped = 0;
    let mut i = 0;
    while let Some(c) = at(i) {
        // Line comment: two or more dashes not followed by `}`.
        if c == '-' && at(i + 1) == Some('-') {
            let mut j = i + 2;
            while at(j) == Some('-') {
                j += 1;
            }
            if at(j) != Some('}') {
                while at(j).is_some_and(|c| c != '\n') {
                    j += 1;
                }
                i = j;
                continue;
            }
        }
        if c == '{' && at(i + 1) == Some('-') {
            i = block_comment(&s, i);
            continue;
        }
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        if is_ident_start(c) {
            let mut j = i + 1;
            while at(j).is_some_and(is_ident_trailing) {
                j += 1;
            }
            let mut dotted = false;
            while at(j) == Some('.') && at(j + 1).is_some_and(is_ident_start) {
                dotted = true;
                j += 2;
                while at(j).is_some_and(is_ident_trailing) {
                    j += 1;
                }
            }
            toks.push(Tok::Ident {
                text: s[i..j].iter().collect(),
                dotted,
            });
            i = j;
            continue;
        }
        if c.is_ascii_digit() {
            let mut j = i + 1;
            while at(j).is_some_and(|c| c.is_ascii_digit()) {
                j += 1;
            }
            toks.push(Tok::Int(s[i..j].iter().collect()));
            i = j;
            continue;
        }
        if c == '"' {
            if let Some(end) = string_end(&s, i) {
                toks.push(Tok::Str(s[i + 1..end - 1].iter().collect()));
                i = end;
            } else {
                skipped += 1;
                i += 1;
            }
            continue;
        }
        let (tok, len) = match (c, at(i + 1)) {
            ('<', Some('=')) => (Tok::Lte, 2),
            ('>', Some('=')) => (Tok::Gte, 2),
            ('=', Some('=')) => (Tok::EqOp, 2),
            ('&', Some('&')) => (Tok::AndOp, 2),
            ('<', _) => (Tok::Lt, 1),
            ('>', _) => (Tok::Gt, 1),
            ('=', _) => (Tok::Equals, 1),
            (',', _) => (Tok::Sep, 1),
            ('.', _) => (Tok::Dot, 1),
            _ => {
                skipped += 1;
                i += 1;
                continue;
            }
        };
        toks.push(tok);
        i += len;
    }
    (toks, skipped)
}

/// Index just past the closing quote of the string literal opening at `i`.
/// Backslash escapes skip the next character; returns `None` if no closing
/// quote is found. Indices count characters, not UTF-8 bytes.
fn string_end(s: &[char], i: usize) -> Option<usize> {
    let mut j = i + 1;
    while j < s.len() {
        match s[j] {
            '\\' => j += 2,
            '"' => return Some(j + 1),
            _ => j += 1,
        }
    }
    None
}

/// Skip a block comment opening at `i` (`{-`). Mirrors Idris2's
/// `toEndComment` / `singleBrace` / `singleDash` / `doubleDash` automaton:
/// comments nest, string and character literals inside them are skipped,
/// and `--` inside a comment hides the rest of its line (including any `-}`).
/// Returns the character index after the closing delimiter, or `s.len()` if
/// the comment is unterminated.
fn block_comment(s: &[char], i: usize) -> usize {
    let at = |j: usize| s.get(j).copied();
    let mut j = i + 2;
    while at(j) == Some('-') {
        j += 1;
    }
    let mut depth = 1usize;
    // `dash`: the previous character was a dash that may start `-}` or `--`.
    let mut dash = false;
    loop {
        let Some(c) = at(j) else { return s.len() };
        if dash {
            dash = false;
            match c {
                '-' => {
                    while at(j) == Some('-') {
                        j += 1;
                    }
                    if at(j) == Some('}') {
                        j += 1;
                        depth -= 1;
                    } else {
                        while at(j).is_some_and(|c| c != '\n') {
                            j += 1;
                        }
                    }
                }
                '}' => {
                    j += 1;
                    depth -= 1;
                }
                _ => {}
            }
            if depth == 0 {
                return j;
            }
            continue;
        }
        match c {
            '{' if at(j + 1) == Some('-') => {
                j += 2;
                while at(j) == Some('-') {
                    j += 1;
                }
                depth += 1;
                dash = true;
            }
            '-' => {
                j += 1;
                dash = true;
            }
            '"' => j = string_end(s, j).unwrap_or(j + 1),
            '\'' => {
                j += match (at(j + 1), at(j + 2), at(j + 3)) {
                    (Some('\\'), Some(_), Some('\'')) => 4,
                    (Some(x), Some('\''), _) if x != '\'' => 3,
                    _ => 1,
                }
            }
            _ => j += 1,
        }
    }
}

/// Borrow the text of an undotted identifier, or return `None` for any other
/// token or missing token.
fn ident(t: Option<&Tok>) -> Option<&str> {
    match t {
        Some(Tok::Ident {
            text,
            dotted: false,
        }) => Some(text),
        _ => None,
    }
}

/// Package name from the `package <name>` declaration, or `None`.
pub fn package_name(text: &str) -> Option<String> {
    let (toks, _) = lex(text);
    (0..toks.len()).find_map(|i| {
        match (ident(toks.get(i)), ident(toks.get(i + 1)), toks.get(i + 2)) {
            (Some("package"), Some(name), next) if next != Some(&Tok::Equals) => {
                Some(name.to_string())
            }
            _ => None,
        }
    })
}

/// Package names in every `depends =` field, in order, without duplicates.
///
/// A field key is an identifier followed by a single `=`; version bounds use
/// `==`, `>=` and friends, which lex as different tokens, so a bound can
/// never be mistaken for the start of the next field.
pub fn depends(text: &str) -> Vec<String> {
    let (toks, _) = lex(text);
    let mut out: Vec<String> = Vec::new();
    let mut i = 0;
    while i + 1 < toks.len() {
        if ident(toks.get(i)) != Some("depends") || toks[i + 1] != Tok::Equals {
            i += 1;
            continue;
        }
        let mut j = i + 2;
        while let Some(name) = ident(toks.get(j)) {
            if toks.get(j + 1) == Some(&Tok::Equals) {
                break; // a trailing comma ran into the next field's key
            }
            if !out.iter().any(|d| d == name) {
                out.push(name.to_string());
            }
            j += 1;
            while matches!(
                toks.get(j),
                Some(
                    Tok::Lt
                        | Tok::Lte
                        | Tok::Gt
                        | Tok::Gte
                        | Tok::EqOp
                        | Tok::AndOp
                        | Tok::Int(_)
                        | Tok::Dot
                )
            ) {
                j += 1;
            }
            if toks.get(j) != Some(&Tok::Sep) {
                break;
            }
            j += 1;
        }
        i = j.max(i + 1);
    }
    out
}
