// SPDX-License-Identifier: MPL-2.0
//! Random `.ipkg` files that Idris2 itself accepts, with their expected
//! `depends`. Shared by the round-trip property test and the Idris2 oracle.
//!
//! Only well-formed files are generated (one `depends` field, at most one
//! lower and one upper bound per dependency, as `mkBound` in Idris2 requires),
//! so the oracle never rejects a case for a reason unrelated to the parser.

#![allow(dead_code)]

use proptest::prelude::*;

#[derive(Debug, Clone)]
pub struct Ipkg {
    pub text: String,
    pub name: String,
    pub deps: Vec<String>,
}

fn pkg_name() -> impl Strategy<Value = String> {
    "[a-z][a-z0-9_']{0,6}(-[a-z0-9]{1,4}){0,2}"
}

fn version() -> impl Strategy<Value = String> {
    prop::collection::vec(0u32..20, 1..4)
        .prop_map(|v| v.iter().map(u32::to_string).collect::<Vec<_>>().join("."))
}

fn bounds() -> impl Strategy<Value = String> {
    prop_oneof![
        Just(String::new()),
        (prop::sample::select(vec![">", ">="]), version()).prop_map(|(o, v)| format!(" {o} {v}")),
        (prop::sample::select(vec!["<", "<="]), version()).prop_map(|(o, v)| format!(" {o} {v}")),
        (version(), version()).prop_map(|(a, b)| format!(" >= {a} && < {b}")),
        (version(), version()).prop_map(|(a, b)| format!(" <= {b} && > {a}")),
        version().prop_map(|v| format!(" == {v}")),
    ]
}

/// Line ending: LF or CRLF.
fn nl() -> impl Strategy<Value = &'static str> {
    prop::sample::select(vec!["\n", "\r\n"])
}

/// Text that is safe inside a string or a line comment and full of
/// comment-like noise (`{-`, `-}`, `depends = fake`).
fn noisy_text() -> impl Strategy<Value = String> {
    prop::collection::vec(
        prop::sample::select(vec![
            "a",
            " ",
            "{-",
            "-}",
            "--",
            "depends = fake",
            ",",
            "x1",
            "}",
        ]),
        0..6,
    )
    .prop_map(|v| v.concat())
}

/// A block comment, possibly nested, containing strings and line comments
/// whose `-}` must not close it.
fn block_comment() -> impl Strategy<Value = String> {
    let leaf = prop_oneof![
        "[a-z ,=.]{0,8}".prop_map(|s| s),
        noisy_text().prop_map(|t| format!("\"{t}\"")),
        (noisy_text(), nl()).prop_map(|(t, n)| format!("-- {t}{n}")),
    ];
    leaf.prop_recursive(3, 12, 4, |inner| {
        prop::collection::vec(inner, 0..4).prop_map(|segs| format!("{{- {} -}}", segs.join(" ")))
    })
    .prop_map(|s| {
        if s.starts_with("{-") {
            s
        } else {
            format!("{{- {s} -}}")
        }
    })
}

/// Whitespace, optionally carrying a comment, between two tokens.
fn gap() -> impl Strategy<Value = String> {
    prop_oneof![
        4 => prop::sample::select(vec![" ", "  ", "\t", " \n  ", " \r\n\t"]).prop_map(String::from),
        1 => (noisy_text(), nl()).prop_map(|(t, n)| format!(" -- {t}{n}  ")),
        1 => block_comment().prop_map(|c| format!(" {c} ")),
    ]
}

fn module_name() -> impl Strategy<Value = String> {
    "[A-Z][a-z]{0,5}(\\.[A-Z][a-z]{0,5}){0,2}"
}

pub fn ipkg() -> impl Strategy<Value = Ipkg> {
    (
        pkg_name(),
        prop::collection::vec((pkg_name(), bounds()), 0..6),
        any::<bool>(), // leading-comma style
        prop::collection::vec(gap(), 16),
        prop::collection::vec(module_name(), 1..4),
        noisy_text(),
        nl(),
        any::<bool>(), // depends before or after the other fields
    )
        .prop_map(
            |(name, raw_deps, leading, gaps, modules, opts, nl, deps_first)| {
                let mut deps: Vec<(String, String)> = Vec::new();
                for (d, b) in raw_deps {
                    if !deps.iter().any(|(x, _)| *x == d) {
                        deps.push((d, b));
                    }
                }
                let g = |i: usize| gaps[i % gaps.len()].clone();
                let mut depends_field = String::new();
                if !deps.is_empty() {
                    depends_field.push_str(&format!("depends{}={}", g(0), g(1)));
                    for (i, (d, b)) in deps.iter().enumerate() {
                        if i > 0 {
                            if leading {
                                depends_field.push_str(&format!("{nl}{}, ", g(i + 2)));
                            } else {
                                depends_field.push_str(&format!(",{}", g(i + 2)));
                            }
                        }
                        depends_field.push_str(&format!("{d}{b}"));
                    }
                    depends_field.push_str(nl);
                }
                let others = format!(
                "version = 1.2.3{nl}opts = \"{opts}\"{nl}modules ={}{}{nl}sourcedir = \"src\"{nl}",
                g(10),
                modules.join(&format!("{},{}", g(11), g(12)))
            );
                let body = if deps_first {
                    format!("{depends_field}{others}")
                } else {
                    format!("{others}{depends_field}")
                };
                let text = format!(
                    "{}package {name}{}{nl}{body}{}",
                    g(13).trim_start(),
                    g(14),
                    g(15)
                );
                Ipkg {
                    text,
                    name,
                    deps: deps.into_iter().map(|(d, _)| d).collect(),
                }
            },
        )
}
