// SPDX-License-Identifier: MPL-2.0
//! End-to-end scan against a mocked GitHub (ported from lib.test.js).

use std::collections::BTreeMap;
use std::time::Duration;

use anyhow::Result;
use idris_dependents::catalogue::Confidence;
use idris_dependents::scan::{run, Io, Options, Response};
use serde_json::json;

struct Mock {
    searches: BTreeMap<&'static str, Vec<serde_json::Value>>,
    contents: BTreeMap<&'static str, &'static str>,
    limited_once: bool,
    logs: Vec<String>,
    gets: usize,
}

fn item(full: &str, path: &str, url: &str) -> serde_json::Value {
    json!({
        "url": url,
        "html_url": format!("https://github.com/{full}/blob/main/{path}"),
        "repository": { "full_name": full, "owner": { "login": full.split('/').next().unwrap() } }
    })
}

fn decode(q: &str) -> String {
    let mut out = Vec::new();
    let b = q.as_bytes();
    let mut i = 0;
    while i < b.len() {
        if b[i] == b'%' {
            out.push(u8::from_str_radix(&q[i + 1..i + 3], 16).unwrap());
            i += 3;
        } else {
            out.push(b[i]);
            i += 1;
        }
    }
    String::from_utf8(out).unwrap()
}

impl Io for Mock {
    fn get(&mut self, url: &str, raw: bool) -> Result<Response> {
        self.gets += 1;
        let ok = |body: String| Response {
            status: 200,
            headers: BTreeMap::new(),
            body,
        };
        if url.contains("/search/code") {
            assert!(!raw);
            if !self.limited_once {
                self.limited_once = true;
                let headers = BTreeMap::from([("retry-after".to_string(), "1".to_string())]);
                return Ok(Response {
                    status: 429,
                    headers,
                    body: String::new(),
                });
            }
            let q = decode(url.split("q=").nth(1).unwrap().split('&').next().unwrap());
            let items = self.searches.get(q.as_str()).cloned().unwrap_or_default();
            return Ok(ok(
                json!({ "total_count": items.len(), "items": items }).to_string()
            ));
        }
        assert!(raw);
        Ok(ok(self.contents[url].to_string()))
    }
    fn sleep(&mut self, _: Duration) {}
    fn now_secs(&self) -> u64 {
        0
    }
    fn log(&mut self, msg: &str) {
        self.logs.push(msg.to_string());
    }
}

#[test]
fn run_filters_self_dedups_files_classifies_survives_rate_limit() {
    let mut io = Mock {
        searches: BTreeMap::from([
            (
                "proven extension:ipkg",
                vec![
                    item("alice/app", "app.ipkg", "c/alice"),
                    item("alice/app", "app.ipkg", "c/alice"), // duplicate file
                    item("bob/b", "b.ipkg", "c/bob"),
                    item("hyperpolymath/x", "x.ipkg", "c/self"),
                    item("dave/d", "d.ipkg", "c/dave"),
                    item("erin/e", "e.ipkg", "c/erin-crlf"),
                ],
            ),
            (
                "hyperpolymath filename:pack.toml",
                vec![item("carol/c", "pack.toml", "c/carol-pack")],
            ),
        ]),
        contents: BTreeMap::from([
            ("c/alice", "package app\ndepends = base, proven-dns"),
            ("c/bob", "package b\ndepends = proven"), // ambiguous -> LOW
            (
                "c/carol-pack",
                "[custom.all.ochrance]\nurl = \"https://github.com/hyperpolymath/ochrance\"",
            ),
            ("c/self", "package mine\ndepends = proven-dns"), // own account -> skipped
            ("c/dave", "package d\ndepends = base, contrib"), // matched search, no real dep
            (
                "c/erin-crlf",
                "package e\r\ndepends = base,\r\n  ochrance == 0.1.0\r\n",
            ), // P1+P2 in the wild
        ]),
        limited_once: false,
        logs: vec![],
        gets: 0,
    };
    let opts = Options {
        owners: vec!["hyperpolymath".into()],
        self_accounts: vec!["hyperpolymath".into()],
        packages: Some(vec![
            "proven".into(),
            "proven-dns".into(),
            "ochrance".into(),
            "proven-tests".into(),
        ]),
    };
    let res = run(&opts, &mut io).unwrap();
    let got: Vec<(&str, Confidence)> = res
        .rows
        .iter()
        .map(|r| (r.repo.as_str(), r.confidence))
        .collect();
    assert_eq!(
        got,
        vec![
            ("carol/c", Confidence::High),
            ("alice/app", Confidence::Medium),
            ("erin/e", Confidence::Medium),
            ("bob/b", Confidence::Low),
        ]
    );
    assert_eq!(
        res.rows
            .iter()
            .find(|r| r.repo == "alice/app")
            .unwrap()
            .files
            .len(),
        1
    );
    assert_eq!(res.catalogue_size, 3);
    assert!(io.logs.iter().any(|l| l.starts_with("rate-limited")));
    assert!(res.incomplete_queries.is_empty());
}

#[test]
fn truncated_search_is_reported_not_silent() {
    struct Big;
    impl Io for Big {
        fn get(&mut self, _: &str, _: bool) -> Result<Response> {
            let body = json!({ "total_count": 5000, "incomplete_results": false, "items": [] })
                .to_string();
            Ok(Response {
                status: 200,
                headers: BTreeMap::new(),
                body,
            })
        }
        fn sleep(&mut self, _: Duration) {}
        fn now_secs(&self) -> u64 {
            0
        }
        fn log(&mut self, _: &str) {}
    }
    let opts = Options {
        owners: vec!["o".into()],
        self_accounts: vec!["o".into()],
        packages: Some(vec!["ochrance".into()]),
    };
    let res = run(&opts, &mut Big).unwrap();
    assert_eq!(
        res.incomplete_queries,
        vec![
            "ochrance extension:ipkg",
            "o filename:pack.toml",
            "o filename:.gitmodules"
        ]
    );
}

#[test]
fn non_rate_limit_errors_fail_loudly() {
    struct Forbidden;
    impl Io for Forbidden {
        fn get(&mut self, _: &str, _: bool) -> Result<Response> {
            Ok(Response {
                status: 401,
                headers: BTreeMap::new(),
                body: String::new(),
            })
        }
        fn sleep(&mut self, _: Duration) {}
        fn now_secs(&self) -> u64 {
            0
        }
        fn log(&mut self, _: &str) {}
    }
    let opts = Options {
        owners: vec!["o".into()],
        self_accounts: vec![],
        packages: Some(vec!["ochrance".into()]),
    };
    let err = run(&opts, &mut Forbidden).unwrap_err().to_string();
    assert!(err.contains("HTTP 401"), "{err}");
}
