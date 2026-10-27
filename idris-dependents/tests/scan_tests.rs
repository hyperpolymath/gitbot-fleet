// SPDX-License-Identifier: MPL-2.0
//! End-to-end scan against a mocked GitHub (ported from lib.test.js).

use std::collections::BTreeMap;
use std::time::Duration;

use anyhow::Result;
use idris_dependents::catalogue::Confidence;
use idris_dependents::scan::{run, Io, Options, Response};
use serde_json::json;

#[derive(Default)]
struct Mock {
    searches: BTreeMap<&'static str, Vec<serde_json::Value>>,
    contents: BTreeMap<&'static str, &'static str>,
    responses: BTreeMap<&'static str, Response>,
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
        if let Some(res) = self.responses.get(url) {
            return Ok(Response {
                status: res.status,
                headers: res.headers.clone(),
                body: res.body.clone(),
            });
        }
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
                    item("alice/app", "other.ipkg", "c/alice-other"), // same dependency
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
            ("c/alice-other", "package other\ndepends = proven-dns"),
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
        responses: BTreeMap::new(),
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
    let alice = res.rows.iter().find(|r| r.repo == "alice/app").unwrap();
    assert_eq!(alice.deps, vec!["proven-dns"]);
    assert_eq!(
        alice.files,
        vec![
            "https://github.com/alice/app/blob/main/app.ipkg",
            "https://github.com/alice/app/blob/main/other.ipkg",
        ]
    );
    assert_eq!(res.catalogue_size, 3);
    assert!(io.logs.iter().any(|l| l.starts_with("rate-limited")));
    assert!(res.incomplete_queries.is_empty());
    assert!(res.skipped_urls.is_empty());
}

fn catalogue_scan() -> (Options, Mock) {
    let opts = Options {
        owners: vec!["o".into()],
        self_accounts: vec!["o".into()],
        packages: None,
    };
    let io = Mock {
        searches: BTreeMap::from([
            (
                "package extension:ipkg user:o",
                vec![
                    item("o/pkg", "gone.ipkg", "c/catalogue-gone"),
                    item("o/pkg", "pkg.ipkg", "c/catalogue"),
                ],
            ),
            (
                "ochrance extension:ipkg",
                vec![
                    item("alice/app", "gone.ipkg", "c/dependent-gone"),
                    item("alice/app", "app.ipkg", "c/dependent"),
                ],
            ),
        ]),
        contents: BTreeMap::from([
            ("c/catalogue-gone", "package ochrance"),
            ("c/catalogue", "package ochrance"),
            ("c/dependent-gone", "package gone\ndepends = ochrance"),
            ("c/dependent", "package app\ndepends = ochrance"),
        ]),
        ..Mock::default()
    };
    (opts, io)
}

fn response(status: u16) -> Response {
    Response {
        status,
        headers: BTreeMap::new(),
        body: String::new(),
    }
}

#[test]
fn unavailable_files_are_logged_reported_and_skipped_in_both_raw_fetch_paths() {
    for status in [404, 410, 451] {
        let (opts, mut io) = catalogue_scan();
        io.responses = BTreeMap::from([
            ("c/catalogue-gone", response(status)),
            ("c/dependent-gone", response(status)),
        ]);
        let report = run(&opts, &mut io).unwrap();
        assert_eq!(report.catalogue_size, 1);
        assert_eq!(report.rows.len(), 1);
        assert_eq!(report.rows[0].deps, vec!["ochrance"]);
        assert_eq!(
            report.rows[0].files,
            vec!["https://github.com/alice/app/blob/main/app.ipkg"]
        );
        assert_eq!(
            report.skipped_urls,
            vec!["c/catalogue-gone", "c/dependent-gone"]
        );
        for url in &report.skipped_urls {
            assert!(io
                .logs
                .iter()
                .any(|l| l.contains(url) && l.contains(&format!("HTTP {status}"))));
        }
        assert_eq!(
            serde_json::to_value(&report).unwrap()["skippedUrls"],
            json!(["c/catalogue-gone", "c/dependent-gone"])
        );
    }
}

#[test]
fn other_raw_errors_abort_in_both_fetch_paths() {
    for url in ["c/catalogue-gone", "c/dependent-gone"] {
        for status in [401, 403, 422, 429, 500, 503] {
            let (opts, mut io) = catalogue_scan();
            io.responses.insert(url, response(status));
            let err = run(&opts, &mut io).unwrap_err().to_string();
            assert!(err.contains(url), "{err}");
            if status == 429 {
                assert!(err.contains("still rate-limited"), "{err}");
            } else {
                assert!(err.contains(&format!("HTTP {status}")), "{err}");
            }
        }
        let (opts, mut io) = catalogue_scan();
        let mut limited = response(403);
        limited
            .headers
            .insert("x-ratelimit-remaining".into(), "0".into());
        io.responses.insert(url, limited);
        let err = run(&opts, &mut io).unwrap_err().to_string();
        assert!(
            err.contains("still rate-limited") && err.contains(url),
            "{err}"
        );
    }
}

#[test]
fn unavailable_search_responses_and_invalid_json_abort() {
    let url = "https://api.github.com/search/code?q=package%20extension%3Aipkg%20user%3Ao&per_page=100&page=1";
    for status in [404, 410, 451, 200] {
        let (opts, mut io) = catalogue_scan();
        io.responses.insert(url, response(status));
        let err = run(&opts, &mut io).unwrap_err().to_string();
        if status == 200 {
            assert!(err.contains("search response"), "{err}");
        } else {
            assert!(err.contains(&format!("HTTP {status}")), "{err}");
        }
        assert!(!io.logs.iter().any(|l| l.contains("skipping")));
    }
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

fn ready_mock() -> Mock {
    Mock {
        searches: BTreeMap::new(),
        contents: BTreeMap::new(),
        limited_once: true,
        logs: vec![],
        gets: 0,
    }
}

#[test]
fn catalogue_discovers_names_deduplicates_and_excludes_internal_packages() {
    let mut io = ready_mock();
    io.searches.insert(
        "package extension:ipkg user:hyperpolymath",
        vec![
            item("hyperpolymath/a", "a.ipkg", "catalogue/a"),
            item("hyperpolymath/b", "b.ipkg", "catalogue/b"),
            item("hyperpolymath/c", "c.ipkg", "catalogue/c"),
            item("hyperpolymath/d", "d.ipkg", "catalogue/d"),
        ],
    );
    io.searches.insert(
        "ochrance extension:ipkg",
        vec![item("alice/app", "app.ipkg", "external/app")],
    );
    io.contents = BTreeMap::from([
        ("catalogue/a", "package ochrance\r\ndepends = base"),
        ("catalogue/b", "package ochrance"), // same name in another repository
        ("catalogue/c", "package ochrance-tests"),
        (
            "catalogue/d",
            "-- package fake\nmodules = MissingDeclaration",
        ),
        ("external/app", "depends = base, ochrance-tests, ochrance"),
    ]);
    let report = run(
        &Options {
            owners: vec!["hyperpolymath".into()],
            self_accounts: vec!["hyperpolymath".into()],
            packages: None,
        },
        &mut io,
    )
    .unwrap();
    assert_eq!(report.catalogue_size, 1);
    assert_eq!(report.rows.len(), 1);
    assert_eq!(report.rows[0].repo, "alice/app");
    assert_eq!(report.rows[0].deps, vec!["ochrance"]);
    assert_eq!(report.rows[0].confidence, Confidence::Medium);
    // 1 catalogue query + 4 catalogue files + 3 dependent queries + 1 external file.
    assert_eq!(io.gets, 9);
}

#[test]
fn scan_merges_all_evidence_and_sorts_by_confidence_then_repository() {
    let mut io = ready_mock();
    let shared = item("zed/app", "one.ipkg", "content/one");
    io.searches = BTreeMap::from([
        (
            "ochrance extension:ipkg",
            vec![
                shared.clone(),
                item("zed/app", "two.ipkg", "content/two"),
                item("zed/app", "noise.ipkg", "content/noise"),
                item("carol/app", "app.ipkg", "content/carol"),
            ],
        ),
        (
            "proven extension:ipkg",
            vec![shared, item("bob/app", "app.ipkg", "content/bob")],
        ),
        (
            "hyperpolymath filename:pack.toml",
            vec![item("zed/app", "pack.toml", "content/pack")],
        ),
        (
            "hyperpolymath filename:.gitmodules",
            vec![
                item("zed/app", ".gitmodules", "content/sub"),
                item("anna/app", ".gitmodules", "content/anna-sub"),
            ],
        ),
    ]);
    io.contents = BTreeMap::from([
        (
            "content/one",
            "depends = proven-dns, ochrance, base, proven-tests",
        ),
        ("content/two", "depends = proven, ochrance, proven-dns"),
        (
            "content/noise",
            "-- depends = ochrance\ndepends = ochrance-extra, base",
        ),
        ("content/carol", "depends = ochrance"),
        ("content/bob", "depends = proven"),
        (
            "content/pack",
            "[custom.all.local-name]\nurl = 'https://github.com/hyperpolymath/proven'",
        ),
        (
            "content/sub",
            "url = git@github.com:hyperpolymath/ochrance.git",
        ),
        (
            "content/anna-sub",
            "url = https://github.com/hyperpolymath/proven.git",
        ),
    ]);
    let report = run(
        &Options {
            owners: vec!["hyperpolymath".into()],
            self_accounts: vec![],
            packages: Some(vec![
                "proven".into(),
                "proven-dns".into(),
                "ochrance".into(),
                "proven-tests".into(),
            ]),
        },
        &mut io,
    )
    .unwrap();
    assert_eq!(
        report
            .rows
            .iter()
            .map(|r| (r.repo.as_str(), r.confidence))
            .collect::<Vec<_>>(),
        vec![
            ("anna/app", Confidence::High),
            ("zed/app", Confidence::High),
            ("carol/app", Confidence::Medium),
            ("bob/app", Confidence::Low),
        ]
    );
    let zed = &report.rows[1];
    assert_eq!(zed.owner, "zed");
    assert_eq!(zed.deps, vec!["ochrance", "proven", "proven-dns"]);
    assert_eq!(
        zed.pack_refs,
        vec![idris_dependents::refs::PackRef {
            name: "local-name".into(),
            url: "https://github.com/hyperpolymath/proven".into(),
        }]
    );
    assert_eq!(
        zed.sub_refs,
        vec!["git@github.com:hyperpolymath/ochrance.git"]
    );
    assert_eq!(
        zed.files,
        vec![
            "https://github.com/zed/app/blob/main/one.ipkg",
            "https://github.com/zed/app/blob/main/two.ipkg",
            "https://github.com/zed/app/blob/main/pack.toml",
            "https://github.com/zed/app/blob/main/.gitmodules",
        ]
    );
    assert_eq!(report.catalogue_size, 3);
    assert_eq!(io.gets, 12); // 4 searches + 8 unique files, including the irrelevant file
}

#[test]
fn self_accounts_are_case_insensitive_exact_matches_and_skipped_before_fetch() {
    let mut io = ready_mock();
    io.searches.insert(
        "ochrance extension:ipkg",
        vec![
            item("HyperPolymath/private", "a.ipkg", "must-not-fetch/self"),
            item("ANOTHER/account", "a.ipkg", "must-not-fetch/alias"),
            item("hyperpolymath-community/app", "a.ipkg", "external/app"),
        ],
    );
    io.contents.insert("external/app", "depends = ochrance");
    let report = run(
        &Options {
            owners: vec![],
            self_accounts: vec!["hyperpolymath".into(), "another".into()],
            packages: Some(vec!["ochrance".into()]),
        },
        &mut io,
    )
    .unwrap();
    assert_eq!(report.rows.len(), 1);
    assert_eq!(report.rows[0].repo, "hyperpolymath-community/app");
    assert_eq!(io.gets, 2);
}

#[test]
fn malformed_pack_files_are_logged_without_losing_other_evidence() {
    let mut io = ready_mock();
    io.searches = BTreeMap::from([
        (
            "ochrance extension:ipkg",
            vec![item("alice/app", "a.ipkg", "content/dep")],
        ),
        (
            "hyperpolymath filename:pack.toml",
            vec![
                item("alice/app", "pack.toml", "content/bad"),
                item("bob/app", "pack.toml", "content/only-bad"),
                item("carol/app", "pack.toml", "content/unrelated"),
            ],
        ),
    ]);
    io.contents = BTreeMap::from([
        ("content/dep", "depends = ochrance"),
        ("content/bad", "[custom.invalid"),
        ("content/only-bad", "url = 'unfinished"),
        (
            "content/unrelated",
            "[custom.all.p]\nurl = 'https://github.com/other/ochrance'",
        ),
    ]);
    let report = run(
        &Options {
            owners: vec!["hyperpolymath".into()],
            self_accounts: vec![],
            packages: Some(vec!["ochrance".into()]),
        },
        &mut io,
    )
    .unwrap();
    assert_eq!(report.rows.len(), 1);
    let row = &report.rows[0];
    assert_eq!(row.repo, "alice/app");
    assert_eq!(row.confidence, Confidence::Medium);
    assert!(row.pack_refs.is_empty());
    assert_eq!(
        row.files,
        vec!["https://github.com/alice/app/blob/main/a.ipkg"]
    );
    let skips: Vec<_> = io
        .logs
        .iter()
        .filter(|s| s.contains("skipping unparseable pack.toml"))
        .collect();
    assert_eq!(skips.len(), 2);
    assert!(skips
        .iter()
        .any(|s| s.contains("https://github.com/alice/app/blob/main/pack.toml")));
    assert!(skips
        .iter()
        .any(|s| s.contains("https://github.com/bob/app/blob/main/pack.toml")));
}
