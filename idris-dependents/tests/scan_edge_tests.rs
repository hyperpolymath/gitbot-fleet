// SPDX-License-Identifier: MPL-2.0
//! Deterministic scan boundaries: requests, pagination and failure handling.

use std::collections::VecDeque;
use std::time::Duration;

use anyhow::{anyhow, Result};
use idris_dependents::catalogue::Confidence;
use idris_dependents::scan::{run, Io, Options, Response, API, SEARCH_GAP};
use serde_json::{json, Value};

const QUERY: &str = "ochrance%20extension%3Aipkg";

struct Request {
    url: String,
    raw: bool,
    response: Result<Response>,
}

#[derive(Default)]
struct Script {
    requests: VecDeque<Request>,
    sleeps: Vec<Duration>,
    logs: Vec<String>,
}

impl Script {
    fn respond(&mut self, url: &str, raw: bool, response: Result<Response>) {
        self.requests.push_back(Request {
            url: url.into(),
            raw,
            response,
        });
    }

    // Queries are encoded explicitly by callers, independently of the production encoder.
    fn search(&mut self, query: &str, page: u32, total: u64, incomplete: bool, items: Vec<Value>) {
        self.respond(
            &search_url(query, page),
            false,
            Ok(response(
                200,
                &[],
                &json!({
                    "total_count": total, "incomplete_results": incomplete, "items": items,
                })
                .to_string(),
            )),
        );
    }

    fn raw(&mut self, url: &str, text: &str) {
        self.respond(url, true, Ok(response(200, &[], text)));
    }

    fn done(&self) {
        assert!(
            self.requests.is_empty(),
            "{} requests not made",
            self.requests.len()
        );
    }
}

impl Io for Script {
    fn get(&mut self, url: &str, raw: bool) -> Result<Response> {
        let request = self.requests.pop_front().expect("unexpected GET");
        assert_eq!(url, request.url);
        assert_eq!(raw, request.raw, "wrong response format for {url}");
        request.response
    }

    fn sleep(&mut self, duration: Duration) {
        self.sleeps.push(duration);
    }

    fn now_secs(&self) -> u64 {
        100
    }

    fn log(&mut self, message: &str) {
        self.logs.push(message.into());
    }
}

fn search_url(query: &str, page: u32) -> String {
    format!("{API}/search/code?q={query}&per_page=100&page={page}")
}

fn response(status: u16, headers: &[(&str, &str)], body: &str) -> Response {
    Response {
        status,
        headers: headers
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect(),
        body: body.into(),
    }
}

fn options() -> Options {
    Options {
        owners: vec![],
        self_accounts: vec![],
        packages: Some(vec!["ochrance".into()]),
    }
}

fn item(repo: &str, path: &str, url: &str) -> Value {
    json!({
        "url": url,
        "html_url": format!("https://github.com/{repo}/blob/main/{path}"),
        "repository": { "full_name": repo, "owner": { "login": repo.split('/').next().unwrap() } },
    })
}

#[test]
fn pagination_fetches_the_second_page_and_reads_each_file_once() {
    let mut io = Script::default();
    let first = item("alice/app", "a.ipkg", "contents/a");
    let last = item("bob/app", "b.ipkg", "contents/b");
    io.search(QUERY, 1, 101, false, vec![first; 100]);
    io.search(QUERY, 2, 101, false, vec![last]);
    io.raw("contents/a", "package a\ndepends = ochrance");
    io.raw("contents/b", "package b\ndepends = ochrance");

    let report = run(&options(), &mut io).unwrap();
    assert_eq!(
        report
            .rows
            .iter()
            .map(|r| r.repo.as_str())
            .collect::<Vec<_>>(),
        vec!["alice/app", "bob/app"]
    );
    assert!(report.rows.iter().all(|r| r.files.len() == 1));
    assert!(report.incomplete_queries.is_empty());
    assert_eq!(io.sleeps, vec![SEARCH_GAP; 2]);
    io.done();
}

#[test]
fn a_full_page_stops_when_total_count_is_reached() {
    let mut io = Script::default();
    io.search(
        QUERY,
        1,
        100,
        false,
        vec![item("alice/app", "a.ipkg", "contents/a"); 100],
    );
    io.raw("contents/a", "depends = ochrance");
    let report = run(&options(), &mut io).unwrap();
    assert_eq!(report.rows.len(), 1);
    assert!(report.incomplete_queries.is_empty());
    assert_eq!(io.sleeps, vec![SEARCH_GAP]);
    io.done();
}

#[test]
fn pagination_caps_at_ten_pages_and_only_totals_above_1000_are_truncated() {
    for total in [1000, 1001] {
        let mut io = Script::default();
        for page in 1..=10 {
            io.search(
                QUERY,
                page,
                total,
                false,
                vec![item("alice/app", "a.ipkg", "contents/a"); 100],
            );
        }
        io.raw("contents/a", "depends = ochrance");
        let report = run(&options(), &mut io).unwrap();
        assert_eq!(report.rows.len(), 1);
        assert_eq!(
            report.incomplete_queries,
            if total > 1000 {
                vec!["ochrance extension:ipkg"]
            } else {
                vec![]
            }
        );
        assert_eq!(io.sleeps, vec![SEARCH_GAP; 10]);
        io.done();
    }
}

#[test]
fn incomplete_flag_on_later_pages_is_reported_once_even_with_truncation() {
    for total in [201, 1001] {
        let mut io = Script::default();
        let hit = item("alice/app", "a.ipkg", "contents/a");
        io.search(QUERY, 1, total, false, vec![hit.clone(); 100]);
        io.search(QUERY, 2, total, true, vec![hit.clone(); 100]);
        io.search(QUERY, 3, total, true, vec![hit]);
        io.raw("contents/a", "depends = ochrance");
        let report = run(&options(), &mut io).unwrap();
        assert_eq!(report.incomplete_queries, vec!["ochrance extension:ipkg"]);
        assert_eq!(report.rows.len(), 1);
        io.done();
    }
}

#[test]
fn incomplete_empty_search_still_appears_in_the_report() {
    let mut io = Script::default();
    io.search(QUERY, 1, 0, true, vec![]);
    let report = run(&options(), &mut io).unwrap();
    assert!(report.rows.is_empty());
    assert_eq!(report.incomplete_queries, vec!["ochrance extension:ipkg"]);
    io.done();
}

#[test]
fn rate_limit_waits_use_retry_after_then_reset_then_fallback() {
    let cases = [
        (429, vec![], 60),
        (
            429,
            vec![("retry-after", "7"), ("x-ratelimit-reset", "999")],
            7,
        ),
        (
            403,
            vec![("x-ratelimit-remaining", "0"), ("x-ratelimit-reset", "120")],
            21,
        ),
        (
            403,
            vec![("retry-after", "0"), ("x-ratelimit-reset", "90")],
            2,
        ),
        (
            403,
            vec![("retry-after", "bad"), ("x-ratelimit-reset", "100")],
            2,
        ),
        (
            429,
            vec![("retry-after", "bad"), ("x-ratelimit-reset", "bad")],
            60,
        ),
        (403, vec![("retry-after", "1")], 1),
    ];
    for (status, headers, wait) in cases {
        let mut io = Script::default();
        io.respond(
            &search_url(QUERY, 1),
            false,
            Ok(response(status, &headers, "")),
        );
        io.search(QUERY, 1, 0, false, vec![]);
        assert!(run(&options(), &mut io).unwrap().rows.is_empty());
        assert_eq!(
            io.sleeps,
            vec![Duration::from_secs(wait), SEARCH_GAP],
            "status {status}, headers {headers:?}"
        );
        assert!(io
            .logs
            .iter()
            .any(|s| s == &format!("rate-limited; waiting {wait}s")));
        io.done();
    }
}

#[test]
fn persistent_rate_limits_stop_after_five_retries() {
    let mut io = Script::default();
    for _ in 0..6 {
        io.respond(
            &search_url(QUERY, 1),
            false,
            Ok(response(429, &[("retry-after", "1")], "")),
        );
    }
    let error = run(&options(), &mut io).unwrap_err().to_string();
    assert!(
        error.contains("still rate-limited after 5 retries"),
        "{error}"
    );
    assert!(error.contains(&search_url(QUERY, 1)), "{error}");
    io.done();
}

#[test]
fn hard_http_errors_are_not_retried_even_with_retry_headers() {
    for status in [401, 403, 404, 500] {
        let mut io = Script::default();
        // Reset alone does not make a 403 a rate-limit response.
        io.respond(
            &search_url(QUERY, 1),
            false,
            Ok(response(status, &[("x-ratelimit-reset", "120")], "")),
        );
        let error = run(&options(), &mut io).unwrap_err().to_string();
        assert!(error.contains(&format!("HTTP {status}")), "{error}");
        assert!(error.contains(&search_url(QUERY, 1)), "{error}");
        assert!(io.sleeps.is_empty());
        io.done();
    }
}

#[test]
fn transport_errors_are_propagated_without_retry() {
    let mut io = Script::default();
    io.respond(
        &search_url(QUERY, 1),
        false,
        Err(anyhow!("synthetic connection failure")),
    );
    let error = run(&options(), &mut io).unwrap_err().to_string();
    assert!(error.contains("synthetic connection failure"), "{error}");
    assert!(io.sleeps.is_empty());
    io.done();
}

#[test]
fn malformed_search_json_identifies_the_query() {
    let mut io = Script::default();
    io.respond(
        &search_url(QUERY, 1),
        false,
        Ok(response(200, &[], "not JSON")),
    );
    let error = run(&options(), &mut io).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("search response for ochrance extension:ipkg"),
        "{error:#}"
    );
    assert!(io.sleeps.is_empty());
    io.done();
}

#[test]
fn search_hits_missing_required_fields_are_skipped() {
    let valid = item("alice/app", "a.ipkg", "contents/a");
    let mut items = Vec::new();
    for pointer in [
        "/url",
        "/html_url",
        "/repository/full_name",
        "/repository/owner/login",
    ] {
        let mut broken = valid.clone();
        *broken.pointer_mut(pointer).unwrap() = Value::Null;
        items.push(broken);
    }
    items.push(valid);
    let mut io = Script::default();
    io.search(QUERY, 1, items.len() as u64, false, items);
    io.raw("contents/a", "depends = ochrance");
    let report = run(&options(), &mut io).unwrap();
    assert_eq!(report.rows.len(), 1);
    assert_eq!(report.rows[0].repo, "alice/app");
    io.done();
}

#[test]
fn raw_content_fetches_retry_rate_limits_and_propagate_hard_failures() {
    let mut io = Script::default();
    io.search(
        QUERY,
        1,
        1,
        false,
        vec![item("alice/app", "a.ipkg", "contents/a")],
    );
    io.respond(
        "contents/a",
        true,
        Ok(response(429, &[("retry-after", "3")], "")),
    );
    io.raw("contents/a", "depends = ochrance");
    assert_eq!(run(&options(), &mut io).unwrap().rows.len(), 1);
    assert_eq!(io.sleeps, vec![SEARCH_GAP, Duration::from_secs(3)]);
    io.done();

    let mut io = Script::default();
    io.search(
        QUERY,
        1,
        1,
        false,
        vec![item("alice/app", "a.ipkg", "contents/a")],
    );
    io.respond("contents/a", true, Ok(response(404, &[], "")));
    let error = run(&options(), &mut io).unwrap_err().to_string();
    assert!(error.contains("HTTP 404 for contents/a"), "{error}");
    io.done();
}

#[test]
fn queries_percent_encode_unicode_and_reserved_characters() {
    let mut io = Script::default();
    let mut opts = options();
    opts.packages = Some(vec!["café+~_".into()]);
    io.search("caf%C3%A9%2B~_%20extension%3Aipkg", 1, 0, false, vec![]);
    assert!(run(&opts, &mut io).unwrap().rows.is_empty());
    io.done();
}

#[test]
fn explicit_empty_catalogue_makes_no_requests_when_no_owners_are_watched() {
    let mut io = Script::default();
    let mut opts = options();
    opts.packages = Some(vec![]);
    let report = run(&opts, &mut io).unwrap();
    assert_eq!(report.catalogue_size, 0);
    assert!(report.rows.is_empty());
    assert!(report.incomplete_queries.is_empty());
    assert!(io.sleeps.is_empty());
    io.done();
}

#[test]
fn report_json_keeps_public_field_names_and_confidence_spelling() {
    let mut io = Script::default();
    io.search(
        QUERY,
        1,
        1,
        false,
        vec![item("alice/app", "a.ipkg", "contents/a")],
    );
    io.raw("contents/a", "depends = ochrance");
    let report = run(&options(), &mut io).unwrap();
    assert_eq!(report.rows[0].confidence, Confidence::Medium);
    assert_eq!(
        serde_json::to_value(&report).unwrap(),
        json!({
            "catalogueSize": 1,
            "rows": [{
                "owner": "alice", "repo": "alice/app", "deps": ["ochrance"],
                "packRefs": [], "subRefs": [],
                "files": ["https://github.com/alice/app/blob/main/a.ipkg"],
                "confidence": "MEDIUM",
            }],
            "incompleteQueries": [],
        })
    );
    io.done();
}
