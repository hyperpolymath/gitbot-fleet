// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! Boundary regressions for the API credential and peer-budget contracts.
use echidnabot::api::auth::{
    is_loopback_host, ApiCredentials, CredentialError, Principal, RepoScope,
    ADMIN_TOKEN_ENV, MIN_TOKEN_LEN, ROUTE_TOKEN_ENV,
};
use echidnabot::api::rate_limit::WebhookRateLimiter;
use echidnabot::config::{ApiConfig, Config};
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr};
use std::sync::{Arc, Barrier};

#[test]
fn minimum_credential_length_is_checked_after_trimming_for_each_role() {
    for admin in [false, true] {
        for len in [MIN_TOKEN_LEN - 1, MIN_TOKEN_LEN, MIN_TOKEN_LEN + 1] {
            let token = Some(format!(" \n{}\t ", "x".repeat(len)));
            let (route, admin_token) = if admin { (None, token) } else { (token, None) };
            let result = ApiCredentials::new(route, admin_token, RepoScope::default());
            if len < MIN_TOKEN_LEN {
                assert_eq!(result.unwrap_err(), CredentialError::TooShort(
                    if admin { ADMIN_TOKEN_ENV } else { ROUTE_TOKEN_ENV }
                ));
            } else {
                let credentials = result.unwrap();
                assert!(!credentials.is_empty());
                assert_eq!(credentials.has_route_token(), !admin);
                let principal = credentials.authenticate(Some(&format!("BeArEr {}", "x".repeat(len))));
                assert_eq!(principal, Some(if admin { Principal::Admin } else {
                    Principal::Route(RepoScope::default())
                }));
            }
        }
    }
}

#[test]
fn equal_tokens_cannot_gain_distinct_roles_through_whitespace() {
    let token = "x".repeat(MIN_TOKEN_LEN);
    assert_eq!(ApiCredentials::new(Some(format!(" {token}\n")), Some(token),
        RepoScope::default()).unwrap_err(), CredentialError::NotDistinct);
}

#[test]
fn near_matches_and_header_suffixes_do_not_authenticate() {
    let token = "x".repeat(MIN_TOKEN_LEN);
    let credentials = ApiCredentials::new(None, Some(token.clone()), RepoScope::default()).unwrap();
    for candidate in [
        format!("Bearer {}y", &token[..MIN_TOKEN_LEN - 1]),
        format!("Bearer y{}", &token[1..]),
        format!("Bearer {}", &token[..MIN_TOKEN_LEN - 1]),
        format!("Bearer {token}x"),
        format!("Bearer {token}, Bearer {token}"),
        format!("Bearer {token} extra"),
        format!("Bearer\t{token}"),
    ] {
        assert_eq!(credentials.authenticate(Some(&candidate)), None, "{candidate}");
    }
    assert_eq!(credentials.authenticate(Some(&format!(" \tBeArEr  {token}\r\n"))),
        Some(Principal::Admin));
}

#[test]
fn route_scope_is_preserved_without_granting_admin_privileges() {
    let scope = RepoScope::new(["team/one", "another/*"]);
    let token = "r".repeat(MIN_TOKEN_LEN);
    let credentials = ApiCredentials::new(Some(token.clone()), None, scope.clone()).unwrap();
    assert_eq!(credentials.route_scope(), &scope);
    assert_eq!(credentials.authenticate(Some(&format!("Bearer {token}"))),
        Some(Principal::Route(scope)));
}

#[test]
fn scope_does_not_treat_partial_wildcards_or_owner_prefixes_as_patterns() {
    let scope = RepoScope::new(["team/*", "other/repo", "partial/re*", "invalid", "/*"]);
    for repo in ["team/repo", "other/repo"] {
        assert!(scope.allows(repo), "{repo}");
    }
    for repo in ["team-extra/repo", "Team/repo", "team", "team/", "/repo",
        "team/repo/child", "other/repository", "partial/repo"] {
        assert!(!scope.allows(repo), "{repo}");
    }
}

#[test]
fn remote_or_mapped_addresses_do_not_count_as_loopback() {
    for host in ["::ffff:127.0.0.1", "localhost.example.org", "127.0.0.1:8080",
        "[::1]:8080", " 127.0.0.1", "0.0.0.0", "::"] {
        assert!(!is_loopback_host(host), "{host}");
    }
    assert!(is_loopback_host("127.255.255.254"));
}

#[test]
fn ipv4_and_ipv6_peers_have_separate_budgets() {
    let limiter = WebhookRateLimiter::new(1);
    let v4 = IpAddr::V4(Ipv4Addr::LOCALHOST);
    let v6 = IpAddr::V6(Ipv6Addr::LOCALHOST);
    assert!(limiter.check_ip(v4));
    assert!(!limiter.check_ip(v4));
    assert!(limiter.check_ip(v6));
    assert!(!limiter.check_ip(v6));
    assert_eq!(limiter.limit(), 1);
}

#[test]
fn simultaneous_requests_cannot_overspend_a_peer_budget() {
    const WORKERS: usize = 16;
    let limiter = Arc::new(WebhookRateLimiter::new(3));
    let barrier = Arc::new(Barrier::new(WORKERS));
    let workers: Vec<_> = (0..WORKERS).map(|_| {
        let limiter = Arc::clone(&limiter);
        let barrier = Arc::clone(&barrier);
        std::thread::spawn(move || {
            barrier.wait();
            limiter.check_ip(IpAddr::V4(Ipv4Addr::LOCALHOST))
        })
    }).collect();
    let accepted = workers.into_iter().map(|w| usize::from(w.join().unwrap())).sum::<usize>();
    assert_eq!(accepted, 3);
}

#[test]
fn api_configuration_defaults_survive_partial_tables_and_explicit_disable() {
    let empty: Config = toml::from_str("").unwrap();
    assert_eq!(empty.server.host, "127.0.0.1");
    assert_eq!(empty.api.rate_limit_rpm, 120);
    assert!(empty.api.route_repos.is_empty());
    let scoped: Config = toml::from_str("[api]\nroute_repos = ['team/*']").unwrap();
    assert_eq!(scoped.api.route_repos, vec!["team/*"]);
    assert_eq!(scoped.api.rate_limit_rpm, 120);
    let disabled: ApiConfig = toml::from_str("rate_limit_rpm = 0").unwrap();
    assert_eq!(disabled.rate_limit_rpm, 0);
    assert!(disabled.route_repos.is_empty());
    for invalid in ["rate_limit_rpm = -1", "rate_limit_rpm = 4294967296",
        "rate_limit_rpm = '120'", "route_repos = 'team/*'"] {
        assert!(toml::from_str::<ApiConfig>(invalid).is_err(), "{invalid}");
    }
}
