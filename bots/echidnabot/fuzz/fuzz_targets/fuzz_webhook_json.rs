// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Fuzz target: webhook JSON body parsing.
//!
//! GitHub/GitLab/Bitbucket send arbitrary JSON in webhook bodies.
//! Verifies that no JSON input causes a panic in the parsing logic.

#![no_main]
use libfuzzer_sys::fuzz_target;
use serde_json::Value;

/// Payloads echidnabot extracts from GitHub PR webhook bodies.
#[derive(Debug, serde::Deserialize)]
struct GitHubPrPayload {
    action: Option<String>,
    #[serde(rename = "pull_request")]
    pull_request: Option<GitHubPr>,
    repository: Option<GitHubRepo>,
    installation: Option<serde_json::Value>,
}

#[derive(Debug, serde::Deserialize)]
struct GitHubPr {
    number: Option<u64>,
    title: Option<String>,
    head: Option<GitHubHead>,
    state: Option<String>,
}

#[derive(Debug, serde::Deserialize)]
struct GitHubHead {
    sha: Option<String>,
    #[serde(rename = "ref")]
    branch: Option<String>,
}

#[derive(Debug, serde::Deserialize)]
struct GitHubRepo {
    id: Option<i64>,
    full_name: Option<String>,
    clone_url: Option<String>,
}

/// GitLab push/MR payload subset.
#[derive(Debug, serde::Deserialize)]
struct GitLabPayload {
    object_kind: Option<String>,
    checkout_sha: Option<String>,
    project: Option<serde_json::Value>,
    object_attributes: Option<serde_json::Value>,
}

/// Bitbucket push payload subset.
#[derive(Debug, serde::Deserialize)]
struct BitbucketPayload {
    push: Option<serde_json::Value>,
    repository: Option<serde_json::Value>,
}

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // All of these must not panic
        let _ = serde_json::from_str::<GitHubPrPayload>(s);
        let _ = serde_json::from_str::<GitLabPayload>(s);
        let _ = serde_json::from_str::<BitbucketPayload>(s);
        let _ = serde_json::from_str::<Value>(s);

        // ProverKind construction from any string must not panic
        let _ = echidnabot::dispatcher::ProverKind::new(s);

        // goal_fingerprint on any string must not panic
        let _ = echidnabot::store::models::goal_fingerprint(s);
    }
});
