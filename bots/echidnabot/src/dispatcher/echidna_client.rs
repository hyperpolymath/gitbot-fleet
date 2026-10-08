// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Client for communicating with ECHIDNA Core

use reqwest::Client;
use serde::{Deserialize, Serialize};
use std::sync::RwLock;
use std::time::Duration;

use super::prove_result::ProveResult;
use super::{ProofResult, ProofStatus, ProverKind, TacticSuggestion, TrustSource};
use crate::config::{EchidnaApiMode, EchidnaConfig};
use crate::error::{Error, Result};
use crate::trust::axiom_tracker::{AxiomFlag, AxiomReport, AxiomTracker};
use crate::trust::confidence::assess_confidence_with_axioms;
use tracing::warn;

/// Oldest ECHIDNA server release echidnabot talks to.
///
/// 2.3.0 is the first tagged release whose REST `/api/verify` returns the
/// typed `outcome` field and whose `/api/health` reports `version`.
pub const MIN_ECHIDNA_VERSION: &str = "2.3.0";

/// What the start-up handshake learned about the ECHIDNA server.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EchidnaHandshake {
    /// Server version, from `/api/provers` if it reports one, else
    /// `/api/health`.
    pub version: semver::Version,
    /// ECHIDNA's own prover identifiers, as listed by `/api/provers`.
    pub provers: Vec<String>,
}

/// Client for ECHIDNA Core GraphQL API
pub struct EchidnaClient {
    client: Client,
    endpoint: String,
    rest_endpoint: String,
    timeout: Duration,
    mode: EchidnaApiMode,
    /// Result of the last successful [`EchidnaClient::handshake`]; its
    /// prover list maps echidnabot slugs onto ECHIDNA's names.
    handshake: RwLock<Option<EchidnaHandshake>>,
}

impl EchidnaClient {
    /// Create a new ECHIDNA client
    ///
    /// # Panics
    /// Panics if the HTTP client cannot be initialised.
    pub fn new(config: &EchidnaConfig) -> Self {
        let client = Client::builder()
            .timeout(Duration::from_secs(config.timeout_secs))
            .build()
            .expect("Failed to create HTTP client");

        Self {
            client,
            endpoint: config.endpoint.clone(),
            rest_endpoint: config.rest_endpoint.clone(),
            timeout: Duration::from_secs(config.timeout_secs),
            mode: config.mode,
            handshake: RwLock::new(None),
        }
    }

    /// Minimum-version handshake with the ECHIDNA server.
    ///
    /// Lists `/api/provers` (which also proves the REST surface is there) and
    /// reads the version from that response if present, otherwise from
    /// `/api/health`. Returns the version and prover names, caching them for
    /// slug resolution unless the cache lock is poisoned.
    ///
    /// # Errors
    /// Returns [`Error::Http`] for request failures, [`Error::Echidna`] for
    /// 5xx responses, and [`Error::EchidnaIncompatible`] for other unsuccessful
    /// statuses, unreadable response bodies, or a missing, unparseable or
    /// older-than-[`MIN_ECHIDNA_VERSION`] version.
    pub async fn handshake(&self) -> Result<EchidnaHandshake> {
        let response = self
            .client
            .get(self.rest_url("/api/provers"))
            .timeout(Duration::from_secs(10))
            .send()
            .await
            .map_err(Error::Http)?;
        check_handshake_status("/api/provers", response.status())?;
        let provers: RestProversResponse = response.json().await.map_err(|e| {
            Error::EchidnaIncompatible(format!("/api/provers body not understood: {e}"))
        })?;

        let raw_version = match provers.version.clone().or(provers.echidna_version.clone()) {
            Some(v) => v,
            None => {
                let health = self
                    .client
                    .get(self.rest_url("/api/health"))
                    .timeout(Duration::from_secs(5))
                    .send()
                    .await
                    .map_err(Error::Http)?;
                check_handshake_status("/api/health", health.status())?;
                let health: RestHealthResponse = health.json().await.map_err(|e| {
                    Error::EchidnaIncompatible(format!("/api/health body not understood: {e}"))
                })?;
                health.version.ok_or_else(|| {
                    Error::EchidnaIncompatible(format!(
                        "ECHIDNA did not report a version; {MIN_ECHIDNA_VERSION} or newer is required"
                    ))
                })?
            }
        };

        let version = check_min_version(&raw_version)?;
        let handshake = EchidnaHandshake {
            version,
            provers: provers.provers.into_iter().map(|p| p.name).collect(),
        };
        if let Ok(mut slot) = self.handshake.write() {
            *slot = Some(handshake.clone());
        }
        Ok(handshake)
    }

    /// Whether this client talks REST at all (and so can run the handshake).
    ///
    /// GraphQL-only deployments have no `/api/provers`; they skip it.
    pub fn uses_rest(&self) -> bool {
        !matches!(self.mode, EchidnaApiMode::Graphql)
    }

    /// Run [`EchidnaClient::handshake`] unless one has already succeeded.
    ///
    /// Returns the cached result without rechecking the server. If the cache
    /// is empty or unreadable, performs the handshake and propagates its errors.
    pub async fn ensure_handshake(&self) -> Result<EchidnaHandshake> {
        let cached = self.handshake.read().ok().and_then(|slot| slot.clone());
        match cached {
            Some(done) => Ok(done),
            None => self.handshake().await,
        }
    }

    /// ECHIDNA's identifier for an echidnabot prover slug.
    ///
    /// Uses the list learned by [`EchidnaClient::handshake`] when one is
    /// available, ignoring case, `-`, `_`, spaces and `/`. With no match,
    /// uses the classic prover mapping or the prover's display name.
    pub fn echidna_name(&self, prover: &ProverKind) -> String {
        let known = self
            .handshake
            .read()
            .ok()
            .and_then(|slot| slot.as_ref().map(|h| h.provers.clone()))
            .unwrap_or_default();
        resolve_echidna_name(prover, &known)
    }

    /// Verify a proof using ECHIDNA Core
    #[tracing::instrument(
        name = "echidna.verify",
        skip(self, content),
        fields(
            prover = %prover,
            content_bytes = content.len(),
            api_mode = ?self.mode,
        )
    )]
    pub async fn verify_proof(&self, prover: &ProverKind, content: &str) -> Result<ProofResult> {
        match self.mode {
            EchidnaApiMode::Graphql => self.verify_proof_graphql(prover, content).await,
            EchidnaApiMode::Rest => self.verify_proof_rest(prover, content).await,
            EchidnaApiMode::Auto => match self.verify_proof_graphql(prover, content).await {
                Ok(result) => Ok(result),
                Err(err) => {
                    warn!("GraphQL verify failed, falling back to REST: {}", err);
                    self.verify_proof_rest(prover, content).await
                }
            },
        }
    }

    /// Request tactic suggestions from ECHIDNA's Julia ML component
    #[tracing::instrument(
        name = "echidna.suggest",
        skip(self, context, goal_state),
        fields(
            prover = %prover,
            context_bytes = context.len(),
            goal_state_bytes = goal_state.len(),
            api_mode = ?self.mode,
        )
    )]
    pub async fn suggest_tactics(
        &self,
        prover: &ProverKind,
        context: &str,
        goal_state: &str,
    ) -> Result<Vec<TacticSuggestion>> {
        match self.mode {
            EchidnaApiMode::Graphql => {
                self.suggest_tactics_graphql(prover, context, goal_state)
                    .await
            }
            EchidnaApiMode::Rest => self.suggest_tactics_rest(prover, context, goal_state).await,
            EchidnaApiMode::Auto => {
                match self
                    .suggest_tactics_graphql(prover, context, goal_state)
                    .await
                {
                    Ok(result) => Ok(result),
                    Err(err) => {
                        warn!("GraphQL suggest failed, falling back to REST: {}", err);
                        self.suggest_tactics_rest(prover, context, goal_state).await
                    }
                }
            }
        }
    }

    /// Check if ECHIDNA Core is available and healthy
    pub async fn health_check(&self) -> Result<bool> {
        match self.mode {
            EchidnaApiMode::Graphql => self.health_check_graphql().await,
            EchidnaApiMode::Rest => self.health_check_rest().await,
            EchidnaApiMode::Auto => match self.health_check_graphql().await {
                Ok(true) => Ok(true),
                _ => self.health_check_rest().await,
            },
        }
    }

    /// Check prover availability
    #[tracing::instrument(
        name = "echidna.status",
        skip(self),
        fields(prover = %prover, api_mode = ?self.mode)
    )]
    pub async fn prover_status(&self, prover: &ProverKind) -> Result<ProverStatus> {
        match self.mode {
            EchidnaApiMode::Graphql => self.prover_status_graphql(prover).await,
            EchidnaApiMode::Rest => self.prover_status_rest(prover).await,
            EchidnaApiMode::Auto => match self.prover_status_graphql(prover).await {
                Ok(result) => Ok(result),
                Err(err) => {
                    warn!(
                        "GraphQL prover_status failed, falling back to REST: {}",
                        err
                    );
                    self.prover_status_rest(prover).await
                }
            },
        }
    }

    fn rest_url(&self, path: &str) -> String {
        let base = self.rest_endpoint.trim_end_matches('/');
        format!("{}{}", base, path)
    }

    /// Submit proof source through GraphQL and derive confidence locally from
    /// its source, prover output and certificate artefact names.
    ///
    /// Returns [`Error::Http`] for request or response-decoding failures and
    /// [`Error::Echidna`] for unsuccessful HTTP statuses, GraphQL errors or
    /// missing response data.
    async fn verify_proof_graphql(
        &self,
        prover: &ProverKind,
        content: &str,
    ) -> Result<ProofResult> {
        let query = GraphQLRequest {
            query: r#"
                mutation VerifyProof($prover: String!, $content: String!) {
                    verifyProof(prover: $prover, content: $content) {
                        status
                        message
                        proverOutput
                        durationMs
                        artifacts
                    }
                }
            "#
            .to_string(),
            variables: serde_json::json!({
                "prover": prover.as_str(),
                "content": content
            }),
        };

        let response = self
            .client
            .post(&self.endpoint)
            .json(&query)
            .timeout(self.timeout)
            .send()
            .await
            .map_err(Error::Http)?;

        if !response.status().is_success() {
            return Err(Error::Echidna(format!(
                "ECHIDNA returned status {}",
                response.status()
            )));
        }

        let gql_response: GraphQLResponse<VerifyProofResponse> =
            response.json().await.map_err(Error::Http)?;

        if let Some(errors) = gql_response.errors {
            return Err(Error::Echidna(
                errors
                    .into_iter()
                    .map(|e| e.message)
                    .collect::<Vec<_>>()
                    .join(", "),
            ));
        }

        let data = gql_response
            .data
            .ok_or_else(|| Error::Echidna("No data in response".to_string()))?;

        let status = parse_proof_status(&data.verify_proof.status);
        let prover_output = data.verify_proof.prover_output;
        let artifacts = data.verify_proof.artifacts;
        let has_cert = artifacts.iter().any(|a| {
            a.ends_with(".alethe")
                || a.ends_with(".lrat")
                || a.ends_with(".drat")
                || a.ends_with(".tstp")
        });
        let axioms = AxiomTracker::scan_source(prover, content)
            .merge(AxiomTracker::scan(prover, &prover_output));
        let confidence =
            assess_confidence_with_axioms(prover, status, has_cert, 1, axioms.worst_danger);
        Ok(ProofResult {
            status,
            message: data.verify_proof.message,
            prover_output,
            duration_ms: data.verify_proof.duration_ms,
            artifacts,
            confidence: Some(confidence),
            axioms: Some(axioms),
            trust_source: TrustSource::LocalFallback,
        })
    }

    async fn suggest_tactics_graphql(
        &self,
        prover: &ProverKind,
        context: &str,
        goal_state: &str,
    ) -> Result<Vec<TacticSuggestion>> {
        let query = GraphQLRequest {
            query: r#"
                mutation SuggestTactics($prover: String!, $context: String!, $goalState: String!) {
                    suggestTactics(prover: $prover, context: $context, goalState: $goalState) {
                        tactic
                        confidence
                        explanation
                    }
                }
            "#
            .to_string(),
            variables: serde_json::json!({
                "prover": prover.as_str(),
                "context": context,
                "goalState": goal_state
            }),
        };

        let response = self
            .client
            .post(&self.endpoint)
            .json(&query)
            .timeout(self.timeout)
            .send()
            .await
            .map_err(Error::Http)?;

        if !response.status().is_success() {
            return Err(Error::Echidna(format!(
                "ECHIDNA returned status {}",
                response.status()
            )));
        }

        let gql_response: GraphQLResponse<SuggestTacticsResponse> =
            response.json().await.map_err(Error::Http)?;

        if let Some(errors) = gql_response.errors {
            return Err(Error::Echidna(
                errors
                    .into_iter()
                    .map(|e| e.message)
                    .collect::<Vec<_>>()
                    .join(", "),
            ));
        }

        let data = gql_response
            .data
            .ok_or_else(|| Error::Echidna("No data in response".to_string()))?;

        Ok(data
            .suggest_tactics
            .into_iter()
            .map(|s| TacticSuggestion {
                tactic: s.tactic,
                confidence: s.confidence,
                explanation: s.explanation,
            })
            .collect())
    }

    async fn health_check_graphql(&self) -> Result<bool> {
        let query = GraphQLRequest {
            query: "{ __typename }".to_string(),
            variables: serde_json::json!({}),
        };

        let response = self
            .client
            .post(&self.endpoint)
            .json(&query)
            .timeout(Duration::from_secs(5))
            .send()
            .await;

        match response {
            Ok(r) => Ok(r.status().is_success()),
            Err(_) => Ok(false),
        }
    }

    async fn prover_status_graphql(&self, prover: &ProverKind) -> Result<ProverStatus> {
        let query = GraphQLRequest {
            query: r#"
                query ProverStatus($prover: String!) {
                    proverStatus(prover: $prover) {
                        available
                        message
                    }
                }
            "#
            .to_string(),
            variables: serde_json::json!({
                "prover": prover.as_str()
            }),
        };

        let response = self
            .client
            .post(&self.endpoint)
            .json(&query)
            .timeout(Duration::from_secs(10))
            .send()
            .await
            .map_err(Error::Http)?;

        if !response.status().is_success() {
            return Ok(ProverStatus::Unavailable);
        }

        let gql_response: GraphQLResponse<ProverStatusResponse> =
            response.json().await.map_err(Error::Http)?;

        match gql_response.data {
            Some(data) if data.prover_status.available => Ok(ProverStatus::Available),
            Some(_) => Ok(ProverStatus::Unavailable),
            None => Ok(ProverStatus::Unknown),
        }
    }

    /// Submit proof source to `/api/verify` using the resolved prover name.
    ///
    /// Returns [`Error::Http`] for request or JSON-decoding failures and
    /// [`Error::Echidna`] for unsuccessful HTTP statuses. Result validation
    /// errors from [`rest_verify_result`] are propagated.
    async fn verify_proof_rest(&self, prover: &ProverKind, content: &str) -> Result<ProofResult> {
        let request = RestVerifyRequest {
            prover: self.echidna_name(prover),
            content: content.to_string(),
        };

        let response = self
            .client
            .post(self.rest_url("/api/verify"))
            .json(&request)
            .timeout(self.timeout)
            .send()
            .await
            .map_err(Error::Http)?;

        if !response.status().is_success() {
            return Err(Error::Echidna(format!(
                "ECHIDNA REST returned status {}",
                response.status()
            )));
        }

        let body: serde_json::Value = response.json().await.map_err(Error::Http)?;
        rest_verify_result(prover, content, body)
    }

    /// Request up to five tactics for `goal_state`, or for `context` when the
    /// goal state is blank. Each returned tactic receives confidence `0.5`
    /// and a heuristic explanation.
    ///
    /// Returns [`Error::Http`] for request or response-decoding failures and
    /// [`Error::Echidna`] for unsuccessful HTTP statuses.
    async fn suggest_tactics_rest(
        &self,
        prover: &ProverKind,
        context: &str,
        goal_state: &str,
    ) -> Result<Vec<TacticSuggestion>> {
        let content = if !goal_state.trim().is_empty() {
            goal_state.to_string()
        } else {
            context.to_string()
        };

        let request = RestSuggestRequest {
            prover: self.echidna_name(prover),
            content,
            limit: Some(5),
        };

        let response = self
            .client
            .post(self.rest_url("/api/suggest"))
            .json(&request)
            .timeout(self.timeout)
            .send()
            .await
            .map_err(Error::Http)?;

        if !response.status().is_success() {
            return Err(Error::Echidna(format!(
                "ECHIDNA REST returned status {}",
                response.status()
            )));
        }

        let data: RestSuggestResponse = response.json().await.map_err(Error::Http)?;
        Ok(data
            .suggestions
            .into_iter()
            .map(|tactic| TacticSuggestion {
                tactic,
                confidence: 0.5,
                explanation: Some("REST heuristic suggestion".to_string()),
            })
            .collect())
    }

    async fn health_check_rest(&self) -> Result<bool> {
        let response = self
            .client
            .get(self.rest_url("/api/health"))
            .timeout(Duration::from_secs(5))
            .send()
            .await;

        match response {
            Ok(resp) => Ok(resp.status().is_success()),
            Err(_) => Ok(false),
        }
    }

    /// Check whether `/api/provers` lists a matching normalised prover name.
    ///
    /// An absent name yields `Unavailable`; unsuccessful HTTP statuses yield
    /// `Unknown`. Request and response-decoding failures return [`Error::Http`].
    async fn prover_status_rest(&self, prover: &ProverKind) -> Result<ProverStatus> {
        let response = self
            .client
            .get(self.rest_url("/api/provers"))
            .timeout(Duration::from_secs(10))
            .send()
            .await
            .map_err(Error::Http)?;

        if !response.status().is_success() {
            return Ok(ProverStatus::Unknown);
        }

        let data: RestProversResponse = response.json().await.map_err(Error::Http)?;
        let names: Vec<String> = data.provers.into_iter().map(|info| info.name).collect();
        let target = normalise_prover_name(&resolve_echidna_name(prover, &names));
        let available = names
            .iter()
            .any(|name| normalise_prover_name(name) == target);

        Ok(if available {
            ProverStatus::Available
        } else {
            ProverStatus::Unavailable
        })
    }
}

// =============================================================================
// REST Types
// =============================================================================

#[derive(Serialize)]
struct RestVerifyRequest {
    prover: String,
    content: String,
}

/// Legacy REST `/api/verify` body (ECHIDNA ≤ 2.3 shape).
#[derive(Deserialize)]
struct RestVerifyResponse {
    valid: bool,
    /// Typed outcome (`PROVED`, `NO_PROOF_FOUND`, `TIMEOUT`, ...); absent on
    /// very old servers.
    #[serde(default)]
    outcome: Option<String>,
    #[allow(dead_code)]
    #[serde(default)]
    goals_remaining: usize,
    #[allow(dead_code)]
    #[serde(default)]
    tactics_used: usize,
}

/// Build a [`ProofResult`] from a REST `/api/verify` body.
///
/// An `echidna.prove.result/1` body supplies status, message, duration in
/// milliseconds and reported axioms ([`TrustSource::Echidna`]). Its axioms
/// are merged with a scan of `content`; confidence is recalculated locally,
/// ignoring the reported confidence. Other bodies use the legacy shape,
/// source-only axiom scanning and a zero duration. Both return empty prover
/// output and artefact lists.
///
/// # Errors
/// Propagates [`ProveResult::from_value`] validation errors for tagged bodies
/// without trying the legacy shape. Legacy deserialisation errors return
/// [`Error::Json`].
fn rest_verify_result(
    prover: &ProverKind,
    content: &str,
    body: serde_json::Value,
) -> Result<ProofResult> {
    if ProveResult::is_prove_result(&body) {
        let result = ProveResult::from_value(body)?;
        let status = ProofStatus::from(result.status);
        let reported = AxiomReport::from_reported(
            prover.clone(),
            result.trust.axioms.iter().map(|a| AxiomFlag::from_name(a)),
        );
        // ECHIDNA's list is the receipt and counts at full severity (a named
        // `sorry` caps the level even if the source looks clean, e.g. a hole
        // in an imported module). The source scan is merged in as well, so a
        // hole ECHIDNA did not name still counts.
        let axioms = reported.merge(AxiomTracker::scan_source(prover, content));
        let confidence =
            assess_confidence_with_axioms(prover, status, false, 1, axioms.worst_danger);
        return Ok(ProofResult {
            status,
            message: result.message,
            prover_output: String::new(),
            duration_ms: result.duration_ms,
            artifacts: Vec::new(),
            confidence: Some(confidence),
            axioms: Some(axioms),
            trust_source: TrustSource::Echidna,
        });
    }

    let data: RestVerifyResponse = serde_json::from_value(body)?;
    let status = match data.outcome.as_deref() {
        Some(outcome) => parse_rest_outcome(outcome, data.valid),
        None if data.valid => ProofStatus::Verified,
        None => ProofStatus::Failed,
    };
    // The legacy REST body carries no prover output, so the only axiom
    // signal is the source scan.
    let axioms = AxiomTracker::scan_source(prover, content);
    let confidence = assess_confidence_with_axioms(prover, status, false, 1, axioms.worst_danger);
    Ok(ProofResult {
        status,
        message: match status {
            ProofStatus::Verified => "Proof verified successfully".to_string(),
            ProofStatus::Timeout => "Proof verification timed out".to_string(),
            ProofStatus::Error => "ECHIDNA reported an error".to_string(),
            _ => "Proof verification failed".to_string(),
        },
        prover_output: String::new(),
        duration_ms: 0,
        artifacts: Vec::new(),
        confidence: Some(confidence),
        axioms: Some(axioms),
        trust_source: TrustSource::LocalFallback,
    })
}

/// Map ECHIDNA's typed REST outcome onto [`ProofStatus`].
///
/// Matching ignores ASCII case. Unknown outcomes yield `Verified` when
/// `valid` is true and `Unknown` otherwise; recognised outcomes ignore `valid`.
fn parse_rest_outcome(outcome: &str, valid: bool) -> ProofStatus {
    match outcome.to_ascii_uppercase().as_str() {
        "PROVED" => ProofStatus::Verified,
        "NO_PROOF_FOUND" | "INVALID_INPUT" | "INCONSISTENT_PREMISES" => ProofStatus::Failed,
        "TIMEOUT" => ProofStatus::Timeout,
        "UNSUPPORTED_FEATURE" | "PROVER_ERROR" | "SYSTEM_ERROR" => ProofStatus::Error,
        _ if valid => ProofStatus::Verified,
        _ => ProofStatus::Unknown,
    }
}

#[derive(Deserialize)]
struct RestHealthResponse {
    /// Absent on servers older than 2.3.0.
    #[serde(default)]
    version: Option<String>,
}

/// Accept successful handshake statuses and classify failures.
///
/// Returns [`Error::Echidna`] for 5xx responses, classified as transient by
/// [`crate::scheduler::retry::is_transient_error`]. Other unsuccessful statuses
/// return [`Error::EchidnaIncompatible`]. `path` identifies the failing endpoint.
fn check_handshake_status(path: &str, status: reqwest::StatusCode) -> Result<()> {
    if status.is_success() {
        Ok(())
    } else if status.is_server_error() {
        Err(Error::Echidna(format!(
            "ECHIDNA {path} unavailable (status {status})"
        )))
    } else {
        Err(Error::EchidnaIncompatible(format!(
            "ECHIDNA {path} returned status {status}"
        )))
    }
}

/// Parse a reported ECHIDNA version and enforce [`MIN_ECHIDNA_VERSION`].
///
/// Surrounding whitespace and leading lowercase `v` characters are ignored.
/// Returns [`Error::EchidnaIncompatible`] if parsing fails or the version is
/// below the minimum, using semantic-version ordering.
pub fn check_min_version(raw: &str) -> Result<semver::Version> {
    let trimmed = raw.trim().trim_start_matches('v');
    let version = semver::Version::parse(trimmed).map_err(|e| {
        Error::EchidnaIncompatible(format!("ECHIDNA reported unparseable version {raw:?}: {e}"))
    })?;
    let minimum = semver::Version::parse(MIN_ECHIDNA_VERSION)
        .map_err(|e| Error::Internal(format!("MIN_ECHIDNA_VERSION is not semver: {e}")))?;
    if version < minimum {
        return Err(Error::EchidnaIncompatible(format!(
            "ECHIDNA {version} is older than the minimum supported {minimum}"
        )));
    }
    Ok(version)
}

#[derive(Serialize)]
struct RestSuggestRequest {
    prover: String,
    content: String,
    limit: Option<usize>,
}

#[derive(Deserialize)]
struct RestSuggestResponse {
    suggestions: Vec<String>,
}

#[derive(Deserialize)]
struct RestProversResponse {
    provers: Vec<RestProverInfo>,
    /// Not sent by ECHIDNA 2.3; accepted if a later server adds it.
    #[serde(default)]
    version: Option<String>,
    /// Alternative spelling, matching the prove-result contract.
    #[serde(default)]
    echidna_version: Option<String>,
}

#[derive(Deserialize)]
struct RestProverInfo {
    name: String,
    #[allow(dead_code)]
    #[serde(default)]
    tier: u8,
    #[allow(dead_code)]
    #[serde(default)]
    complexity: u8,
}

/// Lower-case a prover name and drop `-`, `_`, spaces and `/` for matching.
fn normalise_prover_name(name: &str) -> String {
    name.chars()
        .filter(|c| !matches!(c, '-' | '_' | ' ' | '/'))
        .flat_map(char::to_lowercase)
        .collect()
}

/// Map an echidnabot slug onto ECHIDNA's identifier.
///
/// With a non-empty `known` list (from `/api/provers`) the first normalised
/// match wins. Without one, or with no match, the static mapping for the
/// classic provers applies; these are ECHIDNA's serde enum names, not
/// presentation labels.
fn resolve_echidna_name(prover: &ProverKind, known: &[String]) -> String {
    let wanted = normalise_prover_name(prover.as_str());
    if let Some(hit) = known.iter().find(|k| normalise_prover_name(k) == wanted) {
        return hit.clone();
    }
    match prover.as_str() {
        "lean" | "lean4" => "Lean",
        "isabelle" => "Isabelle",
        "hol-light" => "HOLLight",
        _ => prover.display_name(),
    }
    .to_string()
}

// =============================================================================
// GraphQL Types
// =============================================================================

#[derive(Serialize)]
struct GraphQLRequest {
    query: String,
    variables: serde_json::Value,
}

#[derive(Deserialize)]
struct GraphQLResponse<T> {
    data: Option<T>,
    errors: Option<Vec<GraphQLError>>,
}

#[derive(Deserialize)]
struct GraphQLError {
    message: String,
}

#[derive(Deserialize)]
struct VerifyProofResponse {
    #[serde(rename = "verifyProof")]
    verify_proof: VerifyProofData,
}

#[derive(Deserialize)]
struct VerifyProofData {
    status: String,
    message: String,
    #[serde(rename = "proverOutput")]
    prover_output: String,
    #[serde(rename = "durationMs")]
    duration_ms: u64,
    artifacts: Vec<String>,
}

#[derive(Deserialize)]
struct SuggestTacticsResponse {
    #[serde(rename = "suggestTactics")]
    suggest_tactics: Vec<TacticSuggestionData>,
}

#[derive(Deserialize)]
struct TacticSuggestionData {
    tactic: String,
    confidence: f64,
    explanation: Option<String>,
}

#[derive(Deserialize)]
struct ProverStatusResponse {
    #[serde(rename = "proverStatus")]
    prover_status: ProverStatusData,
}

#[derive(Deserialize)]
struct ProverStatusData {
    available: bool,
    #[allow(dead_code)]
    message: Option<String>,
}

/// Prover availability status
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProverStatus {
    Available,
    Degraded,
    Unavailable,
    Unknown,
}

fn parse_proof_status(s: &str) -> ProofStatus {
    match s.to_uppercase().as_str() {
        "VERIFIED" | "PASS" | "SUCCESS" => ProofStatus::Verified,
        "FAILED" | "FAIL" => ProofStatus::Failed,
        "TIMEOUT" => ProofStatus::Timeout,
        "ERROR" => ProofStatus::Error,
        _ => ProofStatus::Unknown,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prover_file_extensions() {
        assert!(ProverKind::new("metamath")
            .file_extensions()
            .contains(&".mm"));
        assert!(ProverKind::new("lean").file_extensions().contains(&".lean"));
        assert!(ProverKind::new("coq").file_extensions().contains(&".v"));
    }

    #[test]
    fn test_prover_from_extension() {
        assert_eq!(
            ProverKind::from_extension(".mm"),
            Some(ProverKind::new("metamath"))
        );
        assert_eq!(
            ProverKind::from_extension("lean"),
            Some(ProverKind::new("lean"))
        );
        assert_eq!(ProverKind::from_extension(".xyz"), None);
    }

    #[test]
    fn test_prover_tier() {
        assert_eq!(ProverKind::new("metamath").tier(), 2);
        assert_eq!(ProverKind::new("lean").tier(), 1);
        assert_eq!(ProverKind::new("hol4").tier(), 3);
    }

    #[test]
    fn test_min_version_gate() {
        assert!(check_min_version("2.3.0").is_ok());
        assert!(check_min_version("v2.4.1").is_ok());
        assert!(check_min_version("2.2.9").is_err());
        assert!(check_min_version("not-a-version").is_err());
    }

    #[test]
    fn test_resolve_name_prefers_handshake_list() {
        let known = vec!["HOLLight".to_string(), "Idris2".to_string()];
        assert_eq!(
            resolve_echidna_name(&ProverKind::new("hol-light"), &known),
            "HOLLight"
        );
        assert_eq!(
            resolve_echidna_name(&ProverKind::new("idris2"), &known),
            "Idris2"
        );
        // Falls back to the static classic mapping without a list.
        assert_eq!(resolve_echidna_name(&ProverKind::new("lean"), &[]), "Lean");
    }

    #[test]
    fn test_rest_body_in_prove_result_shape_is_transported() {
        let body = serde_json::json!({
            "schema": "echidna.prove.result/1",
            "status": "verified",
            "prover": "Lean",
            "goal": "t",
            "duration_ms": 7,
            "message": "ok",
            "trust": {"confidence": null, "axioms": ["propext"]},
            "echidna_version": "2.4.0"
        });
        let r = rest_verify_result(
            &ProverKind::new("lean"),
            "theorem t : True := trivial",
            body,
        )
        .unwrap();
        assert_eq!(r.status, ProofStatus::Verified);
        assert_eq!(r.trust_source, TrustSource::Echidna);
        assert_eq!(r.duration_ms, 7);
        assert!(r.axioms.unwrap().flags.contains(&AxiomFlag::ClassicalAxiom));
    }

    #[test]
    fn test_reported_sorry_caps_level_even_with_clean_source() {
        let body = serde_json::json!({
            "schema": "echidna.prove.result/1",
            "status": "verified",
            "prover": "Lean",
            "goal": "t",
            "duration_ms": 1,
            "message": "ok",
            "trust": {"confidence": null, "axioms": ["sorryAx"]},
            "echidna_version": "2.4.0"
        });
        let r = rest_verify_result(
            &ProverKind::new("lean"),
            "theorem t : True := trivial",
            body,
        )
        .unwrap();
        assert!(r.axioms.as_ref().unwrap().has_unsound());
        assert_eq!(
            r.confidence.unwrap().level,
            crate::trust::confidence::ConfidenceLevel::Level1
        );
    }

    #[test]
    fn test_handshake_status_classification() {
        use reqwest::StatusCode;
        assert!(check_handshake_status("/api/provers", StatusCode::OK).is_ok());
        let warmup = check_handshake_status("/api/provers", StatusCode::SERVICE_UNAVAILABLE);
        assert!(matches!(warmup, Err(Error::Echidna(_))));
        assert!(crate::scheduler::retry::is_transient_error(
            &warmup.unwrap_err()
        ));
        assert!(matches!(
            check_handshake_status("/api/provers", StatusCode::NOT_FOUND),
            Err(Error::EchidnaIncompatible(_))
        ));
        assert!(matches!(
            check_min_version("2.2.0"),
            Err(Error::EchidnaIncompatible(_))
        ));
    }

    #[test]
    fn test_legacy_rest_body_uses_outcome_and_source_scan() {
        let body = serde_json::json!({
            "valid": false, "outcome": "TIMEOUT", "goals_remaining": 1, "tactics_used": 0
        });
        let r = rest_verify_result(&ProverKind::new("lean"), "  sorry\n", body).unwrap();
        assert_eq!(r.status, ProofStatus::Timeout);
        assert_eq!(r.trust_source, TrustSource::LocalFallback);
        assert!(r.axioms.unwrap().has_unsound());
    }
}
