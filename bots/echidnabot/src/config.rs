// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Configuration management for echidnabot

use serde::Deserialize;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::dispatcher::ProverKind;
use crate::error::Result;
use crate::modes::BotMode;

/// Main configuration structure
#[derive(Debug, Deserialize, Clone, Default)]
pub struct Config {
    /// Server configuration
    #[serde(default)]
    pub server: ServerConfig,

    /// Database configuration
    #[serde(default)]
    pub database: DatabaseConfig,

    /// ECHIDNA Core connection
    #[serde(default)]
    pub echidna: EchidnaConfig,

    /// GitHub integration
    #[serde(default)]
    pub github: Option<GitHubConfig>,

    /// GitLab integration
    #[serde(default)]
    pub gitlab: Option<GitLabConfig>,

    /// Codeberg / Forgejo / Gitea integration (issue #62 scaffold —
    /// adapter is functional but light on features, see
    /// `src/adapters/codeberg.rs`).
    #[serde(default)]
    pub codeberg: Option<CodebergConfig>,

    /// Scheduler configuration
    #[serde(default)]
    pub scheduler: SchedulerConfig,

    /// Corpus-delta / retrain-trigger configuration (feedback loop).
    #[serde(default)]
    pub corpus: CorpusConfig,

    /// Executor configuration — local isolation vs ECHIDNA delegation.
    #[serde(default)]
    pub executor: ExecutorConfig,

    /// BoJ server endpoint for Consultant-mode Q&A (Phase 6 / Bit 6b).
    /// Routes LLM calls through BoJ's `model-router-mcp` cartridge per
    /// the canonical "BoJ-only MCP" estate rule. Optional — when absent
    /// or unreachable, Consultant mode degrades to local-data-only.
    #[serde(default)]
    pub boj: Option<BoJConfig>,

    /// Daemon-wide bot mode default. Per-repo directives and the DB column
    /// take precedence; this is the final fallback before `BotMode::Verifier`.
    ///
    /// TOML: `[bot]\nmode = "advisor"` (or verifier / consultant / regulator)
    #[serde(default)]
    pub bot: BotConfig,

    /// Lifecycle / graceful-shutdown settings.
    ///
    /// TOML: `[lifecycle]\nshutdown_timeout_secs = 30`
    /// Env override: `ECHIDNABOT_SHUTDOWN_TIMEOUT_SECS` (env wins).
    #[serde(default)]
    pub lifecycle: LifecycleConfig,

    /// OpenTelemetry observability settings. Honours the standard
    /// `OTEL_EXPORTER_OTLP_ENDPOINT` env var (env wins over TOML).
    #[serde(default)]
    pub observability: ObservabilityConfig,

    /// GraphQL API access: which repositories the route credential may
    /// submit obligations for, and the per-IP rate limit on `/graphql`.
    /// The credentials themselves are read only from the environment
    /// (`ECHIDNABOT_API_ROUTE_TOKEN`, `ECHIDNABOT_API_ADMIN_TOKEN`).
    ///
    /// TOML: `[api]\nroute_repos = ["hyperpolymath/*"]\nrate_limit_rpm = 120`
    #[serde(default)]
    pub api: ApiConfig,
}

/// Lifecycle settings — how long to wait for in-flight work to drain
/// during a graceful shutdown before forcing teardown.
///
/// ```toml
/// [lifecycle]
/// shutdown_timeout_secs = 30
/// ```
#[derive(Debug, Deserialize, Clone)]
pub struct LifecycleConfig {
    /// Deadline (seconds) for the in-flight-job drain phase of shutdown.
    /// After this elapses the coordinator proceeds to subsystem teardown
    /// (DB close, tracer flush) even if jobs are still running — they
    /// will be aborted as the runtime stops.
    ///
    /// Default 30s — generous enough for most provers to finish a
    /// single verification round, short enough that systemd/k8s won't
    /// escalate to SIGKILL first (systemd's default `TimeoutStopSec` is
    /// 90s, k8s's default `terminationGracePeriodSeconds` is 30s).
    ///
    /// Overridden by env var `ECHIDNABOT_SHUTDOWN_TIMEOUT_SECS`.
    #[serde(default = "default_shutdown_timeout_secs")]
    pub shutdown_timeout_secs: u64,
}

impl Default for LifecycleConfig {
    fn default() -> Self {
        Self {
            shutdown_timeout_secs: default_shutdown_timeout_secs(),
        }
    }
}

/// OpenTelemetry / OTLP exporter configuration.
///
/// ```toml
/// [observability]
/// otlp_endpoint = "http://localhost:4317"
/// service_name  = "echidnabot"
/// ```
///
/// `otlp_endpoint` is also picked up from `OTEL_EXPORTER_OTLP_ENDPOINT`
/// (the standard env var); env takes precedence over the TOML value.
/// When neither is set, span data is not exported — only the local
/// fmt subscriber emits logs.
#[derive(Debug, Deserialize, Clone)]
pub struct ObservabilityConfig {
    /// OTLP/gRPC collector endpoint (e.g. `http://localhost:4317`).
    /// `None` disables span export; fmt-layer logs remain active.
    #[serde(default)]
    pub otlp_endpoint: Option<String>,

    /// `service.name` resource attribute on exported spans. Defaults
    /// to `echidnabot` so dashboards group correctly out of the box.
    #[serde(default = "default_service_name")]
    pub service_name: String,
}

impl Default for ObservabilityConfig {
    fn default() -> Self {
        Self {
            otlp_endpoint: None,
            service_name: default_service_name(),
        }
    }
}

fn default_shutdown_timeout_secs() -> u64 {
    crate::shutdown::DEFAULT_SHUTDOWN_TIMEOUT_SECS
}

fn default_service_name() -> String {
    "echidnabot".to_string()
}

impl ObservabilityConfig {
    /// Resolve the effective OTLP endpoint, applying env-var override.
    ///
    /// Precedence (highest first):
    ///   1. `OTEL_EXPORTER_OTLP_ENDPOINT` env var (standard OTel env)
    ///   2. `[observability].otlp_endpoint` from TOML
    ///   3. `None` (no export)
    pub fn resolved_endpoint(&self) -> Option<String> {
        std::env::var("OTEL_EXPORTER_OTLP_ENDPOINT")
            .ok()
            .filter(|s| !s.is_empty())
            .or_else(|| self.otlp_endpoint.clone())
    }
}

/// Daemon-wide bot operating mode settings.
///
/// ```toml
/// [bot]
/// mode = "advisor"   # verifier | advisor | consultant | regulator
/// ```
#[derive(Debug, Deserialize, Clone, Default)]
pub struct BotConfig {
    /// Daemon-wide default operating mode. Used as a fallback when a repo
    /// has no per-repo directive file and its DB column is still the
    /// built-in default (Verifier). Overridden by per-repo directive or
    /// explicit `register --mode` setting.
    #[serde(default)]
    pub mode: BotMode,
}

/// BoJ server connection settings. Endpoint can also be overridden by
/// the `BOJ_ENDPOINT` env var (env wins so operators can repoint
/// without restarting the daemon's config-load path).
#[derive(Debug, Deserialize, Clone)]
pub struct BoJConfig {
    /// Base URL of the BoJ loader (e.g. `http://127.0.0.1:7700`).
    pub url: String,
}

/// Executor configuration. Controls how proof verification is dispatched:
/// either by delegating to a remote ECHIDNA instance over REST/GraphQL
/// (default — `local_isolation = false`), or by spawning prover binaries
/// locally inside an isolation sandbox (`local_isolation = true`).
///
/// Local isolation needs `podman` (preferred) or `bubblewrap` (`bwrap`)
/// on PATH; the executor refuses to run if neither is available
/// (fail-safe per SONNET-TASKS Task 1).
#[derive(Debug, Deserialize, Clone, Default)]
pub struct ExecutorConfig {
    /// When true, process_job runs proof binaries locally in a sandboxed
    /// container instead of POSTing to ECHIDNA's REST API. Useful for
    /// air-gapped / no-ECHIDNA-available scenarios. Adds prover-binary
    /// install requirements to the host.
    #[serde(default)]
    pub local_isolation: bool,

    /// Default container image used by the Podman backend when no
    /// per-prover override is configured. The bundled image typically
    /// carries the full prover-binary set.
    #[serde(default)]
    pub container_image: Option<String>,

    /// Per-prover container images. Each prover gets the specialised
    /// image carrying just its binaries — smaller, faster cold-start,
    /// reduced attack surface vs the full bundle. Keys are the
    /// lowercase ProverKind variant names (`coq`, `lean`, `agda`, ...).
    /// Falls back to `container_image` for any prover not listed here.
    ///
    /// TOML example:
    ///   [executor.container_images]
    ///   coq = "ghcr.io/hyperpolymath/echidna-provers/coq:2026.04"
    ///   lean = "ghcr.io/hyperpolymath/echidna-provers/lean:2026.04"
    ///   agda = "ghcr.io/hyperpolymath/echidna-provers/agda:2026.04"
    #[serde(default)]
    pub container_images: HashMap<ProverKind, String>,

    /// Memory cap for each proof container. Default `512m`.
    #[serde(default)]
    pub memory_limit: Option<String>,

    /// CPU cap (cores). Default 2.
    #[serde(default)]
    pub cpu_limit: Option<f64>,

    /// Per-proof timeout in seconds. Default 300.
    #[serde(default)]
    pub timeout_secs: Option<u64>,
}

impl ExecutorConfig {
    /// Resolve the container image for a specific prover. Per-prover map
    /// wins over the default `container_image`; both can be unset, in
    /// which case the executor uses its built-in default
    /// (`PodmanExecutor::default().image`).
    pub fn image_for(&self, prover: ProverKind) -> Option<String> {
        self.container_images
            .get(&prover)
            .cloned()
            .or_else(|| self.container_image.clone())
    }
}

/// Corpus-delta writer + retrain-trigger settings. Disabled by default —
/// opt-in to avoid accidentally writing into ECHIDNA's training_data from
/// dev / CI environments.
#[derive(Debug, Deserialize, Clone, Default)]
pub struct CorpusConfig {
    /// Master switch. When false, callers should not instantiate a CorpusDelta.
    #[serde(default)]
    pub enabled: bool,

    /// Directory where delta JSONL files are written.
    /// Typically `{echidna_root}/training_data`.
    #[serde(default)]
    pub training_data_dir: Option<PathBuf>,

    /// Root of the ECHIDNA repo — working directory for `just corpus-refresh`.
    #[serde(default)]
    pub echidna_root: Option<PathBuf>,

    /// Fire the retrain trigger automatically after N successful records.
    /// `None` requires an explicit caller (MCP tool, scheduled job).
    #[serde(default)]
    pub auto_trigger_threshold: Option<u32>,
}

#[derive(Debug, Deserialize, Clone, Copy)]
#[serde(rename_all = "lowercase")]
pub enum EchidnaApiMode {
    Auto,
    Graphql,
    Rest,
}

#[derive(Debug, Deserialize, Clone)]
pub struct ServerConfig {
    #[serde(default = "default_host")]
    pub host: String,

    #[serde(default = "default_port")]
    pub port: u16,

    /// Maximum webhook requests per IP per minute (None = unlimited).
    pub rate_limit_rpm: Option<u32>,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            host: default_host(),
            port: default_port(),
            rate_limit_rpm: None,
        }
    }
}

fn default_host() -> String {
    // Loopback by default: binding every interface is an explicit choice,
    // and the daemon refuses it unless API credentials are configured
    // (see `api::auth`).
    "127.0.0.1".to_string()
}

fn default_port() -> u16 {
    8080
}

/// GraphQL API access settings. The two credentials are not here: they are
/// read only from `ECHIDNABOT_API_ROUTE_TOKEN` and `ECHIDNABOT_API_ADMIN_TOKEN`
/// so they never sit in a config file.
///
/// ```toml
/// [api]
/// route_repos = ["hyperpolymath/*"]
/// rate_limit_rpm = 120
/// ```
#[derive(Debug, Deserialize, Clone)]
pub struct ApiConfig {
    /// Repositories the route credential may submit obligations for:
    /// exact `owner/name` slugs or `owner/*`. Empty allows none.
    #[serde(default)]
    pub route_repos: Vec<String>,

    /// Maximum `/graphql` requests per minute per TCP peer address. Default
    /// 120; set `0` to disable. Forwarding headers are not trusted, so
    /// behind a reverse proxy every client shares the proxy's budget: limit
    /// at the proxy and set `0` here.
    #[serde(default = "default_api_rate_limit_rpm")]
    pub rate_limit_rpm: u32,
}

impl Default for ApiConfig {
    fn default() -> Self {
        Self {
            route_repos: Vec::new(),
            rate_limit_rpm: default_api_rate_limit_rpm(),
        }
    }
}

fn default_api_rate_limit_rpm() -> u32 {
    120
}

#[derive(Debug, Deserialize, Clone)]
pub struct DatabaseConfig {
    #[serde(default = "default_database_url")]
    pub url: String,

    #[serde(default = "default_max_connections")]
    pub max_connections: u32,
}

impl Default for DatabaseConfig {
    fn default() -> Self {
        Self {
            url: default_database_url(),
            max_connections: default_max_connections(),
        }
    }
}

fn default_database_url() -> String {
    "sqlite://echidnabot.db".to_string()
}

fn default_max_connections() -> u32 {
    5
}

#[derive(Debug, Deserialize, Clone)]
pub struct EchidnaConfig {
    /// ECHIDNA Core GraphQL endpoint
    #[serde(default = "default_echidna_endpoint")]
    pub endpoint: String,

    /// ECHIDNA Core REST endpoint
    #[serde(default = "default_echidna_rest_endpoint")]
    pub rest_endpoint: String,

    /// API mode (auto, graphql, rest)
    #[serde(default = "default_echidna_mode")]
    pub mode: EchidnaApiMode,

    /// Timeout for proof verification (seconds)
    #[serde(default = "default_timeout")]
    pub timeout_secs: u64,
}

impl Default for EchidnaConfig {
    fn default() -> Self {
        Self {
            endpoint: default_echidna_endpoint(),
            rest_endpoint: default_echidna_rest_endpoint(),
            mode: default_echidna_mode(),
            timeout_secs: default_timeout(),
        }
    }
}

/// Default ECHIDNA GraphQL endpoint: the separate `echidna-graphql` binary,
/// which serves GraphQL at `/` on 127.0.0.1:8081 (`echidna server` has no
/// GraphQL route; in `auto` mode the REST fallback reaches it instead).
fn default_echidna_endpoint() -> String {
    "http://127.0.0.1:8081/".to_string()
}

/// Default ECHIDNA REST base URL (same `echidna server`, port 8081).
fn default_echidna_rest_endpoint() -> String {
    "http://127.0.0.1:8081".to_string()
}

fn default_echidna_mode() -> EchidnaApiMode {
    EchidnaApiMode::Auto
}

fn default_timeout() -> u64 {
    300 // 5 minutes
}

#[derive(Deserialize, Clone)]
pub struct GitHubConfig {
    /// GitHub App ID
    pub app_id: Option<u64>,

    /// Path to private key file
    pub private_key_path: Option<String>,

    /// Personal access token (alternative to app auth)
    pub token: Option<String>,

    /// Webhook secret for signature verification
    pub webhook_secret: Option<String>,
}

#[derive(Deserialize, Clone)]
pub struct GitLabConfig {
    /// GitLab instance URL
    pub url: String,

    /// Personal access token
    pub token: String,

    /// Webhook secret
    pub webhook_secret: Option<String>,
}

/// Codeberg / Forgejo / Gitea connection settings.
///
/// `url` defaults to `https://codeberg.org` in the adapter when this
/// section is absent. Point it at any self-hosted Forgejo or Gitea
/// instance to reuse the same adapter:
///
/// ```toml
/// [codeberg]
/// url = "https://forgejo.example.org"
/// token = "..."
/// # Set webhook_secret from your credential store locally.
/// ```
///
/// `token` is optional — read-only endpoints (file fetch, default
/// branch) work without it on public repos. Mutating endpoints
/// (statuses, comments, issues) require a token; the adapter
/// returns `Error::Config("CODEBERG_TOKEN not set")` when missing.
/// The `CODEBERG_TOKEN` env var also works as a fallback.
#[derive(Deserialize, Clone)]
pub struct CodebergConfig {
    /// Codeberg / Forgejo / Gitea instance URL.
    pub url: String,

    /// Personal access token (PAT). Forgejo also supports app tokens
    /// but the adapter does not yet distinguish — see TODO in
    /// `src/adapters/codeberg.rs`.
    pub token: Option<String>,

    /// Webhook secret for `X-Gitea-Signature` HMAC-SHA256 verification.
    /// Header name is `X-Gitea-Signature` on both Forgejo and Codeberg
    /// (the Gitea fork name is retained for wire compatibility).
    pub webhook_secret: Option<String>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct SchedulerConfig {
    /// Maximum concurrent proof jobs
    #[serde(default = "default_max_concurrent")]
    pub max_concurrent: usize,

    /// Queue size limit
    #[serde(default = "default_queue_size")]
    pub queue_size: usize,
}

impl Default for SchedulerConfig {
    fn default() -> Self {
        Self {
            max_concurrent: default_max_concurrent(),
            queue_size: default_queue_size(),
        }
    }
}

fn default_max_concurrent() -> usize {
    5
}

fn default_queue_size() -> usize {
    100
}

impl Config {
    /// Load configuration from file
    pub fn load(path: &str) -> Result<Self> {
        Self::load_with_env(path, Self::environment())
    }

    /// The `ECHIDNABOT__SECTION__KEY` environment source.
    fn environment() -> config::Environment {
        config::Environment::with_prefix("ECHIDNABOT").separator("__")
    }

    /// [`Config::load`] with the environment source supplied, so tests can
    /// pass a fixed map instead of mutating the process environment.
    ///
    /// The environment applies whether or not the file exists: a container
    /// that ships no config file is configured by `ECHIDNABOT__…` variables
    /// alone (see `.env.example`).
    fn load_with_env(path: &str, env: config::Environment) -> Result<Self> {
        let path = Path::new(path);

        if !path.exists() {
            tracing::warn!(
                "Config file {} not found, using defaults and ECHIDNABOT__* overrides",
                path.display()
            );
        }

        let builder = config::Config::builder()
            .add_source(config::File::from(path).required(false))
            .add_source(env);

        let config = builder.build()?;
        let parsed: Config = config.try_deserialize()?;

        Ok(parsed)
    }
}

/// Render a secret-bearing field for `Debug` without its value.
/// Returns `"<redacted>"` for `Some` and `"<unset>"` for `None`.
///
/// Pattern from the types fit map (secret-types is not yet a usable
/// library): tokens and webhook secrets never reach logs through `{:?}`.
pub fn redacted<T>(value: &Option<T>) -> &'static str {
    if value.is_some() {
        "<redacted>"
    } else {
        "<unset>"
    }
}

impl std::fmt::Debug for GitHubConfig {
    /// Debug output with `token` and `webhook_secret` redacted.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GitHubConfig")
            .field("app_id", &self.app_id)
            .field("private_key_path", &self.private_key_path)
            .field("token", &redacted(&self.token))
            .field("webhook_secret", &redacted(&self.webhook_secret))
            .finish()
    }
}

impl std::fmt::Debug for GitLabConfig {
    /// Debug output with `token` and `webhook_secret` redacted.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GitLabConfig")
            .field("url", &self.url)
            .field("token", &"<redacted>")
            .field("webhook_secret", &redacted(&self.webhook_secret))
            .finish()
    }
}

impl std::fmt::Debug for CodebergConfig {
    /// Debug output with `token` and `webhook_secret` redacted.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CodebergConfig")
            .field("url", &self.url)
            .field("token", &redacted(&self.token))
            .field("webhook_secret", &redacted(&self.webhook_secret))
            .finish()
    }
}

#[cfg(test)]
mod redaction_tests {
    use super::*;

    /// Secrets never appear in `Debug` output; their presence still does.
    #[test]
    fn forge_configs_redact_secrets_in_debug() {
        let gh = GitHubConfig {
            app_id: Some(1),
            private_key_path: None,
            token: Some("ghp_SECRET".into()),
            webhook_secret: Some("whsec_SECRET".into()),
        };
        let gl = GitLabConfig {
            url: "https://gitlab.com".into(),
            token: "glpat_SECRET".into(),
            webhook_secret: None,
        };
        let cb = CodebergConfig {
            url: "https://codeberg.org".into(),
            token: Some("cb_SECRET".into()),
            webhook_secret: Some("cbwh_SECRET".into()),
        };
        for out in [format!("{gh:?}"), format!("{gl:?}"), format!("{cb:?}")] {
            assert!(!out.contains("SECRET"), "{out}");
            assert!(out.contains("<redacted>"), "{out}");
        }
    }
}

#[cfg(test)]
mod load_tests {
    use super::*;

    fn env(pairs: &[(&str, &str)]) -> config::Environment {
        let map: config::Map<String, String> = pairs
            .iter()
            .map(|(k, v)| ((*k).to_string(), (*v).to_string()))
            .collect();
        Config::environment().source(Some(map))
    }

    /// A container ships no config file and sets `ECHIDNABOT__SERVER__HOST`
    /// in `.env`: the override must apply, not the loopback default.
    #[test]
    fn environment_applies_when_the_config_file_is_missing() {
        let missing = "/nonexistent/echidnabot-test-config.toml";
        let c = Config::load_with_env(
            missing,
            env(&[
                ("ECHIDNABOT__SERVER__HOST", "0.0.0.0"),
                ("ECHIDNABOT__SERVER__PORT", "9091"),
            ]),
        )
        .unwrap();
        assert_eq!(c.server.host, "0.0.0.0");
        assert_eq!(c.server.port, 9091);
    }

    #[test]
    fn missing_file_and_no_overrides_gives_the_defaults() {
        let c =
            Config::load_with_env("/nonexistent/echidnabot-test-config.toml", env(&[])).unwrap();
        assert_eq!(c.server.host, "127.0.0.1");
        assert_eq!(c.server.port, 8080);
        assert_eq!(c.api.rate_limit_rpm, 120);
        assert!(c.api.route_repos.is_empty());
    }
}
