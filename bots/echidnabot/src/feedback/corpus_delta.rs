// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Corpus-delta writer — feeds successful proofs back into ECHIDNA's
//! training_data for the Julia retrainer.
//!
//! Two write paths per successful record:
//!
//! 1. **Audit log** (`delta_YYYY-MM-DD.jsonl`): every attempt, all fields,
//!    including failures. Internal bookkeeping only.
//!
//! 2. **Corpus feed** (`proof_states_echidnabot_YYYY-MM-DD.jsonl`): successes
//!    only, in the `proof_states_*.jsonl` schema that `merge_corpus.jl` globs
//!    for at line 334. Fields match what the dedup key and richness scorer
//!    expect: `prover`, `theorem`, `goal`, `tactic_proof`, `context`, `source`.
//!
//! `merge_corpus.jl` already contains the bridge glob at step 1b; the only
//! reason proofs were dropped silently was the filename mismatch
//! (`delta_*` vs `proof_states_echidnabot_*`).

use std::path::{Path, PathBuf};
use std::sync::Arc;

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use tokio::fs::{self, OpenOptions};
use tokio::io::AsyncWriteExt;
use tokio::process::Command;
use tokio::sync::Mutex;

use crate::dispatcher::ProverKind;
use crate::error::{Error, Result};

/// Provenance of a delta row.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum DeltaSource {
    Webhook,
    Mcp,
    Cli,
}

impl DeltaSource {
    fn as_str(self) -> &'static str {
        match self {
            DeltaSource::Webhook => "echidnabot-webhook",
            DeltaSource::Mcp => "echidnabot-mcp",
            DeltaSource::Cli => "echidnabot-cli",
        }
    }
}

/// Full audit record — written to `delta_YYYY-MM-DD.jsonl` for every attempt.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeltaRow {
    pub timestamp: DateTime<Utc>,
    pub prover: ProverKind,
    pub goal_state: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub context: Option<String>,
    pub chosen_tactic: String,
    pub succeeded: bool,
    pub duration_ms: i64,
    pub source: DeltaSource,
}

impl DeltaRow {
    pub fn new(
        prover: ProverKind,
        goal_state: String,
        chosen_tactic: String,
        succeeded: bool,
        duration_ms: i64,
        source: DeltaSource,
    ) -> Self {
        Self {
            timestamp: Utc::now(),
            prover,
            goal_state,
            context: None,
            chosen_tactic,
            succeeded,
            duration_ms,
            source,
        }
    }

    pub fn with_context(mut self, context: String) -> Self {
        self.context = Some(context);
        self
    }
}

/// Corpus-feed entry — written to `proof_states_echidnabot_YYYY-MM-DD.jsonl`
/// for successful proofs only. Schema matches `merge_corpus.jl`'s expectations:
/// dedup key `(prover, theorem)`, richness scorer reads `goal`/`tactic_proof`.
#[derive(Debug, Clone, Serialize)]
pub struct ProofStateEntry {
    /// 0 here; `merge_corpus.jl` reassigns sequential IDs at merge time.
    pub id: u64,
    /// Canonical prover name (title-cased slug, e.g. "Coq", "Lean", "Z3").
    pub prover: String,
    /// Dedup key: the goal state is used as the theorem identifier for
    /// live proof attempts (no static theorem name is available from webhooks).
    pub theorem: String,
    /// Current proof goal — identical to `theorem` for live CI proofs.
    pub goal: String,
    /// The tactic that closed the proof.
    pub tactic_proof: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context: Option<String>,
    /// Prefixed source string so corpus stats show echidnabot provenance.
    pub source: String,
    pub duration_ms: i64,
}

impl ProofStateEntry {
    fn from_delta_row(row: &DeltaRow) -> Self {
        Self {
            id: 0,
            prover: canonical_prover_name(row.prover.as_str()),
            theorem: row.goal_state.clone(),
            goal: row.goal_state.clone(),
            tactic_proof: row.chosen_tactic.clone(),
            context: row.context.clone(),
            source: row.source.as_str().to_string(),
            duration_ms: row.duration_ms,
        }
    }
}

/// Title-case the prover slug so names match what `merge_corpus.jl` expects
/// (e.g. "coq" → "Coq", "fstar" → "F*", "z3" → "Z3").
fn canonical_prover_name(slug: &str) -> String {
    match slug {
        "coq" | "rocq" => "Coq".to_string(),
        "lean" | "lean4" => "Lean".to_string(),
        "agda" => "Agda".to_string(),
        "isabelle" => "Isabelle".to_string(),
        "idris2" => "Idris2".to_string(),
        "fstar" => "F*".to_string(),
        "z3" => "Z3".to_string(),
        "cvc5" | "cvc4" => "CVC5".to_string(),
        "alt-ergo" | "altergo" => "Alt-Ergo".to_string(),
        "dafny" => "Dafny".to_string(),
        "why3" => "Why3".to_string(),
        "metamath" => "Metamath".to_string(),
        "hol-light" | "hollight" => "HOLLight".to_string(),
        "hol4" => "HOL4".to_string(),
        "mizar" => "Mizar".to_string(),
        "pvs" => "PVS".to_string(),
        "acl2" => "ACL2".to_string(),
        "tlaps" => "TLAPS".to_string(),
        "twelf" => "Twelf".to_string(),
        "nuprl" => "Nuprl".to_string(),
        "minlog" => "Minlog".to_string(),
        "imandra" => "Imandra".to_string(),
        "vampire" => "Vampire".to_string(),
        "eprover" => "EProver".to_string(),
        "spass" => "SPASS".to_string(),
        _ => {
            // Fallback: capitalise first character.
            let mut c = slug.chars();
            match c.next() {
                None => String::new(),
                Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
            }
        }
    }
}

const DEFAULT_TRIGGER_PROGRAM: &str = "just";
const DEFAULT_TRIGGER_ARGS: &[&str] = &["corpus-refresh"];

pub struct CorpusDelta {
    training_data_dir: PathBuf,
    echidna_root: Option<PathBuf>,
    trigger_program: String,
    trigger_args: Vec<String>,
    auto_trigger_threshold: Option<u32>,
    counter: Arc<Mutex<u32>>,
}

impl CorpusDelta {
    pub fn new(training_data_dir: PathBuf) -> Self {
        Self {
            training_data_dir,
            echidna_root: None,
            trigger_program: DEFAULT_TRIGGER_PROGRAM.to_string(),
            trigger_args: DEFAULT_TRIGGER_ARGS.iter().map(|s| s.to_string()).collect(),
            auto_trigger_threshold: None,
            counter: Arc::new(Mutex::new(0)),
        }
    }

    pub fn with_trigger(mut self, echidna_root: PathBuf) -> Self {
        self.echidna_root = Some(echidna_root);
        self
    }

    pub fn with_trigger_command(mut self, program: String, args: Vec<String>) -> Self {
        self.trigger_program = program;
        self.trigger_args = args;
        self
    }

    pub fn with_auto_trigger(mut self, threshold: u32) -> Self {
        self.auto_trigger_threshold = Some(threshold);
        self
    }

    /// Append a delta row to today's audit log (`delta_YYYY-MM-DD.jsonl`).
    /// On success, also append a `ProofStateEntry` to the corpus-feed file
    /// (`proof_states_echidnabot_YYYY-MM-DD.jsonl`) that `merge_corpus.jl`
    /// picks up during corpus-refresh.
    #[tracing::instrument(
        name = "feedback.publish",
        skip(self, row),
        fields(
            prover = %row.prover,
            tactic = %row.chosen_tactic,
            succeeded = row.succeeded,
            source = ?row.source,
        )
    )]
    pub async fn record(&self, row: &DeltaRow) -> Result<PathBuf> {
        fs::create_dir_all(&self.training_data_dir)
            .await
            .map_err(|e| Error::Internal(format!("create training_data_dir: {}", e)))?;

        // 1. Audit log — all rows.
        let delta_path = self.delta_path_for(row.timestamp);
        append_jsonl(&delta_path, row).await?;

        // 2. Corpus feed — successful proofs only, in proof_states schema.
        if row.succeeded {
            let ps_path = self.proof_state_path_for(row.timestamp);
            let entry = ProofStateEntry::from_delta_row(row);
            append_jsonl(&ps_path, &entry).await?;

            if let Some(threshold) = self.auto_trigger_threshold {
                let mut counter = self.counter.lock().await;
                *counter += 1;
                if *counter >= threshold {
                    *counter = 0;
                    drop(counter);
                    let _ = self.trigger_refresh().await?;
                }
            }
        }

        Ok(delta_path)
    }

    pub async fn trigger_refresh(&self) -> Result<RefreshStatus> {
        let cwd = self.echidna_root.as_ref().ok_or_else(|| {
            Error::Internal("corpus refresh: echidna_root not configured".to_string())
        })?;

        let output = Command::new(&self.trigger_program)
            .args(&self.trigger_args)
            .current_dir(cwd)
            .output()
            .await
            .map_err(|e| {
                Error::Internal(format!(
                    "corpus refresh: spawn {:?} failed: {}",
                    self.trigger_program, e
                ))
            })?;

        Ok(RefreshStatus {
            success: output.status.success(),
            exit_code: output.status.code(),
            stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
            stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
        })
    }

    /// Path of the full audit log for a given UTC date.
    pub fn delta_path_for(&self, ts: DateTime<Utc>) -> PathBuf {
        self.training_data_dir
            .join(format!("delta_{}.jsonl", ts.format("%Y-%m-%d")))
    }

    /// Path of the corpus-feed file for a given UTC date.
    /// Named `proof_states_echidnabot_YYYY-MM-DD.jsonl` so `merge_corpus.jl`
    /// picks it up via its step-1b glob (`startswith("proof_states_echidnabot_")`).
    pub fn proof_state_path_for(&self, ts: DateTime<Utc>) -> PathBuf {
        self.training_data_dir.join(format!(
            "proof_states_echidnabot_{}.jsonl",
            ts.format("%Y-%m-%d")
        ))
    }

    pub async fn counter_value(&self) -> u32 {
        *self.counter.lock().await
    }

    pub fn training_data_dir(&self) -> &Path {
        &self.training_data_dir
    }
}

async fn append_jsonl<T: Serialize>(path: &PathBuf, value: &T) -> Result<()> {
    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
        .await
        .map_err(|e| Error::Internal(format!("open {}: {}", path.display(), e)))?;

    let mut line = serde_json::to_string(value)?;
    line.push('\n');
    file.write_all(line.as_bytes())
        .await
        .map_err(|e| Error::Internal(format!("write {}: {}", path.display(), e)))?;
    file.flush()
        .await
        .map_err(|e| Error::Internal(format!("flush {}: {}", path.display(), e)))?;
    Ok(())
}

#[derive(Debug, Clone)]
pub struct RefreshStatus {
    pub success: bool,
    pub exit_code: Option<i32>,
    pub stdout: String,
    pub stderr: String,
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::io::AsyncReadExt;
    use uuid::Uuid;

    fn tmp_dir() -> PathBuf {
        std::env::temp_dir().join(format!("echidnabot-corpus-{}", Uuid::new_v4()))
    }

    fn sample_row(succeeded: bool) -> DeltaRow {
        DeltaRow::new(
            ProverKind::new("coq"),
            "forall x : nat, x + 0 = x".to_string(),
            "rewrite plus_n_O".to_string(),
            succeeded,
            42,
            DeltaSource::Mcp,
        )
    }

    async fn read_file(path: &Path) -> String {
        let mut f = tokio::fs::File::open(path).await.unwrap();
        let mut s = String::new();
        f.read_to_string(&mut s).await.unwrap();
        s
    }

    #[tokio::test]
    async fn record_writes_audit_log_for_all_rows() {
        let dir = tmp_dir();
        let cd = CorpusDelta::new(dir.clone());

        let path = cd.record(&sample_row(true)).await.unwrap();
        assert!(path
            .file_name()
            .unwrap()
            .to_string_lossy()
            .starts_with("delta_"));

        let _ = cd.record(&sample_row(false)).await.unwrap();

        let contents = read_file(&path).await;
        assert_eq!(contents.lines().count(), 2);

        let _ = tokio::fs::remove_dir_all(&dir).await;
    }

    #[tokio::test]
    async fn success_writes_proof_state_entry_with_correct_schema() {
        let dir = tmp_dir();
        let cd = CorpusDelta::new(dir.clone());
        let row = sample_row(true);

        cd.record(&row).await.unwrap();

        let ps_path = cd.proof_state_path_for(row.timestamp);
        assert!(
            ps_path.exists(),
            "proof_states file should have been created"
        );
        assert!(
            ps_path
                .file_name()
                .unwrap()
                .to_string_lossy()
                .starts_with("proof_states_echidnabot_"),
            "filename must match merge_corpus.jl glob"
        );

        let contents = read_file(&ps_path).await;
        assert_eq!(contents.lines().count(), 1);

        let parsed: serde_json::Value = serde_json::from_str(contents.trim()).unwrap();
        assert_eq!(parsed["prover"], "Coq");
        assert_eq!(parsed["theorem"], "forall x : nat, x + 0 = x");
        assert_eq!(parsed["goal"], "forall x : nat, x + 0 = x");
        assert_eq!(parsed["tactic_proof"], "rewrite plus_n_O");
        assert_eq!(parsed["source"], "echidnabot-mcp");

        let _ = tokio::fs::remove_dir_all(&dir).await;
    }

    #[tokio::test]
    async fn failure_does_not_write_proof_state_entry() {
        let dir = tmp_dir();
        let cd = CorpusDelta::new(dir.clone());
        let row = sample_row(false);

        cd.record(&row).await.unwrap();

        let ps_path = cd.proof_state_path_for(row.timestamp);
        assert!(
            !ps_path.exists(),
            "failed proof should not appear in corpus feed"
        );

        let _ = tokio::fs::remove_dir_all(&dir).await;
    }

    #[tokio::test]
    async fn canonical_prover_names_are_title_cased() {
        let cases = [
            ("coq", "Coq"),
            ("lean4", "Lean"),
            ("fstar", "F*"),
            ("z3", "Z3"),
            ("cvc5", "CVC5"),
            ("hol-light", "HOLLight"),
            ("alt-ergo", "Alt-Ergo"),
        ];
        for (slug, expected) in cases {
            assert_eq!(canonical_prover_name(slug), expected, "slug={slug}");
        }
    }

    #[tokio::test]
    async fn trigger_refresh_errors_without_echidna_root() {
        let dir = tmp_dir();
        let cd = CorpusDelta::new(dir.clone());
        let err = cd.trigger_refresh().await.unwrap_err();
        assert!(format!("{}", err).contains("echidna_root not configured"));
        let _ = tokio::fs::remove_dir_all(&dir).await;
    }

    #[tokio::test]
    async fn trigger_refresh_runs_configured_command() {
        let dir = tmp_dir();
        let cd = CorpusDelta::new(dir.clone())
            .with_trigger(std::env::temp_dir())
            .with_trigger_command("/bin/true".to_string(), vec![]);
        let status = cd.trigger_refresh().await.unwrap();
        assert!(status.success);
        let _ = tokio::fs::remove_dir_all(&dir).await;
    }

    #[tokio::test]
    async fn auto_trigger_fires_at_threshold_and_resets_counter() {
        let dir = tmp_dir();
        let cd = CorpusDelta::new(dir.clone())
            .with_trigger(std::env::temp_dir())
            .with_trigger_command("/bin/true".to_string(), vec![])
            .with_auto_trigger(2);

        cd.record(&sample_row(true)).await.unwrap();
        assert_eq!(cd.counter_value().await, 1);
        cd.record(&sample_row(true)).await.unwrap();
        assert_eq!(cd.counter_value().await, 0);

        let _ = tokio::fs::remove_dir_all(&dir).await;
    }

    #[tokio::test]
    async fn failed_rows_do_not_advance_auto_trigger_counter() {
        let dir = tmp_dir();
        let cd = CorpusDelta::new(dir.clone())
            .with_trigger(std::env::temp_dir())
            .with_trigger_command("/bin/true".to_string(), vec![])
            .with_auto_trigger(3);

        cd.record(&sample_row(false)).await.unwrap();
        cd.record(&sample_row(false)).await.unwrap();
        assert_eq!(cd.counter_value().await, 0);
        cd.record(&sample_row(true)).await.unwrap();
        assert_eq!(cd.counter_value().await, 1);

        let _ = tokio::fs::remove_dir_all(&dir).await;
    }
}
