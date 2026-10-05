// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Reranker — blends ECHIDNA's ML-produced confidence with local historical
//! success rate from the `tactic_outcomes` store.
//!
//! Blend policy (Laplace-smoothed):
//!   combined = alpha * base_confidence
//!            + (1 - alpha) * (successes + 1) / (attempts + 2)
//!
//! Lookup precedence:
//!   1. (prover, goal_fingerprint, tactic) — specific-goal history
//!   2. (prover, tactic)                   — global-tactic fallback
//!   3. none                               — base confidence preserved
//!
//! The reranker is side-effect-free — it only reads the store. Recording
//! outcomes after a proof attempt is the caller's responsibility
//! (see `Store::record_tactic_outcome`).

use std::sync::Arc;

use crate::dispatcher::{ProverKind, TacticSuggestion};
use crate::error::Result;
use crate::store::models::goal_fingerprint;
use crate::store::{models::TacticOutcomeRecord, Store};

/// Default blend weight (half base ML confidence, half historical rate).
pub const DEFAULT_ALPHA: f64 = 0.5;

/// Max fingerprint-specific outcomes pulled per rerank call.
pub const DEFAULT_FINGERPRINT_LIMIT: usize = 50;

/// Max global (prover, tactic) fallback outcomes pulled per rerank call.
pub const DEFAULT_GLOBAL_LIMIT: usize = 200;

/// Reranker blending ML confidence with historical success rate.
pub struct Reranker {
    store: Arc<dyn Store>,
    alpha: f64,
    fingerprint_limit: usize,
    global_limit: usize,
}

impl Reranker {
    pub fn new(store: Arc<dyn Store>) -> Self {
        Self {
            store,
            alpha: DEFAULT_ALPHA,
            fingerprint_limit: DEFAULT_FINGERPRINT_LIMIT,
            global_limit: DEFAULT_GLOBAL_LIMIT,
        }
    }

    /// Set the blend weight on base confidence (clamped to 0..=1).
    /// alpha = 1.0 ignores history entirely; alpha = 0.0 trusts history only.
    pub fn with_alpha(mut self, alpha: f64) -> Self {
        self.alpha = alpha.clamp(0.0, 1.0);
        self
    }

    pub fn with_fingerprint_limit(mut self, limit: usize) -> Self {
        self.fingerprint_limit = limit;
        self
    }

    pub fn with_global_limit(mut self, limit: usize) -> Self {
        self.global_limit = limit;
        self
    }

    /// Rerank suggestions. Returns the vec with `confidence` fields updated
    /// and entries sorted by combined confidence descending.
    pub async fn rerank(
        &self,
        prover: &ProverKind,
        goal_state: &str,
        mut suggestions: Vec<TacticSuggestion>,
    ) -> Result<Vec<TacticSuggestion>> {
        if suggestions.is_empty() {
            return Ok(suggestions);
        }

        let fingerprint = goal_fingerprint(goal_state);
        let fingerprint_history = self
            .store
            .list_tactic_outcomes_by_fingerprint(
                prover.clone(),
                &fingerprint,
                self.fingerprint_limit,
            )
            .await?;

        for suggestion in suggestions.iter_mut() {
            suggestion.confidence = self.blend(prover, &fingerprint_history, suggestion).await?;
        }

        suggestions.sort_by(|a, b| {
            b.confidence
                .partial_cmp(&a.confidence)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        Ok(suggestions)
    }

    async fn blend(
        &self,
        prover: &ProverKind,
        fingerprint_history: &[TacticOutcomeRecord],
        suggestion: &TacticSuggestion,
    ) -> Result<f64> {
        let fp_match: Vec<&TacticOutcomeRecord> = fingerprint_history
            .iter()
            .filter(|r| r.tactic == suggestion.tactic)
            .collect();

        let (successes, attempts) = if !fp_match.is_empty() {
            let s = fp_match.iter().filter(|r| r.succeeded).count();
            (s, fp_match.len())
        } else {
            let global = self
                .store
                .list_tactic_outcomes_by_tactic(
                    prover.clone(),
                    &suggestion.tactic,
                    self.global_limit,
                )
                .await?;
            if global.is_empty() {
                return Ok(suggestion.confidence);
            }
            let s = global.iter().filter(|r| r.succeeded).count();
            (s, global.len())
        };

        let success_rate = (successes as f64 + 1.0) / (attempts as f64 + 2.0);
        Ok(self.alpha * suggestion.confidence + (1.0 - self.alpha) * success_rate)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::store::SqliteStore;
    use uuid::Uuid;

    async fn fresh_store() -> (Arc<dyn Store>, std::path::PathBuf) {
        let path =
            std::env::temp_dir().join(format!("echidnabot-rerank-test-{}.db", Uuid::new_v4()));
        let url = format!("sqlite://{}?mode=rwc", path.display());
        let store = SqliteStore::new(&url).await.unwrap();
        (Arc::new(store) as Arc<dyn Store>, path)
    }

    fn sug(tactic: &str, confidence: f64) -> TacticSuggestion {
        TacticSuggestion {
            tactic: tactic.to_string(),
            confidence,
            explanation: None,
        }
    }

    #[tokio::test]
    async fn empty_input_returns_empty() {
        let (store, path) = fresh_store().await;
        let r = Reranker::new(store);
        let out = r
            .rerank(&ProverKind::new("coq"), "goal", vec![])
            .await
            .unwrap();
        assert!(out.is_empty());
        let _ = std::fs::remove_file(&path);
    }

    #[tokio::test]
    async fn no_history_preserves_base_confidence() {
        let (store, path) = fresh_store().await;
        let r = Reranker::new(store);
        let suggestions = vec![sug("intros", 0.7), sug("auto", 0.3)];
        let out = r
            .rerank(&ProverKind::new("coq"), "some goal", suggestions)
            .await
            .unwrap();
        assert_eq!(out.len(), 2);
        // Sorted DESC by confidence
        assert_eq!(out[0].tactic, "intros");
        assert!((out[0].confidence - 0.7).abs() < 1e-9);
        assert_eq!(out[1].tactic, "auto");
        assert!((out[1].confidence - 0.3).abs() < 1e-9);
        let _ = std::fs::remove_file(&path);
    }

    #[tokio::test]
    async fn fingerprint_successes_boost_confidence() {
        let (store, path) = fresh_store().await;
        let goal = "forall x, x = x";
        let fp = goal_fingerprint(goal);

        // 4 successes, 0 failures for "reflexivity" on this fingerprint
        for _ in 0..4 {
            store
                .record_tactic_outcome(&TacticOutcomeRecord::new(
                    None,
                    ProverKind::new("coq"),
                    fp.clone(),
                    "reflexivity".into(),
                    true,
                    1,
                ))
                .await
                .unwrap();
        }

        let r = Reranker::new(store).with_alpha(0.5);
        let out = r
            .rerank(&ProverKind::new("coq"), goal, vec![sug("reflexivity", 0.2)])
            .await
            .unwrap();
        // base=0.2, history=(4+1)/(4+2)=0.833..., alpha=0.5 → 0.5*0.2 + 0.5*0.833 = 0.5166...
        assert!(out[0].confidence > 0.2, "boosted: {}", out[0].confidence);
        assert!(out[0].confidence > 0.5);
        let _ = std::fs::remove_file(&path);
    }

    #[tokio::test]
    async fn fingerprint_failures_depress_confidence() {
        let (store, path) = fresh_store().await;
        let goal = "prove false";
        let fp = goal_fingerprint(goal);

        for _ in 0..5 {
            store
                .record_tactic_outcome(&TacticOutcomeRecord::new(
                    None,
                    ProverKind::new("coq"),
                    fp.clone(),
                    "auto".into(),
                    false,
                    99,
                ))
                .await
                .unwrap();
        }

        let r = Reranker::new(store).with_alpha(0.5);
        let out = r
            .rerank(&ProverKind::new("coq"), goal, vec![sug("auto", 0.9)])
            .await
            .unwrap();
        // base=0.9, history=(0+1)/(5+2)=0.1428..., alpha=0.5 → 0.5*0.9 + 0.5*0.143 = 0.5214
        assert!(out[0].confidence < 0.9, "depressed: {}", out[0].confidence);
        assert!(out[0].confidence < 0.6);
        let _ = std::fs::remove_file(&path);
    }

    #[tokio::test]
    async fn alpha_one_ignores_history() {
        let (store, path) = fresh_store().await;
        let goal = "g";
        let fp = goal_fingerprint(goal);
        for _ in 0..10 {
            store
                .record_tactic_outcome(&TacticOutcomeRecord::new(
                    None,
                    ProverKind::new("coq"),
                    fp.clone(),
                    "t".into(),
                    false,
                    1,
                ))
                .await
                .unwrap();
        }
        let r = Reranker::new(store).with_alpha(1.0);
        let out = r
            .rerank(&ProverKind::new("coq"), goal, vec![sug("t", 0.77)])
            .await
            .unwrap();
        assert!((out[0].confidence - 0.77).abs() < 1e-9);
        let _ = std::fs::remove_file(&path);
    }

    #[tokio::test]
    async fn global_fallback_when_no_fingerprint_match() {
        let (store, path) = fresh_store().await;
        // "tac" succeeded on a different goal
        let other = goal_fingerprint("other goal");
        for _ in 0..3 {
            store
                .record_tactic_outcome(&TacticOutcomeRecord::new(
                    None,
                    ProverKind::new("coq"),
                    other.clone(),
                    "tac".into(),
                    true,
                    1,
                ))
                .await
                .unwrap();
        }

        let r = Reranker::new(store).with_alpha(0.0); // pure history
        let out = r
            .rerank(&ProverKind::new("coq"), "fresh goal", vec![sug("tac", 0.1)])
            .await
            .unwrap();
        // Fingerprint lookup misses → global fallback: (3+1)/(3+2)=0.8
        // alpha=0 → confidence = 0.8 exactly
        assert!(
            (out[0].confidence - 0.8).abs() < 1e-6,
            "got {}",
            out[0].confidence
        );
        let _ = std::fs::remove_file(&path);
    }

    #[tokio::test]
    async fn reranker_sorts_by_combined_score() {
        let (store, path) = fresh_store().await;
        let goal = "g";
        let fp = goal_fingerprint(goal);
        // "good" has 5/5 successes; "bad" has 0/5.
        for _ in 0..5 {
            store
                .record_tactic_outcome(&TacticOutcomeRecord::new(
                    None,
                    ProverKind::new("coq"),
                    fp.clone(),
                    "good".into(),
                    true,
                    1,
                ))
                .await
                .unwrap();
            store
                .record_tactic_outcome(&TacticOutcomeRecord::new(
                    None,
                    ProverKind::new("coq"),
                    fp.clone(),
                    "bad".into(),
                    false,
                    1,
                ))
                .await
                .unwrap();
        }

        // Input: "bad" has higher base confidence than "good".
        let r = Reranker::new(store).with_alpha(0.3);
        let out = r
            .rerank(
                &ProverKind::new("coq"),
                goal,
                vec![sug("bad", 0.9), sug("good", 0.1)],
            )
            .await
            .unwrap();
        // History flips the ranking: "good" should surface above "bad".
        assert_eq!(out[0].tactic, "good");
        assert_eq!(out[1].tactic, "bad");
        let _ = std::fs::remove_file(&path);
    }
}
