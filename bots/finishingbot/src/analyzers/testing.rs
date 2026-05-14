// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Testing infrastructure analyzer

use super::{AnalysisResult, Analyzer, Finding, Severity};
use crate::config::Config;
use crate::error::Result;
use std::path::Path;
use tracing::debug;

/// Testing infrastructure analyzer
pub struct TestingAnalyzer;

impl Default for TestingAnalyzer {
    fn default() -> Self {
        Self
    }
}

impl Analyzer for TestingAnalyzer {
    fn name(&self) -> &str {
        "Testing Infrastructure"
    }

    fn analyze(&self, path: &Path, _config: &Config) -> Result<AnalysisResult> {
        let mut result = AnalysisResult::new();
        let start = std::time::Instant::now();

        let is_rust = path.join("Cargo.toml").exists();
        let is_deno = path.join("deno.json").exists();

        if is_rust {
            self.check_rust_testing(path, &mut result);
        } else if is_deno {
            self.check_deno_testing(path, &mut result);
        } else {
            result.add(
                Finding::new(
                    "TEST-001",
                    "Unknown project type",
                    Severity::Warning,
                    "Could not determine if this is a Rust or Deno project.",
                )
                .with_suggestion("Ensure Cargo.toml or deno.json exists"),
            );
        }

        result.duration_ms = start.elapsed().as_millis() as u64;
        Ok(result)
    }

    fn fix(&self, _path: &Path, _config: &Config, _findings: &[Finding]) -> Result<Vec<String>> {
        Ok(vec![
            "Testing infrastructure must be manually created.".to_string(),
            "Add benchmarks, stress tests, and fuzzing as appropriate.".to_string(),
        ])
    }
}

impl TestingAnalyzer {
    /// Check Rust project testing infrastructure
    fn check_rust_testing(&self, path: &Path, result: &mut AnalysisResult) {
        // Check for benchmarks
        let benches_dir = path.join("benches");
        if !benches_dir.exists() {
            result.add(
                Finding::new(
                    "TEST-RU-001",
                    "Missing benchmarks directory",
                    Severity::Error,
                    "Rust project missing benches/ directory for criterion benchmarks.",
                )
                .with_suggestion("Create benches/ directory with criterion benchmark files"),
            );
        } else {
            debug!("Found benches/ directory");

            // Check if benches directory has content
            if let Ok(entries) = std::fs::read_dir(&benches_dir) {
                let has_benches = entries
                    .filter_map(|e| e.ok())
                    .any(|e| e.path().extension().map_or(false, |ext| ext == "rs"));

                if !has_benches {
                    result.add(
                        Finding::new(
                            "TEST-RU-002",
                            "Empty benchmarks directory",
                            Severity::Warning,
                            "benches/ directory exists but contains no .rs files.",
                        )
                        .with_suggestion("Add benchmark files to benches/ directory"),
                    );
                }
            }
        }

        // Check for fuzzing (cargo-fuzz or ClusterFuzzLite)
        let fuzz_dir = path.join("fuzz");
        let clusterfuzz_dir = path.join(".clusterfuzzlite");

        if !fuzz_dir.exists() && !clusterfuzz_dir.exists() {
            result.add(
                Finding::new(
                    "TEST-RU-003",
                    "Missing fuzzing infrastructure",
                    Severity::Error,
                    "Rust project missing fuzz/ or .clusterfuzzlite/ for fuzzing.",
                )
                .with_suggestion("Add ClusterFuzzLite configuration for automated fuzzing"),
            );
        } else {
            debug!("Found fuzzing infrastructure");

            if clusterfuzz_dir.exists() {
                // Validate ClusterFuzzLite setup
                if !clusterfuzz_dir.join("project.yaml").exists() {
                    result.add(
                        Finding::new(
                            "TEST-RU-004",
                            "Incomplete ClusterFuzzLite setup",
                            Severity::Error,
                            ".clusterfuzzlite/project.yaml not found.",
                        )
                        .with_file(clusterfuzz_dir.to_path_buf())
                        .with_suggestion("Add project.yaml, Dockerfile, and build.sh to .clusterfuzzlite/"),
                    );
                }
            }
        }

        // Check for stress test workflow
        let stress_workflow = path.join(".github/workflows/stress-test.yml");
        if !stress_workflow.exists() {
            result.add(
                Finding::new(
                    "TEST-RU-005",
                    "Missing stress test workflow",
                    Severity::Error,
                    "No .github/workflows/stress-test.yml found.",
                )
                .with_suggestion("Add stress test workflow to test under high load"),
            );
        } else {
            debug!("Found stress test workflow");
        }

        // Check Cargo.toml for criterion dependency
        if let Ok(content) = std::fs::read_to_string(path.join("Cargo.toml")) {
            if !content.contains("criterion") {
                result.add(
                    Finding::new(
                        "TEST-RU-006",
                        "Missing criterion dependency",
                        Severity::Warning,
                        "Cargo.toml does not include criterion for benchmarking.",
                    )
                    .with_file(path.join("Cargo.toml"))
                    .with_suggestion("Add criterion = \"0.5\" to [dev-dependencies]"),
                );
            }
        }
    }

    /// Check Deno project testing infrastructure
    fn check_deno_testing(&self, path: &Path, result: &mut AnalysisResult) {
        // Check for benchmarks
        let bench_dir = path.join("bench");
        let bench_file = path.join("bench.ts");

        if !bench_dir.exists() && !bench_file.exists() {
            result.add(
                Finding::new(
                    "TEST-DN-001",
                    "Missing benchmarks",
                    Severity::Error,
                    "Deno project missing bench/ directory or bench.ts file.",
                )
                .with_suggestion("Create bench/ directory with Deno.bench() benchmarks"),
            );
        } else {
            debug!("Found Deno benchmarks");
        }

        // Check for stress test workflow
        let stress_workflow = path.join(".github/workflows/stress-test.yml");
        if !stress_workflow.exists() {
            result.add(
                Finding::new(
                    "TEST-DN-002",
                    "Missing stress test workflow",
                    Severity::Error,
                    "No .github/workflows/stress-test.yml found.",
                )
                .with_suggestion("Add stress test workflow for load testing"),
            );
        } else {
            debug!("Found stress test workflow");
        }

        // Deno projects don't typically use fuzzing, but check for tests
        let tests_dir = path.join("tests");
        let test_files_exist = std::fs::read_dir(path)
            .ok()
            .and_then(|entries| {
                entries
                    .filter_map(|e| e.ok())
                    .find(|e| {
                        e.path()
                            .file_name()
                            .and_then(|n| n.to_str())
                            .map_or(false, |n| n.ends_with("_test.ts") || n.ends_with(".test.ts"))
                    })
            })
            .is_some();

        if !tests_dir.exists() && !test_files_exist {
            result.add(
                Finding::new(
                    "TEST-DN-003",
                    "Missing test files",
                    Severity::Warning,
                    "No tests/ directory or *_test.ts files found.",
                )
                .with_suggestion("Add test files following Deno conventions (*_test.ts)"),
            );
        }
    }
}
