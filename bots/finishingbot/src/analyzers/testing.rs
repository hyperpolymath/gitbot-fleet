// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Testing infrastructure analyzer

use super::{AnalysisResult, Analyzer, Finding, Severity};
use crate::config::Config;
use crate::error::Result;
use std::path::Path;
use tracing::debug;

// Conservative recognition of a direct command, not a shell/YAML parser and
// not evidence that the compiler ran successfully.
fn is_direct_agda_source_check(command: &str) -> bool {
    let mut tokens = command.split_whitespace();
    if tokens.next() != Some("agda") {
        return false;
    }
    let args: Vec<_> = tokens.collect();
    let informational = args.iter().any(|arg| {
        matches!(*arg, "--version" | "-V" | "--help" | "-h" | "-?")
            || arg.starts_with("--help=")
            || arg.starts_with("--print-")
    });
    !informational
        && args.iter().any(|arg| {
            let operand = arg.trim_matches(['\'', '"']);
            !operand.starts_with('-') && operand.ends_with(".agda")
        })
}

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
        let is_agda = path.join("proofs/agda").is_dir()
            || walkdir::WalkDir::new(path.join("verification/proofs"))
                .follow_links(false)
                .into_iter()
                .filter_map(|entry| entry.ok())
                .any(|entry| {
                    entry.file_type().is_file()
                        && entry
                            .path()
                            .extension()
                            .is_some_and(|extension| extension == "agda")
                });

        if is_rust {
            self.check_rust_testing(path, &mut result);
        }
        if is_deno {
            self.check_deno_testing(path, &mut result);
        }
        if is_agda {
            self.check_agda_testing(path, &mut result);
        }
        if !is_rust && !is_deno && !is_agda {
            result.add(
                Finding::new(
                    "TEST-001",
                    "Unknown project type",
                    Severity::Warning,
                    "No supported Rust, Deno, or Agda source layout was identified.",
                )
                .with_suggestion("Document the actual verification entrypoint; do not add an unrelated language manifest"),
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
    /// Recognize Agda source and direct check infrastructure. No prover or
    /// test is executed here, and command discovery is deliberately limited
    /// to single-line direct calls in known workflow/recipe locations.
    fn check_agda_testing(&self, path: &Path, result: &mut AnalysisResult) {
        let has_source = ["proofs/agda", "verification/proofs"].iter().any(|root| {
            walkdir::WalkDir::new(path.join(root))
                .follow_links(false)
                .into_iter()
                .filter_map(|entry| entry.ok())
                .any(|entry| {
                    entry.file_type().is_file()
                        && entry
                            .path()
                            .extension()
                            .is_some_and(|extension| extension == "agda")
                        && entry.metadata().is_ok_and(|metadata| metadata.len() > 0)
                })
        });
        if !has_source {
            result.add(Finding::new(
                "TEST-AG-001", "Missing Agda source", Severity::Error,
                "The proof layout contains no nonempty .agda source files.",
            ).with_suggestion("Provide actual proof sources; an empty directory is not verification infrastructure"));
        }
        let has_entrypoint = [
            ".github/workflows/agda.yml",
            ".github/workflows/agda.yaml",
            "Justfile",
            "justfile",
            "scripts/verify.sh",
            "scripts/check-agda.sh",
        ]
        .iter()
        .any(|file| {
            std::fs::read_to_string(path.join(file)).is_ok_and(|content| {
                content.lines().any(|line| {
                    let command = line.split('#').next().unwrap_or("").trim();
                    let command = command.strip_prefix("- ").unwrap_or(command);
                    let command = command.strip_prefix("run:").unwrap_or(command).trim();
                    is_direct_agda_source_check(command)
                })
            })
        });
        if !has_entrypoint {
            result.add(Finding::new(
                "TEST-AG-002", "Agda runner not statically identified", Severity::Warning,
                "No direct single-line Agda source check was identified in the known workflow or recipe locations. Dynamic runners need separate review; no proof was executed.",
            ).with_suggestion("Provide or document the real Agda check entrypoint instead of adding Rust or Deno scaffolding"));
        }
    }

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
                    .any(|e| e.path().extension().is_some_and(|ext| ext == "rs"));

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
        let tests_fuzz_dir = path.join("tests/fuzz");
        let clusterfuzz_dir = path.join(".clusterfuzzlite");

        if !has_fuzz_harness(&fuzz_dir)
            && !has_fuzz_harness(&tests_fuzz_dir)
            && !clusterfuzz_dir.exists()
        {
            result.add(
                Finding::new(
                    "TEST-RU-003",
                    "Missing fuzzing infrastructure",
                    Severity::Error,
                    "Rust project has no fuzz harness: expected fuzz/ or tests/fuzz/ with a Cargo.toml declaring [[bin]] targets or fuzz_targets/*.rs using fuzz_target!, or .clusterfuzzlite/.",
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
                        .with_suggestion(
                            "Add project.yaml, Dockerfile, and build.sh to .clusterfuzzlite/",
                        ),
                    );
                }
            }
        }

        // A crate with no code yet has nothing to stress or benchmark, so
        // TEST-RU-005 and TEST-RU-006 wait until there is some (see `rust_has_code`).
        let has_code = rust_has_code(path);

        // Check for stress test workflow
        let stress_workflow = path.join(".github/workflows/stress-test.yml");
        if !stress_workflow.exists() && has_code {
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
            if has_code && !content.contains("criterion") {
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
                entries.filter_map(|e| e.ok()).find(|e| {
                    e.path()
                        .file_name()
                        .and_then(|n| n.to_str())
                        .is_some_and(|n| n.ends_with("_test.ts") || n.ends_with(".test.ts"))
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

#[cfg(test)]
mod agda_layout_tests {
    use super::*;
    use tempfile::TempDir;

    fn write(root: &Path, relative: &str, content: &str) {
        let target = root.join(relative);
        std::fs::create_dir_all(target.parent().unwrap()).unwrap();
        std::fs::write(target, content).unwrap();
    }
    fn analyze(root: &Path) -> AnalysisResult {
        TestingAnalyzer.analyze(root, &Config::default()).unwrap()
    }
    #[test]
    fn real_agda_source_and_workflow_are_recognized_without_rust_or_deno() {
        let root = TempDir::new().unwrap();
        write(
            root.path(),
            "proofs/agda/Witness.agda",
            "module Witness where\ndata Unit : Set where unit : Unit\n",
        );
        write(root.path(), ".github/workflows/agda.yml", "steps:\n  - run: agda --safe --without-K --cubical-compatible -i proofs/agda proofs/agda/Witness.agda\n");
        assert!(analyze(root.path()).findings.is_empty());
    }
    #[test]
    fn empty_or_prose_only_proof_layout_is_not_successful_infrastructure() {
        let root = TempDir::new().unwrap();
        write(root.path(), "proofs/agda/README.adoc", "Intended proofs\n");
        write(root.path(), "proofs/agda/Empty.agda", "");
        let result = analyze(root.path());
        assert!(result
            .findings
            .iter()
            .any(|finding| finding.id == "TEST-AG-001"));
        assert!(result
            .findings
            .iter()
            .any(|finding| finding.id == "TEST-AG-002"));
    }
    #[test]
    fn comments_and_version_checks_are_not_proof_runners() {
        let root = TempDir::new().unwrap();
        write(
            root.path(),
            "verification/proofs/Witness.agda",
            "module Witness where\n",
        );
        write(
            root.path(),
            ".github/workflows/agda.yml",
            "# agda --safe Witness.agda\nrun: agda --version # Witness.agda\n",
        );
        assert!(analyze(root.path())
            .findings
            .iter()
            .any(|finding| finding.id == "TEST-AG-002"));
    }
    #[test]
    fn informational_modes_with_source_operands_are_not_proof_runners() {
        let root = TempDir::new().unwrap();
        write(
            root.path(),
            "proofs/agda/Witness.agda",
            "module Witness where\n",
        );
        for mode in [
            "--version",
            "-V",
            "--help",
            "--help=warning",
            "-h",
            "-?",
            "--print-dir",
            "--print-agda-dir",
        ] {
            write(
                root.path(),
                ".github/workflows/agda.yml",
                &format!("run: agda {mode} Witness.agda # actual-looking operand\n"),
            );
            assert!(
                analyze(root.path())
                    .findings
                    .iter()
                    .any(|finding| finding.id == "TEST-AG-002"),
                "{mode} must not count as a source check"
            );
        }
    }
    #[test]
    fn an_agda_substring_or_option_value_is_not_a_source_operand() {
        assert!(!is_direct_agda_source_check("agda --safe Witness.agda.bak"));
        assert!(!is_direct_agda_source_check("agda --output=Generated.agda"));
        assert!(is_direct_agda_source_check(
            "agda --safe 'proofs/agda/Witness.agda'"
        ));
    }
    #[test]
    fn an_empty_or_unrecognized_project_does_not_gain_an_agda_pass() {
        let root = TempDir::new().unwrap();
        assert!(analyze(root.path())
            .findings
            .iter()
            .any(|finding| finding.id == "TEST-001"));
    }
}

/// Remove Rust block comments, which nest (`/* a /* b */ c */`).
///
/// `/*` only opens a comment in code: not inside a `//` line comment (to the
/// end of the line), a string literal (`"..."` with escapes, `r#"..."#`) or a
/// char literal. Line comments and strings are kept verbatim; inside a block
/// comment only newlines are kept, so line numbers survive.
fn strip_block_comments(src: &str) -> String {
    let b = src.as_bytes();
    let mut out = Vec::with_capacity(b.len());
    let (mut i, mut depth) = (0, 0usize);
    while i < b.len() {
        if depth > 0 {
            if b[i] == b'/' && b.get(i + 1) == Some(&b'*') {
                depth += 1;
                i += 2;
            } else if b[i] == b'*' && b.get(i + 1) == Some(&b'/') {
                depth -= 1;
                i += 2;
            } else {
                if b[i] == b'\n' {
                    out.push(b'\n');
                }
                i += 1;
            }
            continue;
        }
        match b[i] {
            b'/' if b.get(i + 1) == Some(&b'*') => {
                depth = 1;
                i += 2;
            }
            b'/' if b.get(i + 1) == Some(&b'/') => {
                // Line comment: copy to (not including) the newline.
                while i < b.len() && b[i] != b'\n' {
                    out.push(b[i]);
                    i += 1;
                }
            }
            b'r' if raw_string_hashes(b, i).is_some() => {
                let hashes = raw_string_hashes(b, i).unwrap_or(0);
                // r, hashes, opening quote.
                let open = 1 + hashes + 1;
                out.extend_from_slice(&b[i..i + open]);
                i += open;
                while i < b.len() {
                    let closes = b[i] == b'"' && (1..=hashes).all(|k| b.get(i + k) == Some(&b'#'));
                    out.push(b[i]);
                    i += 1;
                    if closes {
                        out.extend_from_slice(&b[i..i + hashes]);
                        i += hashes;
                        break;
                    }
                }
            }
            b'"' => {
                out.push(b'"');
                i += 1;
                while i < b.len() {
                    let c = b[i];
                    out.push(c);
                    i += 1;
                    if c == b'\\' && i < b.len() {
                        out.push(b[i]);
                        i += 1;
                    } else if c == b'"' {
                        break;
                    }
                }
            }
            b'\'' => {
                // A char literal ('x', '\n', '"'); a lifetime ('a) has no
                // closing quote two or three bytes on and is left alone.
                let end = match (b.get(i + 1), b.get(i + 2), b.get(i + 3)) {
                    (Some(b'\\'), _, Some(b'\'')) => Some(i + 4),
                    (Some(c), Some(b'\''), _) if *c != b'\\' => Some(i + 3),
                    _ => None,
                };
                let end = end.unwrap_or(i + 1);
                out.extend_from_slice(&b[i..end]);
                i = end;
            }
            c => {
                out.push(c);
                i += 1;
            }
        }
    }
    String::from_utf8_lossy(&out).into_owned()
}

/// If a raw string literal (`r"`, `r#"`, ...) starts at `b[i]` (an `r` that is
/// not the tail of an identifier), the number of `#`s in its delimiter.
fn raw_string_hashes(b: &[u8], i: usize) -> Option<usize> {
    if i > 0 && (b[i - 1].is_ascii_alphanumeric() || b[i - 1] == b'_') {
        return None;
    }
    let hashes = b[i + 1..].iter().take_while(|&&c| c == b'#').count();
    (b.get(i + 1 + hashes) == Some(&b'"')).then_some(hashes)
}

/// Whether the Rust project at `root` has any code yet.
///
/// "No code" is exact: no `.rs` file outside build output (`target/`),
/// vendored trees, `node_modules/` and hidden directories contains a line that
/// is neither blank nor a comment. An empty `src/lib.rs`, one holding only
/// `//!` docs or an SPDX header, and a crate with no `.rs` files at all, are
/// all "no code"; `fn main() {}` is code. Used to exempt benchmark and stress
/// requirements from a freshly scaffolded crate.
pub(crate) fn rust_has_code(root: &Path) -> bool {
    walkdir::WalkDir::new(root)
        .follow_links(false)
        .into_iter()
        .filter_entry(|e| {
            if e.depth() == 0 || !e.file_type().is_dir() {
                return true;
            }
            let name = e.file_name().to_string_lossy();
            !(name.starts_with('.')
                || matches!(name.as_ref(), "target" | "vendor" | "node_modules"))
        })
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file() && e.path().extension().is_some_and(|x| x == "rs"))
        .any(|e| {
            std::fs::read_to_string(e.path()).is_ok_and(|src| {
                strip_block_comments(&src)
                    .lines()
                    .map(str::trim)
                    .any(|l| !l.is_empty() && !l.starts_with("//"))
            })
        })
}

/// True when `dir` (a `fuzz/` or `tests/fuzz/` directory) holds a recognisable
/// harness: a `Cargo.toml` with a real `[[bin]]` table whose target file exists
/// (cargo-fuzz layout), or a `fuzz_targets/*.rs` file that invokes
/// `fuzz_target!` outside comments. An empty directory, a placeholder file or a
/// commented-out declaration is not fuzzing infrastructure.
fn has_fuzz_harness(dir: &Path) -> bool {
    if let Ok(manifest) = std::fs::read_to_string(dir.join("Cargo.toml")) {
        if manifest_has_existing_bin(dir, &manifest) {
            return true;
        }
    }
    let Ok(entries) = std::fs::read_dir(dir.join("fuzz_targets")) else {
        return false;
    };
    entries.filter_map(|e| e.ok()).any(|e| {
        let p = e.path();
        p.extension().is_some_and(|x| x == "rs")
            && std::fs::read_to_string(&p).is_ok_and(|s| invokes_fuzz_target(&s))
    })
}

/// True when `src` invokes the `fuzz_target!` macro in code: not in a `//` or
/// `/* */` comment, and followed (after optional whitespace) by `(`, `{` or `|`.
fn invokes_fuzz_target(src: &str) -> bool {
    const NAME: &str = "fuzz_target!";
    strip_block_comments(src).lines().any(|line| {
        let code = line.split("//").next().unwrap_or("");
        code.match_indices(NAME).any(|(at, _)| {
            code[at + NAME.len()..]
                .trim_start()
                .starts_with(['(', '{', '|'])
        })
    })
}

/// True when the manifest has a `[[bin]]` table header on a non-comment line
/// whose target file exists under `dir`: its `path` if given, else the cargo
/// default for the named target (`src/bin/<name>.rs`, `src/bin/<name>/main.rs`,
/// or `src/main.rs`).
fn manifest_has_existing_bin(dir: &Path, manifest: &str) -> bool {
    let mut tables: Vec<(Option<String>, Option<String>)> = Vec::new();
    let mut in_bin = false;
    for raw in manifest.lines() {
        let line = raw.split('#').next().unwrap_or("").trim();
        if line.starts_with('[') {
            in_bin = line == "[[bin]]";
            if in_bin {
                tables.push((None, None));
            }
        } else if in_bin {
            if let Some((key, value)) = line.split_once('=') {
                let value = value.trim().trim_matches(['"', '\'']).to_string();
                if let Some(t) = tables.last_mut() {
                    match key.trim() {
                        "path" => t.0 = Some(value),
                        "name" => t.1 = Some(value),
                        _ => {}
                    }
                }
            }
        }
    }
    tables.iter().any(|(path, name)| match (path, name) {
        (Some(p), _) => dir.join(p).is_file(),
        (None, Some(n)) => {
            dir.join(format!("src/bin/{n}.rs")).is_file()
                || dir.join(format!("src/bin/{n}/main.rs")).is_file()
                || dir.join("src/main.rs").is_file()
        }
        (None, None) => dir.join("src/main.rs").is_file(),
    })
}

#[cfg(test)]
mod no_code_tests {
    use super::*;
    use tempfile::TempDir;

    fn write(dir: &Path, rel: &str, body: &str) {
        let p = dir.join(rel);
        std::fs::create_dir_all(p.parent().unwrap()).unwrap();
        std::fs::write(p, body).unwrap();
    }

    fn rust_repo(lib: Option<&str>) -> TempDir {
        let d = TempDir::new().unwrap();
        write(
            d.path(),
            "Cargo.toml",
            "[package]\nname = \"x\"\nversion = \"0.1.0\"\n",
        );
        if let Some(body) = lib {
            write(d.path(), "src/lib.rs", body);
        }
        d
    }

    fn ids(dir: &Path) -> Vec<String> {
        TestingAnalyzer
            .analyze(dir, &Config::default())
            .unwrap()
            .findings
            .into_iter()
            .map(|f| f.id)
            .collect()
    }

    #[test]
    fn no_code_means_no_non_comment_line_in_any_rs_file() {
        for body in [
            None,
            Some(""),
            Some("// SPDX-License-Identifier: MPL-2.0\n\n//! docs only\n"),
            Some("/* block\n comment */\n// c\n"),
        ] {
            let d = rust_repo(body);
            assert!(!rust_has_code(d.path()), "{body:?}");
        }
        for body in [
            "fn main() {}\n",
            "// c\npub fn f() {}\n",
            "/* c */ pub struct S;\n",
        ] {
            let d = rust_repo(Some(body));
            assert!(rust_has_code(d.path()), "{body:?}");
        }
    }

    #[test]
    fn a_block_comment_opener_inside_a_line_comment_or_string_is_not_a_comment() {
        // `/*` after `//` is line-comment text, not the start of block mode.
        for body in [
            "// block comments start with /*\npub fn f() {}\n",
            "/// docs mention /* here\npub fn f() {}\n",
            "pub fn f() {} // trailing /* note\n",
            "const S: &str = \"/*\";\n",
            "const R: &str = r#\"/* \"#;\n",
            "const C: char = '\"';\nconst S: &str = \"/*\";\n",
        ] {
            let d = rust_repo(Some(body));
            assert!(rust_has_code(d.path()), "{body:?}");
        }
        // Real block comments still hide their contents, including a `//`
        // inside them, and `//` inside a string does not hide a real opener.
        for body in [
            "/* // not a line comment */\n// c\n",
            "/* a\n// b\nc */\n",
            "// only /* a comment\n// and another\n",
        ] {
            let d = rust_repo(Some(body));
            assert!(!rust_has_code(d.path()), "{body:?}");
        }
        let d = rust_repo(Some("let u = \"http://x\"; /* real\ncomment */\n"));
        assert!(rust_has_code(d.path()), "code before the opener");
        let d = rust_repo(Some("/* c */ // x /* y\n"));
        assert!(!rust_has_code(d.path()));
    }

    #[test]
    fn nested_block_comments_are_stripped() {
        let d = rust_repo(Some("/* a /* b */ c */\n// x\n"));
        assert!(!rust_has_code(d.path()));
        let d = rust_repo(Some("/* a /* b */ c */ fn f() {}\n"));
        assert!(rust_has_code(d.path()));
    }

    #[test]
    fn code_in_build_output_vendor_or_hidden_dirs_does_not_count() {
        let d = rust_repo(Some("// nothing\n"));
        write(d.path(), "target/debug/build/x.rs", "fn a() {}\n");
        write(d.path(), "vendor/dep/lib.rs", "fn b() {}\n");
        write(d.path(), ".claude/worktrees/w/src/lib.rs", "fn c() {}\n");
        assert!(!rust_has_code(d.path()));
    }

    #[test]
    fn stress_and_criterion_are_exempt_for_a_crate_with_no_code() {
        let d = rust_repo(Some("// SPDX only\n"));
        let got = ids(d.path());
        assert!(!got.contains(&"TEST-RU-005".to_string()), "{got:?}");
        assert!(!got.contains(&"TEST-RU-006".to_string()), "{got:?}");
    }

    #[test]
    fn stress_and_criterion_still_required_once_there_is_code() {
        let d = rust_repo(Some("pub fn f() {}\n"));
        let got = ids(d.path());
        assert!(got.contains(&"TEST-RU-005".to_string()), "{got:?}");
        assert!(got.contains(&"TEST-RU-006".to_string()), "{got:?}");
    }

    #[test]
    fn fuzz_under_tests_satisfies_ru_003() {
        let d = rust_repo(Some("pub fn f() {}\n"));
        assert!(ids(d.path()).contains(&"TEST-RU-003".to_string()));
        // A placeholder is not a harness.
        write(d.path(), "tests/fuzz/README", "todo\n");
        write(d.path(), "tests/fuzz/fuzz_targets/t.rs", "// target\n");
        assert!(ids(d.path()).contains(&"TEST-RU-003".to_string()));
        write(
            d.path(),
            "tests/fuzz/fuzz_targets/t.rs",
            "#![no_main]\nlibfuzzer_sys::fuzz_target!(|data: &[u8]| { let _ = data; });\n",
        );
        assert!(!ids(d.path()).contains(&"TEST-RU-003".to_string()));
    }

    #[test]
    fn fuzz_cargo_manifest_with_bin_targets_satisfies_ru_003() {
        let d = rust_repo(Some("pub fn f() {}\n"));
        write(d.path(), "fuzz/.gitkeep", "");
        assert!(ids(d.path()).contains(&"TEST-RU-003".to_string()));
        write(
            d.path(),
            "fuzz/Cargo.toml",
            "[package]\nname = \"x-fuzz\"\n",
        );
        assert!(ids(d.path()).contains(&"TEST-RU-003".to_string()));
        write(
            d.path(),
            "fuzz/Cargo.toml",
            "[package]\nname = \"x-fuzz\"\n\n[[bin]]\nname = \"t\"\npath = \"fuzz_targets/t.rs\"\n",
        );
        write(d.path(), "fuzz/fuzz_targets/t.rs", "fn main() {}\n");
        assert!(!ids(d.path()).contains(&"TEST-RU-003".to_string()));
    }

    #[test]
    fn comment_only_fuzz_target_mention_is_not_a_harness() {
        let d = rust_repo(Some("pub fn f() {}\n"));
        for body in [
            "// TODO: add fuzz_target!\n",
            "/* fuzz_target!(|d: &[u8]| {}); */\n",
            "// fuzz_target!(|d: &[u8]| {});\nfn main() {}\n",
            "fn main() {} // fuzz_target!(|d: &[u8]| {})\n",
            "fn fuzz_target_name() {}\n",
        ] {
            write(d.path(), "fuzz/fuzz_targets/t.rs", body);
            assert!(
                ids(d.path()).contains(&"TEST-RU-003".to_string()),
                "{body:?}"
            );
        }
        for body in [
            "fuzz_target!(|d: &[u8]| {});\n",
            "libfuzzer_sys::fuzz_target! { |d: &[u8]| { let _ = d; } }\n",
            "/* c */ fuzz_target! (|d: &[u8]| {});\n",
        ] {
            write(d.path(), "fuzz/fuzz_targets/t.rs", body);
            assert!(
                !ids(d.path()).contains(&"TEST-RU-003".to_string()),
                "{body:?}"
            );
        }
    }

    #[test]
    fn commented_or_dangling_bin_table_is_not_a_harness() {
        let d = rust_repo(Some("pub fn f() {}\n"));
        write(d.path(), "fuzz/fuzz_targets/t.rs", "// nothing\n");
        for manifest in [
            "[package]\nname = \"x\"\n# [[bin]]\n# name = \"t\"\n",
            "[package]\nname = \"x\"\n\n[[bin]] # real header, missing file\nname = \"t\"\npath = \"fuzz_targets/missing.rs\"\n",
            "[package]\nname = \"x\"\n\n[[bin]]\nname = \"t\"\n",
        ] {
            write(d.path(), "fuzz/Cargo.toml", manifest);
            assert!(ids(d.path()).contains(&"TEST-RU-003".to_string()), "{manifest:?}");
        }
        write(d.path(), "fuzz/src/bin/t.rs", "fn main() {}\n");
        write(
            d.path(),
            "fuzz/Cargo.toml",
            "[package]\nname = \"x\"\n\n[[bin]]\nname = \"t\"\n",
        );
        assert!(!ids(d.path()).contains(&"TEST-RU-003".to_string()));
    }
}
