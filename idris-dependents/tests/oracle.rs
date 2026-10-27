// SPDX-License-Identifier: MPL-2.0
//! Differential test against Idris2 itself: for the real fixtures and for
//! generated files, `depends()` must agree with `idris2 --dump-ipkg-json`.
//!
//! Runs only when `IDRIS2` names an Idris2 binary new enough to have
//! `--dump-ipkg-json` (Idris2 `main`, not 0.7.0); otherwise it is skipped.
//! `ORACLE_CASES` sets how many generated files to check (default 300).

mod common;

use std::path::Path;
use std::process::Command;

use idris_dependents::ipkg::{depends, package_name};
use proptest::strategy::{Strategy, ValueTree};
use proptest::test_runner::TestRunner;

/// (package name, depends) as Idris2 reads the file, or Idris2's error.
fn idris_reads(idris2: &str, file: &Path) -> Result<(String, Vec<String>), String> {
    let out = Command::new(idris2)
        .arg("--dump-ipkg-json")
        .arg(file)
        .output()
        .map_err(|e| e.to_string())?;
    let stdout = String::from_utf8_lossy(&out.stdout);
    if !out.status.success() {
        return Err(format!("{stdout}{}", String::from_utf8_lossy(&out.stderr)));
    }
    let v: serde_json::Value =
        serde_json::from_str(stdout.trim()).map_err(|e| format!("{e}: {stdout}"))?;
    let name = v["name"].as_str().unwrap_or_default().to_string();
    let deps = v["depends"]
        .as_array()
        .map(|a| {
            a.iter()
                .filter_map(|d| d.as_object()?.keys().next().cloned())
                .collect()
        })
        .unwrap_or_default();
    Ok((name, deps))
}

#[test]
fn agrees_with_idris2() {
    let Ok(idris2) = std::env::var("IDRIS2") else {
        eprintln!("skipped: set IDRIS2 to an idris2 binary with --dump-ipkg-json");
        return;
    };
    let n: usize = std::env::var("ORACLE_CASES")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(300);
    let dir = std::env::temp_dir().join(format!("idris-dependents-oracle-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();

    let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");
    let mut cases: Vec<(String, String)> = std::fs::read_dir(&fixtures)
        .unwrap()
        .filter_map(|e| e.ok()?.path().to_str().map(String::from))
        .filter(|p| p.ends_with(".ipkg"))
        .map(|p| (p.clone(), std::fs::read_to_string(&p).unwrap()))
        .collect();
    let mut runner = TestRunner::deterministic();
    for i in 0..n {
        cases.push((
            format!("generated #{i}"),
            common::ipkg().new_tree(&mut runner).unwrap().current().text,
        ));
    }

    let mut mismatches = Vec::new();
    for (i, (label, text)) in cases.iter().enumerate() {
        let file = dir.join(format!("case{i}.ipkg"));
        std::fs::write(&file, text).unwrap();
        match idris_reads(&idris2, &file) {
            Ok((name, deps)) => {
                let ours = (package_name(text).unwrap_or_default(), depends(text));
                if ours != (name.clone(), deps.clone()) {
                    mismatches.push(format!(
                        "{label}: idris2={name} {deps:?} ours={ours:?}\n{text}"
                    ));
                }
            }
            Err(e) => mismatches.push(format!("{label}: idris2 rejected the file: {e}\n{text}")),
        }
    }
    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        mismatches.is_empty(),
        "{} of {} cases disagree with Idris2; first few:\n\n{}",
        mismatches.len(),
        cases.len(),
        mismatches
            .iter()
            .take(5)
            .cloned()
            .collect::<Vec<_>>()
            .join("\n---\n")
    );
    eprintln!("idris2 agrees on all {} cases", cases.len());
}
