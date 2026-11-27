// SPDX-License-Identifier: MPL-2.0
//! Fail closed on changes to the fleet scanner's deployed security contract.
//!
//! Usage: check-scanner-caller \[WORKFLOW\]   (default: .github/workflows/secret-scanner.yml)
//! Exits 0 when the caller matches the reviewed contract, 1 otherwise.

use std::collections::HashSet;
use std::process::ExitCode;

use yaml_rust2::parser::{Event, MarkedEventReceiver, Parser};
use yaml_rust2::scanner::Marker;
use yaml_rust2::{Yaml, YamlLoader};

const EXPECTED_PIN: &str = "hyperpolymath/proven-cicd-and-workflows/.github/workflows/secret-scanner-reusable.yml@1fd5e6371a50f0fc3687206a5adca0cdf7b39aa6";

/// Walks parser events before loading. Rejects aliases, non-scalar mapping
/// keys and duplicate keys within a mapping. Keeps the first error only.
#[derive(Default)]
struct Validator {
    frames: Vec<Frame>,
    error: Option<String>,
}

enum Frame {
    Seq,
    Map {
        keys: HashSet<String>,
        expect_key: bool,
    },
}

impl Validator {
    fn fail(&mut self, msg: &str) {
        if self.error.is_none() {
            self.error = Some(msg.to_string());
        }
    }

    /// Called at the start of every node. `scalar` is Some(text) for a scalar.
    fn enter_node(&mut self, scalar: Option<&str>) {
        let mut problem: Option<&'static str> = None;
        if let Some(Frame::Map { keys, expect_key }) = self.frames.last_mut() {
            if *expect_key {
                match scalar {
                    Some(text) => {
                        if !keys.insert(text.to_string()) {
                            problem = Some("duplicate YAML key");
                        }
                    }
                    None => problem = Some("non-scalar YAML key"),
                }
            }
            *expect_key = !*expect_key;
        }
        if let Some(p) = problem {
            self.fail(p);
        }
    }
}

impl MarkedEventReceiver for Validator {
    fn on_event(&mut self, ev: Event, _mark: Marker) {
        if self.error.is_some() {
            return;
        }
        match ev {
            Event::Alias(_) => self.fail("YAML aliases are not supported"),
            Event::Scalar(text, ..) => self.enter_node(Some(&text)),
            Event::SequenceStart(..) => {
                self.enter_node(None);
                self.frames.push(Frame::Seq);
            }
            Event::MappingStart(..) => {
                self.enter_node(None);
                self.frames.push(Frame::Map {
                    keys: HashSet::new(),
                    expect_key: true,
                });
            }
            Event::SequenceEnd | Event::MappingEnd => {
                self.frames.pop();
            }
            _ => {}
        }
    }
}

/// Order-insensitive structural equality. Scalars must match exactly, so
/// `false` does not equal `"false"`.
fn same(a: &Yaml, b: &Yaml) -> bool {
    match (a, b) {
        (Yaml::Hash(x), Yaml::Hash(y)) => {
            x.len() == y.len() && x.iter().all(|(k, v)| y.get(k).is_some_and(|w| same(v, w)))
        }
        (Yaml::Array(x), Yaml::Array(y)) => {
            x.len() == y.len() && x.iter().zip(y).all(|(p, q)| same(p, q))
        }
        _ => a == b,
    }
}

fn expected(text: &str) -> Yaml {
    YamlLoader::load_from_str(text)
        .ok()
        .and_then(|mut docs| {
            if docs.is_empty() {
                None
            } else {
                Some(docs.remove(0))
            }
        })
        .expect("built-in contract literal parses")
}

fn require_equal(actual: Option<&Yaml>, want: &Yaml, label: &str) -> Result<(), String> {
    match actual {
        Some(a) if same(a, want) => Ok(()),
        _ => Err(format!("{label} does not match the reviewed contract")),
    }
}

fn check(path: &str) -> Result<(), String> {
    let text = std::fs::read_to_string(path).map_err(|e| format!("{path}: {e}"))?;

    let mut validator = Validator::default();
    let mut parser = Parser::new_from_str(&text);
    parser
        .load(&mut validator, true)
        .map_err(|e| e.to_string())?;
    if let Some(e) = validator.error {
        return Err(e);
    }

    let docs = YamlLoader::load_from_str(&text).map_err(|e| e.to_string())?;
    let workflow = docs.first().ok_or("empty YAML")?;
    let map = workflow.as_hash().ok_or("workflow must be a mapping")?;
    let get = |m: &yaml_rust2::yaml::Hash, k: &str| m.get(&Yaml::String(k.to_string())).cloned();

    // Quoted or unquoted, the key is the string "on" here. A YAML 1.1 loader
    // would read a bare `on` as true, so accept that spelling too.
    let events = get(map, "on").or_else(|| map.get(&Yaml::Boolean(true)).cloned());
    require_equal(
        events.as_ref(),
        &expected("pull_request:\npush:\n  branches: [main]\n"),
        "triggers",
    )?;
    let perms = expected("contents: read\n");
    require_equal(
        get(map, "permissions").as_ref(),
        &perms,
        "workflow permissions",
    )?;

    let jobs = get(map, "jobs")
        .and_then(|j| j.into_hash())
        .filter(|j| j.len() == 1 && j.contains_key(&Yaml::String("scan".into())))
        .ok_or("only the scan job is expected")?;
    let scan = get(&jobs, "scan")
        .and_then(|s| s.into_hash())
        .filter(|s| {
            let mut keys: Vec<&str> = s.keys().filter_map(|k| k.as_str()).collect();
            keys.sort_unstable();
            s.len() == 3 && keys == ["permissions", "uses", "with"]
        })
        .ok_or("unexpected scan job fields")?;

    require_equal(
        get(&scan, "permissions").as_ref(),
        &perms,
        "job permissions",
    )?;
    require_equal(
        get(&scan, "uses").as_ref(),
        &Yaml::String(EXPECTED_PIN.into()),
        "suite pin",
    )?;
    require_equal(
        get(&scan, "with").as_ref(),
        &expected("fast-pr: false\n"),
        "scan policy",
    )?;
    Ok(())
}

fn main() -> ExitCode {
    let path = std::env::args()
        .nth(1)
        .unwrap_or_else(|| ".github/workflows/secret-scanner.yml".into());
    match check(&path) {
        Ok(()) => {
            println!("scanner caller contract: exact suite pin, full-history policy, triggers, least privilege and no secrets verified");
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("scanner caller contract: {e}");
            ExitCode::FAILURE
        }
    }
}
