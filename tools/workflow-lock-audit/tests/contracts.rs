// SPDX-License-Identifier: MPL-2.0
//! CLI-level fixtures: assertions inspect status and evidence, independently
//! of implementation functions. Every invocation parses files from disk.
use saphyr::{LoadableYamlNode, ScalarOwned, YamlOwned};
use serde_json::Value;
use std::{
    fs,
    path::{Path, PathBuf},
    process::Command,
    sync::atomic::{AtomicU64, Ordering},
};
fn parse_yaml(text: &str) -> Result<Value, String> {
    fn convert(y: YamlOwned) -> Result<Value, String> {
        Ok(match y {
            YamlOwned::Value(ScalarOwned::Null) => Value::Null,
            YamlOwned::Value(ScalarOwned::Boolean(v)) => Value::Bool(v),
            YamlOwned::Value(ScalarOwned::Integer(v)) => Value::Number(v.into()),
            YamlOwned::Value(ScalarOwned::FloatingPoint(v)) => serde_json::json!(v.0),
            YamlOwned::Value(ScalarOwned::String(v)) => Value::String(v),
            YamlOwned::Sequence(v) => {
                Value::Array(v.into_iter().map(convert).collect::<Result<_, _>>()?)
            }
            YamlOwned::Mapping(v) => Value::Object(
                v.into_iter()
                    .map(|(k, v)| Ok((k.as_str().ok_or("fixture key")?.to_owned(), convert(v)?)))
                    .collect::<Result<_, String>>()?,
            ),
            _ => return Err("unsupported fixture YAML".into()),
        })
    }
    let mut docs = YamlOwned::load_from_str(text).map_err(|e| e.to_string())?;
    if docs.len() != 1 {
        return Err("fixture document count".into());
    }
    convert(docs.pop().unwrap())
}
fn value(text: &str) -> Value {
    parse_yaml(text).unwrap()
}
fn text(s: &str) -> Value {
    Value::String(s.into())
}
fn remove(doc: &mut Value, key: &str) {
    doc.as_object_mut().unwrap().remove(key);
}
static NEXT: AtomicU64 = AtomicU64::new(0);
struct Fixture {
    root: PathBuf,
    lock: Value,
}
impl Drop for Fixture {
    fn drop(&mut self) {
        fs::remove_dir_all(&self.root).unwrap();
    }
}
impl Fixture {
    fn new() -> Self {
        let root = std::env::temp_dir().join(format!(
            "fleet-lock-audit-{}-{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir_all(root.join(".github/workflows")).unwrap();
        let f = Self { root, lock: value("workflows:\n  .github/workflows/main.yml: [acme/action@v1]\n  .github/workflows/empty.yml: []\ndependencies:\n  acme/action@v1: {ref: v1, commit: sha1-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa}\n") };
        f.write(
            "main.yml",
            "jobs: {build: {steps: [{uses: Acme/action/subpath@v1}]}}",
        );
        f.write("empty.yml", "jobs: {build: {steps: [{run: echo hello}]}}");
        f
    }
    fn write(&self, name: &str, data: &str) {
        fs::write(self.root.join(".github/workflows").join(name), data).unwrap();
    }
    fn audit(&self, args: &[&str]) -> (i32, String) {
        self.write("actions.lock", &serde_json::to_string(&self.lock).unwrap());
        run(&[vec!["map", self.root.to_str().unwrap()], args.to_vec()].concat())
    }
}
fn run(args: &[&str]) -> (i32, String) {
    let result = Command::new(env!("CARGO_BIN_EXE_fleet-workflow-lock-audit"))
        .args(args)
        .output()
        .unwrap();
    (
        result.status.code().unwrap(),
        String::from_utf8(result.stdout).unwrap() + &String::from_utf8(result.stderr).unwrap(),
    )
}
fn expect(result: (i32, String), code: i32, evidence: &str) -> String {
    assert_eq!(result.0, code, "{}", result.1);
    assert!(
        result.1.contains(evidence),
        "expected {evidence:?}: {}",
        result.1
    );
    result.1
}
#[test]
fn mapped_no_uses_is_not_missing_or_unmanaged() {
    let out = expect(
        Fixture::new().audit(&[]),
        0,
        "NO-USES-MAPPED .github/workflows/empty.yml",
    );
    assert!(!out.contains("INTENTIONALLY-UNMANAGED"));
    assert!(out.contains("remote source closure and provenance NOT VERIFIED"));
}
#[test]
fn missing_workflow_is_not_silently_exempted() {
    let mut f = Fixture::new();
    remove(&mut f.lock["workflows"], ".github/workflows/empty.yml");
    expect(
        f.audit(&[]),
        1,
        "MISSING-WORKFLOW .github/workflows/empty.yml",
    );
}
#[test]
fn explicit_unmanaged_is_not_a_closure_pass() {
    let mut f = Fixture::new();
    remove(&mut f.lock["workflows"], ".github/workflows/empty.yml");
    let out = expect(
        f.audit(&["--unmanaged", ".github/workflows/empty.yml"]),
        2,
        "INTENTIONALLY-UNMANAGED",
    );
    assert!(out.contains("closure not verified"));
}
#[test]
fn invalid_unmanaged_exception_fails() {
    expect(
        Fixture::new().audit(&["--unmanaged", ".github/workflows/typo.yml"]),
        1,
        "UNMANAGED-UNKNOWN",
    );
}
#[test]
fn missing_direct_ref_even_with_workflow_key() {
    let mut f = Fixture::new();
    f.lock["workflows"][".github/workflows/main.yml"] = value("[]");
    expect(f.audit(&[]), 1, "MISSING-DIRECT-EDGE");
}
#[test]
fn sha_reusable_empty_map_requires_source_review() {
    let mut f = Fixture::new();
    f.write(
        "main.yml",
        &format!(
            "jobs: {{scan: {{uses: acme/suite/.github/workflows/scan.yml@{}}}}}",
            "a".repeat(40)
        ),
    );
    f.lock["workflows"][".github/workflows/main.yml"] = value("[]");
    let out = expect(f.audit(&[]), 0, "REMOTE-REUSABLE-SOURCE-REVIEW");
    assert!(out.contains("UNSUPPORTED-LOCK-SCOPE"));
    assert!(!out.contains("MISSING-DIRECT-EDGE"));
    assert!(!out.contains("NO-USES-MAPPED .github/workflows/main.yml"));
}
#[test]
fn remote_reusable_floating_and_short_pins_fail() {
    for pin in [
        "main".into(),
        "v1".into(),
        "abc1234".into(),
        "a".repeat(39),
        "a".repeat(41),
        "g".repeat(40),
    ] {
        let mut f = Fixture::new();
        f.write(
            "main.yml",
            &format!("jobs: {{scan: {{uses: acme/suite/.github/workflows/scan.yml@{pin}}}}}"),
        );
        f.lock["workflows"][".github/workflows/main.yml"] = value("[]");
        expect(f.audit(&[]), 1, "REMOTE-REUSABLE-NOT-SHA");
    }
}
#[test]
fn reusable_does_not_exempt_missing_ordinary_action_edge() {
    let mut f = Fixture::new();
    f.write("main.yml", &format!("jobs:\n  scan: {{uses: acme/suite/.github/workflows/scan.yml@{}}}\n  build: {{steps: [{{uses: acme/action@v1}}]}}", "a".repeat(40)));
    f.lock["workflows"][".github/workflows/main.yml"] = value("[]");
    let out = expect(
        f.audit(&[]),
        1,
        "MISSING-DIRECT-EDGE .github/workflows/main.yml acme/action@v1",
    );
    assert!(out.contains("UNSUPPORTED-LOCK-SCOPE"));
}
#[test]
fn missing_nested_record() {
    let mut f = Fixture::new();
    f.lock["dependencies"]["acme/action@v1"]["uses"] = value("[acme/child@v1]");
    expect(f.audit(&[]), 1, "MISSING-TRANSITIVE-RECORD");
}
#[test]
fn local_reusable_is_checked_at_its_own_path() {
    let mut f = Fixture::new();
    f.write(
        "main.yml",
        "jobs: {call: {uses: ./.github/workflows/callee.yml}}",
    );
    f.write(
        "callee.yml",
        "jobs: {run: {steps: [{uses: acme/action@v1}]}}",
    );
    f.lock["workflows"][".github/workflows/main.yml"] = value("[]");
    f.lock["workflows"][".github/workflows/callee.yml"] = value("[]");
    let out = expect(
        f.audit(&[]),
        1,
        "MISSING-DIRECT-EDGE .github/workflows/callee.yml",
    );
    assert!(out.contains("LOCAL-REUSABLE"));
}
#[test]
fn malformed_and_duplicate_yaml_fail_closed() {
    let f = Fixture::new();
    f.write("main.yml", "jobs: {run: {steps: [{uses: no-at-sign}]}}");
    expect(f.audit(&[]), 2, "NOT-RUN");
    f.write("main.yml", "jobs: {}\njobs: {}\n");
    expect(f.audit(&[]), 2, "duplicate YAML key");
}
#[test]
fn extra_edge_is_review_not_proof_of_failure() {
    let mut f = Fixture::new();
    f.lock["workflows"][".github/workflows/empty.yml"] = value("[acme/action@v1]");
    expect(f.audit(&[]), 0, "EXTRA-OR-TRANSITIVE-EDGE");
}
#[test]
fn mapped_ref_without_record_and_stale_workflow() {
    let mut f = Fixture::new();
    f.lock["workflows"][".github/workflows/main.yml"] = value("[acme/action@v1, acme/missing@v2]");
    f.lock["workflows"][".github/workflows/deleted.yml"] = value("[]");
    let out = expect(
        f.audit(&[]),
        1,
        "MISSING-RECORD .github/workflows/main.yml acme/missing@v2",
    );
    assert!(out.contains("STALE-WORKFLOW .github/workflows/deleted.yml"));
}
#[test]
fn mapped_workflow_cannot_also_be_unmanaged() {
    expect(
        Fixture::new().audit(&["--unmanaged", ".github/workflows/main.yml"]),
        1,
        "UNMANAGED-BUT-MAPPED",
    );
}
#[test]
fn invalid_local_workflow_paths() {
    for path in [
        "./.github/workflows/missing.yml",
        "./outside.yml",
        "./.github/workflows/actions.lock",
    ] {
        let mut f = Fixture::new();
        f.write("main.yml", &format!("jobs: {{call: {{uses: {path}}}}}"));
        f.lock["workflows"][".github/workflows/main.yml"] = value("[]");
        expect(
            f.audit(&[]),
            1,
            &format!("INVALID-LOCAL-WORKFLOW .github/workflows/main.yml {path}"),
        );
    }
}
#[test]
fn local_actions_and_docker_are_reported_for_review() {
    let mut f = Fixture::new();
    f.write(
        "main.yml",
        "jobs: {build: {steps: [{uses: ./my-action}, {uses: 'docker://example/image:1'}]}}",
    );
    f.lock["workflows"][".github/workflows/main.yml"] = value("[]");
    let out = expect(
        f.audit(&[]),
        0,
        "NOT-LOCKED-SOURCE-REVIEW .github/workflows/main.yml ./my-action",
    );
    assert!(out
        .contains("NOT-LOCKED-SOURCE-REVIEW .github/workflows/main.yml docker://example/image:1"));
}
#[test]
fn normalizes_owner_repo_and_subpath_but_preserves_ref_case() {
    let mut f = Fixture::new();
    f.write(
        "main.yml",
        "jobs: {build: {steps: [{uses: ACME/ACTION/sub@V1}]}}",
    );
    expect(
        f.audit(&[]),
        1,
        "MISSING-DIRECT-EDGE .github/workflows/main.yml acme/action@V1",
    );
    f.lock["workflows"][".github/workflows/main.yml"] = value("[acme/action@V1]");
    f.lock["dependencies"] = value("{Acme/Action@V1: {}}");
    expect(f.audit(&[]), 0, "findings=0");
}
#[test]
fn duplicate_normalized_refs_and_bad_records_fail_closed() {
    for n in 0..5 {
        let mut f = Fixture::new();
        match n {
            0 => f.lock["dependencies"]["ACME/ACTION@v1"] = value("{}"),
            1 => {
                f.lock["workflows"][".github/workflows/main.yml"] =
                    value("[acme/action@v1, ACME/ACTION/sub@v1]")
            }
            2 => f.lock["dependencies"]["acme/action@v1"] = value("[]"),
            3 => f.lock["dependencies"]["acme/action@v1"]["uses"] = value("[./local]"),
            _ => f.lock["workflows"][".github/workflows/main.yml"] = text("acme/action@v1"),
        }
        expect(f.audit(&[]), 2, "NOT-RUN:");
    }
}
#[test]
fn invalid_workflow_shapes_fail_closed() {
    for doc in [
        "jobs: []",
        "jobs: {build: null}",
        "jobs: {build: {steps: {}}}",
        "jobs: {build: {steps: [null]}}",
        "jobs: {build: {steps: [{uses: 'acme/action@bad ref'}]}}",
    ] {
        let f = Fixture::new();
        f.write("main.yml", doc);
        expect(f.audit(&[]), 2, "NOT-RUN:");
    }
}
#[test]
fn yaml_extension_is_inventoried() {
    let f = Fixture::new();
    f.write("extra.yaml", "jobs: {run: {steps: []}}");
    expect(
        f.audit(&[]),
        1,
        "MISSING-WORKFLOW .github/workflows/extra.yaml",
    );
}
fn repo() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}
#[test]
fn scanner_triggers_and_access() {
    let workflow: Value = parse_yaml(
        &fs::read_to_string(repo().join(".github/workflows/secret-scanner.yml")).unwrap(),
    )
    .unwrap();
    assert_eq!(
        workflow["on"],
        value("{pull_request: null, push: {branches: [main]}}")
    );
    assert_eq!(workflow["permissions"], value("{contents: read}"));
    assert_eq!(workflow["jobs"].as_object().unwrap().len(), 1);
    let job = &workflow["jobs"]["scan"];
    assert_eq!(job["permissions"], value("{contents: read}"));
    assert!(job.get("secrets").is_none());
    assert_eq!(job["with"], value("{fast-pr: false}"));
}
#[test]
fn scanner_source_pin_and_supported_lock_scope() {
    let workflow: Value = parse_yaml(
        &fs::read_to_string(repo().join(".github/workflows/secret-scanner.yml")).unwrap(),
    )
    .unwrap();
    let lock: Value =
        parse_yaml(&fs::read_to_string(repo().join(".github/workflows/actions.lock")).unwrap())
            .unwrap();
    let pin = "4404c3c0474e17753fe7689f3485c02890aac113";
    assert_eq!(
        workflow["jobs"]["scan"]["uses"],
        text(&format!(
            "hyperpolymath/cicd-suite/.github/workflows/secret-scanner-reusable.yml@{pin}"
        ))
    );
    assert_eq!(
        lock["workflows"][".github/workflows/secret-scanner.yml"],
        value("[]")
    );
    assert!(lock["dependencies"]
        .get(format!("hyperpolymath/cicd-suite@{pin}"))
        .is_none());
    assert_eq!(
        lock["dependencies"]["actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1"]["commit"],
        text("sha1-3d3c42e5aac5ba805825da76410c181273ba90b1")
    );
}
fn base() -> Value {
    value("workflows:\n  .github/workflows/test.yml: [org/reusable@v1]\ndependencies:\n  org/reusable@v1: {ref: v1, commit: sha1-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, uses: [org/composite@bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb]}\n  org/composite@bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb: {ref: v2, commit: sha1-bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb, uses: [org/child@v3]}\n  org/child@v3: {ref: v3, commit: sha1-cccccccccccccccccccccccccccccccccccccccc}\n")
}
const COMPOSITE: &str = "org/composite@bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb";
fn regen_raw(old: &str, new: &str) -> (i32, String) {
    let f = Fixture::new();
    let a = f.root.join("old.yml");
    let b = f.root.join("new.yml");
    fs::write(&a, old).unwrap();
    fs::write(&b, new).unwrap();
    run(&["regeneration", a.to_str().unwrap(), b.to_str().unwrap()])
}
fn regen(old: &Value, new: &Value) -> (i32, String) {
    regen_raw(
        &serde_json::to_string(old).unwrap(),
        &serde_json::to_string(new).unwrap(),
    )
}
#[test]
fn unchanged_closure() {
    expect(regen(&base(), &base()), 0, "closure (3 records)");
}
#[test]
fn dropped_reusable_closure_without_dangling_edge() {
    let mut d = base();
    remove(&mut d["dependencies"]["org/reusable@v1"], "uses");
    remove(&mut d["dependencies"], COMPOSITE);
    remove(&mut d["dependencies"], "org/child@v3");
    expect(regen(&base(), &d), 1, "dropped reviewed edges");
}
#[test]
fn dropped_composite_closure_without_dangling_edge() {
    let mut d = base();
    remove(&mut d["dependencies"][COMPOSITE], "uses");
    remove(&mut d["dependencies"], "org/child@v3");
    expect(regen(&base(), &d), 1, "dropped reviewed edges");
}
#[test]
fn dangling_edge() {
    let mut d = base();
    remove(&mut d["dependencies"], "org/child@v3");
    expect(regen(&base(), &d), 1, "missing dependency");
}
#[test]
fn lost_reviewed_tag() {
    let mut d = base();
    d["dependencies"][COMPOSITE]["ref"] = text("sha1-bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb");
    expect(regen(&base(), &d), 1, "invalid ref");
}
#[test]
fn malformed_commit() {
    let mut d = base();
    d["dependencies"]["org/child@v3"]["commit"] = text(&"c".repeat(40));
    expect(regen(&base(), &d), 1, "invalid commit");
}
#[test]
fn wrong_tag() {
    let mut d = base();
    d["dependencies"]["org/child@v3"]["ref"] = text("v4");
    expect(regen(&base(), &d), 1, "tag ref differs");
}
#[test]
fn sha_mismatch() {
    let mut d = base();
    d["dependencies"][COMPOSITE]["commit"] = text(&format!("sha1-{}", "c".repeat(40)));
    expect(regen(&base(), &d), 1, "SHA key and commit differ");
}
#[test]
fn changed_reviewed_tag_resolution_is_rejected() {
    let mut d = base();
    d["dependencies"]["org/child@v3"]["commit"] = text(&format!("sha1-{}", "a".repeat(40)));
    expect(
        regen(&base(), &d),
        1,
        "changed reviewed ref/commit org/child@v3",
    );
}
#[test]
fn changed_reviewed_sha_record_ref_is_rejected() {
    let mut d = base();
    d["dependencies"][COMPOSITE]["ref"] = text("v4");
    expect(
        regen(&base(), &d),
        1,
        &format!("changed reviewed ref/commit {COMPOSITE}"),
    );
}
#[test]
fn added_edges_and_new_roots_are_allowed_but_require_review() {
    let mut d = base();
    d["dependencies"]["org/child@v3"]["uses"] = value("[new/action@v1]");
    d["dependencies"]["new/action@v1"] =
        value("{ref: v1, commit: sha1-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa}");
    d["workflows"]["new.yml"] = value("[new/action@v1]");
    let out = expect(regen(&base(), &d), 0, "closure (3 records)");
    assert!(out.contains("new sources require independent review"));
}
#[test]
fn removed_old_root_does_not_force_unused_closure() {
    expect(
        regen(&base(), &value("{workflows: {}, dependencies: {}}")),
        0,
        "closure (0 records)",
    );
}
#[test]
fn cycles_and_case_insensitive_repository_names() {
    let mut d = base();
    d["dependencies"]["org/child@v3"]["uses"] = value("[ORG/Reusable@v1]");
    let mut proposed = d.clone();
    proposed["workflows"][".github/workflows/test.yml"] = value("[ORG/REUSABLE@v1]");
    expect(regen(&d, &proposed), 0, "closure (3 records)");
}
#[test]
fn dropped_edge_is_identified_even_when_record_remains() {
    let mut d = base();
    d["dependencies"]["org/reusable@v1"]["uses"] = value("[]");
    let out = expect(
        regen(&base(), &d),
        1,
        "dropped reviewed edges from org/reusable@v1",
    );
    assert!(out.contains(COMPOSITE));
}
#[test]
fn invalid_documents_fail_on_either_input() {
    for (doc, message) in [
        ("workflows: {}\nworkflows: {}\ndependencies: {}", "duplicate YAML key"),
        ("{workflows: [], dependencies: {}}", "expected workflows and dependencies mappings"),
        ("{workflows: {test.yml: [org/missing@v1]}, dependencies: {}}", "missing dependency"),
        ("{workflows: {test.yml: org/action@v1}, dependencies: {}}", "invalid workflow"),
        ("{workflows: {}, dependencies: {org/action@v1: []}}", "invalid record"),
        ("{workflows: {}, dependencies: {org/action@v1: {ref: v1, commit: sha1-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, uses: org/child@v1}}}", "invalid uses"),
        ("{workflows: {}, dependencies: {org/action/sub@v1: {}}}", "invalid dependency reference"),
        ("{workflows: {}, dependencies: {org/action@v1: {ref: v1, commit: sha1-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa}, ORG/ACTION@v1: {ref: v1, commit: sha1-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa}}}", "duplicate or invalid record"),
    ] {
        let good = serde_json::to_string(&base()).unwrap();
        expect(regen_raw(doc, &good), 1, message); expect(regen_raw(&good, doc), 1, message);
    }
}
#[test]
fn cli_requires_two_paths_and_handles_missing_file() {
    expect(run(&["regeneration"]), 1, "usage:");
    let f = Fixture::new();
    let a = f.root.join("old.yml");
    fs::write(&a, serde_json::to_string(&base()).unwrap()).unwrap();
    expect(
        run(&[
            "regeneration",
            a.to_str().unwrap(),
            f.root.join("absent.yml").to_str().unwrap(),
        ]),
        1,
        "lock regeneration audit:",
    );
    expect(run(&["map", "--unmanaged"]), 2, "usage:");
}
#[test]
fn strict_yaml_framing_keys_tags_and_nested_duplicates() {
    for invalid in [
        "jobs: {}\n---\njobs: {}",
        "jobs: !Unsupported {}",
        "jobs: {build: {steps: [], steps: []}}",
        "jobs: {[complex, key]: {}}",
        "jobs: {build: {<<: {steps: []}}}",
        "jobs: [",
    ] {
        let f = Fixture::new();
        f.write("main.yml", invalid);
        expect(f.audit(&[]), 2, "NOT-RUN:");
    }
    let good = serde_json::to_string(&base()).unwrap();
    for bad in [
        "workflows: {}\ndependencies: {org/action@v1: {ref: v1, ref: v2}}",
        "workflows: {}\ndependencies: {}\n---\nworkflows: {}",
        "workflows: {}\ndependencies: !Unsupported {}",
    ] {
        expect(regen_raw(bad, &good), 1, "lock regeneration audit:");
        expect(regen_raw(&good, bad), 1, "lock regeneration audit:");
    }
}
#[cfg(unix)]
#[test]
fn local_callee_symlink_cannot_escape_workflow_directory() {
    let mut f = Fixture::new();
    let outside = f.root.join("outside.yml");
    fs::write(&outside, "jobs: {}").unwrap();
    std::os::unix::fs::symlink(&outside, f.root.join(".github/workflows/escape.yml")).unwrap();
    f.write(
        "main.yml",
        "jobs: {call: {uses: ./.github/workflows/escape.yml}}",
    );
    f.lock["workflows"][".github/workflows/main.yml"] = value("[]");
    f.lock["workflows"][".github/workflows/escape.yml"] = value("[]");
    expect(f.audit(&[]), 1, "INVALID-LOCAL-WORKFLOW");
}

#[test]
fn legacy_yaml_boolean_and_timestamp_refs_do_not_become_strings() {
    for bare in [
        "yes",
        "No",
        "ON",
        "off",
        "2026-11-07",
        "1:20",
        "11111111111111111111111111111111111111111",
    ] {
        let good = format!("workflows: {{test.yml: [org/action@{bare}]}}\ndependencies: {{org/action@{bare}: {{ref: '{bare}', commit: sha1-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa}}}}");
        expect(regen_raw(&good, &good), 0, "closure (1 records)");
        let bad = good.replace(&format!("ref: '{bare}'"), &format!("ref: {bare}"));
        expect(regen_raw(&bad, &good), 1, "lock regeneration audit:");
        expect(regen_raw(&good, &bad), 1, "lock regeneration audit:");
    }
    let duplicate = "workflows: {}\ndependencies: {}\n'workflows': {}";
    expect(regen_raw(duplicate, duplicate), 1, "duplicate YAML key");
    let f = Fixture::new();
    f.write("main.yml", "jobs: {}\njobs: {}\n");
    expect(f.audit(&[]), 2, "duplicate YAML key");
}

#[test]
fn unquoted_legacy_numeric_refs_fail_closed_and_quoted_refs_remain_strings() {
    for raw in [
        "1_000",
        "0xFF_FF",
        "0xFFFFFFFFFFFFFFFFFFFFFFFF",
        "0b1_0",
        "+1_000",
        "1_0.0",
        "0b_",
        "0x_",
        "0b__",
        "0x__",
        "+0b_",
    ] {
        let quoted = format!("workflows: {{test.yml: [org/action@{raw}]}}\ndependencies: {{org/action@{raw}: {{ref: '{raw}', commit: sha1-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa}}}}");
        expect(regen_raw(&quoted, &quoted), 0, "closure (1 records)");
        let bare = quoted.replace(&format!("ref: '{raw}'"), &format!("ref: {raw}"));
        expect(regen_raw(&bare, &quoted), 1, "lock regeneration audit:");
        expect(regen_raw(&quoted, &bare), 1, "lock regeneration audit:");
    }
}
