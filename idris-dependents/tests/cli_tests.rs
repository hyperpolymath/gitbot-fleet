// SPDX-License-Identifier: MPL-2.0
//! Offline CLI contracts. Child processes never inherit GitHub credentials.

use std::path::PathBuf;
use std::process::{Command, Output, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};

use serde_json::{json, Value};

fn cli() -> Command {
    let mut command = Command::new(env!("CARGO_BIN_EXE_idris-dependents"));
    command
        .env_remove("GITHUB_TOKEN")
        .env_remove("GH_TOKEN")
        .stdin(Stdio::null());
    command
}

fn fixture(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(name)
}

fn output_json(output: Output) -> Value {
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        output.stderr.is_empty(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    serde_json::from_slice(&output.stdout).expect("CLI stdout must be JSON")
}

struct InputFile(PathBuf);

impl InputFile {
    fn new(contents: &str) -> Self {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let dir = std::env::temp_dir().join(format!(
            "idris-dependents-cli-{}-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos(),
            NEXT.fetch_add(1, Ordering::Relaxed),
        ));
        std::fs::create_dir(&dir).unwrap();
        let file = Self(dir.join("input with spaces.toml"));
        std::fs::write(&file.0, contents).unwrap();
        file
    }
}

impl Drop for InputFile {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(self.0.parent().unwrap());
    }
}

#[test]
fn parse_ipkg_prints_the_package_and_dependencies_without_authentication() {
    let output = cli()
        .arg("parse-ipkg")
        .arg(fixture("ochrance.ipkg"))
        .output()
        .unwrap();
    assert_eq!(
        output_json(output),
        json!({
            "package": "ochrance", "depends": ["base", "contrib", "network"],
        })
    );
}

#[test]
fn parse_ipkg_with_no_declaration_prints_null_and_an_empty_dependency_list() {
    let file = InputFile::new("-- package fake\n{- depends = fake -}");
    let output = cli().arg("parse-ipkg").arg(&file.0).output().unwrap();
    assert_eq!(
        output_json(output),
        json!({ "package": null, "depends": [] })
    );
}

#[test]
fn parse_pack_toml_honours_default_and_comma_delimited_owner_options() {
    let file = InputFile::new(
        r#"
[custom.all.first]
url = "https://github.com/hyperpolymath/first"
[custom.all.second]
url = "https://github.com/another/second"
"#,
    );
    let first = json!({"name": "first", "url": "https://github.com/hyperpolymath/first"});
    let second = json!({"name": "second", "url": "https://github.com/another/second"});
    for (owners, expected) in [
        (None, json!([first.clone()])),
        (Some("ANOTHER"), json!([second.clone()])),
        (Some("hyperpolymath,another"), json!([first, second])),
        (Some("nobody"), json!([])),
    ] {
        let mut command = cli();
        command.arg("parse-pack-toml").arg(&file.0);
        if let Some(owners) = owners {
            command.args(["--owners", owners]);
        }
        assert_eq!(output_json(command.output().unwrap()), expected);
    }
}

#[test]
fn offline_commands_report_missing_paths_and_invalid_toml_as_errors() {
    let file = InputFile::new("[invalid table");
    for subcommand in ["parse-ipkg", "parse-pack-toml"] {
        let missing = file.0.with_file_name("missing input");
        let output = cli().arg(subcommand).arg(&missing).output().unwrap();
        assert!(!output.status.success());
        assert!(output.stdout.is_empty());
        let stderr = String::from_utf8(output.stderr).unwrap();
        assert!(
            stderr.contains(&format!("reading {}", missing.display())),
            "{stderr}"
        );
    }
    let output = cli().arg("parse-pack-toml").arg(&file.0).output().unwrap();
    assert!(!output.status.success());
    assert!(output.stdout.is_empty());
    assert!(String::from_utf8_lossy(&output.stderr).contains("TOML parse error"));
}

#[test]
fn scan_without_credentials_fails_before_searching_or_writing_a_report() {
    let file = InputFile::new("preserve this output");
    for empty_token in [false, true] {
        let mut command = cli();
        if empty_token {
            command.env("GITHUB_TOKEN", "").env("GH_TOKEN", "");
        }
        let output = command
            .args(["scan", "--packages", "ochrance", "--json"])
            .arg(&file.0)
            .output()
            .unwrap();
        assert!(!output.status.success());
        assert!(output.stdout.is_empty());
        let stderr = String::from_utf8(output.stderr).unwrap();
        assert!(stderr.contains("set GITHUB_TOKEN"), "{stderr}");
        assert!(stderr.contains("code search requires auth"), "{stderr}");
        assert_eq!(
            std::fs::read_to_string(&file.0).unwrap(),
            "preserve this output"
        );
    }
}
