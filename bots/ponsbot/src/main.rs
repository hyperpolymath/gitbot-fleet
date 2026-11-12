// SPDX-License-Identifier: MPL-2.0
//! ponsbot entry point. See `lib.rs` and `cli.rs`.
//!
//! Output contract for `--format json` (what the report-back driver reads):
//! - ran: a JSON array of findings (possibly empty), exit 0 (no blocking
//!   finding) or 1 (at least one blocking finding),
//! - error: `{"status":"error","reason":"..."}`, exit 2,
//! - not run: `{"status":"not_run","reason":"..."}`, exit 3.
//!
//! Nothing here ever prints an empty array for a scan that did not run.

use clap::Parser;
use ponsbot::cli::{Cli, Command, OutputFormat};
use ponsbot::scanner::{run_pons, Outcome};
use ponsbot::{pin, translator};
use std::process::ExitCode;
use std::time::Duration;

fn main() -> ExitCode {
    let cli = Cli::parse();
    let Command::Scan { dir, timeout } = cli.command;
    let pin = pin::pinned();
    match run_pons(&dir, Duration::from_secs(timeout.max(1)), &pin.commit) {
        Outcome::Ran(found) => {
            let findings = translator::translate_all(&found);
            match cli.format {
                OutputFormat::Json => println!(
                    "{}",
                    serde_json::to_string_pretty(&findings).expect("findings serialise")
                ),
                OutputFormat::Text => {
                    if findings.is_empty() {
                        println!("No findings (pons at {}).", &pin.commit[..12]);
                    }
                    for f in &findings {
                        println!(
                            "{}:{} [{}] {} {}",
                            f.file, f.line, f.rule_id, f.severity, f.message
                        );
                    }
                }
            }
            if findings.iter().any(|f| f.blocking) {
                ExitCode::from(1)
            } else {
                ExitCode::SUCCESS
            }
        }
        Outcome::Error(reason) => report_not_ran("error", &reason, cli.format, 2),
        Outcome::NotRun(reason) => report_not_ran("not_run", &reason, cli.format, 3),
    }
}

fn report_not_ran(status: &str, reason: &str, format: OutputFormat, code: u8) -> ExitCode {
    match format {
        OutputFormat::Json => {
            println!(
                "{}",
                serde_json::json!({ "status": status, "reason": reason })
            )
        }
        OutputFormat::Text => println!("ponsbot: {status}: {reason}"),
    }
    eprintln!("ponsbot: {status}: {reason}");
    ExitCode::from(code)
}
