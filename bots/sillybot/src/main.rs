// SPDX-License-Identifier: MPL-2.0
//! sillybot entry point. See `lib.rs` and `cli.rs`.
//!
//! Output contract for `--format json` (what the report-back driver reads):
//! - ran: a JSON array of findings (possibly empty), exit 0 (no blocking
//!   finding) or 1 (at least one blocking finding),
//! - error: `{"status":"error","reason":"..."}`, exit 2,
//! - not run: `{"status":"not_run","reason":"..."}`, exit 3.
//!
//! Nothing here ever prints an empty array for a scan that did not run.

use clap::Parser;
use sillybot::cli::{Cli, Command, OutputFormat};
use sillybot::runner::{scan, Outcome, Request};
use sillybot::{pin, translator};
use std::process::ExitCode;
use std::time::Duration;

fn main() -> ExitCode {
    let cli = Cli::parse();
    let Command::Scan {
        path,
        base,
        repo,
        pr,
        timeout,
    } = cli.command;
    if let Err(e) = pin::pinned() {
        return report_not_ran(
            "error",
            &format!("canon/pin.toml is malformed: {e}"),
            cli.format,
            2,
        );
    }
    let req = Request {
        path,
        base,
        repo,
        pr,
        timeout: Duration::from_secs(timeout.max(1)),
    };
    match scan(&req) {
        Outcome::Ran(doc) => match translator::translate_report(&doc) {
            Ok(findings) => print_findings(&findings, cli.format),
            Err(e) => report_not_ran(
                "error",
                &format!("reconcile output is unusable: {e}"),
                cli.format,
                2,
            ),
        },
        Outcome::Error(reason) => report_not_ran("error", &reason, cli.format, 2),
        Outcome::NotRun(reason) => report_not_ran("not_run", &reason, cli.format, 3),
    }
}

fn print_findings(findings: &[translator::Finding], format: OutputFormat) -> ExitCode {
    match format {
        OutputFormat::Json => match serde_json::to_string_pretty(findings) {
            Ok(s) => println!("{s}"),
            Err(e) => return report_not_ran("error", &e.to_string(), format, 2),
        },
        OutputFormat::Text => {
            if findings.is_empty() {
                println!("No docs findings.");
            }
            for f in findings {
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

fn report_not_ran(status: &str, reason: &str, format: OutputFormat, code: u8) -> ExitCode {
    match format {
        OutputFormat::Json => {
            println!(
                "{}",
                serde_json::json!({ "status": status, "reason": reason })
            )
        }
        OutputFormat::Text => println!("sillybot: {status}: {reason}"),
    }
    eprintln!("sillybot: {status}: {reason}");
    ExitCode::from(code)
}
