// SPDX-License-Identifier: MPL-2.0
//! CLI for idris-dependents.
//!
//!   GITHUB_TOKEN=$(gh auth token) idris-dependents scan \
//!     --owners hyperpolymath,metadatastician --json idris-dependents.json

use std::collections::BTreeMap;
use std::path::PathBuf;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};

use idris_dependents::scan::{self, Io, Options, Report, Response};
use idris_dependents::{ipkg, refs};

#[derive(Parser)]
#[command(
    version,
    about = "Find external Idris2 projects that depend on your packages (GitHub code search, GET only)"
)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Search GitHub. Needs GITHUB_TOKEN or GH_TOKEN (code search requires auth).
    Scan {
        /// GitHub owners whose packages to look for.
        #[arg(
            long,
            value_delimiter = ',',
            default_value = "hyperpolymath,metadatastician"
        )]
        owners: Vec<String>,
        /// Your own accounts, never reported (defaults to --owners).
        #[arg(long = "self", value_delimiter = ',')]
        self_accounts: Option<Vec<String>>,
        /// Use these package names instead of cataloguing your .ipkg files.
        #[arg(long, value_delimiter = ',')]
        packages: Option<Vec<String>>,
        /// Also write the report as JSON to this path.
        #[arg(long)]
        json: Option<PathBuf>,
    },
    /// Print the package name and `depends` of a local .ipkg as JSON (offline).
    ParseIpkg { file: PathBuf },
    /// Print the pack.toml custom entries pointing at --owners as JSON (offline).
    ParsePackToml {
        file: PathBuf,
        #[arg(
            long,
            value_delimiter = ',',
            default_value = "hyperpolymath,metadatastician"
        )]
        owners: Vec<String>,
    },
}

struct Live {
    client: reqwest::blocking::Client,
    token: String,
}

impl Io for Live {
    /// Send an authenticated GET requesting raw contents or JSON. Preserve the
    /// HTTP status for the scanner; request and response-body errors propagate.
    fn get(&mut self, url: &str, raw: bool) -> Result<Response> {
        let accept = if raw {
            "application/vnd.github.raw"
        } else {
            "application/vnd.github+json"
        };
        let res = self
            .client
            .get(url)
            .bearer_auth(&self.token)
            .header("Accept", accept)
            .header("X-GitHub-Api-Version", "2022-11-28")
            .header("User-Agent", "idris-dependents")
            .send()
            .with_context(|| format!("GET {url}"))?;
        let status = res.status().as_u16();
        let headers: BTreeMap<String, String> = res
            .headers()
            .iter()
            .filter_map(|(k, v)| {
                Some((
                    k.as_str().to_ascii_lowercase(),
                    v.to_str().ok()?.to_string(),
                ))
            })
            .collect();
        Ok(Response {
            status,
            headers,
            body: res.text()?,
        })
    }
    /// Block the current thread for the requested duration.
    fn sleep(&mut self, d: Duration) {
        std::thread::sleep(d);
    }
    /// Return Unix time in whole seconds, falling back to zero for a clock
    /// before the Unix epoch.
    fn now_secs(&self) -> u64 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0)
    }
    /// Write a diagnostic message to standard error.
    fn log(&mut self, msg: &str) {
        eprintln!("{msg}");
    }
}

/// Print the scan summary, repository evidence, incomplete queries, and skipped URLs to
/// standard output in the report's existing row order.
fn print_report(r: &Report) {
    let owners: std::collections::BTreeSet<&str> =
        r.rows.iter().map(|x| x.owner.as_str()).collect();
    println!(
        "\nChecked {} package names. {} external repo(s), {} owner(s).\n",
        r.catalogue_size,
        r.rows.len(),
        owners.len()
    );
    for row in &r.rows {
        let mut what = Vec::new();
        if !row.deps.is_empty() {
            what.push(format!("depends: {}", row.deps.join(", ")));
        }
        if !row.pack_refs.is_empty() {
            let names: Vec<&str> = row.pack_refs.iter().map(|p| p.name.as_str()).collect();
            what.push(format!("pack: {}", names.join(", ")));
        }
        if !row.sub_refs.is_empty() {
            what.push(format!("submodule: {}", row.sub_refs.join(", ")));
        }
        let conf = serde_json::to_value(row.confidence)
            .ok()
            .and_then(|v| v.as_str().map(str::to_string))
            .unwrap_or_default();
        println!("[{conf}] {}  —  {}", row.repo, what.join(" | "));
        for f in &row.files {
            println!("         {f}");
        }
    }
    if r.rows.is_empty() {
        println!("No external Idris2 dependents found in GitHub's code-search index.");
    }
    if !r.incomplete_queries.is_empty() {
        println!(
            "\nIncomplete (GitHub truncated or timed out): {}",
            r.incomplete_queries.join("; ")
        );
    }
    if !r.skipped_urls.is_empty() {
        println!("\nSkipped unavailable files (HTTP 404, 410, or 451):");
        for url in &r.skipped_urls {
            println!("  {url}");
        }
    }
}

/// Run an offline parser command or an authenticated GitHub scan, printing
/// results to standard output and optionally writing the scan report as JSON.
///
/// Return errors for unreadable input, invalid TOML, missing scan credentials,
/// client construction, scanning, JSON serialization, or report-file writes.
fn main() -> Result<()> {
    match Cli::parse().command {
        Command::ParseIpkg { file } => {
            let text = std::fs::read_to_string(&file)
                .with_context(|| format!("reading {}", file.display()))?;
            let out = serde_json::json!({ "package": ipkg::package_name(&text), "depends": ipkg::depends(&text) });
            println!("{out}");
        }
        Command::ParsePackToml { file, owners } => {
            let text = std::fs::read_to_string(&file)
                .with_context(|| format!("reading {}", file.display()))?;
            println!(
                "{}",
                serde_json::to_string(&refs::pack_toml_refs(&text, &owners)?)?
            );
        }
        Command::Scan {
            owners,
            self_accounts,
            packages,
            json,
        } => {
            let token = std::env::var("GITHUB_TOKEN")
                .or_else(|_| std::env::var("GH_TOKEN"))
                .ok()
                .filter(|t| !t.is_empty())
                .context("set GITHUB_TOKEN (e.g. GITHUB_TOKEN=$(gh auth token)); code search requires auth")?;
            let self_accounts = self_accounts.unwrap_or_else(|| owners.clone());
            let opts = Options {
                owners,
                self_accounts,
                packages,
            };
            let mut io = Live {
                client: reqwest::blocking::Client::builder()
                    .timeout(Duration::from_secs(60))
                    .build()?,
                token,
            };
            let report = scan::run(&opts, &mut io)?;
            print_report(&report);
            if let Some(path) = json {
                std::fs::write(&path, serde_json::to_string_pretty(&report)?)?;
                eprintln!("wrote {}", path.display());
            }
        }
    }
    Ok(())
}
