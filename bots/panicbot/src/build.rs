// SPDX-License-Identifier: MPL-2.0
//! Runs the deed-declared `:build` command for the dynamic modes.
//!
//! `sh -c COMMAND` in the scanned directory, in its own process group, with a
//! timeout. The group is killed on timeout so a build that forked helpers does
//! not outlive the lane. Output is captured (never written to panicbot's
//! stdout, which carries the JSON) and the tail of it is quoted in the error.
//! This EXECUTES repository code: callers gate it exactly like the dynamic
//! modes (see [`crate::suite::dynamic_refusal`]).

use anyhow::{anyhow, bail, Context, Result};
use std::io::Read;
use std::path::Path;
use std::process::{Command, Stdio};
use std::sync::mpsc;
use std::time::{Duration, Instant};

/// How much of the build output an error quotes.
const TAIL_BYTES: usize = 1500;

/// Run `command` with `sh -c` in `dir`. `Ok` only on exit status 0.
pub fn run_build(command: &str, dir: &Path, timeout: Duration) -> Result<()> {
    use std::os::unix::process::CommandExt;
    let mut child = Command::new("sh")
        .arg("-c")
        .arg(command)
        .current_dir(dir)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .process_group(0)
        .spawn()
        .with_context(|| format!("could not start the build `{command}`"))?;

    let (tx, rx) = mpsc::channel::<Vec<u8>>();
    for stream in [
        child
            .stdout
            .take()
            .map(|s| Box::new(s) as Box<dyn Read + Send>),
        child
            .stderr
            .take()
            .map(|s| Box::new(s) as Box<dyn Read + Send>),
    ]
    .into_iter()
    .flatten()
    {
        let tx = tx.clone();
        std::thread::spawn(move || {
            let mut stream = stream;
            let mut tail: Vec<u8> = Vec::new();
            let mut buf = [0u8; 4096];
            while let Ok(n) = stream.read(&mut buf) {
                if n == 0 {
                    break;
                }
                tail.extend_from_slice(&buf[..n]);
                if tail.len() > TAIL_BYTES * 2 {
                    tail.drain(..tail.len() - TAIL_BYTES);
                }
            }
            let _ = tx.send(tail);
        });
    }
    drop(tx);

    let start = Instant::now();
    let status = loop {
        if let Some(st) = child.try_wait().context("waiting for the build failed")? {
            break st;
        }
        if start.elapsed() >= timeout {
            let _ = Command::new("kill")
                .args(["-KILL", "--", &format!("-{}", child.id())])
                .status();
            let _ = child.kill();
            let _ = child.wait();
            bail!(
                "the build `{command}` timed out after {}s and was killed",
                timeout.as_secs()
            );
        }
        std::thread::sleep(Duration::from_millis(20));
    };

    if status.success() {
        return Ok(());
    }
    let mut out: Vec<u8> = Vec::new();
    while let Ok(part) = rx.recv_timeout(Duration::from_millis(500)) {
        out.extend(part);
    }
    let text = String::from_utf8_lossy(&out);
    let text = text.trim();
    let text = if text.len() > TAIL_BYTES {
        let mut from = text.len() - TAIL_BYTES;
        while !text.is_char_boundary(from) {
            from += 1;
        }
        &text[from..]
    } else {
        text
    };
    Err(anyhow!(
        "the build `{command}` failed ({status}){}",
        if text.is_empty() {
            String::new()
        } else {
            format!(": {}", text.replace('\n', " | "))
        }
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_successful_build_runs_in_the_directory() {
        let d = tempfile::tempdir().unwrap();
        run_build("touch built", d.path(), Duration::from_secs(10)).unwrap();
        assert!(d.path().join("built").exists());
    }

    #[test]
    fn a_failing_build_quotes_its_output() {
        let d = tempfile::tempdir().unwrap();
        let e = run_build("echo boom >&2; exit 3", d.path(), Duration::from_secs(10))
            .unwrap_err()
            .to_string();
        assert!(e.contains("failed") && e.contains("boom"), "{e}");
    }

    #[test]
    fn a_hung_build_is_killed_at_the_timeout() {
        let d = tempfile::tempdir().unwrap();
        let t = Instant::now();
        let e = run_build("sleep 30 & sleep 30", d.path(), Duration::from_millis(300))
            .unwrap_err()
            .to_string();
        assert!(e.contains("timed out"), "{e}");
        assert!(t.elapsed() < Duration::from_secs(10));
    }
}
