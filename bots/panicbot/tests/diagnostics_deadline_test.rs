// SPDX-License-Identifier: MPL-2.0
//! `--version` and `diagnostics` share ONE deadline: a preflight given `t`
//! seconds must not take up to `2t`. Own test file (own process) because it
//! sets `PATH`.

use panicbot::scanner;
use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::time::{Duration, Instant};
use tempfile::TempDir;

#[test]
fn version_and_diagnostics_share_one_deadline() {
    let bin = TempDir::new().unwrap();
    let p = bin.path().join("panic-attack");
    fs::write(
        &p,
        "#!/usr/bin/env bash\ncase \"$1\" in --version) sleep 2; echo 'panic-attack 2.5.5' ;; *) sleep 20 ;; esac\n",
    )
    .unwrap();
    fs::set_permissions(&p, fs::Permissions::from_mode(0o755)).unwrap();
    std::env::set_var("PATH", format!("{}:/usr/bin:/bin", bin.path().display()));

    let t = Instant::now();
    let r = scanner::run_diagnostics(Duration::from_secs(3));
    let took = t.elapsed();
    assert!(r.is_err(), "diagnostics hangs, so the preflight must fail");
    assert!(
        took < Duration::from_millis(4200),
        "the two invocations must share one 3s deadline, took {took:?}"
    );
}
