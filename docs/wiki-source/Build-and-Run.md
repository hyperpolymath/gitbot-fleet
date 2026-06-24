<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
<!-- SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell -->

# Build & Run

## Prerequisites

- `just` (>= 1.19.0) — see `hyperpolymath/standards` `R0` rule
- `bash` (>= 4.x) for `set -euo pipefail` semantics
- Rust toolchain (latest stable) for `shared-context/` and `robot-repo-automaton/`
- `jq` for JSON manipulation in shell pipelines
- `cargo` + `OPENSSL_NO_VENDOR=1` for the Rust executor build

## Maintenance gating

```bash
just maintenance-hard-pass /absolute/path/to/repo
just enroll-repos
just scan-supervised
```

- **`maintenance-hard-pass`** enforces fail-on-warn release gating using
  the target repo's maintenance script.
- **`enroll-repos`** refreshes repository coverage metadata. Pass
  `/var$REPOS_DIR true` to write enrollment directives into repos that
  already have `.machine_readable/`.
- **`scan-supervised`** runs Hypatia across the supervised inventory
  (`~/.git-private-farm.scm`, `~/.git-private-repos`, or the enrollment
  registry) and processes findings for fleet dispatch.

## Rust executor

```bash
cd robot-repo-automaton
OPENSSL_NO_VENDOR=1 cargo build --release
./target/release/robot-repo-automaton scan /path/to/repo
./target/release/robot-repo-automaton fix /path/to/repo --create-pr
```

## Useful `just` recipes

| Recipe | Purpose |
|---|---|
| `setup` | One-shot dev environment setup |
| `build` | Build everything (Rust + shell lints) |
| `test` | Run tests |
| `doctor` | Diagnose environment issues |
| `heal` | Auto-repair common issues |
| `hypatia-scan` | Run a Hypatia scan locally |
| `panic-scan` | Run panicbot's crash-surface scan |
| `crg-grade` | Compute Component Readiness Grade |
| `crg-badge` | Refresh CRG badge artefact |

See the `Justfile` at the repo root for the authoritative list.
