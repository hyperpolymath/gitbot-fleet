#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# fleet-report-back-build.sh — build the adapter binaries a report-back run
# needs, into one directory the driver can be pointed at with --bin-dir.
#
# One script, so the reusable workflow and the inline callers (repositories
# whose Actions policy refuses a reusable from another owner) build the same
# way. Per crate: `cargo build --release --locked`. A crate that fails to build
# is reported and skipped, never fatal: the driver then writes that member's
# envelope as `error: adapter binary ... not found`, and the build log holds
# the reason. Failing here would turn every OTHER member's evidence into a
# missing artifact.
#
# panic-attack (for panicbot) is a `cargo install --git` at a pinned commit,
# cached by the caller under $HOME/.panic-attack and reused only when the
# recorded source commit matches (the same recipe as the standards Hypatia
# lane, so both panic-attack-derived rows describe one tool).
#
# Usage:
#   fleet-report-back-build.sh --fleet DIR --bin-dir DIR [--crates "a b c"]
#                              [--panic-attack SHA] [--build-timeout SECONDS]
#
#   --fleet          a gitbot-fleet checkout (bots/<crate>/Cargo.toml)
#   --bin-dir        where the built binaries are copied (created)
#   --crates         crate directory names under bots/, space-separated;
#                    default: read `fleet-report-back.sh --plan` output on stdin
#   --panic-attack   also install panic-attack at this commit into --bin-dir
#   --build-timeout  seconds per cargo invocation (default 900)
#
# Exit 0 when the script ran to completion (whatever built); 2 on usage error.

set -euo pipefail

fleet="" bindir="" crates="" pa_sha="" build_timeout=900
while [ $# -gt 0 ]; do
  case "$1" in
    --fleet) fleet="$2"; shift 2 ;;
    --bin-dir) bindir="$2"; shift 2 ;;
    --crates) crates="$2"; shift 2 ;;
    --panic-attack) pa_sha="$2"; shift 2 ;;
    --build-timeout) build_timeout="$2"; shift 2 ;;
    *) echo "unknown option: $1" >&2; exit 2 ;;
  esac
done
if [ -z "$fleet" ] || [ -z "$bindir" ]; then
  echo "usage: $0 --fleet DIR --bin-dir DIR [--crates \"a b\"] [--panic-attack SHA]" >&2; exit 2
fi
[ -d "$fleet/bots" ] || { echo "not a gitbot-fleet checkout: $fleet" >&2; exit 2; }
mkdir -p "$bindir"; bindir="$(cd "$bindir" && pwd)"
command -v cargo >/dev/null 2>&1 || { echo "::warning title=no cargo::cargo is not on PATH; no adapter can be built" >&2; exit 0; }

if [ -z "$crates" ] && [ ! -t 0 ]; then
  # From `fleet-report-back.sh --plan`: member<TAB>crate<TAB>binary
  crates=$(awk -F'\t' '$2 != "" {print $2}' | sort -u | tr '\n' ' ')
fi

# Print a group heading for $1, using Actions grouping when GITHUB_ACTIONS is nonempty.
group() { if [ -n "${GITHUB_ACTIONS:-}" ]; then echo "::group::$1"; else echo "== $1"; fi; }
# Close an Actions log group; emit nothing when GITHUB_ACTIONS is empty or unset.
endgroup() { if [ -n "${GITHUB_ACTIONS:-}" ]; then echo "::endgroup::"; fi; }
# Report title $1 and message $2 as an Actions warning, or plain text to stderr.
warn() { if [ -n "${GITHUB_ACTIONS:-}" ]; then echo "::warning title=$1::$2"; else echo "warning: $1: $2" >&2; fi; }

built=0 failed=0
for c in $crates; do
  if [ ! -f "$fleet/bots/$c/Cargo.toml" ]; then
    warn "unknown crate" "bots/$c has no Cargo.toml; skipped"; failed=$((failed + 1)); continue
  fi
  group "cargo build --release --locked (bots/$c)"
  if (cd "$fleet/bots/$c" && timeout "$build_timeout" cargo build --release --locked 2>&1); then
    # Every executable the crate produced; the driver picks the one it needs.
    find "$fleet/bots/$c/target/release" -maxdepth 1 -type f -perm -u+x ! -name '*.d' \
      -exec cp -t "$bindir" {} +
    built=$((built + 1))
  else
    warn "adapter build failed" "bots/$c did not build; its row will read error"
    failed=$((failed + 1))
  fi
  endgroup
done

if [ -n "$pa_sha" ]; then
  root="$HOME/.panic-attack"; bin="$root/bin/panic-attack"
  group "panic-attack at $pa_sha"
  if [ -x "$bin" ] && grep -qF "#$pa_sha" "$root/.crates.toml" 2>/dev/null; then
    echo "panic-attack restored from cache at $pa_sha"
  elif ! timeout 600 cargo install --git https://github.com/hyperpolymath/panic-attack \
      --rev "$pa_sha" --locked --force --root "$root" panic-attack; then
    rm -rf "$root"   # leave nothing for the cache to save from a failed build
    warn "panic-attack not installed" "source build at $pa_sha failed or exceeded 10 minutes; panicbot will read error"
  fi
  if [ -x "$bin" ]; then cp "$bin" "$bindir/"; fi
  endgroup
fi

echo "fleet-report-back-build: $built crate(s) built, $failed failed; binaries in $bindir:"
find "$bindir" -maxdepth 1 -type f -printf '  %f\n' | sort
exit 0
