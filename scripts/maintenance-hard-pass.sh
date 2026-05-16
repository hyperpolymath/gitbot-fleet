#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later

set -euo pipefail

usage() {
    cat <<USAGE
usage: $(basename "$0") --repo <path> [--output <path>] [--panic-bin <path>] [-- <extra-args>]

Run repository maintenance in release hard-pass mode.
Fails if the target report has warnings or failures.
USAGE
}

repo=""
output=""
panic_bin="${PANIC_ATTACK_BIN:-}"
extra_args=()

while [[ $# -gt 0 ]]; do
    case "$1" in
        --repo)
            repo="${2:-}"
            shift 2
            ;;
        --output)
            output="${2:-}"
            shift 2
            ;;
        --panic-bin)
            panic_bin="${2:-}"
            shift 2
            ;;
        --help|-h)
            usage
            exit 0
            ;;
        --)
            shift
            extra_args+=("$@")
            break
            ;;
        *)
            extra_args+=("$1")
            shift
            ;;
    esac
done

if [[ -z "$repo" ]]; then
    echo "error: --repo is required" >&2
    usage >&2
    exit 2
fi

repo="$(cd "$repo" && pwd)"
repo_name="$(basename "$repo")"
ts="$(date -u +%Y%m%dT%H%M%SZ)"

if [[ -z "$output" ]]; then
    output="/tmp/gitbot-fleet-maintenance/${repo_name}-${ts}.json"
fi
mkdir -p "$(dirname "$output")"

runner=""
if [[ -x "$repo/scripts/maintenance/run-maintenance.sh" ]]; then
    runner="$repo/scripts/maintenance/run-maintenance.sh"
elif [[ -x "$repo/run-maintenance.sh" ]]; then
    runner="$repo/run-maintenance.sh"
elif [[ -x "/var$REPOS_DIR/run-maintenance.sh" ]]; then
    runner="/var$REPOS_DIR/run-maintenance.sh"
else
    echo "error: no maintenance runner found for $repo" >&2
    echo "expected one of:" >&2
    echo "  $repo/scripts/maintenance/run-maintenance.sh" >&2
    echo "  $repo/run-maintenance.sh" >&2
    echo "  /var$REPOS_DIR/run-maintenance.sh" >&2
    exit 2
fi

if [[ -z "$panic_bin" && -x "/var$REPOS_DIR/panic-attacker/target/release/panic-attack" ]]; then
    panic_bin="/var$REPOS_DIR/panic-attacker/target/release/panic-attack"
fi

cmd=("$runner" --repo "$repo" --output "$output" --strict --fail-on-warn)
if [[ -n "$panic_bin" ]]; then
    cmd+=(--panic-bin "$panic_bin")
fi
if [[ ${#extra_args[@]} -gt 0 ]]; then
    cmd+=("${extra_args[@]}")
fi

"${cmd[@]}"

if command -v jq >/dev/null 2>&1 && [[ -f "$output" ]]; then
    echo "report: $output"
    echo "summary:"
    jq '.summary' "$output"
else
    echo "report: $output"
fi
