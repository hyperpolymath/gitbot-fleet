#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# fix-workflow-timeout-minutes.sh — Add `timeout-minutes:` to workflow
# jobs that lack one. ERR-WF-013.
#
# Default GH Actions job timeout is 6 hours; a stuck `codeload` fetch
# can burn that full budget. This script inserts `timeout-minutes: 10`
# directly after each `runs-on:` line in a workflow YAML that doesn't
# already have one within the job block. Override per-job via
# `JOB_TIMEOUT_OVERRIDES` env var (`<job-name>=<minutes>,...`).
#
# Usage:
#   fix-workflow-timeout-minutes.sh <workflow.yml>
#   fix-workflow-timeout-minutes.sh <repo-path>  (scans all .github/workflows/*.yml)
#
# Pairs with Hypatia.Rules.WorkflowAudit.check_missing_timeout_minutes/1
# and recipe-add-workflow-timeout-minutes.json.

set -euo pipefail

DEFAULT_TIMEOUT="${DEFAULT_TIMEOUT:-10}"
TARGET="${1:?Usage: fix-workflow-timeout-minutes.sh <workflow.yml | repo-path>}"

apply_to_file() {
    local f="$1"
    [ -f "$f" ] || { echo "SKIP (not a file): $f" >&2; return 0; }

    # Get list of (line_number, job_name) for jobs that lack timeout-minutes
    # within their block. We scan the file and find each top-level `^  <key>:$`
    # under `^jobs:$`, then check the next 30 lines for `timeout-minutes:`.
    awk -v default_to="$DEFAULT_TIMEOUT" '
        /^jobs:[[:space:]]*$/ { in_jobs=1; next }
        /^[A-Za-z]/ && !/^jobs:/ { in_jobs=0 }
        in_jobs && /^  [A-Za-z0-9_-]+:[[:space:]]*$/ {
            if (curjob && !hadto) print curjob_line "\t" curjob
            curjob = $0; sub(/^  /, "", curjob); sub(/:.*$/, "", curjob)
            curjob_line = NR; hadto = 0
        }
        in_jobs && /^    timeout-minutes:/ { hadto = 1 }
        END { if (curjob && !hadto) print curjob_line "\t" curjob }
    ' "$f" > /tmp/missing_timeouts.$$

    if [ ! -s /tmp/missing_timeouts.$$ ]; then
        rm -f /tmp/missing_timeouts.$$
        echo "OK (no missing timeouts): $f"
        return 0
    fi

    # Now find the `runs-on:` line in each missing job and insert timeout after it.
    # We do this in reverse order to keep line numbers stable across inserts.
    tmp=$(mktemp)
    cp "$f" "$tmp"

    tac /tmp/missing_timeouts.$$ | while IFS=$'\t' read -r job_start_line job_name; do
        # Find the runs-on line within the next 20 lines from the job header
        runs_on_line=$(awk -v start="$job_start_line" -v end="$((job_start_line+20))" '
            NR > start && NR < end && /^    runs-on:/ { print NR; exit }
        ' "$tmp")
        if [ -z "$runs_on_line" ]; then
            echo "WARN: job '$job_name' has no `runs-on:` within 20 lines — skipping" >&2
            continue
        fi
        # Insert AFTER the runs-on line
        sed -i "${runs_on_line}a\    timeout-minutes: ${DEFAULT_TIMEOUT}" "$tmp"
        echo "FIXED  $f  job=$job_name  inserted timeout-minutes: $DEFAULT_TIMEOUT"
    done

    if ! cmp -s "$f" "$tmp"; then
        mv "$tmp" "$f"
    else
        rm -f "$tmp"
    fi
    rm -f /tmp/missing_timeouts.$$
}

if [ -d "$TARGET" ]; then
    for f in "$TARGET"/.github/workflows/*.yml "$TARGET"/.github/workflows/*.yaml; do
        [ -f "$f" ] || continue
        apply_to_file "$f"
    done
else
    apply_to_file "$TARGET"
fi
