#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Pre-commit hook: a change to the RSR rule table is checked against the estate.
#
# The rule table in src/rsr.rs is the source of truth for RSR compliance, and it
# has rotted twice before: once by demanding LICENSE.txt (0 of 269 repositories
# carried it) and once by holding the .well-known/security.txt checks advisory
# after the migration they were waiting on had finished.
#
# This hook runs scripts/detect-rsr-drift.sh whenever src/rsr.rs is part of the
# change, so that a rule-table edit cannot land without being measured against
# the repositories it will judge.
#
# The estate census is cached, not fetched here: a 269-repository crawl has no
# business inside a commit. Refresh it first with:
#
#     just refresh-rsr-census
#
# If no census is present the hook still validates the table itself (it parses,
# its severity tuples are well-formed, its migration posture is coherent) and
# says loudly that the estate comparison was skipped. It does not fail the
# commit for a missing census; it does fail it for actual drift.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
DETECTOR="$REPO_ROOT/scripts/detect-rsr-drift.sh"
RULE_TABLE="bots/rhodibot/src/rsr.rs"
CENSUS="${RSR_CENSUS_DIR:-$REPO_ROOT/.rsr-census}"

cd "$REPO_ROOT"

if [ ! -x "$DETECTOR" ]; then
    echo "ERROR: detector not found or not executable: $DETECTOR" >&2
    exit 1
fi

# Only run when the rule table is actually part of this change.
staged=$(git diff --cached --name-only 2>/dev/null || true)
if ! printf '%s\n' "$staged" | grep -qxF "$RULE_TABLE"; then
    # Also run when the detector itself is being changed, since its verdicts are
    # only as good as its parser.
    if ! printf '%s\n' "$staged" | grep -qxF "scripts/detect-rsr-drift.sh"; then
        exit 0
    fi
fi

echo "=== RSR rule-table drift check ==="

if [ -d "$CENSUS" ] && [ -n "$(ls -A "$CENSUS" 2>/dev/null)" ]; then
    repos=$(ls -1 "$CENSUS" | wc -l | tr -d ' ')
    echo "using cached census: $CENSUS ($repos repositories)"
    echo "  (age: $(find "$CENSUS" -maxdepth 1 -type f -printf '%TY-%Tm-%Td\n' 2>/dev/null | sort | head -1))"
    set +e
    "$DETECTOR" --source "$RULE_TABLE" --trees-dir "$CENSUS"
    rc=$?
    set -e
else
    echo "no census at $CENSUS — validating the table only"
    echo "the estate comparison is SKIPPED; run: just refresh-rsr-census"
    set +e
    "$DETECTOR" --source "$RULE_TABLE"
    rc=$?
    set -e
    # Exit 1 from a source-only run means the table itself is broken, which
    # must block. Exit 2 means indeterminate.
fi

case "$rc" in
    0) echo "PASS: no rule-table drift" ;;
    1) echo "FAIL: rule-table drift found — see the report above" >&2; exit 1 ;;
    2) echo "WARN: drift check indeterminate (some inputs unavailable)" >&2; exit 0 ;;
    *) echo "FAIL: drift check errored (rc=$rc)" >&2; exit 1 ;;
esac

exit 0
