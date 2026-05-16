#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-workflow-hygiene.sh — Remove irrelevant CI workflows detected by Hypatia
#
# Accepts a finding JSON file. Reads the pattern ID and file path, then
# removes the irrelevant workflow file. Idempotent: no-op if file already gone.
#
# Patterns handled:
#   irrelevant_ts_blocker, irrelevant_npm_blocker, irrelevant_jekyll,
#   irrelevant_guix_nix, irrelevant_wellknown, irrelevant_rsr_antipattern,
#   redundant_scorecard_enforcer, redundant_instant_sync,
#   redundant_security_policy_wf, unnecessary_workflow_linter
set -euo pipefail

FINDING_FILE="${1:?Usage: fix-workflow-hygiene.sh <finding.json>}"

if [[ ! -f "$FINDING_FILE" ]]; then
    echo "ERROR: Finding file not found: $FINDING_FILE" >&2
    exit 1
fi

PATTERN=$(jq -r '.pattern' "$FINDING_FILE")
FILE=$(jq -r '.file' "$FINDING_FILE")

# Validate this is a workflow hygiene finding we can handle
case "$PATTERN" in
    irrelevant_ts_blocker|irrelevant_npm_blocker|irrelevant_jekyll|\
    irrelevant_guix_nix|irrelevant_wellknown|irrelevant_rsr_antipattern|\
    redundant_scorecard_enforcer|redundant_instant_sync|\
    redundant_security_policy_wf|unnecessary_workflow_linter)
        ;;
    *)
        echo "SKIP: Pattern '$PATTERN' not handled by fix-workflow-hygiene.sh" >&2
        exit 0
        ;;
esac

# Idempotent: if file already removed, nothing to do
if [[ ! -f "$FILE" ]]; then
    echo "OK: $FILE already removed (idempotent)"
    exit 0
fi

WF_NAME=$(basename "$FILE")
REPO_DIR=$(git -C "$(dirname "$FILE")" rev-parse --show-toplevel 2>/dev/null || echo "")

if [[ -z "$REPO_DIR" ]]; then
    echo "ERROR: Cannot determine git repo root for $FILE" >&2
    exit 1
fi

# Remove the workflow
rm "$FILE"

# Stage and commit
cd "$REPO_DIR"
git add "$FILE"

# Only commit if there are staged changes
if ! git diff --cached --quiet; then
    git commit -m "$(cat <<EOF
Remove irrelevant workflow: $WF_NAME

Hypatia detected $WF_NAME as irrelevant to this repository's
technology stack (pattern: $PATTERN). Removing to reduce CI noise
and GitHub Actions minutes waste.

Co-Authored-By: Hypatia Scanner <hypatia@reposystem.dev>
EOF
)"
    echo "OK: Removed $WF_NAME from $(basename "$REPO_DIR")"
else
    echo "OK: No staged changes (already clean)"
fi
