#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# propagate-sha-bump.sh — actuation half of the three-system propagation arch.
#
#   hypatia (detection)  →  gitbot-fleet (THIS)  →  .git-private-farm (propagation)
#
# Consumes a hypatia finding with
#   rule = reusable_workflow_sha_bump_needs_propagation
# (see hyperpolymath/hypatia#418), pre-filters by title keyword (HARD —
# per feedback_pr_sweep_title_keyword_exclusion + feedback_no_automated_licence_edits),
# enumerates estate consumers pinning the old SHA, and fires a
# repository_dispatch event of type `propagate-sha-bump` into
# hyperpolymath/.git-private-farm where the receiver workflow runs
# `scripts/sha-bump-propagate.sh`.
#
# Usage (called by dispatch-runner.sh):
#   propagate-sha-bump.sh <repo_path_ignored> <finding.json>
#
# `repo_path` is ignored — this script operates on the finding alone, not
# on the upstream repo's working tree.
#
# Required env:
#   GH_TOKEN              gh CLI auth, repo + workflow scopes
#
# Optional env:
#   FARM_REPO             default "hyperpolymath/.git-private-farm"
#   DRY_RUN               "true" prints the payload without dispatching
set -euo pipefail

usage() {
    echo "Usage: $0 <repo_path_ignored> <finding.json>" >&2
    exit 64
}

[[ $# -ge 2 ]] || usage

FINDING_FILE="$2"
[[ -f "$FINDING_FILE" ]] || { echo "ERROR: finding file not found: $FINDING_FILE" >&2; exit 1; }

FARM_REPO="${FARM_REPO:-hyperpolymath/.git-private-farm}"
DRY_RUN="${DRY_RUN:-false}"

# Title-keyword exclusion regex. Keep in sync with:
#   feedback_pr_sweep_title_keyword_exclusion
#   farm receiver workflow .github/workflows/sha-bump-propagate.yml
# Case-insensitive — grep -iE.
FORBIDDEN_KEYWORDS='license|SPDX|PMPL|MPL|AGPL|GPL|Apache|copyright|attribution|relicens|secret|vulnerab|CVE-'

# --- 1. Parse finding ----------------------------------------------------------

rule=$(jq -r '.rule // ""' "$FINDING_FILE")
source_repo=$(jq -r '.source_repo // ""' "$FINDING_FILE")
source_workflow=$(jq -r '.source_workflow // ""' "$FINDING_FILE")
old_sha=$(jq -r '.old_sha // ""' "$FINDING_FILE")
new_sha=$(jq -r '.new_sha // ""' "$FINDING_FILE")
pr_title=$(jq -r '.pr_title // ""' "$FINDING_FILE")
pr_number=$(jq -r '.pr_number // ""' "$FINDING_FILE")

# Hard rule-name gate — refuse to operate on findings of any other shape.
if [[ "$rule" != "reusable_workflow_sha_bump_needs_propagation" ]]; then
    echo "ERROR: finding rule mismatch: got '$rule', expected 'reusable_workflow_sha_bump_needs_propagation'" >&2
    exit 1
fi

# --- 2. SHA + path validation -------------------------------------------------

for v in old_sha new_sha; do
    val="${!v}"
    if ! printf '%s' "$val" | grep -qE '^[0-9a-f]{40}$'; then
        echo "ERROR: $v is not a 40-char hex SHA: $val" >&2
        exit 1
    fi
done

if [[ "$old_sha" == "$new_sha" ]]; then
    echo "ERROR: old_sha equals new_sha — nothing to propagate" >&2
    exit 1
fi

case "$source_repo" in
    hyperpolymath/*) ;;
    *) echo "ERROR: source_repo not in hyperpolymath/* : '$source_repo'" >&2; exit 1 ;;
esac

case "$source_workflow" in
    .github/workflows/*.yml|.github/workflows/*.yaml|action.yml|action.yaml) ;;
    *) echo "ERROR: source_workflow not in expected shape: '$source_workflow'" >&2; exit 1 ;;
esac

# --- 3. Title-keyword pre-filter (HARD) ---------------------------------------

# Per feedback_no_automated_licence_edits: licence/SPDX changes are MANUAL,
# even if policy-correct. The receiver workflow re-checks this (belt-and-braces),
# but the canonical gate lives HERE.
if printf '%s' "$pr_title" | grep -iqE "$FORBIDDEN_KEYWORDS"; then
    echo "REFUSED: pr_title matched forbidden keyword pattern — routing to manual review." >&2
    echo "         source_repo=$source_repo source_workflow=$source_workflow" >&2
    echo "         pr_title=$pr_title" >&2
    echo "         Owner must approve and apply this bump manually, per-consumer." >&2
    exit 0  # NOT an error — this is the expected, correct refusal path.
fi

# --- 4. Build consumer TSV via code search ------------------------------------

# Construct the search pattern the codebases use to pin this workflow.
# Example: `uses: hyperpolymath/standards/.github/workflows/governance-reusable.yml@<OLD_SHA>`
# (with `@<OLD_SHA>` truncated — gh code-search is whitespace-tolerant).
# We search for the path + SHA combination; the TSV emits `<owner>/<repo>\t<workflow_path>`.

# Strip the `.github/workflows/` prefix for the search needle, since the full
# `uses: …` line includes the source repo path.
needle="${source_repo}/${source_workflow}@${old_sha}"

TMPDIR_RUN=$(mktemp -d -t propagate-sha-bump.XXXXXX)
trap 'rm -rf "$TMPDIR_RUN"' EXIT

CONSUMERS_TSV="$TMPDIR_RUN/consumers.tsv"

echo "Enumerating consumers pinning: $needle" >&2

# gh code-search has a 100-result cap per query. For larger sweeps the
# operator should pre-build a TSV manually and supply it via a CONSUMERS_TSV
# env override. Tracked here for posterity.
if [[ -n "${CONSUMERS_TSV_OVERRIDE:-}" && -f "$CONSUMERS_TSV_OVERRIDE" ]]; then
    cp "$CONSUMERS_TSV_OVERRIDE" "$CONSUMERS_TSV"
    echo "Using override consumers TSV: $CONSUMERS_TSV_OVERRIDE" >&2
else
    gh search code "$needle" --owner hyperpolymath --limit 100 \
        --json repository,path \
        --jq '.[] | select(.path | startswith(".github/workflows/")) | "\(.repository.nameWithOwner)\t\(.path)"' \
        > "$CONSUMERS_TSV" || true
fi

# Drop fork repos — per estate license policy, third-party / forked stuff is
# off-limits. (gh search code does not filter forks; we look up each owner-repo
# pair and skip forks.) For large sweeps this round-trips N times — cache as
# needed.
filter_forks() {
    local tsv="$1"
    local out="${tsv}.no-forks"
    : > "$out"
    while IFS=$'\t' read -r repo path; do
        local is_fork
        is_fork=$(gh repo view "$repo" --json isFork --jq '.isFork' 2>/dev/null || echo "true")
        if [[ "$is_fork" == "false" ]]; then
            printf '%s\t%s\n' "$repo" "$path" >> "$out"
        else
            echo "SKIP (fork): $repo" >&2
        fi
    done < "$tsv"
    mv "$out" "$tsv"
}

# Skip fork-filter if the operator supplied an override TSV — they've already vetted it.
if [[ -s "$CONSUMERS_TSV" && -z "${CONSUMERS_TSV_OVERRIDE:-}" ]]; then
    filter_forks "$CONSUMERS_TSV"
fi

n_consumers=$(wc -l < "$CONSUMERS_TSV")
echo "Consumers identified: $n_consumers" >&2

if [[ "$n_consumers" -eq 0 ]]; then
    echo "No estate consumers found for $needle — nothing to propagate." >&2
    exit 0
fi

# --- 5. Compose payload + fire repository_dispatch ----------------------------

# Slug the workflow basename for branch name.
workflow_slug=$(basename "$source_workflow" .yml | tr '/.' '--')
short_new_sha="${new_sha:0:7}"

branch_name="ci/bump-${workflow_slug}-${short_new_sha}"

# title_suffix re-checked against forbidden keywords; we synthesise it from
# safe metadata only (workflow slug + short SHA), NOT from pr_title.
title_suffix="bump ${source_workflow}@${short_new_sha}"

body_blurb=$(cat <<EOF
Upstream SHA bump propagation.

- Reusable: \`${source_repo}/${source_workflow}\`
- Old: \`${old_sha}\`
- New: \`${new_sha}\`
- Upstream PR: ${source_repo}#${pr_number}
- Driven by: hypatia rule \`reusable_workflow_sha_bump_needs_propagation\` (gitbot-fleet propagate-sha-bump.sh).
EOF
)

# Build client_payload as JSON.
consumers_blob=$(cat "$CONSUMERS_TSV")

payload=$(jq -n \
    --arg reusable_path "${source_repo}/${source_workflow}" \
    --arg old_sha "$old_sha" \
    --arg new_sha "$new_sha" \
    --arg branch_name "$branch_name" \
    --arg title_suffix "$title_suffix" \
    --arg body_blurb "$body_blurb" \
    --arg consumers "$consumers_blob" \
    '{
        event_type: "propagate-sha-bump",
        client_payload: {
            reusable_path: $reusable_path,
            old_sha: $old_sha,
            new_sha: $new_sha,
            branch_name: $branch_name,
            title_suffix: $title_suffix,
            body_blurb: $body_blurb,
            consumers: $consumers
        }
    }')

if [[ "$DRY_RUN" == "true" ]]; then
    echo "DRY-RUN — would dispatch to $FARM_REPO:" >&2
    printf '%s\n' "$payload"
    exit 0
fi

echo "Firing repository_dispatch propagate-sha-bump → $FARM_REPO ($n_consumers consumers)" >&2

printf '%s' "$payload" \
    | gh api -X POST "repos/${FARM_REPO}/dispatches" --input -

echo "OK: dispatch fired. Receiver workflow will run async on $FARM_REPO." >&2
