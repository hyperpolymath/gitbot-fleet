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
#   GH_BIN                GitHub CLI command (test seam; default "gh")
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
GH_BIN="${GH_BIN:-gh}"

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

# A SHA can exist in GitHub's object database yet be unusable as a cross-repo
# reusable workflow. Feature-branch commits that are later squash-merged have
# exactly that shape: Contents/Commits APIs resolve them, but Actions rejects
# `uses: ...@sha` at startup with `workflow was not found`. Prove the proposed
# target is on the source repository's default-branch history before searching
# consumers or dispatching a mutation.
default_branch=$($GH_BIN api "repos/${source_repo}" --jq '.default_branch') || {
    echo "ERROR: could not resolve ${source_repo}'s default branch" >&2
    exit 1
}

compare_status=$($GH_BIN api \
    "repos/${source_repo}/compare/${new_sha}...${default_branch}" \
    --jq '.status') || {
    echo "ERROR: could not verify ${new_sha} against ${source_repo}:${default_branch}" >&2
    exit 1
}

case "$compare_status" in
    identical|ahead) ;;
    *)
        echo "ERROR: new_sha ${new_sha} exists but is not reachable from ${source_repo}:${default_branch} (compare status: ${compare_status:-unknown})" >&2
        exit 1
        ;;
esac

if [[ "$source_workflow" == .github/workflows/* ]]; then
    source_type=$($GH_BIN api \
        "repos/${source_repo}/contents/${source_workflow}?ref=${new_sha}" \
        --jq '.type') || {
        echo "ERROR: ${source_workflow} does not resolve at ${source_repo}@${new_sha}" >&2
        exit 1
    }
    [[ "$source_type" == "file" ]] || {
        echo "ERROR: ${source_workflow} at ${source_repo}@${new_sha} is not a file" >&2
        exit 1
    }
fi

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

# Search both named estates, page to the authoritative `total_count`, and fail
# rather than silently truncate if GitHub's 1,000-result search horizon is ever
# reached. The old `gh search code --limit 100 --owner hyperpolymath` path both
# capped its answer and omitted metadatastician entirely.
if [[ -n "${CONSUMERS_TSV_OVERRIDE:-}" && -f "$CONSUMERS_TSV_OVERRIDE" ]]; then
    cp "$CONSUMERS_TSV_OVERRIDE" "$CONSUMERS_TSV"
    echo "Using override consumers TSV: $CONSUMERS_TSV_OVERRIDE" >&2
else
    : > "$CONSUMERS_TSV"

    # Paginate GitHub code-search for a given scope (user:… or org:…), validate
    # result integrity, and append workflow YAML paths to CONSUMERS_TSV. Fails
    # if total_count exceeds GitHub's 1,000-result horizon or if the API returns
    # incomplete_results=true.
    #
    # Args:
    #   $1  scope qualifier (e.g. "user:hyperpolymath" or "org:metadatastician")
    # Env:
    #   needle          search pattern (reusable path + old SHA)
    #   GH_BIN          GitHub CLI command
    #   CONSUMERS_TSV   output file (appended)
    # Returns:
    #   0 on success, 1 on search failure or integrity violation
    search_scope() {
        local scope="$1" page=1 body total incomplete count
        while :; do
            body=$($GH_BIN api -X GET search/code \
                -f "q=${needle} ${scope}" \
                -F per_page=100 \
                -F "page=${page}") || {
                echo "ERROR: code search failed for scope '${scope}' page ${page}" >&2
                return 1
            }

            total=$(jq -r '.total_count' <<<"$body")
            incomplete=$(jq -r '.incomplete_results' <<<"$body")
            [[ "$total" =~ ^[0-9]+$ && "$incomplete" == false ]] || {
                echo "ERROR: invalid/incomplete code-search result for '${scope}'" >&2
                return 1
            }
            (( total <= 1000 )) || {
                echo "ERROR: '${scope}' has ${total} matches, beyond GitHub's 1,000-result search horizon; split the query before propagating" >&2
                return 1
            }

            jq -r '.items[]
                | select(.path | startswith(".github/workflows/"))
                | select(.path | test("\\.ya?ml$"))
                | "\(.repository.full_name)\t\(.path)"' <<<"$body" >> "$CONSUMERS_TSV"

            count=$(jq '.items | length' <<<"$body")
            (( count == 100 && page * 100 < total )) || break
            page=$((page + 1))
        done
    }

    search_scope "user:hyperpolymath"
    search_scope "org:metadatastician"
    sort -u -o "$CONSUMERS_TSV" "$CONSUMERS_TSV"
fi

# Drop fork repos — per estate license policy, third-party / forked stuff is
# off-limits. (gh search code does not filter forks; we look up each owner-repo
# pair and skip forks.) For large sweeps this round-trips N times — cache as
# needed.
#
# Filters a TSV of repo + workflow path pairs, removing forks, archived repos,
# and inaccessible repositories. Overwrites the input file in-place with the
# filtered result.
#
# Args:
#   $1  path to TSV file (format: "owner/repo<TAB>workflow_path")
# Env:
#   GH_BIN  GitHub CLI command
# Side effects:
#   Overwrites the input TSV with filtered content (non-fork, non-archived,
#   accessible repos only). Logs skipped repos to stderr.
filter_forks() {
    local tsv="$1"
    local out="${tsv}.no-forks"
    : > "$out"
    while IFS=$'\t' read -r repo path; do
        local repo_state
        repo_state=$($GH_BIN api "repos/${repo}" --jq '[.fork, .archived] | @tsv' 2>/dev/null || printf 'true\ttrue\n')
        if [[ "$repo_state" == $'false\tfalse' ]]; then
            printf '%s\t%s\n' "$repo" "$path" >> "$out"
        else
            echo "SKIP (fork, archived, or inaccessible): $repo" >&2
        fi
    done < "$tsv"
    mv "$out" "$tsv"
}

# Apply the same active-root eligibility check to searched and overridden
# consumers so the test seam cannot bypass the production safety boundary.
if [[ -s "$CONSUMERS_TSV" ]]; then
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
    | "$GH_BIN" api -X POST "repos/${FARM_REPO}/dispatches" --input -

echo "OK: dispatch fired. Receiver workflow will run async on $FARM_REPO." >&2
