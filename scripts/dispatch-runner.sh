#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# dispatch-runner.sh — Execute actions from hypatia dispatch manifests
#
# Reads pending.jsonl from verisimdb-data/dispatch/ and executes fixes
# via robot-repo-automaton CLI, gitbot-fleet fix scripts, or advisory logging.
#
# Usage:
#   dispatch-runner.sh [OPTIONS]
#
# Options:
#   --dry-run         Show what would be done without executing
#   --tier TIER       Only process entries matching tier (eliminate|substitute|control)
#   --strategy STRAT  Only process entries matching strategy (auto_execute|review|report_only)
#   --repo REPO       Only process entries for a specific repo
#   --limit N         Process at most N entries
#   --manifest PATH   Path to manifest (default: verisimdb-data/dispatch/pending.jsonl)
#   --parallel N      Run up to N fix scripts concurrently (default: 1)
#   --dedup-repo      Group entries by repo, run only first per repo+category

set -euo pipefail

# --- Input Validation ---
# Validate that a value is a safe path component (no traversal, no slashes in leaf)
validate_safe_name() {
    local name="$1"
    local label="${2:-value}"
    if [[ "$name" == *".."* || "$name" == *"/"* || "$name" == "" ]]; then
        echo "ERROR: Unsafe $label: '$name' (contains path separator or traversal)" >&2
        return 1
    fi
    return 0
}

# Validate that a path stays within a base directory (prevents symlink/traversal escape)
validate_path_within() {
    local path="$1"
    local base="$2"
    local resolved
    resolved=$(realpath -m "$path" 2>/dev/null || echo "$path")
    local resolved_base
    resolved_base=$(realpath -m "$base" 2>/dev/null || echo "$base")
    if [[ "$resolved" != "$resolved_base"/* && "$resolved" != "$resolved_base" ]]; then
        echo "ERROR: Path '$path' escapes base '$base'" >&2
        return 1
    fi
    return 0
}

# --- Configuration ---
# This repo's own root, resolved relative to this script so the fleet never
# reaches into a different checkout of itself.
FLEET_ROOT="${FLEET_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"

# Base directory holding local repo checkouts. Override with REPOS_BASE.
# This value is also the containment boundary enforced by validate_path_within.
REPOS_BASE="${REPOS_BASE:-$HOME/developer/hyper-repos}"

# Hypatia's local data store is the primary source for dispatch manifests.
# Falls back to central verisimdb-data if HYPATIA_DATA is not set.
HYPATIA_DATA="${HYPATIA_DATA:-$REPOS_BASE/verisimdb-data}"
VERISIMDB_DATA="${VERISIMDB_DATA:-$REPOS_BASE/verisimdb-data}"
FLEET_SCRIPTS="${FLEET_SCRIPTS:-$FLEET_ROOT/scripts}"
RRA_BIN="${RRA_BIN:-$FLEET_ROOT/robot-repo-automaton/target/release/robot-repo-automaton}"

# Third-party subdirectories inside monorepos that must NOT be modified.
# Fix scripts will skip these paths entirely.
THIRD_PARTY_PATHS=(
    "developer-ecosystem/package-publishers/winget-pkgs"
    "developer-ecosystem/package-publishers/macports-ports"
    "echidna/HOL"
)

# Try hypatia's data first, then fall back to central verisimdb-data
if [[ -f "${HYPATIA_DATA}/dispatch/pending.jsonl" ]]; then
    MANIFEST_PATH="${HYPATIA_DATA}/dispatch/pending.jsonl"
elif [[ -f "${VERISIMDB_DATA}/dispatch/pending.jsonl" ]]; then
    MANIFEST_PATH="${VERISIMDB_DATA}/dispatch/pending.jsonl"
else
    MANIFEST_PATH="${HYPATIA_DATA}/dispatch/pending.jsonl"
fi

DRY_RUN=false
FILTER_TIER=""
FILTER_STRATEGY=""
FILTER_REPO=""
LIMIT=0
PARALLEL=1
DEDUP_REPO=false

# Counters
TOTAL=0
EXECUTED=0
SKIPPED=0
FAILED=0
SUCCEEDED=0

# --- Run counters ---
# Under --parallel, execute_entry runs in a background subshell, where a plain
# ((FAILED++)) is lost when the job exits: the summary and the heartbeat then
# read 0 failures over real ones. A job (IN_JOB=1) appends the counter's name
# to a tally file instead (one short O_APPEND write, atomic), and the parent
# folds the tally in after `wait`.
count() {
    if [[ "${IN_JOB:-0}" == 1 ]]; then
        printf '%s\n' "$1" >> "$PARALLEL_TMPDIR/tally"
    else
        # shellcheck disable=SC2004 # $1 names the counter; the $ is the indirection
        (( $1++ )) || true
    fi
}

fold_counts() {
    local name
    [[ -f "$PARALLEL_TMPDIR/tally" ]] || return 0
    while IFS= read -r name; do
        # shellcheck disable=SC2004 # $name names the counter; the $ is the indirection
        case "$name" in
            EXECUTED|SKIPPED|FAILED|SUCCEEDED) (( $name++ )) || true ;;
        esac
    done < "$PARALLEL_TMPDIR/tally"
}

# Background wait calls do not retain each child's status. Mark a failed
# identity/start/outcome contract explicitly so the parent also fails closed.
mark_dispatch_error() {
    count FAILED
    if [[ "${IN_JOB:-0}" == 1 ]]; then
        printf '%s\n' failed >> "$PARALLEL_TMPDIR/errors"
    fi
}
# --- end run counters ---

# --- Parse arguments ---
while [[ $# -gt 0 ]]; do
    case "$1" in
        --dry-run)    DRY_RUN=true; shift ;;
        --tier)       FILTER_TIER="$2"; shift 2 ;;
        --strategy)   FILTER_STRATEGY="$2"; shift 2 ;;
        --repo)       FILTER_REPO="$2"; shift 2 ;;
        --limit)      LIMIT="$2"; shift 2 ;;
        --manifest)   MANIFEST_PATH="$2"; shift 2 ;;
        --parallel)   PARALLEL="$2"; shift 2 ;;
        --dedup-repo) DEDUP_REPO=true; shift ;;
        -h|--help)
            head -18 "$0" | tail -16
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            exit 1
            ;;
    esac
done

# Temporary fail-closed quarantine. Even --dry-run writes outcome/heartbeat
# state below, so it is not an authorized policy-only inspection mode.
# No environment override: removal requires reviewed directive enforcement.
printf '%s\n' 'BLOCKED: repository directive enforcement is not qualified; dispatch is quarantined (including --dry-run). See docs/AUTOMATION-QUARANTINE.adoc.' >&2
exit 78

# --- Validate ---
if [[ ! -f "$MANIFEST_PATH" ]]; then
    echo "No manifest found at: $MANIFEST_PATH"
    echo "Run hypatia pipeline first: mix run -e 'Hypatia.PatternAnalyzer.analyze_all_scans()'"
    exit 1
fi

LINE_COUNT=$(wc -l < "$MANIFEST_PATH" | tr -d ' ')
echo "=== Dispatch Runner ==="
echo "  Manifest:  $MANIFEST_PATH ($LINE_COUNT entries)"
echo "  Repos:     $REPOS_BASE"
echo "  Dry run:   $DRY_RUN"
[[ -n "$FILTER_TIER" ]] && echo "  Tier:      $FILTER_TIER"
[[ -n "$FILTER_STRATEGY" ]] && echo "  Strategy:  $FILTER_STRATEGY"
[[ -n "$FILTER_REPO" ]] && echo "  Repo:      $FILTER_REPO"
[[ "$LIMIT" -gt 0 ]] && echo "  Limit:     $LIMIT"
echo ""

# --- Outcome recording ---
# Write outcomes to hypatia's local store (primary) and central store (backup)
OUTCOME_FILE="${HYPATIA_DATA}/outcomes/$(date -u +%Y-%m).jsonl"
OUTCOME_FILE_CENTRAL="${VERISIMDB_DATA}/outcomes/$(date -u +%Y-%m).jsonl"
mkdir -p "$(dirname "$OUTCOME_FILE")"
# E0 receipt design, section 5.2: starts are separate from every outcome log.
ATTEMPT_FILE="${FLEET_ROOT}/shared-context/learning/fix-attempts.jsonl"

# Repos that received at least one successful fix this run. Appended by
# record_outcome (file, not a variable: execute_entry may run in
# background subshells under --parallel) and consumed by the post-run
# estate-rescan trigger so verisimdb-data reflects the fixes without
# waiting for the nightly sweep.
RESCAN_REPOS_FILE="$(mktemp /tmp/dispatch-rescan-repos.XXXXXX)"

# Verdicts come from the recipe's predicate, not the fix script's exit code.
# shellcheck source=lib/outcome-verdict.sh
source "$(dirname "${BASH_SOURCE[0]}")/lib/outcome-verdict.sh"

# Parallel jobs on one repo take a per-repo lock around check/fix/check
# (repo_lock), so --parallel needs flock. Refuse rather than run unlocked.
if [[ "$PARALLEL" -gt 1 ]] && ! command -v flock &>/dev/null; then
    echo "ERROR: --parallel $PARALLEL needs flock (util-linux) to serialise fixes per repo" >&2
    exit 1
fi

# --- Hypatia closed-loop contract ---
#
# Per hypatia/docs/operations/dispatch-runner-contract.adoc, after each
# fix the runner MUST invoke `mix hypatia.record_outcome` so Bayesian
# confidence updating, auto-quarantine, and re-scan verification all see
# real evidence. Without this call, every recipe stays at
# :insufficient_data and the closed-loop verification metric (PR #309)
# never engages. Closes estate-audit blocker C15.
#
# Configuration via env:
#   HYPATIA_HOME             path to the hypatia checkout (defaults to
#                            $REPOS_BASE/hypatia)
#   HYPATIA_OUTCOME_REPORT   set to "off" to disable outcome reports;
#                            defaults to "on". Fix attempts still require
#                            mix and HYPATIA_HOME to mint their event id.
#   HYPATIA_BATCH_ID         when set, exit-2 hints suggest
#                            `mix hypatia.rollback_batch $HYPATIA_BATCH_ID`
#
# Exit codes from the mix task (consumed below):
#   0  outcome recorded; re-scan verified clean OR scan_unavailable
#   2  outcome recorded BUT re-scan still flags the weak point
#   1  bad arguments / unrecoverable error
HYPATIA_HOME="${HYPATIA_HOME:-${REPOS_BASE}/hypatia}"
HYPATIA_OUTCOME_REPORT="${HYPATIA_OUTCOME_REPORT:-on}"
HYPATIA_BATCH_ID="${HYPATIA_BATCH_ID:-}"

# Does this Hypatia know `--verdict`? Learned ONCE per run, from the help text of
# the task and never from an exit code: a Hypatia from before E0-01 ignores an
# option that it does not know, so a no_op call would be recorded as a success.
#   verdict  Hypatia names --verdict: every verdict is passed to it, and no_op and
#            unverified are recorded (they carry no credit).
#   legacy   an older Hypatia: no --verdict, and no call for no_op or unverified
#            (the behaviour before this check, exactly).
#   off      no outcome calls (reporting off, no checkout, no mix).
# A help command that fails is not an older Hypatia: the run stops, visibly.
HYPATIA_VERDICT_MODE="off"
hypatia_detect_verdict_mode() {
    HYPATIA_VERDICT_MODE="off"
    [[ "$HYPATIA_OUTCOME_REPORT" == "on" ]] || return 0
    [[ -d "$HYPATIA_HOME" ]] || return 0
    command -v mix &>/dev/null || return 0

    local help_text rc=0
    help_text=$(cd "$HYPATIA_HOME" && mix help hypatia.record_outcome 2>&1) || rc=$?
    if [[ "$rc" -ne 0 ]]; then
        HYPATIA_VERDICT_MODE="error"
        echo "ERROR: 'mix help hypatia.record_outcome' failed (exit $rc) in $HYPATIA_HOME: Hypatia does not answer, and a Hypatia that does not answer is not an older Hypatia. Stopping." >&2
        return 1
    fi
    if grep -q -- '--verdict' <<< "$help_text"; then
        HYPATIA_VERDICT_MODE="verdict"
    else
        HYPATIA_VERDICT_MODE="legacy"
    fi
    echo "hypatia outcome mode: $HYPATIA_VERDICT_MODE"
}

hypatia_record_outcome() {
    local recipe_id="$1"
    local repo="$2"
    local file="$3"
    local outcome="$4"
    local verdict="${5:-}"

    # Opt-out short-circuit.
    if [[ "$HYPATIA_OUTCOME_REPORT" != "on" ]]; then
        return 0
    fi
    # Precondition: hypatia checkout reachable and mix on PATH.
    if [[ ! -d "$HYPATIA_HOME" ]]; then
        echo "        WARN: hypatia outcome skipped — \$HYPATIA_HOME=$HYPATIA_HOME not a directory" >&2
        return 0
    fi
    if ! command -v mix &>/dev/null; then
        echo "        WARN: hypatia outcome skipped — mix not on PATH" >&2
        return 0
    fi
    # Empty / placeholder recipe_id → skip silently rather than poison
    # hypatia's recipe-health metric with anonymous entries.
    if [[ -z "$recipe_id" || "$recipe_id" == "none" || "$recipe_id" == "null" ]]; then
        return 0
    fi

    # Repo-grain fixes pass "." for --file (contract permits any
    # path-inside-repo string; "." is the explicit repo-root marker).
    local file_arg="${file:-.}"
    [[ -z "$file_arg" ]] && file_arg="."

    # The verdict goes along only when Hypatia knows the option, and only the
    # 5 verdicts of the pair table (a refusal has none).
    local verdict_args=()
    if [[ "$HYPATIA_VERDICT_MODE" == "verdict" && -n "$verdict" ]]; then
        verdict_args=(--verdict "$verdict")
    fi

    local rc=0
    (
        cd "$HYPATIA_HOME"
        mix hypatia.record_outcome \
            --recipe   "$recipe_id" \
            --repo     "$repo" \
            --file     "$file_arg" \
            --outcome  "$outcome" \
            "${verdict_args[@]}"
    ) || rc=$?

    case "$rc" in
        0)
            ;;
        2)
            echo "        ::warning::Recipe $recipe_id still flags $repo/$file_arg after fix" >&2
            if [[ -n "$HYPATIA_BATCH_ID" ]]; then
                echo "        ::warning::consider 'mix hypatia.rollback_batch $HYPATIA_BATCH_ID'" >&2
            fi
            ;;
        *)
            echo "        ::error::record_outcome failed for $repo/$file_arg: exit $rc" >&2
            ;;
    esac
    return "$rc"
}

# begin_attempt <recipe_id> <repo> -> event_id, only after its start is appended.
# event_id is ADR-008 version 8, profile T. E0-31 names Hypatia's existing Mix
# task as the producer tool; there is no fallback generator. E0-08 section 5.2
# fixes this six-field start schema and its separate append-only destination.
begin_attempt() {
    local recipe_id="$1" repo="$2" generated event_id start_json rc=0
    if [[ ! -d "$HYPATIA_HOME" ]]; then
        echo "ERROR: event id unavailable: HYPATIA_HOME is not a directory: $HYPATIA_HOME" >&2
        return 1
    fi
    if ! command -v mix &>/dev/null; then
        echo "ERROR: event id unavailable: mix is not on PATH" >&2
        return 1
    fi
    generated=$(cd "$HYPATIA_HOME" && mix hypatia.event_id) || rc=$?
    if [[ "$rc" -ne 0 ]]; then
        echo "ERROR: event id generation failed: mix hypatia.event_id exited $rc" >&2
        return 1
    fi
    # The task documents compilation messages before its final id line. Never
    # accept trailing output, a malformed id, or output from a failed command.
    event_id="${generated##*$'\n'}"
    if [[ ! "$event_id" =~ ^[0-9a-f]{8}-[0-9a-f]{4}-8[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$ ]]; then
        echo "ERROR: event id generation returned no valid ADR-008 v8 profile T id" >&2
        return 1
    fi
    if ! start_json=$(jq -cnS --arg event_id "$event_id" --arg recipe_id "$recipe_id" --arg repo "$repo" \
        '{chain_version: "e0-receipt/1", event_id: $event_id, kind: "start",
          producer: "dispatch-runner", recipe_id: $recipe_id, repo: $repo}'); then
        echo "ERROR: could not encode attempt start for event_id=$event_id" >&2
        return 1
    fi
    if ! mkdir -p "$(dirname "$ATTEMPT_FILE")" || ! printf '%s\n' "$start_json" >> "$ATTEMPT_FILE"; then
        echo "ERROR: could not append attempt start for event_id=$event_id to $ATTEMPT_FILE" >&2
        return 1
    fi
    printf '%s\n' "$event_id"
}

# record_outcome <pattern_id> <recipe_id> <repo> <file> <verdict> [exit before after event_id]
#
# <verdict> is one of lib/outcome-verdict.sh's verdicts, or refused_* for a
# policy refusal. `outcome` is kept for older readers and reports the exit
# status only; `verdict` says whether anything was fixed.
record_outcome() {
    local pattern_id="$1"
    local recipe_id="$2"
    local repo="$3"
    local file="$4"
    local verdict="$5"
    local exit_code="${6:-}"
    local before="${7:-NA}"
    local after="${8:-NA}"
    # Execution attempts pass the id already committed by begin_attempt.
    # Policy refusals do not execute a fix and keep their existing row shape.
    local event_id="${9:-}"
    local bot="dispatch-runner"

    local outcome
    case "$verdict" in
        refused_*) outcome="$verdict" ;;
        *) outcome=$(legacy_outcome "$verdict") ;;
    esac

    local ts
    ts=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    local json
    if ! json=$(jq -cn \
        --arg pid "$pattern_id" \
        --arg rid "$recipe_id" \
        --arg repo "$repo" \
        --arg file "$file" \
        --arg outcome "$outcome" \
        --arg verdict "$verdict" \
        --arg exit_code "$exit_code" \
        --arg before "$before" \
        --arg after "$after" \
        --arg ts "$ts" \
        --arg bot "$bot" \
        --arg event_id "$event_id" \
        '{pattern_id: $pid, recipe_id: $rid, repo: $repo, file: $file, outcome: $outcome,
          verdict: $verdict,
          exit_code: (if $exit_code == "" then null else ($exit_code | tonumber) end),
          before_hits: (if $before == "NA" then null else ($before | tonumber) end),
          after_hits: (if $after == "NA" then null else ($after | tonumber) end),
          timestamp: $ts, bot: $bot}
          + (if $event_id == "" then {} else {event_id: $event_id} end)'); then
        echo "ERROR: could not encode outcome for $repo/$recipe_id event_id=$event_id" >&2
        return 1
    fi

    if ! printf '%s\n' "$json" >> "$OUTCOME_FILE"; then
        echo "ERROR: could not append outcome for event_id=$event_id to $OUTCOME_FILE" >&2
        return 1
    fi
    # Avoid duplicate JSONL records when the two configured stores are the same.
    local primary_path central_path
    primary_path=$(realpath -m "$OUTCOME_FILE")
    central_path=$(realpath -m "$OUTCOME_FILE_CENTRAL")
    if [[ "$primary_path" != "$central_path" && ! "$OUTCOME_FILE" -ef "$OUTCOME_FILE_CENTRAL" ]]; then
        if ! mkdir -p "$(dirname "$OUTCOME_FILE_CENTRAL")" || ! printf '%s\n' "$json" >> "$OUTCOME_FILE_CENTRAL"; then
            echo "ERROR: could not mirror outcome for event_id=$event_id to $OUTCOME_FILE_CENTRAL; primary row is committed" >&2
            return 1
        fi
    fi

    # Hypatia closed-loop contract — best-effort, never fails the dispatch
    # loop. Map verdicts to the contract's outcome vocabulary:
    #   verified            → success
    #   failed / incomplete → failure (the fix ran but the work remains)
    #   refused_*           → false_positive (the recipe is misfiring, not
    #                         the code; surfaces this to auto-quarantine)
    #   no_op / unverified  → with a Hypatia that knows --verdict: recorded, as
    #                         success WITH the verdict, which Hypatia stores as
    #                         an audit row with no credit (E0-01). With an older
    #                         Hypatia: skipped, as before, because without the
    #                         verdict it would be recorded as a success.
    local hypatia_outcome verdict_arg=""
    case "$verdict" in
        verified)
            hypatia_outcome="success"
            verdict_arg="$verdict"
            echo "$repo" >> "$RESCAN_REPOS_FILE"
            ;;
        failed|incomplete)
            hypatia_outcome="failure"
            verdict_arg="$verdict"
            ;;
        refused_license_policy|refused_*)
            hypatia_outcome="false_positive"
            ;;
        no_op|unverified)
            [[ "$HYPATIA_VERDICT_MODE" == "verdict" ]] || return 0
            hypatia_outcome="success"
            verdict_arg="$verdict"
            ;;
        *)
            return 0
            ;;
    esac
    hypatia_record_outcome "$recipe_id" "$repo" "$file" "$hypatia_outcome" "$verdict_arg" || true
}

# Learn once per run which of the 2 calls Hypatia takes. Stop if it does not answer.
hypatia_detect_verdict_mode || exit 1

# --- Execute a single manifest entry ---
execute_entry() {
    local entry="$1"

    local tier strategy repo pattern_id recipe_id confidence auto_fixable fix_script program_path
    tier=$(echo "$entry" | jq -r '.tier')
    strategy=$(echo "$entry" | jq -r '.strategy')
    repo=$(echo "$entry" | jq -r '.repo')
    pattern_id=$(echo "$entry" | jq -r '.pattern_id')
    recipe_id=$(echo "$entry" | jq -r '.recipe_id // "none"')
    # Report under the canonical id: a deprecated alias must not earn credit
    # for the recipe it was renamed from.
    recipe_id=$(canonical_recipe_id "$FLEET_SCRIPTS/fix-script-registry.json" "$recipe_id")
    confidence=$(echo "$entry" | jq -r '.confidence // 0')
    auto_fixable=$(echo "$entry" | jq -r '.auto_fixable // false')
    fix_script=$(echo "$entry" | jq -r '.fix_script // "none"')
    program_path=$(echo "$entry" | jq -r '.program_path // ""')

    # Resolve repo path: prefer program_path from manifest, fall back to REPOS_BASE/repo
    # Never use "." as program_path — that would scan the entire working directory
    local repo_path=""
    if [[ -n "$program_path" && "$program_path" != "." && "$program_path" != "./" && -d "$program_path" ]]; then
        repo_path="$program_path"
    else
        # Validate repo name (prevent directory traversal)
        if ! validate_safe_name "$repo" "repo"; then
            echo "  SKIP: $pattern_id (unsafe repo name)"
            count SKIPPED
            return
        fi
        repo_path="$REPOS_BASE/$repo"

        # If repo not found, check path overrides (monorepo subdirectories)
        if [[ ! -d "$repo_path" ]]; then
            local overrides="$FLEET_SCRIPTS/repo-path-overrides.json"
            if [[ -f "$overrides" ]]; then
                local override
                override=$(jq -r --arg r "$repo" '.[$r] // empty' "$overrides" 2>/dev/null || true)
                if [[ -n "$override" && -d "$REPOS_BASE/$override" ]]; then
                    repo_path="$REPOS_BASE/$override"
                fi
            fi
        fi
    fi

    # Double-check path stays within REPOS_BASE
    if ! validate_path_within "$repo_path" "$REPOS_BASE"; then
        echo "  SKIP: $pattern_id (repo path escapes base)"
        count SKIPPED
        return
    fi

    # Skip third-party paths that must not be modified
    local rel_repo_path="${repo_path#"$REPOS_BASE"/}"
    for tp in "${THIRD_PARTY_PATHS[@]}"; do
        if [[ "$rel_repo_path" == "$tp" || "$rel_repo_path" == "$tp/"* ]]; then
            echo "  SKIP: $repo (third-party path: $tp)"
            count SKIPPED
            return
        fi
    done

    # --- License/SPDX auto-fix refusal gate (owner directive 2026-06-02) ---
    #
    # Estate policy forbids automated licence/SPDX edits. Licence remediation
    # is manual, file-by-file, owner-only (see canonical memory
    # `feedback_no_automated_licence_edits.md` and
    # `feedback_estate_license_policy_umbrella.md`).
    #
    # Triggered by neurophone#99 (auto-PR reverting PMPL → MPL-2.0 across
    # ~140 files, closed by owner). This gate refuses any auto_execute
    # dispatch whose recipe_id or category looks license/SPDX-related,
    # regardless of confidence score.
    #
    # Carve-out: the three palimpsest carve-out repos may still receive
    # licence-related work — but only with manual owner approval, NOT
    # auto-execute. So even those repos are gated here.
    local category lower_recipe lower_category
    category=$(echo "$entry" | jq -r '.category // ""')
    lower_recipe=$(echo "$recipe_id" | tr '[:upper:]' '[:lower:]')
    lower_category=$(echo "$category" | tr '[:upper:]' '[:lower:]')
    case "$lower_recipe$lower_category" in
        *license*|*spdx*|*pmpl*|*mpl-2*|*agpl*|*palimpsest*)
            echo "  REFUSED: $pattern_id → $repo (license/SPDX auto-edits forbidden per estate policy 2026-06-02)"
            echo "           recipe_id=$recipe_id category=$category"
            echo "           Owner must approve and apply licence changes manually, per-file."
            count SKIPPED
            record_outcome "$pattern_id" "$recipe_id" "$repo" "" "refused_license_policy"
            return
            ;;
    esac

    # Skip if repo doesn't exist locally
    if [[ ! -d "$repo_path" ]]; then
        echo "  SKIP: $repo (not found at $repo_path)"
        count SKIPPED
        return
    fi

    case "$strategy" in
        auto_execute)
            echo "  AUTO: [$tier] $pattern_id → $repo (confidence: $confidence)"

            if [[ "$DRY_RUN" == "true" ]]; then
                echo "        (dry-run) Would execute fix"
                return
            fi

            # Resolve fix_script from registry if not set in manifest
            if [[ "$fix_script" == "none" || "$fix_script" == "null" ]]; then
                local registry="$FLEET_SCRIPTS/fix-script-registry.json"
                if [[ -f "$registry" ]]; then
                    # Try recipe_id first, then category
                    fix_script=$(jq -r --arg rid "$recipe_id" '.registry.by_recipe[$rid] // empty' "$registry" 2>/dev/null || true)
                    if [[ -z "$fix_script" || "$fix_script" == "null" ]]; then
                        local category
                        category=$(echo "$entry" | jq -r '.category // ""')
                        fix_script=$(jq -r --arg cat "$category" '.registry.by_category[$cat] // empty' "$registry" 2>/dev/null || true)
                    fi
                    [[ -z "$fix_script" ]] && fix_script="none"
                fi
            fi

            # Try fix script first, then robot-repo-automaton
            # Validate fix_script: must not contain path separators or traversal
            if [[ "$fix_script" != "none" && "$fix_script" != "null" ]] && \
               validate_safe_name "$fix_script" "fix_script" && \
               [[ -x "$FLEET_SCRIPTS/$fix_script" ]]; then
                # Write finding JSON to temp file for fix script
                local tmp_finding
                tmp_finding=$(mktemp /tmp/dispatch-finding-XXXXXX.json)
                echo "$entry" > "$tmp_finding"

                local before after rc verdict event_id
                if [[ "$PARALLEL" -gt 1 ]]; then repo_lock "$PARALLEL_TMPDIR" "$repo_path"; fi
                before=$(recipe_check_count "$FLEET_SCRIPTS/$fix_script" "$repo_path")
                if ! event_id=$(begin_attempt "$recipe_id" "$repo"); then
                    if [[ "$PARALLEL" -gt 1 ]]; then repo_unlock; fi
                    rm -f "$tmp_finding"
                    mark_dispatch_error
                    return 1
                fi

                echo "        Running: $fix_script $repo_path $tmp_finding"
                rc=0
                "$FLEET_SCRIPTS/$fix_script" "$repo_path" "$tmp_finding" 2>&1 | sed 's/^/        /' \
                    || rc=${PIPESTATUS[0]}

                after=$(recipe_check_count "$FLEET_SCRIPTS/$fix_script" "$repo_path")
                if [[ "$PARALLEL" -gt 1 ]]; then repo_unlock; fi
                verdict=$(outcome_verdict "$rc" "$before" "$after")
                echo "        $verdict (exit $rc, targets $before -> $after)"
                record_outcome "$pattern_id" "$recipe_id" "$repo" "" "$verdict" "$rc" "$before" "$after" "$event_id" || {
                    rm -f "$tmp_finding"
                    count EXECUTED
                    mark_dispatch_error
                    return 1
                }
                if [[ "$verdict" == failed || "$verdict" == incomplete ]]; then
                    count FAILED
                else
                    count SUCCEEDED
                fi

                rm -f "$tmp_finding"

            elif command -v "$RRA_BIN" &>/dev/null; then
                # Use robot-repo-automaton fix command
                local event_id
                if ! event_id=$(begin_attempt "$recipe_id" "$repo"); then
                    mark_dispatch_error
                    return 1
                fi
                echo "        Running: $RRA_BIN fix --repo $repo_path --commit"
                # robot-repo-automaton exposes no predicate, so a clean exit
                # is recorded as unverified, never as a confirmed fix.
                local rc=0
                "$RRA_BIN" fix --repo "$repo_path" --commit 2>&1 | sed 's/^/        /' \
                    || rc=${PIPESTATUS[0]}
                local verdict
                verdict=$(outcome_verdict "$rc" NA NA)
                echo "        $verdict (exit $rc)"
                record_outcome "$pattern_id" "$recipe_id" "$repo" "" "$verdict" "$rc" NA NA "$event_id" || {
                    count EXECUTED
                    mark_dispatch_error
                    return 1
                }
                if [[ "$verdict" == failed ]]; then
                    count FAILED
                else
                    count SUCCEEDED
                fi
            else
                echo "        SKIP: No fix mechanism available (no fix_script, no $RRA_BIN)"
                count SKIPPED
            fi
            count EXECUTED
            ;;

        review)
            echo "  REVIEW: [$tier] $pattern_id → $repo (confidence: $confidence)"

            if [[ "$DRY_RUN" == "true" ]]; then
                echo "        (dry-run) Would write review finding"
                return
            fi

            # Write finding to shared-context for rhodibot pickup
            local findings_dir="$FLEET_ROOT/shared-context/findings/pending"
            mkdir -p "$findings_dir"

            # Sanitize pattern_id for use in filename (strip unsafe chars)
            local safe_pattern_id="${pattern_id//[^a-zA-Z0-9._-]/_}"
            local finding_file="$findings_dir/${repo}--${safe_pattern_id}.json"
            echo "$entry" | jq '. + {dispatch_strategy: "review", needs_pr: true}' > "$finding_file"
            echo "        Written: $finding_file"
            count EXECUTED
            ;;

        report_only)
            echo "  REPORT: [$tier] $pattern_id → $repo"

            if [[ "$DRY_RUN" == "true" ]]; then
                return
            fi

            # Append to sustainabot advisory log
            local advisory_dir="$FLEET_ROOT/shared-context/advisories"
            mkdir -p "$advisory_dir"

            local advisory_file="$advisory_dir/$(date -u +%Y-%m).jsonl"
            echo "$entry" | jq '. + {dispatch_strategy: "report_only"}' >> "$advisory_file"
            count EXECUTED
            ;;

        *)
            echo "  UNKNOWN strategy: $strategy"
            count SKIPPED
            ;;
    esac
}

# --- Deduplication ---
# When --dedup-repo is set, only process the first entry per repo+category combo.
# This avoids running the same fix script on the same repo multiple times.
declare -A SEEN_REPO_CAT=()

should_skip_dedup() {
    local line="$1"
    if [[ "$DEDUP_REPO" != "true" ]]; then
        return 1
    fi
    local repo category key
    repo=$(echo "$line" | jq -r '.repo')
    category=$(echo "$line" | jq -r '.category // "unknown"')
    key="${repo}::${category}"
    if [[ -n "${SEEN_REPO_CAT[$key]+x}" ]]; then
        return 0
    fi
    SEEN_REPO_CAT["$key"]=1
    return 1
}

# --- Main loop ---
LINE_NUM=0
ACTIVE_JOBS=0
PARALLEL_TMPDIR=$(mktemp -d /tmp/dispatch-parallel-XXXXXX)

# Parallel wrapper: run execute_entry in background, track jobs
run_entry() {
    local line="$1"
    if [[ "$PARALLEL" -le 1 ]]; then
        execute_entry "$line"
    else
        # Wait if we've hit the parallel limit
        while [[ "$ACTIVE_JOBS" -ge "$PARALLEL" ]]; do
            wait -n 2>/dev/null || true
            ((ACTIVE_JOBS--)) || true
        done
        IN_JOB=1 execute_entry "$line" &
        ((ACTIVE_JOBS++)) || true
    fi
}

while IFS= read -r line; do
    # Skip empty lines
    [[ -z "$line" ]] && continue

    # Validate JSON
    if ! echo "$line" | jq empty 2>/dev/null; then
        echo "  WARN: Invalid JSON on line $((LINE_NUM + 1)), skipping"
        ((SKIPPED++)) || true
        continue
    fi

    # Apply filters
    if [[ -n "$FILTER_TIER" ]]; then
        entry_tier=$(echo "$line" | jq -r '.tier')
        [[ "$entry_tier" != "$FILTER_TIER" ]] && continue
    fi

    if [[ -n "$FILTER_STRATEGY" ]]; then
        entry_strategy=$(echo "$line" | jq -r '.strategy')
        [[ "$entry_strategy" != "$FILTER_STRATEGY" ]] && continue
    fi

    if [[ -n "$FILTER_REPO" ]]; then
        entry_repo=$(echo "$line" | jq -r '.repo')
        [[ "$entry_repo" != "$FILTER_REPO" ]] && continue
    fi

    # Deduplicate by repo+category
    if should_skip_dedup "$line"; then
        continue
    fi

    ((TOTAL++)) || true

    # Check limit
    if [[ "$LIMIT" -gt 0 && "$TOTAL" -gt "$LIMIT" ]]; then
        echo "  Limit reached ($LIMIT entries)"
        break
    fi

    run_entry "$line"

done < "$MANIFEST_PATH"

# Wait for all background jobs to finish
if [[ "$PARALLEL" -gt 1 ]]; then
    wait
    fold_counts
    if [[ -s "$PARALLEL_TMPDIR/errors" ]]; then
        echo "ERROR: a dispatch job could not commit its attempt identity or outcome" >&2
        rm -rf "$PARALLEL_TMPDIR"
        exit 1
    fi
fi
rm -rf "$PARALLEL_TMPDIR" 2>/dev/null || true

# --- Summary ---
echo ""
echo "=== Summary ==="
echo "  Total processed: $TOTAL"
echo "  Executed:        $EXECUTED"
echo "  Succeeded:       $SUCCEEDED"
echo "  Failed:          $FAILED"
echo "  Skipped:         $SKIPPED"

if [[ "$SUCCEEDED" -gt 0 ]]; then
    echo ""
    echo "Outcomes recorded to: $OUTCOME_FILE"
fi

if [[ "$DRY_RUN" == "true" ]]; then
    echo ""
    echo "(Dry run — no changes were made)"
fi

# --- Trigger targeted estate rescan for fixed repos ---
# Fire a single repository_dispatch to hypatia's estate-rescan workflow
# listing every repo that received a successful fix this run. This is
# the loop-closure step: the rescan refreshes verisimdb-data/scans, the
# next pattern sync marks the fixed patterns resolved, and the numbers
# actually drop. Best-effort: a missing token or API failure must never
# fail the dispatch run itself.
RESCAN_TOKEN="${FLEET_DISPATCH_TOKEN:-${GITHUB_TOKEN:-}}"
if [[ "$DRY_RUN" != "true" && -s "$RESCAN_REPOS_FILE" && -n "$RESCAN_TOKEN" ]]; then
    RESCAN_REPOS=$(sort -u "$RESCAN_REPOS_FILE" | tr '\n' ' ' | sed 's/ $//')
    RESCAN_PAYLOAD=$(jq -n --arg repos "$RESCAN_REPOS" \
        '{event_type: "estate-rescan", client_payload: {repos: $repos}}')
    if curl -sf -X POST \
        -H "Authorization: Bearer ${RESCAN_TOKEN}" \
        -H "Accept: application/vnd.github+json" \
        "https://api.github.com/repos/hyperpolymath/hypatia/dispatches" \
        -d "$RESCAN_PAYLOAD" >/dev/null; then
        echo ""
        echo "Estate rescan requested for: $RESCAN_REPOS"
    else
        echo ""
        echo "WARN: estate-rescan dispatch failed (non-fatal); nightly sweep will catch up" >&2
    fi
elif [[ -s "$RESCAN_REPOS_FILE" && -z "$RESCAN_TOKEN" ]]; then
    echo ""
    echo "WARN: fixes landed but no FLEET_DISPATCH_TOKEN/GITHUB_TOKEN set — skipping targeted rescan (nightly sweep will catch up)" >&2
fi
rm -f "$RESCAN_REPOS_FILE" 2>/dev/null || true

# --- Kin Protocol: write heartbeat ---
KIN_DIR="${KIN_DIR:-${HOME}/.hypatia/kin}"
mkdir -p "$KIN_DIR"

HEARTBEAT_STATUS="healthy"
[[ "$FAILED" -gt 0 ]] && HEARTBEAT_STATUS="degraded"

cat > "${KIN_DIR}/gitbot-fleet.heartbeat.json" <<HEARTBEAT
{
  "kin_id": "gitbot-fleet",
  "role": "executor",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "status": "${HEARTBEAT_STATUS}",
  "version": "0.2.0",
  "last_run": {
    "total_processed": ${TOTAL},
    "executed": ${EXECUTED},
    "succeeded": ${SUCCEEDED},
    "failed": ${FAILED},
    "skipped": ${SKIPPED},
    "dry_run": ${DRY_RUN}
  },
  "errors": [],
  "capabilities": ["dispatch", "fix_execute", "pr_create", "review", "advisory"]
}
HEARTBEAT
