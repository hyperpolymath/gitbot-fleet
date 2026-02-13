#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
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

set -euo pipefail

# --- Configuration ---
VERISIMDB_DATA="${VERISIMDB_DATA:-/var/mnt/eclipse/repos/verisimdb-data}"
REPOS_BASE="${REPOS_BASE:-/var/mnt/eclipse/repos}"
FLEET_SCRIPTS="${FLEET_SCRIPTS:-/var/mnt/eclipse/repos/gitbot-fleet/scripts}"
RRA_BIN="${RRA_BIN:-/var/mnt/eclipse/repos/robot-repo-automaton/target/release/robot-repo-automaton}"
MANIFEST_PATH="${VERISIMDB_DATA}/dispatch/pending.jsonl"

DRY_RUN=false
FILTER_TIER=""
FILTER_STRATEGY=""
FILTER_REPO=""
LIMIT=0

# Counters
TOTAL=0
EXECUTED=0
SKIPPED=0
FAILED=0
SUCCEEDED=0

# --- Parse arguments ---
while [[ $# -gt 0 ]]; do
    case "$1" in
        --dry-run)    DRY_RUN=true; shift ;;
        --tier)       FILTER_TIER="$2"; shift 2 ;;
        --strategy)   FILTER_STRATEGY="$2"; shift 2 ;;
        --repo)       FILTER_REPO="$2"; shift 2 ;;
        --limit)      LIMIT="$2"; shift 2 ;;
        --manifest)   MANIFEST_PATH="$2"; shift 2 ;;
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
OUTCOME_FILE="${VERISIMDB_DATA}/outcomes/$(date -u +%Y-%m).jsonl"
mkdir -p "$(dirname "$OUTCOME_FILE")"

record_outcome() {
    local pattern_id="$1"
    local recipe_id="$2"
    local repo="$3"
    local file="$4"
    local outcome="$5"
    local bot="dispatch-runner"

    local ts
    ts=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    local json
    json=$(jq -n \
        --arg pid "$pattern_id" \
        --arg rid "$recipe_id" \
        --arg repo "$repo" \
        --arg file "$file" \
        --arg outcome "$outcome" \
        --arg ts "$ts" \
        --arg bot "$bot" \
        '{pattern_id: $pid, recipe_id: $rid, repo: $repo, file: $file, outcome: $outcome, timestamp: $ts, bot: $bot}')

    echo "$json" >> "$OUTCOME_FILE"
}

# --- Execute a single manifest entry ---
execute_entry() {
    local entry="$1"

    local tier strategy repo pattern_id recipe_id confidence auto_fixable fix_script
    tier=$(echo "$entry" | jq -r '.tier')
    strategy=$(echo "$entry" | jq -r '.strategy')
    repo=$(echo "$entry" | jq -r '.repo')
    pattern_id=$(echo "$entry" | jq -r '.pattern_id')
    recipe_id=$(echo "$entry" | jq -r '.recipe_id // "none"')
    confidence=$(echo "$entry" | jq -r '.confidence // 0')
    auto_fixable=$(echo "$entry" | jq -r '.auto_fixable // false')
    fix_script=$(echo "$entry" | jq -r '.fix_script // "none"')

    local repo_path="$REPOS_BASE/$repo"

    # Skip if repo doesn't exist locally
    if [[ ! -d "$repo_path" ]]; then
        echo "  SKIP: $repo (not found at $repo_path)"
        ((SKIPPED++)) || true
        return
    fi

    case "$strategy" in
        auto_execute)
            echo "  AUTO: [$tier] $pattern_id → $repo (confidence: $confidence)"

            if [[ "$DRY_RUN" == "true" ]]; then
                echo "        (dry-run) Would execute fix"
                return
            fi

            # Try fix script first, then robot-repo-automaton
            if [[ "$fix_script" != "none" && "$fix_script" != "null" && -x "$FLEET_SCRIPTS/$fix_script" ]]; then
                # Write finding JSON to temp file for fix script
                local tmp_finding
                tmp_finding=$(mktemp /tmp/dispatch-finding-XXXXXX.json)
                echo "$entry" > "$tmp_finding"

                echo "        Running: $fix_script $repo_path $tmp_finding"
                if "$FLEET_SCRIPTS/$fix_script" "$repo_path" "$tmp_finding" 2>&1 | sed 's/^/        /'; then
                    echo "        OK"
                    ((SUCCEEDED++)) || true
                    record_outcome "$pattern_id" "$recipe_id" "$repo" "" "success"
                else
                    echo "        FAILED (exit $?)"
                    ((FAILED++)) || true
                    record_outcome "$pattern_id" "$recipe_id" "$repo" "" "failure"
                fi

                rm -f "$tmp_finding"

            elif command -v "$RRA_BIN" &>/dev/null; then
                # Use robot-repo-automaton fix command
                echo "        Running: $RRA_BIN fix --repo $repo_path --commit"
                if "$RRA_BIN" fix --repo "$repo_path" --commit 2>&1 | sed 's/^/        /'; then
                    echo "        OK"
                    ((SUCCEEDED++)) || true
                    record_outcome "$pattern_id" "$recipe_id" "$repo" "" "success"
                else
                    echo "        FAILED (exit $?)"
                    ((FAILED++)) || true
                    record_outcome "$pattern_id" "$recipe_id" "$repo" "" "failure"
                fi
            else
                echo "        SKIP: No fix mechanism available (no fix_script, no $RRA_BIN)"
                ((SKIPPED++)) || true
            fi
            ((EXECUTED++)) || true
            ;;

        review)
            echo "  REVIEW: [$tier] $pattern_id → $repo (confidence: $confidence)"

            if [[ "$DRY_RUN" == "true" ]]; then
                echo "        (dry-run) Would write review finding"
                return
            fi

            # Write finding to shared-context for rhodibot pickup
            local findings_dir="$REPOS_BASE/gitbot-fleet/shared-context/findings/pending"
            mkdir -p "$findings_dir"

            local finding_file="$findings_dir/${repo}--${pattern_id}.json"
            echo "$entry" | jq '. + {dispatch_strategy: "review", needs_pr: true}' > "$finding_file"
            echo "        Written: $finding_file"
            ((EXECUTED++)) || true
            ;;

        report_only)
            echo "  REPORT: [$tier] $pattern_id → $repo"

            if [[ "$DRY_RUN" == "true" ]]; then
                return
            fi

            # Append to sustainabot advisory log
            local advisory_dir="$REPOS_BASE/gitbot-fleet/shared-context/advisories"
            mkdir -p "$advisory_dir"

            local advisory_file="$advisory_dir/$(date -u +%Y-%m).jsonl"
            echo "$entry" | jq '. + {dispatch_strategy: "report_only"}' >> "$advisory_file"
            ((EXECUTED++)) || true
            ;;

        *)
            echo "  UNKNOWN strategy: $strategy"
            ((SKIPPED++)) || true
            ;;
    esac
}

# --- Main loop ---
LINE_NUM=0

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

    ((TOTAL++)) || true

    # Check limit
    if [[ "$LIMIT" -gt 0 && "$TOTAL" -gt "$LIMIT" ]]; then
        echo "  Limit reached ($LIMIT entries)"
        break
    fi

    execute_entry "$line"

done < "$MANIFEST_PATH"

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
