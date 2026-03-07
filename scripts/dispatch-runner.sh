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
# Hypatia's local data store is the primary source for dispatch manifests.
# Falls back to central verisimdb-data if HYPATIA_DATA is not set.
HYPATIA_DATA="${HYPATIA_DATA:-/var/mnt/eclipse/repos/nextgen-databases/verisimdb/verisimdb-data}"
VERISIMDB_DATA="${VERISIMDB_DATA:-/var/mnt/eclipse/repos/nextgen-databases/verisimdb/verisimdb-data}"
REPOS_BASE="${REPOS_BASE:-/var/mnt/eclipse/repos}"
FLEET_SCRIPTS="${FLEET_SCRIPTS:-/var/mnt/eclipse/repos/gitbot-fleet/scripts}"
RRA_BIN="${RRA_BIN:-/var/mnt/eclipse/repos/gitbot-fleet/robot-repo-automaton/target/release/robot-repo-automaton}"

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
    # Also write to central store if it exists
    if [[ -d "$(dirname "$OUTCOME_FILE_CENTRAL")" ]] || mkdir -p "$(dirname "$OUTCOME_FILE_CENTRAL")" 2>/dev/null; then
        echo "$json" >> "$OUTCOME_FILE_CENTRAL"
    fi
}

# --- Execute a single manifest entry ---
execute_entry() {
    local entry="$1"

    local tier strategy repo pattern_id recipe_id confidence auto_fixable fix_script program_path
    tier=$(echo "$entry" | jq -r '.tier')
    strategy=$(echo "$entry" | jq -r '.strategy')
    repo=$(echo "$entry" | jq -r '.repo')
    pattern_id=$(echo "$entry" | jq -r '.pattern_id')
    recipe_id=$(echo "$entry" | jq -r '.recipe_id // "none"')
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
            ((SKIPPED++)) || true
            return
        fi
        repo_path="$REPOS_BASE/$repo"
    fi

    # Double-check path stays within REPOS_BASE
    if ! validate_path_within "$repo_path" "$REPOS_BASE"; then
        echo "  SKIP: $pattern_id (repo path escapes base)"
        ((SKIPPED++)) || true
        return
    fi

    # Skip third-party paths that must not be modified
    local rel_repo_path="${repo_path#"$REPOS_BASE"/}"
    for tp in "${THIRD_PARTY_PATHS[@]}"; do
        if [[ "$rel_repo_path" == "$tp" || "$rel_repo_path" == "$tp/"* ]]; then
            echo "  SKIP: $repo (third-party path: $tp)"
            ((SKIPPED++)) || true
            return
        fi
    done

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

            # Sanitize pattern_id for use in filename (strip unsafe chars)
            local safe_pattern_id="${pattern_id//[^a-zA-Z0-9._-]/_}"
            local finding_file="$findings_dir/${repo}--${safe_pattern_id}.json"
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
        execute_entry "$line" &
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

# --- Kin Protocol: write heartbeat ---
KIN_DIR="${HOME}/.hypatia/kin"
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
