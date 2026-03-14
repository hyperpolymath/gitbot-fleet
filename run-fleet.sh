#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# run-fleet.sh — Single entry-point for the gitbot-fleet execution pipeline
#
# Orchestrates: hypatia scanning, findings aggregation, and auto-fix dispatch.
#
# Usage:
#   run-fleet.sh scan   [--limit N] [--repo REPO]   Scan repos via hypatia
#   run-fleet.sh report                              Summarise all pending findings
#   run-fleet.sh fix    [--apply] [--severity SEV]   Apply available auto-fixes
#   run-fleet.sh all    [--limit N]                  scan + report + fix (dry run)
#
# Environment:
#   FLEET_REPOS_BASE   Override repos directory (default: ~/Documents/hyperpolymath-repos)
#   HYPATIA_CLI        Override hypatia-cli.sh path

set -euo pipefail

# --- Paths ---
FLEET_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPOS_BASE="${FLEET_REPOS_BASE:-$HOME/Documents/hyperpolymath-repos}"
HYPATIA_CLI="${HYPATIA_CLI:-$FLEET_DIR/../hypatia/hypatia-cli.sh}"
SHARED_CONTEXT="$FLEET_DIR/shared-context"
FINDINGS_DIR="$SHARED_CONTEXT/findings"
LEARNING_DIR="$SHARED_CONTEXT/learning"
SCRIPTS_DIR="$FLEET_DIR/scripts"
HOOKS_DIR="$FLEET_DIR/hooks"
REGISTRY="$SCRIPTS_DIR/fix-script-registry.json"

# --- Colours (disabled when piped) ---
if [[ -t 1 ]]; then
    RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'
    BLUE='\033[0;34m'; CYAN='\033[0;36m'; BOLD='\033[1m'; NC='\033[0m'
else
    RED=''; GREEN=''; YELLOW=''; BLUE=''; CYAN=''; BOLD=''; NC=''
fi

log_info()  { echo -e "${BLUE}[FLEET]${NC} $*"; }
log_ok()    { echo -e "${GREEN}[  OK ]${NC} $*"; }
log_warn()  { echo -e "${YELLOW}[WARN ]${NC} $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $*"; }
log_head()  { echo -e "\n${BOLD}${CYAN}=== $* ===${NC}"; }

timestamp() { date -u +"%Y-%m-%dT%H:%M:%SZ"; }

# --- Pre-flight ---
preflight() {
    local ok=true
    if [[ ! -d "$REPOS_BASE" ]]; then
        log_error "Repos base not found: $REPOS_BASE"
        ok=false
    fi
    if [[ ! -x "$HYPATIA_CLI" ]]; then
        log_error "Hypatia CLI not found or not executable: $HYPATIA_CLI"
        ok=false
    fi
    if ! command -v jq &>/dev/null; then
        log_error "jq is required but not installed"
        ok=false
    fi
    if [[ "$ok" != "true" ]]; then
        exit 1
    fi
    mkdir -p "$FINDINGS_DIR" "$LEARNING_DIR"
}

# ─────────────────────────────────────────────────────────────────────
# SCAN — Run hypatia against repos
# ─────────────────────────────────────────────────────────────────────
cmd_scan() {
    local limit=0
    local filter_repo=""
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --limit)  limit="$2"; shift 2 ;;
            --repo)   filter_repo="$2"; shift 2 ;;
            *)        log_error "Unknown scan option: $1"; exit 1 ;;
        esac
    done

    preflight
    log_head "Scanning repositories"
    log_info "Repos base: $REPOS_BASE"
    log_info "Hypatia CLI: $HYPATIA_CLI"
    [[ "$limit" -gt 0 ]] && log_info "Limit: $limit repos"
    [[ -n "$filter_repo" ]] && log_info "Filter: $filter_repo"
    echo ""

    local scanned=0 total_findings=0 failed=0
    local count_critical=0 count_high=0 count_medium=0 count_low=0
    local scan_start
    scan_start=$(date +%s)

    for repo_path in "$REPOS_BASE"/*/; do
        [[ -d "$repo_path" ]] || continue
        local repo_name
        repo_name=$(basename "$repo_path")

        # Filter by name if requested
        if [[ -n "$filter_repo" && "$repo_name" != "$filter_repo" ]]; then
            continue
        fi

        # Limit check
        if [[ "$limit" -gt 0 && "$scanned" -ge "$limit" ]]; then
            log_info "Limit reached ($limit repos)"
            break
        fi

        # Create findings dir for this repo
        local repo_findings_dir="$FINDINGS_DIR/$repo_name"
        mkdir -p "$repo_findings_dir"

        local ts_file
        ts_file=$(date +%s)
        local findings_file="$repo_findings_dir/${ts_file}.json"

        # Run hypatia scan (JSON mode, suppress stderr progress)
        printf "  %-45s " "$repo_name"
        if HYPATIA_FORMAT=json "$HYPATIA_CLI" scan "$repo_path" > "$findings_file" 2>/dev/null; then
            : # exit 0 means clean
        else
            : # exit 1 means findings (expected)
        fi

        # Validate output is JSON
        if [[ -f "$findings_file" ]] && jq empty "$findings_file" 2>/dev/null; then
            # Update latest.json symlink
            ln -sf "$(basename "$findings_file")" "$repo_findings_dir/latest.json"

            # Extract counts
            local fc
            fc=$(jq '.submission_metadata.finding_count // (.findings | length) // 0' "$findings_file" 2>/dev/null || echo 0)
            local c h m l
            c=$(jq '[.findings[] | select(.severity == "critical")] | length' "$findings_file" 2>/dev/null || echo 0)
            h=$(jq '[.findings[] | select(.severity == "high")] | length' "$findings_file" 2>/dev/null || echo 0)
            m=$(jq '[.findings[] | select(.severity == "medium")] | length' "$findings_file" 2>/dev/null || echo 0)
            l=$(jq '[.findings[] | select(.severity == "low")] | length' "$findings_file" 2>/dev/null || echo 0)

            total_findings=$((total_findings + fc))
            count_critical=$((count_critical + c))
            count_high=$((count_high + h))
            count_medium=$((count_medium + m))
            count_low=$((count_low + l))

            if [[ "$fc" -eq 0 ]]; then
                echo -e "${GREEN}clean${NC}"
            else
                echo -e "${YELLOW}${fc} findings${NC} (C:$c H:$h M:$m L:$l)"
            fi
            ((scanned++)) || true
        else
            # Scan produced invalid/no output
            rm -f "$findings_file"
            echo -e "${RED}failed${NC}"
            ((failed++)) || true
        fi
    done

    local scan_end
    scan_end=$(date +%s)
    local elapsed=$((scan_end - scan_start))

    # Write scan timestamp
    jq -n --arg ts "$(timestamp)" --argjson repos "$scanned" --argjson findings "$total_findings" \
        --argjson failed "$failed" --argjson elapsed "$elapsed" \
        '{timestamp: $ts, repos_scanned: $repos, total_findings: $findings, failed: $failed, elapsed_seconds: $elapsed}' \
        > "$SHARED_CONTEXT/last-scan.json"

    # Summary
    log_head "Scan Summary"
    echo -e "  Repos scanned:  ${BOLD}$scanned${NC}"
    echo -e "  Scan failures:  $failed"
    echo -e "  Total findings: ${BOLD}$total_findings${NC}"
    echo -e "  Critical: ${RED}$count_critical${NC}  High: ${YELLOW}$count_high${NC}  Medium: $count_medium  Low: $count_low"
    echo -e "  Duration:       ${elapsed}s"
    echo ""

    # Run post-scan hook if it exists and there are critical findings
    if [[ -x "$HOOKS_DIR/post-scan.sh" ]]; then
        log_info "Running post-scan hook..."
        "$HOOKS_DIR/post-scan.sh" "$count_critical" "$count_high" "$scanned" || true
    fi
}

# ─────────────────────────────────────────────────────────────────────
# REPORT — Aggregate and summarise all pending findings
# ─────────────────────────────────────────────────────────────────────
cmd_report() {
    preflight
    log_head "Findings Report"

    local total=0 repos_with_findings=0
    local count_critical=0 count_high=0 count_medium=0 count_low=0

    # Temp files for aggregation
    local tmp_repo_counts
    tmp_repo_counts=$(mktemp /tmp/fleet-report-XXXXXX)
    trap "rm -f '$tmp_repo_counts'" RETURN

    for repo_dir in "$FINDINGS_DIR"/*/; do
        [[ -d "$repo_dir" ]] || continue
        local latest="$repo_dir/latest.json"
        [[ -f "$latest" ]] || continue

        local repo_name
        repo_name=$(basename "$repo_dir")

        # Read findings
        local fc c h m l
        fc=$(jq '.findings | length' "$latest" 2>/dev/null || echo 0)
        [[ "$fc" -eq 0 ]] && continue

        c=$(jq '[.findings[] | select(.severity == "critical")] | length' "$latest" 2>/dev/null || echo 0)
        h=$(jq '[.findings[] | select(.severity == "high")] | length' "$latest" 2>/dev/null || echo 0)
        m=$(jq '[.findings[] | select(.severity == "medium")] | length' "$latest" 2>/dev/null || echo 0)
        l=$(jq '[.findings[] | select(.severity == "low")] | length' "$latest" 2>/dev/null || echo 0)

        total=$((total + fc))
        count_critical=$((count_critical + c))
        count_high=$((count_high + h))
        count_medium=$((count_medium + m))
        count_low=$((count_low + l))
        ((repos_with_findings++)) || true

        echo "$fc $repo_name" >> "$tmp_repo_counts"
    done

    echo -e "  Repos with findings: ${BOLD}$repos_with_findings${NC}"
    echo -e "  Total findings:      ${BOLD}$total${NC}"
    echo ""
    echo -e "  ${RED}Critical: $count_critical${NC}"
    echo -e "  ${YELLOW}High:     $count_high${NC}"
    echo -e "  Medium:   $count_medium"
    echo -e "  Low:      $count_low"
    echo ""

    # Top 10 most affected repos
    if [[ -s "$tmp_repo_counts" ]]; then
        echo -e "  ${BOLD}Top 10 Most-Affected Repos:${NC}"
        sort -rn "$tmp_repo_counts" | head -10 | while read -r cnt name; do
            printf "    %4d  %s\n" "$cnt" "$name"
        done
        echo ""
    fi

    # Show most common finding types across all repos
    echo -e "  ${BOLD}Most Common Finding Types:${NC}"
    local tmp_types
    tmp_types=$(mktemp /tmp/fleet-types-XXXXXX)
    for repo_dir in "$FINDINGS_DIR"/*/; do
        [[ -d "$repo_dir" ]] || continue
        local latest="$repo_dir/latest.json"
        [[ -f "$latest" ]] || continue
        jq -r '.findings[].pattern // .findings[].type' "$latest" 2>/dev/null >> "$tmp_types" || true
    done
    if [[ -s "$tmp_types" ]]; then
        sort "$tmp_types" | uniq -c | sort -rn | head -10 | while read -r cnt typ; do
            printf "    %4d  %s\n" "$cnt" "$typ"
        done
    fi
    rm -f "$tmp_types"
    echo ""
}

# ─────────────────────────────────────────────────────────────────────
# FIX — Apply auto-fixes from fix-script-registry
# ─────────────────────────────────────────────────────────────────────
cmd_fix() {
    local dry_run=true
    local severity_filter=""
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --apply)     dry_run=false; shift ;;
            --severity)  severity_filter="$2"; shift 2 ;;
            *)           log_error "Unknown fix option: $1"; exit 1 ;;
        esac
    done

    preflight
    log_head "Auto-Fix Pipeline"

    if [[ "$dry_run" == "true" ]]; then
        log_info "DRY RUN mode (use --apply to execute changes)"
    else
        log_warn "LIVE MODE -- changes will be applied to repos"
    fi
    echo ""

    if [[ ! -f "$REGISTRY" ]]; then
        log_error "Fix script registry not found: $REGISTRY"
        exit 1
    fi

    local total_candidates=0 total_matched=0 total_applied=0 total_skipped=0 total_failed=0

    for repo_dir in "$FINDINGS_DIR"/*/; do
        [[ -d "$repo_dir" ]] || continue
        local latest="$repo_dir/latest.json"
        [[ -f "$latest" ]] || continue

        local repo_name
        repo_name=$(basename "$repo_dir")
        local repo_path="$REPOS_BASE/$repo_name"

        # Skip repos that don't exist locally
        [[ -d "$repo_path" ]] || continue

        # Extract auto-fixable findings
        local filter_expr='.findings[] | select(.auto_fixable == true)'
        if [[ -n "$severity_filter" ]]; then
            filter_expr=".findings[] | select(.auto_fixable == true and .severity == \"$severity_filter\")"
        fi

        local auto_findings
        auto_findings=$(jq -c "$filter_expr" "$latest" 2>/dev/null || true)
        [[ -z "$auto_findings" ]] && continue

        local repo_count=0
        while IFS= read -r finding; do
            [[ -z "$finding" ]] && continue
            ((total_candidates++)) || true

            local ftype fpattern
            ftype=$(echo "$finding" | jq -r '.type // ""')
            fpattern=$(echo "$finding" | jq -r '.pattern // ""')
            local fseverity
            fseverity=$(echo "$finding" | jq -r '.severity // "unknown"')

            # Look up fix script via multiple strategies:
            #   1. Pattern name directly in by_recipe
            #   2. Type directly in by_category
            #   3. Known pattern-to-registry mappings (hypatia patterns -> registry keys)
            local fix_script=""

            # Strategy 1: pattern name in by_recipe
            if [[ -n "$fpattern" ]]; then
                fix_script=$(jq -r --arg p "$fpattern" '.registry.by_recipe[$p] // empty' "$REGISTRY" 2>/dev/null || true)
            fi

            # Strategy 2: type in by_category
            if [[ -z "$fix_script" || "$fix_script" == "null" ]] && [[ -n "$ftype" ]]; then
                fix_script=$(jq -r --arg t "$ftype" '.registry.by_category[$t] // empty' "$REGISTRY" 2>/dev/null || true)
            fi

            # Strategy 3: map hypatia pattern names to registry keys
            if [[ -z "$fix_script" || "$fix_script" == "null" ]]; then
                local registry_key=""
                case "$fpattern" in
                    unpinned_action)            registry_key="DependencyPinning" ;;
                    missing_permissions)         registry_key="TokenPermissions" ;;
                    missing_security_policy)     registry_key="SecurityPolicy" ;;
                    missing_license_file)        registry_key="LicenseCompliance" ;;
                    wildcard_cors)               registry_key="CORSWildcard" ;;
                    missing_spdx*)               registry_key="MissingSPDX" ;;
                    innerhtml_assignment)         registry_key="XSS" ;;
                    rust_unwrap|unwrap_without_check) registry_key="PanicPath" ;;
                    actions_expression_injection) registry_key="CommandInjection" ;;
                esac
                if [[ -n "$registry_key" ]]; then
                    fix_script=$(jq -r --arg k "$registry_key" '.registry.by_category[$k] // empty' "$REGISTRY" 2>/dev/null || true)
                fi
            fi

            # Strategy 4: try recipe-style names
            if [[ -z "$fix_script" || "$fix_script" == "null" ]] && [[ -n "$fpattern" ]]; then
                fix_script=$(jq -r --arg p "recipe-$fpattern" '.registry.by_recipe[$p] // empty' "$REGISTRY" 2>/dev/null || true)
            fi

            if [[ -z "$fix_script" || "$fix_script" == "null" ]]; then
                ((total_skipped++)) || true
                continue
            fi

            # Validate fix script exists and is safe
            if [[ "$fix_script" == *"/"* || "$fix_script" == *".."* ]]; then
                log_warn "  Unsafe fix script name: $fix_script (skipping)"
                ((total_skipped++)) || true
                continue
            fi

            local fix_path="$SCRIPTS_DIR/$fix_script"
            if [[ ! -x "$fix_path" ]]; then
                ((total_skipped++)) || true
                continue
            fi

            ((total_matched++)) || true
            ((repo_count++)) || true

            if [[ "$dry_run" == "true" ]]; then
                if [[ "$repo_count" -eq 1 ]]; then
                    echo -e "  ${BOLD}$repo_name${NC}:"
                fi
                echo -e "    [DRY] [$fseverity] $fpattern -> $fix_script"
            else
                if [[ "$repo_count" -eq 1 ]]; then
                    echo -e "  ${BOLD}$repo_name${NC}:"
                fi
                # Write finding to temp file for fix script
                local tmp_finding
                tmp_finding=$(mktemp /tmp/fleet-fix-XXXXXX.json)
                echo "$finding" > "$tmp_finding"

                printf "    [FIX] %-40s " "$fpattern"
                if "$fix_path" "$repo_path" "$tmp_finding" >/dev/null 2>&1; then
                    echo -e "${GREEN}OK${NC}"
                    ((total_applied++)) || true

                    # Log outcome
                    jq -n --arg repo "$repo_name" --arg pattern "$fpattern" \
                        --arg script "$fix_script" --arg ts "$(timestamp)" \
                        --arg severity "$fseverity" \
                        '{timestamp: $ts, repo: $repo, pattern: $pattern, fix_script: $script, severity: $severity, outcome: "success"}' \
                        >> "$LEARNING_DIR/fix-outcomes.jsonl"
                else
                    echo -e "${RED}FAILED${NC}"
                    ((total_failed++)) || true

                    jq -n --arg repo "$repo_name" --arg pattern "$fpattern" \
                        --arg script "$fix_script" --arg ts "$(timestamp)" \
                        --arg severity "$fseverity" \
                        '{timestamp: $ts, repo: $repo, pattern: $pattern, fix_script: $script, severity: $severity, outcome: "failure"}' \
                        >> "$LEARNING_DIR/fix-outcomes.jsonl"
                fi

                rm -f "$tmp_finding"
            fi
        done <<< "$auto_findings"
    done

    # Summary
    log_head "Fix Summary"
    echo -e "  Auto-fixable candidates: $total_candidates"
    echo -e "  Matched to fix scripts:  $total_matched"
    if [[ "$dry_run" == "true" ]]; then
        echo -e "  Mode: DRY RUN (no changes made)"
        echo -e "  Run with --apply to execute ${total_matched} fixes"
    else
        echo -e "  Applied:  ${GREEN}$total_applied${NC}"
        echo -e "  Failed:   ${RED}$total_failed${NC}"
        echo -e "  Skipped:  $total_skipped"
        echo -e "  Outcomes: $LEARNING_DIR/fix-outcomes.jsonl"
    fi
    echo ""
}

# ─────────────────────────────────────────────────────────────────────
# ALL — scan + report + fix (dry run)
# ─────────────────────────────────────────────────────────────────────
cmd_all() {
    local scan_args=()
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --limit) scan_args+=(--limit "$2"); shift 2 ;;
            *)       scan_args+=("$1"); shift ;;
        esac
    done

    cmd_scan "${scan_args[@]}"
    cmd_report
    cmd_fix
}

# ─────────────────────────────────────────────────────────────────────
# MAIN
# ─────────────────────────────────────────────────────────────────────
usage() {
    cat << EOF
Gitbot Fleet Runner v1.0.0

USAGE:
    run-fleet.sh <command> [options]

COMMANDS:
    scan    [--limit N] [--repo REPO]    Scan repos via hypatia-cli
    report                               Summarise all pending findings
    fix     [--apply] [--severity SEV]   Apply auto-fixes (dry run by default)
    all     [--limit N]                  scan + report + fix (dry run)

EXAMPLES:
    run-fleet.sh scan --limit 10         Scan first 10 repos
    run-fleet.sh report                  Show findings summary
    run-fleet.sh fix                     Dry-run fix preview
    run-fleet.sh fix --apply             Actually apply fixes
    run-fleet.sh fix --severity critical Fix only critical issues
    run-fleet.sh all --limit 50          Full pipeline (50 repos, dry-run fix)
EOF
}

cmd="${1:-help}"
shift || true

case "$cmd" in
    scan)    cmd_scan "$@" ;;
    report)  cmd_report "$@" ;;
    fix)     cmd_fix "$@" ;;
    all)     cmd_all "$@" ;;
    help|-h|--help) usage ;;
    *)
        log_error "Unknown command: $cmd"
        usage
        exit 1
        ;;
esac
