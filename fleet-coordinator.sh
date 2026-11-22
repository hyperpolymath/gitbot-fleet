#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Gitbot Fleet Coordinator - The missing execution layer
# This is what actually runs the bots and coordinates findings

set -euo pipefail

FLEET_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPOS_BASE="${REPOS_BASE:-$HOME/developer/hyper-repos}"
SHARED_CONTEXT="$FLEET_DIR/shared-context"
FINDINGS_DIR="$SHARED_CONTEXT/findings"
SESSION_ID="$(date +%Y%m%d-%H%M%S)"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${BLUE}[FLEET]${NC} $*"; }
log_bot() { echo -e "${GREEN}[${1}]${NC} ${*:2}"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $*"; }

mkdir -p "$FINDINGS_DIR"

usage() {
    cat << EOF
Gitbot Fleet Coordinator v0.2.0

USAGE:
    fleet-coordinator.sh <command> [options]

COMMANDS:
    run-scan <repo>         Run full scan on repository
    scan-supervised         Run hypatia scan across supervised repos inventory
    process-findings        Process pending findings and coordinate fixes
    generate-rules          Retired: rule proposals are no longer generated (always blocked)
    status                  Show fleet status
    deploy-bots             Deploy all bots to their positions

BOT EXECUTION ORDER:
    1. rhodibot    (Verifier - structure, policy, licensing)
    2. echidnabot  (Verifier - verification, fuzzing)
    3. sustainabot       (Verifier - sustainability)
    4. panicbot    (Verifier - panic-attack auditing) [depends: rhodibot]
    5. glambot     (Finisher - accessibility, SEO) [depends: rhodibot]
    6. seambot     (Finisher - integration) [depends: rhodibot, echidnabot]
    7. finishbot (Finisher - release, quality) [depends: rhodibot, glambot]
    8. accessibilitybot (Finisher - WCAG compliance) [depends: rhodibot, glambot]
    9. cipherbot     (Specialist - crypto hygiene) [depends: rhodibot, echidnabot]
   10. robot-repo-automaton (Executor - fixes)
   11. hypatia (Engine - rule coordination)
EOF
}

run_hypatia_scan() {
    local repo_path="$1"
    local repo_name=$(basename "$repo_path")

    log_bot "hypatia" "Scanning $repo_name..."

    # Create findings directory for this repo
    local repo_findings_dir="$FINDINGS_DIR/$repo_name"
    mkdir -p "$repo_findings_dir"

    # Generate findings file with timestamp
    local findings_file="$repo_findings_dir/${SESSION_ID}.json"

    local scan_stderr
    scan_stderr=$(mktemp /tmp/hypatia-stderr-XXXXXX)
    local scan_exit=0

    if [[ -x "$FLEET_DIR/../hypatia/hypatia-cli.sh" ]]; then
        HYPATIA_FORMAT=json "$FLEET_DIR/../hypatia/hypatia-cli.sh" scan "$repo_path" \
            > "$findings_file" 2>"$scan_stderr" || scan_exit=$?
    elif [[ -x "/var/mnt/eclipse/repos/verification-ecosystem/hypatia/hypatia-cli.sh" ]]; then
        HYPATIA_FORMAT=json "/var/mnt/eclipse/repos/verification-ecosystem/hypatia/hypatia-cli.sh" scan "$repo_path" \
            > "$findings_file" 2>"$scan_stderr" || scan_exit=$?
    else
        log_warn "Hypatia CLI not found at expected paths"
        rm -f "$scan_stderr"
        return 1
    fi

    # Exit 2 = real error; exit 0 = clean; exit 1 = findings found (both valid)
    if [[ "$scan_exit" -eq 2 ]]; then
        local err_msg
        err_msg=$(head -3 "$scan_stderr" 2>/dev/null || echo "unknown error")
        log_error "Scanner error for $repo_name (exit 2): $err_msg"
        rm -f "$scan_stderr" "$findings_file"
        return 1
    fi
    rm -f "$scan_stderr"

    # Validate JSON output
    if [[ -f "$findings_file" ]] && [[ -s "$findings_file" ]] && jq empty "$findings_file" 2>/dev/null; then
        local issue_count
        issue_count=$(jq 'if type == "array" then length elif .findings then (.findings | length) else 0 end' "$findings_file" 2>/dev/null || echo 0)
        log_bot "hypatia" "Found $issue_count issues in $repo_name"

        # Create/update latest.json symlink
        ln -sf "$(basename "$findings_file")" "$repo_findings_dir/latest.json"

        echo "$findings_file"
    else
        log_error "Scan produced invalid/empty JSON for $repo_name"
        rm -f "$findings_file"
        return 1
    fi
}

scan_supervised_repos() {
    local process_after=false
    local limit=0
    local inventory=""
    local repos_root="$REPOS_BASE"

    while [[ $# -gt 0 ]]; do
        case "$1" in
            --process)
                process_after=true
                shift
                ;;
            --limit)
                limit="${2:-0}"
                shift 2
                ;;
            --inventory)
                inventory="${2:-}"
                shift 2
                ;;
            --repos-root)
                repos_root="${2:-}"
                shift 2
                ;;
            *)
                log_error "Unknown scan-supervised option: $1"
                return 2
                ;;
        esac
    done

    local list_script="$FLEET_DIR/scripts/list-supervised-repos.sh"
    if [[ ! -x "$list_script" ]]; then
        log_error "Missing inventory resolver: $list_script"
        return 1
    fi

    local -a args
    args=(--repos-root "$repos_root" --limit "$limit")
    if [[ -n "$inventory" ]]; then
        args+=(--inventory "$inventory")
    fi

    local -a repo_paths
    local resolved_repos
    if ! resolved_repos=$("$list_script" "${args[@]}"); then
        log_error "Failed to resolve supervised repository inventory"
        return 1
    fi
    if [[ -n "$resolved_repos" ]]; then
        mapfile -t repo_paths <<< "$resolved_repos"
    fi

    if [[ "${#repo_paths[@]}" -eq 0 ]]; then
        log_error "No supervised repositories resolved; refusing a false-green scan"
        return 1
    fi

    log_info "Scanning ${#repo_paths[@]} supervised repos"
    local scanned=0
    local failed=0

    for repo_path in "${repo_paths[@]}"; do
        if run_hypatia_scan "$repo_path" >/dev/null; then
            scanned=$((scanned + 1))
        else
            failed=$((failed + 1))
        fi
    done

    log_info "Supervised scan complete: scanned=$scanned failed=$failed"

    if [[ "$failed" -gt 0 ]]; then
        log_error "Supervised scan incomplete: $failed of ${#repo_paths[@]} repositories failed"
        return 1
    fi

    if [[ "$process_after" == true ]]; then
        process_findings
    fi
}

run_panicbot_scan() {
    local repo_path="$1"
    local repo_name
    repo_name=$(basename "$repo_path")

    log_bot "panicbot" "Running panic-attack audit on $repo_name..."

    if command -v panicbot &>/dev/null; then
        panicbot fleet "$repo_path" --mode advisor --format json
    elif command -v panic-attack &>/dev/null; then
        panic-attack assail "$repo_path" --output-format json
    else
        log_warn "Neither panicbot nor panic-attack found on PATH"
        return 1
    fi
}

process_findings() {
    # Do not generate false success evidence or acknowledge pending work.
    log_error "BLOCKED: repository directive enforcement is not qualified; findings processing is quarantined. See docs/AUTOMATION-QUARANTINE.adoc."
    return 78

    log_info "Processing pending findings..."

    # Load confidence thresholds
    local thresholds_file="$SHARED_CONTEXT/learning/confidence-thresholds.json"
    local auto_min=0.95
    local review_min=0.85
    local min_fixes=3

    if [[ -f "$thresholds_file" ]]; then
        auto_min=$(jq -r '.auto_execute.min_confidence // 0.95' "$thresholds_file")
        review_min=$(jq -r '.review_required.min_confidence // 0.85' "$thresholds_file")
        min_fixes=$(jq -r '.auto_execute.min_successful_fixes // 3' "$thresholds_file")
        log_info "Confidence thresholds: auto>=$auto_min, review>=$review_min, min_fixes=$min_fixes"
    fi

    # Load recipes for triangle routing
    local recipes_dir="$REPOS_BASE/verisimdb-data/recipes"
    local substitutions_file="$recipes_dir/proven-substitutions.json"

    # Scan for findings in new directory structure: findings/<repo-name>/<timestamp>.json
    local total_findings=0
    local total_auto_fixed=0
    local total_eliminate=0
    local total_substitute=0
    local total_control=0

    for repo_dir in "$FINDINGS_DIR"/*; do
        [[ -d "$repo_dir" ]] || continue

        local repo_slug=$(basename "$repo_dir")

        # Skip if not a repo directory (e.g., README.md, old files)
        [[ "$repo_slug" =~ ^[a-zA-Z0-9-]+$ ]] || continue

        # Process latest findings file (symlinked as latest.json)
        local latest_findings="$repo_dir/latest.json"

        if [[ -L "$latest_findings" && -f "$latest_findings" ]]; then
            local actual_file=$(readlink -f "$latest_findings")

            # Skip if already processed
            if [[ -f "${actual_file}.processed" ]]; then
                continue
            fi

            local repo_name=$(jq -r '.submission_metadata.repo // "unknown"' "$latest_findings")
            log_info "Processing findings for: $repo_name ($repo_slug)"

            # Extract findings array
            local findings=$(jq -r '.findings // []' "$latest_findings")
            local total=$(echo "$findings" | jq 'length')

            if [[ $total -eq 0 ]]; then
                log_info "  No findings in this submission"
                touch "${actual_file}.processed"
                continue
            fi

            total_findings=$((total_findings + total))

            # Check severity distribution
            local critical=$(echo "$findings" | jq '[.[] | select(.severity == "critical")] | length')
            local high=$(echo "$findings" | jq '[.[] | select(.severity == "high")] | length')
            local medium=$(echo "$findings" | jq '[.[] | select(.severity == "medium")] | length')
            local auto_fixable=$(echo "$findings" | jq '[.[] | select(.auto_fixable == true)] | length')

            log_info "  Total: $total (Critical: $critical, High: $high, Medium: $medium)"
            log_info "  Auto-fixable: $auto_fixable"

            # Triangle routing: classify each finding by tier
            if [[ -f "$substitutions_file" ]]; then
                local eliminate=$(echo "$findings" | jq --slurpfile subs "$substitutions_file" \
                    '[.[] | . as $f | ($subs[0].substitutions[] | select(.category == $f.type) | .triangle_tier) // "control" | select(. == "eliminate")] | length')
                local substitute=$(echo "$findings" | jq --slurpfile subs "$substitutions_file" \
                    '[.[] | . as $f | ($subs[0].substitutions[] | select(.category == $f.type) | .triangle_tier) // "control" | select(. == "substitute")] | length')
                local control=$((total - eliminate - substitute))

                total_eliminate=$((total_eliminate + eliminate))
                total_substitute=$((total_substitute + substitute))
                total_control=$((total_control + control))

                log_info "  Triangle: Eliminate=$eliminate, Substitute=$substitute, Control=$control"
            fi

            # Execute auto-fixes for high-confidence eliminate-tier items
            if [[ $auto_fixable -gt 0 ]]; then
                log_bot "robot-repo-automaton" "Executing $auto_fixable auto-fixes for $repo_name"
                local fixed_count=$(execute_auto_fixes "$repo_name" "$latest_findings")
                total_auto_fixed=$((total_auto_fixed + fixed_count))

                # execute_auto_fixes only *generates* fix-batch scripts; nothing
                # has run yet, so this is not a fix outcome. Record it as
                # generated/unverified rather than "success" (experiment E1).
                if [[ $fixed_count -gt 0 ]]; then
                    log_bot "hypatia" "Recording $fixed_count generated fix batches (unverified) to learning pipeline"
                    # Use jq to safely construct JSON (prevents injection via repo_name)
                    jq -cn --arg repo "$repo_name" --argjson count "$fixed_count" \
                        --arg fixed_at "$(date -Iseconds)" \
                        '{pattern: "auto-fix", repo: $repo, outcome: "generated", verdict: "unverified", count: $count, fixed_at: $fixed_at, bot: "robot-repo-automaton"}' \
                        >> "$SHARED_CONTEXT/learning/fix-outcomes.jsonl"
                fi
            fi

            # Trigger learning for all findings
            learn_from_findings "$repo_name" "$latest_findings"

            # Mark as processed
            touch "${actual_file}.processed"
        fi
    done

    if [[ $total_findings -eq 0 ]]; then
        log_info "No pending findings"
    else
        log_info "Summary: Processed $total_findings findings, auto-fixed $total_auto_fixed issues"
        log_info "Triangle: Eliminate=$total_eliminate, Substitute=$total_substitute, Control=$total_control"

        # Process substitute-tier findings via review path (rhodibot creates issues)
        if [[ $total_substitute -gt 0 ]]; then
            local review_script="$FLEET_DIR/scripts/process-review-findings.sh"
            if [[ -x "$review_script" ]]; then
                log_bot "rhodibot" "Processing $total_substitute substitute-tier findings for review"
                FLEET_BASE="${FLEET_ROOT:-$FLEET_DIR}" "$review_script" 2>&1 | sed 's/^/  /' || log_warn "Review processing had errors"
            else
                log_warn "Review processor not found at $review_script"
            fi
        fi

        # Process eliminate-tier findings via dispatch-runner (auto-execute)
        local dispatch_runner="$FLEET_DIR/scripts/dispatch-runner.sh"
        if [[ -x "$dispatch_runner" && -f "$REPOS_BASE/verisimdb-data/dispatch/pending.jsonl" ]]; then
            local auto_count
            auto_count=$(jq -c 'select(.strategy == "auto_execute")' "$REPOS_BASE/verisimdb-data/dispatch/pending.jsonl" 2>/dev/null | wc -l)
            if [[ $auto_count -gt 0 ]]; then
                log_bot "robot-repo-automaton" "Dispatch runner: $auto_count auto-execute entries pending"
            fi
        fi
    fi
}

execute_auto_fixes() {
    local repo_name="$1"
    local findings_file="$2"
    local fixed_count=0

    # Extract auto-fixable findings
    local auto_fixable=$(jq -c '.findings[] | select(.auto_fixable == true)' "$findings_file")

    if [[ -z "$auto_fixable" ]]; then
        return 0
    fi

    # Create fix batch for this repo
    local fix_batch_dir="$SHARED_CONTEXT/fix-batches/${repo_name}-${SESSION_ID}"
    mkdir -p "$fix_batch_dir"

    local finding_num=0
    echo "$auto_fixable" | while IFS= read -r finding; do
        ((finding_num++)) || true
        local finding_type=$(echo "$finding" | jq -r '.type')
        local fix_suggestion=$(echo "$finding" | jq -r '.fix_suggestion // empty')
        local finding_file=$(echo "$finding" | jq -r '.file')
        local finding_line=$(echo "$finding" | jq -r '.line')

        if [[ -z "$fix_suggestion" ]]; then
            log_warn "  No fix suggestion for finding #$finding_num ($finding_type)" >&2
            continue
        fi

        # Generate finding ID from type, file, and line
        local finding_id="${finding_type}-${finding_num}"

        # Save finding to temp file for fix script
        local finding_json="$fix_batch_dir/finding-${finding_id}.json"
        echo "$finding" > "$finding_json"

        # Create fix script for this finding
        local fix_script="$fix_batch_dir/${finding_id}.sh"

        cat > "$fix_script" << 'EOF'
#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
# Auto-generated fix script

set -euo pipefail

REPO_PATH="$1"
FINDING_FILE="$2"

EOF

        # Add finding-specific fix logic based on type
        case "$finding_type" in
            unpinned_action)
                echo "bash \"$FLEET_DIR/scripts/fix-unpinned-actions.sh\" \"\$REPO_PATH\" \"$finding_json\"" >> "$fix_script"
                ;;
            missing_permissions)
                echo "bash \"$FLEET_DIR/scripts/fix-workflow-permissions.sh\" \"\$REPO_PATH\" \"$finding_json\"" >> "$fix_script"
                ;;
            missing_spdx)
                echo "bash \"$FLEET_DIR/scripts/fix-missing-spdx.sh\" \"\$REPO_PATH\" \"$finding_json\"" >> "$fix_script"
                ;;
            cors_misconfiguration)
                echo "bash \"$FLEET_DIR/scripts/fix-cors-wildcard.sh\" \"\$REPO_PATH\" \"$finding_json\"" >> "$fix_script"
                ;;
            *)
                log_warn "  Unknown fix type: $finding_type" >&2
                continue
                ;;
        esac

        chmod +x "$fix_script"
        log_bot "robot-repo-automaton" "  Created fix script: $(basename "$fix_script")" >&2
        ((fixed_count++)) || true
    done

    # Count created scripts
    local script_count=$(find "$fix_batch_dir" -name "*.sh" -type f 2>/dev/null | wc -l)
    echo "$script_count"
}

learn_from_findings() {
    local repo_name="$1"
    local findings_file="$2"

    mkdir -p "$SHARED_CONTEXT/learning"

    # Extract patterns and send to learning engine
    jq -r '.findings[] | select(.severity == "critical" or .severity == "high") |
        "\(.type)|\(.location.file // "unknown")|\(.id)"' "$findings_file" | while IFS='|' read -r type file finding_id; do

        # Record pattern observation
        # Use jq to safely construct JSON (prevents injection via type/file/repo_name)
        jq -n --arg type "$type" --arg file "$file" --arg repo "$repo_name" \
            --arg observed "$(date -Iseconds)" \
            '{type: $type, file: $file, repo: $repo, observed: $observed}' \
            >> "$SHARED_CONTEXT/learning/observed-patterns.jsonl"
    done
}

deploy_bots() {
    log_error "BLOCKED: deployment readiness is not verified; deployment is quarantined. See docs/AUTOMATION-QUARANTINE.adoc."
    return 78

    log_info "Deploying gitbot-fleet bots..."

    # Create deployment status
    local deployment_file="$SHARED_CONTEXT/deployment-status.json"

    cat > "$deployment_file" << EOF
{
  "session_id": "$SESSION_ID",
  "deployed_at": "$(date -Iseconds)",
  "bots": {
    "rhodibot": {"status": "ready", "tier": "verifier"},
    "echidnabot": {"status": "ready", "tier": "verifier"},
    "sustainabot": {"status": "ready", "tier": "verifier"},
    "panicbot": {"status": "ready", "tier": "verifier"},
    "glambot": {"status": "ready", "tier": "finisher"},
    "seambot": {"status": "ready", "tier": "finisher"},
    "finishbot": {"status": "ready", "tier": "finisher"},
    "accessibilitybot": {"status": "ready", "tier": "finisher"},
    "cipherbot": {"status": "ready", "tier": "specialist"},
    "robot-repo-automaton": {"status": "ready", "tier": "executor"},
    "hypatia": {"status": "ready", "tier": "engine"}
  }
}
EOF

    log_info "Deployment status: $deployment_file"
    jq . "$deployment_file"
}

generate_rules() {
    # Rule generation and publication are retired; the command stays blocked.
    log_error "BLOCKED: repository directive enforcement is not qualified; rule generation/publication is quarantined. See docs/AUTOMATION-QUARANTINE.adoc."
    return 78
}

show_status() {
    log_info "Gitbot Fleet Status"

    if [[ -f "$SHARED_CONTEXT/deployment-status.json" ]]; then
        echo ""
        echo "Deployed bots:"
        jq -r '.bots | to_entries[] | "  \(.key): \(.value.status) (\(.value.tier))"' \
            "$SHARED_CONTEXT/deployment-status.json"
    else
        log_warn "Fleet not deployed yet (run: fleet-coordinator.sh deploy-bots)"
    fi

    echo ""
    echo "Pending findings:"
    local pending=$(find "$FINDINGS_DIR" -name "*.json" ! -name "*.processed" 2>/dev/null | wc -l)
    echo "  Unprocessed: $pending"

    local processed=$(find "$FINDINGS_DIR" -name "*.processed" 2>/dev/null | wc -l)
    echo "  Processed: $processed"
}

main() {
    local cmd="${1:-help}"

    case "$cmd" in
        run-scan)
            local repo="${2:-.}"
            run_hypatia_scan "$repo"
            ;;

        scan-supervised)
            shift
            scan_supervised_repos "$@"
            ;;

        process-findings)
            process_findings
            ;;

        generate-rules)
            generate_rules
            ;;

        deploy-bots)
            deploy_bots
            ;;

        status)
            show_status
            ;;

        help|--help|-h)
            usage
            ;;

        *)
            log_error "Unknown command: $cmd"
            usage
            exit 1
            ;;
    esac
}

main "$@"
