#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Gitbot Fleet Coordinator - The missing execution layer
# This is what actually runs the bots and coordinates findings

set -euo pipefail

FLEET_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
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
    process-findings        Process pending findings and coordinate fixes
    status                  Show fleet status
    deploy-bots             Deploy all bots to their positions

BOT EXECUTION ORDER (from bot_integration.lgt):
    1. rhodibot    (Verifier - structure, policy, licensing)
    2. echidnabot  (Verifier - verification, fuzzing)
    3. sustainabot       (Verifier - sustainability)
    4. glambot     (Finisher - accessibility, SEO) [depends: rhodibot]
    5. seambot     (Finisher - integration) [depends: rhodibot, echidnabot]
    6. finishbot (Finisher - release, quality) [depends: rhodibot, glambot]
    7. robot-repo-automaton (Executor - fixes)
    8. hypatia (Engine - rule coordination)
EOF
}

run_hypatia_scan() {
    local repo_path="$1"
    local repo_name=$(basename "$repo_path")

    log_bot "hypatia" "Scanning $repo_name..."

    # Call hypatia scanner
    local findings_file="$FINDINGS_DIR/hypatia-${repo_name}-${SESSION_ID}.json"

    if [[ -x "$FLEET_DIR/../hypatia/hypatia-cli.sh" ]]; then
        HYPATIA_FORMAT=json "$FLEET_DIR/../hypatia/hypatia-cli.sh" scan "$repo_path" \
            > "$findings_file" 2>&1 || true
    else
        log_warn "Hypatia CLI not found, using POC scanner"
        "$FLEET_DIR/../hypatia/poc-scanner.sh" "$repo_path" > "$findings_file" 2>&1 || true
    fi

    if [[ -f "$findings_file" ]]; then
        local issue_count=$(jq 'length' "$findings_file" 2>/dev/null || echo 0)
        log_bot "hypatia" "Found $issue_count issues in $repo_name"
        echo "$findings_file"
    else
        log_error "Scan failed for $repo_name"
        return 1
    fi
}

process_findings() {
    log_info "Processing pending findings..."

    local findings_files=("$FINDINGS_DIR"/hypatia-*.json)

    if [[ ${#findings_files[@]} -eq 0 || ! -f "${findings_files[0]}" ]]; then
        log_info "No pending findings"
        return 0
    fi

    for findings_file in "${findings_files[@]}"; do
        [[ -f "$findings_file" ]] || continue

        local repo_name=$(basename "$findings_file" | sed 's/hypatia-\(.*\)-[0-9]*.json/\1/')
        log_info "Processing findings for: $repo_name"

        # Check severity distribution
        local critical=$(jq '[.[] | select(.severity == "critical")] | length' "$findings_file")
        local high=$(jq '[.[] | select(.severity == "high")] | length' "$findings_file")
        local medium=$(jq '[.[] | select(.severity == "medium")] | length' "$findings_file")

        log_info "  Critical: $critical, High: $high, Medium: $medium"

        # Delegate to robot-repo-automaton for auto-fixes
        if [[ $critical -gt 0 || $high -gt 0 ]]; then
            log_bot "robot-repo-automaton" "Delegating $((critical + high)) issues for auto-fix"
            trigger_auto_fix "$repo_name" "$findings_file"
        fi

        # Mark as processed
        mv "$findings_file" "${findings_file}.processed"
    done
}

trigger_auto_fix() {
    local repo_name="$1"
    local findings_file="$2"

    log_bot "robot-repo-automaton" "Auto-fixing issues in $repo_name"

    # Extract patterns and send to learning engine
    jq -r '.[] | select(.severity == "critical" or .severity == "high") |
        "\(.type)|\(.pattern)|\(.file)"' "$findings_file" | while IFS='|' read -r type pattern file; do

        log_bot "hypatia" "Learning: Observed pattern '$pattern' (type: $type)"

        # Record pattern observation (feeds learning_engine.lgt)
        echo "{\"type\":\"$type\",\"pattern\":\"$pattern\",\"observed\":\"$(date -Iseconds)\"}" \
            >> "$SHARED_CONTEXT/learning/observed-patterns.jsonl"

        # Check if we should generate new rule
        local pattern_count=$(grep -c "\"$pattern\"" "$SHARED_CONTEXT/learning/observed-patterns.jsonl" 2>/dev/null || echo 0)

        if [[ $pattern_count -ge 5 ]]; then
            log_bot "hypatia" "  → Pattern threshold reached ($pattern_count observations)!"
            log_bot "hypatia" "  → Proposing new rule for: $pattern"

            # Generate rule proposal
            propose_new_rule "$type" "$pattern" "$pattern_count"
        fi
    done

    # Apply known fixes
    jq -r '.[] | select(.severity == "critical" or .severity == "high") |
        "  Fix: \(.pattern) at \(.file):\(.line) - \(.fix)"' "$findings_file"
}

propose_new_rule() {
    local type="$1"
    local pattern="$2"
    local observations="$3"

    mkdir -p "$SHARED_CONTEXT/learning/rule-proposals"

    local proposal_file="$SHARED_CONTEXT/learning/rule-proposals/${pattern//[^a-zA-Z0-9]/_}.lgt"

    if [[ -f "$proposal_file" ]]; then
        log_bot "hypatia" "  Rule proposal already exists"
        return 0
    fi

    # Auto-generate Logtalk rule
    cat > "$proposal_file" << EOF
% SPDX-License-Identifier: PMPL-1.0-or-later
% Auto-generated rule proposal
% Pattern: $pattern
% Type: $type
% Observations: $observations
% Generated: $(date -Iseconds)

% TODO: Review and integrate into code-safety-lessons.lgt

has_${type}_issue(Path, ${pattern}_pattern(Line)) :-
    read_code_line(Path, LineNum, Line),
    atom_concat(_, '$pattern', Line),
    LineNum.

classify_severity(${pattern}_pattern(_), high).

suggest_fix(${pattern}_pattern(_),
    'Auto-generated fix suggestion - NEEDS REVIEW').
EOF

    log_bot "hypatia" "  ✓ Rule proposal created: $proposal_file"
    log_info "MANUAL ACTION REQUIRED: Review and approve rule proposal"
}

deploy_bots() {
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
    "glambot": {"status": "ready", "tier": "finisher"},
    "seambot": {"status": "ready", "tier": "finisher"},
    "finishbot": {"status": "ready", "tier": "finisher"},
    "robot-repo-automaton": {"status": "ready", "tier": "executor"},
    "hypatia": {"status": "ready", "tier": "engine"}
  }
}
EOF

    log_info "Deployment status: $deployment_file"
    jq . "$deployment_file"
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

        process-findings)
            process_findings
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
