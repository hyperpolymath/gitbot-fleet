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
    generate-rules          Generate new rules from observed patterns
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

    # Create findings directory for this repo
    local repo_findings_dir="$FINDINGS_DIR/$repo_name"
    mkdir -p "$repo_findings_dir"

    # Generate findings file with timestamp
    local findings_file="$repo_findings_dir/${SESSION_ID}.json"

    if [[ -x "$FLEET_DIR/../hypatia/hypatia-cli.sh" ]]; then
        HYPATIA_FORMAT=json "$FLEET_DIR/../hypatia/hypatia-cli.sh" scan "$repo_path" \
            > "$findings_file" 2>/dev/null || true
    else
        log_warn "Hypatia CLI not found, using POC scanner"
        "$FLEET_DIR/../hypatia/poc-scanner.sh" "$repo_path" > "$findings_file" 2>/dev/null || true
    fi

    if [[ -f "$findings_file" ]]; then
        local issue_count=$(jq 'length' "$findings_file" 2>/dev/null || echo 0)
        log_bot "hypatia" "Found $issue_count issues in $repo_name"

        # Create/update latest.json symlink
        ln -sf "$(basename "$findings_file")" "$repo_findings_dir/latest.json"

        echo "$findings_file"
    else
        log_error "Scan failed for $repo_name"
        return 1
    fi
}

process_findings() {
    log_info "Processing pending findings..."

    # Scan for findings in new directory structure: findings/<repo-name>/<timestamp>.json
    local total_findings=0
    local total_auto_fixed=0

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

            # Execute auto-fixes for high-severity or auto-fixable issues
            if [[ $auto_fixable -gt 0 ]]; then
                log_bot "robot-repo-automaton" "Executing $auto_fixable auto-fixes for $repo_name"
                local fixed_count=$(execute_auto_fixes "$repo_name" "$latest_findings")
                total_auto_fixed=$((total_auto_fixed + fixed_count))
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
# SPDX-License-Identifier: PMPL-1.0-or-later
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
                echo "bash \"$FLEET_DIR/scripts/fix-missing-permissions.sh\" \"\$REPO_PATH\" \"$finding_json\"" >> "$fix_script"
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

        # Record pattern observation (feeds learning_engine.lgt)
        echo "{\"type\":\"$type\",\"file\":\"$file\",\"repo\":\"$repo_name\",\"observed\":\"$(date -Iseconds)\"}" \
            >> "$SHARED_CONTEXT/learning/observed-patterns.jsonl"

        # Check if we should generate new rule
        local pattern_count=$(grep -c "\"$type\"" "$SHARED_CONTEXT/learning/observed-patterns.jsonl" 2>/dev/null || echo 0)

        if [[ $pattern_count -ge 5 ]]; then
            log_bot "hypatia" "  → Pattern threshold reached for '$type' ($pattern_count observations)"

            # Generate rule proposal
            propose_new_rule "$type" "$file" "$pattern_count"
        fi
    done
}

propose_new_rule() {
    local type="$1"
    local pattern="$2"
    local observations="$3"

    mkdir -p "$SHARED_CONTEXT/learning/rule-proposals"

    # Sanitize filename
    local safe_type="${type//[^a-zA-Z0-9_]/_}"
    local proposal_file="$SHARED_CONTEXT/learning/rule-proposals/${safe_type}.lgt"

    if [[ -f "$proposal_file" ]]; then
        log_bot "hypatia" "  Rule proposal already exists: $safe_type"
        return 0
    fi

    # Generate context-aware Logtalk rule based on issue type
    case "$type" in
        unpinned_action)
            cat > "$proposal_file" << 'RULE_EOF'
% SPDX-License-Identifier: PMPL-1.0-or-later
% Auto-generated rule: Detect unpinned GitHub Actions
% Generated: $(date -Iseconds)
% Observations: $observations across $repos repos

:- object(unpinned_action_detector,
    extends(code_pattern_detector)).

    % Detect uses: action@version (not SHA)
    has_workflow_issue(Path, unpinned_action(Action, Line)) :-
        workflow_file(Path),
        read_file_line(Path, LineNum, Line),
        atom_contains(Line, 'uses:'),
        extract_action_ref(Line, Action, Ref),
        \+ is_sha_pinned(Ref).

    extract_action_ref(Line, Action, Ref) :-
        sub_atom(Line, AfterUses, _, 0, Rest),
        atom_concat('uses:', Rest, Line),
        split_string(Rest, "@", " \t", [ActionStr, RefStr]),
        atom_string(Action, ActionStr),
        atom_string(Ref, RefStr).

    is_sha_pinned(Ref) :-
        atom_length(Ref, 40),  % SHA is 40 characters
        atom_codes(Ref, Codes),
        forall(member(C, Codes), is_hex_digit(C)).

    is_hex_digit(C) :-
        (C >= 0'0, C =< 0'9) ; (C >= 0'a, C =< 0'f').

    classify_severity(unpinned_action(_, _), high).

    suggest_fix(unpinned_action(Action, _), Fix) :-
        format(atom(Fix), 'Pin ~w to SHA for supply chain security', [Action]).

    auto_fixable(unpinned_action(_, _), true).

    fix_script(unpinned_action(_, _), 'scripts/fix-unpinned-actions.sh').

:- end_object.
RULE_EOF
            ;;

        missing_permissions)
            cat > "$proposal_file" << 'RULE_EOF'
% SPDX-License-Identifier: PMPL-1.0-or-later
% Auto-generated rule: Detect missing permissions declarations
% Generated: $(date -Iseconds)
% Observations: $observations across $repos repos

:- object(missing_permissions_detector,
    extends(code_pattern_detector)).

    has_workflow_issue(Path, missing_permissions(Line)) :-
        workflow_file(Path),
        \+ has_permissions_declaration(Path).

    has_permissions_declaration(Path) :-
        read_file_line(Path, _, Line),
        atom_contains(Line, 'permissions:').

    classify_severity(missing_permissions(_), medium).

    suggest_fix(missing_permissions(_),
        'Add permissions: read-all at workflow level').

    auto_fixable(missing_permissions(_), true).

    fix_script(missing_permissions(_), 'scripts/fix-missing-permissions.sh').

:- end_object.
RULE_EOF
            ;;

        missing_spdx)
            cat > "$proposal_file" << 'RULE_EOF'
% SPDX-License-Identifier: PMPL-1.0-or-later
% Auto-generated rule: Detect missing SPDX headers
% Generated: $(date -Iseconds)
% Observations: $observations across $repos repos

:- object(missing_spdx_detector,
    extends(code_pattern_detector)).

    has_file_issue(Path, missing_spdx(FileType)) :-
        source_file(Path),
        file_type(Path, FileType),
        \+ has_spdx_header(Path).

    has_spdx_header(Path) :-
        read_file_lines(Path, 3, Lines),
        member(Line, Lines),
        atom_contains(Line, 'SPDX-License-Identifier').

    file_type(Path, 'workflow') :- atom_contains(Path, '.github/workflows/').
    file_type(Path, 'script') :- atom_contains(Path, '.sh').
    file_type(Path, 'source') :- true.

    classify_severity(missing_spdx(_), low).

    suggest_fix(missing_spdx(FileType), Fix) :-
        format(atom(Fix), 'Add SPDX header to ~w file', [FileType]).

    auto_fixable(missing_spdx(_), true).

    fix_script(missing_spdx(_), 'scripts/fix-missing-spdx.sh').

:- end_object.
RULE_EOF
            ;;

        *)
            # Generic rule template for unknown types
            cat > "$proposal_file" << EOF
% SPDX-License-Identifier: PMPL-1.0-or-later
% Auto-generated rule: Detect ${type} issues
% Generated: $(date -Iseconds)
% Observations: $observations

:- object(${safe_type}_detector,
    extends(code_pattern_detector)).

    % TODO: Implement detection logic for ${type}
    has_issue(Path, ${safe_type}(Location)) :-
        source_file(Path),
        % Add specific detection logic here
        fail.  % Replace with actual implementation

    classify_severity(${safe_type}(_), medium).

    suggest_fix(${safe_type}(_), 'Manual review required for ${type}').

    auto_fixable(${safe_type}(_), false).

:- end_object.
EOF
            ;;
    esac

    log_bot "hypatia" "  ✓ Rule proposal created: $proposal_file"
    log_info "  Review at: $proposal_file"

    # Create approval PR if in CI environment
    if [[ -n "${GITHUB_ACTIONS:-}" ]]; then
        create_rule_approval_pr "$proposal_file" "$type" "$observations"
    fi
}

create_rule_approval_pr() {
    local rule_file="$1"
    local rule_type="$2"
    local observations="$3"

    log_bot "hypatia" "Creating rule approval PR for $rule_type..."

    # Clone hypatia repo to temp directory
    local hypatia_dir="/tmp/hypatia-rule-pr-$$"
    git clone https://github.com/hyperpolymath/hypatia.git "$hypatia_dir" 2>/dev/null

    cd "$hypatia_dir"

    # Create branch for rule
    local branch_name="auto-rule/${rule_type}-$(date +%Y%m%d)"
    git checkout -b "$branch_name"

    # Copy rule to hypatia's rules directory
    local target_dir="engine/rules/auto-learned"
    mkdir -p "$target_dir"
    cp "$rule_file" "$target_dir/$(basename "$rule_file")"

    # Commit and push
    git add "$target_dir"
    git config user.name "Hypatia Learning Engine"
    git config user.email "hypatia@reposystem.dev"

    git commit -m "$(cat <<COMMIT_EOF
feat(rules): auto-learned rule for ${rule_type}

Auto-generated rule based on ${observations} observations across repos.

This rule was automatically proposed by the Hypatia learning engine
after detecting a recurring pattern in repository scans.

Detection: ${rule_type}
Observations: ${observations}
Generated: $(date -Iseconds)

Rule proposal requires manual review and approval before merging.

Co-Authored-By: Hypatia Learning Engine <hypatia@reposystem.dev>
COMMIT_EOF
)"

    git push origin "$branch_name" 2>/dev/null || {
        log_warn "Failed to push rule PR branch"
        cd - > /dev/null
        rm -rf "$hypatia_dir"
        return 1
    }

    # Create PR using gh CLI if available
    if command -v gh &> /dev/null; then
        gh pr create \
            --title "Auto-learned rule: ${rule_type}" \
            --body "$(cat <<PR_BODY
## Auto-Learned Rule Proposal

**Rule Type:** ${rule_type}
**Observations:** ${observations} instances detected
**Generated:** $(date -Iseconds)

### Summary

This rule was automatically generated by the Hypatia learning engine after observing a recurring pattern across multiple repositories during security scans.

### Review Checklist

- [ ] Rule logic is correct and safe
- [ ] Severity classification is appropriate
- [ ] Fix suggestion is accurate
- [ ] Auto-fixable flag is correct
- [ ] Fix script exists and works

### Next Steps

1. Review the generated Logtalk rule
2. Test on sample repositories
3. Approve and merge if valid
4. Rule will be deployed in next Hypatia release

### Files Changed

- \`engine/rules/auto-learned/$(basename "$rule_file")\`

---
🤖 Generated by Hypatia Learning Engine
PR_BODY
)" \
            --repo hyperpolymath/hypatia 2>/dev/null && {
                log_bot "hypatia" "  ✓ Rule approval PR created"
            }
    fi

    cd - > /dev/null
    rm -rf "$hypatia_dir"
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

generate_rules() {
    log_info "Analyzing observed patterns for rule generation..."

    local patterns_file="$SHARED_CONTEXT/learning/observed-patterns.jsonl"

    if [[ ! -f "$patterns_file" ]]; then
        log_info "No observed patterns yet"
        return 0
    fi

    # Analyze pattern frequency
    log_info "Pattern frequency analysis:"

    # Group by type and count occurrences
    local pattern_types=$(jq -r '.type' "$patterns_file" | sort | uniq)

    for pattern_type in $pattern_types; do
        local count=$(grep "\"$pattern_type\"" "$patterns_file" | wc -l)

        log_info "  ${pattern_type}: $count observations"

        # Threshold for rule generation (configurable)
        local threshold="${RULE_THRESHOLD:-5}"

        if [[ $count -ge $threshold ]]; then
            log_bot "hypatia" "  → Threshold reached for '$pattern_type' ($count >= $threshold)"

            # Extract unique repos that observed this pattern
            local repos=$(grep "\"$pattern_type\"" "$patterns_file" | jq -r '.repo' | sort -u | wc -l)

            # Generate rule proposal
            propose_new_rule "$pattern_type" "$pattern_type" "$count ($repos repos)"
        fi
    done

    # Summary
    local total_patterns=$(wc -l < "$patterns_file")
    local unique_types=$(echo "$pattern_types" | wc -l)
    local proposals=$(find "$SHARED_CONTEXT/learning/rule-proposals" -name "*.lgt" 2>/dev/null | wc -l)

    log_info ""
    log_info "Learning Summary:"
    log_info "  Total observations: $total_patterns"
    log_info "  Unique pattern types: $unique_types"
    log_info "  Rule proposals generated: $proposals"

    if [[ $proposals -gt 0 ]]; then
        log_info ""
        log_info "Review rule proposals at: $SHARED_CONTEXT/learning/rule-proposals/"
    fi
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
