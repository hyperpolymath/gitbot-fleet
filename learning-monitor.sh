#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Learning Loop Monitor - Track autonomous learning progress

set -euo pipefail

FLEET_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LEARNING_DIR="$FLEET_DIR/shared-context/learning"
FINDINGS_DIR="$FLEET_DIR/shared-context/findings"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${BLUE}╔═══════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║     Gitbot Fleet Learning Loop Monitor       ║${NC}"
echo -e "${BLUE}╚═══════════════════════════════════════════════╝${NC}"
echo ""

# 1. Pattern Observations
echo -e "${CYAN}📊 Pattern Observations${NC}"
if [[ -f "$LEARNING_DIR/observed-patterns.jsonl" ]]; then
    total_observations=$(wc -l < "$LEARNING_DIR/observed-patterns.jsonl")
    echo -e "  Total observations: ${GREEN}$total_observations${NC}"

    echo ""
    echo -e "  ${YELLOW}Top patterns:${NC}"
    jq -r '.type' "$LEARNING_DIR/observed-patterns.jsonl" | \
        sort | uniq -c | sort -rn | head -10 | \
        awk '{printf "    %-30s %s observations\n", $2, $1}'

    echo ""
    echo -e "  ${YELLOW}Repos with most patterns:${NC}"
    jq -r '.repo' "$LEARNING_DIR/observed-patterns.jsonl" | \
        sort | uniq -c | sort -rn | head -5 | \
        awk '{printf "    %-30s %s patterns\n", $2, $1}'
else
    echo -e "  ${RED}No observations yet${NC}"
fi

echo ""
echo -e "${CYAN}🧠 Rule Proposals${NC}"
if [[ -d "$LEARNING_DIR/rule-proposals" ]]; then
    proposal_count=$(find "$LEARNING_DIR/rule-proposals" -name "*.lgt" 2>/dev/null | wc -l)
    echo -e "  Total proposals: ${GREEN}$proposal_count${NC}"

    if [[ $proposal_count -gt 0 ]]; then
        echo ""
        echo -e "  ${YELLOW}Generated rules:${NC}"
        for rule in "$LEARNING_DIR/rule-proposals"/*.lgt; do
            rule_name=$(basename "$rule" .lgt)
            observations=$(grep "% Observations:" "$rule" | awk '{print $3}')
            generated=$(grep "% Generated:" "$rule" | cut -d' ' -f3-)

            # Check if rule is approved (moved to hypatia engine)
            if [[ -f "$FLEET_DIR/../hypatia/engine/rules/auto-learned/$(basename "$rule")" ]]; then
                status="${GREEN}✓ APPROVED${NC}"
            else
                status="${YELLOW}⏳ PENDING${NC}"
            fi

            printf "    %-30s %s observations  %b\n" "$rule_name" "$observations" "$status"
        done
    fi
else
    echo -e "  ${RED}No proposals yet${NC}"
fi

echo ""
echo -e "${CYAN}🔧 Auto-Fix Outcomes${NC}"
if [[ -f "$LEARNING_DIR/fix-outcomes.jsonl" ]]; then
    total_fixes=$(wc -l < "$LEARNING_DIR/fix-outcomes.jsonl")
    successful=$(jq -r 'select(.success == true)' "$LEARNING_DIR/fix-outcomes.jsonl" | wc -l)
    failed=$((total_fixes - successful))

    if [[ $total_fixes -gt 0 ]]; then
        success_rate=$(awk "BEGIN {printf \"%.1f\", ($successful/$total_fixes)*100}")
        echo -e "  Total fixes attempted: $total_fixes"
        echo -e "  Successful: ${GREEN}$successful${NC} (${success_rate}%)"
        echo -e "  Failed: ${RED}$failed${NC}"
    fi
else
    echo -e "  ${YELLOW}No fix attempts yet${NC}"
fi

echo ""
echo -e "${CYAN}📈 Learning Thresholds${NC}"
echo -e "  Pattern observation threshold: ${YELLOW}5${NC} observations"
echo -e "  Rule auto-approval threshold: ${YELLOW}10${NC} observations + ${YELLOW}3${NC} successful fixes"

# Check which patterns are approaching thresholds
if [[ -f "$LEARNING_DIR/observed-patterns.jsonl" ]]; then
    echo ""
    echo -e "  ${YELLOW}Patterns approaching rule generation (3-4 observations):${NC}"
    jq -r '.type' "$LEARNING_DIR/observed-patterns.jsonl" | \
        sort | uniq -c | \
        awk '$1 >= 3 && $1 < 5 {printf "    %-30s %s/5 observations\n", $2, $1}' | \
        head -5
fi

echo ""
echo -e "${CYAN}📦 Current Fleet Status${NC}"
unprocessed=$(find "$FINDINGS_DIR" -name "*.json" ! -name "*.processed" ! -name "latest.json" 2>/dev/null | wc -l)
processed=$(find "$FINDINGS_DIR" -name "*.processed" 2>/dev/null | wc -l)
repos_scanned=$(find "$FINDINGS_DIR" -type d -mindepth 1 -maxdepth 1 ! -name "self-scans" 2>/dev/null | wc -l)

echo -e "  Repos scanned: ${GREEN}$repos_scanned${NC}"
echo -e "  Findings unprocessed: ${YELLOW}$unprocessed${NC}"
echo -e "  Findings processed: ${GREEN}$processed${NC}"

echo ""
echo -e "${BLUE}═══════════════════════════════════════════════${NC}"
echo -e "${CYAN}💡 Next actions:${NC}"

# Suggest next actions based on state
if [[ $unprocessed -gt 0 ]]; then
    echo -e "  • Process $unprocessed pending findings: ${YELLOW}./fleet-coordinator.sh process-findings${NC}"
fi

if [[ -f "$LEARNING_DIR/observed-patterns.jsonl" ]]; then
    patterns_ready=$(jq -r '.type' "$LEARNING_DIR/observed-patterns.jsonl" | \
        sort | uniq -c | awk '$1 >= 5 {print $2}' | wc -l)
    if [[ $patterns_ready -gt 0 ]]; then
        echo -e "  • Generate rules for $patterns_ready patterns: ${YELLOW}./fleet-coordinator.sh generate-rules${NC}"
    fi
fi

if [[ -d "$LEARNING_DIR/rule-proposals" ]] && [[ $proposal_count -gt 0 ]]; then
    echo -e "  • Review rule proposals in: ${CYAN}$LEARNING_DIR/rule-proposals/${NC}"
fi

echo ""
