#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# pre-push-gate.sh — Local pre-push hook for static analysis.
#
# Runs panic-attack and hypatia locally before allowing a push.
# Exits non-zero if critical findings are detected.
#
# Installation (symlink into any repo):
#   ln -sf /path/to/gitbot-fleet/hooks/pre-push-gate.sh .git/hooks/pre-push
#
# Or copy it:
#   cp /path/to/gitbot-fleet/hooks/pre-push-gate.sh .git/hooks/pre-push
#   chmod +x .git/hooks/pre-push

set -euo pipefail

# Colours (disabled when stdout is not a terminal)
if [ -t 1 ]; then
  RED='\033[0;31m'
  YELLOW='\033[0;33m'
  GREEN='\033[0;32m'
  BOLD='\033[1m'
  RESET='\033[0m'
else
  RED='' YELLOW='' GREEN='' BOLD='' RESET=''
fi

REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
CRITICAL_COUNT=0
HIGH_COUNT=0
TOTAL_COUNT=0
SCANNERS_RUN=0

# --------------------------------------------------------------------------
# Helper: parse JSON finding counts (requires jq)
# --------------------------------------------------------------------------
count_findings() {
  local json_file="$1"
  if [ ! -s "$json_file" ] || ! jq empty "$json_file" 2>/dev/null; then
    return
  fi
  local c h m l
  c=$(jq '[.[] | select(.severity == "critical")] | length' "$json_file" 2>/dev/null || echo 0)
  h=$(jq '[.[] | select(.severity == "high")]     | length' "$json_file" 2>/dev/null || echo 0)
  m=$(jq '[.[] | select(.severity == "medium")]   | length' "$json_file" 2>/dev/null || echo 0)
  l=$(jq '[.[] | select(.severity == "low")]      | length' "$json_file" 2>/dev/null || echo 0)
  CRITICAL_COUNT=$(( CRITICAL_COUNT + c ))
  HIGH_COUNT=$(( HIGH_COUNT + h ))
  TOTAL_COUNT=$(( TOTAL_COUNT + c + h + m + l ))
}

# --------------------------------------------------------------------------
# Scanner 1: panic-attack assail
# --------------------------------------------------------------------------
run_panic_attack() {
  if ! command -v panic-attack >/dev/null 2>&1; then
    echo -e "${YELLOW}[skip]${RESET} panic-attack not installed — skipping assail"
    return
  fi

  echo -e "${BOLD}[1/2]${RESET} Running panic-attack assail..."
  local tmp
  tmp=$(mktemp /tmp/pa-findings-XXXXXX.json)

  set +e
  panic-attack assail --format json "$REPO_ROOT" > "$tmp" 2>/dev/null
  local rc=$?
  set -e

  if [ "$rc" -ne 0 ] && [ ! -s "$tmp" ]; then
    echo -e "${YELLOW}[warn]${RESET} panic-attack exited $rc with no output"
    rm -f "$tmp"
    return
  fi

  count_findings "$tmp"
  SCANNERS_RUN=$(( SCANNERS_RUN + 1 ))

  local pa_total
  pa_total=$(jq '. | length' "$tmp" 2>/dev/null || echo 0)
  echo -e "       panic-attack: ${pa_total} finding(s)"
  rm -f "$tmp"
}

# --------------------------------------------------------------------------
# Scanner 2: hypatia-cli.sh scan
# --------------------------------------------------------------------------
run_hypatia() {
  # Look for hypatia-cli.sh in common locations
  local hypatia_bin=""
  for candidate in \
    "$(command -v hypatia-cli.sh 2>/dev/null || true)" \
    "$HOME/hypatia/hypatia-cli.sh" \
    "$HOME/.local/bin/hypatia-cli.sh"; do
    if [ -x "$candidate" ]; then
      hypatia_bin="$candidate"
      break
    fi
  done

  if [ -z "$hypatia_bin" ]; then
    echo -e "${YELLOW}[skip]${RESET} hypatia-cli.sh not found — skipping scan"
    return
  fi

  echo -e "${BOLD}[2/2]${RESET} Running hypatia scan..."
  local tmp
  tmp=$(mktemp /tmp/hyp-findings-XXXXXX.json)

  set +e
  HYPATIA_FORMAT=json "$hypatia_bin" scan "$REPO_ROOT" > "$tmp" 2>/dev/null
  local rc=$?
  set -e

  if [ "$rc" -ne 0 ] && [ ! -s "$tmp" ]; then
    echo -e "${YELLOW}[warn]${RESET} hypatia exited $rc with no output"
    rm -f "$tmp"
    return
  fi

  count_findings "$tmp"
  SCANNERS_RUN=$(( SCANNERS_RUN + 1 ))

  local hyp_total
  hyp_total=$(jq '. | length' "$tmp" 2>/dev/null || echo 0)
  echo -e "       hypatia: ${hyp_total} finding(s)"
  rm -f "$tmp"
}

# --------------------------------------------------------------------------
# Main
# --------------------------------------------------------------------------
echo -e "${BOLD}Static Analysis Gate (pre-push)${RESET}"
echo "Repository: $REPO_ROOT"
echo ""

run_panic_attack
run_hypatia

echo ""
echo -e "${BOLD}--- Summary ---${RESET}"
echo "Scanners run : $SCANNERS_RUN"
echo "Total findings: $TOTAL_COUNT"
echo "  Critical    : $CRITICAL_COUNT"
echo "  High        : $HIGH_COUNT"

if [ "$SCANNERS_RUN" -eq 0 ]; then
  echo ""
  echo -e "${YELLOW}No scanners available — push allowed (install panic-attack or hypatia for local gating).${RESET}"
  exit 0
fi

if [ "$CRITICAL_COUNT" -gt 0 ]; then
  echo ""
  echo -e "${RED}BLOCKED: $CRITICAL_COUNT critical finding(s) detected. Fix before pushing.${RESET}"
  exit 1
fi

echo ""
echo -e "${GREEN}No critical findings — push allowed.${RESET}"
exit 0
