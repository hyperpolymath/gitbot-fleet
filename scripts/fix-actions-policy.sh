#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
#
# fix-actions-policy.sh — repair the estate-wide "Actions-policy CI outage"
#
# Driven by gitbot-fleet#362. An onboarding stamp set
# `allowed_actions=selected` + `sha_pinning_required=true` but never
# populated `patterns_allowed`, leaving it `[]`. An empty selected-list
# rejects every non-github-owned `uses:` at workflow-parse time, so the run
# dies as `startup_failure` with ZERO jobs — indistinguishable from "CI is
# broken" but actually a settings fault.
#
# MEASURED 2026-07-21: 80 of 306 scanned hyperpolymath repos are in this
# state (BROKEN-M1). `hyperpolymath` is a USER account, so there is no org
# lever and each repo needs its own settings PUT. `metadatastician` IS an
# Organization, where a single org-level PUT fixes every repo by
# inheritance (requires the `admin:org` token scope).
#
# ── Why this fixer is shaped differently to every other fix-*.sh ──────────
# Every other script in this directory takes a REPO_PATH and edits files, so
# its blast radius is a diff that a human reviews in a PR. This one writes
# repository *settings* through the API: there is no diff, no branch and no
# PR. Per #362 that means it must be report-by-default and gated. Hence:
#
#   * default mode is --list (READ ONLY — never writes)
#   * --apply is required to write, and is refused without an explicit mode
#   * the HYPATIA_AUTOMATION kill switch halts writes instantly
#   * the estate exclusion registry is consulted, FAIL-CLOSED, before writes
#   * every PUT is GET-verified, and the sweep ABORTS if sha_pinning was
#     silently reset (never trade pinning away for permissiveness)
#
# ── Target posture ───────────────────────────────────────────────────────
# #362's decision of record was `allowed_actions=all` + sha_pinning. A
# parallel remediation (llm-coding-configs/claude-code/notification-storm-
# remediation/FINDINGS.md, 2026-07-21) established a STRICTLY SAFER route
# that fixes the same 98 repos WITHOUT widening anything:
#
#   keep allowed_actions=selected, keep sha_pinning_required=true,
#   and simply POPULATE the empty patterns_allowed list.
#
# Its evidence is a natural experiment over 426 repos:
#   allowed_actions=all                    286 repos -> wrappers run
#   selected + `hyperpolymath/*` present    42 repos -> wrappers run
#   selected + patterns_allowed=[]          98 repos -> ALL startup_failure
# i.e. the breakage is caused by the list being EMPTY, not by it existing.
# Populating it therefore restores CI while KEEPING the owner allowlist as a
# real control — so `selected` is the default here, and `--mode all` is the
# opt-in fallback rather than the target.
#
# NEVER disable sha_pinning_required.
#
# Idempotent: healthy repos are reported `ok` and skipped.
#
# Usage:
#   fix-actions-policy.sh <owner> [--list|--apply] [--mode all|selected]
#                                 [--limit N] [--jsonl FILE] [--repo NAME]
#
# Exit codes: 0 ok · 2 usage · 3 sha_pinning reset (ABORT) · 4 kill switch
#             5 exclusion registry unavailable while applying

set -euo pipefail

OWNER=""
ACTION="list"          # list | apply   (report-by-default)
MODE="selected"        # selected (default, non-widening) | all (opt-in fallback)
ALLOWLIST=""           # curated patterns_allowed JSON; defaults to standards'
LIMIT=500
JSONL=""
ONE_REPO=""
FINDING_FILE=""        # dispatch-runner contract: $2 is the finding JSON
STD_REPO="hyperpolymath/standards"

die() { echo "fix-actions-policy: $*" >&2; exit "${2:-2}"; }

while [ $# -gt 0 ]; do
  case "$1" in
    --list)   ACTION="list" ;;
    --apply)  ACTION="apply" ;;
    --mode)   MODE="${2:-}"; shift ;;
    --allowlist) ALLOWLIST="${2:-}"; shift ;;
    --limit)  LIMIT="${2:-}"; shift ;;
    --jsonl)  JSONL="${2:-}"; shift ;;
    --repo)   ONE_REPO="${2:-}"; shift ;;
    -h|--help) sed -n '2,50p' "$0"; exit 0 ;;
    -*)       die "unknown flag: $1" ;;
    *)        if [ -z "$OWNER" ]; then OWNER="$1"
              elif [ -z "$FINDING_FILE" ]; then FINDING_FILE="$1"   # dispatch-runner passes finding JSON as $2
              else die "unexpected arg: $1"; fi ;;
  esac
  shift
done

# ── Dual invocation contract ─────────────────────────────────────────────
# scripts/dispatch-runner.sh calls every fixer as `<script> <repo_path>
# <finding.json>` — a filesystem path, not an owner. Standalone sweeps
# (#362) call it as `<script> <owner> --apply`. Detect which we got, so the
# same tool serves the fleet AND the 80-repo sweep instead of silently
# misreading a path as an owner.
if [ -n "$OWNER" ] && [ -d "$OWNER" ]; then
  REPO_PATH="$OWNER"
  ORIGIN="$(git -C "$REPO_PATH" remote get-url origin 2>/dev/null || true)"
  [ -n "$ORIGIN" ] || die "dispatcher mode: $REPO_PATH has no git origin"
  SLUG="$(printf '%s' "$ORIGIN" | sed -E 's#(git@|https://)github\.com[:/]##; s#\.git$##')"
  OWNER="${SLUG%%/*}"
  ONE_REPO="${SLUG#*/}"
  [ -n "$OWNER" ] && [ -n "$ONE_REPO" ] || die "dispatcher mode: cannot parse owner/repo from '$ORIGIN'"
  # Report-by-default in the dispatcher path. Per #362 this fixer performs a
  # credentialed, diff-less settings write with no PR to review, so it must
  # NOT auto-apply just because a finding routed to it. Opt in explicitly.
  case "${FIX_ACTIONS_POLICY_APPLY:-}" in
    1|true|yes) ACTION="apply" ;;
    *)          ACTION="list" ;;
  esac
  echo "fix-actions-policy: dispatcher mode — ${OWNER}/${ONE_REPO} (action=${ACTION})"
fi

[ -n "$OWNER" ] || die "usage: fix-actions-policy.sh <owner|repo_path> [--list|--apply] [--mode all|selected]"
case "$MODE" in all|selected) ;; *) die "--mode must be 'all' or 'selected'" ;; esac
command -v gh >/dev/null || die "gh CLI not found"
command -v jq >/dev/null || die "jq not found"

# ── Guard rails (writes only) ────────────────────────────────────────────
if [ "$ACTION" = "apply" ]; then
  case "${HYPATIA_AUTOMATION:-}" in
    off|disabled|0)
      die "HYPATIA_AUTOMATION=${HYPATIA_AUTOMATION} — global kill switch engaged" 4 ;;
  esac

  # Fail-closed: if the estate denylist cannot be read, do not write.
  EXCL_RAW="$(gh api "repos/${STD_REPO}/contents/.machine_readable/bot_exclusion_registry.a2ml" \
                --jq '.content' 2>/dev/null | base64 -d 2>/dev/null || true)"
  if [ -z "$EXCL_RAW" ]; then
    die "exclusion registry unreadable (${STD_REPO}/.machine_readable/bot_exclusion_registry.a2ml) — refusing to write" 5
  fi
  # external-repos axis: exact owner/repo matches that must never be touched.
  EXCLUDED="$(printf '%s' "$EXCL_RAW" | grep -oE '"[A-Za-z0-9._-]+/[A-Za-z0-9._-]+"' | tr -d '"' | sort -u || true)"
fi

is_excluded() {
  [ -n "${EXCLUDED:-}" ] || return 1
  printf '%s\n' "$EXCLUDED" | grep -qxF "$1"
}

emit() { [ -n "$JSONL" ] && printf '%s\n' "$1" >> "$JSONL"; return 0; }

# ── Account shape decides the lever ──────────────────────────────────────
OWNER_TYPE="$(gh api "users/${OWNER}" --jq '.type' 2>/dev/null || echo Unknown)"

if [ "$OWNER_TYPE" = "Organization" ] && [ -z "$ONE_REPO" ]; then
  echo "== ${OWNER} is an Organization — a single org-level PUT covers every repo by inheritance."
  if ORG="$(gh api "orgs/${OWNER}/actions/permissions" 2>/dev/null)"; then
    echo "   current: allowed_actions=$(echo "$ORG" | jq -r '.allowed_actions // "-"')"
    if [ "$ACTION" = "apply" ]; then
      gh api --method PUT "orgs/${OWNER}/actions/permissions" \
        -f enabled_repositories=all -f allowed_actions="$MODE" >/dev/null
      echo "   applied: allowed_actions=${MODE} (org level)"
    else
      echo "   --list only; re-run with --apply to set allowed_actions=${MODE}"
    fi
  else
    echo "   !! cannot read org Actions policy — the token lacks the 'admin:org' scope."
    echo "      Run: gh auth refresh -h github.com -s admin:org"
    echo "      Falling back to a per-repo sweep below."
  fi
fi

# ── Per-repo sweep ───────────────────────────────────────────────────────
if [ -n "$ONE_REPO" ]; then
  REPOS="$ONE_REPO"
else
  REPOS="$(gh repo list "$OWNER" --no-archived --source --limit "$LIMIT" --json name -q '.[].name')"
fi

TOTAL=0; BROKEN=0; FIXED=0; SKIPPED=0
printf '%-44s %-9s %-9s %-8s %s\n' REPO ALLOWED SHA_PIN PATTERNS STATUS

for R in $REPOS; do
  TOTAL=$((TOTAL+1))
  FULL="${OWNER}/${R}"

  P="$(gh api "repos/${FULL}/actions/permissions" 2>/dev/null || true)"
  if [ -z "$P" ]; then
    printf '%-44s %-9s %-9s %-8s %s\n' "$R" - - - "no-access"
    emit "{\"repo\":\"${FULL}\",\"status\":\"no-access\"}"
    continue
  fi

  AL="$(echo "$P" | jq -r '.allowed_actions // "-"')"
  SP="$(echo "$P" | jq -r '.sha_pinning_required // "-"')"
  N="-"
  if [ "$AL" = "selected" ]; then
    N="$(gh api "repos/${FULL}/actions/permissions/selected-actions" --jq '.patterns_allowed|length' 2>/dev/null || echo 0)"
  fi

  # BROKEN-M1: selected with an empty allowlist == every non-github action rejected.
  if [ "$AL" = "selected" ] && [ "$N" = "0" ]; then
    BROKEN=$((BROKEN+1)); STATUS="BROKEN-M1"
  else
    STATUS="ok"
  fi

  if [ "$ACTION" != "apply" ] || [ "$STATUS" != "BROKEN-M1" ]; then
    printf '%-44s %-9s %-9s %-8s %s\n' "$R" "$AL" "$SP" "$N" "$STATUS"
    emit "{\"repo\":\"${FULL}\",\"allowed_actions\":\"${AL}\",\"sha_pinning_required\":\"${SP}\",\"patterns\":\"${N}\",\"status\":\"${STATUS}\"}"
    continue
  fi

  if is_excluded "$FULL"; then
    SKIPPED=$((SKIPPED+1))
    printf '%-44s %-9s %-9s %-8s %s\n' "$R" "$AL" "$SP" "$N" "excluded"
    emit "{\"repo\":\"${FULL}\",\"status\":\"excluded\"}"
    continue
  fi

  if [ "$MODE" = "all" ]; then
    gh api --method PUT "repos/${FULL}/actions/permissions" \
      -f enabled=true -f allowed_actions=all -F sha_pinning_required=true >/dev/null
  else
    # Non-widening path: leave allowed_actions/sha_pinning untouched and only
    # populate the empty patterns_allowed. Prefer an explicitly curated list
    # (--allowlist); otherwise mirror hyperpolymath/standards, which is itself
    # a healthy `selected` repo.
    SRC="$ALLOWLIST"
    if [ -z "$SRC" ]; then
      gh api "repos/${STD_REPO}/actions/permissions/selected-actions" > "/tmp/allow.$$.json" 2>/dev/null \
        || die "cannot read the standards allowlist; pass --allowlist FILE instead"
      SRC="/tmp/allow.$$.json"
    fi
    [ -s "$SRC" ] || die "allowlist '$SRC' is missing or empty"
    jq -e '(.patterns_allowed|length) > 0' "$SRC" >/dev/null 2>&1 \
      || die "allowlist '$SRC' has an EMPTY patterns_allowed — that is the very fault being fixed"
    gh api --method PUT "repos/${FULL}/actions/permissions/selected-actions" --input "$SRC" >/dev/null
    # `&& rm` alone would return 1 when the test is false and, under `set -e`,
    # abort the sweep on the --allowlist path. Keep the guard total.
    if [ "$SRC" = "/tmp/allow.$$.json" ]; then rm -f "/tmp/allow.$$.json"; fi
  fi

  # GET-verify. Trading sha_pinning away for permissiveness is never acceptable,
  # so a silent reset aborts the whole sweep rather than continuing.
  V="$(gh api "repos/${FULL}/actions/permissions" 2>/dev/null || true)"
  VSP="$(echo "$V" | jq -r '.sha_pinning_required // "-"')"
  VAL="$(echo "$V" | jq -r '.allowed_actions // "-"')"
  if [ "$SP" = "true" ] && [ "$VSP" != "true" ]; then
    printf '%-44s %-9s %-9s %-8s %s\n' "$R" "$VAL" "$VSP" "-" "ABORT-PIN-RESET"
    emit "{\"repo\":\"${FULL}\",\"status\":\"abort-sha-pinning-reset\"}"
    die "sha_pinning_required was reset to '${VSP}' on ${FULL} — aborting sweep" 3
  fi

  FIXED=$((FIXED+1))
  printf '%-44s %-9s %-9s %-8s %s\n' "$R" "$VAL" "$VSP" "-" "FIXED"
  emit "{\"repo\":\"${FULL}\",\"allowed_actions\":\"${VAL}\",\"sha_pinning_required\":\"${VSP}\",\"status\":\"fixed\"}"
done

echo
echo "scanned=${TOTAL} broken=${BROKEN} fixed=${FIXED} excluded=${SKIPPED} mode=${ACTION}/${MODE}"
if [ "$ACTION" != "apply" ] && [ "$BROKEN" -gt 0 ]; then
  echo "re-run with --apply to repair the ${BROKEN} BROKEN-M1 repo(s)."
fi
