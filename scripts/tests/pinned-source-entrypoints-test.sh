#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
# Exercise the real preflight on isolated proposed trees. Cached Git objects are
# read as evidence; no remote workflow/action code or live canary is executed.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
: "${PINNED_SOURCE_CACHE:?Set PINNED_SOURCE_CACHE to the authenticated bare Git cache}"
for tool in bash cp jq timeout; do
  command -v "$tool" >/dev/null || { echo "Missing test prerequisite: $tool" >&2; exit 64; }
done
WORK="$(mktemp -d "${TMPDIR:-/tmp}/pinned-source-entrypoints.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT
PASS=0; FAIL=0
MIG='docs/migrations/secret-scanner-cicd-suite'
BASE="$WORK/base"
mkdir -p "$BASE/scripts/lib" "$BASE/$MIG"
cp -R "$ROOT/.github" "$BASE/"
cp "$ROOT/scripts/audit-lock-closure.sh" "$ROOT/scripts/audit-pinned-reusable-sources.sh" "$BASE/scripts/"
for name in lock-closure-receipts.tsv lock-audit-baseline.tsv pinned-reusable-sources.json; do
  cp "$ROOT/scripts/lib/$name" "$BASE/scripts/lib/"
done
cp "$ROOT/$MIG/preflight.sh" "$ROOT/$MIG/required-contexts.txt" "$BASE/$MIG/"
cp -R "$ROOT/$MIG/fixtures" "$ROOT/$MIG/patches" "$BASE/$MIG/"
# Deliberately omit the canary directory. Every invocation binds --receipt to
# a nonexistent isolated path, so these static tests cannot authorize rollout.
new_case() { CASE="$WORK/$1"; cp -R "$BASE" "$CASE"; }
run_preflight() {
  RC=0
  timeout 120s bash "$CASE/$MIG/preflight.sh" --receipt "$CASE/missing-live-canary.json" "$@" > "$CASE/output" 2>&1 || RC=$?
}
pass() { printf 'PASS: %s\n' "$1"; PASS=$((PASS + 1)); }
fail() { printf 'FAIL: %s (exit %s)\n' "$1" "$RC"; cat "$CASE/output"; FAIL=$((FAIL + 1)); }
expect_finding() {
  local label="$1" rule="$2"
  run_preflight
  if [[ "$RC" == 1 || "$RC" == 3 ]] && grep -q '^== FINDINGS ' "$CASE/output" && grep -q "^${rule}"$'\t' "$CASE/output"; then
    pass "$label"
  else fail "$label"; fi
}
manifest_edit() {
  jq "$1" "$CASE/scripts/lib/pinned-reusable-sources.json" > "$CASE/manifest.tmp"
  mv "$CASE/manifest.tmp" "$CASE/scripts/lib/pinned-reusable-sources.json"
}
new_case untouched
run_preflight --quiet
if [[ "$RC" == 2 && ! -s "$CASE/output" ]]; then pass 'untouched candidate remains blocked by missing canary (quiet exit 2)'; else fail 'untouched candidate remains blocked by missing canary'; fi
run_preflight
# Nonquiet offline mode additionally reports ONLINE as an explicit optional
# evidence limit. Neither that limit nor CANARY may become a passing result.
if [[ "$RC" == 2 ]] && ! grep -q '^== FINDINGS ' "$CASE/output" &&
   grep -q $'^NOT-RUN\tCANARY\t' "$CASE/output" &&
   ! grep '^NOT-RUN' "$CASE/output" | grep -Ev $'^NOT-RUN\t(CANARY|ONLINE)\t' >/dev/null; then
  pass 'untouched diagnostics contain only missing canary and optional offline evidence'
else fail 'untouched diagnostic classification'; fi
new_case wrong-blob
manifest_edit '.sources[0].blob = "0000000000000000000000000000000000000000"'
expect_finding 'altered source blob cannot bypass proposed-tree authentication' LOCK-CLOSURE
new_case omitted-catalog-root
manifest_edit '.sources |= map(select(.repository != "hyperpolymath/cicd-suite"))'
expect_finding 'omitted active suite catalog root remains a finding' LOCK-CLOSURE
new_case removed-manifest
rm "$CASE/scripts/lib/pinned-reusable-sources.json"
expect_finding 'removed source route requires the missing vendor records' LOCK-MISSING
new_case dropped-receipt
awk -F '\t' '$1 != "hyperpolymath/cicd-suite"' "$CASE/scripts/lib/lock-closure-receipts.tsv" > "$CASE/receipts.tmp"
mv "$CASE/receipts.tmp" "$CASE/scripts/lib/lock-closure-receipts.tsv"
expect_finding 'dropped reviewed suite receipt remains a finding' CLOSURE-NO-RECEIPT
new_case altered-receipt
sed 's/actions\/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1/actions\/checkout@0000000000000000000000000000000000000000/' "$CASE/scripts/lib/lock-closure-receipts.tsv" > "$CASE/receipts.tmp"
mv "$CASE/receipts.tmp" "$CASE/scripts/lib/lock-closure-receipts.tsv"
expect_finding 'altered reviewed suite receipt remains a finding' CLOSURE-NO-RECEIPT
new_case fast-pr
sed 's/fast-pr: false/fast-pr: true/' "$CASE/.github/workflows/secret-scanner.yml" > "$CASE/workflow.tmp"
mv "$CASE/workflow.tmp" "$CASE/.github/workflows/secret-scanner.yml"
expect_finding 'fast-pr true remains a finding' WF-FAST-PR
new_case secrets-inherit
printf '    secrets: inherit\n' >> "$CASE/.github/workflows/secret-scanner.yml"
expect_finding 'secret inheritance remains a finding' WF-SECRETS
printf '\n%s passed, %s failed\n' "$PASS" "$FAIL"
[[ "$FAIL" == 0 ]]
