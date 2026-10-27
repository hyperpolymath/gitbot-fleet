#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Offline fixture test for scripts/fix-fleet-enrolment.sh: the rollout fixer
# that gives a repository the enrolment manifest, the report-backs directory,
# the fork-governance fleet job and the hypatia wiring, and refuses to call
# the result wired until the pinned Hypatia lane reads the artifact.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
FIXER="$ROOT/scripts/fix-fleet-enrolment.sh"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
export PIN_FIXER_LOCK_DIR="$TMP/locks"; mkdir -m 700 "$PIN_FIXER_LOCK_DIR"

pass=0; fail=0
check() { # name expected actual
    if [ "$2" = "$3" ]; then pass=$((pass + 1)); echo "ok   $1"
    else fail=$((fail + 1)); echo "FAIL $1"; echo "     want: $2"; echo "     got:  $3"; fi
}
check_grep() { # name pattern file
    if grep -qE -- "$2" "$3"; then pass=$((pass + 1)); echo "ok   $1"
    else fail=$((fail + 1)); echo "FAIL $1 (no match for '$2' in $3)"; sed 's/^/     | /' "$3" | head -40; fi
}
check_no_grep() { # name pattern file
    if grep -qE -- "$2" "$3"; then fail=$((fail + 1)); echo "FAIL $1 (unexpected match for '$2' in $3)"
    else pass=$((pass + 1)); echo "ok   $1"; fi
}

REF=1111111111111111111111111111111111111111
PANIC=2222222222222222222222222222222222222222
export FLEET_REF="$REF" PANIC_ATTACK_SHA="$PANIC"

# The canonical caller from standards' ADOPTION.adoc §2 (hypatia with no needs).
canonical_workflow() { # standards-sha
    cat <<EOF
# SPDX-License-Identifier: MPL-2.0
name: Fork governance

on:
  pull_request:

permissions:
  contents: read

jobs:
  deed:
    name: DEED syntax
    uses: hyperpolymath/standards/.github/workflows/deed-conformance.yml@$1

  hypatia:
    name: Hypatia verdict grid
    uses: hyperpolymath/standards/.github/workflows/hypatia-scan-reusable.yml@$1
    permissions:
      actions: read
      contents: read
      security-events: write

  verdict:
    name: Post the fork governance summary
    needs: [deed, hypatia]
    if: \${{ always() && github.event_name == 'pull_request' }}
    uses: hyperpolymath/standards/.github/workflows/fork-governance-reusable.yml@$1
EOF
}

mk_repo() { # dir origin-url
    mkdir -p "$1/.github/workflows"
    git -C "$1" init -q
    git -C "$1" remote add origin "$2"
}

# A fake standards checkout: one commit whose Hypatia reusable reads the
# artifact, one whose reusable does not.
STD="$TMP/standards"; mkdir -p "$STD/.github/workflows"; git -C "$STD" init -q
git -C "$STD" config user.email t@example.invalid; git -C "$STD" config user.name t
printf 'on:\n  workflow_call:\n    inputs:\n      scan-ref:\n        type: string\n' > "$STD/.github/workflows/hypatia-scan-reusable.yml"
git -C "$STD" add -A; git -C "$STD" -c commit.gpgsign=false commit -q -m old
OLD_SHA=$(git -C "$STD" rev-parse HEAD)
printf 'on:\n  workflow_call:\n    inputs:\n      report-backs-artifact:\n        type: string\n        default: fleet-report-backs\n' > "$STD/.github/workflows/hypatia-scan-reusable.yml"
git -C "$STD" add -A; git -C "$STD" -c commit.gpgsign=false commit -q -m new
NEW_SHA=$(git -C "$STD" rev-parse HEAD)
export STANDARDS_DIR="$STD"

# ── 0. the inline template mirrors the reusable step for step ───────────────
step_names() { grep -E '^ *- name: ' "$1" | sed -E 's/^ *- name: //'; }
check "inline job template has the reusable's steps, in order" \
    "$(step_names "$ROOT/.github/workflows/fleet-report-back-reusable.yml")" \
    "$(step_names "$ROOT/scripts/lib/fleet-job.inline.yml")"
check_grep "inline template pins the fleet by placeholder" '__FLEET_REF__' "$ROOT/scripts/lib/fleet-job.inline.yml"
check_grep "reusable template pins the fleet by placeholder" '@__FLEET_REF__$' "$ROOT/scripts/lib/fleet-job.reusable.yml"

# ── 1. hyperpolymath repo, canonical workflow, new standards pin ────────────
A="$TMP/a"; mk_repo "$A" https://github.com/hyperpolymath/example.git
canonical_workflow "$NEW_SHA" > "$A/.github/workflows/fork-governance.yml"
set +e; bash "$FIXER" "$A" > "$TMP/a.out" 2>&1; rc=$?; set -e
check "A: exit 0 — wired end to end" 0 "$rc"
check_grep "A: manifest created" 'CREATED \.gitbot-fleet/enrolment\.json' "$TMP/a.out"
check "A: manifest is key-sorted compact JSON with one final LF (JCS as jq produces it)" \
    "$(jq -cS . "$A/.gitbot-fleet/enrolment.json")" "$(cat "$A/.gitbot-fleet/enrolment.json")"
check "A: manifest ends with exactly one LF" "1" "$(tail -c1 "$A/.gitbot-fleet/enrolment.json" | wc -l | tr -d ' ')"
check "A: every-repo members enrolled" "true true true true" \
    "$(jq -r '[.reporters.rhodibot, .reporters.finishingbot, .reporters.glambot, .reporters.cipherbot][] | .enrolled' "$A/.gitbot-fleet/enrolment.json" | paste -sd' ' -)"
check "A: tree-dependent members not enrolled in an empty tree, with reasons" "4" \
    "$(jq -r '[.reporters.panicbot, .reporters.seambot, .reporters.accessibilitybot, .reporters.echidnabot][] | select(.enrolled == false and (.reason | length) > 0)' "$A/.gitbot-fleet/enrolment.json" | jq -s length)"
check "A: sustainabot stays the reserved slot" "false" "$(jq -r '.reporters.sustainabot.enrolled' "$A/.gitbot-fleet/enrolment.json")"
check "A: report-backs README created" "yes" "$([ -f "$A/.gitbot-fleet/report-backs/README.md" ] && echo yes || echo no)"
WF="$A/.github/workflows/fork-governance.yml"
check_grep "A: reusable form chosen for the hyperpolymath owner" "^    uses: hyperpolymath/gitbot-fleet/.github/workflows/fleet-report-back-reusable.yml@$REF$" "$WF"
check "A: fleet job sits before hypatia" "fleet hypatia" \
    "$(grep -nE '^  (fleet|hypatia):' "$WF" | sort -n | sed -E 's/^[0-9]+:  //; s/://' | paste -sd' ' -)"
# shellcheck disable=SC2016 # literal GitHub expression
check "A: hypatia gained needs and the cancel guard, right under its header" \
    '    needs: [fleet]|    if: ${{ !cancelled() }}' \
    "$(awk '/^  hypatia:/{p=1;next} p&&n<2{print;n++}' "$WF" | paste -sd'|' -)"
check_no_grep "A: no placeholder left in the workflow" '__[A-Z_]+__' "$WF"
check_grep "A: pinned standards lane confirmed to read the artifact" 'ok: standards pin .* reads the fleet-report-backs artifact' "$TMP/a.out"
cp -r "$A" "$TMP/a-before"
set +e; bash "$FIXER" "$A" > "$TMP/a2.out" 2>&1; rc=$?; set -e
check "A: second run exits 0" 0 "$rc"
check "A: second run changes nothing (idempotent)" "" "$(diff -r "$TMP/a-before" "$A" || true)"
check_grep "A: second run reports the manifest as the owner's" 'ok: .gitbot-fleet/enrolment.json exists' "$TMP/a2.out"
if command -v yq >/dev/null 2>&1; then
    check "A: edited workflow parses and has four jobs" "4" "$(yq -r '.jobs | length' "$WF" 2>/dev/null || echo parse-error)"
fi

# ── 2. other owner, old standards pin, a lock file, Cargo/HTML/seams/proofs ─
B="$TMP/b"; mk_repo "$B" git@github.com:metadatastician/example.git
canonical_workflow "$OLD_SHA" > "$B/.github/workflows/fork-governance.yml"
printf 'workflows: {}\n' > "$B/.github/workflows/actions.lock"
mkdir -p "$B/spec/seams" "$B/www" "$B/proofs" "$B/node_modules/x"
: > "$B/Cargo.toml"; : > "$B/spec/seams/seam-register.json"; : > "$B/www/index.html"; : > "$B/proofs/Thm.agda"
set +e; bash "$FIXER" "$B" > "$TMP/b.out" 2>&1; rc=$?; set -e
check "B: exit 3 — written but not wired" 3 "$rc"
WFB="$B/.github/workflows/fork-governance.yml"
check_grep "B: inline form chosen for another owner" '^      FLEET_REF: '"$REF"'$' "$WFB"
check_grep "B: panic-attack pin filled in" '^      PANIC_ATTACK_SHA: '"$PANIC"'$' "$WFB"
check_no_grep "B: no reusable call under another owner" 'fleet-report-back-reusable.yml@' "$WFB"
check_no_grep "B: template header comments are not copied into the workflow" '^# fleet-job' "$WFB"
check "B: inline job has the reusable's steps" \
    "$(step_names "$ROOT/.github/workflows/fleet-report-back-reusable.yml")" \
    "$(awk '/^  fleet:/{p=1;next} /^  hypatia:/{p=0} p' "$WFB" | grep -E '^ *- name: ' | sed -E 's/^ *- name: //')"
check "B: tree-dependent members enrolled from what the tree shows" "true true true true" \
    "$(jq -r '[.reporters.panicbot, .reporters.seambot, .reporters.accessibilitybot, .reporters.echidnabot][] | .enrolled' "$B/.gitbot-fleet/enrolment.json" | paste -sd' ' -)"
check_grep "B: old standards pin named as not reading the artifact" 'MANUAL: standards pin .* does not read the artifact; bump' "$TMP/b.out"
check_grep "B: lock regeneration named" 'MANUAL: actions.lock exists' "$TMP/b.out"
check "B: actions.lock itself untouched" 'workflows: {}' "$(cat "$B/.github/workflows/actions.lock")"
if command -v yq >/dev/null 2>&1; then
    check "B: inline workflow parses with the fleet job's steps" \
        "$(step_names "$ROOT/.github/workflows/fleet-report-back-reusable.yml" | wc -l | tr -d ' ')" \
        "$(yq -r '.jobs.fleet.steps | length' "$WFB" 2>/dev/null || echo parse-error)"
fi

# ── 3. hypatia already has a flow needs list and an always() guard ──────────
C="$TMP/c"; mk_repo "$C" https://github.com/hyperpolymath/example.git
canonical_workflow "$NEW_SHA" | awk '{print} /^    name: Hypatia verdict grid/ {print "    needs: [workflow-contract]"; print "    if: ${{ always() }}"}' > "$C/.github/workflows/fork-governance.yml"
set +e; bash "$FIXER" "$C" > "$TMP/c.out" 2>&1; rc=$?; set -e
check "C: exit 0 (a NOTE is not a blocker)" 0 "$rc"
check_grep "C: fleet appended to the existing needs list" '^    needs: \[workflow-contract, fleet\]$' "$C/.github/workflows/fork-governance.yml"
check "C: the existing if: is left alone, and not duplicated" "1" "$(awk '/^  hypatia:/{p=1;next} /^  [a-z]+:/{p=0} p && /^    if: /{n++} END{print n+0}' "$C/.github/workflows/fork-governance.yml")"
check_grep "C: always() is pointed out" 'NOTE: hypatia runs under always\(\)' "$TMP/c.out"
set +e; bash "$FIXER" "$C" > /dev/null 2>&1; set -e
check "C: fleet not appended twice" "1" "$(grep -c 'fleet' <<< "$(grep -E '^    needs:' "$C/.github/workflows/fork-governance.yml")")"

# ── 4. no fork-governance workflow at all ───────────────────────────────────
D="$TMP/d"; mk_repo "$D" https://github.com/hyperpolymath/example.git
set +e; bash "$FIXER" "$D" > "$TMP/d.out" 2>&1; rc=$?; set -e
check "D: exit 3" 3 "$rc"
check "D: manifest still written" "yes" "$([ -f "$D/.gitbot-fleet/enrolment.json" ] && echo yes || echo no)"
check_grep "D: adoption named" 'MANUAL: no .github/workflows/fork-governance.yml; adopt fork governance first' "$TMP/d.out"

# ── 5. an existing manifest is the owner's ──────────────────────────────────
E="$TMP/e"; mk_repo "$E" https://github.com/hyperpolymath/example.git
canonical_workflow "$NEW_SHA" > "$E/.github/workflows/fork-governance.yml"
mkdir -p "$E/.gitbot-fleet"; printf '{"reporters":{"rhodibot":{"enrolled":false,"reason":"owner says so"}},"schema":1}\n' > "$E/.gitbot-fleet/enrolment.json"
set +e; bash "$FIXER" "$E" > "$TMP/e.out" 2>&1; rc=$?; set -e
check "E: exit 0" 0 "$rc"
check "E: existing manifest byte-identical" '{"reporters":{"rhodibot":{"enrolled":false,"reason":"owner says so"}},"schema":1}' "$(cat "$E/.gitbot-fleet/enrolment.json")"

# ── 6. block-list needs is left to a human ──────────────────────────────────
F="$TMP/f"; mk_repo "$F" https://github.com/hyperpolymath/example.git
canonical_workflow "$NEW_SHA" | awk '{print} /^    name: Hypatia verdict grid/ {print "    needs:"; print "      - workflow-contract"}' > "$F/.github/workflows/fork-governance.yml"
set +e; bash "$FIXER" "$F" > "$TMP/f.out" 2>&1; rc=$?; set -e
check "F: exit 3" 3 "$rc"
check_grep "F: fleet job still inserted" '^  fleet:$' "$F/.github/workflows/fork-governance.yml"
check_grep "F: block list named for a human" "MANUAL: hypatia's needs: is a block list" "$TMP/f.out"

# ── 7. refusals change nothing ──────────────────────────────────────────────
G="$TMP/g"; mk_repo "$G" https://github.com/hyperpolymath/example.git
canonical_workflow "$NEW_SHA" > "$G/.github/workflows/fork-governance.yml"
cp -r "$G" "$TMP/g-before"
set +e; FLEET_REF=main bash "$FIXER" "$G" > "$TMP/g.out" 2>&1; rc=$?; set -e
check "G: a non-SHA FLEET_REF is refused with exit 2" 2 "$rc"
check "G: refusal changed nothing" "" "$(diff -r "$TMP/g-before" "$G" || true)"
set +e; FLEET_JOB_FORM=yaml bash "$FIXER" "$G" > /dev/null 2>&1; rc=$?; set -e
check "G: an unknown job form is refused with exit 2" 2 "$rc"
set +e; bash "$FIXER" "$TMP/does-not-exist" > /dev/null 2>&1; rc=$?; set -e
check "G: a missing repo path is refused with exit 2" 2 "$rc"

# ── 8. a remote pin with no standards checkout is UNVERIFIED, not ok ────────
H="$TMP/h"; mk_repo "$H" https://github.com/hyperpolymath/example.git
canonical_workflow "$NEW_SHA" > "$H/.github/workflows/fork-governance.yml"
set +e; STANDARDS_DIR='' bash "$FIXER" "$H" > "$TMP/h.out" 2>&1; rc=$?; set -e
check "H: exit 3 without a standards checkout to read the pin" 3 "$rc"
check_grep "H: pin reported UNVERIFIED" 'UNVERIFIED: standards pin' "$TMP/h.out"

# ── 9. a local Hypatia reusable (a fork) is read in place ───────────────────
I="$TMP/i"; mk_repo "$I" https://github.com/hyperpolymath/example.git
canonical_workflow "$NEW_SHA" | sed 's|uses: hyperpolymath/standards/.github/workflows/hypatia-scan-reusable.yml@.*|uses: ./.github/workflows/hypatia-scan-reusable.yml|' > "$I/.github/workflows/fork-governance.yml"
printf 'on:\n  workflow_call:\n    inputs:\n      scan-ref:\n        type: string\n' > "$I/.github/workflows/hypatia-scan-reusable.yml"
set +e; bash "$FIXER" "$I" > "$TMP/i.out" 2>&1; rc=$?; set -e
check "I: local reusable without the input → exit 3" 3 "$rc"
check_grep "I: local reusable named" 'MANUAL: local Hypatia reusable .* has no report-backs-artifact input' "$TMP/i.out"
printf '      report-backs-artifact:\n' >> "$I/.github/workflows/hypatia-scan-reusable.yml"
set +e; bash "$FIXER" "$I" > "$TMP/i2.out" 2>&1; rc=$?; set -e
check "I: local reusable with the input → exit 0" 0 "$rc"

echo
echo "fix-fleet-enrolment: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
