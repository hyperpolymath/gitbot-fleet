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

REF=$(git -C "$ROOT" rev-parse HEAD)  # a commit that carries the producer
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
printf 'on:\n  workflow_call:\n    inputs:\n      report-backs-artifact:\n        type: string\n        default: fleet-report-backs\njobs:\n  scan:\n    steps:\n      - uses: actions/download-artifact@9000827ccba6bdab643e8b6fd33ac0654aef8333\n' > "$STD/.github/workflows/hypatia-scan-reusable.yml"
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
canonical_workflow "$NEW_SHA" > "$A/.github/workflows/fork-governance.yml"; chmod 644 "$A/.github/workflows/fork-governance.yml"
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
check_grep "I: local reusable named" 'MANUAL: local Hypatia reusable .* has no report-backs-artifact input with a download-artifact step' "$TMP/i.out"
printf '      report-backs-artifact: # the input alone\n' >> "$I/.github/workflows/hypatia-scan-reusable.yml"
set +e; bash "$FIXER" "$I" > "$TMP/i1.out" 2>&1; rc=$?; set -e
check "I: the input name alone is not wiring → exit 3" 3 "$rc"
printf 'jobs:\n  scan:\n    steps:\n      - uses: actions/download-artifact@9000827ccba6bdab643e8b6fd33ac0654aef8333\n' >> "$I/.github/workflows/hypatia-scan-reusable.yml"
set +e; bash "$FIXER" "$I" > "$TMP/i2.out" 2>&1; rc=$?; set -e
check "I: local reusable with the input → exit 0" 0 "$rc"

# ── 10. a needs item that merely starts with "fleet" is not fleet ───────────
J="$TMP/j"; mk_repo "$J" https://github.com/hyperpolymath/example.git
canonical_workflow "$NEW_SHA" | awk '{print} /^    name: Hypatia verdict grid/ {print "    needs: [fleet-scan]"}' > "$J/.github/workflows/fork-governance.yml"
set +e; bash "$FIXER" "$J" > "$TMP/j.out" 2>&1; rc=$?; set -e
check "J: exit 0" 0 "$rc"
check_grep "J: fleet appended beside fleet-scan, not mistaken for it" '^    needs: \[fleet-scan, fleet\]$' "$J/.github/workflows/fork-governance.yml"
set +e; bash "$FIXER" "$J" > "$TMP/j2.out" 2>&1; set -e
check_grep "J: second run sees fleet as present" 'ok: hypatia already needs fleet' "$TMP/j2.out"

# ── 11. a job merely named fleet is not the producer ────────────────────────
K="$TMP/k"; mk_repo "$K" https://github.com/hyperpolymath/example.git
canonical_workflow "$NEW_SHA" | awk '{print} /^jobs:$/ {print "  fleet:"; print "    name: Something else entirely"; print "    runs-on: ubuntu-24.04"; print "    steps:"; print "      - run: true"; print ""}' > "$K/.github/workflows/fork-governance.yml"
set +e; bash "$FIXER" "$K" > "$TMP/k.out" 2>&1; rc=$?; set -e
check "K: exit 3" 3 "$rc"
check_grep "K: the impostor is named for a human" 'MANUAL: fork-governance.yml has a job named fleet that is not the report-back producer' "$TMP/k.out"
check "K: no second fleet job inserted" "1" "$(grep -cE '^  fleet:$' "$K/.github/workflows/fork-governance.yml" | tr -d ' ')"
check_no_grep "K: the impostor is not called the producer" 'already has the fleet report-back job' "$TMP/k.out"
set +e; bash "$FIXER" "$A" > "$TMP/a3.out" 2>&1; set -e
check_grep "A: the real producer job is recognised" 'ok: fork-governance.yml already has the fleet report-back job' "$TMP/a3.out"

# ── 12. the scalar needs form ───────────────────────────────────────────────
L="$TMP/l"; mk_repo "$L" https://github.com/hyperpolymath/example.git
canonical_workflow "$NEW_SHA" | awk '{print} /^    name: Hypatia verdict grid/ {print "    needs: fleet"}' > "$L/.github/workflows/fork-governance.yml"
set +e; bash "$FIXER" "$L" > "$TMP/l.out" 2>&1; rc=$?; set -e
check "L: scalar needs: fleet is already wired → exit 0" 0 "$rc"
check_grep "L: reported as present" 'ok: hypatia already needs fleet' "$TMP/l.out"
check "L: the scalar line is left as it was" "1" "$(grep -cE '^    needs: fleet$' "$L/.github/workflows/fork-governance.yml" | tr -d ' ')"
N="$TMP/n"; mk_repo "$N" https://github.com/hyperpolymath/example.git
canonical_workflow "$NEW_SHA" | awk '{print} /^    name: Hypatia verdict grid/ {print "    needs: workflow-contract"}' > "$N/.github/workflows/fork-governance.yml"
chmod 644 "$N/.github/workflows/fork-governance.yml"
set +e; bash "$FIXER" "$N" > "$TMP/n.out" 2>&1; rc=$?; set -e
check "N: scalar needs: other → exit 0" 0 "$rc"
check_grep "N: scalar other becomes a flow list with fleet" '^    needs: \[workflow-contract, fleet\]$' "$N/.github/workflows/fork-governance.yml"
check "N: the rewritten workflow keeps its mode (not mktemp's 0600)" "644" "$(stat -c %a "$N/.github/workflows/fork-governance.yml")"
check "A: the inserted-job workflow keeps its mode too" "644" "$(stat -c %a "$A/.github/workflows/fork-governance.yml")"

# ── 13. a pin that does not carry the producer is refused ───────────────────
# The parent of the commit that added the driver lacks it; a shallow clone may
# not hold that commit, in which case the case is reported as skipped.
ADDED=$(git -C "$ROOT" log --format=%H --diff-filter=A -1 -- scripts/fleet-report-back.sh || true)
if [ -n "$ADDED" ] && OLD=$(git -C "$ROOT" rev-parse -q --verify "${ADDED}^{commit}" 2>/dev/null) && OLD=$(git -C "$ROOT" rev-parse -q --verify "${OLD}^" 2>/dev/null) \
   && ! git -C "$ROOT" cat-file -e "$OLD:scripts/fleet-report-back.sh" 2>/dev/null; then
    O="$TMP/o"; mk_repo "$O" https://github.com/hyperpolymath/example.git
    canonical_workflow "$NEW_SHA" > "$O/.github/workflows/fork-governance.yml"
    cp -r "$O" "$TMP/o-before"
    set +e; FLEET_REF="$OLD" bash "$FIXER" "$O" > "$TMP/o.out" 2>&1; rc=$?; set -e
    check "O: a pin without the producer is refused with exit 2" 2 "$rc"
    check_grep "O: the missing producer file is named" 'does not carry .github/workflows/fleet-report-back-reusable.yml' "$TMP/o.out"
    check "O: refusal changed nothing" "" "$(diff -r "$TMP/o-before" "$O" || true)"
else
    echo "skip O: no commit without the producer is reachable from this checkout"
fi

# ── 14. dependencies and generated trees are not enrolment evidence ────────
run_fixer() { # label repo [finding-json]; records rc and output
    local label="$1"; shift
    rc=0
    bash "$FIXER" "$@" > "$TMP/$label.out" 2>&1 || rc=$?
}
dependent_enrolments() {
    jq -c '[.reporters.panicbot.enrolled, .reporters.seambot.enrolled,
            .reporters.accessibilitybot.enrolled, .reporters.echidnabot.enrolled]' "$1/.gitbot-fleet/enrolment.json"
}
for excluded in .git node_modules target vendor .fleet-src; do
    P="$TMP/pruned-$excluded"; mk_repo "$P" https://github.com/hyperpolymath/example.git
    canonical_workflow "$NEW_SHA" > "$P/.github/workflows/fork-governance.yml"
    mkdir -p "$P/$excluded/dependency/spec/seams"
    : > "$P/$excluded/dependency/Cargo.toml"
    : > "$P/$excluded/dependency/index.htm"
    : > "$P/$excluded/dependency/Theorem.lean"
    : > "$P/$excluded/dependency/spec/seams/seam-register.json"
    run_fixer "pruned-$excluded" "$P"
    check "$excluded: a tree containing only excluded evidence is still wired" 0 "$rc"
    check "$excluded: generated or vendored files enrol no conditional member" \
        '[false,false,false,false]' "$(dependent_enrolments "$P")"
    check "$excluded: every conditional exclusion carries an observed reason" 4 \
        "$(jq '[.reporters.panicbot,.reporters.seambot,.reporters.accessibilitybot,.reporters.echidnabot] | map(select(.enrolled == false and (.reason | length) > 0)) | length' "$P/.gitbot-fleet/enrolment.json")"
done

# Proofs can live outside proofs/, in any of the supported source languages.
# Conversely an explicitly declared, currently empty proofs/ is sufficient.
for proof in theorem.agda theorem.lean theorem.idr theorem.v proofs-directory; do
    P="$TMP/proof-$proof"; mk_repo "$P" https://github.com/hyperpolymath/example.git
    canonical_workflow "$NEW_SHA" > "$P/.github/workflows/fork-governance.yml"
    if [ "$proof" = proofs-directory ]; then
        mkdir -p "$P/proofs"
    else
        mkdir -p "$P/nested"; : > "$P/nested/$proof"
    fi
    run_fixer "proof-$proof" "$P"
    check "$proof: proof evidence wires successfully" 0 "$rc"
    check "$proof: only the proof-dependent member is enabled" \
        '[false,false,false,true]' "$(dependent_enrolments "$P")"
done
P="$TMP/nested source with spaces"; mk_repo "$P" https://github.com/hyperpolymath/example.git
canonical_workflow "$NEW_SHA" > "$P/.github/workflows/fork-governance.yml"
mkdir -p "$P/crates/app" "$P/site" "$P/docs"
: > "$P/crates/app/Cargo.toml"; : > "$P/site/index.htm"; : > "$P/docs/proof.txt"
run_fixer nested-source "$P" '{"ignored":"finding input is not tree evidence"}'
check "nested Cargo and .htm files enable their members, while prose is not a proof" \
    '[true,false,true,false]' "$(dependent_enrolments "$P")"
check "nested source in a path with spaces wires successfully" 0 "$rc"

# ── 15. owner detection and explicit job-form overrides ────────────────────
for choice in ssh-auto force-inline force-reusable; do
    P="$TMP/$choice"
    case "$choice" in
        ssh-auto) origin=git@github.com:hyperpolymath/example.git; form=auto; expected=reusable ;;
        force-inline) origin=https://github.com/hyperpolymath/example.git; form=inline; expected=inline ;;
        force-reusable) origin=https://github.com/another-owner/example.git; form=reusable; expected=reusable ;;
    esac
    mk_repo "$P" "$origin"
    canonical_workflow "$NEW_SHA" > "$P/.github/workflows/fork-governance.yml"
    FLEET_JOB_FORM="$form" run_fixer "$choice" "$P"
    check "$choice: explicit form or detected SSH owner wires successfully" 0 "$rc"
    check_grep "$choice: chosen job form is reported" "job form: $expected$" "$TMP/$choice.out"
    if [ "$expected" = reusable ]; then
        check_grep "$choice: reusable job pins the selected producer revision" \
            "uses: hyperpolymath/gitbot-fleet/.github/workflows/fleet-report-back-reusable.yml@$REF$" "$P/.github/workflows/fork-governance.yml"
    else
        check_grep "$choice: inline job pins the selected producer revision" \
            "^      FLEET_REF: $REF$" "$P/.github/workflows/fork-governance.yml"
    fi
done

# ── 16. existing flow dependencies and guards keep their meaning ───────────
for needs in '[]' '["deed", "fleet"]' "[ 'fleet', 'deed' ]"; do
    P="$TMP/flow-$pass"; mk_repo "$P" https://github.com/hyperpolymath/example.git
    canonical_workflow "$NEW_SHA" | awk -v needs="$needs" \
        '{print} /^    name: Hypatia verdict grid/ {print "    needs: " needs; print "    if: ${{ success() }}"}' \
        > "$P/.github/workflows/fork-governance.yml"
    run_fixer flow "$P"
    check "flow $needs: wiring succeeds" 0 "$rc"
    expected="$needs"; [ "$needs" != '[]' ] || expected='[fleet]'
    check_grep "flow $needs: dependency list preserves exact quoted fleet items" \
        '^    needs:' "$P/.github/workflows/fork-governance.yml"
    check "flow $needs: expected dependency list is retained" "    needs: $expected" \
        "$(grep '^    needs:' "$P/.github/workflows/fork-governance.yml" | head -n1)"
    check_grep "flow $needs: existing guard is preserved" \
        '^    if: \$\{\{ success\(\) \}\}$' "$P/.github/workflows/fork-governance.yml"
    cp "$P/.github/workflows/fork-governance.yml" "$TMP/flow-before.yml"
    run_fixer flow-again "$P"
    check "flow $needs: repeated wiring succeeds" 0 "$rc"
    check "flow $needs: repeated wiring is byte-identical" '' \
        "$(diff "$TMP/flow-before.yml" "$P/.github/workflows/fork-governance.yml" || true)"
done

# ── 17. invalid owner manifests and existing notes are never overwritten ────
for invalid in malformed wrong-shape; do
    P="$TMP/manifest-$invalid"; mk_repo "$P" https://github.com/hyperpolymath/example.git
    canonical_workflow "$NEW_SHA" > "$P/.github/workflows/fork-governance.yml"
    mkdir -p "$P/.gitbot-fleet/report-backs"
    case "$invalid" in
        malformed) printf 'not JSON\n' > "$P/.gitbot-fleet/enrolment.json" ;;
        wrong-shape) printf '{"reporters":[]}\n' > "$P/.gitbot-fleet/enrolment.json" ;;
    esac
    printf 'Owner-maintained report-back instructions\n' > "$P/.gitbot-fleet/report-backs/README.md"
    before=$(sha256sum "$P/.gitbot-fleet/enrolment.json" "$P/.gitbot-fleet/report-backs/README.md")
    run_fixer "manifest-$invalid" "$P"
    check "$invalid: invalid owner manifest requires manual action" 3 "$rc"
    check_grep "$invalid: manifest validation failure is explicit" \
        'MANUAL: .gitbot-fleet/enrolment.json exists but does not parse' "$TMP/manifest-$invalid.out"
    check "$invalid: both owner files remain byte-identical" "$before" \
        "$(sha256sum "$P/.gitbot-fleet/enrolment.json" "$P/.gitbot-fleet/report-backs/README.md")"
    check_grep "$invalid: independent workflow wiring still completes" \
        '^  fleet:$' "$P/.github/workflows/fork-governance.yml"
done

# A malformed panic-attack pin is rejected before creating any rollout files.
cp -r "$G" "$TMP/panic-ref-before"
PANIC_ATTACK_SHA=main run_fixer invalid-panic-ref "$G"
check "an invalid panic-attack pin is refused" 2 "$rc"
check_grep "invalid panic-attack pin explains the required format" \
    'PANIC_ATTACK_SHA must be a 40-hex commit' "$TMP/invalid-panic-ref.out"
check "invalid panic-attack pin leaves the entire repository unchanged" '' \
    "$(diff -r "$TMP/panic-ref-before" "$G" || true)"

# ── 18. a standards checkout without the pinned commit is unverified ────────
P="$TMP/unknown-standards-pin"; mk_repo "$P" https://github.com/hyperpolymath/example.git
canonical_workflow 1111111111111111111111111111111111111111 > "$P/.github/workflows/fork-governance.yml"
run_fixer unknown-standards-pin "$P"
check "an absent standards commit is not treated as successful wiring" 3 "$rc"
check_grep "the absent standards commit is identified as unverified" \
    'UNVERIFIED: standards pin 111111111111' "$TMP/unknown-standards-pin.out"

echo
echo "fix-fleet-enrolment: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
