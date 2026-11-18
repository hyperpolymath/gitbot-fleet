#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# enrolment-canary-test.sh — the fleet-enrolment rollout, end to end, on a
# synthetic repository instead of an estate one (owner decision 2026-11-18).
#
# One chain, each stage the real script:
#   1. a repository with a fork-governance Hypatia lane and no manifest;
#   2. scripts/fix-fleet-enrolment.sh writes the manifest, inserts the fleet
#      job and wires `hypatia: needs: [fleet]` (exit 0: no actions.lock here);
#   3. scripts/fleet-report-back.sh runs the members the manifest enrols
#      (adapters stubbed with their real JSON shapes) and writes envelopes;
#   4. standards' scripts/verdict-grid.sh renders the grid twice: without the
#      envelopes (the "before" state: every enrolled row orange, "enrolled,
#      but no report-back was received") and with them (the "after" state: no
#      enrolled row left orange for want of evidence).
#
# What this does NOT test: GitHub's startup rules for the inserted job (the
# lock records, reusable resolution). Those need a live run on a real repo.
#
# Stage 4 needs STANDARDS_DIR (a standards checkout carrying
# scripts/verdict-grid.sh and scripts/lib/verdict-grid.jq); without it the
# renderer stage is skipped and says so.
#
# Run: STANDARDS_DIR=/path/to/standards bash tests/enrolment-canary-test.sh
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
FIXER="$ROOT/scripts/fix-fleet-enrolment.sh"
DRIVER="$ROOT/scripts/fleet-report-back.sh"
GRID_STANDARDS="${STANDARDS_DIR:-}"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
export PIN_FIXER_LOCK_DIR="$TMP/locks"; mkdir -m 700 "$PIN_FIXER_LOCK_DIR"

pass=0; fail=0
check() { # name expected actual
    if [ "$2" = "$3" ]; then pass=$((pass + 1)); echo "ok   $1"
    else fail=$((fail + 1)); echo "FAIL $1"; echo "     want: $2"; echo "     got:  $3"; fi
}

# ── a standards checkout whose Hypatia reusable reads the artifact ───────────
# The fixer verifies the pinned reusable declares `report-backs-artifact`; a
# synthetic checkout keeps that check real without needing the network.
STD="$TMP/standards"; mkdir -p "$STD/.github/workflows"; git -C "$STD" init -q
printf 'on:\n  workflow_call:\n    inputs:\n      report-backs-artifact:\n        type: string\n        default: fleet-report-backs\njobs:\n  scan:\n    steps:\n      - uses: actions/download-artifact@9000827ccba6bdab643e8b6fd33ac0654aef8333\n' \
    > "$STD/.github/workflows/hypatia-scan-reusable.yml"
git -C "$STD" add -A
git -C "$STD" -c user.email=t@example.invalid -c user.name=t -c commit.gpgsign=false commit -q -m reads-artifact
STD_SHA=$(git -C "$STD" rev-parse HEAD)

# ── 1. the canary repository: a Hypatia lane, no manifest ────────────────────
REPO="$TMP/canary"; mkdir -p "$REPO/.github/workflows" "$REPO/src"
git -C "$REPO" init -q
git -C "$REPO" remote add origin https://github.com/hyperpolymath/canary.git
echo 'x' > "$REPO/src/a.jl"
cat > "$REPO/.github/workflows/fork-governance.yml" <<EOF
# SPDX-License-Identifier: MPL-2.0
name: Fork governance

on:
  pull_request:

permissions:
  contents: read

jobs:
  deed:
    name: DEED syntax
    uses: hyperpolymath/standards/.github/workflows/deed-conformance.yml@$STD_SHA

  hypatia:
    name: Hypatia verdict grid
    uses: hyperpolymath/standards/.github/workflows/hypatia-scan-reusable.yml@$STD_SHA
    permissions:
      actions: read
      contents: read
      security-events: write

  verdict:
    name: Post the fork governance summary
    needs: [deed, hypatia]
    if: \${{ always() && github.event_name == 'pull_request' }}
    uses: hyperpolymath/standards/.github/workflows/fork-governance-reusable.yml@$STD_SHA
EOF
check "before: the canary has no manifest" no "$([ -e "$REPO/.gitbot-fleet/enrolment.json" ] && echo yes || echo no)"

# ── 2. the fixer ─────────────────────────────────────────────────────────────
FLEET_REF=$(git -C "$ROOT" rev-parse HEAD)
set +e
STANDARDS_DIR="$STD" FLEET_REF="$FLEET_REF" PANIC_ATTACK_SHA=2222222222222222222222222222222222222222 \
    bash "$FIXER" "$REPO" > "$TMP/fixer.out" 2>&1
rc=$?
set -e
check "fixer: exits 0 (manifest written, job inserted and wired, pin reads the artifact)" 0 "$rc"
[ "$rc" = 0 ] || sed 's/^/     | /' "$TMP/fixer.out"
MANIFEST="$REPO/.gitbot-fleet/enrolment.json"
check "fixer: the manifest is a regular file" yes "$([ -f "$MANIFEST" ] && [ ! -L "$MANIFEST" ] && echo yes || echo no)"
check "fixer: the manifest parses as JSON" yes "$(jq -e . "$MANIFEST" >/dev/null 2>&1 && echo yes || echo no)"
WF="$REPO/.github/workflows/fork-governance.yml"
check "fixer: the fleet job calls the fleet reusable at the fleet ref" yes \
    "$(grep -qE "uses: hyperpolymath/gitbot-fleet/\.github/workflows/fleet-report-back-reusable\.yml@$FLEET_REF\$" "$WF" && echo yes || echo no)"
check "fixer: hypatia waits for the fleet job" yes \
    "$(awk '/^  hypatia:/{p=1;next} p&&/^  [a-z]/{exit} p' "$WF" | grep -qE '^    needs: \[fleet\]$' && echo yes || echo no)"
set +e; STANDARDS_DIR="$STD" FLEET_REF="$FLEET_REF" PANIC_ATTACK_SHA=2222222222222222222222222222222222222222 \
    bash "$FIXER" "$REPO" > "$TMP/fixer2.out" 2>&1; rc2=$?; set -e
check "fixer: a second run is a no-op that still exits 0" 0 "$rc2"
check "fixer: a second run inserts no second fleet job" 1 "$(grep -cE '^  fleet:' "$WF")"

ENROLLED=$(jq -r '.reporters | to_entries[] | select(.value.enrolled == true) | .key' "$MANIFEST" | sort | tr '\n' ' ' | sed 's/ $//')
echo "     enrolled: $ENROLLED"

# ── 3. the producer, with stub adapters ──────────────────────────────────────
BIN="$TMP/bin"; mkdir -p "$BIN"
stub() { printf '#!/usr/bin/env bash\nset -u\n%s\n' "$2" > "$BIN/$1"; chmod +x "$BIN/$1"; }
stub rhodibot 'echo "{\"subject\":\"$3\",\"criteria\":1,\"scored\":1,\"not_applicable\":0,\"counts\":{\"satisfied\":1,\"relocated\":0,\"deprecated\":0,\"missing\":0},\"findings\":[]}"'
stub finishing-bot 'echo "{\"license\":{\"findings\":[]},\"placeholder\":{\"findings\":[]},\"release\":{\"findings\":[]}}"'
stub glambot 'echo "{\"visual\":{\"findings\":[]},\"seo\":{\"findings\":[]}}"'
stub cipherbot 'echo "[]"'
stub panicbot 'echo "[]"'
stub panic-attack 'echo stub'
stub seambot 'echo "{\"status\":\"pass\",\"findings\":[],\"summary\":{\"errors\":0}}"'
stub accessibilitybot 'echo "{\"findings\":[]}"'
repo_before=$(cd "$REPO" && find . -type f -exec sha256sum {} + | sort)
OUT="$TMP/report-backs"
ECHIDNA_URL='' bash "$DRIVER" --repo "$REPO" --out "$OUT" --bin-dir "$BIN" --fleet-ref "$FLEET_REF" > "$TMP/driver.out" 2>&1 \
    || { echo "     driver exit $?"; sed 's/^/     | /' "$TMP/driver.out"; }
GOT=$(find "$OUT" -name '*.json' -printf '%f\n' 2>/dev/null | sed 's/\.json$//' | sort | tr '\n' ' ' | sed 's/ $//')
check "producer: one envelope per enrolled member, no more" "$ENROLLED" "$GOT"
check "producer: the scanned repository is unchanged" "$repo_before" "$(cd "$REPO" && find . -type f -exec sha256sum {} + | sort)"
for m in $ENROLLED; do
    s=$(jq -r .status "$OUT/$m.json")
    case "$m" in
        echidnabot | sustainabot) check "producer: $m is not_run with a reason" not_run "$s" ;;
        *) check "producer: $m ran" ran "$s" ;;
    esac
done

# ── 4. the renderer: before and after ────────────────────────────────────────
GRID="$GRID_STANDARDS/scripts/verdict-grid.sh"
if [ -n "$GRID_STANDARDS" ] && [ -f "$GRID" ]; then
    EMPTY="$TMP/empty"; mkdir -p "$EMPTY"
    render() { # report-backs-dir out-dir
        (cd "$REPO" && bash "$GRID" render --out "$2" --hypatia /dev/null --panic /dev/null --pons /dev/null \
            --report-backs "$1" --enrolment .gitbot-fleet/enrolment.json --sha 0000000 > /dev/null)
    }
    render "$EMPTY" "$TMP/grid-before"
    render "$OUT" "$TMP/grid-after"
    silent() { jq '[.reporters[] | select(.reason == "enrolled, but no report-back was received")] | length' "$1/report.json"; }
    n=$(echo "$ENROLLED" | wc -w)
    check "renderer, before: every enrolled member is 'enrolled, but no report-back'" "$n" "$(silent "$TMP/grid-before")"
    check "renderer, after: no enrolled member is left without a report-back" 0 "$(silent "$TMP/grid-after")"
    for m in $ENROLLED; do
        st=$(jq -r --arg id "$m" '.reporters[] | select(.id == $id) | .state' "$TMP/grid-after/report.json")
        case "$m" in
            echidnabot | sustainabot) check "renderer, after: $m (not_run) stays orange, now with its reason" orange "$st" ;;
            *) check "renderer, after: $m ran clean and is green" green "$st" ;;
        esac
    done
    for m in $(jq -r '.reporters | to_entries[] | select(.value.enrolled == false) | .key' "$MANIFEST"); do
        check "renderer: $m (not enrolled) is grey" grey \
            "$(jq -r --arg id "$m" '.reporters[] | select(.id == $id) | .state' "$TMP/grid-after/report.json")"
    done
else
    echo "SKIP: STANDARDS_DIR not set or has no scripts/verdict-grid.sh; renderer stage not run"
fi

echo
echo "enrolment canary: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
