#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# fix-fleet-enrolment.sh — give a repository what its Hypatia verdict grid needs
# before a gitbot-fleet row can say anything (docs/REPORT-BACK.adoc §5):
#
#   1. `.gitbot-fleet/enrolment.json`, written only when absent. Members that
#      apply to every estate repository (rhodibot, finishingbot, glambot,
#      cipherbot) are enrolled; panicbot, seambot, accessibilitybot and
#      echidnabot are enrolled when the tree shows what they read (a Cargo.toml,
#      spec/seams/seam-register.json, HTML files, proof files) and otherwise
#      carry the observed reason; sustainabot stays the reserved slot. An
#      existing manifest is never rewritten: the owner's enrolment decisions
#      stand, and changing a manifest resets the grid's baseline.
#   2. `.gitbot-fleet/report-backs/README.md`, saying the directory is empty on
#      purpose (envelopes are produced per run, never committed).
#   3. A `fleet` job in `.github/workflows/fork-governance.yml`, inserted before
#      the `hypatia` job, which gains `needs: [fleet]` (and `if: !cancelled()`
#      when it had no `if:`). Under the hyperpolymath owner the job calls this
#      repository's reusable workflow (lib/fleet-job.reusable.yml); under any
#      other owner, where a cross-owner reusable fails at plan time with zero
#      jobs, the same steps go inline (lib/fleet-job.inline.yml). The pin is
#      FLEET_REF, or this checkout's origin/main — never a feature-branch HEAD
#      (the 2026-09-04 incident: a PR-intermediate commit propagated to 70
#      repositories before it had landed).
#   4. A check that the Hypatia lane the workflow pins actually READS the
#      artifact (its `report-backs-artifact` input). Producing an artifact no
#      lane downloads is unwired, and unwired is not done, so that state exits
#      3 with the bump named — the fixer never bumps a standards pin itself;
#      standards' scripts/propagate-workflow-pins.sh does, and this fixer is
#      meant to run in the same branch as that bump.
#
# Idempotent: a second run changes nothing and reports `ok` per item. Touches
# only the three paths above. Never edits actions.lock: when the repository
# carries one and a job was inserted, the lock needs `gh actions-lock` (or the
# repository's check-lock-sync.sh) and the fixer says so.
#
# Exit codes: 0 everything present and wired · 2 usage, refused, or nothing
# changed because an input was invalid · 3 files were written or checked but
# the repository is not yet wired end to end (each reason is printed as a
# MANUAL/UNVERIFIED line) — a dispatcher must not record 3 as a clean success.
#
# Usage:
#   fix-fleet-enrolment.sh <repo-path> [finding-json]
#
# Environment:
#   FLEET_REF          gitbot-fleet commit to pin (40 hex). Default: this
#                      checkout's refs/remotes/origin/main.
#   PANIC_ATTACK_SHA   panic-attack commit for the inline job. Default: the
#                      value in this repository's fleet-report-back-reusable.yml.
#   FLEET_JOB_FORM     auto (default) | inline | reusable.
#   STANDARDS_DIR      a standards checkout used to read the pinned Hypatia
#                      reusable; without it a remote pin is UNVERIFIED.
# The finding JSON (dispatch-runner's second argument) is accepted and ignored:
# the fixer is driven by the tree, not by the finding.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FLEET_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_PATH="${1:?Usage: fix-fleet-enrolment.sh <repo-path> [finding-json]}"

[ -d "$REPO_PATH" ] || { echo "fix-fleet-enrolment: not a directory: $REPO_PATH" >&2; exit 2; }
REPO_PATH="$(cd "$REPO_PATH" && pwd)"

MANIFEST="$REPO_PATH/.gitbot-fleet/enrolment.json"
RB_README="$REPO_PATH/.gitbot-fleet/report-backs/README.md"
WORKFLOW="$REPO_PATH/.github/workflows/fork-governance.yml"
LOCK="$REPO_PATH/.github/workflows/actions.lock"

rc=0
manual() { echo "  MANUAL: $*"; rc=3; }
unverified() { echo "  UNVERIFIED: $*"; rc=3; }

# ── inputs ───────────────────────────────────────────────────────────────────
fleet_ref="${FLEET_REF:-}"
if [ -z "$fleet_ref" ]; then
    fleet_ref=$(git -C "$FLEET_DIR" rev-parse --verify -q refs/remotes/origin/main 2>/dev/null || true)
    [ -n "$fleet_ref" ] || { echo "fix-fleet-enrolment: FLEET_REF is unset and this checkout has no origin/main to pin; nothing was changed" >&2; exit 2; }
fi
[[ "$fleet_ref" =~ ^[0-9a-f]{40}$ ]] || { echo "fix-fleet-enrolment: FLEET_REF must be a 40-hex commit, got '$fleet_ref'; nothing was changed" >&2; exit 2; }

panic_sha="${PANIC_ATTACK_SHA:-}"
if [ -z "$panic_sha" ]; then
    panic_sha=$(grep -m1 -oE 'PANIC_ATTACK_SHA: [0-9a-f]{40}' "$FLEET_DIR/.github/workflows/fleet-report-back-reusable.yml" 2>/dev/null | cut -d' ' -f2 || true)
    [ -n "$panic_sha" ] || { echo "fix-fleet-enrolment: PANIC_ATTACK_SHA is unset and the reusable workflow carries none; nothing was changed" >&2; exit 2; }
fi
[[ "$panic_sha" =~ ^[0-9a-f]{40}$ ]] || { echo "fix-fleet-enrolment: PANIC_ATTACK_SHA must be a 40-hex commit, got '$panic_sha'; nothing was changed" >&2; exit 2; }

form="${FLEET_JOB_FORM:-auto}"
case "$form" in
    auto|inline|reusable) ;;
    *) echo "fix-fleet-enrolment: FLEET_JOB_FORM must be auto, inline or reusable, got '$form'" >&2; exit 2 ;;
esac
if [ "$form" = auto ]; then
    origin=$(git -C "$REPO_PATH" remote get-url origin 2>/dev/null || true)
    # github.com/hyperpolymath/<repo> over https or ssh; anything else is another owner.
    if [[ "$origin" =~ github\.com[:/]hyperpolymath/ ]]; then form=reusable; else form=inline; fi
fi
JOB_TEMPLATE="$SCRIPT_DIR/lib/fleet-job.$form.yml"
[ -r "$JOB_TEMPLATE" ] || { echo "fix-fleet-enrolment: job template not readable: $JOB_TEMPLATE" >&2; exit 2; }

# One workflow-pin fixer per checkout at a time (see lib/pin-fixer-lock.sh).
# shellcheck source-path=SCRIPTDIR source=lib/pin-fixer-lock.sh
. "$SCRIPT_DIR/lib/pin-fixer-lock.sh"
pin_fixer_lock "$REPO_PATH"

echo "=== Fleet enrolment (report-backs) ==="
echo "  Repo: $REPO_PATH"
echo "  Fleet pin: $fleet_ref  job form: $form"

# ── tree facts the manifest is written from ──────────────────────────────────
# Vendored and generated trees are not this repository's surface.
tree_has() { # find-expression...
    [ -n "$(find "$REPO_PATH" \( -name .git -o -name node_modules -o -name target -o -name vendor -o -name .fleet-src \) -prune -o -type f \( "$@" \) -print -quit 2>/dev/null)" ]
}
has_rust=false; has_seams=false; has_html=false; has_proofs=false
tree_has -name Cargo.toml && has_rust=true
[ -f "$REPO_PATH/spec/seams/seam-register.json" ] && has_seams=true
tree_has -name '*.html' -o -name '*.htm' && has_html=true
if [ -d "$REPO_PATH/proofs" ] || tree_has -name '*.agda' -o -name '*.lean' -o -name '*.idr' -o -name '*.v'; then has_proofs=true; fi

# ── 1. manifest ──────────────────────────────────────────────────────────────
if [ -f "$MANIFEST" ]; then
    if jq -e '.reporters | type == "object"' "$MANIFEST" >/dev/null 2>&1; then
        echo "  ok: .gitbot-fleet/enrolment.json exists (left as the owner wrote it)"
    else
        manual ".gitbot-fleet/enrolment.json exists but does not parse as a manifest with a .reporters object; not overwritten"
    fi
else
    mkdir -p "$(dirname "$MANIFEST")"
    # Key-sorted, compact, one final LF: the I-JSON/JCS shape the estate's
    # JSON policy asks of every .json (standards 3-practice/JSON-POLICY.adoc).
    jq -nSc --argjson rust "$has_rust" --argjson seams "$has_seams" --argjson html "$has_html" --argjson proofs "$has_proofs" '
      {schema: 1,
       reporters: {
         rhodibot:     {title: "rhodibot (RSR compliance)", enrolled: true},
         finishingbot: {title: "finishingbot (release readiness)", enrolled: true},
         glambot:      {title: "glambot (presentation quality)", enrolled: true},
         cipherbot:    {title: "cipherbot (cryptographic hygiene)", enrolled: true},
         panicbot: (if $rust then {title: "panicbot (panic recovery, error handling)", enrolled: true}
                    else {enrolled: false, reason: "no Cargo.toml in the tree; panic-attack scans Rust"} end),
         seambot: (if $seams then {title: "seambot (ABI/FFI seam checks)", enrolled: true}
                   else {enrolled: false, reason: "no seam register is declared (spec/seams/seam-register.json)"} end),
         accessibilitybot: (if $html then {title: "accessibilitybot (WCAG conformance)", enrolled: true}
                            else {enrolled: false, reason: "no HTML files in the tree"} end),
         echidnabot: (if $proofs then {title: "echidnabot (formal verification)", enrolled: true}
                      else {enrolled: false, reason: "no proof files in the tree"} end),
         sustainabot: {enrolled: false, reason: "reserved fleet slot; no adapter exists"}}}' > "$MANIFEST"
    echo "  CREATED .gitbot-fleet/enrolment.json (rust=$has_rust seams=$has_seams html=$has_html proofs=$has_proofs; edit it — enrol what is true of this repository)"
fi

# ── 2. report-backs directory ────────────────────────────────────────────────
if [ -f "$RB_README" ]; then
    echo "  ok: .gitbot-fleet/report-backs/README.md exists"
else
    mkdir -p "$(dirname "$RB_README")"
    cat > "$RB_README" <<'EOF'
<!-- SPDX-License-Identifier: MPL-2.0 -->
# Fleet report-backs

Empty on purpose. The `fleet` job of `.github/workflows/fork-governance.yml`
writes one `<member>.json` envelope per enrolled gitbot-fleet member into an
artifact at run time, and the Hypatia lane downloads it here before rendering
the verdict grid. Nothing in this directory is committed: a committed envelope
would pose as evidence about a head it was not produced from, and the lane
clears the directory before the download for that reason.

Envelope contract: `docs/VERDICT-GRID-SPEC.adoc` in standards. Producer:
`docs/REPORT-BACK.adoc` in hyperpolymath/gitbot-fleet.
EOF
    echo "  CREATED .gitbot-fleet/report-backs/README.md"
fi

# ── 3. the fleet job ─────────────────────────────────────────────────────────
job_inserted=false
if [ ! -f "$WORKFLOW" ]; then
    manual "no .github/workflows/fork-governance.yml; adopt fork governance first (standards 3-practice/fork-governance/ADOPTION.adoc), then rerun"
elif grep -qE '^  fleet:[[:space:]]*$' "$WORKFLOW"; then
    echo "  ok: fork-governance.yml already has a fleet job"
elif ! grep -qE '^  hypatia:[[:space:]]*$' "$WORKFLOW"; then
    manual "fork-governance.yml has no 'hypatia:' job at two-space indent to insert the fleet job before; add the job by hand (docs/REPORT-BACK.adoc §5)"
else
    jobtmp=$(mktemp)
    # The template's SPDX/explanatory header lines are for the template file.
    sed -e '/^# /d' -e "s/__FLEET_REF__/$fleet_ref/g" -e "s/__PANIC_ATTACK_SHA__/$panic_sha/g" "$JOB_TEMPLATE" > "$jobtmp"
    if grep -q '__[A-Z_]*__' "$jobtmp"; then
        rm -f "$jobtmp"; echo "fix-fleet-enrolment: template $JOB_TEMPLATE left a placeholder unfilled; workflow not changed" >&2; exit 2
    fi
    out=$(mktemp)
    awk -v tmpl="$jobtmp" '
        BEGIN { while ((getline l < tmpl) > 0) job = job l "\n"; close(tmpl) }
        /^  hypatia:[[:space:]]*$/ && !done { printf "%s", job; done = 1 }
        { print }' "$WORKFLOW" > "$out"
    mv "$out" "$WORKFLOW"; rm -f "$jobtmp"
    job_inserted=true
    echo "  INSERTED fleet job ($form form) before the hypatia job in fork-governance.yml"
fi

# ── 3b. hypatia runs after the fleet job ─────────────────────────────────────
# The hypatia block: from its header to the next two-space job header.
hypatia_block() {
    awk '/^  hypatia:[[:space:]]*$/ {p=1; print; next} p && /^  [A-Za-z_][A-Za-z0-9_-]*:/ {exit} p {print}' "$WORKFLOW"
}
if [ -f "$WORKFLOW" ] && grep -qE '^  hypatia:[[:space:]]*$' "$WORKFLOW"; then
    block=$(hypatia_block)
    needs_line=$(printf '%s\n' "$block" | grep -m1 -E '^    needs:' || true)
    if [ -z "$needs_line" ]; then
        # No needs: add needs and, when the job has no if: of its own, the
        # !cancelled() guard (always() would let a superseded run post a stale grid).
        add='    needs: [fleet]'
        # shellcheck disable=SC2016 # the ${{ }} is GitHub's expression syntax, written literally
        printf '%s\n' "$block" | grep -qE '^    if:' || add="$add"$'\n''    if: ${{ !cancelled() }}'
        out=$(mktemp)
        awk -v add="$add" '{ print } /^  hypatia:[[:space:]]*$/ && !done { print add; done = 1 }' "$WORKFLOW" > "$out"
        mv "$out" "$WORKFLOW"
        echo "  WIRED hypatia: needs: [fleet]"
    elif printf '%s' "$needs_line" | grep -qE '\bfleet\b'; then
        echo "  ok: hypatia already needs fleet"
    elif printf '%s' "$needs_line" | grep -qE '^    needs:[[:space:]]*\[.*\][[:space:]]*$'; then
        out=$(mktemp)
        awk '!done && /^  hypatia:[[:space:]]*$/ {p=1} p && /^    needs:[[:space:]]*\[/ { sub(/\[[[:space:]]*\]/, "[fleet]"); if ($0 !~ /fleet\]/) sub(/\]/, ", fleet]"); done = 1; p = 0 } { print }' "$WORKFLOW" > "$out"
        mv "$out" "$WORKFLOW"
        echo "  WIRED hypatia: fleet added to its needs list"
    else
        manual "hypatia's needs: is a block list; add '- fleet' to it by hand"
    fi
    if printf '%s\n' "$block" | grep -qE '^    if:.*always\(\)'; then
        echo "  NOTE: hypatia runs under always(); with cancel-in-progress a superseded run can post a stale grid — prefer \${{ !cancelled() }}"
    fi
fi

# A job inserted into a workflow with a lock needs the lock regenerated; the
# fixer never edits actions.lock (tool-owned).
if [ "$job_inserted" = true ] && [ -f "$LOCK" ]; then
    manual "actions.lock exists; run 'gh actions-lock' (or the repository's check-lock-sync.sh) so the new uses: entries are locked"
fi
if [ "$job_inserted" = true ] && command -v yq >/dev/null 2>&1; then
    if ! yq -e '.jobs.fleet' "$WORKFLOW" >/dev/null 2>&1; then
        manual "fork-governance.yml does not parse with a fleet job after the edit; inspect it"
    fi
fi

# ── 4. does the pinned Hypatia lane read the artifact? ───────────────────────
if [ -f "$WORKFLOW" ]; then
    uses=$(hypatia_block | grep -m1 -E '^    uses:' | sed -E 's/^    uses:[[:space:]]*//; s/[[:space:]]+#.*$//' || true)
    case "$uses" in
        '')
            manual "hypatia job has no uses: line; cannot tell whether the lane reads the fleet-report-backs artifact" ;;
        ./*)
            local_reusable="$REPO_PATH/.github/workflows/${uses#./.github/workflows/}"
            if [ -f "$local_reusable" ] && grep -q 'report-backs-artifact' "$local_reusable"; then
                echo "  ok: local Hypatia reusable reads the fleet-report-backs artifact"
            else
                manual "local Hypatia reusable ($uses) has no report-backs-artifact input; the artifact is produced but never read"
            fi ;;
        */standards/.github/workflows/hypatia-scan-reusable.yml@*)
            sha="${uses##*@}"
            if [ -n "${STANDARDS_DIR:-}" ] && git -C "$STANDARDS_DIR" cat-file -e "$sha:.github/workflows/hypatia-scan-reusable.yml" 2>/dev/null; then
                if git -C "$STANDARDS_DIR" cat-file -p "$sha:.github/workflows/hypatia-scan-reusable.yml" | grep -q 'report-backs-artifact'; then
                    echo "  ok: standards pin ${sha:0:12} reads the fleet-report-backs artifact"
                else
                    manual "standards pin ${sha:0:12} does not read the artifact; bump the standards pins (standards scripts/propagate-workflow-pins.sh --fix --to <sha that carries report-backs-artifact>) in this same branch"
                fi
            else
                unverified "standards pin ${sha:0:12}: set STANDARDS_DIR to a standards checkout holding that commit to confirm its Hypatia reusable reads the artifact"
            fi ;;
        *)
            manual "hypatia job uses '$uses', which this fixer cannot read; confirm by hand that the lane takes report-backs-artifact" ;;
    esac
fi

echo
if [ "$rc" -eq 0 ]; then echo "Fleet enrolment: wired end to end"; else echo "Fleet enrolment: not yet wired end to end (see MANUAL/UNVERIFIED lines)"; fi
exit "$rc"
