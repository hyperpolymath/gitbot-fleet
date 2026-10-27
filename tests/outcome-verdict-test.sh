#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Fix outcomes are verdicts from each recipe's own predicate, not exit codes
# (experiment E1, docs/experiments/E1-fix-outcome-replay-2026-10-20.adoc).
#
# Covers:
#   1. outcome_verdict truth table.
#   2. recipe_check_count: integer from a --check script, NA without one,
#      and NA when the predicate itself fails (never a false 0).
#   3. The two predicate-carrying fixers, end to end on a fixture repo:
#      targets > 0 before, 0 after, --check itself changes nothing, and a
#      second run is a no_op.
#   4. dispatch-runner's record_outcome row and rescan trigger (extracted,
#      like hypatia-record-outcome-smoke.sh, so the quarantined main loop
#      never runs).
#   5. Deprecated recipe ids report under their replacement.
#   6. Two parallel jobs on one repo record one fix, not two (repo_lock).
#   7. The E1 replay (audit-fix-outcomes.sh) counts as the fixers do, and
#      reads NA when git grep cannot run.
#   8. Run counters from background jobs reach the parent (--parallel).

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

# shellcheck source=../scripts/lib/outcome-verdict.sh
source "$ROOT/scripts/lib/outcome-verdict.sh"

pass=0
fail=0
check() {
    local label="$1" expected="$2" actual="$3"
    if [[ "$expected" == "$actual" ]]; then
        pass=$((pass + 1))
        echo "PASS: $label"
    else
        fail=$((fail + 1))
        echo "FAIL: $label — expected '$expected', got '$actual'"
    fi
}

# --- 1. truth table -------------------------------------------------------
check "exit 0, work found, none left"      verified   "$(outcome_verdict 0 3 0)"
check "exit 0, nothing to fix"             no_op      "$(outcome_verdict 0 0 0)"
check "exit 0, work remains"               incomplete "$(outcome_verdict 0 3 1)"
check "exit 0, targets appear (0 -> n)"    incomplete "$(outcome_verdict 0 0 2)"
check "exit 0, no predicate"               unverified "$(outcome_verdict 0 NA NA)"
check "exit 0, predicate lost after fix"   unverified "$(outcome_verdict 0 3 NA)"
check "non-zero exit wins over counts"     failed     "$(outcome_verdict 1 3 0)"
check "legacy outcome of a failure"        failure    "$(legacy_outcome failed)"
check "legacy outcome of a no-op"          success    "$(legacy_outcome no_op)"

# --- 2. recipe_check_count ------------------------------------------------
cat > "$TMP/no-check.sh" <<'EOF'
#!/usr/bin/env bash
echo "=== fixer without a predicate ==="
EOF
chmod +x "$TMP/no-check.sh"
mkdir -p "$TMP/empty"
check "script without --check is NA" NA "$(recipe_check_count "$TMP/no-check.sh" "$TMP/empty")"

# A fixer that has not opted in must never be run with --check: it would take
# "--check" as its repo path and could run its edits.
cat > "$TMP/unmarked.sh" <<EOF
#!/usr/bin/env bash
touch "$TMP/unmarked-was-run"
echo 0
EOF
chmod +x "$TMP/unmarked.sh"
check "unmarked fixer is NA" NA "$(recipe_check_count "$TMP/unmarked.sh" "$TMP/empty")"
check "unmarked fixer is never invoked" absent "$([[ -e "$TMP/unmarked-was-run" ]] && echo present || echo absent)"
check "missing repo path is NA" NA \
    "$(recipe_check_count "$ROOT/scripts/fix-shell-quoting.sh" "$TMP/does-not-exist")"

# A predicate that could not run must not report 0: after a fix, 0 reads as
# verified. Shims stand in for a failing find and a grep without PCRE.
mkdir -p "$TMP/shim-find" "$TMP/shim-grep" "$TMP/hits"
printf '#!/bin/sh\nexit 1\n' > "$TMP/shim-find/find"
printf '#!/bin/sh\nexit 2\n' > "$TMP/shim-grep/grep"
chmod +x "$TMP/shim-find/find" "$TMP/shim-grep/grep"
printf 'cd $HOME\n' > "$TMP/hits/a.sh"
printf 'f = believe_me 1\n' > "$TMP/hits/A.idr"
for script in fix-shell-quoting.sh fix-believe-me.sh; do
    check "$script: find failure is NA" NA \
        "$(PATH="$TMP/shim-find:$PATH" recipe_check_count "$ROOT/scripts/$script" "$TMP/hits")"
done
rc=0
out=$(PATH="$TMP/shim-grep:$PATH" "$ROOT/scripts/fix-shell-quoting.sh" --check "$TMP/hits" 2>/dev/null) || rc=$?
check "grep error fails --check, prints no count" "2:" "$rc:$out"

# --- 3. fixers end to end -------------------------------------------------
repo="$TMP/repo"
mkdir -p "$repo/src" "$repo/node_modules/dep"
echo '{}' > "$TMP/finding.json"
printf '#!/bin/sh\ncd $HOME\nrm $TMPDIR && echo $X\nmv "$A" b\n' > "$repo/run.sh"
printf 'cd $VENDORED\n' > "$repo/node_modules/dep/vendored.sh"
printf 'module M\nf : Int\nf = believe_me 1\n-- believe_me in a comment\ng = believe_me 2\n' \
    > "$repo/src/A.idr"

for spec in "fix-shell-quoting.sh 3" "fix-believe-me.sh 2"; do
    read -r script expected_before <<< "$spec"
    fixer="$ROOT/scripts/$script"
    snapshot=$(find "$repo" -type f -exec sha256sum {} + | sort)
    before=$(recipe_check_count "$fixer" "$repo")
    check "$script: --check changes nothing" "$snapshot" "$(find "$repo" -type f -exec sha256sum {} + | sort)"
    check "$script: targets before" "$expected_before" "$before"

    rc=0
    "$fixer" "$repo" "$TMP/finding.json" > /dev/null 2>&1 || rc=$?
    after=$(recipe_check_count "$fixer" "$repo")
    check "$script: first run verified" verified "$(outcome_verdict "$rc" "$before" "$after")"

    rc=0
    "$fixer" "$repo" "$TMP/finding.json" > /dev/null 2>&1 || rc=$?
    check "$script: second run is a no-op" no_op \
        "$(outcome_verdict "$rc" "$after" "$(recipe_check_count "$fixer" "$repo")")"
done
check "vendored file left alone" 'cd $VENDORED' "$(cat "$repo/node_modules/dep/vendored.sh")"

# --- 4. dispatch-runner record_outcome ------------------------------------
awk '
  /^# --- Hypatia closed-loop contract ---/ { capture = 1 }
  capture { print }
  /^record_outcome\(\)/ && capture { seen = 1 }
  seen && /^}/ { exit }
' "$ROOT/scripts/dispatch-runner.sh" > "$TMP/extracted.sh"
export REPOS_BASE="$TMP/repos" HYPATIA_OUTCOME_REPORT=off
OUTCOME_FILE="$TMP/outcomes.jsonl"
export OUTCOME_FILE_CENTRAL="$OUTCOME_FILE"  # read by the sourced record_outcome
RESCAN_REPOS_FILE="$TMP/rescan"
: > "$RESCAN_REPOS_FILE"
# shellcheck disable=SC1091
source "$TMP/extracted.sh"

record_outcome "PA009" "recipe-shell-quote-vars" "repo-a" "" verified 0 3 0
record_outcome "PA009" "recipe-shell-quote-vars" "repo-b" "" no_op 0 0 0
record_outcome "PA009" "recipe-x" "repo-c" "" unverified 0
record_outcome "PA009" "recipe-license-hygiene" "repo-d" "" refused_license_policy

check "verified row" '{"verdict":"verified","outcome":"success","exit_code":0,"before_hits":3,"after_hits":0}' \
    "$(sed -n 1p "$OUTCOME_FILE" | jq -c '{verdict, outcome, exit_code, before_hits, after_hits}')"
check "unverified row has no counts" '{"verdict":"unverified","before_hits":null,"after_hits":null}' \
    "$(sed -n 3p "$OUTCOME_FILE" | jq -c '{verdict, before_hits, after_hits}')"
check "refusal keeps its own outcome" '{"verdict":"refused_license_policy","outcome":"refused_license_policy","exit_code":null}' \
    "$(sed -n 4p "$OUTCOME_FILE" | jq -c '{verdict, outcome, exit_code}')"
check "only verified repos trigger a rescan" "repo-a" "$(cat "$RESCAN_REPOS_FILE")"

# --- 5. deprecated recipe ids ---------------------------------------------
registry="$ROOT/scripts/fix-script-registry.json"
check "deprecated id reports as its replacement" recipe-annotate-believe-me \
    "$(canonical_recipe_id "$registry" recipe-remove-believe-me)"
check "canonical id unchanged" recipe-annotate-believe-me \
    "$(canonical_recipe_id "$registry" recipe-annotate-believe-me)"
check "unknown id unchanged" recipe-x "$(canonical_recipe_id "$registry" recipe-x)"
check "no registry, id unchanged" recipe-x "$(canonical_recipe_id "$TMP/none.json" recipe-x)"

# --- 6. parallel jobs on one repo -----------------------------------------
# Without repo_lock both jobs count 3 before either fix and both record
# verified for one fix (measured: "verified verified"). With it, the second
# job counts what the first left.
par="$TMP/par"
mkdir -p "$par" "$TMP/locks"
printf 'cd $A\nrm $B\necho $C\n' > "$par/x.sh"
fixer="$ROOT/scripts/fix-shell-quoting.sh"
job() {
    repo_lock "$TMP/locks" "$par"
    local b a
    b=$(recipe_check_count "$fixer" "$par")
    sleep 0.3
    "$fixer" "$par" "$TMP/finding.json" > /dev/null 2>&1
    a=$(recipe_check_count "$fixer" "$par")
    repo_unlock
    outcome_verdict 0 "$b" "$a" > "$TMP/job$1"
}
job 1 & job 2 & wait
check "two jobs on one repo record one fix" "no_op verified" \
    "$(sort "$TMP/job1" "$TMP/job2" | paste -sd' ')"

# --- 7. the E1 replay agrees with the fixers ------------------------------
# A root-level vendor/ (and .pack/ for Idris) is excluded as the fixers'
# find excludes it, and a git grep that cannot run reads NA, never 0.
ar="$TMP/audit/ar"
mkdir -p "$ar/vendor" "$ar/.pack" "$TMP/shim-git"
printf 'cd $A\n' > "$ar/vendor/a.sh"
printf 'f = believe_me 1\n' > "$ar/A.idr"
printf 'g = believe_me 2\n' > "$ar/.pack/B.idr"
git -C "$ar" init -q -b main
git -C "$ar" add -A
GIT_COMMITTER_DATE=2026-01-01T00:00:00Z git -C "$ar" -c user.email=t@t -c user.name=t \
    commit -q --date=2026-01-01T00:00:00Z -m fixture
git -C "$ar" update-ref refs/remotes/origin/main HEAD
printf '%s\n' \
    '{"repo":"ar","pattern":"recipe-shell-quote-vars","outcome":"success","fixed_at":"2026-02-01T00:00:00Z"}' \
    '{"repo":"ar","pattern_id":"PA004","recipe_id":"recipe-annotate-believe-me","outcome":"success","timestamp":"2026-02-01T00:00:00Z"}' \
    '{"repo":"ar","pattern":"recipe-remove-believe-me","file":"A.idr","outcome":"success","fixed_at":"2026-02-01T00:00:00Z"}' \
    > "$TMP/audit/log.jsonl"
audit() { "$ROOT/scripts/audit-fix-outcomes.sh" "$TMP/audit/log.jsonl" "$ar" 2>/dev/null | awk -F'\t' 'NR > 1 { print $2 "=" $6 }' | sort | paste -sd' '; }
check "replay counts as the fixers do" \
    "recipe-annotate-believe-me=$(recipe_check_count "$ROOT/scripts/fix-believe-me.sh" "$ar") recipe-shell-quote-vars=$(recipe_check_count "$fixer" "$ar")" \
    "$(audit)"
printf '#!/bin/sh\nfor a; do [ "$a" = grep ] && exit 128; done\nexec %s "$@"\n' "$(command -v git)" > "$TMP/shim-git/git"
chmod +x "$TMP/shim-git/git"
check "replay predicate failure is NA" "recipe-annotate-believe-me=NA recipe-shell-quote-vars=NA" \
    "$(PATH="$TMP/shim-git:$PATH" audit)"
# A row naming one file is a narrower claim than a repo-wide predicate: it is
# skipped (above, no recipe-remove-believe-me row) and the skip is reported.
check "file-scoped row is skipped, and reported" "audit: ar: 1 file-scoped row(s) skipped" \
    "$("$ROOT/scripts/audit-fix-outcomes.sh" "$TMP/audit/log.jsonl" "$ar" 2>&1 >/dev/null)"

# --- 8. run counters survive --parallel -----------------------------------
# execute_entry runs in a background subshell under --parallel; its counts
# reach the parent through the tally file (extracted, as in section 4).
awk '/^# --- Run counters ---/,/^# --- end run counters ---/' "$ROOT/scripts/dispatch-runner.sh" > "$TMP/counters.sh"
# shellcheck disable=SC1091
source "$TMP/counters.sh"
PARALLEL_TMPDIR="$TMP/par-counters"
mkdir -p "$PARALLEL_TMPDIR"
EXECUTED=0 SUCCEEDED=0 FAILED=0 SKIPPED=0
job_counts() { count EXECUTED; count "$1"; }
IN_JOB=1 job_counts FAILED & IN_JOB=1 job_counts SUCCEEDED & IN_JOB=1 job_counts FAILED & wait
count SKIPPED
fold_counts
check "background counts reach the parent" "EXECUTED=3 SUCCEEDED=1 FAILED=2 SKIPPED=1" \
    "EXECUTED=$EXECUTED SUCCEEDED=$SUCCEEDED FAILED=$FAILED SKIPPED=$SKIPPED"

echo ""
echo "Results: $pass passed, $fail failed"
[[ "$fail" -eq 0 ]]
