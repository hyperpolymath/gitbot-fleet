#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# outcome-verdict.sh — decide a fix outcome from evidence, not from exit codes.
#
# A fix script that exits 0 has only shown that it did not crash: experiment E1
# (docs/experiments/E1-fix-outcome-replay-2026-10-20.adoc) found 0 of 130
# checkable "success" rows confirmed by a rescan. The verdict instead compares
# the recipe's own detection predicate before and after the fix.
#
# Predicate contract: a fix script that supports verification answers
#     <fix-script> --check <repo-path>
# by printing one non-negative integer (targets it would still fix) and
# changing nothing. The fixer and its checker are the same file, so they
# cannot drift apart. A script opts in with the header line
#     # outcome-predicate: --check
# and only such scripts are ever called with --check: a fixer without the
# mode would read "--check" as its repo path and might run its edits.
#
# Verdicts:
#   verified    work found before, none after
#   no_op       nothing to fix before the run
#   incomplete  work remains after the run
#   unverified  the script has no predicate (no evidence either way)
#   failed      the fix script exited non-zero
#
# Source this file; it defines functions only.

# Count remaining targets for one recipe. Prints the count, or "NA" when the
# script has no --check mode or its answer is not a plain integer.
recipe_check_count() {
    local fix_path="$1" repo_path="$2" out
    if grep -qx '# outcome-predicate: --check' "$fix_path" 2>/dev/null &&
        out=$("$fix_path" --check "$repo_path" 2>/dev/null) && [[ "$out" =~ ^[0-9]+$ ]]; then
        printf '%s\n' "$out"
    else
        printf 'NA\n'
    fi
}

# outcome_verdict <exit_code> <before> <after>  ->  verdict on stdout
outcome_verdict() {
    local rc="$1" before="$2" after="$3"
    if [[ "$rc" != 0 ]]; then
        echo failed
    elif [[ "$before" == NA || "$after" == NA ]]; then
        echo unverified
    elif ((before == 0)); then
        echo no_op
    elif ((after == 0)); then
        echo verified
    else
        echo incomplete
    fi
}

# Legacy `outcome` value for consumers that predate `verdict`. It reports the
# exit status only; read `verdict` for whether anything was fixed.
legacy_outcome() {
    if [[ "$1" == failed ]]; then echo failure; else echo success; fi
}

# canonical_recipe_id <registry.json> <recipe-id>  ->  the id outcomes are
# reported under. A deprecated id (registry `deprecated_recipe_ids`) maps to
# its replacement, so a verified annotation is never credited to the removal
# recipe it was renamed from. Unknown ids, or no registry, pass through.
canonical_recipe_id() {
    local registry="$1" rid="$2" out
    if [[ -f "$registry" ]] &&
        out=$(jq -r --arg r "$rid" '.deprecated_recipe_ids[$r] // $r' "$registry" 2>/dev/null) &&
        [[ -n "$out" ]]; then
        printf '%s\n' "$out"
    else
        printf '%s\n' "$rid"
    fi
}

# repo_lock <lock-dir> <repo-path> / repo_unlock: serialise check/fix/check on
# one repo. Two parallel jobs for the same repo would otherwise both count
# targets before either fix, both count zero after, and both record a
# verified fix that happened once. The lock is released by closing its fd.
repo_lock() {
    local key
    key=$(printf '%s' "$2" | sha256sum | cut -c1-16)
    exec {REPO_LOCK_FD}> "$1/repo-$key.lock"
    flock "$REPO_LOCK_FD"
}

repo_unlock() {
    exec {REPO_LOCK_FD}>&-
}
