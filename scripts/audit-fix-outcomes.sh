#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# audit-fix-outcomes.sh — replay fix-outcomes rows against git history (E1).
#
# For every row of a fix-outcomes log whose repo is checked out locally, this
# evaluates the recipe's own detection predicate on the tree at three commits:
# the one current at fixed_at, the one at fixed_at + 7 days (time for a fix PR
# to land), and origin/main HEAD. It writes TSV to stdout and changes nothing.
# Report: docs/experiments/E1-fix-outcome-replay-2026-10-20.adoc.
#
# Usage: audit-fix-outcomes.sh <fix-outcomes.jsonl> <repo-checkout>...
#
# The log in shared-context/learning/ is a Git LFS pointer; fetch the object
# into a scratch path first rather than over the committed pointer.
# The checkouts need full history (git fetch --unshallow).
#
# Predicates mirror the fixers' own (their --check modes, applied with
# `git grep` to a revision instead of a working tree). Recipes without one
# print NA.
set -euo pipefail
LOG=$1; shift
# The fixers' own exclusion list (lib/third-party-excludes.sh), as glob
# pathspecs: with `glob` magic `**/` also matches no directory, so a
# root-level vendor/ is excluded exactly as the fixers' find excludes it.
# shellcheck source=lib/third-party-excludes.sh
source "$(dirname "${BASH_SOURCE[0]}")/lib/third-party-excludes.sh"
EX=()
for d in "${THIRD_PARTY_SKIP_DIRS[@]}"; do EX+=(":(exclude,glob)**/$d/**"); done
IDR_EX=("${EX[@]}" ':(exclude,glob)**/.pack/**') # fix-believe-me.sh also skips .pack
# grep_hits <repo> <git-grep args...>: the git grep output, or exit 1 when
# git grep itself failed (status > 1: no PCRE, unreadable revision). Status 1
# is "no match", which is a count of 0, not an error.
grep_hits() {
  local repo=$1 rc=0 out; shift
  out=$(git -C "$repo" grep "$@" 2>/dev/null) || rc=$?
  ((rc <= 1)) || return 1
  printf '%s\n' "$out"
}
sum_counts() { awk -F: 'NF { s += $NF } END { print s + 0 }'; }
pred() { # repo rev pattern -> count of remaining hits, or NA
  local repo=$1 rev=$2 pat=$3 out
  case $pat in
    recipe-shell-quote-vars)
      out=$(grep_hits "$repo" -P -h -c '\b(cd|cp|mv|rm|mkdir|rmdir|chmod|chown|echo) \$[A-Z_][A-Z0-9_]*' "$rev" -- '*.sh' '*.bash' "${EX[@]}") || { echo NA; return; }
      sum_counts <<< "$out";;
    # The removal recipe claimed to remove believe_me, so an annotated call
    # still counts against it. The annotate recipe is measured as its fixer's
    # --check measures it: a call is discharged by a PROOF_TODO line above.
    recipe-remove-believe-me)
      out=$(grep_hits "$repo" -P -h -c '^(?!\s*--).*believe_me' "$rev" -- '*.idr' "${IDR_EX[@]}") || { echo NA; return; }
      sum_counts <<< "$out";;
    recipe-annotate-believe-me)
      out=$(grep_hits "$repo" -h -B1 -e believe_me "$rev" -- '*.idr' "${IDR_EX[@]}") || { echo NA; return; }
      awk '/^--$/ { prev = ""; next } !/^[[:space:]]*--/ && /believe_me/ && prev !~ /PROOF_TODO/ { n++ } { prev = $0 } END { print n + 0 }' <<< "$out";;
    *) echo NA;;
  esac
}
printf 'repo\tpattern\toutcome\tfixed_at\trev_at\thits_at\thits_plus7d\thits_head\n'
for repo in "$@"; do
  name=$(basename "$repo"); head=$(git -C "$repo" rev-parse origin/main)
  jq -r --arg r "$name" 'select(.repo==$r) | [.pattern // .recipe_id // .pattern_id, .outcome, .fixed_at // .timestamp] | select(.[2] != null) | @tsv' "$LOG" | sort -u |
  while IFS=$'\t' read -r pat out ts; do
    at=$(git -C "$repo" rev-list -1 --before="$ts" origin/main || true)
    p7=$(git -C "$repo" rev-list -1 --before="$(date -u -d "$ts + 7 days" +%FT%TZ)" origin/main || true)
    [ -z "$at" ] && { printf '%s\t%s\t%s\t%s\t-\tpre-history\t-\t%s\n' "$name" "$pat" "$out" "$ts" "$(pred "$repo" "$head" "$pat")"; continue; }
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$name" "$pat" "$out" "$ts" "${at:0:9}" "$(pred "$repo" "$at" "$pat")" "$(pred "$repo" "$p7" "$pat")" "$(pred "$repo" "$head" "$pat")"
  done
done
