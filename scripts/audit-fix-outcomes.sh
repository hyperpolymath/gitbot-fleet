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
EX=(':!:**/.git/**' ':!:**/target/**' ':!:**/node_modules/**' ':!:**/_build/**' ':!:**/.lake/**' ':!:**/deps/**' ':!:**/vendor/**' ':!:**/dist/**' ':!:**/build/**' ':!:**/winget-pkgs/**')
pred() { # repo rev pattern -> count of remaining hits
  local repo=$1 rev=$2 pat=$3
  case $pat in
    recipe-shell-quote-vars) git -C "$repo" grep -P -h -c '\b(cd|cp|mv|rm|mkdir|rmdir|chmod|chown|echo) \$[A-Z_][A-Z0-9_]*' "$rev" -- '*.sh' '*.bash' "${EX[@]}" 2>/dev/null | awk -F: '{s+=$NF} END{print s+0}';;
    recipe-remove-believe-me) git -C "$repo" grep -P -h -c '^(?!\s*--).*believe_me' "$rev" -- '*.idr' "${EX[@]}" 2>/dev/null | awk -F: '{s+=$NF} END{print s+0}';;
    *) echo NA;;
  esac
}
printf 'repo\tpattern\toutcome\tfixed_at\trev_at\thits_at\thits_plus7d\thits_head\n'
for repo in "$@"; do
  name=$(basename "$repo"); head=$(git -C "$repo" rev-parse origin/main)
  jq -r --arg r "$name" 'select(.repo==$r) | [.pattern,.outcome,.fixed_at] | @tsv' "$LOG" | sort -u |
  while IFS=$'\t' read -r pat out ts; do
    at=$(git -C "$repo" rev-list -1 --before="$ts" origin/main || true)
    p7=$(git -C "$repo" rev-list -1 --before="$(date -u -d "$ts + 7 days" +%FT%TZ)" origin/main || true)
    [ -z "$at" ] && { printf '%s\t%s\t%s\t%s\t-\tpre-history\t-\t%s\n' "$name" "$pat" "$out" "$ts" "$(pred "$repo" "$head" "$pat")"; continue; }
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$name" "$pat" "$out" "$ts" "${at:0:9}" "$(pred "$repo" "$at" "$pat")" "$(pred "$repo" "$p7" "$pat")" "$(pred "$repo" "$head" "$pat")"
  done
done
