#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
#
# fix-chapel-manpath-guard.sh — guard against unset MANPATH on Chapel install
#
# Driven by hypatia WF024 (chapel-manpath-guard recipe). One of the 5 sharp
# edges in panic-attack wiki Chapel-Metalayer.md: the chpl install step
# references $MANPATH unconditionally; unset MANPATH explodes under `set -u`.
#
# Inserts `: \${MANPATH:=}` BEFORE the chapel install runs.
#
# Idempotent.

set -euo pipefail

REPO_PATH="$1"
FINDING_FILE="${2:-}"

cd "$REPO_PATH"

CHANGED=0
for f in $(find .github/workflows -name '*.yml' -o -name '*.yaml' 2>/dev/null); do
  if ! grep -qE 'CHAPEL_DEB_URL|chapel-[0-9]+\.[0-9]+' "$f"; then
    continue
  fi
  if grep -qF ': ${MANPATH:=}' "$f"; then
    continue   # already guarded
  fi
  # Insert guard before the first `chpl --version` or chapel install line
  if grep -qE 'chpl --version|sudo apt-get install.*chapel' "$f"; then
    sed -i -E '0,/(chpl --version|sudo apt-get install.*chapel)/{s|^([[:space:]]*)(chpl --version\|sudo apt-get install.*chapel)|\1: ${MANPATH:=}\n\1\2|}' "$f"
    echo "  MANPATH guard inserted: $f"
    CHANGED=$((CHANGED + 1))
  fi
done

echo "fix-chapel-manpath-guard: $CHANGED change(s) applied"
exit 0
