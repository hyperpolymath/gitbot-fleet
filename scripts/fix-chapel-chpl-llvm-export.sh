#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
#
# fix-chapel-chpl-llvm-export.sh — ensure CHPL_LLVM is exported on Chapel jobs
#
# Driven by hypatia WF024 (chapel-chpl-llvm-export recipe). Sharp edge #2 in
# panic-attack wiki Chapel-Metalayer.md: CHPL_LLVM is required by `chpl`'s
# diagnostic + build steps; unset means the toolchain falls back to a different
# code path that doesn't link against libllvm14 (the .deb's only supported
# binding).
#
# Inserts `export CHPL_LLVM=bundled` (the only supported value for the
# precompiled .deb path) into the install step.
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
  if grep -qE 'CHPL_LLVM[[:space:]]*=' "$f"; then
    continue   # already exported / set
  fi
  if grep -qE 'chpl --version|sudo apt-get install.*chapel' "$f"; then
    sed -i -E '0,/(chpl --version|sudo apt-get install.*chapel)/{s|^([[:space:]]*)(chpl --version\|sudo apt-get install.*chapel)|\1export CHPL_LLVM=bundled\n\1\2|}' "$f"
    echo "  CHPL_LLVM=bundled exported: $f"
    CHANGED=$((CHANGED + 1))
  fi
done

echo "fix-chapel-chpl-llvm-export: $CHANGED change(s) applied"
exit 0
