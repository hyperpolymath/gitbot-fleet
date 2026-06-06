#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
#
# fix-chapel-replace-chpl-about.sh — replace dropped `chpl --about` with
# the 2.8-compatible equivalent.
#
# Driven by hypatia WF024 (chapel-replace-chpl-about-with-version recipe).
# Sharp edge #3 in panic-attack wiki Chapel-Metalayer.md: `chpl --about` was
# removed in Chapel 2.8 — workflows that rely on it for diagnostic output
# break silently (no `--about` flag is a no-op + exits 0 on some versions).
#
# Replace with `chpl --version && printchplenv` which gives the same
# information across the 2.7/2.8 split.
#
# Idempotent.

set -euo pipefail

REPO_PATH="$1"
FINDING_FILE="${2:-}"

cd "$REPO_PATH"

CHANGED=0
for f in $(find .github/workflows -name '*.yml' -o -name '*.yaml' 2>/dev/null); do
  if ! grep -qF 'chpl --about' "$f"; then
    continue
  fi
  sed -i 's|chpl --about|chpl --version \&\& printchplenv|g' "$f"
  echo "  chpl --about → chpl --version && printchplenv: $f"
  CHANGED=$((CHANGED + 1))
done

echo "fix-chapel-replace-chpl-about: $CHANGED change(s) applied"
exit 0
