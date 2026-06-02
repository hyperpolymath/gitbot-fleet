#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
#
# fix-codeql-cron-monthly.sh — convert codeql.yml cron to canonical monthly
#
# Driven by hypatia WF025 (codeql-cron-monthly recipe). Per standards#286
# (Option B 2026-05-30 estate decision): CodeQL schedule monthly canonical
# `0 6 1 * *`. PR-trigger runs unchanged.
#
# Handles both #288-shape (weekly `0 6 * * 1` → monthly) and #324-shape
# (any non-canonical cron → monthly).
#
# Idempotent: skips files already on canonical monthly.

set -euo pipefail

REPO_PATH="$1"
FINDING_FILE="${2:-}"

cd "$REPO_PATH"

F=".github/workflows/codeql.yml"
if [[ ! -f "$F" ]]; then
  echo "fix-codeql-cron-monthly: no $F in $REPO_PATH"
  exit 0
fi

if grep -qF "cron: '0 6 1 * *'" "$F"; then
  echo "fix-codeql-cron-monthly: already canonical monthly: $F"
  exit 0
fi

python3 - <<PY
import re, sys
p = "$F"
content = open(p).read()
new, n = re.subn(
    r"(^[ \t]*-[ \t]*cron:[ \t]*)'[^']+'(.*)",
    r"\1'0 6 1 * *'   # monthly 1st 06:00 UTC",
    content,
    count=1,
    flags=re.MULTILINE,
)
if n == 0:
    print("fix-codeql-cron-monthly: no cron line found in", p)
    sys.exit(0)
open(p, "w").write(new)
print(f"fix-codeql-cron-monthly: 1 cron line replaced in", p)
PY

exit 0
