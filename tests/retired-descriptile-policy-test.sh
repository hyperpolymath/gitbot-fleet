#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TEMP_DIR=$(mktemp -d)
trap 'rm -rf "$TEMP_DIR"' EXIT
mkdir -p "$TEMP_DIR/repo/.github/workflows" "$TEMP_DIR/repo/.machine_readable/descriptiles"
printf '%s\n' 'test -f .machine_readable/STATE.a2ml' > "$TEMP_DIR/repo/.github/workflows/check.yml"
printf '%s\n' '{"file":".github/workflows/check.yml"}' > "$TEMP_DIR/finding.json"
FIXER="$ROOT/scripts/fix-retired-descriptile-policy.sh"
# Missing canonical data must not be papered over with a reference rewrite.
if bash "$FIXER" "$TEMP_DIR/repo" "$TEMP_DIR/finding.json"; then
  echo 'FAIL: repaired policy without its canonical target' >&2; exit 1
fi
grep -Fxq 'test -f .machine_readable/STATE.a2ml' "$TEMP_DIR/repo/.github/workflows/check.yml"
printf '%s\n' '[metadata]' 'name = "fixture"' > "$TEMP_DIR/repo/.machine_readable/descriptiles/STATE.a2ml"
bash "$FIXER" "$TEMP_DIR/repo" "$TEMP_DIR/finding.json"
grep -Fxq 'test -f .machine_readable/descriptiles/STATE.a2ml' "$TEMP_DIR/repo/.github/workflows/check.yml"
bash "$FIXER" "$TEMP_DIR/repo" "$TEMP_DIR/finding.json"
printf '%s\n' '{"file":"../outside"}' > "$TEMP_DIR/finding.json"
if bash "$FIXER" "$TEMP_DIR/repo" "$TEMP_DIR/finding.json"; then
  echo 'FAIL: accepted path traversal' >&2; exit 1
fi
printf '%s\n' '{"file":".github/workflows/link.yml"}' > "$TEMP_DIR/finding.json"
ln -s check.yml "$TEMP_DIR/repo/.github/workflows/link.yml"
if bash "$FIXER" "$TEMP_DIR/repo" "$TEMP_DIR/finding.json"; then
  echo 'FAIL: accepted a symlink' >&2; exit 1
fi
echo 'PASS: canonical prerequisite, repair, idempotence, traversal and symlink rejection'
