#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0

set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SCRIPT="$ROOT/scripts/fix-session-detritus.sh"
REGISTRY="$ROOT/scripts/fix-script-registry.json"
TMP=$(mktemp -d /tmp/session-detritus-smoke-XXXXXX)
trap 'rm -rf "$TMP"' EXIT

REPO="$TMP/repo"
mkdir -p "$REPO/.machine_readable" "$REPO/spec/sub" "$REPO/spec/examples" \
    "$REPO/external" "$REPO/docs/archive"
git -C "$REPO" init -q
git -C "$REPO" config user.name "Smoke Test"
git -C "$REPO" config user.email "smoke@example.invalid"

printf '%s\n' \
    '[[spec]]' \
    'id = "local"' \
    'home = "spec/"' \
    '' \
    '[[spec]]' \
    'id = "external"' \
    'home = "external/"' \
    'kind = "external"' > "$REPO/.machine_readable/REGISTRY.a2ml"

printf 'summary\n' > "$REPO/spec/SESSION_SUMMARY_2026-08-24.md"
printf 'tasks\n' > "$REPO/spec/sub/SONNET-TASKS.md"
printf 'canonical\n' > "$REPO/spec/README.adoc"
printf 'fixture\n' > "$REPO/spec/examples/SESSION_SUMMARY_EXAMPLE.md"
printf 'external\n' > "$REPO/external/SESSION_SUMMARY.md"
printf 'already archived\n' > "$REPO/docs/archive/SESSION_SUMMARY_OLD.md"

git -C "$REPO" add .
git -C "$REPO" commit -qm "fixture"

dry_output=$("$SCRIPT" --dry-run "$REPO")
[[ "$dry_output" == *"spec/SESSION_SUMMARY_2026-08-24.md"* ]]
[[ "$dry_output" == *"spec/sub/SONNET-TASKS.md"* ]]
[[ "$dry_output" != *"external/SESSION_SUMMARY.md"* ]]
[[ -f "$REPO/spec/SESSION_SUMMARY_2026-08-24.md" ]]

"$SCRIPT" "$REPO"

test -f "$REPO/docs/archive/session-detritus/spec/SESSION_SUMMARY_2026-08-24.md"
test -f "$REPO/docs/archive/session-detritus/spec/sub/SONNET-TASKS.md"
test -f "$REPO/spec/README.adoc"
test -f "$REPO/spec/examples/SESSION_SUMMARY_EXAMPLE.md"
test -f "$REPO/external/SESSION_SUMMARY.md"
test -f "$REPO/docs/archive/SESSION_SUMMARY_OLD.md"

status=$(git -C "$REPO" status --short)
[[ $(printf '%s\n' "$status" | grep -c '^R ') -eq 2 ]]

second_output=$("$SCRIPT" "$REPO")
[[ "$second_output" == *"no tracked session detritus"* ]]

printf 'collision\n' > "$REPO/spec/SESSION_SUMMARY_2026-08-24.md"
git -C "$REPO" add spec/SESSION_SUMMARY_2026-08-24.md
set +e
collision_output=$("$SCRIPT" "$REPO" 2>&1)
collision_rc=$?
set -e
[[ "$collision_rc" -eq 2 ]]
[[ "$collision_output" == *"archive collision"* ]]
test -f "$REPO/spec/SESSION_SUMMARY_2026-08-24.md"

test "$(jq -r '.registry.by_recipe["recipe-archive-session-detritus"]' "$REGISTRY")" = \
    "fix-session-detritus.sh"
test "$(jq -r '.registry.by_category.SessionDetritus' "$REGISTRY")" = \
    "fix-session-detritus.sh"

echo "PASS: session-detritus archiver is bounded, collision-safe, and idempotent"
