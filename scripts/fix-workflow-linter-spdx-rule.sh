#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# fix-workflow-linter-spdx-rule.sh: accept the SPDX line in the leading comment block
#
# Older copies of the estate template have this rule in
# .github/workflows/workflow-linter.yml:
#
#     if ! head -1 "$file" | grep -q "^# SPDX-License-Identifier:"; then
#
# The tool gh actions-lock writes its own comment on line 1 of each workflow
# that it manages. Each lock run thus makes every managed workflow fail the
# lint ("missing SPDX header"). The current template (rsr-template-repo)
# accepts the licence line at any position in the leading comment block.
# This recipe replaces the old rule with the rule of the template.
#
# Only the exact old rule is a target. A linter with a different rule (for
# example `head -5`) is not changed.
#
# Idempotent: a file that has the new rule is not changed.
#
# Usage: fix-workflow-linter-spdx-rule.sh <repo-path> [finding-json]
#        fix-workflow-linter-spdx-rule.sh --check <repo-path>
# outcome-predicate: --check

set -euo pipefail

REL=".github/workflows/workflow-linter.yml"
OLD_RULE='^[[:space:]]*if ! head -1 "\$[A-Za-z_][A-Za-z0-9_]*" \| grep -q "\^# SPDX-License-Identifier:"; then[[:space:]]*$'
NEW_RULE='/^# SPDX-License-Identifier:/ { found=1 } /^[[:space:]]*[^#[:space:]]/ { exit } END { exit !found }'

# Print the path of the linter when it is a regular file inside the repository.
# A missing file, a symlink, or a path that leaves the repository is no target.
target_file() {
    local repo="$1" root
    root=$(realpath -e -- "$repo") || return 1
    [[ -f "$root/$REL" && ! -L "$root/$REL" ]] || return 1
    [[ "$(realpath -e -- "$root/$REL")" == "$root/$REL" ]] || return 1
    printf '%s\n' "$root/$REL"
}

# Print the number of old-rule lines in the repository (0 when no target file).
count_targets() {
    local file
    file=$(target_file "$1") || { echo 0; return 0; }
    grep -cE -- "$OLD_RULE" "$file" || true
}

if [[ "${1:-}" == "--check" ]]; then
    [[ -d "${2:-}" ]] || { echo "--check: not a directory: ${2:-}" >&2; exit 2; }
    count_targets "$2"
    exit 0
fi

REPO_PATH="${1:?Usage: $0 <repo-path> [finding-json]}"
[[ -d "$REPO_PATH" ]] || { echo "fix-workflow-linter-spdx-rule: not a directory: $REPO_PATH" >&2; exit 2; }

if [[ "$(count_targets "$REPO_PATH")" == 0 ]]; then
    echo "fix-workflow-linter-spdx-rule: no old SPDX rule in $REL (nothing to do)"
    exit 0
fi

FILE=$(target_file "$REPO_PATH")
TMP=$(mktemp)
trap 'rm -f "$TMP"' EXIT

PROGRAM=$(cat <<'AWK'
/^[[:space:]]*if ! head -1 "\$[A-Za-z_][A-Za-z0-9_]*" \| grep -q "\^# SPDX-License-Identifier:"; then[[:space:]]*$/ {
    indent = $0; sub(/if ! head.*$/, "", indent)
    name = $0; sub(/^[^$]*\$/, "", name); sub(/".*$/, "", name)
    print indent "# actions-lock may prepend its own comment. Require the licence"
    print indent "# in the leading comment block, before the workflow body."
    print indent "if ! awk '" ENVIRON["NEW_RULE"] "' \"$" name "\"; then"
    next
}
/echo "Add '# SPDX-License-Identifier: [A-Za-z0-9.+-]+' as first line"/ {
    sub(/ as first line"/, " to the leading comment block\"")
}
{ print }
AWK
)

NEW_RULE="$NEW_RULE" awk "$PROGRAM" "$FILE" > "$TMP"
[[ -s "$TMP" ]] || { echo "fix-workflow-linter-spdx-rule: the edit gave no text; $REL is not changed" >&2; exit 1; }
cat "$TMP" > "$FILE"

if [[ "$(count_targets "$REPO_PATH")" != 0 ]]; then
    echo "fix-workflow-linter-spdx-rule: the old rule is still in $REL" >&2
    exit 1
fi
echo "fix-workflow-linter-spdx-rule: replaced the SPDX rule in $REL"
