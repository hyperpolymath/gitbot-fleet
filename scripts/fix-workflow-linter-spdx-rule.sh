#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# fix-workflow-linter-spdx-rule.sh: accept the SPDX line in the leading comment block
#
# Older copies of the estate template have 1 of 2 rules in
# .github/workflows/workflow-linter.yml.
#
# Form 1 reads line 1 only:
#
#     if ! head -1 "$file" | grep -q "^# SPDX-License-Identifier:"; then
#
# The tool gh actions-lock writes its own comment on line 1 of each workflow
# that it manages. Each lock run thus makes every managed workflow fail the
# lint ("missing SPDX header").
#
# Form 2 is the first template rule that reads the leading comment block:
#
#     if ! awk '... /^[^#[:space:]]/ { exit } ...' "$file"; then
#
# It stops only at a line that starts in column 1. A workflow with an
# indented body thus passes with a licence line after the body
# (rsr-template-repo pull request 323).
#
# The current template (rsr-template-repo) accepts the licence line at any
# position in the leading comment block, and stops at the first line of the
# body. This recipe replaces each form with the rule of the template.
#
# Only the 2 exact forms are targets. A linter with a different rule (for
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
OLD_AWK_RULE='/^# SPDX-License-Identifier:/ { found=1 } /^[^#[:space:]]/ { exit } END { exit !found }'
NEW_RULE='/^# SPDX-License-Identifier:/ { found=1 } /^[[:space:]]*[^#[:space:]]/ { exit } END { exit !found }'
export OLD_AWK_RULE NEW_RULE

# awk function: the position of form 2 in a line, or 0. A line is a target
# only when the whole line is `if ! awk '<form 2>' "$name"; then`.
OLD_AWK_AT=$(cat <<'AWK'
function old_awk_at(s,    lit, p) {
    lit = "awk '" ENVIRON["OLD_AWK_RULE"] "' \"$"
    p = index(s, lit)
    if (p == 0) return 0
    if (substr(s, 1, p - 1) !~ /^[[:space:]]*if ! $/) return 0
    if (substr(s, p + length(lit)) !~ /^[A-Za-z_][A-Za-z0-9_]*"; then[[:space:]]*$/) return 0
    return p
}
AWK
)

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
    local file form1 form2
    file=$(target_file "$1") || { echo 0; return 0; }
    form1=$(grep -cE -- "$OLD_RULE" "$file" || true)
    form2=$(awk "$OLD_AWK_AT"' old_awk_at($0) { n++ } END { print n + 0 }' "$file")
    echo $((form1 + form2))
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
{
    # Form 2: only the rule text changes. The 2 comment lines are there.
    p = old_awk_at($0)
    if (p) {
        rest = substr($0, p + length("awk '" ENVIRON["OLD_AWK_RULE"] "' \"$"))
        $0 = substr($0, 1, p - 1) "awk '" ENVIRON["NEW_RULE"] "' \"$" rest
    }
}
{ print }
AWK
)

awk "$OLD_AWK_AT"$'\n'"$PROGRAM" "$FILE" > "$TMP"
[[ -s "$TMP" ]] || { echo "fix-workflow-linter-spdx-rule: the edit gave no text; $REL is not changed" >&2; exit 1; }
cat "$TMP" > "$FILE"

if [[ "$(count_targets "$REPO_PATH")" != 0 ]]; then
    echo "fix-workflow-linter-spdx-rule: the old rule is still in $REL" >&2
    exit 1
fi
echo "fix-workflow-linter-spdx-rule: replaced the SPDX rule in $REL"
