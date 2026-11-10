#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# fix-split-comment-keys.sh: restore comment lines that a lock run split in two
#
# The estate sweep that adopted the Actions lock file (2026-08) ran
# gh actions-lock on each workflow. In a job that is commented out, the tool
# changed a comment line that holds only a key, for example
#
#     #   steps:
#
# into 2 lines: a line with only "#", and the key at column 1:
#
#     #
#     steps:
#
# GitHub then refuses the workflow file ("Unexpected value 'steps'"), and no
# job of that file runs. The template example file e2e.yml has many such
# blocks (steps:, with:, strategy:).
#
# Target: in .github/workflows/*.yml and *.yaml, a line with only "#",
# followed by a key at column 1 that has no value and that is not a legal
# top-level key of a workflow, followed by a comment line. A key that is legal
# at the top level (name, run-name, on, permissions, env, defaults,
# concurrency, jobs) is not a target, because the line can be correct.
#
# Repair: the 2 lines become 1 comment line again. The key gets the indentation
# of the next comment line minus 2 spaces (the key is the parent of that line).
# If the next line cannot give an indentation, the file is not changed, and
# the recipe exits 1 so that the run does not read as a repair.
#
# Not verified: that a later lock run does not split these lines again.
#
# Idempotent: a file with no such pair is not changed.
#
# Usage: fix-split-comment-keys.sh <repo-path> [finding-json]
#        fix-split-comment-keys.sh --check <repo-path>
# outcome-predicate: --check

set -euo pipefail

ME="fix-split-comment-keys"

# Shared awk functions: the definition of a split pair.
PAIR_LIB=$(cat <<'AWK'
function is_key(l) {
    return l ~ /^[A-Za-z_][A-Za-z0-9_-]*:[[:space:]]*$/ &&
           l !~ /^(name|run-name|on|permissions|env|defaults|concurrency|jobs):/
}
function is_pair(i) {
    return i + 2 <= NR && line[i] == "#" && is_key(line[i + 1]) && line[i + 2] ~ /^#/
}
# Number of spaces for the restored key, or -1 when the next line gives none.
function key_indent(i,    rest, first) {
    rest = substr(line[i + 2], 2)
    first = match(rest, /[^ ]/)
    return (first >= 4) ? first - 3 : -1
}
{ line[NR] = $0 }
AWK
)

# Print each workflow file that is a regular file inside the repository.
# A symlink, or a workflow directory that leaves the repository, is no target.
list_files() {
    local repo="$1" root dir file
    root=$(realpath -e -- "$repo") || return 0
    dir="$root/.github/workflows"
    [[ -d "$dir" && "$(realpath -e -- "$dir")" == "$dir" ]] || return 0
    for file in "$dir"/*.yml "$dir"/*.yaml; do
        [[ -f "$file" && ! -L "$file" ]] && printf '%s\n' "$file"
    done
    return 0
}

# Print the number of split pairs in one file.
count_file() {
    awk "$PAIR_LIB"'
        END { for (i = 1; i <= NR; i++) if (is_pair(i)) n++; print n + 0 }
    ' "$1"
}

# Print the number of split pairs in the repository.
count_targets() {
    local file total=0
    while IFS= read -r file; do
        total=$((total + $(count_file "$file")))
    done < <(list_files "$1")
    echo "$total"
}

if [[ "${1:-}" == "--check" ]]; then
    [[ -d "${2:-}" ]] || { echo "--check: not a directory: ${2:-}" >&2; exit 2; }
    count_targets "$2"
    exit 0
fi

REPO_PATH="${1:?Usage: $0 <repo-path> [finding-json]}"
[[ -d "$REPO_PATH" ]] || { echo "$ME: not a directory: $REPO_PATH" >&2; exit 2; }

TMP=$(mktemp)
trap 'rm -f "$TMP"' EXIT
FAILED=0
REPAIRED=0

while IFS= read -r FILE; do
    PAIRS=$(count_file "$FILE")
    [[ "$PAIRS" == 0 ]] && continue
    # Validate each pair first, then write: a file is repaired fully or not at all.
    if awk "$PAIR_LIB"'
        END {
            for (i = 1; i <= NR; i++) if (is_pair(i) && key_indent(i) < 0) exit 1
            for (i = 1; i <= NR; i++) {
                if (is_pair(i)) {
                    key = line[i + 1]; sub(/[[:space:]]*$/, "", key)
                    pad = ""; for (s = key_indent(i); s > 0; s--) pad = pad " "
                    print "#" pad key
                    i++
                } else {
                    print line[i]
                }
            }
        }
    ' "$FILE" > "$TMP" && [[ -s "$TMP" ]]; then
        cat "$TMP" > "$FILE"
        REPAIRED=$((REPAIRED + PAIRS))
        echo "$ME: restored $PAIRS comment line(s) in ${FILE##*/}"
    else
        FAILED=1
        echo "$ME: ${FILE##*/} has a split key with no usable next line; not changed" >&2
    fi
done < <(list_files "$REPO_PATH")

if [[ "$FAILED" == 1 || "$(count_targets "$REPO_PATH")" != 0 ]]; then
    echo "$ME: split comment keys remain in $REPO_PATH" >&2
    exit 1
fi
if [[ "$REPAIRED" == 0 ]]; then
    echo "$ME: no split comment key in .github/workflows (nothing to do)"
else
    echo "$ME: restored $REPAIRED comment line(s) in total"
fi
