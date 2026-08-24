#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Archive point-in-time session reports found inside standards REGISTRY homes.
# Dispatch contract: fix-session-detritus.sh <repo-path> <finding-json>
# Operator preview:  fix-session-detritus.sh --dry-run <repo-path>

set -euo pipefail

DRY_RUN=false
if [[ "${1:-}" == "--dry-run" ]]; then
    DRY_RUN=true
    shift
fi

REPO_PATH="${1:?Usage: $0 [--dry-run] <repo-path> [finding-json]}"
FINDING_FILE="${2:-}"
REGISTRY="$REPO_PATH/.machine_readable/REGISTRY.a2ml"
ARCHIVE_ROOT="docs/archive/session-detritus"

if ! git -C "$REPO_PATH" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    echo "ERROR: not a git worktree: $REPO_PATH" >&2
    exit 2
fi

if [[ -n "$FINDING_FILE" ]]; then
    if [[ ! -f "$FINDING_FILE" ]] || ! jq -e . "$FINDING_FILE" >/dev/null 2>&1; then
        echo "ERROR: finding JSON is missing or invalid: $FINDING_FILE" >&2
        exit 2
    fi

    recipe_id=$(jq -r '.recipe_id // empty' "$FINDING_FILE")
    if [[ -n "$recipe_id" && "$recipe_id" != "recipe-archive-session-detritus" ]]; then
        echo "ERROR: finding recipe mismatch: $recipe_id" >&2
        exit 2
    fi
fi

if [[ ! -f "$REGISTRY" ]]; then
    echo "SKIP: no standards REGISTRY.a2ml at $REGISTRY"
    exit 0
fi

is_detritus() {
    local name="$1"
    case "$name" in
        *-COMPLETE*.md|*COMPLETE-*.md|*SESSION_SUMMARY*|DEPLOYMENT-SUCCESS.md|\
        DEPLOYMENT-SESSION_*.md|*SESSION-*.md|SONNET-TASKS.md)
            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

is_archived() {
    local path="$1"
    [[ "/$path/" == */docs/archive/* ]]
}

is_exempt_path() {
    local path="/$1/"
    [[ "$path" == */examples/* || "$path" == */templates/* || \
       "$path" == */test/fixtures/* || "$path" == */tests/fixtures/* ]]
}

declare -a LOCAL_HOMES=()
while IFS=$'\t' read -r spec_id home; do
    [[ -z "$spec_id" || -z "$home" ]] && continue

    if [[ "$home" == /* || "/$home/" == */../* || "$home" == "." ]]; then
        echo "ERROR: invalid LOCAL registry home for $spec_id: $home" >&2
        exit 2
    fi

    LOCAL_HOMES+=("${home%/}")
done < <(
    awk '
        /^\[\[spec\]\]$/ {
            if (in_spec && kind != "external" && id != "" && home != "") {
                print id "\t" home
            }
            in_spec=1; id=""; home=""; kind=""; next
        }
        in_spec && /^id[[:space:]]*=/ {
            value=$0; sub(/^[^=]*=[[:space:]]*"/, "", value); sub(/"[[:space:]]*$/, "", value); id=value; next
        }
        in_spec && /^home[[:space:]]*=/ {
            value=$0; sub(/^[^=]*=[[:space:]]*"/, "", value); sub(/"[[:space:]]*$/, "", value); home=value; next
        }
        in_spec && /^kind[[:space:]]*=/ {
            value=$0; sub(/^[^=]*=[[:space:]]*"/, "", value); sub(/"[[:space:]]*$/, "", value); kind=value; next
        }
        END {
            if (in_spec && kind != "external" && id != "" && home != "") {
                print id "\t" home
            }
        }
    ' "$REGISTRY"
)

if [[ ${#LOCAL_HOMES[@]} -eq 0 ]]; then
    echo "ERROR: registry contains no valid LOCAL spec homes" >&2
    exit 2
fi

in_local_home() {
    local path="$1"
    local home
    for home in "${LOCAL_HOMES[@]}"; do
        if [[ "$path" == "$home/"* ]]; then
            return 0
        fi
    done
    return 1
}

declare -a SOURCES=()
declare -a DESTINATIONS=()

while IFS= read -r -d '' path; do
    in_local_home "$path" || continue
    is_archived "$path" && continue
    is_exempt_path "$path" && continue
    is_detritus "$(basename "$path")" || continue

    destination="$ARCHIVE_ROOT/$path"
    if [[ -e "$REPO_PATH/$destination" ]]; then
        echo "ERROR: archive collision: $destination already exists" >&2
        exit 2
    fi

    SOURCES+=("$path")
    DESTINATIONS+=("$destination")
done < <(git -C "$REPO_PATH" ls-files -z)

if [[ ${#SOURCES[@]} -eq 0 ]]; then
    echo "OK: no tracked session detritus in LOCAL spec homes"
    exit 0
fi

for index in "${!SOURCES[@]}"; do
    source_path="${SOURCES[$index]}"
    destination="${DESTINATIONS[$index]}"

    if [[ "$DRY_RUN" == "true" ]]; then
        echo "WOULD ARCHIVE: $source_path -> $destination"
        continue
    fi

    mkdir -p "$REPO_PATH/$(dirname "$destination")"
    git -C "$REPO_PATH" mv -- "$source_path" "$destination"
    echo "ARCHIVED: $source_path -> $destination"
done

echo "Archived ${#SOURCES[@]} tracked session-detritus file(s); review and commit the staged renames in a PR."
