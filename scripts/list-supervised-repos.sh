#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# Emit one supervised repository path per line.
# Sources (first existing source wins unless --inventory is provided):
#  1) --inventory / $FLEET_SUPERVISED_REPOS_FILE
#  2) ~/.git-private-repos (newline list)
#  3) ~/.git-private-farm.scm (SCM config with (repos . (...)))
#  4) shared-context/enrollment/repos.json

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FLEET_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

DEFAULT_REPOS_ROOT="/var/mnt/eclipse/repos"
REPOS_ROOT="${REPOS_ROOT:-$DEFAULT_REPOS_ROOT}"
LIMIT=0
INVENTORY_FILE="${FLEET_SUPERVISED_REPOS_FILE:-}"

usage() {
    cat <<'EOF'
usage: list-supervised-repos.sh [options]

Options:
  --repos-root <path>   Base path for repo names (default: /var/mnt/eclipse/repos)
  --inventory <path>    Inventory file to parse (.git-private-repos or .git-private-farm.scm)
  --limit <n>           Emit at most n repositories (0 = unlimited)
  --help, -h            Show this help
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --repos-root)
            REPOS_ROOT="${2:-}"
            shift 2
            ;;
        --inventory)
            INVENTORY_FILE="${2:-}"
            shift 2
            ;;
        --limit)
            LIMIT="${2:-0}"
            shift 2
            ;;
        --help|-h)
            usage
            exit 0
            ;;
        *)
            echo "error: unknown argument: $1" >&2
            usage >&2
            exit 2
            ;;
    esac
done

if [[ ! -d "$REPOS_ROOT" ]]; then
    echo "error: repos root does not exist: $REPOS_ROOT" >&2
    exit 1
fi

declare -A seen
declare -a repos=()

resolve_repo_path() {
    local candidate="$1"
    candidate="${candidate%/}"
    [[ -z "$candidate" ]] && return 1

    # Expand placeholders sometimes used in generated enrollment metadata.
    candidate="${candidate//\/var\$REPOS_DIR/$REPOS_ROOT}"
    candidate="${candidate//\$REPOS_DIR/${REPOS_ROOT#/var/}}"

    if [[ "$candidate" != /* ]]; then
        candidate="$REPOS_ROOT/$candidate"
    fi

    if [[ -d "$candidate/.git" ]]; then
        printf '%s\n' "$candidate"
        return 0
    fi

    return 1
}

add_repo() {
    local raw="$1"
    local resolved=""
    resolved="$(resolve_repo_path "$raw" 2>/dev/null || true)"
    [[ -z "$resolved" ]] && return 0

    if [[ -z "${seen[$resolved]:-}" ]]; then
        seen["$resolved"]=1
        repos+=("$resolved")
    fi
}

add_from_plain_list() {
    local file="$1"
    while IFS= read -r line; do
        line="${line%%#*}"
        line="$(echo "$line" | sed 's/^[[:space:]]*//; s/[[:space:]]*$//')"
        [[ -z "$line" ]] && continue
        add_repo "$line"
    done < "$file"
}

add_from_private_farm_scm() {
    local file="$1"
    while IFS= read -r repo_name; do
        add_repo "$repo_name"
    done < <(
        awk '
          /\(repos[[:space:]]+\.[[:space:]]+\(/ {in_repos=1; next}
          in_repos && /\)\)/ {in_repos=0}
          in_repos {
            while (match($0, /"[^"]+"/)) {
              raw = substr($0, RSTART + 1, RLENGTH - 2)
              print raw
              $0 = substr($0, RSTART + RLENGTH)
            }
          }
        ' "$file"
    )
}

add_from_registry_json() {
    local file="$1"
    if ! command -v jq >/dev/null 2>&1; then
        return 0
    fi

    while IFS= read -r item; do
        add_repo "$item"
    done < <(jq -r '.repos[]?.path // .repos[]?.name // empty' "$file" 2>/dev/null)
}

SOURCE_USED=""

if [[ -n "$INVENTORY_FILE" && -f "$INVENTORY_FILE" ]]; then
    SOURCE_USED="$INVENTORY_FILE"
elif [[ -f "$HOME/.git-private-repos" ]]; then
    INVENTORY_FILE="$HOME/.git-private-repos"
    SOURCE_USED="$INVENTORY_FILE"
elif [[ -f "$HOME/.git-private-farm.scm" ]]; then
    INVENTORY_FILE="$HOME/.git-private-farm.scm"
    SOURCE_USED="$INVENTORY_FILE"
elif [[ -f "$FLEET_DIR/shared-context/enrollment/repos.json" ]]; then
    INVENTORY_FILE="$FLEET_DIR/shared-context/enrollment/repos.json"
    SOURCE_USED="$INVENTORY_FILE"
fi

if [[ -n "$INVENTORY_FILE" && -f "$INVENTORY_FILE" ]]; then
    case "$INVENTORY_FILE" in
        *.scm)
            add_from_private_farm_scm "$INVENTORY_FILE"
            ;;
        *.json)
            add_from_registry_json "$INVENTORY_FILE"
            ;;
        *)
            add_from_plain_list "$INVENTORY_FILE"
            ;;
    esac
fi

# Last-resort fallback: include current repo only to keep CI paths functional.
if [[ "${#repos[@]}" -eq 0 ]]; then
    add_repo "$FLEET_DIR"
fi

emitted=0
for repo_path in "${repos[@]}"; do
    if [[ "$LIMIT" -gt 0 && "$emitted" -ge "$LIMIT" ]]; then
        break
    fi
    printf '%s\n' "$repo_path"
    emitted=$((emitted + 1))
done
