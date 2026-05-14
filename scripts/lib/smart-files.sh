#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# smart-files.sh — Fast file listing for fix scripts
#
# Uses git ls-files instead of find for tracked repos.
# Falls back to find with aggressive exclusions for non-git dirs.
#
# Usage:
#   source "$(dirname "$0")/lib/smart-files.sh"
#   smart_list_files "$REPO_PATH" "*.sh" "*.bash"   # by extension
#   smart_list_files "$REPO_PATH" "*.rs"             # Rust files
#
# Output: NUL-delimited file paths (use: while IFS= read -r -d '' f)

# Directories to always skip (vendored, generated, massive)
SKIP_DIRS=(
    .git target node_modules _build .lake deps
    vendor dist build __pycache__ .tox .mypy_cache
    winget-pkgs  # 50k+ YAML manifests
    compiler-source  # ReScript compiler tests
    .elixir_ls .hex .mix
    coverage .cache .parcel-cache
)

# List files matching given globs, using git ls-files when possible
# Args: repo_path glob1 [glob2 ...]
# Output: NUL-delimited absolute paths to stdout
smart_list_files() {
    local repo_path="$1"
    shift
    local globs=("$@")

    if [[ -d "$repo_path/.git" ]]; then
        # Use git ls-files — much faster, only tracked files
        local git_args=()
        for g in "${globs[@]}"; do
            git_args+=("$g")
        done

        # git ls-files with glob matching, NUL-delimited
        (cd "$repo_path" && git ls-files -z "${git_args[@]}" 2>/dev/null) | \
            while IFS= read -r -d '' f; do
                # Skip vendored/generated directories
                local skip=false
                for d in "${SKIP_DIRS[@]}"; do
                    if [[ "$f" == *"/$d/"* || "$f" == "$d/"* ]]; then
                        skip=true
                        break
                    fi
                done
                if [[ "$skip" == "false" ]]; then
                    printf '%s\0' "$repo_path/$f"
                fi
            done
    else
        # Fallback to find with exclusions
        local find_excludes=()
        for d in "${SKIP_DIRS[@]}"; do
            find_excludes+=(-not -path "*/${d}/*")
        done

        local find_names=()
        local first=true
        for g in "${globs[@]}"; do
            if [[ "$first" == "true" ]]; then
                find_names+=(-name "$g")
                first=false
            else
                find_names+=(-o -name "$g")
            fi
        done

        find "$repo_path" -type f \( "${find_names[@]}" \) \
            "${find_excludes[@]}" -print0 2>/dev/null
    fi
}

# List files changed since a given timestamp (epoch seconds)
# Args: repo_path since_epoch glob1 [glob2 ...]
smart_list_changed_since() {
    local repo_path="$1"
    local since_epoch="$2"
    shift 2
    local globs=("$@")

    if [[ -d "$repo_path/.git" ]]; then
        # Use git diff to find changed files
        local since_date
        since_date=$(date -d "@$since_epoch" -u +"%Y-%m-%dT%H:%M:%SZ" 2>/dev/null || date -u +"%Y-%m-%dT%H:%M:%SZ")

        (cd "$repo_path" && git log --since="$since_date" --diff-filter=ACMR --name-only --pretty=format: HEAD 2>/dev/null | sort -u) | \
            while IFS= read -r f; do
                [[ -z "$f" ]] && continue
                for g in "${globs[@]}"; do
                    # Simple glob match using bash pattern
                    local ext="${g#\*.}"
                    if [[ "$f" == *".$ext" && -f "$repo_path/$f" ]]; then
                        printf '%s\0' "$repo_path/$f"
                        break
                    fi
                done
            done
    else
        # No git — fall back to find with -newer
        smart_list_files "$repo_path" "${globs[@]}"
    fi
}
