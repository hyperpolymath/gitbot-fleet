#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# scan-cache.sh — Hash-based scan cache for fix scripts
#
# Source this library to skip files that haven't changed since last scan.
# Uses SHA-256 hashes stored per-repo in .hypatia/scan-cache/<script-name>.hashes
#
# Usage (in a fix script):
#   source "$(dirname "$0")/lib/scan-cache.sh"
#   cache_init "fix-http-to-https" "$REPO_PATH"
#   for file in ...; do
#       if cache_unchanged "$file"; then continue; fi
#       # ... process file ...
#       cache_update "$file"
#   done
#   cache_save

CACHE_SCRIPT_NAME=""
CACHE_REPO_PATH=""
CACHE_DIR=""
CACHE_FILE=""
CACHE_FILE_NEW=""

# Associative arrays for old and new hashes
declare -gA _CACHE_OLD=()
declare -gA _CACHE_NEW=()
CACHE_HITS=0
CACHE_MISSES=0

# Initialize cache for a script+repo combination
cache_init() {
    CACHE_SCRIPT_NAME="$1"
    CACHE_REPO_PATH="$2"
    CACHE_DIR="$CACHE_REPO_PATH/.hypatia/scan-cache"
    CACHE_FILE="$CACHE_DIR/${CACHE_SCRIPT_NAME}.hashes"
    CACHE_FILE_NEW="$CACHE_FILE.new"
    CACHE_HITS=0
    CACHE_MISSES=0

    # Load existing cache
    _CACHE_OLD=()
    _CACHE_NEW=()
    if [[ -f "$CACHE_FILE" ]]; then
        while IFS='  ' read -r hash path; do
            [[ -n "$hash" && -n "$path" ]] && _CACHE_OLD["$path"]="$hash"
        done < "$CACHE_FILE"
    fi
}

# Check if a file is unchanged since last scan
# Returns 0 (true) if unchanged, 1 (false) if changed or new
cache_unchanged() {
    local filepath="$1"
    local rel_path="${filepath#"$CACHE_REPO_PATH"/}"

    # No previous hash → file is new/changed
    if [[ -z "${_CACHE_OLD[$rel_path]+x}" ]]; then
        ((CACHE_MISSES++)) || true
        return 1
    fi

    # Compute current hash
    local current_hash
    current_hash=$(sha256sum "$filepath" 2>/dev/null | cut -d' ' -f1)

    if [[ "$current_hash" == "${_CACHE_OLD[$rel_path]}" ]]; then
        # Unchanged — carry forward to new cache
        _CACHE_NEW["$rel_path"]="$current_hash"
        ((CACHE_HITS++)) || true
        return 0
    fi

    ((CACHE_MISSES++)) || true
    return 1
}

# Update cache entry for a processed file
cache_update() {
    local filepath="$1"
    local rel_path="${filepath#"$CACHE_REPO_PATH"/}"
    local current_hash
    current_hash=$(sha256sum "$filepath" 2>/dev/null | cut -d' ' -f1)
    _CACHE_NEW["$rel_path"]="$current_hash"
}

# Save the new cache to disk
cache_save() {
    if [[ ${#_CACHE_NEW[@]} -eq 0 && "$CACHE_MISSES" -eq 0 ]]; then
        return
    fi

    mkdir -p "$CACHE_DIR"
    : > "$CACHE_FILE_NEW"

    for path in "${!_CACHE_NEW[@]}"; do
        echo "${_CACHE_NEW[$path]}  $path" >> "$CACHE_FILE_NEW"
    done

    mv "$CACHE_FILE_NEW" "$CACHE_FILE"
}

# Print cache stats
cache_stats() {
    echo "  Cache: $CACHE_HITS hits, $CACHE_MISSES misses (${#_CACHE_NEW[@]} entries saved)"
}
