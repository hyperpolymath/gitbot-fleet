#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Auto-fix: Replace CORS wildcard (*) with environment-based origin whitelist

set -euo pipefail

REPO_PATH="${1:-.}"
FINDING_FILE="${2:-}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${BLUE}[fix-cors]${NC} $*"; }
log_success() { echo -e "${GREEN}[fix-cors]${NC} $*"; }
log_error() { echo -e "${RED}[fix-cors]${NC} $*"; }

if [[ -z "$FINDING_FILE" ]]; then
    log_error "No finding file provided"
    exit 1
fi

# Extract file and line from finding
FILE=$(jq -r '.file' "$FINDING_FILE")
LINE=$(jq -r '.line' "$FINDING_FILE")
CODE=$(jq -r '.code' "$FINDING_FILE")

log_info "Fixing CORS wildcard in $FILE:$LINE"

if [[ ! -f "$FILE" ]]; then
    log_error "File not found: $FILE"
    exit 1
fi

# Create backup
cp "$FILE" "${FILE}.bak"

# Determine replacement and perform substitution based on file type
if [[ "$FILE" == *.rs ]]; then
    # Rust/Axum pattern - use environment variable
    sed -i "${LINE}s|\"\\*\"|env::var(\\\"ALLOWED_ORIGINS\\\").unwrap_or_else(\|_\| \\\"http://localhost:3000\\\".to_string())|g" "$FILE"
elif [[ "$FILE" == *.py ]]; then
    # Python/Flask pattern
    sed -i "${LINE}s|\"\\*\"|os.getenv(\\\"ALLOWED_ORIGINS\\\", \\\"http://localhost:3000\\\")|g" "$FILE"
elif [[ "$FILE" == *.go ]]; then
    # Go pattern
    sed -i "${LINE}s|\"\\*\"|os.Getenv(\\\"ALLOWED_ORIGINS\\\")|g" "$FILE"
elif [[ "$FILE" == *.res ]]; then
    # ReScript pattern - use Node.js.Process.env
    sed -i "${LINE}s|\"\\*\"|Node.process->Node.Process.env->Js.Dict.get(\\\"ALLOWED_ORIGINS\\\")->Belt.Option.getWithDefault(\\\"http://localhost:3000\\\")|g" "$FILE"
else
    # JavaScript/Node.js default
    sed -i "${LINE}s|\"\\*\"|process.env.ALLOWED_ORIGINS \|\| \\\"http://localhost:3000\\\"|g" "$FILE"
fi

if ! diff -q "$FILE" "${FILE}.bak" >/dev/null 2>&1; then
    log_success "Fixed CORS wildcard in $FILE:$LINE"

    # Add comment explaining the change
    sed -i "${LINE}i\\    // SECURITY FIX: Replaced CORS wildcard with environment-based origin" "$FILE"

    # Create git commit if in git repo
    if [[ -d "$REPO_PATH/.git" ]]; then
        cd "$REPO_PATH"
        git add "$FILE"
        git commit -m "security: replace CORS wildcard with environment variable

Replaced Access-Control-Allow-Origin: '*' with environment-based origin
whitelist to prevent CORS misconfiguration vulnerability (CWE-942).

Set ALLOWED_ORIGINS environment variable to configure permitted origins.

Auto-fixed by: robot-repo-automaton
Finding: $(jq -r '.id' "$FINDING_FILE")

Co-Authored-By: Hypatia Scanner <hypatia@reposystem.dev>"
    fi

    rm -f "${FILE}.bak"
    exit 0
else
    log_error "No changes made to $FILE"
    rm -f "${FILE}.bak"
    exit 1
fi
