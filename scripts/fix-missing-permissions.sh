#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Add missing permissions declaration to GitHub workflows

set -euo pipefail

REPO_PATH="$1"
FINDING_FILE="$2"

echo "Fixing missing permissions in $REPO_PATH..."

# Extract file path from finding
WORKFLOW_FILE=$(jq -r '.location.file' "$FINDING_FILE")
WORKFLOW_PATH="$REPO_PATH/$WORKFLOW_FILE"

if [[ ! -f "$WORKFLOW_PATH" ]]; then
    echo "ERROR: Workflow file not found: $WORKFLOW_PATH"
    exit 1
fi

# Check if workflow already has permissions
if grep -q "^permissions:" "$WORKFLOW_PATH"; then
    echo "  Workflow already has permissions declaration"
    exit 0
fi

# Add permissions after the first 'on:' block
# Strategy: Find line with 'on:', then insert 'permissions: read-all' before next top-level key

# Temporary file for editing
TEMP_FILE=$(mktemp)

# Insert permissions: read-all after the 'on:' block
awk '
/^on:/ { in_on_block = 1; print; next }
in_on_block && /^[a-z]/ && !/^  / {
    print "permissions: read-all\n"
    in_on_block = 0
}
{ print }
' "$WORKFLOW_PATH" > "$TEMP_FILE"

# Replace original file
mv "$TEMP_FILE" "$WORKFLOW_PATH"

echo "✅ Added permissions declaration to $WORKFLOW_FILE"
