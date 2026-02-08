#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Add SPDX license identifier to files missing it

set -euo pipefail

REPO_PATH="$1"
FINDING_FILE="$2"

echo "Fixing missing SPDX header in $REPO_PATH..."

# Extract file path and license from finding
FILE_PATH=$(jq -r '.location.file' "$FINDING_FILE")
FULL_PATH="$REPO_PATH/$FILE_PATH"

# Default license (can be overridden by repo's LICENSE file)
DEFAULT_LICENSE="AGPL-3.0-or-later"

if [[ ! -f "$FULL_PATH" ]]; then
    echo "ERROR: File not found: $FULL_PATH"
    exit 1
fi

# Detect file type and comment syntax
case "$FILE_PATH" in
    *.yml|*.yaml)
        COMMENT_PREFIX="#"
        ;;
    *.sh|*.bash|*.py)
        COMMENT_PREFIX="#"
        ;;
    *.js|*.ts|*.jsx|*.tsx|*.res)
        COMMENT_PREFIX="//"
        ;;
    *.rs|*.go|*.java)
        COMMENT_PREFIX="//"
        ;;
    *)
        echo "WARN: Unknown file type, using # as comment"
        COMMENT_PREFIX="#"
        ;;
esac

# Check if file already has SPDX header
if head -3 "$FULL_PATH" | grep -q "SPDX-License-Identifier"; then
    echo "  File already has SPDX header"
    exit 0
fi

# Create temp file with SPDX header
TEMP_FILE=$(mktemp)

# Write SPDX header
echo "${COMMENT_PREFIX} SPDX-License-Identifier: ${DEFAULT_LICENSE}" > "$TEMP_FILE"

# Append original file content
cat "$FULL_PATH" >> "$TEMP_FILE"

# Replace original file
mv "$TEMP_FILE" "$FULL_PATH"

echo "✅ Added SPDX header to $FILE_PATH"
