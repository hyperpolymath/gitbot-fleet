#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
# Add SPDX license identifier to files missing it
#
# DISABLED 2026-06-02 per owner directive on licence policy.
# See `feedback_no_automated_licence_edits.md` and
# `feedback_estate_license_policy_umbrella.md`. Licence remediation is
# manual, file-by-file, owner-only. Triggered by neurophone#99 (auto-PR
# reverting PMPL → MPL-2.0 across ~140 files, closed by owner).

set -euo pipefail

echo "REFUSED: fix-missing-spdx.sh is disabled per estate policy 2026-06-02." >&2
echo "        Licence/SPDX edits MUST be manual, per-file, owner-approved." >&2
echo "        SPDX choice depends on the five-way owner classification" >&2
echo "        (sole-repo / 007 / son-shared / fork / palimpsest carve-out)." >&2
echo "        See feedback_estate_license_policy_umbrella.md." >&2
exit 1

REPO_PATH="$1"
FINDING_FILE="$2"

echo "Fixing missing SPDX header in $REPO_PATH..."

# Extract file path and license from finding
FILE_PATH=$(jq -r '.location.file' "$FINDING_FILE")
FULL_PATH="$REPO_PATH/$FILE_PATH"

# Default license for hyperpolymath repos
DEFAULT_LICENSE="MPL-2.0"

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

# Check if line 1 is a shebang — if so, preserve it before the SPDX header
FIRST_LINE=$(head -1 "$FULL_PATH")
if [[ "$FIRST_LINE" == "#!"* ]]; then
    echo "$FIRST_LINE" > "$TEMP_FILE"
    echo "${COMMENT_PREFIX} SPDX-License-Identifier: ${DEFAULT_LICENSE}" >> "$TEMP_FILE"
    tail -n +2 "$FULL_PATH" >> "$TEMP_FILE"
else
    echo "${COMMENT_PREFIX} SPDX-License-Identifier: ${DEFAULT_LICENSE}" > "$TEMP_FILE"
    cat "$FULL_PATH" >> "$TEMP_FILE"
fi

# Replace original file
mv "$TEMP_FILE" "$FULL_PATH"

echo "✅ Added SPDX header to $FILE_PATH"
