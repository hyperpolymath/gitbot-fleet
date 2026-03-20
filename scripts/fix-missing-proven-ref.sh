#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-missing-proven-ref.sh — Add proven integration declaration if missing
#
# Fixes MissingProvenRef by creating .machine_readable/integrations/proven.a2ml
# declaring the proven formally verified safety library dependency.
#
# Usage: fix-missing-proven-ref.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_FILE="${2:?Missing finding JSON file}"

TARGET_DIR="$REPO_PATH/.machine_readable/integrations"
TARGET_FILE="$TARGET_DIR/proven.a2ml"

echo "=== Missing Proven Ref Fix ==="
echo "  Repo: $REPO_PATH"

# Idempotency: skip if already exists
if [[ -f "$TARGET_FILE" ]]; then
    echo "  SKIP: proven.a2ml already exists"
    exit 0
fi

# Create parent directories
mkdir -p "$TARGET_DIR"

# Write template content
cat > "$TARGET_FILE" <<'PROVEN'
# SPDX-License-Identifier: PMPL-1.0-or-later
# Proven Integration — Formally Verified Safety Library

[integration]
name = "proven"
type = "safety-library"
repository = "https://github.com/hyperpolymath/proven"
version = "1.2.0"

[binding-policy]
approach = "thin-ffi-wrapper"
unsafe-patterns = "replace-with-proven-equivalent"
modules-available = ["SafeMath", "SafeString", "SafeJSON", "SafeURL", "SafeRegex", "SafeSQL", "SafeFile", "SafeTemplate", "SafeCrypto"]

[adoption-guidance]
priority = "high"
scope = "all-string-json-url-crypto-operations"
migration = "incremental — replace unsafe patterns as encountered"
PROVEN

echo "  CREATED .machine_readable/integrations/proven.a2ml"
echo ""
echo "Fixed: 1 file created"
exit 0
