#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-missing-trustfile.sh — Add Trustfile.a2ml to repositories that lack one
#
# Fixes MissingTrustfile by creating .machine_readable/contractiles/trust/Trustfile.a2ml
# with integrity and provenance contract declarations.
#
# Usage: fix-missing-trustfile.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_FILE="${2:?Missing finding JSON file}"

TARGET_DIR="$REPO_PATH/.machine_readable/contractiles/trust"
TARGET_FILE="$TARGET_DIR/Trustfile.a2ml"

echo "=== Missing Trustfile Fix ==="
echo "  Repo: $REPO_PATH"

# Idempotency: skip if already exists
if [[ -f "$TARGET_FILE" ]]; then
    echo "  SKIP: Trustfile.a2ml already exists"
    exit 0
fi

# Create parent directories
mkdir -p "$TARGET_DIR"

# Write template content
cat > "$TARGET_FILE" <<'TRUSTFILE'
# SPDX-License-Identifier: PMPL-1.0-or-later
# Trustfile — Integrity and Provenance Contract

[trustfile]
version = "1.0.0"
format = "a2ml"

[provenance]
source-control = "git"
forge = "github"
ci-verified = true
signing-policy = "commit-signing-preferred"

[integrity]
spdx-compliant = true
license-audit = "required"
dependency-pinning = "sha-pinned"

[verification]
reproducible-builds = "goal"
sbom-generation = "required"
attestation = "sigstore-preferred"
TRUSTFILE

echo "  CREATED .machine_readable/contractiles/trust/Trustfile.a2ml"
echo ""
echo "Fixed: 1 file created"
exit 0
