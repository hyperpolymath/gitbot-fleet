#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-missing-ai-manifest.sh — Add 0-AI-MANIFEST.a2ml to repositories that lack one
#
# Fixes MissingAIManifest by creating 0-AI-MANIFEST.a2ml at the repository root
# with universal AI agent entry point declarations.
#
# Usage: fix-missing-ai-manifest.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_FILE="${2:?Missing finding JSON file}"

TARGET_FILE="$REPO_PATH/0-AI-MANIFEST.a2ml"

echo "=== Missing AI Manifest Fix ==="
echo "  Repo: $REPO_PATH"

# Idempotency: skip if already exists (check both naming variants)
if [[ -f "$TARGET_FILE" ]] || [[ -f "$REPO_PATH/AI.a2ml" ]]; then
    echo "  SKIP: AI manifest already exists"
    exit 0
fi

# Write template content
cat > "$TARGET_FILE" <<'MANIFEST'
# SPDX-License-Identifier: PMPL-1.0-or-later
# 0-AI-MANIFEST.a2ml — Universal AI Agent Entry Point

[manifest]
version = "0.2"
format = "a2ml"

[canonical-locations]
machine-readable = ".machine_readable/"
state = ".machine_readable/6a2/STATE.a2ml"
meta = ".machine_readable/6a2/META.a2ml"
ecosystem = ".machine_readable/6a2/ECOSYSTEM.a2ml"
contractiles = ".machine_readable/contractiles/"
bot-directives = ".machine_readable/bot_directives/"

[invariants]
scm-files-location = ".machine_readable/ ONLY — never root"
container-file = "Containerfile — never Dockerfile"
package-manager = "Deno preferred — never npm"
license = "PMPL-1.0-or-later"

[session-startup]
step-1 = "Read this manifest"
step-2 = "Read STATE.a2ml for current project state"
step-3 = "Read META.a2ml for architecture decisions"
step-4 = "Read ECOSYSTEM.a2ml for ecosystem position"
MANIFEST

echo "  CREATED 0-AI-MANIFEST.a2ml"
echo ""
echo "Fixed: 1 file created"
exit 0
