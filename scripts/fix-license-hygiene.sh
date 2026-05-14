#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-license-hygiene.sh — Ensure REUSE-compliant dual-license setup
#
# Applies: LICENSE (MPL-2.0), LICENSES/ dir, NOTICE file
# Idempotent: only modifies what's missing or incorrect.
#
# Requires canonical templates at:
#   /var$REPOS_DIR/palimpsest-license/legal/MPL-2.0.txt
#   /var$REPOS_DIR/palimpsest-license/legal/PALIMPSEST-MPL-1.0.txt
set -euo pipefail

FINDING_FILE="${1:?Usage: fix-license-hygiene.sh <finding.json>}"

if [[ ! -f "$FINDING_FILE" ]]; then
    echo "ERROR: Finding file not found: $FINDING_FILE" >&2
    exit 1
fi

PATTERN=$(jq -r '.pattern' "$FINDING_FILE")
FILE=$(jq -r '.file' "$FINDING_FILE")

# Determine repo root
if [[ -d "$FILE" ]]; then
    REPO_DIR="$FILE"
elif [[ -f "$FILE" ]]; then
    REPO_DIR=$(git -C "$(dirname "$FILE")" rev-parse --show-toplevel 2>/dev/null || echo "")
else
    REPO_DIR=$(echo "$FILE" | sed 's|/LICENSE$||; s|/LICENSES.*||; s|/NOTICE$||')
fi

if [[ -z "$REPO_DIR" || ! -d "$REPO_DIR/.git" ]]; then
    echo "ERROR: Cannot determine git repo root" >&2
    exit 1
fi

cd "$REPO_DIR"

# Template sources
PALIMPSEST_REPO="/var$REPOS_DIR/palimpsest-license"
MPL2_SRC="$PALIMPSEST_REPO/legal/MPL-2.0.txt"
PMPL_SRC="$PALIMPSEST_REPO/legal/PALIMPSEST-MPL-1.0.txt"

# Fallback to boj-server if palimpsest-license not available
if [[ ! -f "$MPL2_SRC" ]]; then
    MPL2_SRC="/var$REPOS_DIR/boj-server/LICENSE"
fi

changes=false

case "$PATTERN" in
    license_not_mpl2|missing_license_file)
        if [[ -f "$MPL2_SRC" ]]; then
            cp "$MPL2_SRC" LICENSE
            git add LICENSE
            changes=true
        fi
        ;;
    missing_licenses_dir|missing_pmpl_text)
        mkdir -p LICENSES
        [[ -f "$MPL2_SRC" && ! -f LICENSES/MPL-2.0.txt ]] && cp "$MPL2_SRC" LICENSES/MPL-2.0.txt
        [[ -f "$PMPL_SRC" && ! -f LICENSES/PMPL-1.0-or-later.txt ]] && cp "$PMPL_SRC" LICENSES/PMPL-1.0-or-later.txt
        git add LICENSES/
        changes=true
        ;;
    missing_notice)
        if [[ ! -f NOTICE ]]; then
            cat > NOTICE << 'NOTICEEOF'
Licensing Notice
================

This project is authored by Jonathan D.A. Jewell (hyperpolymath) and
is licensed under the Palimpsest License (PMPL-1.0-or-later).

The PMPL-1.0-or-later is a philosophical extension of the Mozilla Public
License 2.0, adding provisions for cryptographic provenance, emotional
lineage preservation, and quantum-safe signatures. The full PMPL text is
available in LICENSES/PMPL-1.0-or-later.txt.

For compatibility with automated license detection tools and platforms
that require OSI-approved licenses, the root LICENSE file contains the
standard Mozilla Public License 2.0 text. This ensures that package
registries, CI systems, and other tooling correctly identify the license.

The legally binding terms are:
  - Source files: governed by PMPL-1.0-or-later (per SPDX headers)
  - Combined works: compatible with MPL-2.0 (per PMPL Section 6)

For more information about the Palimpsest License:
  https://github.com/hyperpolymath/palimpsest-license
NOTICEEOF
            git add NOTICE
            changes=true
        fi
        ;;
    *)
        echo "SKIP: Pattern '$PATTERN' not handled by fix-license-hygiene.sh" >&2
        exit 0
        ;;
esac

if [[ "$changes" == true ]] && ! git diff --cached --quiet; then
    git commit -m "$(cat <<EOF
Fix license hygiene: $PATTERN

Applied REUSE-compliant dual-license setup (MPL-2.0 for machine
detection + PMPL-1.0-or-later as governing license).

Co-Authored-By: Hypatia Scanner <hypatia@reposystem.dev>
EOF
)"
    echo "OK: Fixed $PATTERN in $(basename "$REPO_DIR")"
else
    echo "OK: No changes needed (idempotent)"
fi
