#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
# fix-spdx-double-suffix.sh — Repair SPDX identifiers with duplicated
# `-or-later` (or `+`) clauses.
#
# Class of regression observed 2026-05-27 in
# `the-nash-equilibrium/pull/41` and likely elsewhere: a deployed
# rhodibot replace rule applied `s/AGPL-3.0/AGPL-3.0-or-later/`
# without a word-boundary check, producing
# `AGPL-3.0-or-later-or-later` when the file already carried the
# `-or-later` suffix. This script:
#
#   1. Collapses `-or-later-or-later` -> `-or-later` everywhere it
#      appears in SPDX-License-Identifier lines.
#   2. Collapses `+` short-form double-suffix forms (e.g.
#      `AGPL-3.0-or-later+`, `AGPL-3.0+-or-later`) -> `-or-later`.
#   3. Leaves all other SPDX identifiers untouched.
#
# Hypatia rule pairing: ERR-LIC-001 (Hypatia.Rules.CicdRules
# validate_license/2 returns `{:error, :spdx_double_suffix, …}`).

set -euo pipefail

REPO_PATH="${1:-.}"
FINDING_FILE="${2:-}"

if [[ ! -d "$REPO_PATH" ]]; then
    echo "ERROR: Not a directory: $REPO_PATH" >&2
    exit 1
fi

cd "$REPO_PATH"

# If a finding-file was provided, fix just that file; otherwise scan all
# tracked files for the malformed pattern.
if [[ -n "$FINDING_FILE" && -f "$FINDING_FILE" ]]; then
    FILES=$(jq -r '.location.file' "$FINDING_FILE")
else
    # Repo-wide scan — tracked files only, skip binaries by relying on
    # grep -I.
    mapfile -t FILES < <(
        git ls-files -z |
        xargs -0 grep -lI -E "SPDX-License-Identifier:MPL-2.0*(-or-later-or-later|-or-later\+|\+-or-later)" 2>/dev/null || true
    )
fi

if [[ ${#FILES[@]} -eq 0 ]]; then
    echo "OK: no SPDX double-suffix occurrences found in $REPO_PATH"
    exit 0
fi

CHANGED=0
for f in "${FILES[@]}"; do
    [[ -z "$f" || ! -f "$f" ]] && continue

    # Three repairs, applied only on lines that match
    # SPDX-License-Identifier: — never globally, to avoid breaking
    # unrelated text or test fixtures.
    if grep -qE "SPDX-License-Identifier:MPL-2.0*(-or-later-or-later|-or-later\+|\+-or-later)" "$f"; then
        # Use a temp file (sed -i portability across BSD/GNU)
        tmp=$(mktemp)
        sed -E \
            -e '/SPDX-License-Identifier:/ s/-or-later-or-later/-or-later/g' \
            -e '/SPDX-License-Identifier:/ s/-or-later\+/-or-later/g' \
            -e '/SPDX-License-Identifier:/ s/\+-or-later/-or-later/g' \
            "$f" > "$tmp"
        if ! cmp -s "$f" "$tmp"; then
            mv "$tmp" "$f"
            echo "FIXED  $f"
            CHANGED=$((CHANGED + 1))
        else
            rm -f "$tmp"
        fi
    fi
done

echo ""
echo "SUMMARY: $CHANGED file(s) repaired in $REPO_PATH"
exit 0
