#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-elixir-ssl-verify.sh — Replace verify: :verify_none with :verify_peer
#
# SSL verify_none disables TLS certificate validation entirely, allowing
# MITM attacks. This script adds :verify_peer with either ca_store (CAStore
# hex package) or the system-provided cacerts fallback.
#
# NOTE: This fix requires manual review — the correct cacertfile depends on
# whether the app uses CAStore, :certifi, or the OTP built-in :public_key
# cacerts. The script flags occurrences and substitutes a safe template;
# the developer must verify the CA source is appropriate for their use case.
#
# Usage: fix-elixir-ssl-verify.sh <repo-path> <finding-json>

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$SCRIPT_DIR/lib/third-party-excludes.sh" 2>/dev/null || true

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON}"

FIND_EXCLUDES=(-not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]:-}")

EX_FILES=()
while IFS= read -r -d '' f; do
    EX_FILES+=("$f")
done < <(find "$REPO_PATH" -type f \( -name "*.ex" -o -name "*.exs" \) "${FIND_EXCLUDES[@]}" -print0 2>/dev/null)

if [[ ${#EX_FILES[@]} -eq 0 ]]; then
    echo "[fix-elixir-ssl-verify] No Elixir files found."
    exit 0
fi

FLAGGED=0

for file in "${EX_FILES[@]}"; do
    if ! grep -qP 'verify:\s*:verify_none|ssl:\s*\[verify:\s*:verify_none\]' "$file" 2>/dev/null; then
        continue
    fi

    rel="${file#"$REPO_PATH"/}"
    echo ""
    echo "  [WARNING] SSL verify_none found in: $rel"
    echo "  Lines:"
    grep -n 'verify:\s*:verify_none' "$file" | head -5 | sed 's/^/    /'

    tmpfile=$(mktemp)
    # Replace standalone verify: :verify_none
    sed 's/verify: :verify_none/verify: :verify_peer, cacertfile: CAStore.file_path()  # REVIEW: set appropriate CA source/g' "$file" > "$tmpfile"

    if ! diff -q "$file" "$tmpfile" >/dev/null 2>&1; then
        cp "$tmpfile" "$file"
        echo "  -> Substituted verify: :verify_peer + CAStore reference in $rel"
        echo "     REVIEW REQUIRED: confirm CA source (CAStore, :certifi, or :public_key.cacerts_get/0)"
        ((FLAGGED++)) || true
    fi
    rm -f "$tmpfile"
done

echo ""
if [[ "$FLAGGED" -gt 0 ]]; then
    echo "Modified $FLAGGED file(s). Manual review required before merging."
    echo "Recommended: add {:ca_store, \"~> 0.1\"} to mix.exs deps if not present."
else
    echo "No verify: :verify_none patterns found."
fi
