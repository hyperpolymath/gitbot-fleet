#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-proven-substitute.sh — Generic proven module substitution script
#
# Takes a finding and generates the import/substitution for a proven safety module.
# Used by rhodibot to create PRs with proven module replacements.
#
# Usage: fix-proven-substitute.sh <repo-path> <finding-json> <proven-module> <language>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json> <proven-module> <language>}"
FINDING_JSON="${2:?Missing finding JSON file}"
PROVEN_MODULE="${3:?Missing proven module name (e.g., SafeCommand)}"
LANGUAGE="${4:?Missing language (e.g., rust, elixir, rescript)}"

PROVEN_BINDINGS_BASE="/var/mnt/eclipse/repos/proven/bindings"

# Extract finding details
FILE=$(jq -r '.file // .location // "unknown"' "$FINDING_JSON")
CATEGORY=$(jq -r '.category // "unknown"' "$FINDING_JSON")
DESCRIPTION=$(jq -r '.description // "unknown"' "$FINDING_JSON")
LINE=$(jq -r '.line // 0' "$FINDING_JSON")

echo "=== proven Module Substitution ==="
echo "  Repository: $REPO_PATH"
echo "  File:       $FILE"
echo "  Category:   $CATEGORY"
echo "  Module:     $PROVEN_MODULE"
echo "  Language:   $LANGUAGE"
echo ""

# Check if binding exists for this language
BINDING_DIR="$PROVEN_BINDINGS_BASE/$LANGUAGE"
if [[ ! -d "$BINDING_DIR" ]]; then
    echo "WARNING: No proven binding found for language '$LANGUAGE'"
    echo "  Expected: $BINDING_DIR"
    echo "  Available bindings:"
    ls "$PROVEN_BINDINGS_BASE/" 2>/dev/null || echo "    (none)"
    echo ""
    echo "ACTION: Manual substitution required"
    echo "  1. Check proven/bindings/ for available languages"
    echo "  2. Use the Zig FFI bridge if no native binding exists"
    echo "  3. Import proven/$PROVEN_MODULE via the FFI layer"
    exit 0
fi

echo "Binding found: $BINDING_DIR"

# Generate language-specific import statement
case "$LANGUAGE" in
    rust)
        echo ""
        echo "SUGGESTED IMPORT:"
        echo "  use proven::${PROVEN_MODULE};"
        echo ""
        echo "CARGO.TOML DEPENDENCY:"
        echo "  [dependencies]"
        echo "  proven = { path = \"/var/mnt/eclipse/repos/proven/bindings/rust\" }"
        ;;

    elixir)
        echo ""
        echo "SUGGESTED IMPORT:"
        echo "  alias Proven.${PROVEN_MODULE}"
        echo ""
        echo "MIX.EXS DEPENDENCY:"
        echo "  {:proven, path: \"/var/mnt/eclipse/repos/proven/bindings/elixir\"}"
        ;;

    rescript)
        echo ""
        echo "SUGGESTED IMPORT:"
        echo "  @module(\"proven/${PROVEN_MODULE}\") external ${PROVEN_MODULE}: 'a = \"default\""
        echo ""
        echo "Or via Deno:"
        echo "  import { ${PROVEN_MODULE} } from \"proven/${PROVEN_MODULE}\""
        ;;

    shell|bash)
        echo ""
        echo "SHELL SUBSTITUTION:"
        echo "  Source the proven shell wrapper:"
        echo "  . /var/mnt/eclipse/repos/proven/bindings/bash/proven.sh"
        echo "  ${PROVEN_MODULE}_call \"\$@\""
        ;;

    zig)
        echo ""
        echo "SUGGESTED IMPORT:"
        echo "  const proven = @import(\"proven\");"
        echo "  const ${PROVEN_MODULE} = proven.${PROVEN_MODULE};"
        ;;

    *)
        echo ""
        echo "GENERIC SUBSTITUTION:"
        echo "  Import proven/${PROVEN_MODULE} using the $LANGUAGE binding"
        echo "  Binding dir: $BINDING_DIR"
        ;;
esac

echo ""
echo "FINDING CONTEXT:"
echo "  $DESCRIPTION"
echo "  At: $FILE:$LINE"
echo ""
echo "SAFETY TRIANGLE: SUBSTITUTE tier"
echo "  Replace unsafe pattern with formally verified proven/$PROVEN_MODULE"
