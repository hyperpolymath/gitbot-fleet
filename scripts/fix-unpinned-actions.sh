#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Fix unpinned GitHub Actions by pinning to SHA

set -euo pipefail

REPO_PATH="$1"
FINDING_FILE="$2"

echo "Fixing unpinned actions in $REPO_PATH..."

# Common action SHA pins (updated 2026-02-04)
declare -A ACTION_PINS=(
    ["actions/checkout@v4"]="34e114876b0b11c390a56381ad16ebd13914f8d5"
    ["actions/checkout@v5"]="93cb6efe18208431cddfb8368fd83d5badbf9bfd"
    ["github/codeql-action/init@v3"]="6624720a57d4c312633c7b953db2f2da5bcb4c3a"
    ["github/codeql-action/analyze@v3"]="6624720a57d4c312633c7b953db2f2da5bcb4c3a"
    ["github/codeql-action/upload-sarif@v3"]="6624720a57d4c312633c7b953db2f2da5bcb4c3a"
    ["ossf/scorecard-action@v2.4.0"]="62b2cac7ed8198b15735ed49ab1e5cf35480ba46"
    ["trufflesecurity/trufflehog@main"]="7ee2e0fdffec27d19ccbb8fb3dcf8a83b9d7f9e8"
    ["editorconfig-checker/action-editorconfig-checker@main"]="4054fa83a075fdf090bd098bdb1c09aaf64a4169"
    ["dtolnay/rust-toolchain@stable"]="4be9e76fd7c4901c61fb841f559994984270fce7"
    ["Swatinem/rust-cache@v2"]="779680da715d629ac1d338a641029a2f4372abb5"
    ["codecov/codecov-action@v5"]="671740ac38dd9b0130fbe1cec585b89eea48d3de"
    ["slsa-framework/slsa-github-generator@v2.1.0"]="f7dd8c54c2067bafc12ca7a55595d5ee9b75204a"
    ["webfactory/ssh-agent@v0.9.0"]="dc588b651fe13675774614f8e6a936a468676387"
    ["ocaml/setup-ocaml@v3"]="dec6499fef64fc5d7ed43d43a87251b7b1c306f5"
    ["softprops/action-gh-release@v2"]="a06a81a03ee405af7f2048a818ed3f03bbf83c7b"
    ["actions/configure-pages@v5"]="983d7736d9b0ae728b81ab479565c72886d7745b"
    ["actions/jekyll-build-pages@v1"]="44a6e6beabd48582f863aeeb6cb2151cc1716697"
    ["actions/upload-pages-artifact@v3"]="56afc609e74202658d3ffba0e8f6dda462b719fa"
    ["actions/deploy-pages@v4"]="d6db90164ac5ed86f2b6aed7e0febac5b3c0c03e"
    ["ruby/setup-ruby@v1"]="09a7688d3b55cf0e976497ff046b70949eeaccfd"
    ["actions/upload-artifact@v4"]="65c79d7f54e76e4e3c7a8f34db0f4ac8b515c478"
)

# Find all workflow files in repo
FIXED_COUNT=0
while IFS= read -r -d '' workflow; do
    rel_path="${workflow#$REPO_PATH/}"
    changed=false

    for action_version in "${!ACTION_PINS[@]}"; do
        SHA="${ACTION_PINS[$action_version]}"
        ACTION_NAME="${action_version%@*}"
        VERSION="${action_version#*@}"

        # Replace unpinned action with SHA-pinned version
        # Match both "uses: action@version" and "uses: action@version # comment"
        if grep -q "uses: ${action_version}" "$workflow" 2>/dev/null; then
            sed -i "s|uses: ${action_version}|uses: ${ACTION_NAME}@${SHA} # ${VERSION}|g" "$workflow"
            echo "  Pinned ${ACTION_NAME} (${VERSION}) in ${rel_path}"
            changed=true
        fi
    done

    if [[ "$changed" == "true" ]]; then
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH/.github/workflows" -type f \( -name "*.yml" -o -name "*.yaml" \) -print0 2>/dev/null)

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Pinned actions in $FIXED_COUNT workflow(s)"
else
    echo "No unpinned actions found"
fi
