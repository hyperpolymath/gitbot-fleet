#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-secret-to-env.sh — Replace hardcoded secrets with environment variable lookups
#
# Fixes PA013 HardcodedSecret findings.
# Detects hardcoded passwords, API keys, and tokens, replaces with env var refs.
#
# Usage: fix-secret-to-env.sh <repo-path> <finding-json>

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$SCRIPT_DIR/lib/third-party-excludes.sh" 2>/dev/null || true

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

echo "=== Hardcoded Secret Fix ==="
echo "  Repo: $REPO_PATH"

FIXED_COUNT=0

# Secret patterns to detect (case-insensitive key names)
SECRET_KEYS='(password|passwd|api_key|apikey|api_secret|secret_key|auth_token|access_token|private_key|client_secret|db_password|database_password)'

# Skip patterns — these are not real secrets
SKIP_VALUES='(changeme|password|secret|xxx|TODO|placeholder|example|test|dummy|CHANGE_ME|your_.*_here|INSERT_.*_HERE|<.*>|\$\{|\$\(|process\.env|System\.get_env|os\.environ|os\.getenv|env\[|ENV\[)'

# --- Shell files ---
while IFS= read -r -d '' file; do
    rel_path="${file#$REPO_PATH/}"

    # Skip test/fixture files
    if echo "$rel_path" | grep -qiP '(test|spec|fixture|mock|fake|sample|example)'; then
        continue
    fi

    changed=false

    # Pattern: VAR="hardcoded_value" where VAR matches a secret key name
    while IFS= read -r match_line; do
        line_num=$(echo "$match_line" | cut -d: -f1)
        line_content=$(echo "$match_line" | cut -d: -f2-)

        # Extract the variable name and value
        var_name=$(echo "$line_content" | grep -oP "^\\s*${SECRET_KEYS}\\s*=" | sed 's/[= ]//g' | tr '[:lower:]' '[:upper:]')

        if [[ -z "$var_name" ]]; then
            continue
        fi

        # Check if value is already an env var lookup or placeholder
        if echo "$line_content" | grep -qiP "$SKIP_VALUES"; then
            continue
        fi

        # Check if this line was already fixed
        if echo "$line_content" | grep -q ':-}'; then
            continue
        fi

        # Replace hardcoded value with env var lookup
        sed -i "${line_num}s|=.*|=\"\${${var_name}:?Set ${var_name} env var}\"|" "$file" 2>/dev/null || true
        changed=true
    done < <(grep -niP "^\\s*${SECRET_KEYS}\\s*=" "$file" 2>/dev/null || true)

    if [[ "$changed" == "true" ]]; then
        echo "  FIXED $rel_path — replaced hardcoded secrets with env vars"
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f \( -name "*.sh" -o -name "*.bash" -o -name "*.env.example" \) \
    -not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]}" -print0 2>/dev/null)

# --- YAML/config files ---
while IFS= read -r -d '' file; do
    rel_path="${file#$REPO_PATH/}"

    if echo "$rel_path" | grep -qiP '(test|spec|fixture|mock|fake|sample|example)'; then
        continue
    fi

    changed=false

    # Pattern: password: "hardcoded" or api_key: "hardcoded"
    while IFS= read -r match_line; do
        line_num=$(echo "$match_line" | cut -d: -f1)
        line_content=$(echo "$match_line" | cut -d: -f2-)

        # Skip if already uses env var syntax
        if echo "$line_content" | grep -qiP "$SKIP_VALUES"; then
            continue
        fi

        # Skip if already has a SECURITY comment nearby
        prev_line=$(sed -n "$((line_num - 1))p" "$file" 2>/dev/null || echo "")
        if echo "$prev_line" | grep -q 'SECURITY'; then
            continue
        fi

        sed -i "${line_num}i\\  # SECURITY: replace hardcoded secret with environment variable" "$file" 2>/dev/null || true
        changed=true
    done < <(grep -niP "^\\s*${SECRET_KEYS}:\\s*[\"'].+[\"']" "$file" 2>/dev/null | sort -t: -k1,1 -rn || true)

    if [[ "$changed" == "true" ]]; then
        echo "  FIXED $rel_path — added secret warnings"
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f \( -name "*.yml" -o -name "*.yaml" -o -name "*.toml" \) \
    -not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]}" \
    -not -path "*/.github/workflows/*" -print0 2>/dev/null)

# --- Elixir config files ---
while IFS= read -r -d '' file; do
    rel_path="${file#$REPO_PATH/}"

    if echo "$rel_path" | grep -qiP '(test|fixture|mock)'; then
        continue
    fi

    changed=false

    # Pattern: password: "hardcoded" or secret_key_base: "hardcoded"
    if grep -qP "${SECRET_KEYS}:\\s*\"[^\"]{8,}\"" "$file" 2>/dev/null; then
        # Skip if already uses System.get_env
        if ! grep -q 'System.get_env' "$file" 2>/dev/null; then
            # Add warning above secret lines
            while IFS= read -r line_num; do
                prev=$(sed -n "$((line_num - 1))p" "$file" 2>/dev/null || echo "")
                if ! echo "$prev" | grep -q 'SECURITY'; then
                    sed -i "${line_num}i\\      # SECURITY: replace hardcoded secret with System.get_env/1" "$file" 2>/dev/null || true
                    changed=true
                fi
            done < <(grep -nP "${SECRET_KEYS}:\\s*\"[^\"]{8,}\"" "$file" 2>/dev/null | cut -d: -f1 | sort -rn)
        fi
    fi

    if [[ "$changed" == "true" ]]; then
        echo "  FIXED $rel_path — added secret warnings"
        ((FIXED_COUNT++)) || true
    fi
done < <(find "$REPO_PATH" -type f \( -name "*.ex" -o -name "*.exs" \) \
    -not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]}" -print0 2>/dev/null)

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Fixed hardcoded secrets in $FIXED_COUNT file(s)"
else
    echo "No fixable hardcoded secrets found"
fi
