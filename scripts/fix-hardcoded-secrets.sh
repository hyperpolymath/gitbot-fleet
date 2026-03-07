#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-hardcoded-secrets.sh — Replace hardcoded secrets with env var lookups
#
# Detects hardcoded passwords, API keys, tokens, and secrets in source files
# and replaces them with environment variable references.
#
# Language-specific replacements:
#   Shell:  PASSWORD="hardcoded" → PASSWORD="${PASSWORD:?Set PASSWORD env var}"
#   Elixir: password: "hardcoded" → password: System.get_env("PASSWORD") || raise "..."
#   JS:     const apiKey = "hardcoded" → const apiKey = process.env.API_KEY || ...
#
# Skips test files, fixtures, and obvious placeholder values.
#
# Usage: fix-hardcoded-secrets.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"

DESCRIPTION=$(jq -r '.description // ""' "$FINDING_JSON")
PATTERN_ID=$(jq -r '.pattern_id // ""' "$FINDING_JSON")

echo "=== Hardcoded Secret Fix ==="
echo "  Repo:    $REPO_PATH"
echo "  Pattern: $PATTERN_ID"
echo ""

# Directories to skip
SKIP_DIRS=(".git" "target" "node_modules" "_build" ".lake")

FIND_EXCLUDES=()
for d in "${SKIP_DIRS[@]}"; do
    FIND_EXCLUDES+=(-not -path "*/${d}/*")
done

FIXED_COUNT=0

# ---------------------------------------------------------------------------
# Helper: Check if a file is a test/fixture file that should be skipped
# ---------------------------------------------------------------------------
is_test_file() {
    local filepath="$1"
    local basename
    basename=$(basename "$filepath")
    local dirpath
    dirpath=$(dirname "$filepath")

    # Skip by filename patterns
    case "$basename" in
        *_test.* | *_spec.* | *Test.* | *Spec.* | *.test.* | *.spec.*)
            return 0 ;;
    esac

    # Skip by directory patterns
    case "$dirpath" in
        */test/* | */tests/* | */spec/* | */specs/* | */fixtures/* | \
        */testdata/* | */__tests__/* | */__mocks__/* | */test_helpers/*)
            return 0 ;;
    esac

    return 1
}

# ---------------------------------------------------------------------------
# Helper: Check if a value is an obvious placeholder (not a real secret)
# ---------------------------------------------------------------------------
is_placeholder() {
    local value="$1"
    # Lowercase for comparison
    local lower
    lower=$(echo "$value" | tr '[:upper:]' '[:lower:]')

    case "$lower" in
        "" | "changeme" | "xxx" | "xxxx" | "xxxxx" | "todo" | "placeholder" | \
        "replace_me" | "replace-me" | "your_"* | "your-"* | "example" | \
        "test" | "testing" | "dummy" | "sample" | "demo" | "default" | \
        "none" | "null" | "nil" | "empty" | "fake" | "mock" | \
        "password" | "secret" | "token" | "api_key" | "apikey" | \
        "<"*">" | "{"*"}" | "change_me" | "change-me" | "insert_"* | \
        "insert-"* | "put_"* | "put-"* | "set_"* | "set-"*)
            return 0 ;;
    esac

    # Skip very short values (1-2 chars) — unlikely to be real secrets
    if [[ ${#value} -le 2 ]]; then
        return 0
    fi

    return 1
}

# ---------------------------------------------------------------------------
# Helper: Convert a secret key name to an env var name
# e.g., apiKey → API_KEY, api_key → API_KEY, password → PASSWORD
# ---------------------------------------------------------------------------
to_env_var_name() {
    local name="$1"
    # Insert underscore before uppercase letters (camelCase → camel_Case)
    local result
    result=$(echo "$name" | sed 's/\([a-z]\)\([A-Z]\)/\1_\2/g')
    # Uppercase everything, replace hyphens with underscores
    result=$(echo "$result" | tr '[:lower:]-' '[:upper:]_')
    # Remove non-alphanumeric/underscore chars
    result=$(echo "$result" | sed 's/[^A-Z0-9_]//g')
    echo "$result"
}

# ---------------------------------------------------------------------------
# Phase 1: Shell scripts (.sh, .bash, .env)
# ---------------------------------------------------------------------------
# Pattern: VAR_NAME="literal_value" where VAR_NAME contains secret-like words
SHELL_FILES=()
while IFS= read -r -d '' f; do
    SHELL_FILES+=("$f")
done < <(find "$REPO_PATH" -type f \( -name "*.sh" -o -name "*.bash" \) \
    "${FIND_EXCLUDES[@]}" -print0 2>/dev/null)

for file in "${SHELL_FILES[@]}"; do
    if is_test_file "$file"; then
        continue
    fi
    if file "$file" | grep -q "binary"; then
        continue
    fi

    rel_path="${file#"$REPO_PATH"/}"
    CHANGES=0

    # Match: SECRET_LIKE_NAME="hardcoded value"
    # Secret-like names: *PASSWORD*, *SECRET*, *TOKEN*, *API_KEY*, *APIKEY*, *PRIVATE_KEY*
    while IFS= read -r line_num; do
        # Extract the line
        line=$(sed -n "${line_num}p" "$file")

        # Extract variable name and value
        var_name=$(echo "$line" | grep -oP '^[A-Za-z_][A-Za-z0-9_]*(?==)' 2>/dev/null || true)
        # Extract the quoted value
        value=$(echo "$line" | grep -oP '=\s*"([^"]*)"' 2>/dev/null | sed 's/^=\s*"//;s/"$//' || true)

        if [[ -z "$var_name" || -z "$value" ]]; then
            continue
        fi

        # Skip if already an env var reference
        if echo "$value" | grep -qP '^\$\{' 2>/dev/null; then
            continue
        fi

        # Skip placeholders
        if is_placeholder "$value"; then
            continue
        fi

        env_name=$(to_env_var_name "$var_name")

        # Replace the line (idempotent — won't match if already using ${...})
        escaped_value=$(printf '%s\n' "$value" | sed 's/[&/\]/\\&/g')
        sed -i "s|^${var_name}=\"${escaped_value}\"|${var_name}=\"\${${env_name}:?Set ${env_name} env var}\"|" "$file"
        ((CHANGES++)) || true

    done < <(grep -nP '^[A-Za-z_]*(?i)(password|passwd|secret|token|api_key|apikey|private_key|auth_key)[A-Za-z_]*\s*=\s*"[^"$]' "$file" 2>/dev/null | cut -d: -f1 || true)

    if [[ "$CHANGES" -gt 0 ]]; then
        ((FIXED_COUNT++)) || true
        echo "  [shell] Fixed $CHANGES hardcoded secret(s) in $rel_path"
    fi
done

# ---------------------------------------------------------------------------
# Phase 2: Elixir files (.ex, .exs)
# ---------------------------------------------------------------------------
# Pattern: password: "hardcoded", secret: "hardcoded", token: "hardcoded"
ELIXIR_FILES=()
while IFS= read -r -d '' f; do
    ELIXIR_FILES+=("$f")
done < <(find "$REPO_PATH" -type f \( -name "*.ex" -o -name "*.exs" \) \
    "${FIND_EXCLUDES[@]}" -print0 2>/dev/null)

for file in "${ELIXIR_FILES[@]}"; do
    if is_test_file "$file"; then
        continue
    fi

    rel_path="${file#"$REPO_PATH"/}"
    CHANGES=0

    # Match keyword-style: password: "value", secret_key: "value", etc.
    while IFS= read -r line_num; do
        line=$(sed -n "${line_num}p" "$file")

        # Extract key name (atom-style, before the colon)
        key_name=$(echo "$line" | grep -oP '[a-z_]*(?i)(password|passwd|secret|token|api_key|apikey|private_key|auth_key)[a-z_]*(?=:\s*")' 2>/dev/null || true)
        # Extract value
        value=$(echo "$line" | grep -oP ':\s*"([^"]*)"' 2>/dev/null | sed 's/^:\s*"//;s/"$//' || true)

        if [[ -z "$key_name" || -z "$value" ]]; then
            continue
        fi

        # Skip if already a System.get_env call
        if echo "$line" | grep -q "System.get_env" 2>/dev/null; then
            continue
        fi

        if is_placeholder "$value"; then
            continue
        fi

        env_name=$(to_env_var_name "$key_name")
        escaped_value=$(printf '%s\n' "$value" | sed 's/[&/\]/\\&/g')

        # Replace: key: "value" → key: System.get_env("ENV_NAME") || raise "Set ENV_NAME env var"
        sed -i "s|${key_name}: \"${escaped_value}\"|${key_name}: System.get_env(\"${env_name}\") \|\| raise \"Set ${env_name} env var\"|" "$file"
        ((CHANGES++)) || true

    done < <(grep -nP '[a-z_]*(password|passwd|secret|token|api_key|apikey|private_key|auth_key)[a-z_]*:\s*"[^"$]' "$file" 2>/dev/null | cut -d: -f1 || true)

    if [[ "$CHANGES" -gt 0 ]]; then
        ((FIXED_COUNT++)) || true
        echo "  [elixir] Fixed $CHANGES hardcoded secret(s) in $rel_path"
    fi
done

# ---------------------------------------------------------------------------
# Phase 3: JavaScript / ReScript-generated JS (.js, .mjs)
# ---------------------------------------------------------------------------
# Pattern: const apiKey = "hardcoded"; let password = "hardcoded"
JS_FILES=()
while IFS= read -r -d '' f; do
    JS_FILES+=("$f")
done < <(find "$REPO_PATH" -type f \( -name "*.js" -o -name "*.mjs" \) \
    "${FIND_EXCLUDES[@]}" -print0 2>/dev/null)

for file in "${JS_FILES[@]}"; do
    if is_test_file "$file"; then
        continue
    fi
    if file "$file" | grep -q "binary"; then
        continue
    fi

    rel_path="${file#"$REPO_PATH"/}"
    CHANGES=0

    # Match: const/let/var secretName = "value"
    while IFS= read -r line_num; do
        line=$(sed -n "${line_num}p" "$file")

        # Extract variable name
        var_name=$(echo "$line" | grep -oP '(?:const|let|var)\s+\K[a-zA-Z_][a-zA-Z0-9_]*(?=\s*=\s*")' 2>/dev/null || true)
        # Extract value
        value=$(echo "$line" | grep -oP '=\s*"([^"]*)"' 2>/dev/null | sed 's/^=\s*"//;s/"$//' || true)

        if [[ -z "$var_name" || -z "$value" ]]; then
            continue
        fi

        # Skip if already using process.env
        if echo "$line" | grep -q "process\.env" 2>/dev/null; then
            continue
        fi

        if is_placeholder "$value"; then
            continue
        fi

        env_name=$(to_env_var_name "$var_name")
        escaped_value=$(printf '%s\n' "$value" | sed 's/[&/\]/\\&/g')

        # Build replacement — use IIFE for throw expression
        # const apiKey = "val" → const apiKey = process.env.API_KEY || (() => { throw new Error("Set API_KEY env var"); })()
        sed -i "s|\\(\\(const\\|let\\|var\\)\\s\\+${var_name}\\s*=\\s*\\)\"${escaped_value}\"|\\1process.env.${env_name} \\|\\| (() => { throw new Error(\"Set ${env_name} env var\"); })()|" "$file"
        ((CHANGES++)) || true

    done < <(grep -nP '(?:const|let|var)\s+[a-zA-Z_]*(?i)(password|passwd|secret|token|api_key|apikey|private_key|auth_key)[a-zA-Z_]*\s*=\s*"[^"$]' "$file" 2>/dev/null | cut -d: -f1 || true)

    if [[ "$CHANGES" -gt 0 ]]; then
        ((FIXED_COUNT++)) || true
        echo "  [js] Fixed $CHANGES hardcoded secret(s) in $rel_path"
    fi
done

echo ""
if [[ "$FIXED_COUNT" -gt 0 ]]; then
    echo "Fixed $FIXED_COUNT file(s)"
else
    echo "No fixable patterns found (may need manual review)"
fi
