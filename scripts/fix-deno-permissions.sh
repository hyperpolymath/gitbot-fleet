#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-deno-permissions.sh — Replace --allow-all with least-privilege Deno permissions
#
# Category: ExcessivePermissions
# Usage: fix-deno-permissions.sh <repo-path> <finding-json>
#
# Targets:
#   - deno.json / deno.jsonc: replaces --allow-all in task definitions
#   - Shell scripts (*.sh): replaces `deno run --allow-all`
#   - Justfile / Makefile: replaces `deno run --allow-all`
#
# Idempotent: only modifies files that contain --allow-all patterns.
# Does NOT commit — dispatch-runner handles that.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$SCRIPT_DIR/lib/third-party-excludes.sh" 2>/dev/null || true

REPO_PATH="${1:?Usage: fix-deno-permissions.sh <repo-path> <finding-json>}"
FINDING_JSON="${2:?Usage: fix-deno-permissions.sh <repo-path> <finding-json>}"

# Least-privilege replacement for --allow-all
LEAST_PRIV="--allow-read --allow-write --allow-net"

MODIFIED=0

# --- Fix deno.json and deno.jsonc ---
for deno_config in "${REPO_PATH}/deno.json" "${REPO_PATH}/deno.jsonc"; do
  if [[ -f "${deno_config}" ]]; then
    if grep -q '\-\-allow-all' "${deno_config}"; then
      sed -i 's/--allow-all/--allow-read --allow-write --allow-net/g' "${deno_config}"
      echo "[fix-deno-permissions] Fixed: ${deno_config}"
      MODIFIED=$((MODIFIED + 1))
    fi
  fi
done

# --- Fix shell scripts (.sh files, excluding .git/ and node_modules/) ---
while IFS= read -r -d '' script_file; do
  if grep -q 'deno run --allow-all' "${script_file}" 2>/dev/null; then
    sed -i "s|deno run --allow-all|deno run ${LEAST_PRIV}|g" "${script_file}"
    echo "[fix-deno-permissions] Fixed: ${script_file}"
    MODIFIED=$((MODIFIED + 1))
  fi
  # Also catch `deno run -A` (short form of --allow-all)
  if grep -q 'deno run -A' "${script_file}" 2>/dev/null; then
    sed -i "s|deno run -A|deno run ${LEAST_PRIV}|g" "${script_file}"
    echo "[fix-deno-permissions] Fixed (short form): ${script_file}"
    MODIFIED=$((MODIFIED + 1))
  fi
done < <(find "${REPO_PATH}" -name '*.sh' -type f \
  -not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]}" -print0 2>/dev/null)

# --- Fix Justfile and Makefile ---
for build_file in "${REPO_PATH}/justfile" "${REPO_PATH}/Justfile" \
                  "${REPO_PATH}/Makefile" "${REPO_PATH}/makefile"; do
  if [[ -f "${build_file}" ]]; then
    if grep -q 'deno run --allow-all\|deno run -A' "${build_file}" 2>/dev/null; then
      sed -i "s|deno run --allow-all|deno run ${LEAST_PRIV}|g" "${build_file}"
      sed -i "s|deno run -A|deno run ${LEAST_PRIV}|g" "${build_file}"
      echo "[fix-deno-permissions] Fixed: ${build_file}"
      MODIFIED=$((MODIFIED + 1))
    fi
  fi
done

# --- Fix deno task invocations in nested deno.json files (one level deep) ---
while IFS= read -r -d '' nested_config; do
  if grep -q '\-\-allow-all\|-A' "${nested_config}" 2>/dev/null; then
    sed -i 's/--allow-all/--allow-read --allow-write --allow-net/g' "${nested_config}"
    # Only replace standalone -A flag (preceded by space, followed by space or quote)
    sed -i 's/\(deno run\) -A\b/\1 --allow-read --allow-write --allow-net/g' "${nested_config}"
    echo "[fix-deno-permissions] Fixed nested: ${nested_config}"
    MODIFIED=$((MODIFIED + 1))
  fi
done < <(find "${REPO_PATH}" \
  \( -name 'deno.json' -o -name 'deno.jsonc' \) \
  -not -path "${REPO_PATH}/deno.json" \
  -not -path "${REPO_PATH}/deno.jsonc" \
  -not -path "*/.git/*" "${FIND_THIRD_PARTY_EXCLUDES[@]}" \
  -type f -print0 2>/dev/null)

if [[ ${MODIFIED} -eq 0 ]]; then
  echo "[fix-deno-permissions] No --allow-all patterns found — nothing to fix."
else
  echo "[fix-deno-permissions] Fixed ${MODIFIED} file(s)."
fi

exit 0
