#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# fix-dependabot-coverage.sh
#
# Scans a repository for package manager files (Cargo.toml, package.json,
# mix.exs, etc.) in subdirectories and ensures .github/dependabot.yml has
# matching directory entries. Also removes semver-patch ignore rules that
# block security patch updates.
#
# Usage:
#   fix-dependabot-coverage.sh [repo-path]
#
# If repo-path is omitted, the current directory is used.

set -euo pipefail

# --- Configuration ---

REPO_ROOT="${1:-.}"
DEPENDABOT_FILE="${REPO_ROOT}/.github/dependabot.yml"

# Map of filename -> dependabot ecosystem name
declare -A ECOSYSTEM_MAP=(
  ["Cargo.toml"]="cargo"
  ["package.json"]="npm"
  ["mix.exs"]="mix"
  ["go.mod"]="gomod"
  ["Gemfile"]="bundler"
  ["requirements.txt"]="pip"
  ["setup.py"]="pip"
  ["pyproject.toml"]="pip"
  ["build.gradle"]="gradle"
  ["build.gradle.kts"]="gradle"
  ["pom.xml"]="maven"
  ["composer.json"]="composer"
  ["flake.nix"]="nix"
  ["pubspec.yaml"]="pub"
)

# --- Validation ---

if [[ ! -d "${REPO_ROOT}" ]]; then
  echo "ERROR: Repository path '${REPO_ROOT}' does not exist." >&2
  exit 1
fi

if [[ ! -f "${DEPENDABOT_FILE}" ]]; then
  echo "ERROR: ${DEPENDABOT_FILE} not found. Create it first." >&2
  exit 1
fi

# --- Functions ---

# Check whether a given ecosystem+directory pair already exists in dependabot.yml.
# Uses a simple text-based check: looks for the ecosystem line followed by
# the directory line within a few lines of each other.
entry_exists() {
  local ecosystem="$1"
  local directory="$2"

  # Normalise directory for matching: dependabot uses "/" for root,
  # "/subdir" for subdirectories.
  local dir_pattern
  if [[ "${directory}" == "/" ]]; then
    dir_pattern='directory: "/"'
  else
    dir_pattern="directory: \"${directory}\""
  fi

  # Search for the ecosystem+directory pair within the file.
  # We look for both on adjacent lines (within 5 lines of each other).
  if awk -v eco="\"${ecosystem}\"" -v dir="${dir_pattern}" '
    /package-ecosystem:/ && $0 ~ eco { found_eco=NR }
    found_eco && NR <= found_eco+5 && $0 ~ dir { exit 0 }
    END { exit 1 }
  ' "${DEPENDABOT_FILE}"; then
    return 0
  else
    return 1
  fi
}

# Append a new ecosystem+directory entry to dependabot.yml.
append_entry() {
  local ecosystem="$1"
  local directory="$2"

  cat >> "${DEPENDABOT_FILE}" <<EOF

  - package-ecosystem: "${ecosystem}"
    directory: "${directory}"
    schedule:
      interval: "daily"
EOF
  echo "  ADDED: ${ecosystem} @ ${directory}"
}

# Remove semver-patch ignore rules from dependabot.yml.
# Matches the block:
#   ignore:
#     - dependency-name: "*"
#       update-types: ["version-update:semver-patch"]
remove_semver_patch_ignores() {
  local tmp_file
  tmp_file="$(mktemp)"

  # Use awk to strip ignore blocks that contain semver-patch.
  # This handles the 3-line ignore block pattern.
  awk '
    /^[[:space:]]*ignore:/ {
      # Buffer lines to check if this is a semver-patch ignore block
      buf[0] = $0; n = 1
      while ((getline line) > 0) {
        buf[n++] = line
        # If we see semver-patch, this entire ignore block should be removed
        if (line ~ /version-update:semver-patch/) {
          # Skip this entire block (do not print buffered lines)
          removed = 1
          break
        }
        # If we hit a non-indented line or a new key at the same level,
        # this is not a simple semver-patch block - print and move on
        if (line !~ /^[[:space:]]+(- dependency-name:|update-types:)/) {
          for (i = 0; i < n; i++) print buf[i]
          removed = 0
          break
        }
      }
      if (!removed && n > 0) {
        # End of file reached while buffering - print everything
        for (i = 0; i < n; i++) print buf[i]
      }
      next
    }
    { print }
  ' "${DEPENDABOT_FILE}" > "${tmp_file}"

  if ! diff -q "${DEPENDABOT_FILE}" "${tmp_file}" > /dev/null 2>&1; then
    mv "${tmp_file}" "${DEPENDABOT_FILE}"
    echo "  REMOVED: semver-patch ignore rule(s)"
  else
    rm -f "${tmp_file}"
  fi
}

# --- Main ---

echo "=== fix-dependabot-coverage ==="
echo "Repository: $(cd "${REPO_ROOT}" && pwd)"
echo ""

# Step 1: Remove semver-patch ignore rules
echo "--- Checking for semver-patch ignore rules ---"
remove_semver_patch_ignores

# Step 2: Find all package manager files and ensure coverage
echo ""
echo "--- Scanning for package manager files ---"

changes_made=0

for pkg_file in "${!ECOSYSTEM_MAP[@]}"; do
  ecosystem="${ECOSYSTEM_MAP[${pkg_file}]}"

  # Find all instances of this package file in the repo,
  # excluding common non-project directories.
  while IFS= read -r found_path; do
    # Determine the directory relative to repo root
    dir="$(dirname "${found_path}")"
    rel_dir="${dir#"${REPO_ROOT}"}"

    # Skip common non-project directories
    if [[ "${rel_dir}" == *"/node_modules/"* ]] || \
       [[ "${rel_dir}" == *"/target/"* ]] || \
       [[ "${rel_dir}" == *"/vendor/"* ]] || \
       [[ "${rel_dir}" == *"/.git/"* ]] || \
       [[ "${rel_dir}" == *"/deps/"* ]] || \
       [[ "${rel_dir}" == *"/_build/"* ]] || \
       [[ "${rel_dir}" == *"/.lake/"* ]] || \
       [[ "${rel_dir}" == *"/build/"* ]]; then
      continue
    fi

    # Normalise: empty relative dir becomes "/"
    if [[ -z "${rel_dir}" ]]; then
      rel_dir="/"
    fi

    # Check if this entry already exists
    if entry_exists "${ecosystem}" "${rel_dir}"; then
      echo "  OK: ${ecosystem} @ ${rel_dir}"
    else
      append_entry "${ecosystem}" "${rel_dir}"
      changes_made=$((changes_made + 1))
    fi
  done < <(find "${REPO_ROOT}" -name "${pkg_file}" -not -path "*/.git/*" -not -path "*/node_modules/*" -not -path "*/target/*" -not -path "*/vendor/*" -not -path "*/_build/*" -not -path "*/.lake/*" 2>/dev/null)
done

echo ""
if [[ ${changes_made} -gt 0 ]]; then
  echo "=== Done: ${changes_made} entries added ==="
else
  echo "=== Done: dependabot.yml already has full coverage ==="
fi
