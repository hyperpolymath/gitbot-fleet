#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# third-party-excludes.sh — Shared exclusion list for fix scripts
#
# Source this to get FIND_THIRD_PARTY_EXCLUDES for use with find commands.
# These are directories containing third-party code that must not be modified.

# Third-party / vendored directories to always skip
THIRD_PARTY_SKIP_DIRS=(
    .git target node_modules _build .lake deps
    vendor dist build __pycache__ .tox .mypy_cache
    winget-pkgs
    macports-ports
    compiler-source
    HOL
    .elixir_ls .hex .mix
    coverage .cache .parcel-cache
)

# Build find -not -path exclusions
FIND_THIRD_PARTY_EXCLUDES=()
for d in "${THIRD_PARTY_SKIP_DIRS[@]}"; do
    FIND_THIRD_PARTY_EXCLUDES+=(-not -path "*/${d}/*")
done
