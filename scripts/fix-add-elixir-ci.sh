#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-add-elixir-ci.sh — Add GitHub Actions CI workflow for Elixir/Phoenix projects
#
# Triggered when: mix.exs present, no CI workflow found running mix test.
#
# Generated workflow runs on ubuntu-latest with Erlang/OTP + Elixir via
# erlef/setup-beam, SHA-pinned. Includes:
#   - mix deps.get
#   - mix compile --warnings-as-errors
#   - mix test
#   - mix credo (if credo dep detected)
#   - mix dialyzer (if dialyxir dep detected)
#
# Idempotent: skips if elixir.yml / mix.yml already exists.
# Does NOT commit — dispatch-runner handles that.
#
# Usage: fix-add-elixir-ci.sh <repo-path> <finding-json>

set -euo pipefail

REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON}"

WORKFLOWS_DIR="${REPO_PATH}/.github/workflows"

# Idempotency check
for wf in elixir.yml mix.yml beam.yml elixir-ci.yml; do
    if [[ -f "${WORKFLOWS_DIR}/${wf}" ]]; then
        echo "[fix-add-elixir-ci] ${wf} already exists — skipping."
        exit 0
    fi
done
# Also check for any workflow containing mix test
if [[ -d "${WORKFLOWS_DIR}" ]] && grep -rl 'mix test' "${WORKFLOWS_DIR}/" >/dev/null 2>&1; then
    echo "[fix-add-elixir-ci] Workflow with 'mix test' already exists — skipping."
    exit 0
fi

mkdir -p "${WORKFLOWS_DIR}"

# Detect optional tooling from mix.exs
MIX_EXS="${REPO_PATH}/mix.exs"
HAS_CREDO=false
HAS_DIALYXIR=false
if [[ -f "${MIX_EXS}" ]]; then
    grep -q 'credo' "${MIX_EXS}" && HAS_CREDO=true || true
    grep -q 'dialyxir\|dialyzir' "${MIX_EXS}" && HAS_DIALYXIR=true || true
fi

# Detect OTP/Elixir version hints from .tool-versions or mix.exs
OTP_VERSION="27"
ELIXIR_VERSION="1.17"
if [[ -f "${REPO_PATH}/.tool-versions" ]]; then
    otp_line=$(grep '^erlang ' "${REPO_PATH}/.tool-versions" | awk '{print $2}' | cut -d. -f1 || true)
    elixir_line=$(grep '^elixir ' "${REPO_PATH}/.tool-versions" | awk '{print $2}' | cut -d- -f1 || true)
    [[ -n "${otp_line}" ]] && OTP_VERSION="${otp_line}"
    [[ -n "${elixir_line}" ]] && ELIXIR_VERSION="${elixir_line}"
fi

# Build optional steps
OPTIONAL_STEPS=""
if [[ "${HAS_CREDO}" == "true" ]]; then
    OPTIONAL_STEPS="${OPTIONAL_STEPS}
      - name: Credo static analysis
        run: mix credo --strict
"
fi
if [[ "${HAS_DIALYXIR}" == "true" ]]; then
    OPTIONAL_STEPS="${OPTIONAL_STEPS}
      - name: Dialyzer type checking
        run: mix dialyzer
"
fi

cat > "${WORKFLOWS_DIR}/elixir.yml" << WORKFLOW
# SPDX-License-Identifier: PMPL-1.0-or-later
name: Elixir CI

on:
  push:
    branches: [main, master]
  pull_request:
    branches: [main, master]

permissions:
  contents: read

jobs:
  test:
    name: Build and test
    runs-on: ubuntu-latest

    strategy:
      matrix:
        otp: ['${OTP_VERSION}']
        elixir: ['${ELIXIR_VERSION}']

    steps:
      - uses: actions/checkout@34e114876b0b11c390a56381ad16ebd13914f8d5 # v4

      - name: Set up Elixir
        uses: erlef/setup-beam@5304e04ea2b355f03681464e683d92e3b2f18451 # v1
        with:
          otp-version: \${{ matrix.otp }}
          elixir-version: \${{ matrix.elixir }}

      - name: Restore dependencies cache
        uses: actions/cache@5a3ec84eff668545956fd18022155c47e93e2684 # v4
        with:
          path: deps
          key: \${{ runner.os }}-mix-\${{ hashFiles('**/mix.lock') }}
          restore-keys: \${{ runner.os }}-mix-

      - name: Install dependencies
        run: mix deps.get

      - name: Compile (warnings as errors)
        run: mix compile --warnings-as-errors
${OPTIONAL_STEPS}
      - name: Run tests
        run: mix test
WORKFLOW

echo "[fix-add-elixir-ci] Created ${WORKFLOWS_DIR}/elixir.yml"
echo "  OTP ${OTP_VERSION} / Elixir ${ELIXIR_VERSION}"
[[ "${HAS_CREDO}" == "true" ]] && echo "  + credo step" || true
[[ "${HAS_DIALYXIR}" == "true" ]] && echo "  + dialyzer step" || true
