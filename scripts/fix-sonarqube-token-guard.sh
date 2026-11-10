#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# fix-sonarqube-token-guard.sh: add the SONAR_TOKEN guard to sonarqube.yml
#
# Older copies of the estate template run the SonarQube scan with no test of
# the token. In a repository with no SONAR_TOKEN secret, the scan fails on
# each run ("Not authorized or organization not found"). That red check is not
# a code-quality signal: the scan did not run.
#
# The current template (rsr-template-repo, since 2026-07-28) has a guard: the
# first step tests the token. With no token, the job says that the scan is not
# configured and passes. With a token, the scan runs, with no further edit.
# This recipe adds that guard.
#
# One difference from the template: the template puts the token in the
# environment of the job. This recipe puts it in the environment of the guard
# step only. The scan step keeps its own entry. No other step, for example
# the checkout, can then read the secret.
#
# Target: .github/workflows/sonarqube.yml that uses a SonarSource scan action
# with `secrets.SONAR_TOKEN` and has no `if:` line. A file with an `if:` line
# is not a target, because a person can have written a different guard.
#
# The recipe edits only the old template: the `jobs:` section must be the text
# of the old template, line for line. Only these can differ: the reference of
# the 2 actions, the version of the Ubuntu runner, the value of the timeout,
# and the comment on `fetch-depth`. Before `jobs:`, the workflow can have only
# the top-level keys of the old template: `name`, `on`, `concurrency` and
# `permissions`. A target with any other difference is not changed, and the
# recipe exits 1 so that the run does not read as a repair.
#
# Idempotent: a file that has the guard is not a target.
#
# Usage: fix-sonarqube-token-guard.sh <repo-path> [finding-json]
#        fix-sonarqube-token-guard.sh --check <repo-path>
# outcome-predicate: --check

set -euo pipefail

REL=".github/workflows/sonarqube.yml"
ME="fix-sonarqube-token-guard"

# Print the path of the workflow when it is a regular file inside the repository.
# A missing file, a symlink, or a path that leaves the repository is no target.
target_file() {
    local repo="$1" root
    root=$(realpath -e -- "$repo") || return 1
    [[ -f "$root/$REL" && ! -L "$root/$REL" ]] || return 1
    [[ "$(realpath -e -- "$root/$REL")" == "$root/$REL" ]] || return 1
    printf '%s\n' "$root/$REL"
}

# Print 1 when the workflow scans with secrets.SONAR_TOKEN and has no `if:`
# line, else 0.
count_targets() {
    local file
    file=$(target_file "$1") || { echo 0; return 0; }
    if grep -qE '^[[:space:]]*(- )?uses:[[:space:]]*SonarSource/(sonarqube-scan-action|sonarcloud-github-action)@' "$file" &&
        grep -qF 'secrets.SONAR_TOKEN' "$file" &&
        ! grep -qE '^[[:space:]]*(- )?if:' "$file"; then
        echo 1
    else
        echo 0
    fi
}

# The `jobs:` section of the old template, in a neutral form: no action
# reference, no runner version, no timeout value, no comment on `fetch-depth`.
OLD_JOBS=$(cat <<'YAML'
jobs:
  sonarqube:
    name: SonarQube
    runs-on: ubuntu
    timeout-minutes: N
    steps:
      - name: Checkout
        uses: actions/checkout@REF
        with:
          fetch-depth: 0
      - name: SonarQube Scan
        uses: SonarSource/sonarqube-scan-action@REF
        env:
          SONAR_TOKEN: ${{ secrets.SONAR_TOKEN }}
YAML
)

# Print the `jobs:` section of a workflow in the neutral form.
neutral_jobs() {
    sed -n '/^jobs:[[:space:]]*$/,$p' "$1" | sed -E \
        -e 's/[[:space:]]+$//' \
        -e 's/^( +uses:[[:space:]]*[^@[:space:]]+)@.*$/\1@REF/' \
        -e 's/^(    runs-on:[[:space:]]*ubuntu)-[A-Za-z0-9.]+$/\1/' \
        -e 's/^(    timeout-minutes:[[:space:]]*)[0-9]+$/\1N/' \
        -e 's/^(          fetch-depth: 0)[[:space:]]+#.*$/\1/'
}

# Succeed when each top-level key before `jobs:` is a key of the old template.
# This is an allow list: a key in any other form, for example `defaults:` or
# `"defaults":` (a default shell), is refused.
known_header() {
    awk '
        /^jobs:[[:space:]]*$/ { exit }
        /^[[:space:]#]/ || /^$/ || /^(name|on|concurrency|permissions):/ { next }
        { bad = 1; exit }
        END { exit bad }
    ' "$1"
}

# Succeed when the workflow IS the old template: the known header, and then
# the `jobs:` section. This is an exact comparison: a workflow with one more
# step, another runner, another step name or another place for the token is
# not the old template, and it is refused.
known_shape() {
    known_header "$1" && [[ "$(neutral_jobs "$1")" == "$OLD_JOBS" ]]
}

if [[ "${1:-}" == "--check" ]]; then
    [[ -d "${2:-}" ]] || { echo "--check: not a directory: ${2:-}" >&2; exit 2; }
    count_targets "$2"
    exit 0
fi

REPO_PATH="${1:?Usage: $0 <repo-path> [finding-json]}"
[[ -d "$REPO_PATH" ]] || { echo "$ME: not a directory: $REPO_PATH" >&2; exit 2; }

if [[ "$(count_targets "$REPO_PATH")" == 0 ]]; then
    echo "$ME: no unguarded scan in $REL (nothing to do)"
    exit 0
fi

FILE=$(target_file "$REPO_PATH")
if ! known_shape "$FILE"; then
    echo "$ME: $REL does not have the shape of the old template; not changed" >&2
    exit 1
fi

TMP=$(mktemp)
trap 'rm -f "$TMP"' EXIT

GUARD=$(cat <<'YAML'
    steps:
      - name: Report configuration state
        id: cfg
        # A job-level `if:` cannot read a secret. This step reads the token
        # and gives the result to the steps after it. The token is in the
        # environment of this step and of the scan step only.
        env:
          SONAR_TOKEN: ${{ secrets.SONAR_TOKEN }}
        run: |
          if [ -z "${SONAR_TOKEN}" ]; then
            echo "configured=false" >> "$GITHUB_OUTPUT"
            echo "::notice::SONAR_TOKEN is not configured for this repository - skipping the CI-based scan."
            echo "SonarCloud automatic analysis (check: 'SonarCloud Code Analysis') is unaffected."
            echo "To enable this scan: Settings > Secrets and variables > Actions > new secret SONAR_TOKEN."
          else
            echo "configured=true" >> "$GITHUB_OUTPUT"
            echo "SONAR_TOKEN present - running the CI-based scan."
          fi
YAML
)

GUARD="$GUARD" awk '
    !in_steps && /^    steps:[[:space:]]*$/ { print ENVIRON["GUARD"]; in_steps = 1; next }
    in_steps && /^      - name: /           { print; print "        if: steps.cfg.outputs.configured == \047true\047"; next }
    { print }
' "$FILE" > "$TMP"
[[ -s "$TMP" ]] || { echo "$ME: the edit gave no text; $REL is not changed" >&2; exit 1; }
cat "$TMP" > "$FILE"

if [[ "$(count_targets "$REPO_PATH")" != 0 ]]; then
    echo "$ME: $REL is still a target after the edit" >&2
    exit 1
fi
echo "$ME: added the token guard to $REL"
