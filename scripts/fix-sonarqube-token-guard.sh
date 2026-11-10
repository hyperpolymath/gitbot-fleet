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
# Target: .github/workflows/sonarqube.yml that uses a SonarSource scan action
# with `secrets.SONAR_TOKEN` and has no `if:` line. A file with an `if:` line
# is not a target, because a person can have written a different guard.
#
# The recipe edits only the shape of the old template: 1 job, a `steps:` line
# with 4 spaces, each step with `- name:` and 6 spaces, and no `env:` line at
# the job level. A target with a different shape is not changed, and the
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

# Succeed when the file has the shape of the old template.
known_shape() {
    local file="$1"
    awk '
        /^jobs:[[:space:]]*$/                           { in_jobs = 1; next }
        in_jobs && /^  [A-Za-z0-9_-]+:[[:space:]]*$/    { jobs++ }
        in_jobs && /^    steps:[[:space:]]*$/           { steps_lines++; in_steps = 1; next }
        in_jobs && !in_steps && /^    env:/             { job_env++ }
        in_steps && /^      - /                         { items++ }
        in_steps && /^      - name: /                   { named++ }
        in_steps && /^ ? ? ? ? ?[^ #]/                    { after_steps++ }
        END { exit !(jobs == 1 && steps_lines == 1 && job_env == 0 && items >= 1 && items == named && after_steps == 0) }
    ' "$file"
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
    # Secrets cannot be referenced from a job-level `if:`, so the presence test
    # is carried through an env var set at job scope and read in a first step.
    env:
      SONAR_TOKEN: ${{ secrets.SONAR_TOKEN }}
    steps:
      - name: Report configuration state
        id: cfg
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
cat "$TMP" > "$FILE"

if [[ "$(count_targets "$REPO_PATH")" != 0 ]]; then
    echo "$ME: $REL is still a target after the edit" >&2
    exit 1
fi
echo "$ME: added the token guard to $REL"
