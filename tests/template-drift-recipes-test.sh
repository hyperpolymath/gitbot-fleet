#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Contract test of the 3 template-drift recipes
# (campaigns/TEMPLATE-DRIFT-CAMPAIGN.adoc). For each recipe: the predicate
# finds the old shape, the repair gives the exact expected text, a second run
# changes nothing, and a file that is not a target is not changed.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

fail() { echo "FAIL: $*" >&2; exit 1; }

# new_repo <name>: make an empty fixture repository and print its path.
new_repo() { mkdir -p "$TMP/$1/.github/workflows"; printf '%s\n' "$TMP/$1"; }

# expect_count <recipe> <repo> <number>: the predicate prints that number.
expect_count() {
    local got
    got=$(bash "$ROOT/scripts/$1" --check "$2")
    [[ "$got" == "$3" ]] || fail "$1 --check ${2##*/}: expected $3, got $got"
}

# expect_same <label> <file> <expected-file>
expect_same() { cmp -s "$2" "$3" || { diff "$3" "$2" >&2 || true; fail "$1"; }; }

# expect_refused <recipe> <repo> <file>: exit 1, and the file is not changed.
expect_refused() {
    cp "$3" "$TMP/before"
    if bash "$ROOT/scripts/$1" "$2" >/dev/null 2>&1; then fail "$1 ${2##*/}: reported a repair that it cannot make"; fi
    expect_same "$1 ${2##*/}: changed a file that it refused" "$3" "$TMP/before"
}

# expect_untouched <recipe> <repo> <file>: no target, exit 0, file not changed.
expect_untouched() {
    cp "$3" "$TMP/before"
    expect_count "$1" "$2" 0
    bash "$ROOT/scripts/$1" "$2" >/dev/null || fail "$1 ${2##*/}: failed on a file that is not a target"
    expect_same "$1 ${2##*/}: changed a file that is not a target" "$3" "$TMP/before"
}

# repair <recipe> <repo> <file> <targets> <expected-file>: full repair cycle.
repair() {
    expect_count "$1" "$2" "$4"
    bash "$ROOT/scripts/$1" "$2" >/dev/null || fail "$1 ${2##*/}: repair failed"
    expect_same "$1 ${2##*/}: wrong repair" "$3" "$5"
    expect_untouched "$1" "$2" "$3"
}

# ── Registry routes and the predicate contract ──────────────────────────────
# shellcheck source=scripts/lib/outcome-verdict.sh
source "$ROOT/scripts/lib/outcome-verdict.sh"
EMPTY=$(new_repo empty)
while read -r recipe script; do
    jq -e --arg k "$recipe" --arg v "$script" '.registry.by_recipe[$k] == $v' \
        "$ROOT/scripts/fix-script-registry.json" >/dev/null || fail "registry does not route $recipe to $script"
    [[ -x "$ROOT/scripts/$script" ]] || fail "$script is not executable"
    [[ "$(recipe_check_count "$ROOT/scripts/$script" "$EMPTY")" == 0 ]] || fail "$script: no usable --check predicate"
    bash "$ROOT/scripts/$script" "$EMPTY" >/dev/null || fail "$script: failed on a repository with no workflow"
done <<'ROUTES'
recipe-workflow-linter-spdx-rule fix-workflow-linter-spdx-rule.sh
recipe-sonarqube-token-guard fix-sonarqube-token-guard.sh
recipe-workflow-split-comment-keys fix-split-comment-keys.sh
ROUTES

# ── fix-workflow-linter-spdx-rule.sh ────────────────────────────────────────
R=fix-workflow-linter-spdx-rule.sh
REPO=$(new_repo spdx-old); F="$REPO/.github/workflows/workflow-linter.yml"
cat > "$F" <<'YAML'
      - name: Check SPDX Headers
        run: |
          for file in .github/workflows/*.yml; do
            if ! head -1 "$file" | grep -q "^# SPDX-License-Identifier:"; then
              echo "ERROR: $file missing SPDX header"
            fi
          done
          echo "Add '# SPDX-License-Identifier: MPL-2.0' as first line"
YAML
cat > "$TMP/expected" <<'YAML'
      - name: Check SPDX Headers
        run: |
          for file in .github/workflows/*.yml; do
            # actions-lock may prepend its own comment. Require the licence
            # in the leading comment block, before the workflow body.
            if ! awk '/^# SPDX-License-Identifier:/ { found=1 } /^[^#[:space:]]/ { exit } END { exit !found }' "$file"; then
              echo "ERROR: $file missing SPDX header"
            fi
          done
          echo "Add '# SPDX-License-Identifier: MPL-2.0' to the leading comment block"
YAML
repair "$R" "$REPO" "$F" 1 "$TMP/expected"

# The new rule does what the recipe says: the tool comment on line 1 passes,
# and a file with no licence line, or with the line after the body, fails.
RULE=$(sed -n "s/^ *if ! awk '\(.*\)' \"\$file\"; then\$/\1/p" "$F")
[[ -n "$RULE" ]] || fail "$R: cannot read the new rule from the repaired file"
printf '# managed by a tool\n# SPDX-License-Identifier: MPL-2.0\nname: a\n' > "$TMP/s1"
printf '# no licence\nname: a\n' > "$TMP/s2"
printf '# comment\nname: a\n# SPDX-License-Identifier: MPL-2.0\n' > "$TMP/s3"
awk "$RULE" "$TMP/s1" || fail "$R: the new rule refuses a licence on line 2"
if awk "$RULE" "$TMP/s2"; then fail "$R: the new rule accepts a file with no licence"; fi
if awk "$RULE" "$TMP/s3"; then fail "$R: the new rule accepts a licence after the body"; fi

# A linter with a different rule is not a target.
REPO=$(new_repo spdx-other); F="$REPO/.github/workflows/workflow-linter.yml"
printf '%s\n' '            if ! head -5 "$f" | grep -q "SPDX-License-Identifier"; then' > "$F"
expect_untouched "$R" "$REPO" "$F"

# A symlink is not followed.
REPO=$(new_repo spdx-link)
printf '%s\n' '            if ! head -1 "$file" | grep -q "^# SPDX-License-Identifier:"; then' > "$TMP/outside.yml"
ln -s "$TMP/outside.yml" "$REPO/.github/workflows/workflow-linter.yml"
expect_untouched "$R" "$REPO" "$TMP/outside.yml"

# ── fix-sonarqube-token-guard.sh ────────────────────────────────────────────
R=fix-sonarqube-token-guard.sh
REPO=$(new_repo sonar-old); F="$REPO/.github/workflows/sonarqube.yml"
cat > "$F" <<'YAML'
# SPDX-License-Identifier: MPL-2.0
name: SonarQube
on:
  pull_request:
permissions:
  contents: read
jobs:
  sonarqube:
    name: SonarQube
    runs-on: ubuntu-latest
    timeout-minutes: 15
    steps:
      - name: Checkout
        uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1 # v7.0.1
        with:
          fetch-depth: 0   # full history for accurate new-code detection
      - name: SonarQube Scan
        uses: SonarSource/sonarqube-scan-action@e805d875275f728fac1885b413934be6d7c63f65 # v8.4.1
        env:
          SONAR_TOKEN: ${{ secrets.SONAR_TOKEN }}
YAML
cp "$F" "$TMP/sonar-old.orig"
cat > "$TMP/expected" <<'YAML'
# SPDX-License-Identifier: MPL-2.0
name: SonarQube
on:
  pull_request:
permissions:
  contents: read
jobs:
  sonarqube:
    name: SonarQube
    runs-on: ubuntu-latest
    timeout-minutes: 15
    steps:
      - name: Report configuration state
        id: cfg
        # A job-level `if:` cannot read a secret. This step reads the token
        # and gives the result to the steps after it. The token is in the
        # environment of this step and of the scan step only.
        env:
          SONAR_TOKEN: ${{ secrets.SONAR_TOKEN }}
        # The script of this step is Bash. `shell: bash` says so in the step:
        # a default shell or a runner with a different shell cannot change it.
        shell: bash
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
      - name: Checkout
        if: steps.cfg.outputs.configured == 'true'
        uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1 # v7.0.1
        with:
          fetch-depth: 0   # full history for accurate new-code detection
      - name: SonarQube Scan
        if: steps.cfg.outputs.configured == 'true'
        uses: SonarSource/sonarqube-scan-action@e805d875275f728fac1885b413934be6d7c63f65 # v8.4.1
        env:
          SONAR_TOKEN: ${{ secrets.SONAR_TOKEN }}
YAML
repair "$R" "$REPO" "$F" 1 "$TMP/expected"

# The older name of the scan action is the same shape: repaired, name not changed.
REPO=$(new_repo sonar-legacy-action); F="$REPO/.github/workflows/sonarqube.yml"
sed 's#/sonarqube-scan-action@#/sonarcloud-github-action@#' "$TMP/sonar-old.orig" > "$F"
sed 's#/sonarqube-scan-action@#/sonarcloud-github-action@#' "$TMP/expected" > "$TMP/expected-legacy"
grep -q 'sonarcloud-github-action@' "$TMP/expected-legacy" || fail "sonar-legacy-action: the test case is the fixture"
repair "$R" "$REPO" "$F" 1 "$TMP/expected-legacy"

# The old template with one difference is not the old template: refused, and
# the file is not changed. Each case is one change of the fixture.
refuse_variant() {  # <name> <awk-program>
    local repo file
    repo=$(new_repo "sonar-$1"); file="$repo/.github/workflows/sonarqube.yml"
    awk "$2" "$TMP/sonar-old.orig" > "$file"
    cmp -s "$file" "$TMP/sonar-old.orig" && fail "$R sonar-$1: the test case is the fixture"
    expect_count "$R" "$repo" 1
    expect_refused "$R" "$repo" "$file"
}
# One more step, with the step identifier that the guard uses.
refuse_variant step-id-cfg '{ print } /^    steps:$/ { print "      - name: Other"; print "        id: cfg"; print "        run: echo x" }'
# A step name that is a block scalar.
refuse_variant block-name '/^      - name: Checkout$/ { print "      - name: >"; print "          Checkout"; next } { print }'
# The token below `with:` and not below `env:`.
refuse_variant token-in-with '/^        env:$/ { print "        with:"; next } { print }'
# A runner that is not Ubuntu.
refuse_variant windows-runner '/^    runs-on: / { print "    runs-on: windows-latest"; next } { print }'
# A label that starts with `ubuntu` and is not a hosted Ubuntu version.
refuse_variant custom-runner '/^    runs-on: / { print "    runs-on: ubuntu-windows"; next } { print }'
refuse_variant bare-runner '/^    runs-on: / { print "    runs-on: ubuntu"; next } { print }'
# A default shell for the workflow.
refuse_variant default-shell '/^jobs:$/ { print "defaults:"; print "  run:"; print "    shell: pwsh" } { print }'
# The same key in the quoted form. Each top-level key that is not a key of the
# old template is refused, in any form.
refuse_variant quoted-defaults '/^jobs:$/ { print "\"defaults\":"; print "  run:"; print "    shell: pwsh" } { print }'

# The guard step does what the recipe says, with and with no token.
sed -n '/^        run: |$/,/^          fi$/p' "$F" | sed '1d; s/^          //' > "$TMP/guard.sh"
SONAR_TOKEN='' GITHUB_OUTPUT="$TMP/out-empty" bash "$TMP/guard.sh" >/dev/null
grep -qx 'configured=false' "$TMP/out-empty" || fail "$R: the guard does not report a missing token"
SONAR_TOKEN='x' GITHUB_OUTPUT="$TMP/out-set" bash "$TMP/guard.sh" >/dev/null
grep -qx 'configured=true' "$TMP/out-set" || fail "$R: the guard does not report a present token"

# An unguarded scan with a different shape (2 jobs) is refused, not half-edited.
REPO=$(new_repo sonar-two-jobs); F="$REPO/.github/workflows/sonarqube.yml"
cat > "$F" <<'YAML'
jobs:
  sonarqube:
    runs-on: ubuntu-latest
    steps:
      - name: SonarQube Scan
        uses: SonarSource/sonarqube-scan-action@e805d875275f728fac1885b413934be6d7c63f65 # v8.4.1
        env:
          SONAR_TOKEN: ${{ secrets.SONAR_TOKEN }}
  other:
    runs-on: ubuntu-latest
    steps:
      - name: Other
        run: echo other
YAML
expect_count "$R" "$REPO" 1
expect_refused "$R" "$REPO" "$F"
expect_count "$R" "$REPO" 1

# The token must not be in the environment of the job or of the checkout step.
if grep -qE '^    env:' "$TMP/sonar-old/.github/workflows/sonarqube.yml"; then
    fail "$R: the repair puts an environment on the job"
fi
[[ "$(grep -c 'secrets.SONAR_TOKEN' "$TMP/sonar-old/.github/workflows/sonarqube.yml")" == 2 ]] \
    || fail "$R: the token is not in exactly 2 steps (the guard and the scan)"

# The scan step uses another credential, and the secret is named in another
# step: refused, because the secret is not in the scan step.
REPO=$(new_repo sonar-other-credential); F="$REPO/.github/workflows/sonarqube.yml"
cat > "$F" <<'YAML'
jobs:
  sonarqube:
    runs-on: ubuntu-latest
    steps:
      - name: Note
        run: echo "the old name was secrets.SONAR_TOKEN"
      - name: SonarQube Scan
        uses: SonarSource/sonarqube-scan-action@e805d875275f728fac1885b413934be6d7c63f65 # v8.4.1
        env:
          SONAR_TOKEN: ${{ vars.OTHER_TOKEN }}
YAML
expect_count "$R" "$REPO" 1
expect_refused "$R" "$REPO" "$F"

# A scan with an `if:` line can have a guard of a person: not a target.
REPO=$(new_repo sonar-own-guard); F="$REPO/.github/workflows/sonarqube.yml"
cat > "$F" <<'YAML'
jobs:
  sonarqube:
    runs-on: ubuntu-latest
    steps:
      - name: SonarQube Scan
        if: env.SONAR_TOKEN != ''
        uses: SonarSource/sonarqube-scan-action@e805d875275f728fac1885b413934be6d7c63f65 # v8.4.1
        env:
          SONAR_TOKEN: ${{ secrets.SONAR_TOKEN }}
YAML
expect_untouched "$R" "$REPO" "$F"

# ── fix-split-comment-keys.sh ───────────────────────────────────────────────
R=fix-split-comment-keys.sh
REPO=$(new_repo split-old); F="$REPO/.github/workflows/e2e.yml"
cat > "$F" <<'YAML'
# SPDX-License-Identifier: MPL-2.0
name: E2E
#
jobs:
# The line above is a legal top-level key. It is not a target.
  smoke:
    runs-on: ubuntu-latest
    steps:
      - run: echo ok
# e2e:
#   runs-on: ubuntu-latest
#
strategy:
#     matrix:
#       os: [a, b]
#
steps:
#     - uses: actions/checkout@v4
#
with:
#         fetch-depth: 0
#     - run: bash tests/e2e.sh
YAML
cat > "$TMP/expected" <<'YAML'
# SPDX-License-Identifier: MPL-2.0
name: E2E
#
jobs:
# The line above is a legal top-level key. It is not a target.
  smoke:
    runs-on: ubuntu-latest
    steps:
      - run: echo ok
# e2e:
#   runs-on: ubuntu-latest
#   strategy:
#     matrix:
#       os: [a, b]
#   steps:
#     - uses: actions/checkout@v4
#       with:
#         fetch-depth: 0
#     - run: bash tests/e2e.sh
YAML
repair "$R" "$REPO" "$F" 3 "$TMP/expected"

# A split key with no usable next line is refused, and the file is not changed.
REPO=$(new_repo split-no-next); F="$REPO/.github/workflows/e2e.yml"
printf '%s\n' 'name: E2E' '#' 'steps:' '#' > "$F"
expect_count "$R" "$REPO" 1
expect_refused "$R" "$REPO" "$F"

# One file that cannot be repaired refuses the repository: no file is changed.
REPO=$(new_repo split-partial); F="$REPO/.github/workflows/a.yml"
printf '%s\n' '#' 'steps:' '#     - run: x' > "$F"
printf '%s\n' 'name: E2E' '#' 'steps:' '#' > "$REPO/.github/workflows/b.yml"
expect_count "$R" "$REPO" 2
expect_refused "$R" "$REPO" "$F"

# A symlink is not followed.
REPO=$(new_repo split-link)
printf '%s\n' '#' 'steps:' '#     - run: x' > "$TMP/outside-e2e.yml"
ln -s "$TMP/outside-e2e.yml" "$REPO/.github/workflows/e2e.yml"
expect_untouched "$R" "$REPO" "$TMP/outside-e2e.yml"

echo 'PASS: registry routes, predicates, exact repairs, idempotence, refusals and symlink rejection for 3 recipes'
