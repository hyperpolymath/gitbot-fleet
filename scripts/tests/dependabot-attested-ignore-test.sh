#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Every ACTIVE reusable-workflow source attested in
# scripts/lib/pinned-reusable-sources.json must be a name-only Dependabot
# ignore entry in .github/dependabot.yml (github-actions), and no ignore entry
# may name an attested-looking reusable workflow that is not active. Dependabot
# cannot re-attest a pin, so a bump from it breaks 'Pinned source closure' on
# main (#686 did). Such pins move only by a deliberate PR that runs
# scripts/reattest-pinned-source.sh.
#
# Usage: bash scripts/tests/dependabot-attested-ignore-test.sh
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
command -v ruby >/dev/null || { echo "NOT RUN: needs ruby (YAML parser)" >&2; exit 2; }
command -v jq >/dev/null || { echo "NOT RUN: needs jq" >&2; exit 2; }

pass=0 fail=0
ok() { echo "PASS: $1"; pass=$((pass+1)); }
no() { echo "FAIL: $1"; fail=$((fail+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

# drift <dependabot.yml> <manifest.json> -> prints differences, exit 1 if any.
drift() {
  local want have
  want="$(jq -r '.sources[] | select(.role == "active") | "\(.repository)/\(.path)"' "$2" | LC_ALL=C sort -u)"
  have="$(ruby -ryaml -e '
    y = YAML.safe_load(File.read(ARGV[0]))
    (y["updates"] || []).each do |u|
      next unless u["package-ecosystem"] == "github-actions"
      (u["ignore"] || []).each do |i|
        n = i["dependency-name"].to_s
        # A reusable-workflow ignore is any name with a workflow path in it.
        next unless n.include?("/.github/workflows/")
        # Name-only: a versions/update-types scope lets a SHA re-bump through.
        puts "SCOPED\t#{n}" if i["versions"] || i["update-types"]
        puts n
      end
    end' "$1" | LC_ALL=C sort -u)"
  local scoped missing extra
  scoped="$(grep '^SCOPED' <<< "$have" || true)"
  have="$(grep -v '^SCOPED' <<< "$have" || true)"
  missing="$(comm -23 <(printf '%s\n' "$want") <(printf '%s\n' "$have") | grep . || true)"
  extra="$(comm -13 <(printf '%s\n' "$want") <(printf '%s\n' "$have") | grep . || true)"
  [[ -z "$missing$extra$scoped" ]] && return 0
  local n
  while read -r n; do [[ -n "$n" ]] && printf 'missing ignore (attested, Dependabot may bump it): %s\n' "$n"; done <<< "$missing"
  while read -r n; do [[ -n "$n" ]] && printf 'stale ignore (not an active attested source): %s\n' "$n"; done <<< "$extra"
  [[ -n "$scoped" ]] && printf '%s (must be name-only)\n' "$scoped"
  return 1
}

DEP="$ROOT/.github/dependabot.yml" MAN="$ROOT/scripts/lib/pinned-reusable-sources.json"

if out="$(drift "$DEP" "$MAN")"; then ok "the repo's ignore list equals the manifest's active sources"
else no "the repo's ignore list drifted from the manifest:"; printf '  %s\n' "$out"; fi

# Negative controls: the check must catch each kind of drift.
jq '.sources += [{repository: "acme/new", path: ".github/workflows/x.yml", commit: "0000000000000000000000000000000000000000", role: "active"}]' "$MAN" > "$TMP/man-extra.json"
if out="$(drift "$DEP" "$TMP/man-extra.json")"; then no "a newly attested source without an ignore passed"
elif grep -q 'missing ignore.*acme/new/.github/workflows/x.yml' <<< "$out"; then ok "a newly attested source without an ignore is caught"
else no "wrong report for a missing ignore: $out"; fi

jq '(.sources[] | select(.repository == "hyperpolymath/hypatia") | .role) = "retained-review"' "$MAN" > "$TMP/man-retired.json"
if out="$(drift "$DEP" "$TMP/man-retired.json")"; then no "an ignore for a no-longer-active source passed"
elif grep -q 'stale ignore.*hyperpolymath/hypatia' <<< "$out"; then ok "an ignore left behind for a retired source is caught"
else no "wrong report for a stale ignore: $out"; fi

sed 's|dependency-name: "hyperpolymath/hypatia/.github/workflows/credential-read-check.yml",|&\n          versions: [">= 0"],|' "$DEP" > "$TMP/dep-scoped.yml"
if out="$(drift "$TMP/dep-scoped.yml" "$MAN")"; then no "a version-scoped ignore passed"
elif grep -q 'SCOPED.*hypatia.*must be name-only' <<< "$out"; then ok "a version-scoped ignore is caught (a SHA re-bump would slip past it)"
else no "wrong report for a scoped ignore: $out"; fi

echo "dependabot-attested-ignore-test: $pass passed, $fail failed"
[[ "$fail" -eq 0 ]]
