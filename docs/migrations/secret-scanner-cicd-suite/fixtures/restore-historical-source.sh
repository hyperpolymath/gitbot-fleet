#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Build the archived 4404c3c proposal in a disposable tree only.
set -euo pipefail
tree=${1:?disposable tree required}
fixture=${2:?historical source JSON required}
manifest="$tree/scripts/lib/pinned-reusable-sources.json"
workflow="$tree/.github/workflows/secret-scanner.yml"
receipts="$tree/scripts/lib/lock-closure-receipts.tsv"
old='hyperpolymath/cicd-suite/.github/workflows/secret-scanner-reusable.yml@4404c3c0474e17753fe7689f3485c02890aac113'
new='hyperpolymath/proven-cicd-and-workflows/.github/workflows/secret-scanner-reusable.yml@1fd5e6371a50f0fc3687206a5adca0cdf7b39aa6'
# Refuse anything other than the known deployed successor; do not silently
# reinterpret a later caller or a damaged source catalog as this old plan.
OLD_CALL="$old" NEW_CALL="$new" python3 - "$workflow" <<'PY'
import os
import pathlib
import sys
p = pathlib.Path(sys.argv[1])
s = p.read_text()
old, new = os.environ['OLD_CALL'], os.environ['NEW_CALL']
if s.count(new) != 1 or old in s:
    raise SystemExit('historical projection: unexpected scanner caller')
p.write_text(s.replace(new, old))
PY
jq -e '[.sources[] | select(.repository=="hyperpolymath/proven-cicd-and-workflows" and .commit=="1fd5e6371a50f0fc3687206a5adca0cdf7b39aa6" and .role=="active")] | length == 1' "$manifest" >/dev/null
jq --slurpfile prior "$fixture" '
  .sources |= (map(select(.repository != "hyperpolymath/proven-cicd-and-workflows" or .commit != "1fd5e6371a50f0fc3687206a5adca0cdf7b39aa6")) + [$prior[0].source]) |
  del(.dependencies["hyperpolymath/proven-cicd-and-workflows@1fd5e6371a50f0fc3687206a5adca0cdf7b39aa6"]) |
  .dependencies["hyperpolymath/cicd-suite@4404c3c0474e17753fe7689f3485c02890aac113"] = $prior[0].dependency
' "$manifest" > "$manifest.tmp"
mv "$manifest.tmp" "$manifest"
# The archived proposal has its own source receipt; keep unrelated receipts.
awk '$0 !~ /^# Reviewed source: hyperpolymath\/proven-cicd-and-workflows\// && $0 !~ /^hyperpolymath\/proven-cicd-and-workflows\t/' "$receipts" > "$receipts.tmp"
printf '%s\n' '# Reviewed historical cicd-suite scanner source' >> "$receipts.tmp"
printf 'hyperpolymath/cicd-suite\t4404c3c0474e17753fe7689f3485c02890aac113\tactions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1\n' >> "$receipts.tmp"
mv "$receipts.tmp" "$receipts"
