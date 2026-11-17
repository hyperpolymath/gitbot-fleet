#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Tests for scripts/fix-a2ml-via-kyx.sh and scripts/lib/a2ml-census.js.
#
# kyX is a STUB here (a Bun script driven by STUB_KYX): these tests pin the
# fixer's gates and outputs, not kyX's translation, which kyX's own acceptance
# suite covers (metadatastician/kyx tests/kyx/run.sh). The canon grammar gate
# uses the REAL standards deed_lint.js: set DEED_LINT, or STANDARDS to a
# metadatastician/standards checkout. Without it the suite does not run and
# exits 2 (NOT RUN), never 0.
#
# Usage: STANDARDS=/path/to/standards bash scripts/tests/fix-a2ml-via-kyx-test.sh
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
FIX="$ROOT/scripts/fix-a2ml-via-kyx.sh"
CENSUS="$ROOT/scripts/lib/a2ml-census.js"
DEED_LINT="${DEED_LINT:-${STANDARDS:+$STANDARDS/1-formats/deed/tools/deed_lint.js}}"
if [[ -z "$DEED_LINT" || ! -f "$DEED_LINT" ]] || ! command -v bun >/dev/null || ! command -v jq >/dev/null; then
  echo "NOT RUN: needs bun, jq and the real standards deed_lint.js (set STANDARDS or DEED_LINT)" >&2
  exit 2
fi
export DEED_LINT
export STANDARDS_COMMIT=b49a33e991f37a2249d873cbdc02c27a08f05d6d
export KYX_COMMIT=731e63ab4c3b02a6e4751de97208ea65eb8bdf6e
export AFFIRMED=2026-11-17

pass=0 fail=0
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
ok() { echo "PASS: $1"; pass=$((pass+1)); }
no() { echo "FAIL: $1"; fail=$((fail+1)); }
check() { if [[ "$2" == "$3" ]]; then ok "$1"; else no "$1 (want ${2@Q}, got ${3@Q})"; fi; }

CHORA=';; SPDX-License-Identifier: MPL-2.0
(repo-deed
  :schema-version "1.0.0"
  :canonical-name "demo"
  :repo-uuid #uuid"00000000-0000-8000-8000-0000000000a1"
  (identity :owner "hyperpolymath" :primary-forge github :prefixed-name "demo"))'

SOURCE='# SPDX-License-Identifier: MPL-2.0
[project]
name = "demo"
purpose = "A demo used by the fixer tests."
[position-in-ecosystem]
ecosystem = "hyperpolymath"
type = "tool"
[[related-projects.projects]]
name = "standards"
relationship = "standard-source"
notes = "Reads its grammar."'

# The deed text is data with literal quotes, exported to the stub as-is.
# shellcheck disable=SC2089,SC2090
GOOD_DEED=';; SPDX-License-Identifier: MPL-2.0
(descriptile-deed
  :schema-version "1.0.0"
  :canonical-name "demo"
  :of-chora #uuid"00000000-0000-8000-8000-0000000000a1"
  :affirmed "2026-11-17" :affirmed-by agent
  :project "demo"
  :ecosystem "hyperpolymath"
  :position-type "tool"
  :purpose "A demo used by the fixer tests."
  (related :name "standards" :relationship standard-source :notes "Reads its grammar."))'

# The stub: STUB_KYX = ok | refuse | drop | lossy | badgrammar | crash
cat > "$TMP/kyx.js" <<'JS'
import { writeFileSync } from "node:fs";
const a = process.argv.slice(2);
const out = a[a.indexOf("--out") + 1];
const mode = process.env.STUB_KYX;
const good = process.env.GOOD_DEED;
if (a[0] !== "a2ml" || a[1] !== "ecosystem" || !a.includes("--chora") || !a.includes("--affirmed")) {
  process.stderr.write("kyx: usage\n"); process.exit(2);
}
if (mode === "refuse") { process.stderr.write("kyx: refused: line 3: [extra] is not in §3\nkyx: 1 reason(s); nothing written\n"); process.exit(1); }
if (mode === "crash") { process.stderr.write("kyx: cannot read source\n"); process.exit(2); }
let text = good;
if (mode === "lossy") text = good.replace('"A demo used by the fixer tests."', '"A demo."');
if (mode === "badgrammar") text = good.replace("standard-source", "depends-on");
writeFileSync(out, text + "\n");
if (mode === "drop") process.stderr.write("kyx: dropped by the drop doctrine (spec §4): 1 comment line(s), 0 directive(s)\n");
process.exit(0);
JS
# shellcheck disable=SC2090
export KYX="$TMP/kyx.js" GOOD_DEED

mkrepo() { # name -> path of a fresh repo with chora + sources
  local r="$TMP/$1"; rm -rf "$r"; mkdir -p "$r/.machine_readable/descriptiles" "$r/contractiles/must"
  git -C "$r" init -q; git -C "$r" config user.email t@t; git -C "$r" config user.name t
  printf '%s\n' "$CHORA" > "$r/demo_chora.deed"
  printf '%s\n' "$SOURCE" > "$r/.machine_readable/descriptiles/ECOSYSTEM.a2ml"
  printf '[state]\nphase = "beta"\n' > "$r/.machine_readable/descriptiles/STATE.a2ml"
  printf '# x\n' > "$r/contractiles/must/Mustfile.a2ml"
  git -C "$r" add -A; git -C "$r" commit -qm init
  printf '%s\n' "$r"
}
run() { # repo -> sets RC, REPORT
  REPORT="$TMP/report-$RANDOM"; mkdir -p "$REPORT"; RC=0
  A2ML_REPORT_DIR="$REPORT" bash "$FIX" "$1" > "$TMP/out" 2> "$TMP/err" || RC=$?
}
staged() { git -C "$1" diff --cached --name-status | tr '\t' ' ' | LC_ALL=C sort | tr '\n' ';'; }

# --- the census on its own ---------------------------------------------------
printf '%s\n' "$GOOD_DEED" > "$TMP/ECOSYSTEM.deed"
bun "$DEED_LINT" --ast-json "$TMP/ECOSYSTEM.deed" > "$TMP/ast.json"
printf '%s\n' "$SOURCE" > "$TMP/src.a2ml"
check "census: every source value is in the good deed" 0 "$(bun "$CENSUS" check "$TMP/src.a2ml" "$TMP/ast.json" >/dev/null; echo $?)"
printf '%s\nextra = "never mapped"\n' "$SOURCE" > "$TMP/src2.a2ml"
check "census: a value the deed lacks is reported" '["never mapped"]' "$(bun "$CENSUS" check "$TMP/src2.a2ml" "$TMP/ast.json" | jq -c .missing || true)"
printf 'a = """\nmulti\nline"""\nb = [ "x", # c\n  "y" ]\nc = { d = 1, e = true }\n@directive(skip)\nf = \x27lit\x27\n' > "$TMP/src3.a2ml"
check "census: multi-line, arrays with comments, inline tables, literals; directives skipped" \
  '["multi\nline","x","y","1","true","lit"]' \
  "$(bun -e 'const {a2mlValues} = await import(process.argv[1]); console.log(JSON.stringify(a2mlValues(require("fs").readFileSync(process.argv[2],"utf8"))))' "$CENSUS" "$TMP/src3.a2ml")"
printf 'oops\n' > "$TMP/bad.a2ml"
check "census: an unreadable source is exit 2, not a pass" 2 "$(bun "$CENSUS" check "$TMP/bad.a2ml" "$TMP/ast.json" >/dev/null 2>&1; echo $?)"

# --- --check (the outcome predicate) -----------------------------------------
r=$(mkrepo check)
check "--check counts tracked .a2ml" 3 "$(bash "$FIX" --check "$r")"
check "the outcome predicate is declared" 1 "$(grep -cx '# outcome-predicate: --check' "$FIX")"

# --- a clean conversion, plus refusals for what kyX cannot do ----------------
r=$(mkrepo ok); STUB_KYX=ok run "$r"
check "partial: ECOSYSTEM converted, STATE and Mustfile refused -> exit 3" 3 "$RC"
check "the deed is staged and the source deletion staged" \
  "A .machine_readable/descriptiles/ECOSYSTEM.deed;D .machine_readable/descriptiles/ECOSYSTEM.a2ml;" "$(staged "$r")"
check "the staged deed is kyX's output, byte for byte" "$(printf '%s\n' "$GOOD_DEED")" "$(cat "$r/.machine_readable/descriptiles/ECOSYSTEM.deed")"
check "nothing is committed" 1 "$(git -C "$r" rev-list --count HEAD)"
check "report: one converted, with provenance" "$KYX_COMMIT" "$(jq -r '.converted[0].kyx_commit' "$REPORT/a2ml-report.json")"
check "report: STATE refused as no front end" STATE "$(jq -r '.refused[] | select(.path|endswith("STATE.a2ml")) | .family' "$REPORT/a2ml-report.json")"
check "report: Mustfile refused as a contractile with no spec" CONTRACTILE "$(jq -r '.refused[] | select(.path|endswith("Mustfile.a2ml")) | .family' "$REPORT/a2ml-report.json")"
check "a PR body is written" 1 "$(grep -c 'kyX conversion' "$REPORT/a2ml-pr.md")"
check "a P1 issue body is written, fingerprinted" 1 "$(grep -c '^<!-- a2ml-kyx:fingerprint=' "$REPORT/a2ml-p1-issue.md")"
check "the issue says never hand-convert" 1 "$(grep -c 'Do not hand-convert them' "$REPORT/a2ml-p1-issue.md")"
fp1=$(head -1 "$REPORT/a2ml-p1-issue.md")
git -C "$r" commit -qm convert
STUB_KYX=ok run "$r"
check "idempotent: a second run converts nothing more and refuses the same -> exit 2" 2 "$RC"
check "idempotent: same fingerprint, so no second issue" "$fp1" "$(head -1 "$REPORT/a2ml-p1-issue.md")"
check "idempotent: no PR body when nothing converted" 0 "$([[ -e "$REPORT/a2ml-pr.md" ]] && echo 1 || echo 0)"

# --- every gate refuses and leaves the tree untouched ------------------------
for mode in refuse drop lossy badgrammar crash; do
  r=$(mkrepo "g-$mode"); STUB_KYX=$mode run "$r"
  check "$mode: nothing converted -> exit 2" 2 "$RC"
  check "$mode: the tree is untouched" "" "$(git -C "$r" status --porcelain)"
  reason=$(jq -r '.refused[] | select(.path|endswith("ECOSYSTEM.a2ml")) | .reasons | join(" | ")' "$REPORT/a2ml-report.json")
  case "$mode" in
    refuse) want='kyX refused: line 3' ;;
    drop) want='not lossless: kyX dropped' ;;
    lossy) want='not lossless: the value census' ;;
    badgrammar) want='does not pass standards deed_lint' ;;
    crash) want='kyX failed (exit 2)' ;;
  esac
  if [[ "$reason" == *"$want"* ]]; then ok "$mode: the reason names the gate"; else no "$mode: reason ${reason@Q} lacks ${want@Q}"; fi
done
r=$(mkrepo drop-ok); STUB_KYX=drop A2ML_ACCEPT_SPEC_DROPS=1 run "$r"
check "A2ML_ACCEPT_SPEC_DROPS=1 accepts a spec §4 drop and lists it in the PR" 1 "$(grep -c 'Dropped under spec §4' "$REPORT/a2ml-pr.md")"

# --- a root 0-AI-MANIFEST.a2ml (the shape hyperpolymath/proven-tests-and-benches
# carries): its mapping (ai-manifest-decision.adoc) is an open owner ruling, so
# it must be refused loudly, naming the spec, never guessed or converted.
r=$(mkrepo manifest); printf '# SPDX-License-Identifier: MPL-2.0\n# AI manifest\n' > "$r/0-AI-MANIFEST.a2ml"; git -C "$r" add -A; git -C "$r" commit -qm x
STUB_KYX=ok run "$r"
check "0-AI-MANIFEST.a2ml: refused as AI-MANIFEST, citing ai-manifest-decision.adoc" \
  "AI-MANIFEST ai-manifest-decision.adoc" \
  "$(jq -r '.refused[] | select(.path == "0-AI-MANIFEST.a2ml") | "\(.family) \(.spec)"' "$REPORT/a2ml-report.json")"
check "0-AI-MANIFEST.a2ml: never handed to kyX, never deleted" "0-AI-MANIFEST.a2ml" "$(git -C "$r" ls-files 0-AI-MANIFEST.a2ml)"
check "0-AI-MANIFEST.a2ml: listed in the P1 issue under its family" 1 "$(grep -c '^### AI-MANIFEST' "$REPORT/a2ml-p1-issue.md")"

# --- preconditions --------------------------------------------------------------
r=$(mkrepo nochora); git -C "$r" rm -q demo_chora.deed; git -C "$r" commit -qm x; STUB_KYX=ok run "$r"
check "no chora deed: refused, never minted" 1 "$(jq '[.refused[] | select(.reasons[0]|test("never mints"))] | length' "$REPORT/a2ml-report.json")"
r=$(mkrepo exists); cp "$TMP/ECOSYSTEM.deed" "$r/.machine_readable/descriptiles/ECOSYSTEM.deed"; git -C "$r" add -A; git -C "$r" commit -qm x
STUB_KYX=ok run "$r"
check "an existing ECOSYSTEM.deed is never overwritten" 1 "$(jq '[.refused[] | select(.reasons[0]|test("never overwritten"))] | length' "$REPORT/a2ml-report.json")"
r=$(mkrepo nested); mkdir -p "$r/templates/skel/.machine_readable/descriptiles"; printf '%s\n' "$SOURCE" > "$r/templates/skel/.machine_readable/descriptiles/ECOSYSTEM.a2ml"; git -C "$r" add -A; git -C "$r" commit -qm x
STUB_KYX=ok run "$r"
check "a non-canonical ECOSYSTEM.a2ml (a template's) is refused, not guessed" 1 "$(jq '[.refused[] | select(.path|startswith("templates/"))] | length' "$REPORT/a2ml-report.json")"
r=$(mkrepo notools); RC=0; KYX='' A2ML_REPORT_DIR="$TMP/nt" bash "$FIX" "$r" >/dev/null 2>&1 || RC=$?
check "kyX missing when a file needs it: exit 69, operator error" 69 "$RC"
check "kyX missing: nothing changed" "" "$(git -C "$r" status --porcelain)"
r=$(mkrepo inside); RC=0; A2ML_REPORT_DIR="$r/reports" STUB_KYX=ok bash "$FIX" "$r" >/dev/null 2>&1 || RC=$?
check "a report directory inside the repository is refused" 64 "$RC"
r="$TMP/none"; mkdir -p "$r"; git -C "$r" init -q
check "no .a2ml: exit 0, nothing to do" 0 "$(bash "$FIX" "$r" >/dev/null 2>&1; echo $?)"

echo "fix-a2ml-via-kyx-test: $pass passed, $fail failed"
[[ "$fail" -eq 0 ]]
