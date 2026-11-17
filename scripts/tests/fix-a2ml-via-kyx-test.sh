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
printf 'a = """\nA\\u00e9\\U0001F600"""\nb = "\\u00e9"\n' > "$TMP/src4.a2ml"
check "census: \\u and \\U escapes decode in multi-line strings as in single-line ones" \
  '["Aé😀","é"]' \
  "$(bun -e 'const {a2mlValues} = await import(process.argv[1]); console.log(JSON.stringify(a2mlValues(require("fs").readFileSync(process.argv[2],"utf8"))))' "$CENSUS" "$TMP/src4.a2ml")"
printf 'a = """\none \\  \t\n    two \\\n  three"""\n' > "$TMP/pad.a2ml"
check "census: a line-ending backslash with spaces or tabs after it is still a continuation" \
  '["one two three"]' \
  "$(bun -e 'const {a2mlValues} = await import(process.argv[1]); console.log(JSON.stringify(a2mlValues(require("fs").readFileSync(process.argv[2],"utf8"))))' "$CENSUS" "$TMP/pad.a2ml")"
printf 'a = """\none \\  two"""\n' > "$TMP/padbad.a2ml"
check "census: a backslash followed by spaces and more text on the line is still a bad escape (exit 2)" 2 \
  "$(bun "$CENSUS" check "$TMP/padbad.a2ml" "$TMP/ast.json" >/dev/null 2>&1; echo $?)"
printf 'a = "same"\nb = "same"\nc = "once"\n' > "$TMP/dup.a2ml"
printf '{"items":[["field","a",["string","same"]],["field","c",["string","once"]]]}\n' > "$TMP/dup-ast.json"
check "census: a value in two source fields but one deed field is reported (counted, not just present)" \
  '["same"] 2 1' \
  "$(bun "$CENSUS" check "$TMP/dup.a2ml" "$TMP/dup-ast.json" | jq -r '"\(.missing | tojson) \(.short[0].source) \(.short[0].deed)"' || true)"
printf '{"items":[["field","a",["string","same"]],["field","b",["string","same"]],["field","c",["string","once"]]]}\n' > "$TMP/dup-ast2.json"
check "census: the same value twice in the deed passes" 0 "$(bun "$CENSUS" check "$TMP/dup.a2ml" "$TMP/dup-ast2.json" >/dev/null; echo $?)"
printf 'oops\n' > "$TMP/bad.a2ml"
check "census: an unreadable source is exit 2, not a pass" 2 "$(bun "$CENSUS" check "$TMP/bad.a2ml" "$TMP/ast.json" >/dev/null 2>&1; echo $?)"

# Empty strings may be dropped, but whitespace is a scalar value.
printf 'a = ""\nb = " "\nc = "\\t"\n' > "$TMP/space.a2ml"
printf '{"items":[]}\n' > "$TMP/space-ast.json"
check "census: missing whitespace values are deficits, empty strings excluded" \
  '2 [" ","\t"]' \
  "$(bun "$CENSUS" check "$TMP/space.a2ml" "$TMP/space-ast.json" | jq -r '"\(.source_values) \(.missing|tojson)"' || true)"
printf '{"items":[["field","b",["string"," "]],["field","c",["string","\\t"]]]}\n' > "$TMP/space-ast.json"
check "census: matching whitespace values pass" 0 "$(bun "$CENSUS" check "$TMP/space.a2ml" "$TMP/space-ast.json" >/dev/null; echo $?)"

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
check "report: a contractile has no spec (null), not a shifted field" null "$(jq -c '.refused[] | select(.path|endswith("Mustfile.a2ml")) | .spec' "$REPORT/a2ml-report.json")"
check "the issue says a contractile has no mapping spec yet, and links no spec" "1 0" \
  "$(grep -c '^### CONTRACTILE — no mapping spec yet' "$REPORT/a2ml-p1-issue.md") $(grep -c 'mappings/0' "$REPORT/a2ml-p1-issue.md" || true)"
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
r=$(mkrepo g-fp); STUB_KYX=badgrammar run "$r"; fpa=$(head -1 "$REPORT/a2ml-p1-issue.md")
STUB_KYX=badgrammar run "$r"
check "badgrammar: the same refusal keeps its fingerprint across runs (no work path in it)" "$fpa" "$(head -1 "$REPORT/a2ml-p1-issue.md")"
check "badgrammar: no reason names the work directory" 0 "$(grep -c 'a2ml-kyx-work' "$REPORT/a2ml-report.json" || true)"

# A reason that starts with a dash is data, not a jq option.
cat > "$TMP/dashlint.js" <<'JS'
process.stdout.write("-x: not a deed\n"); process.exit(1);
JS
r=$(mkrepo dash); RC=0; REPORT="$TMP/report-dash"; mkdir -p "$REPORT"
DEED_LINT="$TMP/dashlint.js" STUB_KYX=ok A2ML_REPORT_DIR="$REPORT" bash "$FIX" "$r" >/dev/null 2>&1 || RC=$?
check "a lint line starting with '-' is recorded as a reason -> exit 2" 2 "$RC"
check "the dash line is in the reasons" 1 "$(jq '[.refused[].reasons[] | select(. == "-x: not a deed")] | length' "$REPORT/a2ml-report.json")"

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

# --- the work tree must be in a state the PR can carry ------------------------
r=$(mkrepo dirtysrc); printf 'extra = "local edit"\n' >> "$r/.machine_readable/descriptiles/ECOSYSTEM.a2ml"
STUB_KYX=ok run "$r"
check "a source with uncommitted changes is refused before kyX runs" 1 "$(jq '[.refused[] | select(.reasons[0]|test("source has uncommitted"))] | length' "$REPORT/a2ml-report.json")"
check "dirty source: only the local edit is in the tree; nothing staged" " M .machine_readable/descriptiles/ECOSYSTEM.a2ml" "$(git -C "$r" status --porcelain)"
r=$(mkrepo dirtychora); printf ';; local edit\n' >> "$r/demo_chora.deed"; STUB_KYX=ok run "$r"
check "a chora deed with uncommitted changes is refused" 1 "$(jq '[.refused[] | select(.reasons[0]|test("demo_chora.deed has uncommitted"))] | length' "$REPORT/a2ml-report.json")"
r=$(mkrepo untrackedchora); git -C "$r" rm -q --cached demo_chora.deed; git -C "$r" commit -qm x; STUB_KYX=ok run "$r"
check "an untracked chora deed does not count; refused, found 0" 1 "$(jq '[.refused[] | select(.reasons[0]|test("found 0"))] | length' "$REPORT/a2ml-report.json")"
r=$(mkrepo ignored); printf '.machine_readable/descriptiles/*.deed\n' > "$r/.gitignore"; git -C "$r" add .gitignore; git -C "$r" commit -qm x
STUB_KYX=ok run "$r"
check "a gitignored target is refused before kyX runs" 1 "$(jq '[.refused[] | select(.reasons[0]|test("gitignored"))] | length' "$REPORT/a2ml-report.json")"
check "gitignored target: the tree is untouched" "" "$(git -C "$r" status --porcelain)"
r=$(mkrepo symdir); mkdir -p "$TMP/outside-desc"
git -C "$r" mv .machine_readable/descriptiles/ECOSYSTEM.a2ml .machine_readable/ECOSYSTEM.a2ml
git -C "$r" mv .machine_readable/descriptiles/STATE.a2ml .machine_readable/STATE.a2ml
rmdir "$r/.machine_readable/descriptiles"; ln -s "$TMP/outside-desc" "$r/.machine_readable/descriptiles"; git -C "$r" commit -qm x
STUB_KYX=ok run "$r"
check "a symlinked descriptiles directory is refused" 1 "$(jq '[.refused[] | select(.reasons[0]|test("symlink"))] | length' "$REPORT/a2ml-report.json")"
check "symlinked descriptiles: nothing written outside the work tree" 0 "$(find "$TMP/outside-desc" -type f | wc -l | tr -d ' ')"
r=$(mkrepo stray); mkdir -p "$r/docs"; printf 'x = "y"\n' > "$r/docs/profile.a2ml"; git -C "$r" add -A; git -C "$r" commit -qm x
STUB_KYX=ok run "$r"
check "a *file.a2ml outside contractiles/ is UNMAPPED, not a contractile" UNMAPPED "$(jq -r '.refused[] | select(.path=="docs/profile.a2ml") | .family' "$REPORT/a2ml-report.json")"

# --- names and report paths -------------------------------------------------------
r=$(mkrepo nl); : > "$r/odd"$'\n'"name.a2ml"; git -C "$r" add -A; git -C "$r" commit -qm x
check "--check counts a name with a newline once" 4 "$(bash "$FIX" --check "$r")"
RC=0; A2ML_REPORT_DIR="$TMP/nl-rep" bash "$FIX" "$r" >/dev/null 2> "$TMP/err" || RC=$?
check "a tracked name with a newline is refused whole, never split" "2 1" "$RC $(grep -c "odd\\\\nname.a2ml" "$TMP/err")"
r=$(mkrepo inside-link); mkdir -p "$r/reports"; ln -s "$r/reports" "$TMP/looks-outside"
RC=0; A2ML_REPORT_DIR="$TMP/looks-outside" STUB_KYX=ok bash "$FIX" "$r" >/dev/null 2>&1 || RC=$?
check "a report directory that symlinks into the repository is refused" 64 "$RC"
r=$(mkrepo replink); mkdir -p "$TMP/rl"; printf 'precious\n' > "$r/precious.txt"; ln -s "$r/precious.txt" "$TMP/rl/a2ml-report.json"
RC=0; A2ML_REPORT_DIR="$TMP/rl" STUB_KYX=ok bash "$FIX" "$r" >/dev/null 2>&1 || RC=$?
check "a report file that is a symlink is refused, and its target untouched" "64 precious" "$RC $(cat "$r/precious.txt")"
r=$(mkrepo hardlink); mkdir -p "$TMP/hl"; printf 'precious\n' > "$TMP/hl-target"; ln "$TMP/hl-target" "$TMP/hl/a2ml-report.json"
STUB_KYX=ok A2ML_REPORT_DIR="$TMP/hl" bash "$FIX" "$r" >/dev/null 2>&1 || true
check "a hard-linked report file is replaced, not written through" "precious" "$(cat "$TMP/hl-target")"
mkdir -p "$TMP/nojq-bin"; for t in git dirname; do ln -sf "$(command -v "$t")" "$TMP/nojq-bin/$t"; done
r=$(mkrepo nojq); RC=0; PATH="$TMP/nojq-bin" A2ML_REPORT_DIR="$TMP/nojq-rep" "$(command -v bash)" "$FIX" "$r" >/dev/null 2>&1 || RC=$?
check "jq missing: exit 69 before anything is classified" 69 "$RC"


# A committed source symlink is clean in Git but its referent is untracked.
r=$(mkrepo source-link)
cp "$r/.machine_readable/descriptiles/ECOSYSTEM.a2ml" "$TMP/link-source"
rm "$r/.machine_readable/descriptiles/ECOSYSTEM.a2ml"
ln -s "$TMP/link-source" "$r/.machine_readable/descriptiles/ECOSYSTEM.a2ml"
git -C "$r" add -A; git -C "$r" commit -qm symlink
STUB_KYX=ok run "$r"
check "a tracked source symlink is refused" 1 "$(jq '[.refused[] | select(.reasons[0]|test("source is a symlink"))] | length' "$REPORT/a2ml-report.json")"
check "source symlink: tree unchanged" "" "$(git -C "$r" status --porcelain)"

for deletion in staged unstaged; do
  r=$(mkrepo "target-$deletion")
  cp "$TMP/ECOSYSTEM.deed" "$r/.machine_readable/descriptiles/ECOSYSTEM.deed"
  git -C "$r" add -A; git -C "$r" commit -qm target
  if [[ "$deletion" == staged ]]; then
    git -C "$r" rm -q .machine_readable/descriptiles/ECOSYSTEM.deed
  else
    rm "$r/.machine_readable/descriptiles/ECOSYSTEM.deed"
  fi
  before="$(git -C "$r" status --porcelain)"
  STUB_KYX=ok run "$r"
  check "$deletion target deletion: refused" 1 "$(jq '[.refused[] | select(.reasons[0]|test("has uncommitted changes"))] | length' "$REPORT/a2ml-report.json")"
  check "$deletion target deletion: preserved" "$before" "$(git -C "$r" status --porcelain)"
done

# --- failures after the gates: staging, rollback and reporting ----------------
# Wrappers first on PATH fail one command at a chosen point and pass everything
# else to the real tool, so the script needs no test hooks.
WRAP="$TMP/wrap"; mkdir -p "$WRAP"
REAL_GIT="$(command -v git)" REAL_MV="$(command -v mv)" REAL_MKTEMP="$(command -v mktemp)"
cat > "$WRAP/git" <<SH
#!/usr/bin/env bash
for a in "\$@"; do
  case "\$a" in
    rm)
      if [[ -n "\${EDIT_SOURCE_ON_RM:-}" ]]; then
        printf '# owner edit\\n' >> "\$2/.machine_readable/descriptiles/ECOSYSTEM.a2ml"
        exit 128
      fi
      [[ -n "\${FAIL_GIT_RM:-}" ]] && { : > "$WRAP/rm-failed"; echo "fatal: injected rm failure" >&2; exit 128; } ;;
    ls-files) [[ -n "\${FAIL_GIT_LS_FILES:-}" ]] && { printf 'partial.a2ml\\0'; exit 128; } ;;
    status)
      [[ -n "\${FAIL_GIT_STATUS:-}" ]] && { echo "fatal: injected status failure" >&2; exit 128; }
      [[ -n "\${FAIL_GIT_STATUS_AFTER_RM:-}" && -e "$WRAP/rm-failed" ]] && { echo "fatal: injected status failure" >&2; exit 128; } ;;
    reset) [[ -n "\${FAIL_GIT_RESET:-}" ]] && { echo "fatal: injected reset failure" >&2; exit 128; } ;;
  esac
done
exec "$REAL_GIT" "\$@"
SH
cat > "$WRAP/mv" <<SH
#!/usr/bin/env bash
[[ -n "\${FAIL_MV:-}" ]] && { echo "mv: injected failure" >&2; exit 1; }
exec "$REAL_MV" "\$@"
SH
cat > "$WRAP/mktemp" <<SH
#!/usr/bin/env bash
[[ -n "\${FAIL_PROBE:-}" && "\$*" == *a2ml-probe* ]] && { echo "mktemp: injected failure" >&2; exit 1; }
exec "$REAL_MKTEMP" "\$@"
SH
chmod +x "$WRAP"/*
wrun() { # repo -> RC; the wrapped PATH, plus whatever FAIL_* the caller set
  rm -f "$WRAP/rm-failed"
  REPORT="$TMP/report-$RANDOM"; mkdir -p "$REPORT"; RC=0
  PATH="$WRAP:$PATH" A2ML_REPORT_DIR="$REPORT" STUB_KYX=ok bash "$FIX" "$1" > "$TMP/out" 2> "$TMP/err" || RC=$?
}
r=$(mkrepo discovery-fails)
FAIL_GIT_LS_FILES=1 wrun "$r"
check "normal discovery failure propagates past mapfile" 128 "$RC"
RC=0
FAIL_GIT_LS_FILES=1 PATH="$WRAP:$PATH" bash "$FIX" --check "$r" > "$TMP/out" 2> "$TMP/err" || RC=$?
check "--check discovery failure propagates" 128 "$RC"
check "failed discovery prints no count" "" "$(cat "$TMP/out")"
r=$(mkrepo owner-edit); EDIT_SOURCE_ON_RM=1 wrun "$r"
check "rollback with source edits exits INCONSISTENT" 70 "$RC"
check "rollback preserves source edits" '# owner edit' "$(tail -1 "$r/.machine_readable/descriptiles/ECOSYSTEM.a2ml")"
r=$(mkrepo rm-fails); FAIL_GIT_RM=1 wrun "$r"
check "git rm fails mid-staging: refused, not converted -> exit 2" 2 "$RC"
check "git rm fails: the rollback leaves the tree exactly as it was" "" "$(git -C "$r" status --porcelain)"
check "git rm fails: the reason says it was rolled back" 1 "$(jq '[.refused[] | select(.reasons[0]|test("rolled back"))] | length' "$REPORT/a2ml-report.json")"
r=$(mkrepo undo-fails); FAIL_GIT_RM=1 FAIL_GIT_RESET=1 wrun "$r"
check "git rm AND the rollback fail: exit 70, never a clean refusal" 70 "$RC"
check "a failed rollback says INCONSISTENT and needs a human" 1 "$(grep -c 'INCONSISTENT: staging the conversion' "$TMP/err")"
r=$(mkrepo verify-fails); FAIL_GIT_RM=1 FAIL_GIT_STATUS_AFTER_RM=1 wrun "$r"
check "the rollback runs but git status cannot verify it: exit 70, not \"rolled back\"" 70 "$RC"
r=$(mkrepo status-fails); FAIL_GIT_STATUS=1 wrun "$r"
check "git status fails in the preflight: refused as unknown, not read as clean" 1 "$(jq '[.refused[] | select(.reasons[0]|test("state is unknown"))] | length' "$REPORT/a2ml-report.json")"
check "git status fails in the preflight: nothing staged" "" "$(git -C "$r" diff --cached --name-only)"
r=$(mkrepo publish-fails); FAIL_MV=1 wrun "$r"
check "a report that cannot be published after staging: non-zero exit" 1 "$([[ "$RC" -ne 0 ]] && echo 1 || echo 0)"
check "publish fails: the staged conversion is rolled back, tree as it was" "" "$(git -C "$r" status --porcelain)"
check "publish fails: it says so" 1 "$(grep -c 'staged conversion of .* was rolled back' "$TMP/err")"
r=$(mkrepo probe-fails); FAIL_PROBE=1 wrun "$r"
check "an unwritable report directory: exit 73 before anything changes" "73 " "$RC $(git -C "$r" status --porcelain)"
r=$(mkrepo dangling); ln -s "$TMP/no-such-deed" "$r/.machine_readable/descriptiles/ECOSYSTEM.deed"; STUB_KYX=ok run "$r"
check "a dangling symlink at the target is refused as existing" 1 "$(jq '[.refused[] | select(.reasons[0]|test("never overwritten"))] | length' "$REPORT/a2ml-report.json")"
check "dangling symlink: left in place, nothing written through it" "1 0" \
  "$([[ -L "$r/.machine_readable/descriptiles/ECOSYSTEM.deed" ]] && echo 1 || echo 0) $([[ -e "$TMP/no-such-deed" ]] && echo 1 || echo 0)"

r=$(mkrepo staged-del); cp "$TMP/ECOSYSTEM.deed" "$r/.machine_readable/descriptiles/ECOSYSTEM.deed"
git -C "$r" add -A; git -C "$r" commit -qm deed; git -C "$r" rm -q .machine_readable/descriptiles/ECOSYSTEM.deed
STUB_KYX=ok run "$r"
check "a staged deletion of the target is refused, never overwritten by git add" 1 \
  "$(jq '[.refused[] | select(.reasons[0]|test("ECOSYSTEM.deed has uncommitted changes"))] | length' "$REPORT/a2ml-report.json")"
check "staged deletion of the target: kept as the owner staged it" "D  .machine_readable/descriptiles/ECOSYSTEM.deed" \
  "$(git -C "$r" status --porcelain -- .machine_readable/descriptiles/ECOSYSTEM.deed)"

r="$TMP/none"; mkdir -p "$r"; git -C "$r" init -q
check "no .a2ml: exit 0, nothing to do" 0 "$(bash "$FIX" "$r" >/dev/null 2>&1; echo $?)"

# --- ambiguity and canonical-location boundaries ----------------------------
for location in .machine_readable/ECOSYSTEM.a2ml .machine_readable/6a2/ECOSYSTEM.a2ml; do
  r=$(mkrepo alternate)
  mkdir -p "$r/$(dirname "$location")"
  git -C "$r" mv .machine_readable/descriptiles/ECOSYSTEM.a2ml "$location"
  git -C "$r" rm -q .machine_readable/descriptiles/STATE.a2ml contractiles/must/Mustfile.a2ml
  git -C "$r" commit -qm alternate
  STUB_KYX=ok run "$r"
  check "$location: conversion-only success exits 0" 0 "$RC"
  check "$location: report identifies the original source and canonical target" \
    "$location .machine_readable/descriptiles/ECOSYSTEM.deed" \
    "$(jq -r '.converted[0] | "\(.source) \(.target)"' "$REPORT/a2ml-report.json")"
  check "$location: no refusal issue" 0 "$([[ -e "$REPORT/a2ml-p1-issue.md" ]] && echo 1 || echo 0)"
  check "$location: outcome predicate reaches zero" 0 "$(bash "$FIX" --check "$r")"
done
r=$(mkrepo ambiguous)
cp "$r/.machine_readable/descriptiles/ECOSYSTEM.a2ml" "$r/.machine_readable/ECOSYSTEM.a2ml"
git -C "$r" add -A; git -C "$r" commit -qm duplicate
KYX='' run "$r"
check "two canonical sources: both refused before requiring kyX" "2 2" \
  "$RC $(jq '[.refused[] | select(.reasons[0]|contains("more than one ECOSYSTEM"))] | length' "$REPORT/a2ml-report.json")"
check "two canonical sources: tree untouched" "" "$(git -C "$r" status --porcelain)"
r=$(mkrepo two-choras)
cp "$r/demo_chora.deed" "$r/other_chora.deed"; git -C "$r" add -A; git -C "$r" commit -qm duplicate
KYX='' run "$r"
check "two chora deeds: refuse ambiguous identity before requiring kyX" "2 1" \
  "$RC $(jq '[.refused[] | select(.reasons[0]|contains("found 2"))] | length' "$REPORT/a2ml-report.json")"
check "two chora deeds: tree untouched" "" "$(git -C "$r" status --porcelain)"

echo "fix-a2ml-via-kyx-test: $pass passed, $fail failed"
[[ "$fail" -eq 0 ]]
