#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# fleet-report-back-test.sh — the report-back driver writes exactly the
# envelope the Hypatia verdict grid reads (standards docs/VERDICT-GRID-SPEC.adoc),
# and never turns a silent, missing or crashed adapter into a clean row.
#
# Adapters are stubbed: each stub prints the real adapter's JSON SHAPE (taken
# from a live run against hyperpolymath/metamanifold-webui on 2026-10-27), so
# the translation is tested without building seven Rust crates. The stubs
# write nothing, so a stub that is invoked with the wrong arguments fails the
# test rather than quietly passing.
#
# Covers:
#   1. enrolment: only `enrolled: true` members run; `false` ones get no file;
#      an unreadable manifest runs nothing; --member honours `enrolled: false`.
#   2. every adapter's output translates to {rule_id, severity, file, line,
#      message} with repository-relative paths (absolute and "./" stripped).
#   3. missing binary, crash without JSON, and timeout are `error` WITH a
#      reason; non-zero exit WITH valid JSON is still `ran`.
#   4. echidnabot and sustainabot are `not_run` with a reason; an unknown
#      member is `error`; the driver never writes `not_applicable`.
#   5. the envelope is key-sorted compact JSON with one final LF, carrying
#      the fleet ref and the manifest's title.
#   6. --plan lists member, crate and binary for the members that would run.
#   7. the summary table has one row per member.
#   8. (when STANDARDS_DIR points at a hyperpolymath/standards checkout) the
#      real renderer turns the rows non-orange from these envelopes alone.
#   9. empty scans, malformed output, normalization boundaries, aliases,
#      failure isolation, invocation context, and an unchanged scanned tree.
#  10. the build-helper unit suite (stubbed Cargo, no Rust builds or network).
#
# Run: bash tests/fleet-report-back-test.sh

# shellcheck disable=SC2016  # stub bodies are single-quoted on purpose: they expand when the stub runs
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DRIVER="$ROOT/scripts/fleet-report-back.sh"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

pass=0; fail=0
check() {
  local label="$1" expected="$2" actual="$3"
  if [[ "$expected" == "$actual" ]]; then
    pass=$((pass + 1)); echo "PASS: $label"
  else
    fail=$((fail + 1)); echo "FAIL: $label (expected '$expected', got '$actual')"
  fi
}
field() { jq -r "$2" "$1"; }

# ── fixture repository and stub adapters ─────────────────────────────────────
REPO="$TMP/repo with spaces"; mkdir -p "$REPO/.gitbot-fleet" "$REPO/src"
echo 'x' > "$REPO/src/a.jl"
cat > "$REPO/.gitbot-fleet/enrolment.json" <<'JSON'
{ "schema": 1,
  "reporters": {
    "rhodibot": { "title": "rhodibot (RSR compliance)", "enrolled": true },
    "panicbot": { "enrolled": true },
    "echidnabot": { "title": "echidnabot (formal verification: proofs/)", "enrolled": true },
    "seambot": { "enrolled": true },
    "finishingbot": { "enrolled": true },
    "accessibilitybot": { "enrolled": true },
    "glambot": { "enrolled": true },
    "cipherbot": { "enrolled": false, "reason": "no cryptography is implemented" },
    "sustainabot": { "enrolled": false, "reason": "reserved slot" } } }
JSON

BIN="$TMP/bin with spaces"; mkdir -p "$BIN"
stub() { # name body
  printf '#!/usr/bin/env bash\nset -u\n%s\n' "$2" > "$BIN/$1"; chmod +x "$BIN/$1"
}
# Each stub asserts the invocation the driver documents, then prints the shape.
stub rhodibot '[ "$1 $2 $4 $5" = "canon --path --format json" ] || { echo "bad args: $*" >&2; exit 9; }
cat <<J
{"subject":"$3","criteria":30,"scored":20,"not_applicable":10,"counts":{"satisfied":18,"relocated":1,"deprecated":0,"missing":1},
 "findings":[
  {"id":"1.1.2","name":"justfile","tier":"T1","severity":"missing","canon_keeps_it_at":"Justfile","expected":["Justfile"],"found":[],"deprecated_copies":[],"details":["absent"]},
  {"id":"2.3","name":"readme","tier":"T1","severity":"satisfied","canon_keeps_it_at":"README.adoc","expected":["README.adoc"],"found":["README.adoc"],"deprecated_copies":[],"details":[]},
  {"id":"4.1","name":"security.txt","tier":"T2","severity":"relocated","canon_keeps_it_at":"www/.well-known/security.txt","expected":["www/.well-known/security.txt"],"found":[".well-known/security.txt"],"deprecated_copies":[],"details":["found elsewhere"]}]}
J'
stub panicbot '[ "$1 $2 $3" = "--format json scan" ] || { echo "bad args: $*" >&2; exit 9; }
command -v panic-attack >/dev/null || { echo "Error: Failed to run panic-attack assail" >&2; exit 1; }
cat <<J
[{"id":"u1","source":"panicbot","rule_id":"PA018","rule_name":"DynamicCodeExecution","severity":"warning","message":"eval in src/a.jl","category":"security","file":"src/a.jl","line":null,"column":null,"fixable":false,"fixed":false}]
J'
stub panic-attack 'echo "stub"'
stub seambot '[ "$1 $3 $4 $5" = "--path --format json check" ] || { echo "bad args: $*" >&2; exit 9; }
cat <<J
{"status":"fail","findings":[{"check":"register-exists","severity":"error","message":"Seam register not found at spec/seams/seam-register.json","location":"spec/seams/seam-register.json","suggestion":"Run seambot init","seam_id":null}],"summary":{"errors":1}}
J'
stub finishing-bot '[ "$1 $3 $4 $5" = "--path --format json audit" ] || { echo "bad args: $*" >&2; exit 9; }
cat <<J
{"license":{"findings":[{"id":"LIC-002","name":"Missing SPDX Header","severity":"warning","message":"Source file missing SPDX license identifier","file":"./config/tools.yml","line":null,"column":null,"suggestion":"s","fixable":true}]},
 "placeholder":{"findings":[]},
 "release":{"findings":[{"id":"REL-001","name":"No release","severity":"error","message":"No release artefacts","file":null,"line":null,"column":null,"suggestion":null,"fixable":false}]}}
J
exit 1'
stub glambot '[ "$1 $3 $4 $5" = "--path --format json audit" ] || { echo "bad args: $*" >&2; exit 9; }
cat <<J
{"visual":{"findings":[{"id":"VIS-005","name":"Line too long","severity":"info","message":"Line 88 characters exceeds max length of 80.","file":"./SECURITY.md","line":7,"column":null,"suggestion":"s","fixable":false}]},"seo":{"findings":[]}}
J'
stub accessibilitybot '[ "$1" = "fleet" ] || { echo "bad args: $*" >&2; exit 9; }
cat <<J
{"findings":[{"id":"u2","source":"accessibilitybot","rule_id":"WCAG-1.3.1-first-heading","rule_name":"Heading Hierarchy","severity":"error","message":"First heading is <h2>","category":"accessibility/wcag-a","file":"$2/src/index.html","line":12,"column":null,"fixable":false,"fixed":false}]}
J
exit 1'
stub cipherbot '[ "$1 $2 $3" = "--format json scan" ] || { echo "bad args: $*" >&2; exit 9; }
echo "[]"'

run() { # out-dir extra-args...
  bash "$DRIVER" --repo "$REPO" --out "$1" --bin-dir "$BIN" --fleet-ref deadbeef "${@:2}" > "$1.log" 2>&1
}

# 1. enrolment ────────────────────────────────────────────────────────────────
repo_state() {
  find "$REPO" -printf '%P %y %l\n' | sort
  find "$REPO" -type f -exec sha256sum {} + | sort
}
repo_before=$(repo_state)
ECHIDNA_URL='' run "$TMP/o1" --summary "$TMP/o1.md"
check "enrolled members each get a file" "accessibilitybot echidnabot finishingbot glambot panicbot rhodibot seambot" \
  "$(find "$TMP/o1" -name '*.json' -printf '%f\n' | sed 's/\.json$//' | sort | tr '\n' ' ' | sed 's/ $//')"
check "a member marked enrolled:false gets no file" no "$([ -e "$TMP/o1/cipherbot.json" ] && echo yes || echo no)"
check "the driver never writes not_applicable" 0 "$(cat "$TMP/o1"/*.json | jq -r .status | grep -c not_applicable || true)"

run "$TMP/o1b" --member cipherbot --member rhodibot
check "--member honours enrolled:false" "rhodibot" "$(find "$TMP/o1b" -name '*.json' -printf '%f\n' | sed 's/\.json$//' | tr '\n' ' ' | sed 's/ $//')"

echo 'nonsense' > "$TMP/bad.json"
run "$TMP/o1c" --enrolment "$TMP/bad.json"
check "an unreadable manifest runs nothing" 0 "$(find "$TMP/o1c" -name '*.json' | wc -l)"
check "...and says so" yes "$(grep -q 'enrolment manifest unreadable' "$TMP/o1c.log" && echo yes || echo no)"

# 2. translation ───────────────────────────────────────────────────────────────
r="$TMP/o1/rhodibot.json"
check "rhodibot: satisfied rows are not findings" 2 "$(field "$r" '.findings | length')"
check "rhodibot: missing is error" error "$(field "$r" '.findings[] | select(.rule_id == "1.1.2") | .severity')"
check "rhodibot: relocated is info" info "$(field "$r" '.findings[] | select(.rule_id == "4.1") | .severity')"
check "rhodibot: a missing file is named where the canon keeps it" Justfile "$(field "$r" '.findings[] | select(.rule_id == "1.1.2") | .file')"
check "rhodibot: a relocated file is named where it was found, so the diff can match it" .well-known/security.txt "$(field "$r" '.findings[] | select(.rule_id == "4.1") | .file')"
check "rhodibot: title comes from the manifest" "rhodibot (RSR compliance)" "$(field "$r" .title)"
check "panicbot: fleet Finding passes through" "PA018|warning|src/a.jl|0" "$(field "$TMP/o1/panicbot.json" '.findings[0] | "\(.rule_id)|\(.severity)|\(.file)|\(.line)"')"
check "seambot: check name becomes the rule id" "SEAM-register-exists|error|spec/seams/seam-register.json" \
  "$(field "$TMP/o1/seambot.json" '.findings[0] | "\(.rule_id)|\(.severity)|\(.file)"')"
check "finishingbot: analyzers are flattened" 2 "$(field "$TMP/o1/finishingbot.json" '.findings | length')"
check "finishingbot: ./ prefix is stripped" config/tools.yml "$(field "$TMP/o1/finishingbot.json" '.findings[] | select(.rule_id == "LIC-002") | .file')"
check "finishingbot: a repository-level finding has an empty path" "" "$(field "$TMP/o1/finishingbot.json" '.findings[] | select(.rule_id == "REL-001") | .file')"
check "glambot: line numbers survive" 7 "$(field "$TMP/o1/glambot.json" '.findings[0].line')"
check "accessibilitybot: absolute paths become repository-relative" src/index.html "$(field "$TMP/o1/accessibilitybot.json" '.findings[0].file')"
check "a default title is used when the manifest has none" "glambot (presentation quality)" "$(field "$TMP/o1/glambot.json" .title)"

# 3. failure modes ─────────────────────────────────────────────────────────────
check "non-zero exit with valid JSON is still a run (finishingbot)" ran "$(field "$TMP/o1/finishingbot.json" .status)"
check "non-zero exit with valid JSON is still a run (accessibilitybot)" ran "$(field "$TMP/o1/accessibilitybot.json" .status)"

rm "$BIN/panic-attack"
run "$TMP/o3" --member panicbot
check "a crash without JSON is error" error "$(field "$TMP/o3/panicbot.json" .status)"
check "...with the adapter's first diagnostic as the reason" yes \
  "$(field "$TMP/o3/panicbot.json" .reason | grep -q 'Failed to run panic-attack' && echo yes || echo no)"
stub panic-attack 'echo stub'

run "$TMP/o4" --member glambot --bin-dir "$TMP/nowhere"
check "a missing binary is error" error "$(field "$TMP/o4/glambot.json" .status)"
check "...naming the binary and the crate" yes \
  "$(field "$TMP/o4/glambot.json" .reason | grep -q "glambot.*bots/glambot" && echo yes || echo no)"

stub seambot 'sleep 5; echo "{}"'
run "$TMP/o5" --member seambot --timeout 1
check "a timeout is error" error "$(field "$TMP/o5/seambot.json" .status)"
check "...and says so" yes "$(field "$TMP/o5/seambot.json" .reason | grep -q timeout && echo yes || echo no)"

# 4. members without a local mode ──────────────────────────────────────────────
check "echidnabot without an engine is not_run" not_run "$(field "$TMP/o1/echidnabot.json" .status)"
check "...naming ECHIDNA_URL" yes "$(field "$TMP/o1/echidnabot.json" .reason | grep -q ECHIDNA_URL && echo yes || echo no)"
run "$TMP/o6" --enrolment "" --member sustainabot --member nosuchbot
check "sustainabot is not_run (reserved slot)" not_run "$(field "$TMP/o6/sustainabot.json" .status)"
check "an unknown member is error" error "$(field "$TMP/o6/nosuchbot.json" .status)"

# 4b. a large findings list must not die on ARG_MAX (seen live: 146 panicbot
#     findings killed jq with "Argument list too long" when passed via argv).
stub cipherbot '[ "$1 $2 $3" = "--format json scan" ] || { echo "bad args: $*" >&2; exit 9; }
jq -n -c '"'"'[range(6000) | {rule_id: "CIPHER-\(.)", severity: "warning", file: "src/f\(.).jl", line: ., message: ("m" * 60)}]'"'"''
run "$TMP/o4b" --enrolment "" --member cipherbot
check "a 6000-finding envelope is written (no ARG_MAX failure)" ran "$(field "$TMP/o4b/cipherbot.json" .status)"
check "...with every finding kept" 6000 "$(field "$TMP/o4b/cipherbot.json" '.findings | length')"
stub cipherbot '[ "$1 $2 $3" = "--format json scan" ] || { echo "bad args: $*" >&2; exit 9; }
echo "[]"'

# 5. envelope shape ────────────────────────────────────────────────────────────
check "envelope keys are exactly the spec's, sorted" "findings reason ref reporter status title" \
  "$(field "$r" 'keys | join(" ")')"
check "the envelope is compact with one final LF" 1 "$(wc -l < "$r")"
check "the fleet ref is recorded" deadbeef "$(field "$r" .ref)"
check "reason is null when the member ran" null "$(field "$r" .reason)"
check "finding keys are exactly the spec's" "file line message rule_id severity" "$(field "$r" '.findings[0] | keys | join(" ")')"

# 6. --plan ────────────────────────────────────────────────────────────────────
plan=$(bash "$DRIVER" --plan --enrolment "$REPO/.gitbot-fleet/enrolment.json")
check "--plan lists only enrolled members" 7 "$(printf '%s\n' "$plan" | wc -l)"
check "--plan names the finishingbot crate and binary" "finishingbot	finishingbot	finishing-bot" "$(printf '%s\n' "$plan" | grep '^finishingbot')"
check "--plan gives echidnabot no crate: nothing is built for a member without a local mode" "echidnabot		" "$(printf '%s\n' "$plan" | grep '^echidnabot')"

# 7. summary ───────────────────────────────────────────────────────────────────
check "summary has one row per member" 7 "$(grep -c '^| [a-z]' "$TMP/o1.md")"
check "summary states the error reason" yes "$(grep -q '| echidnabot | not_run | 0 | no ECHIDNA' "$TMP/o1.md" && echo yes || echo no)"

# 9. boundary and regression cases ────────────────────────────────────────────
# Only boolean true enrols a member; JSON truthiness must not enrol a string.
cat > "$TMP/strict.json" <<'JSON'
{"reporters":{"cipherbot":{"enrolled":true,"title":""},
  "panicbot":{"enrolled":"true"},"glambot":{"enrolled":1},
  "seambot":{"enrolled":null},"rhodibot":{},"sustainabot":{"enrolled":false}}}
JSON
run "$TMP/strict" --enrolment "$TMP/strict.json"
check "only boolean true enrols a member" cipherbot \
  "$(find "$TMP/strict" -name '*.json' -printf '%f\n' | sed 's/\.json$//')"
check "an empty title falls back to the built-in title" 'cipherbot (cryptographic hygiene)' \
  "$(field "$TMP/strict/cipherbot.json" .title)"

for manifest in missing empty wrong-shape nobody; do
  case "$manifest" in
    missing) ;; # Deliberately absent.
    empty) : > "$TMP/$manifest.json" ;;
    wrong-shape) echo '{"reporters":[]}' > "$TMP/$manifest.json" ;;
    nobody) echo '{"reporters":{}}' > "$TMP/$manifest.json" ;;
  esac
  run "$TMP/manifest-$manifest" --enrolment "$TMP/$manifest.json"
  check "$manifest manifest produces no envelopes" 0 \
    "$(find "$TMP/manifest-$manifest" -name '*.json' | wc -l)"
done
run "$TMP/disabled" --member cipherbot --member sustainabot --summary "$TMP/disabled.md"
check "an explicitly selected list of disabled members stays empty" 0 \
  "$(find "$TMP/disabled" -name '*.json' | wc -l)"
check "an empty selection gets an explanatory summary" yes \
  "$(grep -q 'No member is enrolled' "$TMP/disabled.md" && echo yes || echo no)"

# A clean scan has an envelope, for each distinct adapter JSON shape.
for member in rhodibot panicbot seambot finishingbot glambot accessibilitybot cipherbot; do
  case "$member" in
    panicbot|cipherbot) shape='[]' ;;
    finishingbot|glambot) shape='{"first":{"findings":[]},"second":{"findings":[]}}' ;;
    *) shape='{"findings":[]}' ;;
  esac
  binary="$member"; [ "$member" != finishingbot ] || binary=finishing-bot
  stub "$binary" "printf '%s\n' '$shape'"
  run "$TMP/clean-$member" --enrolment "" --member "$member"
  check "$member clean scan is ran with no findings or reason" 'ran|0|null' \
    "$(field "$TMP/clean-$member/$member.json" '"\(.status)|\(.findings | length)|\(.reason)"')"
done

for alias in finishbot finishing-bot; do
  run "$TMP/alias-$alias" --enrolment "" --member "$alias"
  check "$alias uses finishing-bot and retains the requested reporter name" "$alias|ran" \
    "$(field "$TMP/alias-$alias/$alias.json" '"\(.reporter)|\(.status)"')"
done
plan=$(bash "$DRIVER" --plan --enrolment "" --member finishbot --member finishing-bot --member cipherbot)
check "--plan maps aliases and cipherbot to their crates and binaries" \
  $'finishbot\tfinishingbot\tfinishing-bot\nfinishing-bot\tfinishingbot\tfinishing-bot\ncipherbot\tcipherbot\tcipherbot' "$plan"

stub rhodibot 'cat <<JSON
{"findings":[
 {"id":"D","name":"Old canon","severity":"deprecated","found":["./legacy.txt"],"canon_keeps_it_at":"new.txt","details":["replace","retire"]},
 {"id":"M","name":"Missing","severity":"missing","expected":["expected.txt"],"found":["wrong.txt"]},
 {"id":"R","name":"Relocated","severity":"relocated","found":[],"expected":["fallback.txt"]},
 {"id":"S","name":"Satisfied","severity":"satisfied"}]}
JSON'
run "$TMP/canon" --member rhodibot
check "canon deprecated severity, actual path, details and fallback paths survive" \
  '[{"file":"legacy.txt","line":0,"message":"Old canon: deprecated — replace; retire","rule_id":"D","severity":"warning"},{"file":"expected.txt","line":0,"message":"Missing: missing","rule_id":"M","severity":"error"},{"file":"fallback.txt","line":0,"message":"Relocated: relocated","rule_id":"R","severity":"info"}]' \
  "$(jq -S -c .findings "$TMP/canon/rhodibot.json")"

stub cipherbot 'cat <<JSON
[{"id":"fallback","severity":null,"file":42,"line":"12","message":17},
 {},
 {"rule_id":"primary","id":"ignored","severity":"high","file":"$4/src/a.jl","line":12,"message":"message"},
 {"rule_id":7,"severity":3,"location":"./src/b.jl","line":null},
 {"rule_id":"sibling","file":"$4-other/src/a.jl","line":0}]
JSON'
run "$TMP/normalize" --enrolment "" --member cipherbot
check "fleet fields have stable defaults, types and precedence" \
  '[{"file":"","line":0,"message":"17","rule_id":"fallback","severity":"unknown"},{"file":"","line":0,"message":"","rule_id":"?","severity":"unknown"},{"file":"src/a.jl","line":12,"message":"message","rule_id":"primary","severity":"high"},{"file":"src/b.jl","line":0,"message":"","rule_id":"7","severity":"3"}]' \
  "$(jq -S -c '.findings[:4]' "$TMP/normalize/cipherbot.json")"
check "relativization does not strip a sibling directory with the same prefix" "$REPO-other/src/a.jl" \
  "$(field "$TMP/normalize/cipherbot.json" '.findings[4].file')"

stub cipherbot 'jq -n '\''["error","warning","info","suggestion","critical","high","medium","low"] | map({severity: .})'\'''
run "$TMP/severities" --enrolment "" --member cipherbot
check "fleet and scanner severities reach the renderer unchanged" \
  'error warning info suggestion critical high medium low' \
  "$(field "$TMP/severities/cipherbot.json" '[.findings[].severity] | join(" ")')"

# Success exit codes alone do not prove the adapter produced usable findings.
for output in empty malformed wrong-shape; do
  case "$output" in
    empty) stub cipherbot 'exit 0' ;;
    malformed) stub cipherbot 'echo "not JSON"' ;;
    wrong-shape) stub cipherbot 'echo 42' ;;
  esac
  run "$TMP/output-$output" --enrolment "" --member cipherbot --member glambot
  check "$output adapter output is an error with no findings" 'error|0' \
    "$(field "$TMP/output-$output/cipherbot.json" '"\(.status)|\(.findings | length)"')"
  case "$output" in wrong-shape) phrase="unexpected output shape" ;; *) phrase="no readable JSON" ;; esac
  check "$output adapter output explains the error" yes \
    "$(field "$TMP/output-$output/cipherbot.json" .reason | grep -q "$phrase" && echo yes || echo no)"
  check "$output adapter output does not prevent a later member running" ran \
    "$(field "$TMP/output-$output/glambot.json" .status)"
done

stub cipherbot 'printf "\nStack backtrace:\nfirst diagnostic\nsecond diagnostic\n" >&2; exit 17'
run "$TMP/diagnostic" --enrolment "" --member cipherbot
check "errors use the first nonblank diagnostic, skipping backtrace headers" \
  'adapter produced no readable JSON: first diagnostic' "$(field "$TMP/diagnostic/cipherbot.json" .reason)"
stub cipherbot 'exit 17'
run "$TMP/no-diagnostic" --enrolment "" --member cipherbot
check "a silent crash records its exit status" \
  'adapter produced no readable JSON: exit status 17 with no diagnostic' "$(field "$TMP/no-diagnostic/cipherbot.json" .reason)"
stub cipherbot 'printf "%0300d\n" 0 >&2; exit 1'
run "$TMP/long-diagnostic" --enrolment "" --member cipherbot
check "diagnostics are bounded to 240 characters" 240 \
  "$(field "$TMP/long-diagnostic/cipherbot.json" '.reason | ltrimstr("adapter produced no readable JSON: ") | length')"
stub cipherbot 'echo "[]"; exit 124'
run "$TMP/timeout-json" --enrolment "" --member cipherbot
check "a timeout cannot become a clean scan even with valid JSON" \
  'error|adapter exceeded the 300s timeout|0' \
  "$(field "$TMP/timeout-json/cipherbot.json" '"\(.status)|\(.reason)|\(.findings | length)"')"
stub cipherbot 'echo "[]"; exit 2'
run "$TMP/nonzero-json" --enrolment "" --member cipherbot
check "valid empty JSON with a nonzero exit is still a completed scan" 'ran|0|null' \
  "$(field "$TMP/nonzero-json/cipherbot.json" '"\(.status)|\(.findings | length)|\(.reason)"')"

ECHIDNA_URL=https://engine.invalid run "$TMP/engine" --enrolment "" --member echidnabot
check "a configured engine still reports the unsupported dispatch honestly" 'not_run|0' \
  "$(field "$TMP/engine/echidnabot.json" '"\(.status)|\(.findings | length)"')"
check "a configured engine reason names the missing dispatch implementation" yes \
  "$(field "$TMP/engine/echidnabot.json" .reason | grep -q 'does not yet dispatch' && echo yes || echo no)"

# Verify process context and argument boundaries using paths containing spaces.
stub cipherbot '[ "$#" -eq 4 ] && [ "$1 $2 $3" = "--format json scan" ] && [ "$PWD" = "$4" ] && [ "$RUST_LOG" = off ] || exit 9
jq -n --arg path "$(command -v cipherbot)" '\''[{rule_id: "CONTEXT", severity: "info", message: $path}]'\'''
run "$TMP/context with spaces" --enrolment "" --member cipherbot
check "adapters run in the scanned tree with RUST_LOG off and bin-dir on PATH" "$BIN/cipherbot" \
  "$(field "$TMP/context with spaces/cipherbot.json" '.findings[0].message')"
PATH="$BIN:$PATH" run "$TMP/path-fallback" --enrolment "" --member cipherbot --bin-dir "$TMP/no-bin"
check "an absent bin-dir falls back to PATH" ran "$(field "$TMP/path-fallback/cipherbot.json" .status)"

printf 'Existing summary\n' > "$TMP/append.md"
run "$TMP/append" --enrolment "" --member cipherbot --summary "$TMP/append.md"
check "--summary appends and preserves existing content" 'Existing summary' "$(head -n1 "$TMP/append.md")"
check "the default ref is the fleet checkout revision" "$(git -C "$ROOT" rev-parse HEAD)" \
  "$(bash "$DRIVER" --repo "$REPO" --out "$TMP/default-ref" --enrolment "" --member sustainabot >/dev/null; field "$TMP/default-ref/sustainabot.json" .ref)"

expect_usage() {
  local label="$1" rc=0; shift
  bash "$DRIVER" "$@" > "$TMP/usage.log" 2>&1 || rc=$?
  check "$label exits with usage status" 2 "$rc"
}
expect_usage "missing required paths"
expect_usage "nonexistent repository" --repo "$TMP/absent" --out "$TMP/unused"
expect_usage "unknown option" --unrecognized
for seconds in 0 -1 1.5 invalid; do
  expect_usage "invalid timeout $seconds" --repo "$REPO" --out "$TMP/unused" --timeout "$seconds"
done
# Valid JSON of the wrong shape is a shape error, never a clean scan.
stub cipherbot 'echo "{}"'
run "$TMP/shape-obj" --enrolment "" --member cipherbot
check "an array adapter printing {} is an error" "error" "$(field "$TMP/shape-obj/cipherbot.json" .status)"
check "...naming the expected shape" yes "$(field "$TMP/shape-obj/cipherbot.json" .reason | grep -q "unexpected output shape: expected an array" && echo yes || echo no)"
stub seambot 'echo "[]"'
run "$TMP/shape-arr" --enrolment "" --member seambot
check "an object adapter printing [] is an error" "error" "$(field "$TMP/shape-arr/seambot.json" .status)"
stub accessibilitybot 'echo "{\"findings\": null}"'
run "$TMP/shape-null" --enrolment "" --member accessibilitybot
check "a null findings array is an error, not a clean scan" "error" "$(field "$TMP/shape-null/accessibilitybot.json" .status)"
# A manifest title is PR-controlled and rides into jq as an argument: bounded.
stub cipherbot 'echo "[]"'
jq -n --arg t "$(printf '%0500d' 0)" '{schema: 1, reporters: {cipherbot: {enrolled: true, title: $t}}}' > "$TMP/longtitle.json"
run "$TMP/longtitle" --enrolment "$TMP/longtitle.json" --member cipherbot
check "a 500-character manifest title is cut to 120" 120 "$(field "$TMP/longtitle/cipherbot.json" '.title | length')"

# A member name from the manifest is untrusted: a tab or a path in a key must
# never become a plan field, an envelope file name or an adapter argument.
printf '{"reporters":{"rhodibot":{"enrolled":true},"x\\t../../../evil\\tz":{"enrolled":true},"Bad Name":{"enrolled":true}},"schema":1}\n' > "$TMP/hostile.json"
plan=$(bash "$DRIVER" --plan --enrolment "$TMP/hostile.json" 2> "$TMP/hostile.err")
check "a member key with a tab or a path never reaches the plan" "rhodibot	rhodibot	rhodibot" "$plan"
check "...and each dropped name is warned about" 2 "$(grep -c "warning: ignoring member" "$TMP/hostile.err")"
mkdir -p "$TMP/hostile-out"
bash "$DRIVER" --repo "$REPO" --out "$TMP/hostile-out" --bin-dir "$BIN" --enrolment "" --member "../escape" > "$TMP/hostile-run.log" 2>&1 || true
check "an explicit --member that is not a slug writes nothing" "" "$(find "$TMP/hostile-out" -type f | sort | tr "\n" " ")"
check "...and leaves no file beside the output directory" no "$([ -e "$TMP/escape.json" ] && echo yes || echo no)"
check "usage errors create no output directory" no "$([ -e "$TMP/unused" ] && echo yes || echo no)"
check "reporting never changes the scanned tree's files or directories" "$repo_before" "$(repo_state)"

# 8. the real renderer ─────────────────────────────────────────────────────────
GRID="${STANDARDS_DIR:-}/scripts/verdict-grid.sh"
if [ -n "${STANDARDS_DIR:-}" ] && [ -f "$GRID" ]; then
  ( cd "$REPO" && bash "$GRID" render --out "$TMP/grid" --hypatia /dev/null --panic /dev/null --pons /dev/null \
      --report-backs "$TMP/o1" --enrolment .gitbot-fleet/enrolment.json --sha 0000000 >/dev/null )
  rowstate() { jq -r --arg id "$1" '.reporters[] | select(.id == $id) | .state' "$TMP/grid/report.json"; }
  check "renderer: rhodibot's error finding is a red row" red "$(rowstate rhodibot)"
  check "renderer: glambot's info finding is a blue row" blue "$(rowstate glambot)"
  check "renderer: panicbot's warning is a yellow row" yellow "$(rowstate panicbot)"
  check "renderer: echidnabot not_run is orange" orange "$(rowstate echidnabot)"
  check "renderer: cipherbot not enrolled is grey" grey "$(rowstate cipherbot)"
  check "renderer: no fleet row is left as 'enrolled, but no report-back'" 0 \
    "$(jq '[.reporters[] | select(.reason == "enrolled, but no report-back was received")] | length' "$TMP/grid/report.json")"
  check "renderer: fleet rows are advisory, so the lane stays orange on missing Hypatia evidence" orange "$(jq -r .state "$TMP/grid/report.json")"
else
  echo "SKIP: STANDARDS_DIR not set or has no scripts/verdict-grid.sh; renderer round-trip not run"
fi

echo
echo "fleet-report-back-test: $pass passed, $fail failed"
[ "$fail" -eq 0 ]

# Keep the build helper covered by the same entry point used by e2e.yml.
bash "$ROOT/tests/fleet-report-back-build-test.sh"
