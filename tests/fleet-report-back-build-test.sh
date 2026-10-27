#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Unit tests for fleet-report-back-build.sh. Cargo and timeout are stubbed:
# exercise planning, copying and failure isolation without a toolchain,
# downloads, or touching the user's panic-attack cache.
# Run: bash tests/fleet-report-back-build-test.sh
# Also run by tests/fleet-report-back-test.sh, the existing CI entry point.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BUILDER="$ROOT/scripts/fleet-report-back-build.sh"
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

FLEET="$TMP/fleet with spaces"; TOOLS="$TMP/tools"
mkdir -p "$FLEET/bots" "$TOOLS"
for crate in rhodibot panicbot finishingbot glambot broken slow; do
  mkdir -p "$FLEET/bots/$crate"
  printf '[package]\nname = "%s"\nversion = "0.0.0"\n' "$crate" > "$FLEET/bots/$crate/Cargo.toml"
done
export FLEET_BUILD_CALLS="$TMP/cargo-calls" FLEET_BUILD_TIMEOUTS="$TMP/timeout-calls"

cat > "$TOOLS/cargo" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
crate="${PWD##*/}"
printf '%s\t%s\n' "$crate" "$*" >> "$FLEET_BUILD_CALLS"
# Refuse installs and any flags other than the locked release build recipe.
# An accidental real dependency install must never escape the fixture.
[[ "$*" == 'build --release --locked' ]] || exit 99
mkdir -p target/release/deps
binary="$crate"; [[ "$crate" != finishingbot ]] || binary=finishing-bot
printf '#!/usr/bin/env bash\necho %s\n' "$crate" > "target/release/$binary"
chmod +x "target/release/$binary"
[[ "$crate" != broken ]] || exit 1
# Only top-level executables belong in the destination.
printf 'library\n' > target/release/libfixture.rlib
printf 'dependency metadata\n' > target/release/fixture.d
chmod +x target/release/fixture.d
printf '#!/usr/bin/env bash\nexit 0\n' > target/release/deps/internal-helper
chmod +x target/release/deps/internal-helper
if [[ "$crate" == rhodibot ]]; then
  cp "target/release/$binary" target/release/rhodi-helper
fi
STUB
cat > "$TOOLS/timeout" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\t%s\n' "${PWD##*/}" "$*" >> "$FLEET_BUILD_TIMEOUTS"
# Model a timed-out crate deterministically, without waiting or spawning it.
[[ "${PWD##*/}" != slow ]] || exit 124
shift
exec "$@"
STUB
chmod +x "$TOOLS/cargo" "$TOOLS/timeout"

run_build() { # label arguments... ; sets rc, clears invocation logs
  local label="$1"; shift
  : > "$FLEET_BUILD_CALLS"; : > "$FLEET_BUILD_TIMEOUTS"
  rc=0
  PATH="$TOOLS:$PATH" GITHUB_ACTIONS='' bash "$BUILDER" \
    --fleet "$FLEET" --bin-dir "$TMP/$label" "$@" \
    < "$TMP/plan" > "$TMP/$label.log" 2>&1 || rc=$?
}
files() { find "$1" -maxdepth 1 -type f -printf '%f\n' | sort | paste -sd ' ' -; }
: > "$TMP/plan"

# Explicit crate list: continue after both an unknown crate and a failed build.
run_build mixed --crates 'rhodibot unknown broken finishingbot glambot'
check "mixed successful and failing builds do not fail the helper" 0 "$rc"
check "builds run inside each selected crate with release and locked flags" \
  $'rhodibot\tbuild --release --locked\nbroken\tbuild --release --locked\nfinishingbot\tbuild --release --locked\nglambot\tbuild --release --locked' \
  "$(cat "$FLEET_BUILD_CALLS")"
check "only top-level executables from successful builds are copied" \
  'finishing-bot glambot rhodi-helper rhodibot' "$(files "$TMP/mixed")"
check "copied adapters remain executable" yes "$([ -x "$TMP/mixed/finishing-bot" ] && echo yes || echo no)"
check "copied adapter bytes match the crate output" \
  "$(sha256sum < "$FLEET/bots/rhodibot/target/release/rhodibot")" "$(sha256sum < "$TMP/mixed/rhodibot")"
check "failed crates' partial binaries are not copied" no "$([ -e "$TMP/mixed/broken" ] && echo yes || echo no)"
check "unknown crate warning names the absent manifest" yes \
  "$(grep -q 'bots/unknown has no Cargo.toml' "$TMP/mixed.log" && echo yes || echo no)"
check "failed crate warning explains its report-back consequence" yes \
  "$(grep -q 'bots/broken did not build; its row will read error' "$TMP/mixed.log" && echo yes || echo no)"
check "build summary counts successes and failures" yes \
  "$(grep -q '3 crate(s) built, 2 failed' "$TMP/mixed.log" && echo yes || echo no)"
check "default timeout is applied to every attempted build" 4 \
  "$(grep -c $'\t900 cargo build --release --locked$' "$FLEET_BUILD_TIMEOUTS")"

# Consume the real driver's plan, including aliases for the same crate and
# rows with deliberately empty crate columns. Each crate should build once.
bash "$ROOT/scripts/fleet-report-back.sh" --plan --enrolment "" \
  --member finishingbot --member finishbot --member rhodibot \
  --member echidnabot --member sustainabot > "$TMP/plan"
run_build planned
check "stdin plan completes successfully" 0 "$rc"
check "stdin plan deduplicates aliases and skips members without crates" \
  $'finishingbot\tbuild --release --locked\nrhodibot\tbuild --release --locked' "$(cat "$FLEET_BUILD_CALLS")"
check "stdin plan publishes just the selected crates' executables" \
  'finishing-bot rhodi-helper rhodibot' "$(files "$TMP/planned")"

# --crates takes precedence over a nonempty stdin plan.
run_build explicit --crates glambot
check "an explicit crate list overrides stdin" $'glambot\tbuild --release --locked' "$(cat "$FLEET_BUILD_CALLS")"
check "unselected adapters are not copied" glambot "$(files "$TMP/explicit")"

: > "$TMP/plan"
run_build empty
check "an empty plan succeeds" 0 "$rc"
check "an empty plan invokes no Cargo commands" 0 "$(wc -l < "$FLEET_BUILD_CALLS")"
check "an empty plan publishes no binaries" '' "$(files "$TMP/empty")"
check "an empty plan summary reports zero work" yes \
  "$(grep -q '0 crate(s) built, 0 failed' "$TMP/empty.log" && echo yes || echo no)"

run_build timed --crates 'slow panicbot' --build-timeout 23
check "a timed-out build does not fail the helper" 0 "$rc"
check "a timed-out build still allows the next crate to build" panicbot "$(files "$TMP/timed")"
check "the configured timeout reaches each build" \
  $'slow\t23 cargo build --release --locked\npanicbot\t23 cargo build --release --locked' "$(cat "$FLEET_BUILD_TIMEOUTS")"
check "a timed-out crate counts as a failure" yes \
  "$(grep -q '1 crate(s) built, 1 failed' "$TMP/timed.log" && echo yes || echo no)"

run_build failed --crates 'unknown broken'
check "even when every crate fails, the helper completes" 0 "$rc"
check "all-failed builds publish nothing" '' "$(files "$TMP/failed")"
check "all-failed summary counts both failures" yes \
  "$(grep -q '0 crate(s) built, 2 failed' "$TMP/failed.log" && echo yes || echo no)"

# Exercise the Actions log format as well as local stderr warnings.
rc=0
PATH="$TOOLS:$PATH" GITHUB_ACTIONS=true bash "$BUILDER" \
  --fleet "$FLEET" --bin-dir "$TMP/actions" --crates broken \
  > "$TMP/actions.log" 2>&1 || rc=$?
check "Actions builds preserve advisory failure handling" 0 "$rc"
check "Actions build groups open and close" $'::group::cargo build --release --locked (bots/broken)\n::endgroup::' \
  "$(grep -E '^::(group|endgroup)::' "$TMP/actions.log")"
check "Actions failures use warning annotations" yes \
  "$(grep -q '^::warning title=adapter build failed::bots/broken' "$TMP/actions.log" && echo yes || echo no)"

# A minimal PATH makes the missing-Cargo branch independent of the host's
# installed Rust toolchains. Invoke bash by its existing absolute path.
BASH_BIN="$(command -v bash)"
mkdir -p "$TMP/no-cargo"
ln -s "$(command -v mkdir)" "$TMP/no-cargo/mkdir"
rc=0
PATH="$TMP/no-cargo" "$BASH_BIN" "$BUILDER" --fleet "$FLEET" \
  --bin-dir "$TMP/no-toolchain" --crates rhodibot > "$TMP/no-toolchain.log" 2>&1 || rc=$?
check "missing Cargo does not fail the producer" 0 "$rc"
check "missing Cargo gets an explicit warning" yes \
  "$(grep -q 'cargo is not on PATH; no adapter can be built' "$TMP/no-toolchain.log" && echo yes || echo no)"
check "missing Cargo publishes no binaries" '' "$(files "$TMP/no-toolchain")"

expect_usage() {
  local label="$1" rc=0; shift
  bash "$BUILDER" "$@" < /dev/null > "$TMP/usage.log" 2>&1 || rc=$?
  check "$label exits with usage status" 2 "$rc"
}
expect_usage "builder missing required options"
expect_usage "builder missing bin-dir" --fleet "$FLEET"
expect_usage "builder invalid fleet directory" --fleet "$TMP/absent" --bin-dir "$TMP/invalid"
expect_usage "builder unknown option" --unknown
check "invalid builder arguments create no bin directory" no "$([ -e "$TMP/invalid" ] && echo yes || echo no)"

# --- the build budget: one job timeout must not swallow every envelope --------
run_build budget-zero --crates "rhodibot panicbot" --panic-attack 0123456789abcdef0123456789abcdef01234567 --build-budget 0
check "budget spent: no cargo invocation is attempted" "0" "$(grep -c . "$FLEET_BUILD_CALLS" || true)"
check "budget spent: every crate is warned as unbuilt" "2" "$(grep -c 'build budget exhausted' "$TMP/budget-zero.log" || true)"
check "budget spent: panic-attack is warned, not attempted" "1" "$(grep -c 'spent before its turn' "$TMP/budget-zero.log" || true)"
check "budget spent: the run still completes (exit 0)" 0 "$rc"
run_build budget-wide --crates rhodibot --build-budget 100000 --build-timeout 900
check "budget above the per-build limit: the per-build limit applies" "1" "$(grep -c $'\t900 cargo build --release --locked$' "$FLEET_BUILD_TIMEOUTS" || true)"
run_build budget-narrow --crates rhodibot --build-budget 100000 --build-timeout 60
check "per-build limit below the budget: the limit applies" "1" "$(grep -c $'\t60 cargo build --release --locked$' "$FLEET_BUILD_TIMEOUTS" || true)"

# --- a crate name that is a path is refused before any cargo invocation --------
run_build traversal --crates "../../evil rhodibot"
check "a path in the crate list is skipped" "1" "$(grep -c 'is not a crate directory name' "$TMP/traversal.log" || true)"
check "...and only the real crate is built" $'rhodibot\tbuild --release --locked' "$(cat "$FLEET_BUILD_CALLS")"

echo
echo "fleet-report-back-build-test: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
