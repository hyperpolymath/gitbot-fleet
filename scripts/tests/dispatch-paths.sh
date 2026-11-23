#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Exercise the actual dispatch orchestration with inert fixes and isolated data.
# Optional DISPATCH_TEST_HYPATIA_HOME and DISPATCH_TEST_MIX_BIN use the canonical
# generator for successful cases. DISPATCH_TEST_EVIDENCE_DIR retains emitted rows
# including the actual canonical feed for the separate consumer witness.
set -euo pipefail
fleet_test_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
fixture="$(mktemp -d)"
trap 'rm -rf "$fixture"' EXIT
mkdir -p "$fixture/repos/sample" "$fixture/checkout/scripts/lib" "$fixture/primary/dispatch" \
    "$fixture/bin" "$fixture/hypatia"
cp "$fleet_test_root/scripts/repo-path-overrides.json" "$fixture/checkout/scripts/"
cp "$fleet_test_root/scripts/lib/outcome-verdict.sh" "$fixture/checkout/scripts/lib/"
month="$(date -u +%Y-%m)"
starts="$fixture/checkout/shared-context/learning/fix-attempts.jsonl"
feed="$fixture/checkout/shared-context/learning/fix-outcomes.jsonl"
primary="$fixture/primary/dispatch/fleet-outcomes/$month.jsonl"
central="$fixture/central/dispatch/fleet-outcomes/$month.jsonl"
manifest="$fixture/primary/dispatch/pending.jsonl"
canonical_mix="${DISPATCH_TEST_MIX_BIN:-$(command -v mix || true)}"
pass=0
check() {
    local label="$1"; shift
    if ! "$@" > "$fixture/assertion-result" 2>&1; then
        echo "FAIL: $label" >&2
        cat "$fixture/assertion-result" >&2
        [[ ! -f "$fixture/result" ]] || cat "$fixture/result" >&2
        exit 1
    fi
    pass=$((pass + 1))
    printf 'PASS: %s\n' "$label"
}

# While dispatch is quarantined, the production runner refuses every call. Only
# this disposable copy has exactly the BLOCKED printf and following exit removed;
# it is unreachable from production entry points and uses only fixture adapters.
real_runner="$fleet_test_root/scripts/dispatch-runner.sh"
seam_runner="$fixture/checkout/scripts/dispatch-runner.sh"
interlock_msg='BLOCKED: repository directive enforcement is not qualified'
check 'production interlock exists' grep -qF "$interlock_msg" "$real_runner"
awk -v msg="$interlock_msg" '
    skip_exit && /^exit 78$/ { skip_exit = 0; next }
    index($0, msg) && /^printf / { skip_exit = 1; next }
    { print }
' "$real_runner" > "$seam_runner"
check 'test copy removed only the two interlock lines' test \
    "$(( $(wc -l < "$real_runner") - $(wc -l < "$seam_runner") ))" -eq 2
if grep -qF "$interlock_msg" "$seam_runner" || cmp -s "$real_runner" "$seam_runner"; then
    echo 'FAIL: could not build the test-only seam runner' >&2
    exit 1
fi

cat > "$fixture/bin/mix" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$*" >> "$FIXTURE_ROOT/mix-calls"
case "$*" in
    'help hypatia.record_outcome') echo '--verdict verified|no_op|incomplete|unverified|failed'; exit 0 ;;
    hypatia.record_outcome*)
        if [[ "$FIXTURE_ALLOW_REFUSAL" != yes ]]; then
            touch "$FIXTURE_ROOT/unexpected-record"
            echo 'independent recording forbidden for a producer attempt' >&2
            exit 99
        fi
        exit 0 ;;
    hypatia.event_id) ;;
    *) echo 'unexpected mix arguments' >&2; exit 98 ;;
esac
fixture_id=018e4000-0000-8000-8000-000000000001
case "$FIXTURE_GENERATOR_MODE" in
    fail) echo "$fixture_id"; exit 3 ;;
    empty) exit 0 ;;
    bad) echo 'not-an-event-id'; exit 0 ;;
    v4) echo "${fixture_id:0:14}4${fixture_id:15}"; exit 0 ;;
    variant) echo "${fixture_id:0:19}c${fixture_id:20}"; exit 0 ;;
    uppercase) echo "${fixture_id^^}"; exit 0 ;;
    trailing) printf '%s\ntrailing noise\n' "$fixture_id"; exit 0 ;;
    chatter) echo 'Compiling fixture source' ;;
esac
if [[ -n "${DISPATCH_TEST_HYPATIA_HOME:-}" ]]; then
    exec "$DISPATCH_TEST_MIX_BIN" hypatia.event_id
fi
# These are fixed wire fixtures, not a replacement event-id generator. The lock
# only lets two background runner jobs consume distinct fixture rows safely.
{
    flock 9
    head -n 1 "$FIXTURE_ROOT/ids"
    tail -n +2 "$FIXTURE_ROOT/ids" > "$FIXTURE_ROOT/ids.next"
    mv "$FIXTURE_ROOT/ids.next" "$FIXTURE_ROOT/ids"
} 9> "$FIXTURE_ROOT/ids.lock"
STUB
cat > "$fixture/checkout/scripts/fix-fixture.sh" <<'STUB'
#!/usr/bin/env bash
# outcome-predicate: --check
set -euo pipefail
if [[ "$1" == --check ]]; then
    if [[ -e "$2/needs-fix" ]]; then echo 1; else echo 0; fi
    exit 0
fi
# This observation is made by the fixer, before it changes the fixture repo.
jq -e '.kind == "start" and .producer == "dispatch-runner"' \
    "$FIXTURE_ROOT/checkout/shared-context/learning/fix-attempts.jsonl" >/dev/null
printf '%s\n' started >> "$FIXTURE_ROOT/fix-calls"
case "$FIXTURE_FIX_MODE" in
    interrupt) kill -TERM "$PPID"; exit 0 ;;
    fail) exit 7 ;;
esac
rm -f "$1/needs-fix"
STUB
cat > "$fixture/bin/rra" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
[[ "$1" == fix && "$2" == --repo && "$4" == --commit ]]
exec "$FIXTURE_ROOT/checkout/scripts/fix-fixture.sh" "$3"
STUB
chmod +x "$fixture/bin/mix" "$fixture/bin/rra" "$fixture/checkout/scripts/fix-fixture.sh"

reset_fixture() {
    rm -rf "$fixture/primary/outcomes" "$fixture/primary/dispatch/fleet-outcomes" \
        "$fixture/central" "$fixture/checkout/shared-context" "$fixture/kin"
    rm -f "$fixture/unexpected-record"
    : > "$fixture/mix-calls"
    : > "$fixture/fix-calls"
    printf '%s\n' '018e4000-0000-8000-8000-000000000001' \
        '018e4000-0000-8000-8000-000000000002' > "$fixture/ids"
    touch "$fixture/repos/sample/needs-fix"
    printf '%s\n' '{"repo":"sample","tier":"eliminate","strategy":"auto_execute","pattern_id":"fixture","recipe_id":"recipe-fixture","fix_script":"fix-fixture.sh","auto_fixable":true}' > "$manifest"
    generator_mode=ok fix_mode=ok report_mode=on parallel=1 limit=1 allow_refusal=no
    hypatia_store="$fixture/learning-store"
    hypatia_fleet="$fixture/checkout"
    runner_args=()
    hypatia_home="${DISPATCH_TEST_HYPATIA_HOME:-$fixture/hypatia}"
    runner_path="$fixture/bin:$PATH"
    rra_bin="$fixture/bin/rra"
}
invoke_runner() {
    local runner="$1" store="$2"
    env -u GITHUB_TOKEN -u FLEET_DISPATCH_TOKEN -u BASH_ENV -u ENV \
        PATH="$runner_path" FLEET_ROOT="$fixture/checkout" REPOS_BASE="$fixture/repos" \
        HYPATIA_DATA="$fixture/primary" VERISIMDB_DATA="$store" \
        HYPATIA_HOME="$hypatia_home" HYPATIA_OUTCOME_REPORT="$report_mode" \
        HYPATIA_FLEET_PATH="$hypatia_fleet" HYPATIA_VERISIM_DATA_PATH="$hypatia_store" \
        KIN_DIR="$fixture/kin" RRA_BIN="$rra_bin" FIXTURE_ROOT="$fixture" \
        FIXTURE_GENERATOR_MODE="$generator_mode" FIXTURE_FIX_MODE="$fix_mode" \
        FIXTURE_ALLOW_REFUSAL="$allow_refusal" \
        DISPATCH_TEST_MIX_BIN="$canonical_mix" \
        bash "$runner" --limit "$limit" --parallel "$parallel" "${runner_args[@]}"
}
run_dispatch() { invoke_runner "$seam_runner" "${1:-$fixture/central}" > "$fixture/result" 2>&1; }
expect_failure() {
    local label="$1" rc=0
    run_dispatch || rc=$?
    check "$label: runner refuses" test "$rc" -ne 0
}
no_fix_or_outcome() {
    check "$1: fixer was not called" test ! -s "$fixture/fix-calls"
    check "$1: no outcome was written" test ! -s "$primary"
    check "$1: no canonical feed row was written" test ! -s "$feed"
}
retain_rows() {
    [[ -n "${DISPATCH_TEST_EVIDENCE_DIR:-}" ]] || return 0
    mkdir -p "$DISPATCH_TEST_EVIDENCE_DIR"
    cp "$starts" "$DISPATCH_TEST_EVIDENCE_DIR/$1-starts.jsonl"
    cp "$feed" "$DISPATCH_TEST_EVIDENCE_DIR/$1-feed.jsonl"
    cp "$primary" "$DISPATCH_TEST_EVIDENCE_DIR/$1-primary.jsonl"
    cp "$central" "$DISPATCH_TEST_EVIDENCE_DIR/$1-central.jsonl"
}

reset_fixture
quarantine_rc=0
invoke_runner "$real_runner" "$fixture/primary/." > "$fixture/result" 2>&1 || quarantine_rc=$?
check 'production runner exits 78 before manifest work' test "$quarantine_rc" -eq 78
check 'quarantine calls no generator' test ! -s "$fixture/mix-calls"
check 'quarantine writes no attempt or outcome state' test ! -e "$fixture/checkout/shared-context"
check 'quarantine writes no primary state' test ! -e "$fixture/primary/outcomes"

# Two identical manifest entries and initial repo states are two attempts.
run_dispatch "$fixture/primary/."
check 'actual runner delivers to the canonical fleet feed' test -s "$feed"
check 'attempt declares frozen v1 and actual relative target' jq -e \
    '.attempt_contract == "fleet-attempt/1" and .repo_relative_path == "sample"' "$feed"
check 'identified attempt never calls the independent recorder' test ! -e "$fixture/unexpected-record"
check 'canonical feed and local mirror are byte-identical' cmp -s "$feed" "$primary"
check 'raw transport never writes the monthly learning ledger' test ! -e "$fixture/primary/outcomes"
check 'first outcome is verified' jq -e '.verdict == "verified" and .before_hits == 1 and .after_hits == 0' "$primary"
check 'same store gets exactly one outcome' test "$(wc -l < "$primary")" -eq 1
check 'start has exactly the ruled six fields' jq -e \
    'keys == ["chain_version","event_id","kind","producer","recipe_id","repo"] and .chain_version == "e0-receipt/1" and .kind == "start" and .producer == "dispatch-runner" and .recipe_id == "recipe-fixture" and .repo == "sample"' "$starts"
check 'start uses canonical compact sorted JSON' test "$(cat "$starts")" "=" "$(jq -cS . "$starts")"
check 'event id has ADR-008 v8 text and variant' jq -e \
    '.event_id | test("^[0-9a-f]{8}-[0-9a-f]{4}-8[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$")' "$starts"
check 'first start and outcome have one id' test "$(jq -r .event_id "$starts")" "=" "$(jq -r .event_id "$primary")"
touch "$fixture/repos/sample/needs-fix"
run_dispatch
check 'each identical attempt minted exactly once' test "$(grep -c '^hypatia.event_id$' "$fixture/mix-calls")" -eq 2
check 'identical attempts have distinct event ids' jq -se 'map(.event_id) | length == 2 and (unique | length == 2)' "$primary"
check 'both starts match their outcomes' test "$(jq -r .event_id "$starts")" "=" "$(jq -r .event_id "$primary")"
check 'mirrored outcome is the exact same row' test "$(tail -n 1 "$primary")" "=" "$(cat "$central")"
check 'mirror contains one outcome' test "$(wc -l < "$central")" -eq 1
check 'all canonical feed rows match the local transport copy' cmp -s "$feed" "$primary"
check 'both identified attempts avoid the independent recorder' test ! -e "$fixture/unexpected-record"
retain_rows success

reset_fixture
fix_mode=fail
run_dispatch "$fixture/primary/."
run_dispatch
check 'failed fix retains its producer id and exit code' jq -e '.verdict == "failed" and .exit_code == 7 and (.event_id | type == "string")' "$primary"
check 'failed attempt keeps one id across all copies' test "$(jq -r .event_id "$starts")" "=" "$(jq -r .event_id "$primary")"
check 'identical failed attempts have distinct event ids' jq -se 'map(.event_id) | length == 2 and (unique | length == 2)' "$primary"
check 'failed mirrored outcome is byte-identical' test "$(tail -n 1 "$primary")" "=" "$(cat "$central")"
check 'failed attempts go to the same canonical feed' cmp -s "$feed" "$primary"
check 'failed attempts avoid the independent recorder' test ! -e "$fixture/unexpected-record"
retain_rows failure

reset_fixture
fix_mode=interrupt
expect_failure 'interrupted after start'
check 'interrupted attempt leaves exactly one start' test "$(wc -l < "$starts")" -eq 1
check 'interrupted fixer observed its durable start' test "$(wc -l < "$fixture/fix-calls")" -eq 1
check 'interrupted attempt has no outcome' test ! -s "$primary"

for mode in fail empty bad v4 variant uppercase trailing; do
    reset_fixture
    generator_mode="$mode"
    expect_failure "generator $mode"
    no_fix_or_outcome "generator $mode"
    check "generator $mode: no start was written" test ! -e "$starts"
    check "generator $mode: diagnostic is visible" grep -qi 'event.id' "$fixture/result"
done
reset_fixture
generator_mode=chatter
run_dispatch
check 'compile chatter is excluded from the id' jq -e '.event_id | test("^[0-9a-f-]{36}$")' "$primary"

reset_fixture
hypatia_home="$fixture/missing-hypatia"
expect_failure 'missing Hypatia checkout'
no_fix_or_outcome 'missing Hypatia checkout'
mkdir "$fixture/no-mix"
for command_name in bash dirname wc tr date mkdir mktemp jq realpath sed grep rm; do
    ln -s "$(command -v "$command_name")" "$fixture/no-mix/$command_name"
done
reset_fixture
runner_path="$fixture/no-mix"
expect_failure 'missing mix executable'
no_fix_or_outcome 'missing mix executable'

for failure in directory append; do
    reset_fixture
    mkdir -p "$fixture/checkout/shared-context"
    if [[ "$failure" == directory ]]; then
        : > "$fixture/checkout/shared-context/learning"
    else
        mkdir -p "$starts"
    fi
    expect_failure "start $failure failure"
    no_fix_or_outcome "start $failure failure"
done
for failure in feed primary central; do
    reset_fixture
    report_mode=on
    case "$failure" in feed) mkdir -p "$feed";; primary) mkdir -p "$primary";; central) mkdir -p "$central";; esac
    expect_failure "$failure outcome append failure"
    check "$failure append failure leaves its start" test -s "$starts"
    check "$failure append failure does not call direct outcome reporting" test \
        "$(grep -c '^hypatia.record_outcome ' "$fixture/mix-calls" || true)" -eq 0
    case "$failure" in
        feed)
            check 'feed failure writes no local mirror' test ! -e "$primary"
            check 'feed failure writes no central mirror' test ! -e "$central" ;;
        primary|central)
            check "$failure mirror failure preserves the committed canonical feed" jq -e '.event_id | type == "string"' "$feed"
            check "$failure mirror failure reports the existing commit" grep -q 'canonical feed row is committed' "$fixture/result"
            check "$failure mirror failure never retries the fixer" test "$(wc -l < "$fixture/fix-calls")" -eq 1 ;;
    esac
done

# Infrastructure failures must reach the parent under --parallel too: wait
# without a job id otherwise hides a failed child behind an overall exit 0.
for failure in generator start feed primary central; do
    reset_fixture
    limit=0 parallel=2
    cat "$manifest" > "$fixture/entry"
    cat "$fixture/entry" >> "$manifest"
    case "$failure" in
        generator) generator_mode=fail ;;
        start) mkdir -p "$starts" ;;
        feed) mkdir -p "$feed" ;;
        primary) mkdir -p "$primary" ;;
        central) mkdir -p "$central" ;;
    esac
    expect_failure "parallel $failure failure"
    if [[ "$failure" == generator || "$failure" == start ]]; then
        no_fix_or_outcome "parallel $failure failure"
    fi
done

# Both executable mechanisms require the same pre-execution identity contract.
reset_fixture
jq -c 'del(.fix_script)' "$manifest" > "$fixture/entry"
mv "$fixture/entry" "$manifest"
run_dispatch
check 'RRA attempt keeps start identity' test "$(jq -r .event_id "$starts")" "=" "$(jq -r .event_id "$primary")"
check 'RRA remains unverified without a predicate' jq -e '.verdict == "unverified"' "$primary"
reset_fixture
generator_mode=fail
jq -c 'del(.fix_script)' "$manifest" > "$fixture/entry"
mv "$fixture/entry" "$manifest"
expect_failure 'RRA generator failure'
no_fix_or_outcome 'RRA generator failure'

reset_fixture
generator_mode=fail
runner_args=(--dry-run)
run_dispatch
no_fix_or_outcome 'fixture dry run'
check 'fixture dry run mints no id' test ! -s "$fixture/mix-calls"
reset_fixture
generator_mode=fail rra_bin="$fixture/no-rra"
jq -c 'del(.fix_script)' "$manifest" > "$fixture/entry"
mv "$fixture/entry" "$manifest"
run_dispatch
no_fix_or_outcome 'no executable fix mechanism'
check 'no executable fix mechanism mints no id' test ! -s "$fixture/mix-calls"

reset_fixture
limit=0 parallel=2
cat "$manifest" > "$fixture/entry"
cat "$fixture/entry" >> "$manifest"
run_dispatch
check 'parallel attempts each receive an id' jq -se 'map(.event_id) | length == 2 and (unique | length == 2)' "$primary"
check 'parallel starts and outcomes name the same attempts' test \
    "$(jq -r .event_id "$starts" | sort)" "=" "$(jq -r .event_id "$primary" | sort)"
check 'parallel repo lock classifies only one verified fix' jq -se 'map(.verdict) | sort == ["no_op","verified"]' "$primary"
check 'parallel identified attempts avoid the independent recorder' test ! -e "$fixture/unexpected-record"

# Physical aliases coalesce transport writes but may never mix starts or raw
# transport with a canonical learning ledger. These fixtures use real symlinks
# and hardlinks; no permission test relies on the test user being unprivileged.
for alias_kind in symlink hardlink; do
    reset_fixture
    mkdir -p "$(dirname "$primary")" "$(dirname "$feed")"
    : > "$feed"
    if [[ "$alias_kind" == symlink ]]; then ln -s "$feed" "$primary"; else ln "$feed" "$primary"; fi
    run_dispatch
    check "$alias_kind alias of feed is appended once" test "$(wc -l < "$feed")" -eq 1
    check "$alias_kind feed alias preserves mirror bytes" cmp -s "$feed" "$central"

    reset_fixture
    mkdir -p "$(dirname "$feed")"
    : > "$starts"
    if [[ "$alias_kind" == symlink ]]; then ln -s "$starts" "$feed"; else ln "$starts" "$feed"; fi
    expect_failure "$alias_kind alias between feed and starts"
    no_fix_or_outcome "$alias_kind alias between feed and starts"
    check "$alias_kind start alias mints no event" test ! -s "$fixture/mix-calls"
done
reset_fixture
mkdir -p "$(dirname "$primary")" "$(dirname "$central")"
: > "$primary"
ln "$primary" "$central"
run_dispatch
check 'hardlinked local and central transports are appended once' test "$(wc -l < "$primary")" -eq 1
check 'hardlinked transports contain the canonical bytes' cmp -s "$feed" "$primary"
reset_fixture
mkdir -p "$(dirname "$primary")" "$(dirname "$starts")"
: > "$starts"
ln "$starts" "$primary"
expect_failure 'transport alias of starts'
no_fix_or_outcome 'transport alias of starts'

for alias_target in feed primary; do
    reset_fixture
    mkdir -p "$hypatia_store/outcomes" "$(dirname "$feed")" "$(dirname "$primary")"
    printf '%s\n' '{"existing":"learning-row"}' > "$hypatia_store/outcomes/existing.jsonl"
    if [[ "$alias_target" == feed ]]; then
        ln "$hypatia_store/outcomes/existing.jsonl" "$feed"
    else
        ln "$hypatia_store/outcomes/existing.jsonl" "$primary"
    fi
    expect_failure "$alias_target hardlink into explicit learning ledger"
    check "$alias_target ledger alias never calls the fixer" test ! -s "$fixture/fix-calls"
    check "$alias_target ledger alias mints no event" test ! -s "$fixture/mix-calls"
    check "$alias_target ledger bytes are unchanged" test "$(cat "$hypatia_store/outcomes/existing.jsonl")" = '{"existing":"learning-row"}'
done
reset_fixture
mkdir -p "$hypatia_store/outcomes" "$fixture/checkout/shared-context"
ln -s "$hypatia_store/outcomes" "$fixture/checkout/shared-context/learning"
expect_failure 'canonical feed directory inside explicit learning ledger'
check 'ledger directory alias never executes' test ! -s "$fixture/fix-calls"
check 'ledger directory alias creates no start' test ! -e "$hypatia_store/outcomes/fix-attempts.jsonl"
reset_fixture
hypatia_fleet="$fixture/another-fleet"
expect_failure 'explicit consumer fleet root differs'
no_fix_or_outcome 'explicit consumer fleet root differs'

# The v1 path is the actual target, after alias/program_path/symlink resolution.
reset_fixture
mkdir -p "$fixture/repos/actual/nested"
touch "$fixture/repos/actual/nested/needs-fix"
jq -c --arg path "$fixture/repos/sample/../actual/./nested" '.repo="manifest-name" | .program_path=$path' "$manifest" > "$fixture/entry"
mv "$fixture/entry" "$manifest"
run_dispatch
check 'program_path binds the actual normalized relative target' jq -e \
    '.repo == "manifest-name" and .repo_relative_path == "actual/nested"' "$feed"
reset_fixture
ln -s "$fixture/repos/actual/nested" "$fixture/repos/target-link"
jq -c '.repo="target-link"' "$manifest" > "$fixture/entry"
mv "$fixture/entry" "$manifest"
run_dispatch
check 'target symlink records its actual resolved path' jq -e '.repo_relative_path == "actual/nested"' "$feed"
reset_fixture
mkdir -p "$fixture/repos/developer-ecosystem/julia-ecosystem/packages/Axiom.jl"
jq -c '.repo="Axiom.jl"' "$manifest" > "$fixture/entry"
mv "$fixture/entry" "$manifest"
run_dispatch
check 'repository override records its actual relative target' jq -e \
    '.repo_relative_path == "developer-ecosystem/julia-ecosystem/packages/Axiom.jl"' "$feed"

# No attempted execution means no new attempt id. Preserve the licence refusal.
reset_fixture
generator_mode=fail
allow_refusal=yes
jq -c '.recipe_id = "recipe-license-hygiene"' "$manifest" > "$fixture/entry"
mv "$fixture/entry" "$manifest"
run_dispatch
check 'licence refusal still has its original outcome' jq -e '.verdict == "refused_license_policy" and (has("event_id") | not)' "$primary"
check 'licence refusal mints no execution id' test "$(grep -c '^hypatia.event_id$' "$fixture/mix-calls" || true)" -eq 0
check 'licence refusal retains only its false-positive callback' test "$(cat "$fixture/mix-calls")" = \
    'hypatia.record_outcome --recipe recipe-license-hygiene --repo sample --file . --outcome false_positive'
check 'licence refusal has no attempt contract or relative target' jq -e \
    '(has("attempt_contract") or has("repo_relative_path")) | not' "$feed"
check 'licence refusal never calls the fixer' test ! -s "$fixture/fix-calls"
check 'licence refusal creates no start' test ! -e "$starts"
check 'licence callback is the only permitted recording call' test ! -e "$fixture/unexpected-record"

reset_fixture
generator_mode=fail
printf '%s\n' '{"repo":"sample","tier":"control","strategy":"review","pattern_id":"review-fixture"}' > "$manifest"
run_dispatch
check 'review still writes its finding' test -s "$fixture/checkout/shared-context/findings/pending/sample--review-fixture.json"
printf '%s\n' '{"repo":"sample","tier":"control","strategy":"report_only","pattern_id":"advisory-fixture"}' > "$manifest"
run_dispatch
check 'report-only still writes its advisory' test -s "$fixture/checkout/shared-context/advisories/$month.jsonl"
check 'review and report-only mint no ids' test ! -s "$fixture/mix-calls"
check 'paths stay inside configured checkout' test ! -d "$fixture/repos/gitbot-fleet"
mkdir -p "$fixture/repos/developer-ecosystem/julia-ecosystem/packages/Axiom.jl"
printf '%s\n' '{"repo":"Axiom.jl","tier":"control","strategy":"review","pattern_id":"alias-fixture"}' > "$manifest"
run_dispatch
check 'legacy repository path alias remains a fallback' test -s "$fixture/checkout/shared-context/findings/pending/Axiom.jl--alias-fixture.json"
printf 'Dispatch path and producer identity contracts: %s passed\n' "$pass"
