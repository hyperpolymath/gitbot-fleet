#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Contracts of the three workflow-pin fix scripts, each against a planted
# defect, a planted non-defect that must be left alone, and a second run that
# must change nothing. Offline: no network, no gh. Run by rust.yml
# (dispatch-contracts) and `just test`.
# shellcheck disable=SC2034  # out/rc variables are read inside check()'s eval strings
set -euo pipefail
fleet="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
fx="$(mktemp -d)"
trap 'rm -rf "$fx"' EXIT
# Keep the fixers' own temporary files inside the fixture, including failures.
export TMPDIR="$fx"
pass=0; fail=0

# Increment the pass count and print assertion label $1 to stdout.
ok()   { pass=$((pass + 1)); echo "  ok   $1"; }

# Increment the failure count and print assertion label $1 to stderr.
bad()  { fail=$((fail + 1)); echo "  FAIL $1" >&2; }

# Evaluate trusted fixture assertion $2 and record its result under label $1.
check(){ if eval "$2"; then ok "$1"; else bad "$1"; fi; }
# Capture both channels and the exit status, including expected failures.
run_fix() { rc=0; out=$("$@" 2>&1) || rc=$?; }

# ── 1. fix-rust-toolchain-input.sh ───────────────────────────────────────────
mkdir -p "$fx/tc/.github/workflows"
cat > "$fx/tc/.github/workflows/rust.yml" <<'YML'
name: Rust
jobs:
  check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1 # v7.0.1
      - name: Install Rust toolchain
        uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772 # stable
        with:
          components: clippy, rustfmt
      - uses: dtolnay/rust-toolchain@e2a55d2ffb04f378e9626c28d38b36d230d1e12f
      - uses: dtolnay/rust-toolchain@stable
        with:
          components: rustfmt
      - uses: dtolnay/rust-toolchain@683b588c487a69d01f6c6ddf40f036ff682cc2c8
        with:
          toolchain: 1.85.0
      - run: cargo check --locked
YML
cp "$fx/tc/.github/workflows/rust.yml" "$fx/tc-before.yml"
bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc" >/dev/null 2>&1
check "toolchain: inserted as first key of an existing with: block" \
  "grep -A2 '6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772' '$fx/tc/.github/workflows/rust.yml' | grep -q '^          toolchain: stable$'"
check "toolchain: inserted with a new with: block under a bare uses:" \
  "grep -A2 'e2a55d2ffb04f378e9626c28d38b36d230d1e12f' '$fx/tc/.github/workflows/rust.yml' | grep -q '^        with:$' && grep -A2 'e2a55d2ffb04f378e9626c28d38b36d230d1e12f' '$fx/tc/.github/workflows/rust.yml' | grep -q '^          toolchain: stable$'"
check "a tag-pinned step is untouched" \
  "grep -A2 'rust-toolchain@stable' '$fx/tc/.github/workflows/rust.yml' | grep -vq 'toolchain: stable$' "
check "a step that already names a toolchain keeps it, once" \
  "[ \$(grep -c 'toolchain: 1.85.0' '$fx/tc/.github/workflows/rust.yml') -eq 1 ] && ! grep -B1 'toolchain: 1.85.0' '$fx/tc/.github/workflows/rust.yml' | grep -q 'toolchain: stable'"
check "the checkout step and the run step are untouched" \
  "grep -q 'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1 # v7.0.1' '$fx/tc/.github/workflows/rust.yml' && grep -q 'cargo check --locked' '$fx/tc/.github/workflows/rust.yml'"
check "exactly two steps were fixed" \
  "[ \$(grep -c 'toolchain: stable' '$fx/tc/.github/workflows/rust.yml') -eq 2 ]"
cp "$fx/tc/.github/workflows/rust.yml" "$fx/tc-after1.yml"
bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc" >/dev/null 2>&1
check "second run is a no-op" "cmp -s '$fx/tc-after1.yml' '$fx/tc/.github/workflows/rust.yml'"
check "the fixed file still parses as a step list (actionlint-free structural probe)" \
  "! grep -nE '^\s*with:\s*$' '$fx/tc/.github/workflows/rust.yml' | awk -F: '{print \$1}' | while read -r n; do sed -n \"\$((n+1))p\" '$fx/tc/.github/workflows/rust.yml' | grep -qvE '^\s+[a-z-]+:' && echo bad; done | grep -q bad"
# Planted control: a tree with no SHA-pinned dtolnay step is left byte-identical.
mkdir -p "$fx/tc0/.github/workflows"; printf 'name: x\njobs:\n  a:\n    steps:\n      - uses: dtolnay/rust-toolchain@stable\n' > "$fx/tc0/.github/workflows/x.yml"
cp "$fx/tc0/.github/workflows/x.yml" "$fx/tc0-before.yml"
bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc0" >/dev/null 2>&1
check "control: a tree without the defect is untouched" "cmp -s '$fx/tc0-before.yml' '$fx/tc0/.github/workflows/x.yml'"


# Review cases (gitbot-fleet#615): a `with:` with a trailing comment, a flow
# `with:`, `with:` written above `uses:`, `env.toolchain` (not an input, so it
# must not count), and a quoted key. Each step is inspected whole.
mkdir -p "$fx/tc2/.github/workflows"
cat > "$fx/tc2/.github/workflows/rust.yml" <<'YML'
jobs:
  a:
    steps:
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: # pinned inputs
          components: clippy
      - uses: dtolnay/rust-toolchain@e2a55d2ffb04f378e9626c28d38b36d230d1e12f
        with: { components: rustfmt }
      - with:
          components: llvm-tools
        uses: dtolnay/rust-toolchain@683b588c487a69d01f6c6ddf40f036ff682cc2c8
      - with: { toolchain: nightly }
        uses: dtolnay/rust-toolchain@1234567890abcdef1234567890abcdef12345678
      - uses: dtolnay/rust-toolchain@abcdefabcdefabcdefabcdefabcdefabcdefabcd
        env:
          toolchain: beta
      - env:
          toolchain: beta
        uses: dtolnay/rust-toolchain@0000000000000000000000000000000000000001
        with:
          components: clippy
      - "uses": "dtolnay/rust-toolchain@0000000000000000000000000000000000000002"
      - run: cargo test
YML
f2="$fx/tc2/.github/workflows/rust.yml"
bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc2" >/dev/null 2>&1
# Print each match of pattern $1 in fixture $f2 with $2 following lines,
# merging overlapping groups as grep -A does; return grep's exit status.
after() { grep -A"$2" -- "$1" "$f2"; }   # the N lines after the first match
check "a with: with a trailing comment is reused: toolchain: goes under it, no second with:" \
  "after 6c977a6c 2 | sed -n '2p' | grep -q '^        with: # pinned inputs$' && after 6c977a6c 2 | sed -n '3p' | grep -q '^          toolchain: stable$'"
check "a flow with: { } gets toolchain as its first entry" \
  "grep -q '^        with: { toolchain: stable, components: rustfmt }$' '$f2'"
check "a with: written above uses: is found and extended; nothing is inserted under uses:" \
  "grep -B3 683b588c '$f2' | grep -q '^          toolchain: stable$' && after 683b588c 1 | sed -n '2p' | grep -qv '^        with:'"
check "a flow with: above uses: that names a toolchain is left alone" \
  "grep -q '^      - with: { toolchain: nightly }$' '$f2' && after 12345678 1 | sed -n '2p' | grep -qv '^        with:'"
check "env.toolchain is not an input: the step still gets with: toolchain:, env: is kept" \
  "after abcdefab 2 | sed -n '2p' | grep -q '^        with:$' && after abcdefab 2 | sed -n '3p' | grep -q '^          toolchain: stable$' && grep -A4 abcdefab '$f2' | grep -q '^          toolchain: beta$'"
check "env.toolchain above uses: does not count either; the existing with: block is extended" \
  "grep -A3 '0000000000000000000000000000000000000001' '$f2' | grep -q '^          toolchain: stable$' && grep -B3 '0000000000000000000000000000000000000001' '$f2' | grep -q '^          toolchain: beta$'"
check "a quoted \"uses\" key is recognised" \
  "after 0000000000000000000000000000000000000002 2 | sed -n '2p' | grep -q '^        with:$' && after 0000000000000000000000000000000000000002 2 | sed -n '3p' | grep -q '^          toolchain: stable$'"
check "exactly six steps were fixed, one toolchain: each" \
  "[ \$(grep -c 'toolchain: stable' '$f2') -eq 6 ] && [ \$(grep -c 'toolchain: nightly' '$f2') -eq 1 ] && grep -q 'cargo test' '$f2'"
cp "$f2" "$fx/tc2-after1.yml"
bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc2" >/dev/null 2>&1
check "second run over the review cases is a no-op" "cmp -s '$fx/tc2-after1.yml' '$f2'"

# Review cases (Codex, gitbot-fleet#615): a quoted "with" key; the same text
# inside a `run: |` script and under `env:` (data, not the step's action); a
# tag ref that starts with hex digits (`@1.85.0`); bare `-` openers; and a
# flow-style step the parser must refuse out loud rather than report clean.
mkdir -p "$fx/tc3/.github/workflows"
cat > "$fx/tc3/.github/workflows/rust.yml" <<'YML'
jobs:
  a:
    steps:
      - "uses": "dtolnay/rust-toolchain@0000000000000000000000000000000000000003"
        "with": { components: clippy }
      - name: write a fixture
        run: |
          cat > fixture.yml <<'EOF'
          uses: dtolnay/rust-toolchain@0000000000000000000000000000000000000004
          EOF
      - uses: dtolnay/rust-toolchain@0000000000000000000000000000000000000005
        env:
          uses: dtolnay/rust-toolchain@0000000000000000000000000000000000000006
      - uses: dtolnay/rust-toolchain@1.85.0
      -
        with:
          toolchain: nightly
        uses: dtolnay/rust-toolchain@0000000000000000000000000000000000000007
      -
        uses: dtolnay/rust-toolchain@0000000000000000000000000000000000000008
      - run: cargo test
YML
f3="$fx/tc3/.github/workflows/rust.yml"
cp "$f3" "$fx/tc3-before.yml"
set +e
bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc3" >/dev/null 2>&1; rc_tc3=$?
set -e
check "a quoted \"with\" key is reused: toolchain goes into it, nothing inserted under uses:" \
  "grep -A1 '0000000000000000000000000000000000000003' '$f3' | sed -n '2p' | grep -q '^        \"with\": { toolchain: stable, components: clippy }$'"
check "a SHA ref inside a run: | script is data: left alone, nothing inserted after it" \
  "grep -A1 '0000000000000000000000000000000000000004' '$f3' | sed -n '2p' | grep -q '^          EOF$' && grep -q '^          uses: dtolnay/rust-toolchain@0000000000000000000000000000000000000004$' '$f3'"
check "a SHA ref under env: is data: the step gets with: under its own uses:, env: is kept as is" \
  "grep -A2 '0000000000000000000000000000000000000005' '$f3' | sed -n '2p' | grep -q '^        with:$' && grep -A2 '0000000000000000000000000000000000000005' '$f3' | sed -n '3p' | grep -q '^          toolchain: stable$' && grep -A1 '0000000000000000000000000000000000000006' '$f3' | sed -n '2p' | grep -q '^      - uses: dtolnay/rust-toolchain@1.85.0$'"
check "a tag ref that starts with hex digits (@1.85.0) is a tag: untouched" \
  "grep -A1 'rust-toolchain@1.85.0' '$f3' | sed -n '2p' | grep -q '^      -$'"
check "a bare - opener: a with: toolchain: above uses: counts, nothing inserted" \
  "grep -A1 '0000000000000000000000000000000000000007' '$f3' | sed -n '2p' | grep -q '^      -$'"
check "a bare - opener with a bare uses: gets with: at the keys' column" \
  "grep -A2 '0000000000000000000000000000000000000008' '$f3' | sed -n '2p' | grep -q '^        with:$' && grep -A2 '0000000000000000000000000000000000000008' '$f3' | sed -n '3p' | grep -q '^          toolchain: stable$'"
check "exactly three steps were fixed and the run exits 0" \
  "[ \$rc_tc3 -eq 0 ] && [ \$(grep -c 'toolchain: stable' '$f3') -eq 3 ] && grep -q 'toolchain: nightly' '$f3' && grep -q 'cargo test' '$f3'"
cp "$f3" "$fx/tc3-after1.yml"
bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc3" >/dev/null 2>&1
check "second run over these cases is a no-op" "cmp -s '$fx/tc3-after1.yml' '$f3'"
if command -v mawk >/dev/null; then
    mkdir -p "$fx/mawk-bin" "$fx/tc3m/.github/workflows"; ln -sf "$(command -v mawk)" "$fx/mawk-bin/awk"
    cp "$fx/tc3-before.yml" "$fx/tc3m/.github/workflows/rust.yml"
    PATH="$fx/mawk-bin:$PATH" bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc3m" >/dev/null 2>&1 || true
    check "under mawk the same cases produce the same file" "cmp -s '$fx/tc3-after1.yml' '$fx/tc3m/.github/workflows/rust.yml'"
fi
# A `toolchain: stable` line inside ANOTHER input's block scalar is text, not
# an input: the step still gets its own toolchain, as the first child of with:.
mkdir -p "$fx/tc5/.github/workflows"
printf 'jobs:\n  a:\n    steps:\n      - uses: dtolnay/rust-toolchain@000000000000000000000000000000000000000b\n        with:\n          components: |\n            toolchain: stable\n' > "$fx/tc5/.github/workflows/x.yml"
f5="$fx/tc5/.github/workflows/x.yml"
bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc5" >/dev/null 2>&1
check "toolchain: inside another input's block scalar does not count: a real input is added first" \
  "grep -A2 '000000000000000000000000000000000000000b' '$f5' | sed -n '3p' | grep -q '^          toolchain: stable$' && grep -q '^            toolchain: stable$' '$f5' && [ \$(grep -c 'toolchain: stable' '$f5') -eq 2 ]"
# A flow `with: {` spread over several lines is read to its closing brace:
# a toolchain inside it counts; without one it gets the input as first entry.
# A list item whose parent is not `steps:` — a fixture kept in a top-level
# `env:` block scalar, even one that spells out `steps:` inside — is text.
mkdir -p "$fx/tc6/.github/workflows"
cat > "$fx/tc6/.github/workflows/x.yml" <<'YML'
env:
  FIXTURE: |
    jobs:
      a:
        steps:
          - uses: dtolnay/rust-toolchain@000000000000000000000000000000000000000c
jobs:
  a:
    steps:
      - uses: dtolnay/rust-toolchain@000000000000000000000000000000000000000d
        with: {
          toolchain: nightly,
        }
      - uses: dtolnay/rust-toolchain@000000000000000000000000000000000000000e
        with: {
          components: clippy,
        }
      - uses: dtolnay/rust-toolchain@000000000000000000000000000000000000000f
YML
f6="$fx/tc6/.github/workflows/x.yml"
set +e
bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc6" >/dev/null 2>&1; rc_tc6=$?
set -e
check "a fixture in a top-level env: block scalar is text: its dtolnay line is untouched, nothing inserted" \
  "grep -A1 '000000000000000000000000000000000000000c' '$f6' | sed -n '2p' | grep -q '^jobs:$' && grep -q '^          - uses: dtolnay/rust-toolchain@000000000000000000000000000000000000000c$' '$f6'"
check "a multi-line flow with: naming a toolchain counts: untouched" \
  "grep -A1 '000000000000000000000000000000000000000d' '$f6' | sed -n '2p' | grep -q '^        with: {$' && [ \$(grep -c 'toolchain: nightly' '$f6') -eq 1 ]"
check "a multi-line flow with: without a toolchain gets it as its first entry" \
  "grep -A1 '000000000000000000000000000000000000000e' '$f6' | sed -n '2p' | grep -q '^        with: { toolchain: stable,$' && grep -A2 '000000000000000000000000000000000000000e' '$f6' | sed -n '3p' | grep -q '^          components: clippy,$'"
check "the real steps were fixed, the fixture was not, exit 0" \
  "[ \$rc_tc6 -eq 0 ] && [ \$(grep -c 'toolchain: stable' '$f6') -eq 2 ] && grep -A2 '000000000000000000000000000000000000000f' '$f6' | sed -n '2p' | grep -q '^        with:$'"
cp "$f6" "$fx/tc6-after1.yml"
bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc6" >/dev/null 2>&1
check "second run over these cases is a no-op" "cmp -s '$fx/tc6-after1.yml' '$f6'"
# An expression inside a multi-line flow with: (`${{ matrix.component }}`)
# opens and closes its own braces: the mapping ends when the depth returns to
# zero, so a toolchain written after the expression still counts.
mkdir -p "$fx/tc7/.github/workflows"
cat > "$fx/tc7/.github/workflows/x.yml" <<'YML'
jobs:
  a:
    steps:
      - uses: dtolnay/rust-toolchain@0000000000000000000000000000000000000010
        with: {
          components: ${{ matrix.component }},
          toolchain: nightly,
        }
      - uses: dtolnay/rust-toolchain@0000000000000000000000000000000000000011
        with: {
          components: ${{ matrix.component }},
        }
YML
f7="$fx/tc7/.github/workflows/x.yml"
set +e
bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc7" >/dev/null 2>&1; rc_tc7=$?
set -e
check "a toolchain after an expression's braces in a multi-line flow with: counts: untouched" \
  "grep -A1 '0000000000000000000000000000000000000010' '$f7' | sed -n '2p' | grep -q '^        with: {$' && [ \$(grep -c 'toolchain: nightly' '$f7') -eq 1 ]"
check "the same shape without a toolchain gets it as first entry, the expression kept" \
  "[ \$rc_tc7 -eq 0 ] && grep -A1 '0000000000000000000000000000000000000011' '$f7' | sed -n '2p' | grep -q '^        with: { toolchain: stable,$' && [ \$(grep -c 'components: \${{ matrix.component }},' '$f7') -eq 2 ] && [ \$(grep -c 'toolchain: stable' '$f7') -eq 1 ]"
# A `with:` whose value the parser does not read (an anchor, an alias) and a
# one-line `steps: [ { uses: ... } ]` are refused out loud, file untouched.
mkdir -p "$fx/tc8/.github/workflows" "$fx/tc9/.github/workflows"
cat > "$fx/tc8/.github/workflows/x.yml" <<'YML'
jobs:
  a:
    steps:
      - uses: dtolnay/rust-toolchain@0000000000000000000000000000000000000012
        with: &rust-inputs
          components: clippy
      - uses: dtolnay/rust-toolchain@0000000000000000000000000000000000000013
        with: *rust-inputs
YML
printf 'jobs:\n  a:\n    steps: [ { uses: dtolnay/rust-toolchain@0000000000000000000000000000000000000014 } ]\n' > "$fx/tc9/.github/workflows/x.yml"
cp "$fx/tc8/.github/workflows/x.yml" "$fx/tc8-before.yml"; cp "$fx/tc9/.github/workflows/x.yml" "$fx/tc9-before.yml"
set +e
out8=$(bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc8" 2>&1); rc_tc8=$?
out9=$(bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc9" 2>&1); rc_tc9=$?
set -e
check "with: &anchor and with: *alias are UNSUPPORTED (no second with:), file untouched, exit 2" \
  "[ \$rc_tc8 -eq 2 ] && grep -q 'UNSUPPORTED.*line(s): 5 8' <<<\"\$out8\" && cmp -s '$fx/tc8-before.yml' '$fx/tc8/.github/workflows/x.yml'"
check "a one-line steps: [ { uses: ... } ] is UNSUPPORTED, file untouched, exit 2, never OK" \
  "[ \$rc_tc9 -eq 2 ] && grep -q 'UNSUPPORTED.*line(s): 3' <<<\"\$out9\" && ! grep -q '^OK' <<<\"\$out9\" && cmp -s '$fx/tc9-before.yml' '$fx/tc9/.github/workflows/x.yml'"
# A flow-style step: refused out loud, file untouched, exit 2 — never "OK".
mkdir -p "$fx/tc4/.github/workflows"
printf 'jobs:\n  a:\n    steps:\n      - { name: toolchain, uses: dtolnay/rust-toolchain@0000000000000000000000000000000000000009 }\n      - uses: dtolnay/rust-toolchain@000000000000000000000000000000000000000a\n' > "$fx/tc4/.github/workflows/x.yml"
cp "$fx/tc4/.github/workflows/x.yml" "$fx/tc4-before.yml"
set +e
out4=$(bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc4" 2>&1); rc_tc4=$?
set -e
check "a flow-style step is UNSUPPORTED: named with its line, file untouched, exit 2, never OK" \
  "[ \$rc_tc4 -eq 2 ] && grep -q 'UNSUPPORTED.*line(s): 4' <<<\"\$out4\" && ! grep -q '^OK' <<<\"\$out4\" && cmp -s '$fx/tc4-before.yml' '$fx/tc4/.github/workflows/x.yml'"

# Custom toolchains must work with empty mappings, quoted keys, reordered
# inputs, .yaml files and paths containing spaces. Existing expressions stay.
tc_custom="$fx/tc custom/.github/workflows"
mkdir -p "$tc_custom"
cat > "$tc_custom/rust.yaml" <<'YML'
jobs:
  check:
    steps:
      - 'uses': 'dtolnay/rust-toolchain@aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
        'with': { }
      - uses: dtolnay/rust-toolchain@bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
        with: # intentionally empty
      - uses: dtolnay/rust-toolchain@cccccccccccccccccccccccccccccccccccccccc
        with:
          toolchain: ${{ matrix.rust }}
      - with: {}
        uses: dtolnay/rust-toolchain@dddddddddddddddddddddddddddddddddddddddd
YML
cat > "$fx/tc-custom-expected.yaml" <<'YML'
jobs:
  check:
    steps:
      - 'uses': 'dtolnay/rust-toolchain@aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
        'with': { toolchain: 1.85.0 }
      - uses: dtolnay/rust-toolchain@bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
        with: # intentionally empty
          toolchain: 1.85.0
      - uses: dtolnay/rust-toolchain@cccccccccccccccccccccccccccccccccccccccc
        with:
          toolchain: ${{ matrix.rust }}
      - with: { toolchain: 1.85.0 }
        uses: dtolnay/rust-toolchain@dddddddddddddddddddddddddddddddddddddddd
YML
run_fix env TOOLCHAIN=1.85.0 bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc custom"
check "custom toolchain fills empty inputs and preserves expressions, quotes and comments exactly" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/tc-custom-expected.yaml' '$tc_custom/rust.yaml'"
run_fix env TOOLCHAIN=beta bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$tc_custom/rust.yaml"
check "direct-file mode preserves existing inputs even when the requested toolchain changes" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/tc-custom-expected.yaml' '$tc_custom/rust.yaml'"

# An unsupported file must not prevent a supported sibling from being fixed,
# and a sibling's success must not hide the overall error. Nested files and
# files without a YAML extension are outside repository-mode discovery.
tc_mixed="$fx/tc-mixed/.github/workflows"
mkdir -p "$tc_mixed/nested"
cp "$fx/tc4-before.yml" "$tc_mixed/a-unsupported.yml"
printf 'jobs:\n  check:\n    steps:\n      - uses: dtolnay/rust-toolchain@aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n' > "$tc_mixed/z-supported.yaml"
cp "$tc_mixed/z-supported.yaml" "$tc_mixed/nested/ignored.yml"
cp "$tc_mixed/z-supported.yaml" "$tc_mixed/ignored.txt"
cp "$tc_mixed/z-supported.yaml" "$fx/tc-mixed-before.yaml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc-mixed"
check "repository mode fixes a supported .yaml sibling while returning UNSUPPORTED for another file" \
  "[ \$rc -eq 2 ] && grep -q 'UNSUPPORTED.*a-unsupported.yml' <<<\"\$out\" && grep -q '^          toolchain: stable$' '$tc_mixed/z-supported.yaml' && cmp -s '$fx/tc4-before.yml' '$tc_mixed/a-unsupported.yml'"
check "repository mode leaves nested workflows and non-YAML files byte-identical" \
  "cmp -s '$fx/tc-mixed-before.yaml' '$tc_mixed/nested/ignored.yml' && cmp -s '$fx/tc-mixed-before.yaml' '$tc_mixed/ignored.txt'"

# tc10: the key present with no value (`toolchain:`, `null`, `~`) is filled
# where it stands — block or flow, inline or on a continuation line — never
# written a second time; a trailing comment survives.
mkdir -p "$fx/tc10/.github/workflows"
cat > "$fx/tc10/.github/workflows/x.yml" <<'YML'
jobs:
  check:
    steps:
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with:
          toolchain:
          components: clippy
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with:
          components: clippy
          toolchain: null # chosen at runtime
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: { toolchain: , components: clippy }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: { components: clippy, toolchain: ~ }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: {
          toolchain: null,
          components: clippy,
        }
YML
cat > "$fx/tc10-expected.yml" <<'YML'
jobs:
  check:
    steps:
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with:
          toolchain: stable
          components: clippy
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with:
          components: clippy
          toolchain: stable # chosen at runtime
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: { toolchain: stable, components: clippy }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: { components: clippy, toolchain: stable }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: {
          toolchain: stable,
          components: clippy,
        }
YML
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc10"
check "an empty toolchain: input is filled in place in every shape, never duplicated" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/tc10-expected.yml' '$fx/tc10/.github/workflows/x.yml' && [ \$(grep -c 'toolchain:' '$fx/tc10/.github/workflows/x.yml') -eq 5 ]"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc10"
check "filled inputs are inputs: the second run is byte-identical and OK" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/tc10-expected.yml' '$fx/tc10/.github/workflows/x.yml' && grep -q '^OK' <<<\"\$out\""

# tc11: an empty `toolchain:` with a mapping nested under it is not an empty
# value; the fixer refuses rather than write a value over a mapping.
mkdir -p "$fx/tc11/.github/workflows"
printf 'jobs:\n  check:\n    steps:\n      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772\n        with:\n          toolchain:\n            channel: stable\n' > "$fx/tc11/.github/workflows/x.yml"
cp "$fx/tc11/.github/workflows/x.yml" "$fx/tc11-before.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc11"
check "a toolchain: key with a nested mapping is UNSUPPORTED: file untouched, exit 2" \
  "[ \$rc -eq 2 ] && grep -q 'UNSUPPORTED.*line(s): 6' <<<\"\$out\" && cmp -s '$fx/tc11-before.yml' '$fx/tc11/.github/workflows/x.yml'"

# tc12: braces inside quoted scalars and comments are text. A `"}"` value or
# a `# }` comment inside a multi-line flow `with:` must not end the mapping
# early (and hide a later toolchain, or misplace the insertion).
mkdir -p "$fx/tc12/.github/workflows"
cat > "$fx/tc12/.github/workflows/x.yml" <<'YML'
jobs:
  check:
    steps:
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: {
          components: "}", # a } in a comment
          toolchain: nightly,
        }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: {
          components: '{',
        }
YML
cat > "$fx/tc12-expected.yml" <<'YML'
jobs:
  check:
    steps:
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: {
          components: "}", # a } in a comment
          toolchain: nightly,
        }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: { toolchain: stable,
          components: '{',
        }
YML
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc12"
check "quoted braces and comment braces do not close a flow with: — the named toolchain is seen, the missing one added once" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/tc12-expected.yml' '$fx/tc12/.github/workflows/x.yml'"

# tc13: a ref that merely begins with 40 hex characters (`@<sha>-patched`) is
# a tag, carries its own channel, and is not ours.
mkdir -p "$fx/tc13/.github/workflows"
printf 'jobs:\n  check:\n    steps:\n      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772-patched\n      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772 # pinned\n' > "$fx/tc13/.github/workflows/x.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc13"
check "a tag beginning with a SHA is a tag: untouched; the real SHA pin beside it is fixed" \
  "[ \$rc -eq 0 ] && sed -n '4p' '$fx/tc13/.github/workflows/x.yml' | grep -q 'patched\$' && sed -n '5p' '$fx/tc13/.github/workflows/x.yml' | grep -q '# pinned\$' && [ \$(grep -c 'toolchain: stable' '$fx/tc13/.github/workflows/x.yml') -eq 1 ] && sed -n '6,7p' '$fx/tc13/.github/workflows/x.yml' | grep -q '^          toolchain: stable$'"

# tc14: inputs are read at the `with:` mapping's own level. `toolchain:`
# text inside a quoted value or a nested mapping is not an input; a quoted
# toolchain value still is.
mkdir -p "$fx/tc14/.github/workflows"
cat > "$fx/tc14/.github/workflows/x.yml" <<'YML'
jobs:
  check:
    steps:
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: { components: "clippy, toolchain: nightly" }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: { env: { toolchain: nightly } }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: {
          env: {
            toolchain: nightly,
          },
        }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: { components: "clippy", toolchain: "1.85.0" }
YML
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc14"
check "toolchain: inside a quoted value or a nested mapping is not an input: the three steps are fixed, the quoted real one is left" \
  "[ \$rc -eq 0 ] && [ \$(grep -c 'toolchain: stable' '$fx/tc14/.github/workflows/x.yml') -eq 3 ] && sed -n '5p' '$fx/tc14/.github/workflows/x.yml' | grep -q '^        with: { toolchain: stable, components: \"clippy, toolchain: nightly\" }$' && sed -n '7p' '$fx/tc14/.github/workflows/x.yml' | grep -q '^        with: { toolchain: stable, env: { toolchain: nightly } }$' && sed -n '9p' '$fx/tc14/.github/workflows/x.yml' | grep -q '^        with: { toolchain: stable,$' && sed -n '15p' '$fx/tc14/.github/workflows/x.yml' | grep -q 'toolchain: \"1.85.0\" }$'"

# Ref length is part of the contract: abbreviated and overlong hex refs are
# not full SHA pins, and another action at the same SHA is not dtolnay.
tc_boundary="$fx/tc-boundary/.github/workflows"
mkdir -p "$tc_boundary"
cat > "$tc_boundary/ci.yml" <<'YML'
jobs:
  check:
    steps:
      - uses: dtolnay/rust-toolchain@aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      - uses: dtolnay/rust-toolchain@aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      - uses: other/rust-toolchain@aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      - uses: dtolnay/rust-toolchain@aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
YML
cat > "$fx/tc-boundary-expected.yml" <<'YML'
jobs:
  check:
    steps:
      - uses: dtolnay/rust-toolchain@aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      - uses: dtolnay/rust-toolchain@aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      - uses: other/rust-toolchain@aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      - uses: dtolnay/rust-toolchain@aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
        with:
          toolchain: stable
YML
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc-boundary"
check "only the exact 40-hex dtolnay pin is repaired, including the last step at EOF" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/tc-boundary-expected.yml' '$tc_boundary/ci.yml'"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$tc_boundary/ci.yml"
check "SHA boundary controls remain unchanged on a second direct-file run" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/tc-boundary-expected.yml' '$tc_boundary/ci.yml' && ! grep -q '^FIXED' <<<\"\$out\""

# Refusing an unsupported step is atomic for its file, even when a preceding
# supported step could be repaired. No partial output may replace the file.
tc_atomic="$fx/tc-atomic/.github/workflows"
mkdir -p "$tc_atomic"
cat > "$tc_atomic/ci.yml" <<'YML'
jobs:
  check:
    steps:
      - uses: dtolnay/rust-toolchain@aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      - { uses: dtolnay/rust-toolchain@bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb }
YML
cp "$tc_atomic/ci.yml" "$fx/tc-atomic-before.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc-atomic"
check "an unsupported step prevents earlier supported repairs in the same file" \
  "[ \$rc -eq 2 ] && grep -q 'UNSUPPORTED.*line(s): 5' <<<\"\$out\" && ! grep -q '^FIXED' <<<\"\$out\" && cmp -s '$fx/tc-atomic-before.yml' '$tc_atomic/ci.yml'"

# Indentless step sequences and a following job use different key columns.
# A folded run scalar is data; a quoted null input in the next job is filled.
tc_indent="$fx/tc-indent/.github/workflows"
mkdir -p "$tc_indent"
cat > "$tc_indent/ci.yaml" <<'YML'
jobs:
  first:
    steps:
    - with:
        components: clippy
      uses: dtolnay/rust-toolchain@aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    - run: >-
        echo dtolnay/rust-toolchain@bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
  second:
    steps:
      - uses: dtolnay/rust-toolchain@cccccccccccccccccccccccccccccccccccccccc
        with:
          'toolchain': null # keep this comment
YML
cat > "$fx/tc-indent-expected.yaml" <<'YML'
jobs:
  first:
    steps:
    - with:
        toolchain: stable
        components: clippy
      uses: dtolnay/rust-toolchain@aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    - run: >-
        echo dtolnay/rust-toolchain@bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
  second:
    steps:
      - uses: dtolnay/rust-toolchain@cccccccccccccccccccccccccccccccccccccccc
        with:
          'toolchain': stable # keep this comment
YML
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc-indent"
check "indentless steps and a following job get inputs at their own key columns, preserving scalar data" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/tc-indent-expected.yaml' '$tc_indent/ci.yaml'"

# ── 2. fix-stale-reusable-pin.sh ─────────────────────────────────────────────
STALE_A=d135b05bfc647d0c0fbfedc7e80f37ea50f49236   # Window A (table row for rust-ci-reusable only)
STALE_B=8f2ee50841e216cd8c192eeb68953118190f105c   # Window B (table row for every standards reusable)
GOOD=75bd5994470661a099df562d6d5ad2fe744dbd66
mkdir -p "$fx/sp/.github/workflows"
printf 'jobs:\n  rust-ci:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$STALE_A" > "$fx/sp/.github/workflows/rust-ci.yml"
printf 'jobs:\n  governance:\n    uses: hyperpolymath/standards/.github/workflows/governance-reusable.yml@%s\n' "$STALE_B" > "$fx/sp/.github/workflows/governance.yml"
printf 'jobs:\n  mirror:\n    uses: hyperpolymath/standards/.github/workflows/mirror-reusable.yml@%s\n' "$STALE_A" > "$fx/sp/.github/workflows/mirror.yml"
printf 'jobs:\n  audit:\n    uses: hyperpolymath/cicd-suite/.github/workflows/main-estate-audit.yml@%s\n' "$STALE_B" > "$fx/sp/.github/workflows/audit.yml"
cat > "$fx/sp/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/governance.yml':
        - 'hyperpolymath/standards@$STALE_B'
dependencies:
    'hyperpolymath/standards@$STALE_B':
        ref: '$STALE_B'
        commit: 'sha1-$STALE_B'
        owner_id: 6759885
        repo_id: 1116521501
LOCK
set +e
out=$(bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp" 2>&1); rc_sp=$?
set -e
check "Window A pin of rust-ci-reusable is moved to the replacement" "grep -q \"rust-ci-reusable.yml@$GOOD\" '$fx/sp/.github/workflows/rust-ci.yml'"
check "Window B pin of any standards reusable is moved (wildcard row)" "grep -q \"governance-reusable.yml@$GOOD\" '$fx/sp/.github/workflows/governance.yml'"
check "a Window-A SHA on a reusable the window does not cover is left alone" "grep -q \"mirror-reusable.yml@$STALE_A\" '$fx/sp/.github/workflows/mirror.yml'"
check "a cicd-suite pin that happens to share the SHA is not a standards pin and is untouched" "grep -q \"main-estate-audit.yml@$STALE_B\" '$fx/sp/.github/workflows/audit.yml'"
check "the lock's workflows entry and record follow the pin" "[ \$(grep -c \"$GOOD\" '$fx/sp/.github/workflows/actions.lock') -eq 4 ] && ! grep -q \"$STALE_B\" '$fx/sp/.github/workflows/actions.lock'"
check "touching the lock is said out loud and exits 3, never a clean success" "[ \$rc_sp -eq 3 ] && grep -q 'nested uses: list was NOT recomputed' <<<\"\$out\""
tar -C "$fx/sp" -cf "$fx/sp1.tar" .github
bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp" >/dev/null 2>&1; rc_sp2=$?
tar -C "$fx/sp" -cf "$fx/sp2.tar" .github
check "second run is a no-op and exits 0 (nothing left to say about the lock)" "[ \$rc_sp2 -eq 0 ] && cmp -s '$fx/sp1.tar' '$fx/sp2.tar'"
# Planted control: an empty table rewrites nothing.
printf '# empty\n' > "$fx/empty.tsv"
mkdir -p "$fx/sp0/.github/workflows"; printf 'jobs:\n  rust-ci:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$STALE_A" > "$fx/sp0/.github/workflows/rust-ci.yml"
PINS_TABLE="$fx/empty.tsv" bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp0" >/dev/null 2>&1; rc_sp0=$?
check "control: with no table rows nothing is rewritten and the run exits 0" "[ \$rc_sp0 -eq 0 ] && grep -q \"@$STALE_A\" '$fx/sp0/.github/workflows/rust-ci.yml'"
check "the shipped table names both windows' edges and never lists the replacement as stale" \
  "grep -q \"rust-ci-reusable.yml.$STALE_A\" '$fleet/scripts/lib/stale-reusable-pins.tsv' && grep -q \"workflows/\\*.$STALE_B\" '$fleet/scripts/lib/stale-reusable-pins.tsv' && ! awk -F'\\t' -v g=\"$GOOD\" '\$2 == g' '$fleet/scripts/lib/stale-reusable-pins.tsv' | grep -q ."
# Two callers at two different dead SHAs that move to the SAME replacement must
# leave ONE record for it in the lock, with the union of both nested lists.
mkdir -p "$fx/sp2/.github/workflows"
printf 'jobs:\n  governance:\n    uses: hyperpolymath/standards/.github/workflows/governance-reusable.yml@%s\n' "$STALE_B" > "$fx/sp2/.github/workflows/governance.yml"
STALE_C=da2c748aad55c1a1dcba00b60fe4a35017bc6540
printf 'jobs:\n  scorecard:\n    uses: hyperpolymath/standards/.github/workflows/scorecard-reusable.yml@%s\n' "$STALE_C" > "$fx/sp2/.github/workflows/scorecard.yml"
cat > "$fx/sp2/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/governance.yml':
        - 'hyperpolymath/standards@$STALE_B'
    '.github/workflows/scorecard.yml':
        - 'hyperpolymath/standards@$STALE_C'
dependencies:
    'hyperpolymath/standards@$STALE_B':
        ref: '$STALE_B'
        commit: 'sha1-$STALE_B'
        owner_id: 6759885
        repo_id: 1116521501
        uses:
            - 'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1'
            - 'erlef/setup-beam@54075bcc5e249e4758d363f27d099f55d843f124'
    'hyperpolymath/standards@$STALE_C':
        ref: '$STALE_C'
        commit: 'sha1-$STALE_C'
        owner_id: 6759885
        repo_id: 1116521501
        uses:
            - 'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1'
            - 'ossf/scorecard-action@2d1146689b8cda280b9bc96326124645441f03bc'
LOCK
bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp2" >/dev/null 2>&1 || true
check "two dead SHAs moving to one replacement leave ONE record for it" "[ \$(grep -c \"^    'hyperpolymath/standards@$GOOD':\" '$fx/sp2/.github/workflows/actions.lock') -eq 1 ]"
check "the merged record carries the union of both nested lists, once each" \
  "[ \$(grep -c \"            - 'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1'\" '$fx/sp2/.github/workflows/actions.lock') -eq 1 ] && grep -q 'erlef/setup-beam' '$fx/sp2/.github/workflows/actions.lock' && grep -q 'ossf/scorecard-action' '$fx/sp2/.github/workflows/actions.lock'"
check "both workflows: entries point at the replacement" "[ \$(grep -c \"        - 'hyperpolymath/standards@$GOOD'\" '$fx/sp2/.github/workflows/actions.lock') -eq 2 ]"

check "the merged record carries its header once (one ref: line)" \
  "[ \$(grep -c \"        ref: '$GOOD'\" '$fx/sp2/.github/workflows/actions.lock') -eq 1 ] && [ \$(grep -c \"        commit: 'sha1-$GOOD'\" '$fx/sp2/.github/workflows/actions.lock') -eq 1 ]"
# Partial migration (review): two callers share a Window-A SHA but the table
# row covers only rust-ci-reusable. The lock must follow the rewritten caller
# only: mirror's edge and the stale record stay, the replacement gets its own.
mkdir -p "$fx/sp3/.github/workflows"
printf 'jobs:\n  rust-ci:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$STALE_A" > "$fx/sp3/.github/workflows/rust-ci.yml"
printf 'jobs:\n  mirror:\n    uses: hyperpolymath/standards/.github/workflows/mirror-reusable.yml@%s\n' "$STALE_A" > "$fx/sp3/.github/workflows/mirror.yml"
cat > "$fx/sp3/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/mirror.yml':
        - 'hyperpolymath/standards@$STALE_A'
    '.github/workflows/rust-ci.yml':
        - 'hyperpolymath/standards@$STALE_A'
dependencies:
    'hyperpolymath/standards@$STALE_A':
        ref: '$STALE_A'
        commit: 'sha1-$STALE_A'
        owner_id: 6759885
        repo_id: 1116521501
        uses:
            - 'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1'
LOCK
set +e
out3=$(bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp3" 2>&1); rc_sp3=$?
set -e
l3="$fx/sp3/.github/workflows/actions.lock"
check "partial migration: the rewritten caller's lock edge moves" \
  "grep -A1 \"'.github/workflows/rust-ci.yml':\" '$l3' | grep -q \"hyperpolymath/standards@$GOOD\""
check "partial migration: the untouched caller's lock edge stays at the stale SHA" \
  "grep -A1 \"'.github/workflows/mirror.yml':\" '$l3' | grep -q \"hyperpolymath/standards@$STALE_A\" && grep -q \"mirror-reusable.yml@$STALE_A\" '$fx/sp3/.github/workflows/mirror.yml'"
check "partial migration: both dependency records exist, each with its own ref and commit" \
  "[ \$(grep -c \"^    'hyperpolymath/standards@\" '$l3') -eq 2 ] && grep -q \"ref: '$GOOD'\" '$l3' && grep -q \"ref: '$STALE_A'\" '$l3' && grep -q \"commit: 'sha1-$GOOD'\" '$l3'"
check "partial migration is said out loud and exits 3" "[ \$rc_sp3 -eq 3 ] && grep -q 'keeps the record for' <<<\"\$out3\""
tar -C "$fx/sp3" -cf "$fx/sp3a.tar" .github
bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp3" >/dev/null 2>&1; rc_sp3b=$?
tar -C "$fx/sp3" -cf "$fx/sp3b.tar" .github
check "partial migration: second run is a no-op and exits 0" "[ \$rc_sp3b -eq 0 ] && cmp -s '$fx/sp3a.tar' '$fx/sp3b.tar'"
# A quoted key and value are still a pin.
mkdir -p "$fx/sp4/.github/workflows"
printf 'jobs:\n  governance:\n    "uses": "hyperpolymath/standards/.github/workflows/governance-reusable.yml@%s"\n' "$STALE_B" > "$fx/sp4/.github/workflows/governance.yml"
bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp4" >/dev/null 2>&1; rc_sp4=$?
check "a quoted \"uses\" key with a quoted value is rewritten" "[ \$rc_sp4 -eq 0 ] && grep -q \"\\\"hyperpolymath/standards/.github/workflows/governance-reusable.yml@$GOOD\\\"\" '$fx/sp4/.github/workflows/governance.yml'"
# Review cases (Codex, gitbot-fleet#615): a commented-out caller is not a
# caller; a workflow calling two reusables of one repository at the same dead
# SHA, only one covered by the table, keeps the stale lock edge beside the new.
mkdir -p "$fx/sp5/.github/workflows"
printf 'jobs:\n  mirror:\n    uses: hyperpolymath/standards/.github/workflows/mirror-reusable.yml@%s\n  # rust-ci:\n  #   uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$STALE_A" "$STALE_A" > "$fx/sp5/.github/workflows/ci.yml"
cat > "$fx/sp5/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'hyperpolymath/standards@$STALE_A'
dependencies:
    'hyperpolymath/standards@$STALE_A':
        ref: '$STALE_A'
        commit: 'sha1-$STALE_A'
        owner_id: 6759885
        repo_id: 1116521501
LOCK
tar -C "$fx/sp5" -cf "$fx/sp5a.tar" .github
set +e
bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp5" >/dev/null 2>&1; rc_sp5=$?
set -e
tar -C "$fx/sp5" -cf "$fx/sp5b.tar" .github
check "a commented-out caller of a covered reusable moves nothing: workflow and lock byte-identical, exit 0" \
  "[ \$rc_sp5 -eq 0 ] && cmp -s '$fx/sp5a.tar' '$fx/sp5b.tar'"
mkdir -p "$fx/sp6/.github/workflows"
printf 'jobs:\n  rust-ci:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n  mirror:\n    uses: hyperpolymath/standards/.github/workflows/mirror-reusable.yml@%s\n' "$STALE_A" "$STALE_A" > "$fx/sp6/.github/workflows/ci.yml"
sed 's#sp5#sp6#' "$fx/sp5/.github/workflows/actions.lock" > "$fx/sp6/.github/workflows/actions.lock"
set +e
bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp6" >/dev/null 2>&1; rc_sp6=$?
set -e
l6="$fx/sp6/.github/workflows/actions.lock"
check "two reusables of one repository at one dead SHA, one covered: the covered pin moves, the other stays" \
  "grep -q \"rust-ci-reusable.yml@$GOOD\" '$fx/sp6/.github/workflows/ci.yml' && grep -q \"mirror-reusable.yml@$STALE_A\" '$fx/sp6/.github/workflows/ci.yml'"
check "the workflow's lock entry then carries BOTH edges, the stale one kept beside the new" \
  "grep -A2 \"'.github/workflows/ci.yml':\" '$l6' | grep -q \"hyperpolymath/standards@$STALE_A\" && grep -A2 \"'.github/workflows/ci.yml':\" '$l6' | grep -q \"hyperpolymath/standards@$GOOD\""
check "both dependency records exist, each with its ref, and the run exits 3" \
  "[ \$rc_sp6 -eq 3 ] && grep -q \"ref: '$GOOD'\" '$l6' && grep -q \"ref: '$STALE_A'\" '$l6'"
tar -C "$fx/sp6" -cf "$fx/sp6a.tar" .github
bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp6" >/dev/null 2>&1; rc_sp6b=$?
tar -C "$fx/sp6" -cf "$fx/sp6b.tar" .github
check "kept-edge case: second run is a no-op and exits 0" "[ \$rc_sp6b -eq 0 ] && cmp -s '$fx/sp6a.tar' '$fx/sp6b.tar'"
# A flow-style job mapping carrying a known-dead pin: the parser cannot place
# it as a `uses` key, so the file is refused out loud — never "OK".
mkdir -p "$fx/sp7/.github/workflows"
printf 'jobs:\n  mirror:\n    uses: hyperpolymath/standards/.github/workflows/mirror-reusable.yml@%s\n  rust: { uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s }\n' "$STALE_A" "$STALE_A" > "$fx/sp7/.github/workflows/ci.yml"
cp "$fx/sp7/.github/workflows/ci.yml" "$fx/sp7-before.yml"
set +e
out7=$(bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp7" 2>&1); rc_sp7=$?
set -e
check "a flow-style job with a known-dead pin is UNSUPPORTED: named with its line, file untouched, exit 2, never OK" \
  "[ \$rc_sp7 -eq 2 ] && grep -q 'UNSUPPORTED  .github/workflows/ci.yml:4:' <<<\"\$out7\" && ! grep -q '^OK' <<<\"\$out7\" && cmp -s '$fx/sp7-before.yml' '$fx/sp7/.github/workflows/ci.yml'"
# A nested `uses:` edge of ANOTHER record still pointing at the stale SHA (a
# reusable that itself calls the stale revision) keeps the stale record alive.
mkdir -p "$fx/sp8/.github/workflows"
printf 'jobs:\n  governance:\n    uses: hyperpolymath/standards/.github/workflows/governance-reusable.yml@%s\n' "$STALE_B" > "$fx/sp8/.github/workflows/governance.yml"
cat > "$fx/sp8/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/governance.yml':
        - 'hyperpolymath/standards@$STALE_B'
dependencies:
    'hyperpolymath/cicd-suite@1111111111111111111111111111111111111111':
        ref: '1111111111111111111111111111111111111111'
        commit: 'sha1-1111111111111111111111111111111111111111'
        owner_id: 6759885
        repo_id: 1
        uses:
            - 'hyperpolymath/standards@$STALE_B'
    'hyperpolymath/standards@$STALE_B':
        ref: '$STALE_B'
        commit: 'sha1-$STALE_B'
        owner_id: 6759885
        repo_id: 1116521501
LOCK
set +e
bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp8" >/dev/null 2>&1; rc_sp8=$?
set -e
l8="$fx/sp8/.github/workflows/actions.lock"
check "transitive edge: the caller's own edge moves, the other record's nested edge stays at the stale SHA" \
  "grep -A1 \"'.github/workflows/governance.yml':\" '$l8' | grep -q \"standards@$GOOD\" && grep -A6 \"'hyperpolymath/cicd-suite@1111\" '$l8' | grep -q \"            - 'hyperpolymath/standards@$STALE_B'\""
check "transitive edge: the stale record is kept beside the replacement's, exit 3" \
  "[ \$rc_sp8 -eq 3 ] && grep -q \"^    'hyperpolymath/standards@$STALE_B':\" '$l8' && grep -q \"^    'hyperpolymath/standards@$GOOD':\" '$l8'"
# Only a `uses:` key of a job is a call: the same text under `env:` or inside
# a `run: |` fixture is data, left byte-identical, and never moves the lock.
mkdir -p "$fx/sp9/.github/workflows"
cat > "$fx/sp9/.github/workflows/ci.yml" <<YML
jobs:
  fixture:
    env:
      uses: hyperpolymath/standards/.github/workflows/governance-reusable.yml@$STALE_B
    steps:
      - run: |
          cat > caller.yml <<'EOF'
          uses: hyperpolymath/standards/.github/workflows/governance-reusable.yml@$STALE_B
          EOF
  governance:
    uses: hyperpolymath/standards/.github/workflows/governance-reusable.yml@$STALE_B
YML
cat > "$fx/sp9/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'hyperpolymath/standards@$STALE_B'
dependencies:
    'hyperpolymath/standards@$STALE_B':
        ref: '$STALE_B'
        commit: 'sha1-$STALE_B'
        owner_id: 6759885
        repo_id: 1116521501
LOCK
set +e
bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp9" >/dev/null 2>&1; rc_sp9=$?
set -e
check "only the job-level call moves; the env: copy and the run: | fixture line are untouched" \
  "sed -n '11p' '$fx/sp9/.github/workflows/ci.yml' | grep -q \"governance-reusable.yml@$GOOD\" && sed -n '4p;8p' '$fx/sp9/.github/workflows/ci.yml' | grep -c \"@$STALE_B\" | grep -qx 2"
check "the lock follows the call: edge and record at the replacement, stale gone, exit 3" \
  "[ \$rc_sp9 -eq 3 ] && grep -q \"hyperpolymath/standards@$GOOD\" '$fx/sp9/.github/workflows/actions.lock' && ! grep -q \"$STALE_B\" '$fx/sp9/.github/workflows/actions.lock'"

# A private table selects the replacement, independent of the shipped estate
# history. A wildcard is scoped to its repository and unknown SHAs stay put.
sp_custom="$fx/sp custom/.github/workflows"
mkdir -p "$sp_custom"
printf '# fixture table\n\nhyperpolymath/example/.github/workflows/*\t%s\t%s\tfixture replacement\n' \
  "$STALE_A" "$GOOD" > "$fx/custom-pins.tsv"
cat > "$sp_custom/ci.yaml" <<YML
jobs:
  first:
    'uses': 'hyperpolymath/example/.github/workflows/check.yaml@$STALE_A' # retain me
  second:
    uses: hyperpolymath/example/.github/workflows/audit.yml@$STALE_A
  unknown:
    uses: hyperpolymath/example/.github/workflows/check.yaml@1111111111111111111111111111111111111111
  other:
    uses: hyperpolymath/example-other/.github/workflows/check.yaml@$STALE_A
YML
cat > "$fx/sp-custom-expected.yaml" <<YML
jobs:
  first:
    'uses': 'hyperpolymath/example/.github/workflows/check.yaml@$GOOD' # retain me
  second:
    uses: hyperpolymath/example/.github/workflows/audit.yml@$GOOD
  unknown:
    uses: hyperpolymath/example/.github/workflows/check.yaml@1111111111111111111111111111111111111111
  other:
    uses: hyperpolymath/example-other/.github/workflows/check.yaml@$STALE_A
YML
run_fix env PINS_TABLE="$fx/custom-pins.tsv" bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp custom"
check "custom wildcard table fixes both matching .yaml callers and preserves unrelated pins and comments" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/sp-custom-expected.yaml' '$sp_custom/ci.yaml' && ! grep -q 'Exit 3' <<<\"\$out\""
run_fix env PINS_TABLE="$fx/custom-pins.tsv" bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp custom"
check "custom-table migration without a lock is byte-identical on the second run" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/sp-custom-expected.yaml' '$sp_custom/ci.yaml' && ! grep -q '^FIXED' <<<\"\$out\""

# Tag refs cannot appear in a full-SHA replacement table. Encountering one
# must be a successful no-op, not an error from the SHA extraction pipeline.
mkdir -p "$fx/sp-tag/.github/workflows"
printf 'jobs:\n  check:\n    uses: hyperpolymath/example/.github/workflows/check.yaml@v1\n' > "$fx/sp-tag/.github/workflows/ci.yaml"
cp "$fx/sp-tag/.github/workflows/ci.yaml" "$fx/sp-tag-before.yaml"
run_fix env PINS_TABLE="$fx/custom-pins.tsv" bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp-tag"
check "a tag-pinned reusable is a successful no-op (observed exit $rc)" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/sp-tag-before.yaml' '$fx/sp-tag/.github/workflows/ci.yaml'"

# Invalid configuration must fail before either the caller or its lock moves.
sp_invalid="$fx/sp-invalid/.github/workflows"
mkdir -p "$sp_invalid"
cp "$fx/sp0/.github/workflows/rust-ci.yml" "$sp_invalid/rust-ci.yml"
cp "$fx/sp5/.github/workflows/actions.lock" "$sp_invalid/actions.lock"
cp "$sp_invalid/actions.lock" "$fx/sp-invalid-before.lock"
run_fix env PINS_TABLE="$fx/missing-table.tsv" bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp-invalid"
check "a missing table exits 2 with a diagnostic and preserves the workflow and lock" \
  "[ \$rc -eq 2 ] && grep -q 'table not readable' <<<\"\$out\" && cmp -s '$fx/sp0/.github/workflows/rust-ci.yml' '$sp_invalid/rust-ci.yml' && cmp -s '$fx/sp-invalid-before.lock' '$sp_invalid/actions.lock'"
for replacement in 123456789012345678901234567890123456789 AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA stable; do
    printf 'hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml\t%s\t%s\tinvalid replacement\n' \
      "$STALE_A" "$replacement" > "$fx/invalid-pins.tsv"
    run_fix env PINS_TABLE="$fx/invalid-pins.tsv" bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp-invalid"
    check "invalid replacement $replacement exits 2 without changing caller or lock" \
      "[ \$rc -eq 2 ] && grep -q 'bad replacement SHA' <<<\"\$out\" && cmp -s '$fx/sp0/.github/workflows/rust-ci.yml' '$sp_invalid/rust-ci.yml' && cmp -s '$fx/sp-invalid-before.lock' '$sp_invalid/actions.lock'"
done
run_fix env PINS_TABLE="$fx/empty.tsv" bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/no-workflows"
check "a repository without workflows is a successful no-op" \
  "[ \$rc -eq 0 ] && grep -q 'OK (no workflows)' <<<\"\$out\" && [ ! -e '$fx/no-workflows' ]"

# Exit 2 takes precedence over the exit 3 warning from a successfully repaired
# sibling's lock. The unsupported workflow and its stale edge must survive.
sp_mixed="$fx/sp-mixed/.github/workflows"
mkdir -p "$sp_mixed"
cp "$fx/sp7-before.yml" "$sp_mixed/ci.yml"
cp "$fx/sp0/.github/workflows/rust-ci.yml" "$sp_mixed/rust-ci.yaml"
cat > "$sp_mixed/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'hyperpolymath/standards@$STALE_A'
    '.github/workflows/rust-ci.yaml':
        - 'hyperpolymath/standards@$STALE_A'
dependencies:
    'hyperpolymath/standards@$STALE_A':
        ref: '$STALE_A'
        commit: 'sha1-$STALE_A'
        owner_id: 6759885
        repo_id: 1116521501
LOCK
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp-mixed"
check "unsupported stale-pin callers take exit-code precedence over a repaired sibling's lock" \
  "[ \$rc -eq 2 ] && grep -q 'UNSUPPORTED' <<<\"\$out\" && grep -q 'nested uses: list was NOT recomputed' <<<\"\$out\" && cmp -s '$fx/sp7-before.yml' '$sp_mixed/ci.yml' && grep -q \"rust-ci-reusable.yml@$GOOD\" '$sp_mixed/rust-ci.yaml'"
check "mixed migration preserves the unsupported caller's edge and adds the supported .yaml caller's replacement" \
  "grep -A1 \"'.github/workflows/ci.yml':\" '$sp_mixed/actions.lock' | grep -q \"standards@$STALE_A\" && grep -A1 \"'.github/workflows/rust-ci.yaml':\" '$sp_mixed/actions.lock' | grep -q \"standards@$GOOD\" && grep -q \"^    'hyperpolymath/standards@$STALE_A':\" '$sp_mixed/actions.lock' && grep -q \"^    'hyperpolymath/standards@$GOOD':\" '$sp_mixed/actions.lock'"

# sp10: the lock merger reads both quote styles and keeps what it does not
# understand. Before this a double-quoted record (a hand-added one) was
# silently dropped from `dependencies:` by any stale-pin repair.
mkdir -p "$fx/sp10/.github/workflows"
printf 'jobs:\n  rust:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$STALE_A" > "$fx/sp10/.github/workflows/rust-ci.yml"
cat > "$fx/sp10/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/rust-ci.yml':
        - 'hyperpolymath/standards@$STALE_A'
        - "actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1"
dependencies:
    "actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1":
        ref: "v7.0.1"
        commit: "sha1-3d3c42e5aac5ba805825da76410c181273ba90b1"
        owner_id: 1
        repo_id: 2
    'hyperpolymath/standards@$STALE_A':
        ref: '$STALE_A'
        # a line the merger has no rule for
        commit: 'sha1-$STALE_A'
        owner_id: 6759885
        repo_id: 1116521501
LOCK
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp10"
check "a double-quoted lock record survives a stale-pin repair, ref and all" \
  "[ \$rc -eq 3 ] && grep -q '^    \"actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1\":$' '$fx/sp10/.github/workflows/actions.lock' && grep -q '^        ref: \"v7.0.1\"$' '$fx/sp10/.github/workflows/actions.lock'"
check "a line the merger does not understand is kept under its record, and the stale record moved" \
  "grep -q '^        # a line the merger has no rule for$' '$fx/sp10/.github/workflows/actions.lock' && grep -q \"^    'hyperpolymath/standards@$GOOD':\" '$fx/sp10/.github/workflows/actions.lock' && ! grep -q \"$STALE_A\" '$fx/sp10/.github/workflows/actions.lock'"

# sp11: a ref that merely STARTS with a dead SHA (`<sha>-patched`) is another
# ref the table never named: left alone, while the real dead pin beside it moves.
mkdir -p "$fx/sp11/.github/workflows"
printf 'jobs:\n  patched:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s-patched\n  dead:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$STALE_A" "$STALE_A" > "$fx/sp11/.github/workflows/ci.yml"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp11"
check "a tag that begins with a dead SHA is not that SHA: untouched, exit 0, the dead pin beside it moved" \
  "[ \$rc -eq 0 ] && sed -n '3p' '$fx/sp11/.github/workflows/ci.yml' | grep -q \"@$STALE_A-patched\$\" && sed -n '5p' '$fx/sp11/.github/workflows/ci.yml' | grep -q \"@$GOOD\$\" && ! grep -q 'UNSUPPORTED' <<<\"\$out\""

# sp12: the whole table is validated before any caller is touched: a good
# row followed by a malformed one leaves every workflow and the lock as they
# were, exit 2.
mkdir -p "$fx/sp12/.github/workflows"
printf 'jobs:\n  rust:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$STALE_A" > "$fx/sp12/.github/workflows/rust-ci.yml"
cp "$fx/sp12/.github/workflows/rust-ci.yml" "$fx/sp12-before.yml"
cp "$fx/sp5/.github/workflows/actions.lock" "$fx/sp12/.github/workflows/actions.lock"
cp "$fx/sp12/.github/workflows/actions.lock" "$fx/sp12-before.lock"
printf 'hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml\t%s\t%s\tgood row\nhyperpolymath/standards/.github/workflows/*\t%s\tnot-a-sha\tbad row after a good one\n' \
  "$STALE_A" "$GOOD" "$STALE_B" > "$fx/sp12-pins.tsv"
run_fix env PINS_TABLE="$fx/sp12-pins.tsv" bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp12"
check "a malformed row anywhere in the table stops the run before any caller or lock is touched: exit 2" \
  "[ \$rc -eq 2 ] && grep -q 'table row 2: bad replacement SHA' <<<\"\$out\" && cmp -s '$fx/sp12-before.yml' '$fx/sp12/.github/workflows/rust-ci.yml' && cmp -s '$fx/sp12-before.lock' '$fx/sp12/.github/workflows/actions.lock'"

# sp13: a lock edge is per repository@SHA. An unrelated repository that sits
# at the same SHA keeps its edge and its record when the covered one moves.
mkdir -p "$fx/sp13/.github/workflows"
printf 'jobs:\n  rust:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n  other:\n    uses: octo-org/example/.github/workflows/ci.yml@%s\n' "$STALE_A" "$STALE_A" > "$fx/sp13/.github/workflows/ci.yml"
cat > "$fx/sp13/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'hyperpolymath/standards@$STALE_A'
        - 'octo-org/example@$STALE_A'
dependencies:
    'hyperpolymath/standards@$STALE_A':
        ref: '$STALE_A'
        commit: 'sha1-$STALE_A'
        owner_id: 6759885
        repo_id: 1116521501
    'octo-org/example@$STALE_A':
        ref: '$STALE_A'
        commit: 'sha1-$STALE_A'
        owner_id: 1
        repo_id: 2
LOCK
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp13"
check "only the covered repository's edge and record move; the unrelated repository at the same SHA keeps both" \
  "[ \$rc -eq 3 ] && grep -q \"^        - 'hyperpolymath/standards@$GOOD'\$\" '$fx/sp13/.github/workflows/actions.lock' && grep -q \"^        - 'octo-org/example@$STALE_A'\$\" '$fx/sp13/.github/workflows/actions.lock' && grep -q \"^    'hyperpolymath/standards@$GOOD':\$\" '$fx/sp13/.github/workflows/actions.lock' && grep -A1 \"^    'octo-org/example@$STALE_A':\$\" '$fx/sp13/.github/workflows/actions.lock' | grep -q \"ref: '$STALE_A'\" && [ \$(grep -c \"$STALE_A\" '$fx/sp13/.github/workflows/actions.lock') -eq 4 ]"

# sp14: a lock that exists but carries no edge for the rewritten caller was
# already out of sync; the caller still moves, but the run says so and ends 3,
# never 0 — dispatch must not record a clean repair over a stale lock.
mkdir -p "$fx/sp14/.github/workflows"
printf 'jobs:\n  rust:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$STALE_A" > "$fx/sp14/.github/workflows/ci.yml"
cat > "$fx/sp14/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/other.yml':
        - 'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1'
dependencies:
    'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1':
        ref: 'v7.0.1'
        commit: 'sha1-3d3c42e5aac5ba805825da76410c181273ba90b1'
        owner_id: 1
        repo_id: 2
LOCK
cp "$fx/sp14/.github/workflows/actions.lock" "$fx/sp14-before.lock"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp14"
check "a rewritten caller with no lock edge is reported and the run ends 3, lock untouched, caller moved" \
  "[ \$rc -eq 3 ] && grep -q 'has no edge' <<<\"\$out\" && grep -q \"@$GOOD\" '$fx/sp14/.github/workflows/ci.yml' && cmp -s '$fx/sp14-before.lock' '$fx/sp14/.github/workflows/actions.lock'"

# sp15: the candidate scan is built from the owners the table names, so a
# private table for another owner finds its callers.
mkdir -p "$fx/sp15/.github/workflows"
printf 'octo/example/.github/workflows/*\t%s\t%s\tprivate owner\n' "$STALE_A" "$GOOD" > "$fx/sp15-pins.tsv"
printf 'jobs:\n  ci:\n    uses: octo/example/.github/workflows/ci.yml@%s\n' "$STALE_A" > "$fx/sp15/.github/workflows/ci.yml"
run_fix env PINS_TABLE="$fx/sp15-pins.tsv" bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp15"
check "a table row for another owner finds and moves that owner's caller" \
  "[ \$rc -eq 0 ] && grep -q \"octo/example/.github/workflows/ci.yml@$GOOD\$\" '$fx/sp15/.github/workflows/ci.yml' && grep -q '^FIXED' <<<\"\$out\""

# sp16: a double-quoted lock edge moves like a single-quoted one and keeps
# its quotes; a record whose key merely CONTAINS repo@sha
# (`evilhyperpolymath/standards@<sha>`) is another record and stays.
mkdir -p "$fx/sp16/.github/workflows"
printf 'jobs:\n  rust:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$STALE_A" > "$fx/sp16/.github/workflows/ci.yml"
cat > "$fx/sp16/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - "hyperpolymath/standards@$STALE_A"
dependencies:
    "hyperpolymath/standards@$STALE_A":
        ref: "$STALE_A"
        commit: "sha1-$STALE_A"
        owner_id: 6759885
        repo_id: 1116521501
    'evilhyperpolymath/standards@$STALE_A':
        ref: '$STALE_A'
        commit: 'sha1-$STALE_A'
        owner_id: 1
        repo_id: 2
LOCK
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp16"
check "a double-quoted edge and record move with their quotes; a key that merely contains repo@sha is left alone" \
  "[ \$rc -eq 3 ] && grep -q \"^        - \\\"hyperpolymath/standards@$GOOD\\\"\$\" '$fx/sp16/.github/workflows/actions.lock' && grep -q \"^    \\\"hyperpolymath/standards@$GOOD\\\":\$\" '$fx/sp16/.github/workflows/actions.lock' && grep -q \"^        ref: \\\"$GOOD\\\"\$\" '$fx/sp16/.github/workflows/actions.lock' && grep -A1 \"^    'evilhyperpolymath/standards@$STALE_A':\$\" '$fx/sp16/.github/workflows/actions.lock' | grep -q \"ref: '$STALE_A'\" && [ \$(grep -c \"$STALE_A\" '$fx/sp16/.github/workflows/actions.lock') -eq 3 ]"

# A supported call beside an unsupported flow-style job must remain stale:
# the file and its existing lock edge are one refused migration.
sp_atomic="$fx/sp-atomic/.github/workflows"
mkdir -p "$sp_atomic"
cat > "$sp_atomic/ci.yml" <<YML
jobs:
  supported:
    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@$STALE_A
  unsupported: { uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@$STALE_A }
YML
cat > "$sp_atomic/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'hyperpolymath/standards@$STALE_A'
dependencies:
    'hyperpolymath/standards@$STALE_A':
        ref: '$STALE_A'
        commit: 'sha1-$STALE_A'
LOCK
cp "$sp_atomic/ci.yml" "$fx/sp-atomic-before.yml"
cp "$sp_atomic/actions.lock" "$fx/sp-atomic-before.lock"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp-atomic"
check "an unsupported job prevents every rewrite in its file and leaves the lock untouched" \
  "[ \$rc -eq 2 ] && grep -q 'UNSUPPORTED.*ci.yml:4:' <<<\"\$out\" && ! grep -q '^FIXED' <<<\"\$out\" && cmp -s '$fx/sp-atomic-before.yml' '$sp_atomic/ci.yml' && cmp -s '$fx/sp-atomic-before.lock' '$sp_atomic/actions.lock'"

# Overlapping table rows are evaluated in order, including exact and wildcard
# rows. Each order chooses its first match rather than a newer revision.
sp_order="$fx/sp-order/.github/workflows"
mkdir -p "$sp_order"
alternate=2222222222222222222222222222222222222222
for first_match in exact wildcard; do
    if [ "$first_match" = exact ]; then
        printf 'fixture/reusable/.github/workflows/ci.yml\t%s\t%s\texact first\nfixture/reusable/.github/workflows/*\t%s\t%s\twildcard second\n' \
          "$STALE_A" "$GOOD" "$STALE_A" "$alternate" > "$fx/ordered-pins.tsv"
        chosen="$GOOD"
    else
        printf 'fixture/reusable/.github/workflows/*\t%s\t%s\twildcard first\nfixture/reusable/.github/workflows/ci.yml\t%s\t%s\texact second\n' \
          "$STALE_A" "$alternate" "$STALE_A" "$GOOD" > "$fx/ordered-pins.tsv"
        chosen="$alternate"
    fi
    printf 'jobs:\n  call:\n    uses: fixture/reusable/.github/workflows/ci.yml@%s\n' "$STALE_A" > "$sp_order/ci.yml"
    printf 'jobs:\n  call:\n    uses: fixture/reusable/.github/workflows/ci.yml@%s\n' "$chosen" > "$fx/sp-order-expected.yml"
    run_fix env PINS_TABLE="$fx/ordered-pins.tsv" bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp-order"
    check "overlapping table rows choose the first match when $first_match comes first" \
      "[ \$rc -eq 0 ] && cmp -s '$fx/sp-order-expected.yml' '$sp_order/ci.yml'"
    run_fix env PINS_TABLE="$fx/ordered-pins.tsv" bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp-order"
    check "the $first_match-first replacement is a stable no-op on the next run" \
      "[ \$rc -eq 0 ] && cmp -s '$fx/sp-order-expected.yml' '$sp_order/ci.yml' && ! grep -q '^FIXED' <<<\"\$out\""
done

# A replacement may already have a dependency record. Merging must preserve
# its spelling and metadata and produce a sorted union without duplicate uses.
sp_existing="$fx/sp-existing/.github/workflows"
mkdir -p "$sp_existing"
printf 'jobs:\n  call:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$STALE_A" > "$sp_existing/ci.yml"
cat > "$sp_existing/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'hyperpolymath/standards@$STALE_A'
dependencies:
    "hyperpolymath/standards@$GOOD":
        ref: "$GOOD"
        commit: "sha1-$GOOD"
        owner_id: 1
        repo_id: 2
        # keep replacement metadata
        uses:
            - 'fixture/shared@$GOOD'
            - 'fixture/zeta@$GOOD'
    'hyperpolymath/standards@$STALE_A':
        ref: '$STALE_A'
        commit: 'sha1-$STALE_A'
        owner_id: 1
        repo_id: 2
        uses:
            - 'fixture/alpha@$GOOD'
            - 'fixture/shared@$GOOD'
LOCK
cat > "$fx/sp-existing-expected.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'hyperpolymath/standards@$GOOD'
dependencies:
    "hyperpolymath/standards@$GOOD":
        ref: "$GOOD"
        commit: "sha1-$GOOD"
        owner_id: 1
        repo_id: 2
        # keep replacement metadata
        uses:
            - 'fixture/alpha@$GOOD'
            - 'fixture/shared@$GOOD'
            - 'fixture/zeta@$GOOD'
LOCK
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp-existing"
check "migration into an existing replacement record keeps its header and merges nested uses exactly once" \
  "[ \$rc -eq 3 ] && cmp -s '$fx/sp-existing-expected.lock' '$sp_existing/actions.lock'"
printf 'jobs:\n  call:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$GOOD" > "$fx/sp-existing-expected.yml"
check "the caller follows the existing replacement and the run reports the incomplete closure" \
  "cmp -s '$fx/sp-existing-expected.yml' '$sp_existing/ci.yml' && grep -q 'nested uses: list was NOT recomputed' <<<\"\$out\""
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp-existing"
check "migration into an existing record is byte-identical on the second run and exits 0" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/sp-existing-expected.lock' '$sp_existing/actions.lock' && cmp -s '$fx/sp-existing-expected.yml' '$sp_existing/ci.yml'"

# Validate stale refs and reusable paths as well as replacement refs. Invalid
# rows after a matching good row must still stop all mutations up front.
while IFS=$'\t' read -r invalid_path invalid_sha diagnostic; do
    printf 'hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml\t%s\t%s\tgood row\n%s\t%s\t%s\tbad row\n' \
      "$STALE_A" "$GOOD" "$invalid_path" "$invalid_sha" "$GOOD" > "$fx/invalid-row-pins.tsv"
    run_fix env PINS_TABLE="$fx/invalid-row-pins.tsv" bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp12"
    check "$diagnostic in a later table row refuses the run before caller or lock changes" \
      "[ \$rc -eq 2 ] && grep -q 'table row 2: $diagnostic' <<<\"\$out\" && cmp -s '$fx/sp12-before.yml' '$fx/sp12/.github/workflows/rust-ci.yml' && cmp -s '$fx/sp12-before.lock' '$fx/sp12/.github/workflows/actions.lock'"
done <<'TSV'
fixture/reusable/.github/workflows/ci.yml	123456789012345678901234567890123456789	bad stale SHA
fixture/reusable/.github/workflows/../ci.yml	aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa	bad reusable path
TSV

# ── 3. fix-lock-self-sha-refs.sh ─────────────────────────────────────────────
# Only a workflow site proves a record is an action (a record no workflow
# uses is a transitive dependency, which the fixer will not classify by
# guess), so each action fixture carries the step that uses it.
action_steps() { # <workflows-dir> <owner/repo@sha>... : a ci.yml with one step per action
    { printf 'jobs:\n  build:\n    steps:\n'
      for a in "${@:2}"; do printf '      - uses: %s\n' "$a"; done
    } > "$1/ci.yml"
}
mkdir -p "$fx/ls/.github/workflows"
action_steps "$fx/ls/.github/workflows" "actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1"
cat > "$fx/ls/.github/workflows/actions.lock" <<'LOCK'
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1'
dependencies:
    'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1':
        ref: '3d3c42e5aac5ba805825da76410c181273ba90b1'
        commit: 'sha1-3d3c42e5aac5ba805825da76410c181273ba90b1'
        owner_id: 44036562
        repo_id: 197814629
    'actions/checkout@v7.0.1':
        ref: 'v7.0.1'
        commit: 'sha1-3d3c42e5aac5ba805825da76410c181273ba90b1'
        owner_id: 44036562
        repo_id: 197814629
    'hyperpolymath/standards@427eb35c40552d912d8c757518e8326f0d20f597':
        ref: '427eb35c40552d912d8c757518e8326f0d20f597'
        commit: 'sha1-427eb35c40552d912d8c757518e8326f0d20f597'
        owner_id: 6759885
        repo_id: 1116521501
    'nobody/unknown-action@0000000000000000000000000000000000000001':
        ref: '0000000000000000000000000000000000000001'
        commit: 'sha1-0000000000000000000000000000000000000001'
        owner_id: 1
        repo_id: 1
LOCK
set +e
bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls" --offline > "$fx/ls.out" 2>&1; rc=$?
set -e
check "the self-SHA checkout record gets its tag from the offline map" "grep -A1 \"'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1':\" '$fx/ls/.github/workflows/actions.lock' | grep -q \"ref: 'v7.0.1'\""
check "a tag-ref record is untouched" "[ \$(grep -c \"ref: 'v7.0.1'\" '$fx/ls/.github/workflows/actions.lock') -eq 2 ]"
check "a reusable-source record keeps its SHA ref (standards' accepted lock does the same)" "grep -A1 \"'hyperpolymath/standards@427eb35c\" '$fx/ls/.github/workflows/actions.lock' | grep -q \"ref: '427eb35c40552d912d8c757518e8326f0d20f597'\""
check "an unresolvable record is left alone and reported, and the exit code says so" "[ \$rc -eq 1 ] && grep -q 'UNRESOLVED  nobody/unknown-action' '$fx/ls.out' && grep -A1 \"'nobody/unknown-action@\" '$fx/ls/.github/workflows/actions.lock' | grep -q \"ref: '0000000000000000000000000000000000000001'\""
check "commit: lines and ids are untouched" "[ \$(grep -c \"commit: 'sha1-3d3c42e5aac5ba805825da76410c181273ba90b1'\" '$fx/ls/.github/workflows/actions.lock') -eq 2 ] && grep -q 'repo_id: 197814629' '$fx/ls/.github/workflows/actions.lock'"
sed -i '/nobody\/unknown-action/,/repo_id: 1$/d' "$fx/ls/.github/workflows/actions.lock"
cp "$fx/ls/.github/workflows/actions.lock" "$fx/ls1.lock"
bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls" --offline >/dev/null 2>&1
check "second run is a no-op and exits 0" "cmp -s '$fx/ls1.lock' '$fx/ls/.github/workflows/actions.lock'"
check "a repository without a lock is OK, not an error" "mkdir -p '$fx/nolock' && bash '$fleet/scripts/fix-lock-self-sha-refs.sh' '$fx/nolock' --offline | grep -q 'OK (no actions.lock)'"
# A reusable source is whatever the workflows call as a reusable, not a
# hard-coded pair of names: its record keeps its SHA ref and is not "unresolved".
mkdir -p "$fx/ls4/.github/workflows"
printf 'jobs:\n  corpus:\n    uses: hyperpolymath/proven-tests-and-benches/.github/workflows/corpus-check.yml@2222222222222222222222222222222222222222\n' > "$fx/ls4/.github/workflows/ci.yml"
cat > "$fx/ls4/.github/workflows/actions.lock" <<'LOCK'
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'hyperpolymath/proven-tests-and-benches@2222222222222222222222222222222222222222'
dependencies:
    'hyperpolymath/proven-tests-and-benches@2222222222222222222222222222222222222222':
        ref: '2222222222222222222222222222222222222222'
        commit: 'sha1-2222222222222222222222222222222222222222'
        owner_id: 6759885
        repo_id: 2
LOCK
cp "$fx/ls4/.github/workflows/actions.lock" "$fx/ls4-before.lock"
set +e
bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls4" --offline > "$fx/ls4.out" 2>&1; rc4=$?
set -e
check "a reusable source read from the workflows keeps its SHA ref: untouched, not UNRESOLVED, exit 0" \
  "[ \$rc4 -eq 0 ] && ! grep -q UNRESOLVED '$fx/ls4.out' && cmp -s '$fx/ls4-before.lock' '$fx/ls4/.github/workflows/actions.lock'"
# One repository, two revisions, two classes: a reusable call keeps its SHA
# ref; a composite action from the same repository at another revision is
# repaired. The class is per repository@revision, read from the workflows.
SHA_A=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa; SHA_B=bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
mkdir -p "$fx/ls5/.github/workflows"
printf 'jobs:\n  gate:\n    uses: hyperpolymath/cicd-suite/.github/workflows/rust-ci.yml@%s\n  check:\n    steps:\n      - uses: hyperpolymath/cicd-suite/actions/manifest-check@%s\n' "$SHA_A" "$SHA_B" > "$fx/ls5/.github/workflows/ci.yml"
cat > "$fx/ls5/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'hyperpolymath/cicd-suite@$SHA_A'
        - 'hyperpolymath/cicd-suite@$SHA_B'
dependencies:
    'hyperpolymath/cicd-suite@$SHA_A':
        ref: '$SHA_A'
        commit: 'sha1-$SHA_A'
        owner_id: 6759885
        repo_id: 3
    'hyperpolymath/cicd-suite@$SHA_B':
        ref: '$SHA_B'
        commit: 'sha1-$SHA_B'
        owner_id: 6759885
        repo_id: 3
LOCK
printf 'hyperpolymath/cicd-suite\t%s\tv1.2.3\n' "$SHA_B" > "$fx/ls5-tags.tsv"
set +e
TAG_MAP="$fx/ls5-tags.tsv" bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls5" --offline > "$fx/ls5.out" 2>&1; rc5=$?
set -e
check "same repository, two revisions: the reusable call keeps its SHA ref, the action record gets its tag" \
  "[ \$rc5 -eq 0 ] && grep -A1 \"'hyperpolymath/cicd-suite@$SHA_A':\" '$fx/ls5/.github/workflows/actions.lock' | grep -q \"ref: '$SHA_A'\" && grep -A1 \"'hyperpolymath/cicd-suite@$SHA_B':\" '$fx/ls5/.github/workflows/actions.lock' | grep -q \"ref: 'v1.2.3'\" && ! grep -q UNRESOLVED '$fx/ls5.out'"
# A double-quoted record is the same record: repaired, quote style kept.
mkdir -p "$fx/ls6/.github/workflows"
action_steps "$fx/ls6/.github/workflows" "actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1"
printf 'version: "v0.0.3"\nworkflows:\n    ".github/workflows/ci.yml":\n        - "actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1"\ndependencies:\n    "actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1":\n        ref: "3d3c42e5aac5ba805825da76410c181273ba90b1"\n        commit: "sha1-3d3c42e5aac5ba805825da76410c181273ba90b1"\n        owner_id: 44036562\n        repo_id: 197814629\n' > "$fx/ls6/.github/workflows/actions.lock"
set +e
bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls6" --offline > "$fx/ls6.out" 2>&1; rc6=$?
set -e
check "a double-quoted self-SHA record is parsed and repaired, keeping its quotes" \
  "[ \$rc6 -eq 0 ] && grep -q 'ref: \"v7.0.1\"' '$fx/ls6/.github/workflows/actions.lock' && grep -q 'FIXED  actions/checkout' '$fx/ls6.out'"
# A tag Git allows but this fixer will not write (`&`, `|`, a quote) is refused.
SHA_C=cccccccccccccccccccccccccccccccccccccccc
mkdir -p "$fx/ls7/.github/workflows"
action_steps "$fx/ls7/.github/workflows" "nobody/weird@$SHA_C"
printf "version: 'v0.0.3'\nworkflows: {}\ndependencies:\n    'nobody/weird@%s':\n        ref: '%s'\n        commit: 'sha1-%s'\n        owner_id: 1\n        repo_id: 1\n" "$SHA_C" "$SHA_C" "$SHA_C" > "$fx/ls7/.github/workflows/actions.lock"
printf 'nobody/weird\t%s\trelease&stable\n' "$SHA_C" > "$fx/ls7-tags.tsv"
cp "$fx/ls7/.github/workflows/actions.lock" "$fx/ls7-before.lock"
set +e
TAG_MAP="$fx/ls7-tags.tsv" bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls7" --offline > "$fx/ls7.out" 2>&1; rc7=$?
set -e
check "a tag with characters the fixer will not write is UNRESOLVED, the record untouched, exit 1" \
  "[ \$rc7 -eq 1 ] && grep -q 'carries characters' '$fx/ls7.out' && cmp -s '$fx/ls7-before.lock' '$fx/ls7/.github/workflows/actions.lock'"

# Exercise tag resolution through a local Git stub: even the online branch
# stays offline, and the call log proves when a lookup did or did not occur.
write_action_lock() { # <lock-path> <repository> <sha> <ref>
    cat > "$1" <<LOCK
version: 'v0.0.3'
workflows: {}
dependencies:
    '$2@$3':
        ref: '$4'
        commit: 'sha1-$3'
        owner_id: 1
        repo_id: 2
LOCK
}
lookup_repo="$fx/tag lookup"
lookup_lock="$lookup_repo/.github/workflows/actions.lock"
mkdir -p "$lookup_repo/.github/workflows" "$fx/git-bin"
action_steps "$lookup_repo/.github/workflows" "fixture/action@$SHA_A"
cat > "$fx/git-bin/git" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$*" >> "$PIN_TEST_GIT_LOG"
if [ "$#" -ne 3 ] || [ "$1" != ls-remote ] || [ "$2" != --tags ] || [ "$3" != https://github.com/fixture/action ]; then
    exit 99
fi
cat "$PIN_TEST_GIT_RESPONSE"
exit "${PIN_TEST_GIT_STATUS:-0}"
SH
chmod +x "$fx/git-bin/git"
: > "$fx/git-calls"
cat > "$fx/git-response" <<TAGS
$SHA_A	refs/tags/v2
$SHA_A	refs/tags/v2.4
$SHA_B	refs/tags/v2.4.7
$SHA_A	refs/tags/v2.4.7^{}
$SHA_A	refs/tags/release-with-a-longer-name
$SHA_C	refs/tags/v99.99.99
TAGS
# Wrong repository and wrong revision rows must not shadow the exact match.
printf 'other/action\t%s\tv9.9.9\nfixture/action\t%s\tv8.8.8\nfixture/action\t%s\trelease/v3.2.1+build-4\n' \
  "$SHA_A" "$SHA_B" "$SHA_A" > "$fx/lookup-tags.tsv"
write_action_lock "$lookup_lock" fixture/action "$SHA_A" "$SHA_A"
write_action_lock "$fx/lookup-expected.lock" fixture/action "$SHA_A" release/v3.2.1+build-4
run_lookup() {
    run_fix env PATH="$fx/git-bin:$PATH" PIN_TEST_GIT_LOG="$fx/git-calls" \
      PIN_TEST_GIT_RESPONSE="$fx/git-response" TAG_MAP="$fx/lookup-tags.tsv" "$@" \
      bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$lookup_repo" "${lookup_mode[@]}"
}
lookup_mode=()
run_lookup
check "tag map matches repository and revision exactly, accepts safe punctuation and avoids Git" \
  "[ \$rc -eq 0 ] && [ ! -s '$fx/git-calls' ] && cmp -s '$fx/lookup-expected.lock' '$lookup_lock'"

# Empty map: --offline refuses the unresolved record without consulting Git.
: > "$fx/lookup-tags.tsv"
write_action_lock "$lookup_lock" fixture/action "$SHA_A" "$SHA_A"
cp "$lookup_lock" "$fx/lookup-before.lock"
lookup_mode=(--offline)
run_lookup
check "offline mode never calls Git, leaves an unknown record byte-identical and exits 1" \
  "[ \$rc -eq 1 ] && grep -q 'UNRESOLVED  fixture/action' <<<\"\$out\" && [ ! -s '$fx/git-calls' ] && cmp -s '$fx/lookup-before.lock' '$lookup_lock'"

# The highest specificity wins before length; an annotated tag resolves via
# its peeled commit, and unrelated commits must not influence the choice.
lookup_mode=()
write_action_lock "$fx/lookup-expected.lock" fixture/action "$SHA_A" v2.4.7
run_lookup
check "Git fallback chooses the most specific matching tag including peeled annotated tags" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/lookup-expected.lock' '$lookup_lock'"
printf '%s\n' 'ls-remote --tags https://github.com/fixture/action' > "$fx/expected-git-calls"
check "Git fallback requests only the action repository's tags, once" \
  "cmp -s '$fx/expected-git-calls' '$fx/git-calls'"
: > "$fx/git-calls"
run_lookup
check "resolved lock is idempotent and does not perform another Git lookup" \
  "[ \$rc -eq 0 ] && [ ! -s '$fx/git-calls' ] && cmp -s '$fx/lookup-expected.lock' '$lookup_lock'"

# A transport failure and a successful response containing no matching commit
# both leave the lock unchanged and explicitly unresolved.
printf '%s\trefs/tags/v99.99.99\n' "$SHA_C" > "$fx/git-response"
for git_status in 0 128; do
    write_action_lock "$lookup_lock" fixture/action "$SHA_A" "$SHA_A"
    : > "$fx/git-calls"
    run_lookup PIN_TEST_GIT_STATUS="$git_status"
    check "Git response with no usable tag (status $git_status) exits 1 and preserves the lock" \
      "[ \$rc -eq 1 ] && grep -q 'UNRESOLVED  fixture/action' <<<\"\$out\" && cmp -s '$fx/expected-git-calls' '$fx/git-calls' && cmp -s '$fx/lookup-before.lock' '$lookup_lock'"
done

# Sed/YAML metacharacters must never enter a lock, even from a local map.
lookup_mode=(--offline)
: > "$fx/git-calls"
for unsafe_tag in 'release|stable' "release'candidate" 'release"candidate' 'release&stable'; do
    write_action_lock "$lookup_lock" fixture/action "$SHA_A" "$SHA_A"
    printf 'fixture/action\t%s\t%s\n' "$SHA_A" "$unsafe_tag" > "$fx/lookup-tags.tsv"
    run_lookup
    check "unsafe mapped tag $unsafe_tag is refused without changing the lock" \
      "[ \$rc -eq 1 ] && grep -q 'carries characters' <<<\"\$out\" && [ ! -s '$fx/git-calls' ] && cmp -s '$fx/lookup-before.lock' '$lookup_lock'"
done

# Similar-looking records are non-defects: a 41-digit key and a full SHA key
# whose ref names another SHA are not self-SHA action records.
write_action_lock "$lookup_lock" fixture/action "${SHA_A}a" "${SHA_A}a"
cp "$lookup_lock" "$fx/non-self-before.lock"
run_lookup
check "a 41-hex record key is untouched and never reported unresolved" \
  "[ \$rc -eq 0 ] && ! grep -q UNRESOLVED <<<\"\$out\" && [ ! -s '$fx/git-calls' ] && cmp -s '$fx/non-self-before.lock' '$lookup_lock'"
write_action_lock "$lookup_lock" fixture/action "$SHA_A" "$SHA_B"
cp "$lookup_lock" "$fx/non-self-before.lock"
run_lookup
check "a ref different from its record's SHA is untouched" \
  "[ \$rc -eq 0 ] && ! grep -q UNRESOLVED <<<\"\$out\" && [ ! -s '$fx/git-calls' ] && cmp -s '$fx/non-self-before.lock' '$lookup_lock'"

# Equal specificity is resolved by tag length. A missing optional map still
# allows the Git fallback, whose response remains controlled by the stub.
lookup_mode=()
write_action_lock "$lookup_lock" fixture/action "$SHA_A" "$SHA_A"
printf '%s\trefs/tags/v2.4.7\n%s\trefs/tags/v2.4.7-rc1\n' "$SHA_A" "$SHA_A" > "$fx/git-response"
write_action_lock "$fx/lookup-length-expected.lock" fixture/action "$SHA_A" v2.4.7-rc1
: > "$fx/lookup-tags.tsv"
: > "$fx/git-calls"
run_lookup
check "Git fallback breaks equal dot-count ties by tag length" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/lookup-length-expected.lock' '$lookup_lock' && cmp -s '$fx/expected-git-calls' '$fx/git-calls'"
write_action_lock "$lookup_lock" fixture/action "$SHA_A" "$SHA_A"
rm "$fx/lookup-tags.tsv"
: > "$fx/git-calls"
run_lookup
check "a missing tag map falls back to Git and repairs the record" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/lookup-length-expected.lock' '$lookup_lock' && cmp -s '$fx/expected-git-calls' '$fx/git-calls'"
write_action_lock "$lookup_lock" fixture/action "$SHA_A" "$SHA_A"
printf '%s\trefs/tags/v2.4.7&unsafe\n' "$SHA_A" > "$fx/git-response"
: > "$fx/git-calls"
run_lookup
check "unsafe tags from Git are refused just like unsafe mapped tags" \
  "[ \$rc -eq 1 ] && grep -q 'carries characters' <<<\"\$out\" && cmp -s '$fx/lookup-before.lock' '$lookup_lock' && cmp -s '$fx/expected-git-calls' '$fx/git-calls'"

# ls8: a record's class comes from real `uses:` keys. The same reusable path
# inside a `run: |` script or a fixture under `env:` is text: it cannot make an
# action record "a reusable call" and so hide its self-SHA ref from repair.
mkdir -p "$fx/ls8/.github/workflows"
cat > "$fx/ls8/.github/workflows/ci.yml" <<YML
jobs:
  test:
    runs-on: ubuntu-latest
    env:
      FIXTURE: |
        uses: foo/bar/.github/workflows/test.yml@$SHA_A
    steps:
      - run: |
          echo "uses: foo/bar/.github/workflows/test.yml@$SHA_A"
      - uses: foo/bar@$SHA_A
YML
cat > "$fx/ls8/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'foo/bar@$SHA_A'
dependencies:
    'foo/bar@$SHA_A':
        ref: '$SHA_A'
        commit: 'sha1-$SHA_A'
        owner_id: 1
        repo_id: 2
LOCK
printf 'foo/bar\t%s\tv1.0.0\n' "$SHA_A" > "$fx/ls8-tags.tsv"
TAG_MAP="$fx/ls8-tags.tsv" bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls8" --offline > "$fx/ls8.out" 2>&1; rc8=$?
check "a reusable path inside run: | or env: is text: the action record it names is still repaired" \
  "[ \$rc8 -eq 0 ] && grep -q \"ref: 'v1.0.0'\" '$fx/ls8/.github/workflows/actions.lock' && ! grep -q UNRESOLVED '$fx/ls8.out'"

# ls9: a record is read to its end: `ref:` after `commit:` and the ids is
# still the record's ref, and still repaired.
mkdir -p "$fx/ls9/.github/workflows"
printf 'jobs:\n  check:\n    steps:\n      - uses: foo/bar@%s\n' "$SHA_A" > "$fx/ls9/.github/workflows/ci.yml"
cat > "$fx/ls9/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'foo/bar@$SHA_A'
dependencies:
    'foo/bar@$SHA_A':
        commit: 'sha1-$SHA_A'
        owner_id: 1
        repo_id: 2
        ref: '$SHA_A'
LOCK
printf 'foo/bar\t%s\tv1.0.0\n' "$SHA_A" > "$fx/ls9-tags.tsv"
TAG_MAP="$fx/ls9-tags.tsv" bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls9" --offline > "$fx/ls9.out" 2>&1; rc9=$?
check "a ref: that is not the first line of its record is still the record's ref: repaired, exit 0" \
  "[ \$rc9 -eq 0 ] && grep -q \"^        ref: 'v1.0.0'\$\" '$fx/ls9/.github/workflows/actions.lock' && ! grep -q \"ref: '$SHA_A'\" '$fx/ls9/.github/workflows/actions.lock' && grep -q '^FIXED' '$fx/ls9.out'"

# ls10: a plain-scalar `ref: <sha>` (no quotes) is the same ref: found and
# repaired, the absence of quotes kept.
mkdir -p "$fx/ls10/.github/workflows"
printf 'jobs:\n  check:\n    steps:\n      - uses: foo/bar@%s\n' "$SHA_A" > "$fx/ls10/.github/workflows/ci.yml"
cat > "$fx/ls10/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'foo/bar@$SHA_A'
dependencies:
    'foo/bar@$SHA_A':
        ref: $SHA_A
        commit: sha1-$SHA_A
        owner_id: 1
        repo_id: 2
LOCK
printf 'foo/bar\t%s\tv1.0.0\n' "$SHA_A" > "$fx/ls10-tags.tsv"
TAG_MAP="$fx/ls10-tags.tsv" bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls10" --offline > "$fx/ls10.out" 2>&1; rc10=$?
check "an unquoted self-SHA ref is found and repaired, unquoted" \
  "[ \$rc10 -eq 0 ] && grep -q '^        ref: v1.0.0$' '$fx/ls10/.github/workflows/actions.lock' && grep -q '^FIXED' '$fx/ls10.out'"

# ls11: a flow-style job (`call: { uses: ... }`) is a reusable call and keeps
# its SHA ref; a flow-style step (`- { uses: ... }`) is an action and is repaired.
mkdir -p "$fx/ls11/.github/workflows"
cat > "$fx/ls11/.github/workflows/ci.yml" <<YML
jobs:
  call: { uses: octo/example/.github/workflows/ci.yml@$SHA_A }
  build:
    steps:
      - { uses: octo/act@$SHA_B }
YML
cat > "$fx/ls11/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'octo/example@$SHA_A'
        - 'octo/act@$SHA_B'
dependencies:
    'octo/example@$SHA_A':
        ref: '$SHA_A'
        commit: 'sha1-$SHA_A'
        owner_id: 1
        repo_id: 2
    'octo/act@$SHA_B':
        ref: '$SHA_B'
        commit: 'sha1-$SHA_B'
        owner_id: 1
        repo_id: 3
LOCK
printf 'octo/example\t%s\tv9.9.9\nocto/act\t%s\tv1.2.3\n' "$SHA_A" "$SHA_B" > "$fx/ls11-tags.tsv"
TAG_MAP="$fx/ls11-tags.tsv" bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls11" --offline > "$fx/ls11.out" 2>&1; rc11=$?
check "a flow-style job is a reusable call (SHA ref kept); a flow-style step is an action (repaired)" \
  "[ \$rc11 -eq 0 ] && grep -A1 \"^    'octo/example@$SHA_A':\$\" '$fx/ls11/.github/workflows/actions.lock' | grep -q \"ref: '$SHA_A'\" && grep -A1 \"^    'octo/act@$SHA_B':\$\" '$fx/ls11/.github/workflows/actions.lock' | grep -q \"ref: 'v1.2.3'\" && ! grep -q UNRESOLVED '$fx/ls11.out'"

# At the same revision a reusable call takes precedence over an action use,
# even across workflow files and when a tag is available. Removing the call
# exposes the action use, including its subdirectory, for repair.
ls_shared="$fx/ls-shared/.github/workflows"
mkdir -p "$ls_shared"
printf 'jobs:\n  call:\n    uses: fixture/dual/.github/workflows/ci.yml@%s\n' "$SHA_A" > "$ls_shared/reusable.yml"
printf 'jobs:\n  build:\n    steps:\n      - name: Nested action\n        uses: fixture/dual/actions/check@%s\n' "$SHA_A" > "$ls_shared/action.yaml"
write_action_lock "$ls_shared/actions.lock" fixture/dual "$SHA_A" "$SHA_A"
cp "$ls_shared/actions.lock" "$fx/ls-shared-before.lock"
printf 'fixture/dual\t%s\tv1.2.3\n' "$SHA_A" > "$fx/ls-shared-tags.tsv"
run_fix env TAG_MAP="$fx/ls-shared-tags.tsv" bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls-shared" --offline
check "a reusable and action at the same revision keep their shared SHA ref despite an available tag" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/ls-shared-before.lock' '$ls_shared/actions.lock' && ! grep -qE '^(FIXED|UNRESOLVED)' <<<\"\$out\""
rm "$ls_shared/reusable.yml"
write_action_lock "$fx/ls-shared-expected.lock" fixture/dual "$SHA_A" v1.2.3
run_fix env TAG_MAP="$fx/ls-shared-tags.tsv" bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls-shared" --offline
check "without the reusable call the subdirectory action in a .yaml workflow gets its tag" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/ls-shared-expected.lock' '$ls_shared/actions.lock' && grep -q '^FIXED  fixture/dual' <<<\"\$out\""

# A missing tag and an unsafe tag bracket a resolvable record. Neither may
# stop that repair, alter neighboring records, or hide the unresolved status.
ls_partial="$fx/ls-partial/.github/workflows"
mkdir -p "$ls_partial"
action_steps "$ls_partial" "fixture/missing@$SHA_A" "fixture/resolved@$SHA_B" "fixture/unsafe@$SHA_C"
cat > "$ls_partial/actions.lock" <<LOCK
version: 'v0.0.3'
workflows: {}
dependencies:
    'fixture/missing@$SHA_A':
        ref: '$SHA_A' # no tag
        commit: 'sha1-$SHA_A'
    "fixture/resolved@$SHA_B":
        commit: "sha1-$SHA_B"
        owner_id: 1
        repo_id: 2
        ref: "$SHA_B" # keep quote style and comment
    'fixture/unsafe@$SHA_C':
        ref: '$SHA_C'
        commit: 'sha1-$SHA_C'
LOCK
cat > "$fx/ls-partial-expected.lock" <<LOCK
version: 'v0.0.3'
workflows: {}
dependencies:
    'fixture/missing@$SHA_A':
        ref: '$SHA_A' # no tag
        commit: 'sha1-$SHA_A'
    "fixture/resolved@$SHA_B":
        commit: "sha1-$SHA_B"
        owner_id: 1
        repo_id: 2
        ref: "v2.3.4" # keep quote style and comment
    'fixture/unsafe@$SHA_C':
        ref: '$SHA_C'
        commit: 'sha1-$SHA_C'
LOCK
printf 'fixture/resolved\t%s\tv2.3.4\nfixture/unsafe\t%s\trelease&unsafe\n' "$SHA_B" "$SHA_C" > "$fx/ls-partial-tags.tsv"
run_fix env TAG_MAP="$fx/ls-partial-tags.tsv" bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls-partial" --offline
check "mixed records repair the resolvable ref in place and preserve both unresolved records" \
  "[ \$rc -eq 1 ] && cmp -s '$fx/ls-partial-expected.lock' '$ls_partial/actions.lock' && [ \$(grep -c '^FIXED' <<<\"\$out\") -eq 1 ] && [ \$(grep -c '^UNRESOLVED' <<<\"\$out\") -eq 2 ] && grep -q '^Rewrote 1 record(s)' <<<\"\$out\""
run_fix env TAG_MAP="$fx/ls-partial-tags.tsv" bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls-partial" --offline
check "a partially resolved lock is byte-identical on rerun and still exits 1" \
  "[ \$rc -eq 1 ] && cmp -s '$fx/ls-partial-expected.lock' '$ls_partial/actions.lock' && ! grep -qE '^(FIXED|OK)' <<<\"\$out\" && [ \$(grep -c '^UNRESOLVED' <<<\"\$out\") -eq 2 ]"
printf 'fixture/missing\t%s\tv1.0.0\nfixture/unsafe\t%s\tv3.0.0\n' "$SHA_A" "$SHA_C" > "$fx/ls-partial-tags.tsv"
run_fix env TAG_MAP="$fx/ls-partial-tags.tsv" bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls-partial" --offline
check "supplying safe tags resolves the remaining records without revisiting the repaired one" \
  "[ \$rc -eq 0 ] && [ \$(grep -c '^FIXED' <<<\"\$out\") -eq 2 ] && ! grep -q '^UNRESOLVED' <<<\"\$out\" && grep -q \"ref: 'v1.0.0' # no tag\" '$ls_partial/actions.lock' && grep -q \"ref: 'v3.0.0'\" '$ls_partial/actions.lock' && grep -q 'ref: \"v2.3.4\" # keep quote style and comment' '$ls_partial/actions.lock'"
cp "$ls_partial/actions.lock" "$fx/ls-partial-resolved.lock"
run_fix env TAG_MAP="$fx/ls-partial-tags.tsv" bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls-partial" --offline
check "a fully resolved mixed lock is an idempotent success" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/ls-partial-resolved.lock' '$ls_partial/actions.lock' && grep -q '^OK (no self-SHA action records)' <<<\"\$out\""

# Portability (review): Debian/Ubuntu's default awk is mawk. Run the same
# fixture through mawk when it is installed, and refuse a lock the parser
# cannot read instead of reporting OK.
if command -v mawk >/dev/null; then
    mkdir -p "$fx/mawk-bin" "$fx/ls2/.github/workflows"; ln -sf "$(command -v mawk)" "$fx/mawk-bin/awk"
    cp "$fx/ls1.lock" "$fx/ls2/.github/workflows/actions.lock"
    cp "$fx/ls/.github/workflows/ci.yml" "$fx/ls2/.github/workflows/ci.yml"
    sed -i "s/ref: 'v7.0.1'/ref: '3d3c42e5aac5ba805825da76410c181273ba90b1'/" "$fx/ls2/.github/workflows/actions.lock"
    sed -i '0,/ref: .3d3c42e5aac5ba805825da76410c181273ba90b1./!{0,/ref: .3d3c42e5aac5ba805825da76410c181273ba90b1./s//ref: '"'"'v7.0.1'"'"'/}' "$fx/ls2/.github/workflows/actions.lock"
    printf "    'x/thirty-nine@123456789012345678901234567890123456789':\n        ref: '123456789012345678901234567890123456789'\n        commit: 'sha1-123456789012345678901234567890123456789'\n" >> "$fx/ls2/.github/workflows/actions.lock"
    set +e
    PATH="$fx/mawk-bin:$PATH" bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls2" --offline > "$fx/ls2.out" 2>&1; rc2=$?
    set -e
    check "under mawk the self-SHA record is repaired and the run exits 0" \
      "[ \$rc2 -eq 0 ] && grep -A1 \"'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1':\" '$fx/ls2/.github/workflows/actions.lock' | grep -q \"ref: 'v7.0.1'\""
    check "a 39-hex key is not a record and is neither rewritten nor reported" \
      "grep -q \"ref: '123456789012345678901234567890123456789'\" '$fx/ls2/.github/workflows/actions.lock' && ! grep -q UNRESOLVED '$fx/ls2.out'"
else
    echo "  skip mawk portability (mawk not installed)"
fi
mkdir -p "$fx/noawk-bin"; printf '#!/bin/sh\nexit 1\n' > "$fx/noawk-bin/awk"; chmod +x "$fx/noawk-bin/awk"
cp "$fx/ls1.lock" "$fx/ls3.lock"; mkdir -p "$fx/ls3/.github/workflows"; cp "$fx/ls1.lock" "$fx/ls3/.github/workflows/actions.lock"
set +e
PATH="$fx/noawk-bin:$PATH" bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls3" --offline > "$fx/ls3.out" 2>&1; rc3=$?
set -e
check "a parser failure exits 2, says so, and leaves the lock untouched (never OK)" \
  "[ \$rc3 -eq 2 ] && grep -q 'could not parse' '$fx/ls3.out' && ! grep -q '^OK' '$fx/ls3.out' && cmp -s '$fx/ls3.lock' '$fx/ls3/.github/workflows/actions.lock'"

# ── 4. Registry routes for the three new fixers ─────────────────────────────
# jq is also used by dispatch-paths.sh in the same CI job. Assert only the
# routes introduced here so unrelated registry maintenance stays independent.
while IFS=$'\t' read -r section key expected; do
    check "registry $section/$key selects $expected" \
      "jq -e --arg section '$section' --arg key '$key' --arg expected '$expected' '.registry[\$section][\$key] == \$expected' '$fleet/scripts/fix-script-registry.json' >/dev/null"
done <<'TSV'
by_category	ToolchainInputMissing	fix-rust-toolchain-input.sh
by_category	StaleReusablePin	fix-stale-reusable-pin.sh
by_category	LockSelfShaRef	fix-lock-self-sha-refs.sh
by_recipe	WF026	fix-rust-toolchain-input.sh
by_recipe	workflow_audit/sha_pinned_toolchain_without_input	fix-rust-toolchain-input.sh
by_recipe	WF027	fix-stale-reusable-pin.sh
by_recipe	workflow_audit/known_broken_reusable_revision	fix-stale-reusable-pin.sh
by_recipe	PIN-003	fix-stale-reusable-pin.sh
by_recipe	PI006	fix-lock-self-sha-refs.sh
by_recipe	pin_integrity/self_sha_lock_record	fix-lock-self-sha-refs.sh
TSV


# ── Review cases (gitbot-fleet#615, rounds 10–11) ───────────────────────────
# tc15: the empty string (`""`, `''`), the other null spellings YAML admits
# unquoted (`Null`, `NULL`) and an upper-case SHA are all the key with no
# value: filled in place, never duplicated. `"null"` quoted is the string
# null — a value the step names, left alone.
mkdir -p "$fx/tc15/.github/workflows"
cat > "$fx/tc15/.github/workflows/x.yml" <<'YML'
jobs:
  check:
    steps:
      - uses: dtolnay/rust-toolchain@6C977A6CA4077A0CEB28FFBE03F59D46E9AC8772
        with:
          toolchain: ""
          components: clippy
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: { toolchain: '', components: clippy }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with:
          toolchain: NULL # chosen later
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: { toolchain: Null }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: { toolchain: "null" }
YML
cat > "$fx/tc15-expected.yml" <<'YML'
jobs:
  check:
    steps:
      - uses: dtolnay/rust-toolchain@6C977A6CA4077A0CEB28FFBE03F59D46E9AC8772
        with:
          toolchain: stable
          components: clippy
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: { toolchain: stable, components: clippy }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with:
          toolchain: stable # chosen later
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: { toolchain: stable }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: { toolchain: "null" }
YML
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc15"
check "quoted-empty, Null/NULL and an upper-case SHA are filled in place; a quoted \"null\" string is a value" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/tc15-expected.yml' '$fx/tc15/.github/workflows/x.yml'"
cp "$fx/tc15/.github/workflows/x.yml" "$fx/tc15-after1.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc15"
check "tc15 second run is a no-op" "[ \$rc -eq 0 ] && cmp -s '$fx/tc15-after1.yml' '$fx/tc15/.github/workflows/x.yml'"

# tc16: an empty `toolchain:` whose next PLAIN line sits deeper is a nested
# mapping even when a comment or blank line sits between them: unsupported,
# the file untouched, exit 2 — never a second `toolchain:` key.
mkdir -p "$fx/tc16/.github/workflows"
cat > "$fx/tc16/.github/workflows/x.yml" <<'YML'
jobs:
  check:
    steps:
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with:
          toolchain:
          # chosen below

            channel: stable
YML
cp "$fx/tc16/.github/workflows/x.yml" "$fx/tc16-before.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc16"
check "a nested toolchain mapping behind a comment line is unsupported: exit 2, file untouched" \
  "[ \$rc -eq 2 ] && grep -q UNSUPPORTED <<<\"\$out\" && cmp -s '$fx/tc16-before.yml' '$fx/tc16/.github/workflows/x.yml'"

# tc17: a multi-line flow `with:` whose empty entry has its delimiter on the
# next line — `toolchain: null` then `}`, or a bare `toolchain:` followed by
# the closing brace — is filled where it stands; a bare key whose next token
# is a value (spread over lines) is not this fixer's to place.
mkdir -p "$fx/tc17/.github/workflows" "$fx/tc17b/.github/workflows"
cat > "$fx/tc17/.github/workflows/x.yml" <<'YML'
jobs:
  check:
    steps:
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: {
          toolchain: null
        }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: {
          components: clippy,
          toolchain:
        }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: { toolchain: ~
        }
YML
cat > "$fx/tc17-expected.yml" <<'YML'
jobs:
  check:
    steps:
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: {
          toolchain: stable
        }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: {
          components: clippy,
          toolchain: stable
        }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: { toolchain: stable
        }
YML
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc17"
check "an empty flow entry whose delimiter is on the next line is filled in place, never duplicated" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/tc17-expected.yml' '$fx/tc17/.github/workflows/x.yml'"
cat > "$fx/tc17b/.github/workflows/x.yml" <<'YML'
jobs:
  check:
    steps:
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: {
          toolchain:
            nightly }
YML
cp "$fx/tc17b/.github/workflows/x.yml" "$fx/tc17b-before.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc17b"
check "a bare flow toolchain: whose value starts on the next line is unsupported: exit 2, file untouched" \
  "[ \$rc -eq 2 ] && grep -q UNSUPPORTED <<<\"\$out\" && cmp -s '$fx/tc17b-before.yml' '$fx/tc17b/.github/workflows/x.yml'"

# tc18: only a pin the parser cannot place AS A STEP is unsupported. A
# workflow-level `env:` string naming the action is data: the step below it
# is still repaired and the run is clean. A key of a flow-style step spread
# over several lines IS a step the parser cannot place.
mkdir -p "$fx/tc18/.github/workflows" "$fx/tc18b/.github/workflows"
cat > "$fx/tc18/.github/workflows/x.yml" <<'YML'
env:
  TOOLCHAIN_ACTION: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
jobs:
  check:
    steps:
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
YML
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc18"
check "a workflow-level env string naming the pin is data: the step is repaired and the run is clean" \
  "[ \$rc -eq 0 ] && ! grep -q UNSUPPORTED <<<\"\$out\" && grep -q '^  TOOLCHAIN_ACTION: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772$' '$fx/tc18/.github/workflows/x.yml' && [ \$(grep -c 'toolchain: stable' '$fx/tc18/.github/workflows/x.yml') -eq 1 ]"
cat > "$fx/tc18b/.github/workflows/x.yml" <<'YML'
jobs:
  check:
    steps: [
      { name: toolchain,
        uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772 }
    ]
YML
cp "$fx/tc18b/.github/workflows/x.yml" "$fx/tc18b-before.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc18b"
check "a key of a flow-style step spread over lines is still unsupported: exit 2, file untouched" \
  "[ \$rc -eq 2 ] && grep -q UNSUPPORTED <<<\"\$out\" && cmp -s '$fx/tc18b-before.yml' '$fx/tc18b/.github/workflows/x.yml'"

# ls12: only a DIRECT key of a step is its `uses:`; the same key nested under
# `with:` or `env:` is an input and names no action. An estate record such
# an input names is then unreferenced and keeps its SHA ref (the reusable
# fallback) — as an action site it would have been given the tag. A
# bare-dash item learns its key column from its first key line.
mkdir -p "$fx/ls12/.github/workflows"
cat > "$fx/ls12/.github/workflows/ci.yml" <<YML
jobs:
  check:
    steps:
      - name: run
        uses: foo/bar@$SHA_A
        with:
          uses: hyperpolymath/standards/.github/workflows/x.yml@$SHA_B
      -
        uses: foo/bar@$SHA_C
YML
cat > "$fx/ls12/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'foo/bar@$SHA_A'
        - 'hyperpolymath/standards@$SHA_B'
        - 'foo/bar@$SHA_C'
dependencies:
    'foo/bar@$SHA_A':
        ref: '$SHA_A'
        commit: 'sha1-$SHA_A'
        owner_id: 1
        repo_id: 2
    'hyperpolymath/standards@$SHA_B':
        ref: '$SHA_B'
        commit: 'sha1-$SHA_B'
        owner_id: 1
        repo_id: 2
    'foo/bar@$SHA_C':
        ref: '$SHA_C'
        commit: 'sha1-$SHA_C'
        owner_id: 1
        repo_id: 2
LOCK
printf 'foo/bar\t%s\tv1.0.0\nhyperpolymath/standards\t%s\tv2.0.0\nfoo/bar\t%s\tv3.0.0\n' "$SHA_A" "$SHA_B" "$SHA_C" > "$fx/ls12-tags.tsv"
TAG_MAP="$fx/ls12-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls12" --offline
check "a step's direct uses: (under - name: and under a bare dash) is an action site; a uses: nested under with: is not" \
  "grep -q \"^        ref: 'v1.0.0'\" '$fx/ls12/.github/workflows/actions.lock' && grep -q \"^        ref: 'v3.0.0'\" '$fx/ls12/.github/workflows/actions.lock' && grep -q \"^        ref: '$SHA_B'\" '$fx/ls12/.github/workflows/actions.lock'"

# ls13: the whole `jobs:` mapping written on one line is still a job map:
# its `uses:` entries are reusable calls, whose records keep their SHA ref.
# Two steps on one `steps: [ ... ]` line are each an action site.
mkdir -p "$fx/ls13/.github/workflows"
cat > "$fx/ls13/.github/workflows/ci.yml" <<YML
jobs: { call: { uses: hyperpolymath/standards/.github/workflows/x.yml@$SHA_A }, other: { uses: hyperpolymath/standards/.github/workflows/y.yml@$SHA_A } }
YML
cat > "$fx/ls13/.github/workflows/two.yml" <<YML
jobs:
  check:
    steps: [ { uses: foo/bar@$SHA_B }, { uses: foo/baz@$SHA_C } ]
YML
cat > "$fx/ls13/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'hyperpolymath/standards@$SHA_A'
    '.github/workflows/two.yml':
        - 'foo/bar@$SHA_B'
        - 'foo/baz@$SHA_C'
dependencies:
    'hyperpolymath/standards@$SHA_A':
        ref: '$SHA_A'
        commit: 'sha1-$SHA_A'
        owner_id: 1
        repo_id: 2
    'foo/bar@$SHA_B':
        ref: '$SHA_B'
        commit: 'sha1-$SHA_B'
        owner_id: 1
        repo_id: 2
    'foo/baz@$SHA_C':
        ref: '$SHA_C'
        commit: 'sha1-$SHA_C'
        owner_id: 1
        repo_id: 2
LOCK
printf 'hyperpolymath/standards\t%s\tv9.9.9\nfoo/bar\t%s\tv2.0.0\nfoo/baz\t%s\tv3.0.0\n' "$SHA_A" "$SHA_B" "$SHA_C" > "$fx/ls13-tags.tsv"
TAG_MAP="$fx/ls13-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls13" --offline
check "an inline jobs: { ... } mapping is a reusable call: its record keeps the SHA ref; two inline steps are both actions" \
  "[ \$rc -eq 0 ] && grep -q \"^        ref: '$SHA_A'\" '$fx/ls13/.github/workflows/actions.lock' && grep -q \"^        ref: 'v2.0.0'\" '$fx/ls13/.github/workflows/actions.lock' && grep -q \"^        ref: 'v3.0.0'\" '$fx/ls13/.github/workflows/actions.lock'"

# sp17: an upper-case SHA is the same dead pin: looked up in lower case, the
# caller rewritten as written, the (lower-case) lock edge moved.
mkdir -p "$fx/sp17/.github/workflows"
printf 'jobs:\n  rust:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "${STALE_A^^}" > "$fx/sp17/.github/workflows/ci.yml"
cat > "$fx/sp17/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'hyperpolymath/standards@$STALE_A'
dependencies:
    'hyperpolymath/standards@$STALE_A':
        ref: '$STALE_A'
        commit: 'sha1-$STALE_A'
        owner_id: 1
        repo_id: 2
LOCK
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp17"
check "an upper-case dead pin is rewritten and its lock edge and record moved (exit 3: the record's nested list is not recomputed)" \
  "[ \$rc -eq 3 ] && grep -q \"rust-ci-reusable.yml@$GOOD\" '$fx/sp17/.github/workflows/ci.yml' && grep -q \"'hyperpolymath/standards@$GOOD'\" '$fx/sp17/.github/workflows/actions.lock' && ! grep -qi \"$STALE_A\" '$fx/sp17/.github/workflows/actions.lock'"

# sp18: the only wildcard a table row may carry is a terminal `/*`; a `*`
# anywhere else is rejected before any caller is touched.
mkdir -p "$fx/sp18/.github/workflows"
printf 'jobs:\n  rust:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$STALE_A" > "$fx/sp18/.github/workflows/ci.yml"
cp "$fx/sp18/.github/workflows/ci.yml" "$fx/sp18-before.yml"
printf 'hyperpolymath/standards/.github/workflows/rust-*.yml\t%s\t%s\tglob in the file name\n' "$STALE_A" "$GOOD" > "$fx/sp18-table.tsv"
PINS_TABLE="$fx/sp18-table.tsv" run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp18"
check "a wildcard anywhere but a terminal /* is a bad row: exit 2, nothing changed" \
  "[ \$rc -eq 2 ] && grep -q 'terminal /\\*' <<<\"\$out\" && cmp -s '$fx/sp18-before.yml' '$fx/sp18/.github/workflows/ci.yml'"

# sp19: a hand-edited lock whose workflow key is double-quoted is the same
# key: the edge is found and moved, not reported as missing.
mkdir -p "$fx/sp19/.github/workflows"
printf 'jobs:\n  rust:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$STALE_A" > "$fx/sp19/.github/workflows/ci.yml"
cat > "$fx/sp19/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    ".github/workflows/ci.yml":
        - "hyperpolymath/standards@$STALE_A"
dependencies:
    "hyperpolymath/standards@$STALE_A":
        ref: "$STALE_A"
        commit: "sha1-$STALE_A"
        owner_id: 1
        repo_id: 2
LOCK
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp19"
check "a double-quoted workflow key is matched: the edge moves, no 'has no edge' warning (exit 3 for the renamed record, as always)" \
  "[ \$rc -eq 3 ] && ! grep -q 'has no edge' <<<\"\$out\" && grep -q \"\\\"hyperpolymath/standards@$GOOD\\\"\" '$fx/sp19/.github/workflows/actions.lock'"

# sp20: a partly migrated workflow — one job at the dead pin, another already
# at the replacement — lists both edges; moving the dead one must not write
# the replacement edge a second time.
mkdir -p "$fx/sp20/.github/workflows"
printf 'jobs:\n  rust:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n  done:\n    uses: hyperpolymath/standards/.github/workflows/mirror-reusable.yml@%s\n' "$STALE_A" "$GOOD" > "$fx/sp20/.github/workflows/ci.yml"
cat > "$fx/sp20/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'hyperpolymath/standards@$STALE_A'
        - 'hyperpolymath/standards@$GOOD'
dependencies:
    'hyperpolymath/standards@$STALE_A':
        ref: '$STALE_A'
        commit: 'sha1-$STALE_A'
        owner_id: 1
        repo_id: 2
    'hyperpolymath/standards@$GOOD':
        ref: '$GOOD'
        commit: 'sha1-$GOOD'
        owner_id: 1
        repo_id: 2
LOCK
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp20"
check "an edge the workflow entry already lists is not added again: exactly one replacement edge, no stale edge" \
  "[ \$rc -eq 3 ] && [ \$(grep -c \"^        - 'hyperpolymath/standards@$GOOD'\" '$fx/sp20/.github/workflows/actions.lock') -eq 1 ] && ! grep -q \"standards@$STALE_A\" '$fx/sp20/.github/workflows/actions.lock'"

# pl1: one workflow-pin fixer per checkout at a time. While another holder
# has the repository's lock, a fixer waits PIN_FIXER_LOCK_WAIT seconds, then
# reports it and exits 2 with nothing changed; once the holder is gone the
# same run succeeds.
mkdir -p "$fx/pl1/.github/workflows"
printf 'jobs:\n  rust:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$STALE_A" > "$fx/pl1/.github/workflows/ci.yml"
cp "$fx/pl1/.github/workflows/ci.yml" "$fx/pl1-before.yml"
pl1_key=$(cd "$fx/pl1" && pwd -P | tr -d '\n' | sha256sum | cut -c1-16)
flock -x "$fx/pin-fixer-$pl1_key.lock" sleep 3 &
pl1_holder=$!
sleep 0.3
PIN_FIXER_LOCK_DIR="$fx" PIN_FIXER_LOCK_WAIT=1 run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/pl1"
check "a fixer that cannot take the checkout's lock in time reports it, exits 2 and changes nothing" \
  "[ \$rc -eq 2 ] && grep -q 'another workflow-pin fixer holds' <<<\"\$out\" && cmp -s '$fx/pl1-before.yml' '$fx/pl1/.github/workflows/ci.yml'"
wait "$pl1_holder" || true
PIN_FIXER_LOCK_DIR="$fx" run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/pl1"
check "once the holder is gone the same run takes the lock and repairs" \
  "[ \$rc -eq 0 ] && grep -q \"rust-ci-reusable.yml@$GOOD\" '$fx/pl1/.github/workflows/ci.yml'"


# ── Review cases (gitbot-fleet#615, round 12) ───────────────────────────────
# tc19: an upper-case SHA that begins with a letter is still the step's own
# `uses:` key (USES_RE accepts the same hex as sha_ref), so it is repaired,
# not reported as a pin the parser cannot place.
mkdir -p "$fx/tc19/.github/workflows"
cat > "$fx/tc19/.github/workflows/x.yml" <<'YML'
jobs:
  check:
    steps:
      - uses: dtolnay/rust-toolchain@AC977A6CA4077A0CEB28FFBE03F59D46E9AC8772
        with:
          components: clippy
YML
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc19"
check "an upper-case SHA beginning with a letter is the step's uses: key: repaired, not unsupported" \
  "[ \$rc -eq 0 ] && ! grep -q UNSUPPORTED <<<\"\$out\" && grep -A2 'AC977A6CA4077A0CEB28FFBE03F59D46E9AC8772' '$fx/tc19/.github/workflows/x.yml' | grep -q '^          toolchain: stable$'"

# pl2: a single workflow file named relative to the checkout root locks the
# checkout, not the file: the lock file is the one every fixer run on that
# checkout computes.
mkdir -p "$fx/pl2/.github/workflows"
printf 'jobs:\n  check:\n    steps:\n      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772\n' > "$fx/pl2/.github/workflows/x.yml"
pl2_key=$(cd "$fx/pl2" && pwd -P | tr -d '\n' | sha256sum | cut -c1-16)
(cd "$fx/pl2" && PIN_FIXER_LOCK_DIR="$fx/pl2-locks" bash -c 'mkdir -p "$PIN_FIXER_LOCK_DIR"; bash "$1" .github/workflows/x.yml' _ "$fleet/scripts/fix-rust-toolchain-input.sh") > "$fx/pl2.out" 2>&1; rc_pl2=$?
check "a relative workflow-file target is locked by its checkout (same lock file as a checkout-path run)" \
  "[ \$rc_pl2 -eq 0 ] && [ -f '$fx/pl2-locks/pin-fixer-$pl2_key.lock' ] && grep -q 'toolchain: stable' '$fx/pl2/.github/workflows/x.yml'"

# ls14: a flow mapping on one of a step's own keys (`with: { uses: ... }`) is
# that input's data, not the step's action: the estate record it names stays
# unreferenced and keeps its SHA ref.
mkdir -p "$fx/ls14/.github/workflows"
cat > "$fx/ls14/.github/workflows/ci.yml" <<YML
jobs:
  check:
    steps:
      - uses: foo/bar@$SHA_A
        with: { uses: hyperpolymath/standards/.github/workflows/x.yml@$SHA_B }
YML
cat > "$fx/ls14/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'foo/bar@$SHA_A'
        - 'hyperpolymath/standards@$SHA_B'
dependencies:
    'foo/bar@$SHA_A':
        ref: '$SHA_A'
        commit: 'sha1-$SHA_A'
        owner_id: 1
        repo_id: 2
    'hyperpolymath/standards@$SHA_B':
        ref: '$SHA_B'
        commit: 'sha1-$SHA_B'
        owner_id: 1
        repo_id: 2
LOCK
printf 'foo/bar\t%s\tv1.0.0\nhyperpolymath/standards\t%s\tv2.0.0\n' "$SHA_A" "$SHA_B" > "$fx/ls14-tags.tsv"
TAG_MAP="$fx/ls14-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls14" --offline
check "a with: { uses: ... } flow input on a step key is data: the step's action is repaired, the estate record keeps its SHA ref" \
  "grep -q \"^        ref: 'v1.0.0'\" '$fx/ls14/.github/workflows/actions.lock' && grep -q \"^        ref: '$SHA_B'\" '$fx/ls14/.github/workflows/actions.lock'"

# Review cases (Codex round on 5fa7192, gitbot-fleet#615).

# tc20: a continuation line of a multi-line flow `with:` is read at the
# mapping's own level even when it STARTS inside a nested mapping: the text
# after the brace that closes the nested group (`channel: beta }, toolchain:
# nightly`) is an entry of `with:` — a toolchain that must count, or an
# empty one that is filled where it stands — while `toolchain:` inside the
# nested group is not.
mkdir -p "$fx/tc20/.github/workflows"
cat > "$fx/tc20/.github/workflows/x.yml" <<'YML'
jobs:
  check:
    steps:
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: {
          env: {
            channel: beta }, toolchain: nightly
        }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: {
          env: {
            channel: beta }, toolchain: null }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: {
          env: {
            toolchain: nightly }, components: clippy
        }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: {
          env: { channel: "}" }, # toolchain: nightly
          toolchain: ${{ matrix.rust }}
        }
YML
cat > "$fx/tc20-expected.yml" <<'YML'
jobs:
  check:
    steps:
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: {
          env: {
            channel: beta }, toolchain: nightly
        }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: {
          env: {
            channel: beta }, toolchain: stable }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: { toolchain: stable,
          env: {
            toolchain: nightly }, components: clippy
        }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with: {
          env: { channel: "}" }, # toolchain: nightly
          toolchain: ${{ matrix.rust }}
        }
YML
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc20"
check "tc20 an entry after a nested mapping closes mid-line counts (kept, or filled in place); one inside it does not" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/tc20-expected.yml' '$fx/tc20/.github/workflows/x.yml'"
cp "$fx/tc20/.github/workflows/x.yml" "$fx/tc20-after1.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc20"
check "tc20 second run is a no-op" "[ \$rc -eq 0 ] && cmp -s '$fx/tc20-after1.yml' '$fx/tc20/.github/workflows/x.yml'"

# sp21: a table whose last row has no trailing newline is read whole — the
# row is matched and, when it is malformed, rejected — never skipped.
mkdir -p "$fx/sp21/.github/workflows"
printf 'jobs:\n  rust:\n    uses: octo/example/.github/workflows/ci.yml@%s\n' "$STALE_A" > "$fx/sp21/.github/workflows/ci.yml"
printf '# a private table\nocto/example/.github/workflows/*\t%s\t%s\tdead window' "$STALE_A" "$GOOD" > "$fx/sp21-table.tsv"
PINS_TABLE="$fx/sp21-table.tsv" run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp21"
check "sp21 a final table row without a newline is a row: its owner is scanned and its dead pin moved" \
  "[ \$rc -eq 0 ] && grep -q \"ci.yml@$GOOD\$\" '$fx/sp21/.github/workflows/ci.yml' && grep -q '^FIXED' <<<\"\$out\""
printf 'jobs:\n  rust:\n    uses: octo/example/.github/workflows/ci.yml@%s\n' "$STALE_A" > "$fx/sp21/.github/workflows/ci.yml"
printf 'octo/example/.github/workflows/*\t%s\tnot-a-sha\tbad row' "$STALE_A" > "$fx/sp21-bad.tsv"
PINS_TABLE="$fx/sp21-bad.tsv" run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp21"
check "sp21 a malformed final row without a newline is still validated: exit 2, nothing changed" \
  "[ \$rc -eq 2 ] && grep -q 'bad replacement SHA' <<<\"\$out\" && grep -q \"ci.yml@$STALE_A\$\" '$fx/sp21/.github/workflows/ci.yml'"

# sp22: a hand-written lock may leave its keys and edges unquoted. A plain
# record (`actions/checkout@<sha>:`) written before the quoted ones used to
# fall through the merger with no current record and vanish from
# `dependencies:` on any repair; it is a record like the others, and a plain
# edge or record of the dead pin itself moves like a quoted one.
mkdir -p "$fx/sp22/.github/workflows"
printf 'jobs:\n  rust:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$STALE_A" > "$fx/sp22/.github/workflows/ci.yml"
cat > "$fx/sp22/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - hyperpolymath/standards@$STALE_A
        - actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1
dependencies:
    actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1:
        ref: v7.0.1
        commit: sha1-3d3c42e5aac5ba805825da76410c181273ba90b1
        owner_id: 1
        repo_id: 2
    hyperpolymath/standards@$STALE_A:
        ref: $STALE_A
        commit: sha1-$STALE_A
        owner_id: 6759885
        repo_id: 1116521501
        uses:
            - actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1
LOCK
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp22"
check "sp22 an unquoted record before the quoted ones survives a repair, ref and all" \
  "[ \$rc -eq 3 ] && ! grep -q 'has no edge' <<<\"\$out\" && grep -q '^    actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1:$' '$fx/sp22/.github/workflows/actions.lock' && grep -q '^        ref: v7.0.1$' '$fx/sp22/.github/workflows/actions.lock' && grep -q '^        commit: sha1-3d3c42e5aac5ba805825da76410c181273ba90b1$' '$fx/sp22/.github/workflows/actions.lock'"
check "sp22 an unquoted edge and record of the dead pin move; the nested unquoted uses: entry is kept" \
  "grep -q \"^        - hyperpolymath/standards@$GOOD\$\" '$fx/sp22/.github/workflows/actions.lock' && grep -q \"^    hyperpolymath/standards@$GOOD:\$\" '$fx/sp22/.github/workflows/actions.lock' && grep -q \"^        ref: $GOOD\$\" '$fx/sp22/.github/workflows/actions.lock' && grep -q '^            - actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1$' '$fx/sp22/.github/workflows/actions.lock' && ! grep -q \"$STALE_A\" '$fx/sp22/.github/workflows/actions.lock' && [ \$(wc -l < '$fx/sp22/.github/workflows/actions.lock') -eq 18 ]"

# ls15: a `{ uses: ... }` written inside a job's `name:` string, or after a
# `#`, on a one-line `jobs:` mapping is text, not a reusable call: it must
# not give a repository used as an action elsewhere the reusable precedence
# that would keep its self-SHA record.
mkdir -p "$fx/ls15/.github/workflows"
cat > "$fx/ls15/.github/workflows/ci.yml" <<YML
jobs: { call: { uses: octo/example/.github/workflows/ci.yml@$SHA_B, with: { name: "{ uses: foo/bar/.github/workflows/x.yml@$SHA_A }" } } } # { uses: foo/baz/.github/workflows/y.yml@$SHA_C }
YML
cat > "$fx/ls15/.github/workflows/two.yml" <<YML
jobs:
  check:
    steps:
      - { name: 'see { uses: foo/bar/.github/workflows/z.yml@$SHA_A }', uses: foo/bar@$SHA_A }
      - uses: foo/baz@$SHA_C # not { uses: foo/baz/.github/workflows/y.yml@$SHA_C }
YML
cat > "$fx/ls15/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'octo/example@$SHA_B'
    '.github/workflows/two.yml':
        - 'foo/bar@$SHA_A'
        - 'foo/baz@$SHA_C'
dependencies:
    'octo/example@$SHA_B':
        ref: '$SHA_B'
        commit: 'sha1-$SHA_B'
        owner_id: 1
        repo_id: 2
    'foo/bar@$SHA_A':
        ref: '$SHA_A'
        commit: 'sha1-$SHA_A'
        owner_id: 1
        repo_id: 2
    'foo/baz@$SHA_C':
        ref: '$SHA_C'
        commit: 'sha1-$SHA_C'
        owner_id: 1
        repo_id: 2
LOCK
printf 'foo/bar\t%s\tv1.0.0\nocto/example\t%s\tv9.9.9\nfoo/baz\t%s\tv3.0.0\n' "$SHA_A" "$SHA_B" "$SHA_C" > "$fx/ls15-tags.tsv"
TAG_MAP="$fx/ls15-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls15" --offline
check "ls15 a uses: inside a quoted string or a comment names no site: the two action records are repaired, the real reusable call keeps its SHA" \
  "[ \$rc -eq 0 ] && grep -q \"^        ref: 'v1.0.0'\" '$fx/ls15/.github/workflows/actions.lock' && grep -q \"^        ref: 'v3.0.0'\" '$fx/ls15/.github/workflows/actions.lock' && grep -q \"^        ref: '$SHA_B'\" '$fx/ls15/.github/workflows/actions.lock' && [ \$(grep -c '^FIXED' <<<\"\$out\") -eq 2 ]"

# ls16: a hand-added record may spell its commit ID in upper-case hex, in
# its key, its ref, or the workflow's pin — git resolves it the same. The
# record is found, looked up in lower case, and its ref rewritten as the
# ref line spells it; a plain (unquoted) key is a record too.
mkdir -p "$fx/ls16/.github/workflows"
printf 'jobs:\n  check:\n    steps:\n      - uses: foo/bar@%s\n      - uses: foo/baz@%s\n' "${SHA_A^^}" "$SHA_B" > "$fx/ls16/.github/workflows/ci.yml"
cat > "$fx/ls16/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'foo/bar@${SHA_A^^}'
        - 'foo/baz@$SHA_B'
dependencies:
    'foo/bar@${SHA_A^^}':
        ref: '${SHA_A^^}'
        commit: 'sha1-$SHA_A'
        owner_id: 1
        repo_id: 2
    foo/baz@${SHA_B^^}:
        ref: $SHA_B
        commit: sha1-$SHA_B
        owner_id: 1
        repo_id: 2
LOCK
printf 'foo/bar\t%s\tv1.0.0\nfoo/baz\t%s\tv2.0.0\n' "$SHA_A" "$SHA_B" > "$fx/ls16-tags.tsv"
TAG_MAP="$fx/ls16-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls16" --offline
check "ls16 an upper-case self-SHA record (quoted) and a plain-keyed one whose key and ref differ in case are both found and repaired" \
  "[ \$rc -eq 0 ] && grep -q \"^        ref: 'v1.0.0'\$\" '$fx/ls16/.github/workflows/actions.lock' && grep -q '^        ref: v2.0.0\$' '$fx/ls16/.github/workflows/actions.lock' && ! grep -q 'no self-SHA' <<<\"\$out\" && [ \$(grep -c '^FIXED' <<<\"\$out\") -eq 2 ]"

# ls17: a repository name is compared as a name, not as a pattern: the `.`
# of `foo/action.test` is a dot, so a reusable call of `foo/actionXtest` at
# the same SHA is not a reusable use of the action, whose self-SHA record is
# repaired.
mkdir -p "$fx/ls17/.github/workflows"
cat > "$fx/ls17/.github/workflows/ci.yml" <<YML
jobs:
  call:
    uses: foo/actionXtest/.github/workflows/x.yml@$SHA_A
  check:
    steps:
      - uses: foo/action.test@$SHA_A
YML
cat > "$fx/ls17/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'foo/actionXtest@$SHA_A'
        - 'foo/action.test@$SHA_A'
dependencies:
    'foo/actionXtest@$SHA_A':
        ref: '$SHA_A'
        commit: 'sha1-$SHA_A'
        owner_id: 1
        repo_id: 2
    'foo/action.test@$SHA_A':
        ref: '$SHA_A'
        commit: 'sha1-$SHA_A'
        owner_id: 1
        repo_id: 3
LOCK
printf 'foo/action.test\t%s\tv1.0.0\nfoo/actionXtest\t%s\tv2.0.0\n' "$SHA_A" "$SHA_A" > "$fx/ls17-tags.tsv"
TAG_MAP="$fx/ls17-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls17" --offline
check "ls17 a dot in a repository name is a dot: the action's record is repaired, the reusable at the same SHA keeps its SHA ref" \
  "[ \$rc -eq 0 ] && grep -A1 \"^    'foo/action.test@$SHA_A':\$\" '$fx/ls17/.github/workflows/actions.lock' | grep -q \"ref: 'v1.0.0'\" && grep -A1 \"^    'foo/actionXtest@$SHA_A':\$\" '$fx/ls17/.github/workflows/actions.lock' | grep -q \"ref: '$SHA_A'\""

# Review cases (Codex round on 709c153, gitbot-fleet#615).

# pl3: the lock file is opened only inside a directory private to this user.
# By default that is a `pin-fixer-<uid>` directory created 0700 under the
# runtime/temp base, so a symlink planted at the old predictable name in a
# shared, world-writable directory is never followed. A lock directory that
# is group/world-writable or a symlink is refused: exit 2, nothing changed.
mkdir -p "$fx/pl3/.github/workflows" "$fx/pl3-shared" "$fx/pl3-private"
chmod 1777 "$fx/pl3-shared"; chmod 700 "$fx/pl3-private"
printf 'jobs:\n  rust:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$STALE_A" > "$fx/pl3/.github/workflows/ci.yml"
cp "$fx/pl3/.github/workflows/ci.yml" "$fx/pl3-before.yml"
pl3_key=$(cd "$fx/pl3" && pwd -P | tr -d '\n' | sha256sum | cut -c1-16)
printf 'keep\n' > "$fx/pl3-victim"
ln -s "$fx/pl3-victim" "$fx/pl3-shared/pin-fixer-$pl3_key.lock"
PIN_FIXER_LOCK_DIR="$fx/pl3-shared" run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/pl3"
check "pl3 a world-writable lock directory is refused: exit 2, nothing changed, the planted link not followed" \
  "[ \$rc -eq 2 ] && grep -q 'not a private directory' <<<\"\$out\" && cmp -s '$fx/pl3-before.yml' '$fx/pl3/.github/workflows/ci.yml' && [ \"\$(cat '$fx/pl3-victim')\" = keep ]"
ln -s "$fx/pl3-private" "$fx/pl3-link"
PIN_FIXER_LOCK_DIR="$fx/pl3-link" run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/pl3"
check "pl3 a symlinked lock directory is refused: exit 2, nothing changed" \
  "[ \$rc -eq 2 ] && grep -q 'not a private directory' <<<\"\$out\" && cmp -s '$fx/pl3-before.yml' '$fx/pl3/.github/workflows/ci.yml'"
rc=0; out=$(env -u PIN_FIXER_LOCK_DIR -u XDG_RUNTIME_DIR TMPDIR="$fx/pl3-shared" bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/pl3" 2>&1) || rc=$?
pl3_dir="$fx/pl3-shared/pin-fixer-$(id -u)"
check "pl3 by default the lock sits in a 0700 per-user directory; the shared name is never opened" \
  "[ \$rc -eq 0 ] && [ -f '$pl3_dir/pin-fixer-$pl3_key.lock' ] && [ -z \"\$(find '$pl3_dir' -maxdepth 0 \\( -perm -0020 -o -perm -0002 -o -perm -0040 -o -perm -0004 \\) -print)\" ] && [ \"\$(cat '$fx/pl3-victim')\" = keep ] && grep -q \"rust-ci-reusable.yml@$GOOD\" '$fx/pl3/.github/workflows/ci.yml'"

# sp23: a dead pin named only in a trailing comment or a quoted scalar is
# data: no caller to repair and no UNSUPPORTED exit. The real caller beside
# it is moved; a flow-style job carrying the dead pin as a `uses:` key is
# still a caller the parser cannot place.
mkdir -p "$fx/sp23/.github/workflows" "$fx/sp23b/.github/workflows"
cat > "$fx/sp23/.github/workflows/ci.yml" <<YML
name: test # hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@$STALE_A
env:
  OLD: "{ uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@$STALE_A }"
jobs:
  rust:
    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@$STALE_A
YML
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp23"
check "sp23 a dead pin in a comment or a quoted scalar is text: the caller moves, the text stays, exit 0" \
  "[ \$rc -eq 0 ] && ! grep -q UNSUPPORTED <<<\"\$out\" && sed -n '6p' '$fx/sp23/.github/workflows/ci.yml' | grep -q \"@$GOOD\$\" && [ \$(grep -c \"$STALE_A\" '$fx/sp23/.github/workflows/ci.yml') -eq 2 ]"
printf 'jobs:\n  rust: { uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s }\n' "$STALE_A" > "$fx/sp23b/.github/workflows/ci.yml"
cp "$fx/sp23b/.github/workflows/ci.yml" "$fx/sp23b-before.yml"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp23b"
check "sp23 a flow-style job whose uses: key carries the dead pin is still UNSUPPORTED: exit 2, untouched" \
  "[ \$rc -eq 2 ] && grep -q UNSUPPORTED <<<\"\$out\" && cmp -s '$fx/sp23b-before.yml' '$fx/sp23b/.github/workflows/ci.yml'"

# sp24: GitHub reads an owner or repository name in either case. A caller
# spelled `Hyperpolymath/Standards/...` meets the table's row, is rewritten
# with its own spelling kept, and finds its (lower-case) lock edge and record.
mkdir -p "$fx/sp24/.github/workflows"
printf 'jobs:\n  rust:\n    uses: Hyperpolymath/Standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$STALE_A" > "$fx/sp24/.github/workflows/ci.yml"
cat > "$fx/sp24/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'hyperpolymath/standards@$STALE_A'
dependencies:
    'hyperpolymath/standards@$STALE_A':
        ref: '$STALE_A'
        commit: 'sha1-$STALE_A'
        owner_id: 1
        repo_id: 2
LOCK
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp24"
check "sp24 a mixed-case caller is the table's pin: rewritten with its spelling, its lower-case edge and record moved" \
  "[ \$rc -eq 3 ] && ! grep -q 'has no edge' <<<\"\$out\" && grep -q \"^    uses: Hyperpolymath/Standards/.github/workflows/rust-ci-reusable.yml@$GOOD\$\" '$fx/sp24/.github/workflows/ci.yml' && grep -q \"^        - 'hyperpolymath/standards@$GOOD'\$\" '$fx/sp24/.github/workflows/actions.lock' && grep -q \"^    'hyperpolymath/standards@$GOOD':\$\" '$fx/sp24/.github/workflows/actions.lock' && ! grep -q \"$STALE_A\" '$fx/sp24/.github/workflows/actions.lock'"

# sp25: a nested `uses:` entry with a trailing comment, and a comment line
# inside the list, stay under their `uses:` header through the merge. Before,
# the header was dropped and the entry kept: a child with no parent key.
mkdir -p "$fx/sp25/.github/workflows"
printf 'jobs:\n  rust:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$STALE_A" > "$fx/sp25/.github/workflows/ci.yml"
cat > "$fx/sp25/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'hyperpolymath/standards@$STALE_A'
dependencies:
    'actions/cache@0057852bfaa89a56745cba8c7296529d2fc39830':
        ref: 'v4.3.0'
        commit: 'sha1-0057852bfaa89a56745cba8c7296529d2fc39830'
        owner_id: 1
        repo_id: 3
        uses:
            - 'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1' # nested by the action
            # resolved 2026-10-19
    'hyperpolymath/standards@$STALE_A':
        ref: '$STALE_A'
        commit: 'sha1-$STALE_A'
        owner_id: 1
        repo_id: 2
LOCK
cat > "$fx/sp25-expected.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'hyperpolymath/standards@$GOOD'
dependencies:
    'actions/cache@0057852bfaa89a56745cba8c7296529d2fc39830':
        ref: 'v4.3.0'
        commit: 'sha1-0057852bfaa89a56745cba8c7296529d2fc39830'
        owner_id: 1
        repo_id: 3
        uses:
            - 'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1' # nested by the action
            # resolved 2026-10-19
    'hyperpolymath/standards@$GOOD':
        ref: '$GOOD'
        commit: 'sha1-$GOOD'
        owner_id: 1
        repo_id: 2
LOCK
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp25"
check "sp25 a commented nested entry and a comment line stay under their uses: header through the merge" \
  "[ \$rc -eq 3 ] && cmp -s '$fx/sp25-expected.lock' '$fx/sp25/.github/workflows/actions.lock'"

# ls18: a step inside a one-line `jobs:` map is an action, not a reusable
# call. `jobs: { build: { steps: [ { uses: hyperpolymath/standards@<sha> } ] } }`
# used to classify every uses: on the line as R, so the estate action record
# kept its self-SHA ref; the job-level reusable call beside it still keeps
# its SHA.
mkdir -p "$fx/ls18/.github/workflows"
cat > "$fx/ls18/.github/workflows/ci.yml" <<YML
jobs: { build: { steps: [ { uses: hyperpolymath/standards@$SHA_A } ] }, call: { uses: hyperpolymath/cicd-suite/.github/workflows/x.yml@$SHA_B } }
YML
cat > "$fx/ls18/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'hyperpolymath/standards@$SHA_A'
        - 'hyperpolymath/cicd-suite@$SHA_B'
dependencies:
    'hyperpolymath/standards@$SHA_A':
        ref: '$SHA_A'
        commit: 'sha1-$SHA_A'
        owner_id: 1
        repo_id: 2
    'hyperpolymath/cicd-suite@$SHA_B':
        ref: '$SHA_B'
        commit: 'sha1-$SHA_B'
        owner_id: 1
        repo_id: 3
LOCK
printf 'hyperpolymath/standards\t%s\tv1.0.0\nhyperpolymath/cicd-suite\t%s\tv2.0.0\n' "$SHA_A" "$SHA_B" > "$fx/ls18-tags.tsv"
TAG_MAP="$fx/ls18-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls18" --offline
check "ls18 a step in a one-line jobs map is an action (tag written); the job-level reusable call keeps its SHA" \
  "[ \$rc -eq 0 ] && grep -A1 \"^    'hyperpolymath/standards@$SHA_A':\$\" '$fx/ls18/.github/workflows/actions.lock' | grep -q \"ref: 'v1.0.0'\" && grep -A1 \"^    'hyperpolymath/cicd-suite@$SHA_B':\$\" '$fx/ls18/.github/workflows/actions.lock' | grep -q \"ref: '$SHA_B'\""

# Review cases (Codex round on c7392ee, gitbot-fleet#615).

# sp26: an entry that already lists the replacement edge WITH a trailing
# comment is the same edge: moving the stale one must not add it again.
mkdir -p "$fx/sp26/.github/workflows"
printf 'jobs:\n  rust:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n  done:\n    uses: hyperpolymath/standards/.github/workflows/mirror-reusable.yml@%s\n' "$STALE_A" "$GOOD" > "$fx/sp26/.github/workflows/ci.yml"
cat > "$fx/sp26/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'hyperpolymath/standards@$STALE_A'
        - 'hyperpolymath/standards@$GOOD' # mirror already moved
dependencies:
    'hyperpolymath/standards@$STALE_A':
        ref: '$STALE_A'
        commit: 'sha1-$STALE_A'
        owner_id: 1
        repo_id: 2
    'hyperpolymath/standards@$GOOD':
        ref: '$GOOD'
        commit: 'sha1-$GOOD'
        owner_id: 1
        repo_id: 2
LOCK
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp26"
check "sp26 a commented replacement edge is the same edge: one replacement edge, its comment kept, no stale edge" \
  "[ \$rc -eq 3 ] && [ \$(grep -c \"^        - 'hyperpolymath/standards@$GOOD'\" '$fx/sp26/.github/workflows/actions.lock') -eq 1 ] && grep -q \"^        - 'hyperpolymath/standards@$GOOD' # mirror already moved\$\" '$fx/sp26/.github/workflows/actions.lock' && ! grep -q \"standards@$STALE_A\" '$fx/sp26/.github/workflows/actions.lock'"

# ls19: the tag map is read case-folded: an action spelled `Actions/Checkout`
# in its record meets the shipped lower-case `actions/checkout` row offline.
mkdir -p "$fx/ls19/.github/workflows"
printf 'jobs:\n  check:\n    steps:\n      - uses: Actions/Checkout@%s\n' "$SHA_A" > "$fx/ls19/.github/workflows/ci.yml"
cat > "$fx/ls19/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'Actions/Checkout@$SHA_A'
dependencies:
    'Actions/Checkout@$SHA_A':
        ref: '$SHA_A'
        commit: 'sha1-$SHA_A'
        owner_id: 1
        repo_id: 2
LOCK
printf 'actions/checkout\t%s\tv7.0.1\n' "$SHA_A" > "$fx/ls19-tags.tsv"
TAG_MAP="$fx/ls19-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls19" --offline
check "ls19 a mixed-case action meets its lower-case tag-map row offline: repaired, exit 0" \
  "[ \$rc -eq 0 ] && grep -q \"^        ref: 'v7.0.1'\$\" '$fx/ls19/.github/workflows/actions.lock' && ! grep -q UNRESOLVED <<<\"\$out\""

# ls20: a flow `steps:` sequence spread over lines is read to its closing
# bracket: an estate repository used there is an action, so its self-SHA
# record gets the tag instead of falling back to "estate reusable source"
# and being reported OK. The block reusable call after it keeps its SHA.
lock_records() { # <dir> <owner/repo@sha>... : a lock with one record per argument
    { printf "version: 'v0.0.3'\nworkflows:\n    '.github/workflows/ci.yml':\n"
      for r in "${@:2}"; do printf "        - '%s'\n" "$r"; done
      printf "dependencies:\n"
      for r in "${@:2}"; do printf "    '%s':\n        ref: '%s'\n        commit: 'sha1-%s'\n" "$r" "${r##*@}" "${r##*@}"; done
    } > "$1/.github/workflows/actions.lock"
}
printf 'hyperpolymath/standards\t%s\tv2.0.0\nfoo/bar\t%s\tv1.0.0\nhyperpolymath/cicd-suite\t%s\tv9.9.9\n' "$SHA_A" "$SHA_B" "$SHA_C" > "$fx/ls20-tags.tsv"
mkdir -p "$fx/ls20/.github/workflows"
cat > "$fx/ls20/.github/workflows/ci.yml" <<YML
jobs:
  check:
    steps: [
      { uses: hyperpolymath/standards@$SHA_A },
      { name: build, uses: foo/bar@$SHA_B }
    ]
  call:
    uses: hyperpolymath/cicd-suite/.github/workflows/x.yml@$SHA_C
YML
lock_records "$fx/ls20" "hyperpolymath/standards@$SHA_A" "foo/bar@$SHA_B" "hyperpolymath/cicd-suite@$SHA_C"
TAG_MAP="$fx/ls20-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls20" --offline
check "ls20 a multi-line flow steps: sequence is read whole: the estate action gets its tag, the block reusable keeps its SHA" \
  "[ \$rc -eq 0 ] && grep -q \"^        ref: 'v2.0.0'\$\" '$fx/ls20/.github/workflows/actions.lock' && grep -q \"^        ref: 'v1.0.0'\$\" '$fx/ls20/.github/workflows/actions.lock' && grep -q \"^        ref: '$SHA_C'\$\" '$fx/ls20/.github/workflows/actions.lock'"

# ls21: the same on one line — `steps: [ { uses: ... } ]` under a block job
# was never read at all (only `key: {` opened the flow branch), which ls13
# did not show because a non-estate action falls back to "action" anyway.
mkdir -p "$fx/ls21/.github/workflows"
printf 'jobs:\n  check:\n    runs-on: ubuntu-latest\n    steps: [ { uses: hyperpolymath/standards@%s } ]\n' "$SHA_A" > "$fx/ls21/.github/workflows/ci.yml"
lock_records "$fx/ls21" "hyperpolymath/standards@$SHA_A"
TAG_MAP="$fx/ls20-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls21" --offline
check "ls21 an estate action in a one-line steps: [ ] under a block job is an action: tag written" \
  "[ \$rc -eq 0 ] && grep -q \"^        ref: 'v2.0.0'\$\" '$fx/ls21/.github/workflows/actions.lock'"

# ls22: a KYAML workflow is one flow mapping from its first line: its steps
# are actions and its job-level calls reusable, read through the brackets.
mkdir -p "$fx/ls22/.github/workflows"
cat > "$fx/ls22/.github/workflows/ci.yml" <<YML
{
  jobs: {
    check: {
      steps: [
        { uses: "hyperpolymath/standards@$SHA_A" },
      ],
    },
    call: { uses: "hyperpolymath/cicd-suite/.github/workflows/x.yml@$SHA_C" },
  },
}
YML
lock_records "$fx/ls22" "hyperpolymath/standards@$SHA_A" "hyperpolymath/cicd-suite@$SHA_C"
TAG_MAP="$fx/ls20-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls22" --offline
check "ls22 a KYAML workflow: its step is an action (tag written), its job-level call keeps its SHA" \
  "[ \$rc -eq 0 ] && grep -q \"^        ref: 'v2.0.0'\$\" '$fx/ls22/.github/workflows/actions.lock' && grep -q \"^        ref: '$SHA_C'\$\" '$fx/ls22/.github/workflows/actions.lock'"

# ls23: a flow collection that never closes hides the kind of every site
# after it: the fixer refuses (exit 2, lock untouched) rather than guess.
mkdir -p "$fx/ls23/.github/workflows"
printf 'jobs:\n  check:\n    steps: [\n      { uses: hyperpolymath/standards@%s }\n' "$SHA_A" > "$fx/ls23/.github/workflows/ci.yml"
lock_records "$fx/ls23" "hyperpolymath/standards@$SHA_A"
cp "$fx/ls23/.github/workflows/actions.lock" "$fx/ls23-before.lock"
TAG_MAP="$fx/ls20-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls23" --offline
check "ls23 an unclosed flow collection is refused: exit 2, said, lock untouched" \
  "[ \$rc -eq 2 ] && grep -q 'never closes' <<<\"\$out\" && cmp -s '$fx/ls23-before.lock' '$fx/ls23/.github/workflows/actions.lock'"

# ls24: the estate-source fallback for a record no workflow uses reads the
# repository case-folded: `Hyperpolymath/Standards` is the reusable source
# and keeps its SHA ref, even with a tag-map row that would retag it.
mkdir -p "$fx/ls24/.github/workflows"
printf 'jobs:\n  check:\n    steps:\n      - uses: foo/bar@v1\n' > "$fx/ls24/.github/workflows/ci.yml"
lock_records "$fx/ls24" "Hyperpolymath/Standards@$SHA_A"
TAG_MAP="$fx/ls20-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls24" --offline
check "ls24 an unreferenced Hyperpolymath/Standards record is the estate source in any case: keeps its SHA ref" \
  "[ \$rc -eq 0 ] && grep -q \"^        ref: '$SHA_A'\$\" '$fx/ls24/.github/workflows/actions.lock' && ! grep -q FIXED <<<\"\$out\""

# ls25/ls26: a hand-added record may quote its `ref` key, and may carry a
# trailing comment on its own key line; both are the same record.
mkdir -p "$fx/ls25/.github/workflows"
printf 'jobs:\n  check:\n    steps:\n      - uses: foo/bar@%s\n      - uses: foo/baz@%s\n' "$SHA_B" "$SHA_C" > "$fx/ls25/.github/workflows/ci.yml"
cat > "$fx/ls25/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'foo/bar@$SHA_B'
        - 'foo/baz@$SHA_C'
dependencies:
    'foo/bar@$SHA_B':
        "ref": "$SHA_B"
        commit: 'sha1-$SHA_B'
    'foo/baz@$SHA_C': # added by hand
        ref: '$SHA_C'
        commit: 'sha1-$SHA_C'
LOCK
printf 'foo/bar\t%s\tv1.0.0\nfoo/baz\t%s\tv3.0.0\n' "$SHA_B" "$SHA_C" > "$fx/ls25-tags.tsv"
TAG_MAP="$fx/ls25-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls25" --offline
check "ls25 a quoted \"ref\": key is the record's ref: rewritten in place, its quotes kept" \
  "[ \$rc -eq 0 ] && grep -q '^        \"ref\": \"v1.0.0\"\$' '$fx/ls25/.github/workflows/actions.lock'"
check "ls26 a record key with a trailing comment is a record: its ref rewritten, the comment kept" \
  "grep -q \"^    'foo/baz@$SHA_C': # added by hand\$\" '$fx/ls25/.github/workflows/actions.lock' && grep -q \"^        ref: 'v3.0.0'\$\" '$fx/ls25/.github/workflows/actions.lock'"

# tc21: the pin text inside an ordinary step field is data, not a step the
# parser cannot place: no UNSUPPORTED, and a real bare step beside it is fixed.
mkdir -p "$fx/tc21/.github/workflows" "$fx/tc21b/.github/workflows"
cat > "$fx/tc21/.github/workflows/x.yml" <<'YML'
jobs:
  check:
    steps:
      - name: "fixture dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772"
        uses: actions/checkout@v4
YML
cp "$fx/tc21/.github/workflows/x.yml" "$fx/tc21-before.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc21"
check "tc21 the pin named in a step's name: string is data: exit 0, no UNSUPPORTED, file untouched" \
  "[ \$rc -eq 0 ] && ! grep -q UNSUPPORTED <<<\"\$out\" && cmp -s '$fx/tc21-before.yml' '$fx/tc21/.github/workflows/x.yml'"
cat > "$fx/tc21b/.github/workflows/x.yml" <<'YML'
jobs:
  check:
    steps:
      - name: "pins dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772"
        uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
YML
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc21b"
check "tc21b the same name: beside the real bare step: the step is repaired, the name kept" \
  "[ \$rc -eq 0 ] && ! grep -q UNSUPPORTED <<<\"\$out\" && grep -q 'toolchain: stable' '$fx/tc21b/.github/workflows/x.yml' && grep -q '^      - name: \"pins dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772\"\$' '$fx/tc21b/.github/workflows/x.yml'"

# sp27: the fallback for pins the parser cannot place reads each `uses:`
# site of the masked line. A dead ref inside `name:` beside a live `uses:`
# is text (no UNSUPPORTED); a live first site does not hide a dead second
# one; and a flow brace right after the SHA ends the ref.
RC_REUSABLE=hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml
mkdir -p "$fx/sp27a/.github/workflows" "$fx/sp27b/.github/workflows" "$fx/sp27c/.github/workflows"
printf 'jobs:\n  call: { name: "%s@%s", uses: %s@%s }\n' "$RC_REUSABLE" "$STALE_A" "$RC_REUSABLE" "$GOOD" > "$fx/sp27a/.github/workflows/ci.yml"
cp "$fx/sp27a/.github/workflows/ci.yml" "$fx/sp27a-before.yml"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp27a"
check "sp27a a dead ref inside name: beside a live flow uses: is text: exit 0, no UNSUPPORTED, untouched" \
  "[ \$rc -eq 0 ] && ! grep -q UNSUPPORTED <<<\"\$out\" && cmp -s '$fx/sp27a-before.yml' '$fx/sp27a/.github/workflows/ci.yml'"
printf 'jobs: { a: { uses: %s@%s }, b: { uses: %s@%s } }\n' "$RC_REUSABLE" "$GOOD" "$RC_REUSABLE" "$STALE_A" > "$fx/sp27b/.github/workflows/ci.yml"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp27b"
check "sp27b a dead second flow site behind a live first one is UNSUPPORTED: exit 2" \
  "[ \$rc -eq 2 ] && grep -q \"UNSUPPORTED.*@$STALE_A\" <<<\"\$out\""
printf 'jobs:\n  call: { uses: %s@%s}\n' "$RC_REUSABLE" "$STALE_A" > "$fx/sp27c/.github/workflows/ci.yml"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp27c"
check "sp27c a dead flow site closed by a brace with no space is UNSUPPORTED, never OK: exit 2" \
  "[ \$rc -eq 2 ] && grep -q UNSUPPORTED <<<\"\$out\""

# sp28: `#` with no whitespace before it is part of a plain scalar, so
# `<dead-sha>#patched` is another ref the table never named: left alone.
mkdir -p "$fx/sp28/.github/workflows"
printf 'jobs:\n  rust:\n    uses: %s@%s#patched\n' "$RC_REUSABLE" "$STALE_A" > "$fx/sp28/.github/workflows/ci.yml"
cp "$fx/sp28/.github/workflows/ci.yml" "$fx/sp28-before.yml"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp28"
check "sp28 <dead-sha>#patched is a different ref: not rewritten, exit 0" \
  "[ \$rc -eq 0 ] && cmp -s '$fx/sp28-before.yml' '$fx/sp28/.github/workflows/ci.yml'"

# sp29: a lock record or workflow key with a trailing comment is a key:
# the merger keeps the unrelated commented record (it used to vanish), and
# the commented workflow key still carries the edge that moves.
mkdir -p "$fx/sp29/.github/workflows"
printf 'jobs:\n  rust:\n    uses: %s@%s\n' "$RC_REUSABLE" "$STALE_A" > "$fx/sp29/.github/workflows/ci.yml"
cat > "$fx/sp29/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml': # main CI
        - 'actions/checkout@$SHA_A'
        - 'hyperpolymath/standards@$STALE_A'
dependencies:
    'actions/checkout@$SHA_A': # pinned by hand
        ref: 'v4.2.2'
        commit: 'sha1-$SHA_A'
    'hyperpolymath/standards@$STALE_A':
        ref: '$STALE_A'
        commit: 'sha1-$STALE_A'
LOCK
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp29"
check "sp29 a commented record key survives the merger, and a commented workflow key still moves its edge" \
  "[ \$rc -eq 3 ] && grep -q \"^    'actions/checkout@$SHA_A': # pinned by hand\$\" '$fx/sp29/.github/workflows/actions.lock' && grep -q \"^        ref: 'v4.2.2'\$\" '$fx/sp29/.github/workflows/actions.lock' && grep -q \"^        - 'hyperpolymath/standards@$GOOD'\$\" '$fx/sp29/.github/workflows/actions.lock' && ! grep -q 'already out of sync' <<<\"\$out\""

# sp30: first-match rows may send two reusables of one repository at one
# stale SHA to two replacements. One lock edge and one record cannot carry
# both, so the run refuses before any rewrite; one caller alone (the
# sp-order cases above) is still applied in table order.
mkdir -p "$fx/sp30/.github/workflows"
printf 'jobs:\n  a:\n    uses: fixture/reusable/.github/workflows/ci.yml@%s\n  b:\n    uses: fixture/reusable/.github/workflows/other.yml@%s\n' "$STALE_A" "$STALE_A" > "$fx/sp30/.github/workflows/ci.yml"
cp "$fx/sp30/.github/workflows/ci.yml" "$fx/sp30-before.yml"
printf 'fixture/reusable/.github/workflows/ci.yml\t%s\t%s\texact\nfixture/reusable/.github/workflows/*\t%s\t%s\twildcard\n' "$STALE_A" "$GOOD" "$STALE_A" "$SHA_B" > "$fx/sp30-pins.tsv"
run_fix env PINS_TABLE="$fx/sp30-pins.tsv" bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp30"
check "sp30 one repository@stale sent to two replacements: CONFLICT, exit 2, nothing rewritten" \
  "[ \$rc -eq 2 ] && grep -q 'CONFLICT' <<<\"\$out\" && ! grep -q '^FIXED' <<<\"\$out\" && cmp -s '$fx/sp30-before.yml' '$fx/sp30/.github/workflows/ci.yml'"

# sp31: the stale edge quoted in another edge's trailing comment is not the
# edge: the entry that lacks it is reported out of sync, and the other edge
# is kept as written (it used to be moved, then dropped as a duplicate).
mkdir -p "$fx/sp31/.github/workflows"
printf 'jobs:\n  rust:\n    uses: %s@%s\n' "$RC_REUSABLE" "$STALE_A" > "$fx/sp31/.github/workflows/ci.yml"
cat > "$fx/sp31/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'actions/checkout@$SHA_A' # was 'hyperpolymath/standards@$STALE_A'
dependencies:
    'actions/checkout@$SHA_A':
        ref: 'v4.2.2'
        commit: 'sha1-$SHA_A'
LOCK
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp31"
check "sp31 a stale edge named only in another edge's comment: out of sync said (exit 3), that edge kept as written" \
  "[ \$rc -eq 3 ] && grep -q 'already out of sync' <<<\"\$out\" && grep -q \"^        - 'actions/checkout@$SHA_A' # was 'hyperpolymath/standards@$STALE_A'\$\" '$fx/sp31/.github/workflows/actions.lock'"

# sp32: a hand-written record may quote its field keys; its ref and commit
# move with its key, so the record is not left naming the stale SHA.
mkdir -p "$fx/sp32/.github/workflows"
printf 'jobs:\n  rust:\n    uses: %s@%s\n' "$RC_REUSABLE" "$STALE_A" > "$fx/sp32/.github/workflows/ci.yml"
cat > "$fx/sp32/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/ci.yml':
        - 'hyperpolymath/standards@$STALE_A'
dependencies:
    'hyperpolymath/standards@$STALE_A':
        "ref": '$STALE_A'
        'commit': 'sha1-$STALE_A'
LOCK
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp32"
check "sp32 quoted ref/commit field keys move with the record: no stale SHA left, quotes kept" \
  "[ \$rc -eq 3 ] && grep -q \"^        \\\"ref\\\": '$GOOD'\$\" '$fx/sp32/.github/workflows/actions.lock' && grep -q \"^        'commit': 'sha1-$GOOD'\$\" '$fx/sp32/.github/workflows/actions.lock' && ! grep -q '$STALE_A' '$fx/sp32/.github/workflows/actions.lock'"

# sp33 / tc22: a `uses:` value carrying a YAML anchor is not one the
# rewrites read (its aliases would change with it): reported, never OK.
mkdir -p "$fx/sp33/.github/workflows" "$fx/tc22/.github/workflows"
printf 'jobs:\n  rust:\n    uses: &call %s@%s\n' "$RC_REUSABLE" "$STALE_A" > "$fx/sp33/.github/workflows/ci.yml"
cp "$fx/sp33/.github/workflows/ci.yml" "$fx/sp33-before.yml"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp33"
check "sp33 an anchored dead reusable value is UNSUPPORTED: exit 2, file untouched, never OK" \
  "[ \$rc -eq 2 ] && grep -q 'UNSUPPORTED.*a value carrying an anchor or tag' <<<\"\$out\" && cmp -s '$fx/sp33-before.yml' '$fx/sp33/.github/workflows/ci.yml'"
printf 'jobs:\n  check:\n    steps:\n      - uses: &rt dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772\n' > "$fx/tc22/.github/workflows/x.yml"
cp "$fx/tc22/.github/workflows/x.yml" "$fx/tc22-before.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc22"
check "tc22 an anchored SHA-pinned toolchain step is UNSUPPORTED: exit 2, file untouched, never OK" \
  "[ \$rc -eq 2 ] && grep -q UNSUPPORTED <<<\"\$out\" && cmp -s '$fx/tc22-before.yml' '$fx/tc22/.github/workflows/x.yml'"

# ls27: the self-SHA fixer reads an anchored `uses:` value as the site it
# is: the step is an action (tag written), the job call a reusable (kept).
mkdir -p "$fx/ls27/.github/workflows"
cat > "$fx/ls27/.github/workflows/ci.yml" <<YML
jobs:
  check:
    steps:
      - uses: &act hyperpolymath/standards@$SHA_A
  call:
    uses: &rc hyperpolymath/cicd-suite/.github/workflows/x.yml@$SHA_C
YML
lock_records "$fx/ls27" "hyperpolymath/standards@$SHA_A" "hyperpolymath/cicd-suite@$SHA_C"
TAG_MAP="$fx/ls20-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls27" --offline
check "ls27 anchored uses: values are read: the anchored step is an action (tag), the anchored job call keeps its SHA" \
  "[ \$rc -eq 0 ] && grep -q \"^        ref: 'v2.0.0'\$\" '$fx/ls27/.github/workflows/actions.lock' && grep -q \"^        ref: '$SHA_C'\$\" '$fx/ls27/.github/workflows/actions.lock'"

# ls28: `#` with no whitespace before it is part of the scalar, so
# `.../x.yml@<sha>#patched` is another ref, not a reusable call at <sha>
# that would keep the action's self-SHA record from its tag.
mkdir -p "$fx/ls28/.github/workflows"
cat > "$fx/ls28/.github/workflows/ci.yml" <<YML
jobs:
  call:
    uses: foo/action.test/.github/workflows/x.yml@$SHA_A#patched
  check:
    steps:
      - uses: foo/action.test@$SHA_A
YML
lock_records "$fx/ls28" "foo/action.test@$SHA_A"
printf 'foo/action.test\t%s\tv1.2.3\n' "$SHA_A" > "$fx/ls28-tags.tsv"
TAG_MAP="$fx/ls28-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls28" --offline
check "ls28 an unspaced # keeps the ref whole: <sha>#patched is no reusable call at <sha>, the action gets its tag" \
  "[ \$rc -eq 0 ] && grep -q \"^        ref: 'v1.2.3'\$\" '$fx/ls28/.github/workflows/actions.lock'"

# sp34: a row that moves a SHA to itself repairs nothing: the table is
# refused rather than every caller reported FIXED on every run.
mkdir -p "$fx/sp34/.github/workflows"
printf 'jobs:\n  rust:\n    uses: %s@%s\n' "$RC_REUSABLE" "$STALE_A" > "$fx/sp34/.github/workflows/ci.yml"
printf 'hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml\t%s\t%s\tself\n' "$STALE_A" "$STALE_A" > "$fx/sp34-pins.tsv"
run_fix env PINS_TABLE="$fx/sp34-pins.tsv" bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp34"
check "sp34 a row whose replacement equals its stale SHA is refused: exit 2, nothing FIXED" \
  "[ \$rc -eq 2 ] && grep -q 'replacement equals the stale SHA' <<<\"\$out\" && ! grep -q '^FIXED' <<<\"\$out\""

# mode: a repaired workflow or lock keeps the mode it had (a mktemp file is
# 0600; renamed as it is, the file would be unreadable to the checkout's
# other users). 0640 is no default either path could produce by chance.
mkdir -p "$fx/mode-tc/.github/workflows" "$fx/mode-sp/.github/workflows" "$fx/mode-ls/.github/workflows"
printf 'jobs:\n  check:\n    steps:\n      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772\n' > "$fx/mode-tc/.github/workflows/x.yml"
chmod 0640 "$fx/mode-tc/.github/workflows/x.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/mode-tc"
check "mode: the toolchain fixer's repaired workflow keeps its 0640" \
  "[ \$rc -eq 0 ] && grep -q 'toolchain: stable' '$fx/mode-tc/.github/workflows/x.yml' && [ \$(stat -c %a '$fx/mode-tc/.github/workflows/x.yml') = 640 ]"
printf 'jobs:\n  rust:\n    uses: %s@%s\n' "$RC_REUSABLE" "$STALE_A" > "$fx/mode-sp/.github/workflows/ci.yml"
lock_records "$fx/mode-sp" "hyperpolymath/standards@$STALE_A"
chmod 0640 "$fx/mode-sp/.github/workflows/ci.yml" "$fx/mode-sp/.github/workflows/actions.lock"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/mode-sp"
check "mode: the stale-pin fixer's repaired workflow and lock keep their 0640" \
  "grep -q '@$GOOD' '$fx/mode-sp/.github/workflows/ci.yml' && [ \$(stat -c %a '$fx/mode-sp/.github/workflows/ci.yml') = 640 ] && [ \$(stat -c %a '$fx/mode-sp/.github/workflows/actions.lock') = 640 ]"
printf 'jobs:\n  check:\n    steps:\n      - uses: foo/bar@%s\n' "$SHA_B" > "$fx/mode-ls/.github/workflows/ci.yml"
lock_records "$fx/mode-ls" "foo/bar@$SHA_B"
chmod 0640 "$fx/mode-ls/.github/workflows/actions.lock"
TAG_MAP="$fx/ls20-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/mode-ls" --offline
check "mode: the self-SHA fixer's repaired lock keeps its 0640" \
  "[ \$rc -eq 0 ] && grep -q \"^        ref: 'v1.0.0'\$\" '$fx/mode-ls/.github/workflows/actions.lock' && [ \$(stat -c %a '$fx/mode-ls/.github/workflows/actions.lock') = 640 ]"

# tc23: GitHub resolves `Dtolnay/Rust-Toolchain` to the same action, so the
# step lacks its input all the same: it is repaired, its spelling kept.
mkdir -p "$fx/tc23/.github/workflows"
printf 'jobs:\n  check:\n    steps:\n      - uses: Dtolnay/Rust-Toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772\n' > "$fx/tc23/.github/workflows/x.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc23"
check "tc23 a SHA-pinned toolchain step spelled in another case is repaired, the spelling kept" \
  "[ \$rc -eq 0 ] && grep -q '^      - uses: Dtolnay/Rust-Toolchain@6c977a6' '$fx/tc23/.github/workflows/x.yml' && grep -q '^          toolchain: stable\$' '$fx/tc23/.github/workflows/x.yml'"

# tc24 / tc25: a step's `uses:` whose value continues below the key (a
# folded scalar, or the reference alone on the next line) names its action
# where the parser does not read it: UNSUPPORTED, never "every step is fine".
mkdir -p "$fx/tc24/.github/workflows" "$fx/tc25/.github/workflows"
printf 'jobs:\n  check:\n    steps:\n      - uses: >-\n          dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772\n      - run: cargo check\n' > "$fx/tc24/.github/workflows/x.yml"
cp "$fx/tc24/.github/workflows/x.yml" "$fx/tc24-before.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc24"
check "tc24 a folded uses: >- toolchain pin is UNSUPPORTED: exit 2, file untouched" \
  "[ \$rc -eq 2 ] && grep -q 'UNSUPPORTED.*line(s): 5\\.' <<<\"\$out\" && cmp -s '$fx/tc24-before.yml' '$fx/tc24/.github/workflows/x.yml'"
printf 'jobs:\n  check:\n    steps:\n      - name: tc\n        uses:\n          dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772\n' > "$fx/tc25/.github/workflows/x.yml"
cp "$fx/tc25/.github/workflows/x.yml" "$fx/tc25-before.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc25"
check "tc25 a uses: whose reference sits on the next line is UNSUPPORTED: exit 2, file untouched" \
  "[ \$rc -eq 2 ] && grep -q UNSUPPORTED <<<\"\$out\" && cmp -s '$fx/tc25-before.yml' '$fx/tc25/.github/workflows/x.yml'"

# tc26 (control): the same text inside a `run: |` script is the script's,
# and a real step after it is still repaired.
mkdir -p "$fx/tc26/.github/workflows"
printf 'jobs:\n  check:\n    steps:\n      - run: |\n          cat > fx.yml <<EOF\n          uses: >-\n            dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772\n          EOF\n      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772\n' > "$fx/tc26/.github/workflows/x.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc26"
check "tc26 a folded uses: inside a run: | script is text: the real step is repaired, the script unchanged" \
  "[ \$rc -eq 0 ] && [ \$(grep -c 'toolchain: stable' '$fx/tc26/.github/workflows/x.yml') -eq 1 ] && grep -q '^          uses: >-\$' '$fx/tc26/.github/workflows/x.yml'"

# sp35: a job's `uses:` written as a folded scalar holds the call on the
# line below: a dead pin there is UNSUPPORTED, never "OK" and left dead.
mkdir -p "$fx/sp35/.github/workflows"
printf 'jobs:\n  rust:\n    uses: >-\n      %s@%s\n' "$RC_REUSABLE" "$STALE_A" > "$fx/sp35/.github/workflows/ci.yml"
cp "$fx/sp35/.github/workflows/ci.yml" "$fx/sp35-before.yml"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp35"
check "sp35 a folded uses: >- dead reusable pin is UNSUPPORTED: exit 2, file untouched, never OK" \
  "[ \$rc -eq 2 ] && grep -q 'UNSUPPORTED.*ci.yml:4:.*continued below its key' <<<\"\$out\" && ! grep -q '^OK' <<<\"\$out\" && cmp -s '$fx/sp35-before.yml' '$fx/sp35/.github/workflows/ci.yml'"

# sp36: a caller at `<stale>-patched` is another ref, not the stale SHA: it
# does not keep the stale lock edge next to the replacement.
mkdir -p "$fx/sp36/.github/workflows"
printf 'jobs:\n  rust:\n    uses: %s@%s\n  other:\n    uses: hyperpolymath/standards/.github/workflows/other.yml@%s-patched\n' "$RC_REUSABLE" "$STALE_A" "$STALE_A" > "$fx/sp36/.github/workflows/ci.yml"
lock_records "$fx/sp36" "hyperpolymath/standards@$STALE_A"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp36"
check "sp36 a <stale>-patched caller keeps no stale edge: the edge moves to the replacement" \
  "grep -q \"^        - 'hyperpolymath/standards@$GOOD'\$\" '$fx/sp36/.github/workflows/actions.lock' && ! grep -q \"standards@$STALE_A'\" '$fx/sp36/.github/workflows/actions.lock'"

# ls29 / ls30: a `uses:` site whose value continues below the key cannot be
# classified, so the record it names would read `unknown` and a reusable's
# SHA ref be retagged as if it were an action's: the lock is left alone.
mkdir -p "$fx/ls29/.github/workflows" "$fx/ls30/.github/workflows"
cat > "$fx/ls29/.github/workflows/ci.yml" <<YML
jobs:
  call:
    uses: >-
      foo/reus/.github/workflows/x.yml@$SHA_A
YML
lock_records "$fx/ls29" "foo/reus@$SHA_A"
printf 'foo/reus\t%s\tv1.2.3\n' "$SHA_A" > "$fx/ls29-tags.tsv"
cp "$fx/ls29/.github/workflows/actions.lock" "$fx/ls29-before.lock"
TAG_MAP="$fx/ls29-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls29" --offline
check "ls29 a folded job uses: is refused: exit 2, the reusable's SHA ref not retagged" \
  "[ \$rc -eq 2 ] && grep -q 'ci.yml:3 is a block scalar or continues' <<<\"\$out\" && cmp -s '$fx/ls29-before.lock' '$fx/ls29/.github/workflows/actions.lock'"
cat > "$fx/ls30/.github/workflows/ci.yml" <<YML
jobs:
  check:
    steps:
      - uses:
          foo/bar@$SHA_B
YML
lock_records "$fx/ls30" "foo/bar@$SHA_B"
cp "$fx/ls30/.github/workflows/actions.lock" "$fx/ls30-before.lock"
TAG_MAP="$fx/ls20-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls30" --offline
check "ls30 a step uses: with its reference on the next line is refused: exit 2, lock untouched" \
  "[ \$rc -eq 2 ] && grep -q 'block scalar or continues' <<<\"\$out\" && cmp -s '$fx/ls30-before.lock' '$fx/ls30/.github/workflows/actions.lock'"

# ls31 (control): a folded uses: inside a run: | script is the script's text.
mkdir -p "$fx/ls31/.github/workflows"
cat > "$fx/ls31/.github/workflows/ci.yml" <<YML
jobs:
  check:
    steps:
      - run: |
          uses: >-
            foo/reus/.github/workflows/x.yml@$SHA_A
      - uses: foo/bar@$SHA_B
YML
lock_records "$fx/ls31" "foo/bar@$SHA_B"
TAG_MAP="$fx/ls20-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls31" --offline
check "ls31 a folded uses: inside run: | is text: the action's record still gets its tag" \
  "[ \$rc -eq 0 ] && grep -q \"^        ref: 'v1.0.0'\$\" '$fx/ls31/.github/workflows/actions.lock'"

# tc27: an alias of an anchor that holds the SHA-pinned toolchain reference
# (`uses: *rust`) runs that action all the same: UNSUPPORTED, never "fine".
mkdir -p "$fx/tc27/.github/workflows"
printf 'env: { ACTION: &rust dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772 }\njobs:\n  check:\n    steps:\n      - uses: *rust\n' > "$fx/tc27/.github/workflows/x.yml"
cp "$fx/tc27/.github/workflows/x.yml" "$fx/tc27-before.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc27"
check "tc27 an aliased SHA-pinned toolchain step is UNSUPPORTED: exit 2, file untouched" \
  "[ \$rc -eq 2 ] && grep -q 'UNSUPPORTED.*line(s): 5\\.' <<<\"\$out\" && cmp -s '$fx/tc27-before.yml' '$fx/tc27/.github/workflows/x.yml'"

# tc28 / tc29: TOOLCHAIN is written as a YAML value, so it must be a
# toolchain name; a number YAML would read as a float is written quoted.
mkdir -p "$fx/tc28/.github/workflows" "$fx/tc29/.github/workflows"
printf 'jobs:\n  check:\n    steps:\n      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772\n' > "$fx/tc28/.github/workflows/x.yml"
cp "$fx/tc28/.github/workflows/x.yml" "$fx/tc28-before.yml"
cp "$fx/tc28/.github/workflows/x.yml" "$fx/tc29/.github/workflows/x.yml"
run_fix env TOOLCHAIN=$'stable\n        timeout-minutes: 1' bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc28"
check "tc28 a TOOLCHAIN that is not a toolchain name is refused: exit 2, file untouched" \
  "[ \$rc -eq 2 ] && grep -q 'is not a toolchain name' <<<\"\$out\" && cmp -s '$fx/tc28-before.yml' '$fx/tc28/.github/workflows/x.yml'"
run_fix env TOOLCHAIN=1.80 bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc29"
check "tc29 TOOLCHAIN=1.80 is written quoted, never read as the float 1.8" \
  "[ \$rc -eq 0 ] && grep -q \"^          toolchain: '1.80'\$\" '$fx/tc29/.github/workflows/x.yml'"

# sp37: a job calling `uses: *call`, the anchor holding a known-dead pin:
# UNSUPPORTED, never "OK" with the dead caller left in place.
mkdir -p "$fx/sp37/.github/workflows"
printf 'env: { CALL: &call %s@%s }\njobs:\n  rust:\n    uses: *call\n' "$RC_REUSABLE" "$STALE_A" > "$fx/sp37/.github/workflows/ci.yml"
cp "$fx/sp37/.github/workflows/ci.yml" "$fx/sp37-before.yml"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp37"
check "sp37 an aliased dead reusable call is UNSUPPORTED: exit 2, file untouched, never OK" \
  "[ \$rc -eq 2 ] && grep -q 'UNSUPPORTED.*ci.yml:4:.*uses: \\*call' <<<\"\$out\" && ! grep -q '^OK' <<<\"\$out\" && cmp -s '$fx/sp37-before.yml' '$fx/sp37/.github/workflows/ci.yml'"

# ls32 / ls33: an aliased job call is classified by its anchor's value, so a
# reusable called through `uses: *call` keeps its SHA ref; an alias with no
# anchor read before it cannot be classified and the lock is left alone.
mkdir -p "$fx/ls32/.github/workflows" "$fx/ls33/.github/workflows"
cat > "$fx/ls32/.github/workflows/ci.yml" <<YML
env: { CALL: &call foo/reus/.github/workflows/x.yml@$SHA_A }
jobs:
  call:
    uses: *call
YML
lock_records "$fx/ls32" "foo/reus@$SHA_A"
TAG_MAP="$fx/ls29-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls32" --offline
check "ls32 a reusable called through an alias keeps its SHA ref" \
  "[ \$rc -eq 0 ] && grep -q \"^        ref: '$SHA_A'\$\" '$fx/ls32/.github/workflows/actions.lock'"
printf 'jobs:\n  check:\n    steps:\n      - uses: *nowhere\n' > "$fx/ls33/.github/workflows/ci.yml"
lock_records "$fx/ls33" "foo/reus@$SHA_A"
cp "$fx/ls33/.github/workflows/actions.lock" "$fx/ls33-before.lock"
TAG_MAP="$fx/ls29-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls33" --offline
check "ls33 an alias of no anchor is refused: exit 2, lock untouched" \
  "[ \$rc -eq 2 ] && cmp -s '$fx/ls33-before.lock' '$fx/ls33/.github/workflows/actions.lock'"

# tc30: TOOLCHAIN=null is a name YAML reads as null: written quoted, so the
# input is the string, and a second run finds it present (no FIXED again).
mkdir -p "$fx/tc30/.github/workflows"
printf 'jobs:\n  check:\n    steps:\n      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772\n' > "$fx/tc30/.github/workflows/x.yml"
run_fix env TOOLCHAIN=null bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc30"
first_rc=$rc
run_fix env TOOLCHAIN=null bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc30"
check "tc30 TOOLCHAIN=null is written quoted, and a rerun repairs nothing" \
  "[ $first_rc -eq 0 ] && [ \$rc -eq 0 ] && grep -q \"^          toolchain: 'null'\$\" '$fx/tc30/.github/workflows/x.yml' && ! grep -q '^FIXED' <<<\"\$out\""

# tc31 / sp38 / ls34: anchor-like text inside a quoted scalar is text, not
# the anchor an alias resolves to (lib/yaml-anchors.awk).
mkdir -p "$fx/tc31/.github/workflows" "$fx/sp38/.github/workflows" "$fx/ls34/.github/workflows"
printf 'env:\n  ACTION: &rust dtolnay/rust-toolchain@stable\n  NOTE: "fixture &rust dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772"\njobs:\n  check:\n    steps:\n      - uses: *rust\n' > "$fx/tc31/.github/workflows/x.yml"
cp "$fx/tc31/.github/workflows/x.yml" "$fx/tc31-before.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc31"
check "tc31 a quoted &rust is no anchor: the alias of a tag-pinned anchor is fine, not UNSUPPORTED" \
  "[ \$rc -eq 0 ] && ! grep -q UNSUPPORTED <<<\"\$out\" && cmp -s '$fx/tc31-before.yml' '$fx/tc31/.github/workflows/x.yml'"
printf 'env:\n  CALL: &call %s@%s\n  NOTE: "fixture &call %s@%s"\njobs:\n  rust:\n    uses: *call\n' "$RC_REUSABLE" "$GOOD" "$RC_REUSABLE" "$STALE_A" > "$fx/sp38/.github/workflows/ci.yml"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp38"
check "sp38 a quoted &call naming a dead pin is no anchor: the live alias is not UNSUPPORTED" \
  "[ \$rc -eq 0 ] && ! grep -q UNSUPPORTED <<<\"\$out\""
cat > "$fx/ls34/.github/workflows/ci.yml" <<YML
env:
  CALL: &call octo/repo/.github/workflows/x.yml@$SHA_A
  NOTE: "fixture &call octo/repo@$SHA_A"
jobs:
  call:
    uses: *call
YML
lock_records "$fx/ls34" "octo/repo@$SHA_A"
printf 'octo/repo\t%s\tv1.2.3\n' "$SHA_A" > "$fx/ls34-tags.tsv"
TAG_MAP="$fx/ls34-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls34" --offline
check "ls34 a quoted &call does not replace the real anchor: the aliased reusable keeps its SHA ref" \
  "[ \$rc -eq 0 ] && grep -q \"^        ref: '$SHA_A'\$\" '$fx/ls34/.github/workflows/actions.lock'"

# tc32 / tc33: anchor names are case-sensitive (`&Rust` is not `&rust`), and
# an alias of no anchor read could be the pinned toolchain: UNSUPPORTED.
mkdir -p "$fx/tc32/.github/workflows" "$fx/tc33/.github/workflows"
printf 'env:\n  A: &Rust dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772\n  B: &rust dtolnay/rust-toolchain@stable\njobs:\n  check:\n    steps:\n      - uses: *Rust\n' > "$fx/tc32/.github/workflows/x.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc32"
check "tc32 *Rust is the SHA-pinned &Rust, not the tag-pinned &rust: UNSUPPORTED" \
  "[ \$rc -eq 2 ] && grep -q 'UNSUPPORTED.*line(s): 7\\.' <<<\"\$out\""
# The anchor sits mid-line in a flow sequence continued from the line
# above, where the line-local reader does not take `&` for an anchor; the
# file does carry the pinned reference, so the unread alias is reported.
printf 'x-list: [a,\n  b, &rust dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772]\njobs:\n  check:\n    steps:\n      - uses: *rust\n' > "$fx/tc33/.github/workflows/x.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc33"
check "tc33 an alias of no anchor read, in a file carrying a SHA-pinned toolchain ref, is UNSUPPORTED" \
  "[ \$rc -eq 2 ] && grep -q UNSUPPORTED <<<\"\$out\""

# sp39: a nested `uses: # rationale` header is the list header: two records
# merging onto the replacement leave one header, its comment kept.
mkdir -p "$fx/sp39/.github/workflows"
printf 'jobs:\n  governance:\n    uses: hyperpolymath/standards/.github/workflows/governance-reusable.yml@%s\n' "$STALE_B" > "$fx/sp39/.github/workflows/governance.yml"
printf 'jobs:\n  scorecard:\n    uses: hyperpolymath/standards/.github/workflows/scorecard-reusable.yml@%s\n' "$STALE_C" > "$fx/sp39/.github/workflows/scorecard.yml"
cat > "$fx/sp39/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/governance.yml':
        - 'hyperpolymath/standards@$STALE_B'
    '.github/workflows/scorecard.yml':
        - 'hyperpolymath/standards@$STALE_C'
dependencies:
    'hyperpolymath/standards@$STALE_B':
        ref: '$STALE_B'
        commit: 'sha1-$STALE_B'
        uses: # transitive actions of the reusable
            - 'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1'
    'hyperpolymath/standards@$STALE_C':
        ref: '$STALE_C'
        commit: 'sha1-$STALE_C'
        uses:
            - 'ossf/scorecard-action@2d1146689b8cda280b9bc96326124645441f03bc'
LOCK
bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp39" >/dev/null 2>&1 || true
check "sp39 a commented nested uses: header is the one header of the merged record" \
  "[ \$(grep -c '^        uses:' '$fx/sp39/.github/workflows/actions.lock') -eq 1 ] && grep -q '^        uses: # transitive actions of the reusable\$' '$fx/sp39/.github/workflows/actions.lock' && grep -q 'ossf/scorecard-action' '$fx/sp39/.github/workflows/actions.lock'"

# ls35 / ls36: `&call` inside a plain scalar is text; a tag YAML would read
# as a non-string is written quoted into a plain ref field.
mkdir -p "$fx/ls35/.github/workflows" "$fx/ls36/.github/workflows"
cat > "$fx/ls35/.github/workflows/ci.yml" <<YML
env:
  CALL: &call octo/repo/.github/workflows/x.yml@$SHA_A
  NOTE: fixture &call octo/repo@$SHA_A
jobs:
  call:
    uses: *call
YML
lock_records "$fx/ls35" "octo/repo@$SHA_A"
TAG_MAP="$fx/ls34-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls35" --offline
check "ls35 &call inside a plain scalar is text: the aliased reusable keeps its SHA ref" \
  "[ \$rc -eq 0 ] && grep -q \"^        ref: '$SHA_A'\$\" '$fx/ls35/.github/workflows/actions.lock'"
printf 'jobs:\n  check:\n    steps:\n      - uses: foo/bar@%s\n' "$SHA_B" > "$fx/ls36/.github/workflows/ci.yml"
printf "version: 'v0.0.3'\nworkflows:\n    '.github/workflows/ci.yml':\n        - 'foo/bar@%s'\ndependencies:\n    'foo/bar@%s':\n        ref: %s\n        commit: 'sha1-%s'\n" "$SHA_B" "$SHA_B" "$SHA_B" "$SHA_B" > "$fx/ls36/.github/workflows/actions.lock"
printf 'foo/bar\t%s\ttrue\n' "$SHA_B" > "$fx/ls36-tags.tsv"
TAG_MAP="$fx/ls36-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls36" --offline
check "ls36 a tag YAML reads as a boolean is written quoted into a plain ref field" \
  "[ \$rc -eq 0 ] && grep -q \"^        ref: 'true'\$\" '$fx/ls36/.github/workflows/actions.lock'"

# sp40 / sp41: a nested `uses:` list written flow-style merges like a block
# one — the merged record keeps ONE `uses:` key and every entry: `uses: []`
# on the first record does not stand beside the block list folded into it,
# and a flow list on the record folded away is not dropped with it.
sp_flow_fixture() { # <dir> <governance uses: lines> <scorecard uses: lines>
    mkdir -p "$1/.github/workflows"
    printf 'jobs:\n  governance:\n    uses: hyperpolymath/standards/.github/workflows/governance-reusable.yml@%s\n' "$STALE_B" > "$1/.github/workflows/governance.yml"
    printf 'jobs:\n  scorecard:\n    uses: hyperpolymath/standards/.github/workflows/scorecard-reusable.yml@%s\n' "$STALE_C" > "$1/.github/workflows/scorecard.yml"
    printf "version: 'v0.0.3'\nworkflows:\n    '.github/workflows/governance.yml':\n        - 'hyperpolymath/standards@%s'\n    '.github/workflows/scorecard.yml':\n        - 'hyperpolymath/standards@%s'\ndependencies:\n    'hyperpolymath/standards@%s':\n        ref: '%s'\n        commit: 'sha1-%s'\n%b    'hyperpolymath/standards@%s':\n        ref: '%s'\n        commit: 'sha1-%s'\n%b" \
        "$STALE_B" "$STALE_C" "$STALE_B" "$STALE_B" "$STALE_B" "$2" "$STALE_C" "$STALE_C" "$STALE_C" "$3" > "$1/.github/workflows/actions.lock"
}
sp_flow_fixture "$fx/sp40" "        uses: []\n" "        uses:\n            - 'ossf/scorecard-action@2d1146689b8cda280b9bc96326124645441f03bc'\n"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp40"
check "sp40 uses: [] on the first record and a block list on the merged one leave one uses: key holding the entry" \
  "[ \$rc -eq 3 ] && [ \$(grep -c '^        uses:' '$fx/sp40/.github/workflows/actions.lock') -eq 1 ] && grep -q \"^            - 'ossf/scorecard-action@2d1146689b8cda280b9bc96326124645441f03bc'\\\$\" '$fx/sp40/.github/workflows/actions.lock'"
sp_flow_fixture "$fx/sp41" "        uses:\n            - 'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1'\n" "        uses: ['ossf/scorecard-action@2d1146689b8cda280b9bc96326124645441f03bc', \"actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1\"]\n"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp41"
check "sp41 a one-line flow list on the record folded away merges into the block list: one key, each entry once" \
  "[ \$rc -eq 3 ] && [ \$(grep -c '^        uses:' '$fx/sp41/.github/workflows/actions.lock') -eq 1 ] && grep -q \"^            - 'ossf/scorecard-action@2d1146689b8cda280b9bc96326124645441f03bc'\\\$\" '$fx/sp41/.github/workflows/actions.lock' && [ \$(grep -c 'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1' '$fx/sp41/.github/workflows/actions.lock') -eq 1 ]"
sp_flow_fixture "$fx/sp41b" "        uses: [] # none yet\n" "        uses: []\n"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp41b"
check "sp41b two empty flow lists merge to the first one as written" \
  "[ \$rc -eq 3 ] && [ \$(grep -c '^        uses:' '$fx/sp41b/.github/workflows/actions.lock') -eq 1 ] && grep -q '^        uses: \\[\\] # none yet\$' '$fx/sp41b/.github/workflows/actions.lock'"

# sp42: a record listed twice whose `uses:` is a flow list over several lines
# is a shape the merger cannot fold. It is not folded with its entries lost:
# the duplicates stay, the record is named UNSUPPORTED, exit 2.
sp_flow_fixture "$fx/sp42" "        uses:\n            - 'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1'\n" "        uses: [\n            'ossf/scorecard-action@2d1146689b8cda280b9bc96326124645441f03bc' ]\n"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp42"
check "sp42 a duplicated record with a multi-line flow uses: is refused, named and left with every entry" \
  "[ \$rc -eq 2 ] && grep -q \"UNSUPPORTED .*actions.lock: the record hyperpolymath/standards@$GOOD is listed 2 times\" <<<\"\$out\" && grep -q 'ossf/scorecard-action@2d1146689b8cda280b9bc96326124645441f03bc' '$fx/sp42/.github/workflows/actions.lock' && grep -q 'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1' '$fx/sp42/.github/workflows/actions.lock'"

# sp43 (control): a record with a flow list and nothing merged into it is
# written back exactly as it was, not re-spelled as a block list.
mkdir -p "$fx/sp43/.github/workflows"
printf 'jobs:\n  governance:\n    uses: hyperpolymath/standards/.github/workflows/governance-reusable.yml@%s\n' "$STALE_B" > "$fx/sp43/.github/workflows/governance.yml"
printf "version: 'v0.0.3'\nworkflows:\n    '.github/workflows/governance.yml':\n        - 'hyperpolymath/standards@%s'\ndependencies:\n    'hyperpolymath/standards@%s':\n        ref: '%s'\n        commit: 'sha1-%s'\n        uses: ['b/b@3d3c42e5aac5ba805825da76410c181273ba90b1', 'a/a@3d3c42e5aac5ba805825da76410c181273ba90b1']\n" \
    "$STALE_B" "$STALE_B" "$STALE_B" "$STALE_B" > "$fx/sp43/.github/workflows/actions.lock"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp43"
check "sp43 a flow list nothing was merged into is kept as written" \
  "[ \$rc -eq 3 ] && grep -qF \"        uses: ['b/b@3d3c42e5aac5ba805825da76410c181273ba90b1', 'a/a@3d3c42e5aac5ba805825da76410c181273ba90b1']\" '$fx/sp43/.github/workflows/actions.lock'"

# pl4: without flock the lock is a directory created with mkdir — still one
# fixer per checkout. A held lock is waited for, reported with its holder,
# and the run exits 2 having changed nothing; a free one is taken, and the
# run removes it on exit.
nofl="$fx/no-flock-bin"; mkdir -p "$nofl"
IFS=: read -ra pl4_dirs <<<"$PATH"
for d in "${pl4_dirs[@]}"; do [ -d "$d" ] && { ln -s "$d"/* "$nofl"/ 2>/dev/null || true; }; done
rm -f "$nofl/flock"
mkdir -p "$fx/pl4/.github/workflows"
printf 'jobs:\n  rust:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$STALE_A" > "$fx/pl4/.github/workflows/ci.yml"
cp "$fx/pl4/.github/workflows/ci.yml" "$fx/pl4-before.yml"
pl4_key=$(cd "$fx/pl4" && pwd -P | tr -d '\n' | sha256sum | cut -c1-16)
mkdir "$fx/pin-fixer-$pl4_key.lock.d"; echo 4242 > "$fx/pin-fixer-$pl4_key.lock.d/pid"
PATH="$nofl" PIN_FIXER_LOCK_DIR="$fx" PIN_FIXER_LOCK_WAIT=1 run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/pl4"
check "pl4 without flock a held lock directory makes the fixer report its holder, exit 2 and change nothing" \
  "[ \$rc -eq 2 ] && grep -q 'another workflow-pin fixer holds.*held by pid 4242' <<<\"\$out\" && cmp -s '$fx/pl4-before.yml' '$fx/pl4/.github/workflows/ci.yml'"
rm -rf "$fx/pin-fixer-$pl4_key.lock.d"
PATH="$nofl" PIN_FIXER_LOCK_DIR="$fx" PIN_FIXER_LOCK_WAIT=1 run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/pl4"
check "pl4 without flock a free lock is taken, the run repairs, and the lock directory is gone afterwards" \
  "[ \$rc -eq 0 ] && grep -q \"rust-ci-reusable.yml@$GOOD\" '$fx/pl4/.github/workflows/ci.yml' && [ ! -e '$fx/pin-fixer-$pl4_key.lock.d' ]"

# ── Review cases (gitbot-fleet#615, round on ccfc924) ───────────────────────
# sp44: a stale record spelled `Hyperpolymath/Standards@<stale>` renamed onto
# the replacement that `hyperpolymath/standards@<good>` already holds is the
# same dependency — GitHub reads the repository in either case — so the two
# records merge into one, the first spelling kept.
mkdir -p "$fx/sp44/.github/workflows"
printf 'jobs:\n  governance:\n    uses: hyperpolymath/standards/.github/workflows/governance-reusable.yml@%s\n' "$STALE_B" > "$fx/sp44/.github/workflows/governance.yml"
cat > "$fx/sp44/.github/workflows/actions.lock" <<LOCK
version: 'v0.0.3'
workflows:
    '.github/workflows/governance.yml':
        - 'Hyperpolymath/Standards@$STALE_B'
dependencies:
    'hyperpolymath/standards@$GOOD':
        ref: '$GOOD'
        commit: 'sha1-$GOOD'
    'Hyperpolymath/Standards@$STALE_B':
        ref: '$STALE_B'
        commit: 'sha1-$STALE_B'
LOCK
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp44"
check "sp44 dependency keys differing only in the repository's case merge into one record, first spelling kept" \
  "[ \$rc -eq 3 ] && [ \$(grep -ciE \"^    'hyperpolymath/standards@$GOOD':\" '$fx/sp44/.github/workflows/actions.lock') -eq 1 ] && grep -q \"^    'hyperpolymath/standards@$GOOD':\" '$fx/sp44/.github/workflows/actions.lock'"

# tc34: `with:` whose value is written on the next line as a flow mapping or
# an alias is not a block mapping a `toolchain:` child can join: reported,
# exit 2, the file untouched. A block child on the next line is repaired.
mkdir -p "$fx/tc34/.github/workflows" "$fx/tc34b/.github/workflows"
cat > "$fx/tc34/.github/workflows/x.yml" <<'YML'
jobs:
  check:
    steps:
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with:
          { components: rustfmt }
      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772
        with:
          # shared inputs
          *inputs
YML
cp "$fx/tc34/.github/workflows/x.yml" "$fx/tc34-before.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc34"
check "tc34 with: holding a flow mapping or an alias on the next line is reported, exit 2, file untouched" \
  "[ \$rc -eq 2 ] && grep -q 'UNSUPPORTED.*line(s): 6 10' <<<\"\$out\" && cmp -s '$fx/tc34-before.yml' '$fx/tc34/.github/workflows/x.yml'"
printf 'jobs:\n  check:\n    steps:\n      - uses: dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772\n        with:\n          components: rustfmt\n' > "$fx/tc34b/.github/workflows/x.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc34b"
check "tc34 (control) a block child on the next line is a block mapping: toolchain added beside it" \
  "[ \$rc -eq 0 ] && grep -q '^          toolchain: stable\$' '$fx/tc34b/.github/workflows/x.yml' && grep -q '^          components: rustfmt\$' '$fx/tc34b/.github/workflows/x.yml'"

# sp45: an anchor defined after another item of a flow sequence begun on an
# earlier line is one the line-local anchor reader does not see; a job's
# `uses: *call` naming it, in a file that holds a known-dead pin, is
# reported (exit 2, untouched), never passed as OK. The same unreadable
# alias in a file with no known-dead pin is no finding.
mkdir -p "$fx/sp45/.github/workflows" "$fx/sp45b/.github/workflows"
cat > "$fx/sp45/.github/workflows/ci.yml" <<YML
on:
  push:
    branches: [main,
      release, &call hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@$STALE_A]
jobs:
  rust:
    uses: *call
YML
cp "$fx/sp45/.github/workflows/ci.yml" "$fx/sp45-before.yml"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp45"
check "sp45 an alias the reader cannot resolve, in a file holding a known-dead pin, is UNSUPPORTED, exit 2" \
  "[ \$rc -eq 2 ] && grep -q 'UNSUPPORTED  .github/workflows/ci.yml:7: \`uses: \*call\` names an anchor this parser cannot read' <<<\"\$out\" && cmp -s '$fx/sp45-before.yml' '$fx/sp45/.github/workflows/ci.yml'"
sed "s/@$STALE_A/@$GOOD/" "$fx/sp45/.github/workflows/ci.yml" > "$fx/sp45b/.github/workflows/ci.yml"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp45b"
check "sp45 (control) the same alias in a file with no known-dead pin is no finding" \
  "[ \$rc -eq 0 ] && grep -q '^OK (no known-dead reusable pins)' <<<\"\$out\""

# pin_fixer_yaml_nonstring: the YAML 1.1 / core-schema plain scalars that are
# not strings — timestamps, signed and dotted numbers, base 60, .inf/.nan,
# ~ and nothing — and names that only look like them.
yn_out=$(bash -c '. "$1"; for v in 2026-10-20 "2026-10-20T10:00:00Z" -1 +2 .5 1:30 .inf -.Inf .NaN "~" "" 0x1F 1e3 null Yes; do pin_fixer_yaml_nonstring "$v" || echo "STRING:$v"; done; for v in v2026-10-20 nightly-2026-10-20 2026-10 1.2.3 stable release v1 1.85.0 .nanx; do pin_fixer_yaml_nonstring "$v" && echo "NONSTRING:$v"; done; true' _ "$fleet/scripts/lib/pin-fixer-lock.sh")
check "pin_fixer_yaml_nonstring reads timestamps, signed/dotted numbers, base 60, .inf/.nan and ~ as non-strings, and only those" \
  "[ -z \"\$yn_out\" ]"

# ls37: a tag spelled as a date is written quoted into a plain ref field, so
# the lock holds the tag string, not a timestamp.
mkdir -p "$fx/ls37/.github/workflows"
action_steps "$fx/ls37/.github/workflows" "foo/bar@$SHA_B"
printf "version: 'v0.0.3'\nworkflows:\n    '.github/workflows/ci.yml':\n        - 'foo/bar@%s'\ndependencies:\n    'foo/bar@%s':\n        ref: %s\n        commit: 'sha1-%s'\n" "$SHA_B" "$SHA_B" "$SHA_B" "$SHA_B" > "$fx/ls37/.github/workflows/actions.lock"
printf 'foo/bar\t%s\t2026-10-20\n' "$SHA_B" > "$fx/ls37-tags.tsv"
TAG_MAP="$fx/ls37-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls37" --offline
check "ls37 a tag spelled as a date is written quoted into a plain ref field" \
  "[ \$rc -eq 0 ] && grep -q \"^        ref: '2026-10-20'\$\" '$fx/ls37/.github/workflows/actions.lock'"

# ls38: a self-SHA record no workflow here uses (a transitive dependency: a
# reusable's own call, which may be a reusable whose SHA ref is legitimate)
# is not rewritten to a tag on a guess, even with a tag to hand: reported
# UNRESOLVED, the record unchanged, exit 1.
mkdir -p "$fx/ls38/.github/workflows"
action_steps "$fx/ls38/.github/workflows" "actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1"
lock_records "$fx/ls38" "third/party@$SHA_A"
printf 'third/party\t%s\tv1.0.0\n' "$SHA_A" > "$fx/ls38-tags.tsv"
cp "$fx/ls38/.github/workflows/actions.lock" "$fx/ls38-before.lock"
TAG_MAP="$fx/ls38-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls38" --offline
check "ls38 a record no workflow uses is UNRESOLVED and left as is, never rewritten on a guess" \
  "[ \$rc -eq 1 ] && grep -q 'UNRESOLVED  third/party@aaaaaaa: no workflow here uses it' <<<\"\$out\" && cmp -s '$fx/ls38-before.lock' '$fx/ls38/.github/workflows/actions.lock'"

# tc35 / sp46: a `uses:` value carrying a YAML tag (`!!str`), alone or with
# an anchor, is still the step's or job's pin, but not one the rewrite
# places: reported UNSUPPORTED, exit 2, the file untouched — never passed as
# OK. (hypatia WF026/WF027 route such findings to review.)
mkdir -p "$fx/tc35/.github/workflows" "$fx/sp46/.github/workflows"
printf 'jobs:\n  check:\n    steps:\n      - uses: !!str dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772\n      - uses: !!str &rust "dtolnay/rust-toolchain@6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772"\n' > "$fx/tc35/.github/workflows/x.yml"
cp "$fx/tc35/.github/workflows/x.yml" "$fx/tc35-before.yml"
run_fix bash "$fleet/scripts/fix-rust-toolchain-input.sh" "$fx/tc35"
check "tc35 a tagged uses: value is reported UNSUPPORTED, exit 2, file untouched" \
  "[ \$rc -eq 2 ] && grep -q 'UNSUPPORTED.*line(s): 4 5' <<<\"\$out\" && cmp -s '$fx/tc35-before.yml' '$fx/tc35/.github/workflows/x.yml'"
printf 'jobs:\n  rust:\n    uses: !!str hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$STALE_A" > "$fx/sp46/.github/workflows/ci.yml"
cp "$fx/sp46/.github/workflows/ci.yml" "$fx/sp46-before.yml"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp46"
check "sp46 a tagged job uses: holding a known-dead pin is UNSUPPORTED, exit 2, file untouched" \
  "[ \$rc -eq 2 ] && grep -q 'UNSUPPORTED  .github/workflows/ci.yml:3: .*a value carrying an anchor or tag' <<<\"\$out\" && cmp -s '$fx/sp46-before.yml' '$fx/sp46/.github/workflows/ci.yml'"

mkdir -p "$fx/sp46b/.github/workflows"
printf 'jobs:\n  rust:\n    uses: !!str "hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s"\n' "$STALE_A" > "$fx/sp46b/.github/workflows/ci.yml"
cp "$fx/sp46b/.github/workflows/ci.yml" "$fx/sp46b-before.yml"
run_fix bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp46b"
check "sp46 a quoted value after a tag is the job's value too: UNSUPPORTED, exit 2, untouched" \
  "[ \$rc -eq 2 ] && grep -q 'UNSUPPORTED  .github/workflows/ci.yml:3: ' <<<\"\$out\" && cmp -s '$fx/sp46b-before.yml' '$fx/sp46b/.github/workflows/ci.yml'"
# ls39: a step whose `uses:` value carries a tag still proves its record an
# action: the lock's self-SHA ref gets its tag.
mkdir -p "$fx/ls39/.github/workflows"
printf 'jobs:\n  build:\n    steps:\n      - uses: !!str "foo/bar@%s"\n' "$SHA_B" > "$fx/ls39/.github/workflows/ci.yml"
lock_records "$fx/ls39" "foo/bar@$SHA_B"
printf 'foo/bar\t%s\tv1.0.0\n' "$SHA_B" > "$fx/ls39-tags.tsv"
TAG_MAP="$fx/ls39-tags.tsv" run_fix bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls39" --offline
check "ls39 a tagged step value still classifies its record as an action: the ref gets its tag" \
  "[ \$rc -eq 0 ] && grep -q \"^        ref: 'v1.0.0'\$\" '$fx/ls39/.github/workflows/actions.lock'"

bash "$fleet/scripts/tests/fix-workflow-resources.sh"

echo "fix-workflow-pins: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
