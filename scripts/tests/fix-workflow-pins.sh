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

# ── 3. fix-lock-self-sha-refs.sh ─────────────────────────────────────────────
mkdir -p "$fx/ls/.github/workflows"
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
printf 'version: "v0.0.3"\nworkflows:\n    ".github/workflows/ci.yml":\n        - "actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1"\ndependencies:\n    "actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1":\n        ref: "3d3c42e5aac5ba805825da76410c181273ba90b1"\n        commit: "sha1-3d3c42e5aac5ba805825da76410c181273ba90b1"\n        owner_id: 44036562\n        repo_id: 197814629\n' > "$fx/ls6/.github/workflows/actions.lock"
set +e
bash "$fleet/scripts/fix-lock-self-sha-refs.sh" "$fx/ls6" --offline > "$fx/ls6.out" 2>&1; rc6=$?
set -e
check "a double-quoted self-SHA record is parsed and repaired, keeping its quotes" \
  "[ \$rc6 -eq 0 ] && grep -q 'ref: \"v7.0.1\"' '$fx/ls6/.github/workflows/actions.lock' && grep -q 'FIXED  actions/checkout' '$fx/ls6.out'"
# A tag Git allows but this fixer will not write (`&`, `|`, a quote) is refused.
SHA_C=cccccccccccccccccccccccccccccccccccccccc
mkdir -p "$fx/ls7/.github/workflows"
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

# Portability (review): Debian/Ubuntu's default awk is mawk. Run the same
# fixture through mawk when it is installed, and refuse a lock the parser
# cannot read instead of reporting OK.
if command -v mawk >/dev/null; then
    mkdir -p "$fx/mawk-bin" "$fx/ls2/.github/workflows"; ln -sf "$(command -v mawk)" "$fx/mawk-bin/awk"
    cp "$fx/ls1.lock" "$fx/ls2/.github/workflows/actions.lock"
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

echo "fix-workflow-pins: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
