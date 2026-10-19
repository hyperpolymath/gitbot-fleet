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
pass=0; fail=0
ok()   { pass=$((pass + 1)); echo "  ok   $1"; }
bad()  { fail=$((fail + 1)); echo "  FAIL $1" >&2; }
check(){ if eval "$2"; then ok "$1"; else bad "$1"; fi; }

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
out=$(bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp" 2>&1)
check "Window A pin of rust-ci-reusable is moved to the replacement" "grep -q \"rust-ci-reusable.yml@$GOOD\" '$fx/sp/.github/workflows/rust-ci.yml'"
check "Window B pin of any standards reusable is moved (wildcard row)" "grep -q \"governance-reusable.yml@$GOOD\" '$fx/sp/.github/workflows/governance.yml'"
check "a Window-A SHA on a reusable the window does not cover is left alone" "grep -q \"mirror-reusable.yml@$STALE_A\" '$fx/sp/.github/workflows/mirror.yml'"
check "a cicd-suite pin that happens to share the SHA is not a standards pin and is untouched" "grep -q \"main-estate-audit.yml@$STALE_B\" '$fx/sp/.github/workflows/audit.yml'"
check "the lock's workflows entry and record follow the pin" "[ \$(grep -c \"$GOOD\" '$fx/sp/.github/workflows/actions.lock') -eq 4 ] && ! grep -q \"$STALE_B\" '$fx/sp/.github/workflows/actions.lock'"
check "touching the lock is said out loud" "grep -q 'nested uses: list was NOT recomputed' <<<\"\$out\""
tar -C "$fx/sp" -cf "$fx/sp1.tar" .github
bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp" >/dev/null 2>&1
tar -C "$fx/sp" -cf "$fx/sp2.tar" .github
check "second run is a no-op" "cmp -s '$fx/sp1.tar' '$fx/sp2.tar'"
# Planted control: an empty table rewrites nothing.
printf '# empty\n' > "$fx/empty.tsv"
mkdir -p "$fx/sp0/.github/workflows"; printf 'jobs:\n  rust-ci:\n    uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@%s\n' "$STALE_A" > "$fx/sp0/.github/workflows/rust-ci.yml"
PINS_TABLE="$fx/empty.tsv" bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp0" >/dev/null 2>&1
check "control: with no table rows nothing is rewritten" "grep -q \"@$STALE_A\" '$fx/sp0/.github/workflows/rust-ci.yml'"
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
bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp2" >/dev/null 2>&1
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
out3=$(bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp3" 2>&1)
l3="$fx/sp3/.github/workflows/actions.lock"
check "partial migration: the rewritten caller's lock edge moves" \
  "grep -A1 \"'.github/workflows/rust-ci.yml':\" '$l3' | grep -q \"hyperpolymath/standards@$GOOD\""
check "partial migration: the untouched caller's lock edge stays at the stale SHA" \
  "grep -A1 \"'.github/workflows/mirror.yml':\" '$l3' | grep -q \"hyperpolymath/standards@$STALE_A\" && grep -q \"mirror-reusable.yml@$STALE_A\" '$fx/sp3/.github/workflows/mirror.yml'"
check "partial migration: both dependency records exist, each with its own ref and commit" \
  "[ \$(grep -c \"^    'hyperpolymath/standards@\" '$l3') -eq 2 ] && grep -q \"ref: '$GOOD'\" '$l3' && grep -q \"ref: '$STALE_A'\" '$l3' && grep -q \"commit: 'sha1-$GOOD'\" '$l3'"
check "partial migration is said out loud" "grep -q 'keeps the record for' <<<\"\$out3\""
tar -C "$fx/sp3" -cf "$fx/sp3a.tar" .github
bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp3" >/dev/null 2>&1
tar -C "$fx/sp3" -cf "$fx/sp3b.tar" .github
check "partial migration: second run is a no-op" "cmp -s '$fx/sp3a.tar' '$fx/sp3b.tar'"
# A quoted key and value are still a pin.
mkdir -p "$fx/sp4/.github/workflows"
printf 'jobs:\n  governance:\n    "uses": "hyperpolymath/standards/.github/workflows/governance-reusable.yml@%s"\n' "$STALE_B" > "$fx/sp4/.github/workflows/governance.yml"
bash "$fleet/scripts/fix-stale-reusable-pin.sh" "$fx/sp4" >/dev/null 2>&1
check "a quoted \"uses\" key with a quoted value is rewritten" "grep -q \"\\\"hyperpolymath/standards/.github/workflows/governance-reusable.yml@$GOOD\\\"\" '$fx/sp4/.github/workflows/governance.yml'"

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

echo "fix-workflow-pins: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
