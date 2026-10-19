#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# fix-rust-toolchain-input.sh — give every SHA-pinned dtolnay/rust-toolchain
# step a `toolchain:` input. Hypatia rule WF026.
#
# dtolnay/rust-toolchain reads the toolchain from its `@`-ref (`@stable`,
# `@1.85.0`). A full-SHA ref carries no version, so a SHA-pinned step with no
# `with: toolchain:` fails its first step with
#     'toolchain' is a required input
# before any cargo command runs — and the red reads as a code failure.
# plasma-parser-writer's `rust-ci / Cargo check + clippy + fmt` was red this
# way from 2026-09-29 to 2026-10-14 (plasma-parser-writer#105); standards'
# rust-ci-reusable.yml carried the defect from d73c93e0 to 49ed4715
# (fixed by standards#439, 9fe0d8ff).
#
# What it does, per workflow file, for every list item under a `steps:` key
# that holds a step whose `uses:` is dtolnay/rust-toolchain@<40-hex>:
#   * only a list item whose parent key is `steps:` is a step: a list item
#     inside a block scalar (`run: |`, a fixture kept in `env:`) or under any
#     other key is text and is never touched;
#   * the step is inspected whole — from its opening `- ` line (a bare `-` on
#     its own line opens a step too), so a `with:` written above `uses:`
#     counts (YAML mapping keys carry no order);
#   * only a `uses:` that is a KEY OF THE STEP MAPPING is the step's action:
#     the same text nested under `env:`, inside a `run: |` script that writes
#     a fixture, or under any other key is data and is left alone;
#   * only the step's `with:` mapping decides, and only its DIRECT children:
#     `toolchain:` under `env:` or any other key is not an action input, and
#     the same text inside another input's block scalar (`components: |`) is
#     text, not a key, and does not count either;
#   * a block `with:` (with or without a trailing comment) that names no
#     `toolchain:` gets `toolchain: <TOOLCHAIN>` (default `stable`) as its
#     first key, at the indentation its other keys use;
#   * a flow `with: { ... }` — on one line or spread over several up to its
#     closing brace — without `toolchain` gets it as its first entry;
#   * a step with no `with:` at all gets one, directly under `uses:`;
#   * keys may be quoted (`"uses":`, "with":) — the same key to GitHub;
#   * a tag-pinned step (`@stable`, `@1.85.0`) and a step that already names a
#     toolchain are left alone;
#   * a step written as a flow mapping (`- { name: x, uses: ... }`) is NOT
#     rewritten: the script names the line as UNSUPPORTED and exits 2 rather
#     than report the file as clean. Fix it by hand.
# Idempotent: a second run changes nothing.
#
# Exit 0 whether or not anything changed. Exit 2 when some file holds a
# SHA-pinned step the parser cannot place as a block-style step mapping:
# that file is left untouched (other files are still fixed) and a dispatcher
# must not record the run as a clean success.
#
# Usage:
#   fix-rust-toolchain-input.sh <workflow.yml>
#   fix-rust-toolchain-input.sh <repo-path>   (every .github/workflows/*.yml)
#   TOOLCHAIN=1.85.0 fix-rust-toolchain-input.sh <repo-path>
#
# Pairs with Hypatia.Rules.WorkflowAudit.check_sha_pinned_toolchain_without_input/1
# (WF026) and recipe id `WF026` in fix-script-registry.json.

set -euo pipefail

TOOLCHAIN="${TOOLCHAIN:-stable}"
TARGET="${1:?Usage: fix-rust-toolchain-input.sh <workflow.yml | repo-path>}"

apply_to_file() {
    local f="$1" tmp rc
    [ -f "$f" ] || { echo "SKIP (not a file): $f" >&2; return 0; }
    grep -qE 'dtolnay/rust-toolchain@[0-9a-f]{40}' "$f" || return 0

    tmp=$(mktemp)
    set +e
    awk -v toolchain="$TOOLCHAIN" '
        function indent_of(s,   t) { t = s; sub(/[^ ].*$/, "", t); return length(t) }
        # Column of a key, with a list dash counted as indentation: `  - with:` -> 4.
        function key_indent_of(s,   t) { t = s; sub(/[A-Za-z"'"'"'].*$/, "", t); gsub(/-/, " ", t); return t }
        function plain(s) { return (s !~ /^[[:space:]]*$/ && s !~ /^[[:space:]]*#/) }
        function spaces(n,   t) { t = ""; while (n-- > 0) t = t " "; return t }
        # A 40-hex pin of the action on a non-comment line, trailing comment
        # stripped. `@stable` and `@1.85.0` are tags: the ref carries the
        # channel and the step is not ours.
        function sha_ref(s,   t, m) {
            if (!plain(s)) return 0
            t = s; sub(/[[:space:]]#.*$/, "", t)
            if (!match(t, /dtolnay\/rust-toolchain@[0-9a-f]+/)) return 0
            m = substr(t, RSTART + 23, RLENGTH - 23)
            return (length(m) == 40)
        }
        # YAML context outside list items: the key at each indentation, so a
        # list item opens a step only when its parent key is `steps:`.
        function record_key(s,   ind, k, t) {
            ind = indent_of(s)
            t = s; sub(/^[[:space:]]*/, "", t); sub(/[[:space:]]*:.*$/, "", t); gsub(/["'"'"']/, "", t)
            for (k in keyat) if (k + 0 > ind) delete keyat[k]
            keyat[ind] = t
        }
        function parent_is_steps(ind,   k, best) {
            best = -1
            for (k in keyat) if (k + 0 <= ind && k + 0 > best) best = k + 0
            return (best >= 0 && keyat[best] == "steps")
        }
        # Emit the buffered step, patched if it is a SHA-pinned dtolnay step
        # whose `with:` names no toolchain.
        function flush(   i, j, line, ind, uses_i, has_with, with_i, with_flow, with_indent, in_with, in_flow, child_indent, has_tc, key_indent, skey, flow_item) {
            if (n == 0) return
            # The step'"'"'s key column: the opener'"'"'s key (`- uses:` -> the column of
            # `uses`), or for a bare `-` the indentation of the first plain line under it.
            flow_item = (buf[1] ~ /^[[:space:]]*-[[:space:]]*\{/)
            if (buf[1] ~ /^[[:space:]]*-[[:space:]]+[^[:space:]]/) skey = length(key_indent_of(buf[1]))
            else {
                skey = -1
                for (j = 2; j <= n; j++) if (plain(buf[j])) { skey = indent_of(buf[j]); break }
                if (skey < 0) skey = indent_of(buf[1]) + 2
            }
            # Place every SHA-pinned reference in the step: the step'"'"'s own `uses:`
            # key, data nested under some other key (left alone), or something the
            # parser cannot place (a flow-style step) — that is reported, not guessed.
            uses_i = 0
            for (i = 1; i <= n; i++) {
                if (!sha_ref(buf[i])) continue
                if (!flow_item && buf[i] ~ USES_RE && length(key_indent_of(buf[i])) == skey) { if (!uses_i) uses_i = i; continue }
                if (!flow_item && i > 1 && indent_of(buf[i]) > skey) continue
                unrec[++nunrec] = lnum[i]
            }
            if (!uses_i) { for (i = 1; i <= n; i++) print buf[i]; n = 0; return }
            has_with = 0; with_i = 0; with_flow = 0; with_indent = -1; in_with = 0; in_flow = 0; child_indent = -1; has_tc = 0
            for (i = 1; i <= n; i++) {
                line = buf[i]; ind = indent_of(line)
                if (in_flow) {
                    # A flow `with: {` spread over several lines: every entry up to
                    # the closing brace is an input.
                    if (line ~ TC_RE || line ~ TC_FLOW_RE) has_tc = 1
                    if (index(line, "}")) in_flow = 0
                    continue
                }
                if (in_with) {
                    if (plain(line) && ind <= with_indent) in_with = 0
                    else if (plain(line)) {
                        # Only a DIRECT child of `with:` is an input: the contents of
                        # another input'"'"'s block scalar (`components: |`) sit deeper.
                        if (child_indent < 0) child_indent = ind
                        if (ind == child_indent && line ~ TC_RE) has_tc = 1
                    }
                }
                if (!in_with && !has_with && length(key_indent_of(line)) == skey) {
                    if (line ~ WITH_BLOCK_RE) {
                        has_with = 1; with_i = i; with_indent = skey; in_with = 1
                    } else if (line ~ WITH_FLOW_RE) {
                        has_with = 1; with_i = i; with_flow = 1
                        if (line ~ TC_FLOW_RE) has_tc = 1
                        if (!index(line, "}")) in_flow = 1
                    }
                }
            }
            if (has_tc) { for (i = 1; i <= n; i++) print buf[i]; n = 0; return }
            for (i = 1; i <= n; i++) {
                line = buf[i]
                if (has_with && with_flow && i == with_i) {
                    if (line ~ /\{[[:space:]]*\}/) sub(/\{[[:space:]]*\}/, "{ toolchain: " toolchain " }", line)
                    else if (line ~ /\{[[:space:]]*$/) sub(/\{[[:space:]]*$/, "{ toolchain: " toolchain ",", line)
                    else sub(/\{[[:space:]]*/, "{ toolchain: " toolchain ", ", line)
                    print line; fixed++
                } else if (has_with && !with_flow && i == with_i) {
                    print line
                    print spaces(child_indent >= 0 ? child_indent : with_indent + 2) "toolchain: " toolchain
                    fixed++
                } else if (!has_with && i == uses_i) {
                    print line
                    # `with:` sits at the column of the step'"'"'s keys: the `uses:`
                    # column, with a list dash counted as indentation.
                    key_indent = key_indent_of(line)
                    print key_indent "with:"
                    print key_indent "  toolchain: " toolchain
                    fixed++
                } else print line
            }
            n = 0
        }
        BEGIN {
            n = 0; in_step = 0; in_scalar = 0; fixed = 0; nunrec = 0
            Q = "[\"'"'"']?"
            LIST_RE = "^[[:space:]]*-([[:space:]]|$)"
            KEY_RE = "^[[:space:]]*" Q "[A-Za-z0-9_.-]+" Q "[[:space:]]*:([[:space:]]|$)"
            SCALAR_RE = ":[[:space:]]*[|>][-+0-9]*[[:space:]]*(#.*)?$"
            USES_RE = "^[[:space:]]*(-[[:space:]]+)?" Q "uses" Q "[[:space:]]*:[[:space:]]*" Q "dtolnay/rust-toolchain@[0-9a-f]+"
            WITH_BLOCK_RE = "^[[:space:]]*(-[[:space:]]+)?" Q "with" Q "[[:space:]]*:[[:space:]]*(#.*)?$"
            WITH_FLOW_RE = "^[[:space:]]*(-[[:space:]]+)?" Q "with" Q "[[:space:]]*:[[:space:]]*\\{"
            TC_RE = "^[[:space:]]*" Q "toolchain" Q "[[:space:]]*:[[:space:]]*[^[:space:]]"
            TC_FLOW_RE = "[{,][[:space:]]*" Q "toolchain" Q "[[:space:]]*:[[:space:]]*[^[:space:],}]"
        }
        {
            ind = indent_of($0)
            # A block scalar outside any step (`FIXTURE: |` under `env:`) is text
            # until a plain line at its key'"'"'s indentation or shallower.
            if (!in_step && in_scalar) {
                if (plain($0) && ind <= scalar_indent) in_scalar = 0
                else { print; next }
            }
            if (in_step) {
                # The step ends at the next list item at its own indentation (or
                # shallower), or at a shallower key. Blank and comment lines and
                # deeper lines belong to it.
                if ($0 ~ LIST_RE && ind <= step_indent) { flush(); in_step = 0 }
                else if (plain($0) && ind < step_indent) { flush(); in_step = 0 }
            }
            if (in_step) { buf[++n] = $0; lnum[n] = NR; next }
            if ($0 ~ LIST_RE) {
                if (parent_is_steps(ind)) { in_step = 1; step_indent = ind; n = 0; buf[++n] = $0; lnum[n] = NR; next }
                # A list item whose parent is not `steps:` (a matrix, a fixture) is data.
                print; next
            }
            if (plain($0) && $0 ~ KEY_RE) {
                record_key($0)
                if ($0 ~ SCALAR_RE) { in_scalar = 1; scalar_indent = ind }
            } else if (sha_ref($0) && parent_is_steps(ind)) unrec[++nunrec] = NR
            print
        }
        END {
            flush()
            if (nunrec) {
                printf "UNSUPPORTED  %s: %d SHA-pinned dtolnay/rust-toolchain reference(s) the parser cannot place as a block-style step key (a flow-style `- { ... }` step?), line(s):", FILENAME, nunrec > "/dev/stderr"
                for (j = 1; j <= nunrec; j++) printf " %d", unrec[j] > "/dev/stderr"
                printf ". Fix by hand; the file was not changed.\n" > "/dev/stderr"
                exit 2
            }
            if (fixed) printf "FIXED  %s  steps=%d  toolchain=%s\n", FILENAME, fixed, toolchain > "/dev/stderr"
        }
    ' "$f" > "$tmp"; rc=$?
    set -e
    if [ "$rc" -ne 0 ]; then rm -f "$tmp"; return 2; fi

    if ! cmp -s "$f" "$tmp"; then mv "$tmp" "$f"; else rm -f "$tmp"; echo "OK (every SHA-pinned dtolnay step names a toolchain): $f"; fi
}

unsupported=0
if [ -d "$TARGET" ]; then
    for f in "$TARGET"/.github/workflows/*.yml "$TARGET"/.github/workflows/*.yaml; do
        [ -f "$f" ] || continue
        apply_to_file "$f" || unsupported=$((unsupported + 1))
    done
else
    apply_to_file "$TARGET" || unsupported=$((unsupported + 1))
fi
if [ "$unsupported" -ne 0 ]; then
    echo "fix-rust-toolchain-input: $unsupported file(s) left untouched (UNSUPPORTED above); not a clean success." >&2
    exit 2
fi
