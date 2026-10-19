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
# What it does, per workflow file, for every list item that holds a step
# whose `uses:` is dtolnay/rust-toolchain@<40-hex>:
#   * the step is inspected whole — from its opening `- ` line, so a `with:`
#     written above `uses:` counts (YAML mapping keys carry no order);
#   * only the step's `with:` mapping decides: `toolchain:` under `env:` or any
#     other key is not an action input and does not count;
#   * a block `with:` (with or without a trailing comment) that names no
#     `toolchain:` gets `toolchain: <TOOLCHAIN>` (default `stable`) as its
#     first key, at the indentation its other keys use;
#   * a flow `with: { ... }` without `toolchain` gets it as its first entry;
#   * a step with no `with:` at all gets one, directly under `uses:`;
#   * a tag-pinned step (`@stable`, `@1.85.0`) and a step that already names a
#     toolchain are left alone.
# Idempotent: a second run changes nothing.
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
    local f="$1" tmp
    [ -f "$f" ] || { echo "SKIP (not a file): $f" >&2; return 0; }
    grep -qE '["'"'"']?uses["'"'"']?[[:space:]]*:[[:space:]]*["'"'"']?dtolnay/rust-toolchain@[0-9a-f]{40}' "$f" || return 0

    tmp=$(mktemp)
    awk -v toolchain="$TOOLCHAIN" '
        function indent_of(s,   t) { t = s; sub(/[^ ].*$/, "", t); return length(t) }
        # Column of a key, with a list dash counted as indentation: `  - with:` -> 4.
        function key_indent_of(s,   t) { t = s; sub(/[A-Za-z"'"'"'].*$/, "", t); gsub(/-/, " ", t); return t }
        function plain(s) { return (s !~ /^[[:space:]]*$/ && s !~ /^[[:space:]]*#/) }
        function spaces(n,   t) { t = ""; while (n-- > 0) t = t " "; return t }
        # Emit the buffered list item, patched if it is a SHA-pinned dtolnay step
        # whose `with:` names no toolchain.
        function flush(   i, line, ind, uses_i, has_with, with_i, with_flow, with_indent, in_with, child_indent, has_tc, key_indent) {
            if (n == 0) return
            uses_i = 0
            for (i = 1; i <= n; i++) if (buf[i] ~ USES_RE) { uses_i = i; break }
            if (!uses_i) { for (i = 1; i <= n; i++) print buf[i]; n = 0; return }
            has_with = 0; with_i = 0; with_flow = 0; with_indent = -1; in_with = 0; child_indent = -1; has_tc = 0
            for (i = 1; i <= n; i++) {
                line = buf[i]; ind = indent_of(line)
                if (in_with) {
                    if (plain(line) && ind <= with_indent) in_with = 0
                    else if (plain(line)) {
                        if (child_indent < 0) child_indent = ind
                        if (line ~ /^[[:space:]]*["'"'"']?toolchain["'"'"']?[[:space:]]*:[[:space:]]*[^[:space:]]/) has_tc = 1
                    }
                }
                if (!in_with && !has_with && line ~ /^[[:space:]]*(-[[:space:]]+)?with:[[:space:]]*(#.*)?$/) {
                    has_with = 1; with_i = i; with_indent = length(key_indent_of(line)); in_with = 1
                } else if (!in_with && !has_with && line ~ /^[[:space:]]*(-[[:space:]]+)?with:[[:space:]]*\{/) {
                    has_with = 1; with_i = i; with_flow = 1
                    if (line ~ /[{,][[:space:]]*["'"'"']?toolchain["'"'"']?[[:space:]]*:[[:space:]]*[^[:space:],}]/) has_tc = 1
                }
            }
            if (has_tc) { for (i = 1; i <= n; i++) print buf[i]; n = 0; return }
            for (i = 1; i <= n; i++) {
                line = buf[i]
                if (has_with && with_flow && i == with_i) {
                    if (line ~ /\{[[:space:]]*\}/) sub(/\{[[:space:]]*\}/, "{ toolchain: " toolchain " }", line)
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
            n = 0; in_step = 0; fixed = 0
            USES_RE = "^[[:space:]]*(-[[:space:]]+)?[\"'"'"']?uses[\"'"'"']?[[:space:]]*:[[:space:]]*[\"'"'"']?dtolnay/rust-toolchain@[0-9a-f]+"
        }
        {
            if (in_step) {
                # The item ends at the next list item at its own indentation (or
                # shallower), or at a shallower key. Blank and comment lines and
                # deeper lines belong to it.
                ind = indent_of($0)
                if ($0 ~ /^[[:space:]]*-([[:space:]]|$)/ && ind <= step_indent) { flush(); in_step = 0 }
                else if (plain($0) && ind < step_indent) { flush(); in_step = 0 }
            }
            if (!in_step && $0 ~ /^[[:space:]]*-([[:space:]]|$)/) { in_step = 1; step_indent = indent_of($0); buf[++n] = $0; next }
            if (in_step) { buf[++n] = $0; next }
            print
        }
        END { flush(); if (fixed) printf "FIXED  %s  steps=%d  toolchain=%s\n", FILENAME, fixed, toolchain > "/dev/stderr" }
    ' "$f" > "$tmp"

    if ! cmp -s "$f" "$tmp"; then mv "$tmp" "$f"; else rm -f "$tmp"; echo "OK (every SHA-pinned dtolnay step names a toolchain): $f"; fi
}

if [ -d "$TARGET" ]; then
    for f in "$TARGET"/.github/workflows/*.yml "$TARGET"/.github/workflows/*.yaml; do
        [ -f "$f" ] || continue
        apply_to_file "$f"
    done
else
    apply_to_file "$TARGET"
fi
