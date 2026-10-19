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
# What it does, per workflow file:
#   * a step whose `uses:` is dtolnay/rust-toolchain@<40-hex> and whose
#     `with:` block has no `toolchain:` key gets `toolchain: <TOOLCHAIN>`
#     (default `stable`) as the first key of that block;
#   * a step with no `with:` block at all gets one, directly under `uses:`;
#   * a tag-pinned step (`@stable`, `@1.85.0`) and a step that already
#     names a toolchain are left alone.
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
    grep -qE 'uses:[[:space:]]*["'"'"']?dtolnay/rust-toolchain@[0-9a-f]{40}' "$f" || return 0

    tmp=$(mktemp)
    awk -v toolchain="$TOOLCHAIN" '
        # Emit the buffered step, patched if it needs it.
        function flush(   i, has_with, has_tc, with_indent, out) {
            if (n == 0) return
            has_with = 0; has_tc = 0; with_indent = ""
            for (i = 1; i <= n; i++) {
                if (buf[i] ~ /^[[:space:]]*with:[[:space:]]*$/) { has_with = 1; with_indent = buf[i]; sub(/with:.*$/, "", with_indent) }
                if (buf[i] ~ /^[[:space:]]*toolchain:[[:space:]]*[^[:space:]]/) has_tc = 1
            }
            if (has_tc) { for (i = 1; i <= n; i++) print buf[i]; n = 0; return }
            for (i = 1; i <= n; i++) {
                print buf[i]
                if (has_with && buf[i] ~ /^[[:space:]]*with:[[:space:]]*$/) {
                    print with_indent "  toolchain: " toolchain
                    fixed++
                } else if (!has_with && buf[i] ~ /^[[:space:]]*(-[[:space:]]+)?uses:[[:space:]]*["'"'"']?dtolnay\/rust-toolchain@[0-9a-f]{40}/) {
                    # `with:` sits at the indentation of the step'"'"'s keys: the
                    # `uses:` column when `uses:` opened the step with `- `, else
                    # the `uses:` line'"'"'s own indentation.
                    key_indent = buf[i]; sub(/uses:.*$/, "", key_indent); gsub(/-/, " ", key_indent)
                    print key_indent "with:"
                    print key_indent "  toolchain: " toolchain
                    fixed++
                }
            }
            n = 0
        }
        BEGIN { n = 0; in_step = 0; fixed = 0 }
        {
            if (in_step) {
                # The step ends at the next list item at the step'"'"'s own indentation
                # (or shallower), or at a shallower key, or at a blank line followed
                # by such a line. Comments and deeper lines belong to the step.
                ind = $0; sub(/[^ ].*$/, "", ind)
                if ($0 ~ /^[[:space:]]*-[[:space:]]/ && length(ind) <= length(step_indent)) { flush(); in_step = 0 }
                else if ($0 !~ /^[[:space:]]*$/ && $0 !~ /^[[:space:]]*#/ && length(ind) < length(step_indent)) { flush(); in_step = 0 }
            }
            if (!in_step && $0 ~ /^[[:space:]]*(-[[:space:]]+)?uses:[[:space:]]*["'"'"']?dtolnay\/rust-toolchain@[0-9a-f]{40}/) {
                in_step = 1; step_indent = $0; sub(/[^ ].*$/, "", step_indent)
                # A `- name:` line may have opened this step above; it was already
                # printed, which is fine: we only ever insert below `uses:`.
                buf[++n] = $0; next
            }
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
