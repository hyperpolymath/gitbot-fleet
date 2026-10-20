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
#   * the action is matched in either case (`Dtolnay/Rust-Toolchain@<sha>`
#     is the same action to GitHub) and its spelling is kept;
#   * a tag-pinned step (`@stable`, `@1.85.0`) and a step that already names a
#     toolchain are left alone;
#   * a step written as a flow mapping (`- { name: x, uses: ... }`), a
#     one-line `steps: [ { uses: ... } ]`, a `uses:` whose value continues
#     below its key (`uses: >-`, or the reference alone on the next line),
#     a `uses: *alias` of an anchored pinned reference, or a `with:` whose
#     value this
#     parser does not read (`with: &anchor`, `with: *alias`) is NOT
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
#     (a toolchain name only — letters, digits, `.`, `_`, `-`; anything else
#     exits 2 having changed nothing)
#
# Pairs with Hypatia.Rules.WorkflowAudit.check_sha_pinned_toolchain_without_input/1
# (WF026) and recipe id `WF026` in fix-script-registry.json.

set -euo pipefail

TOOLCHAIN="${TOOLCHAIN:-stable}"
# The override is written into the workflow as a YAML value, so only a
# rustup toolchain name is accepted (`stable`, `1.85.0`, `nightly-2026-01-01`,
# `stable-x86_64-unknown-linux-gnu`): never text that could end the value,
# add a key or reach awk's replacement syntax. A name YAML would read as
# anything but a string is written single-quoted: a number (`1.80` is the
# float 1.8, `0x1f` an integer) or a null or boolean spelling (`null`, `true`,
# `no`, ... as YAML 1.1 and the core schema read them), which would otherwise
# leave the input empty or wrong.
if [[ ! "$TOOLCHAIN" =~ ^[A-Za-z0-9][A-Za-z0-9._-]*$ ]]; then
    printf 'fix-rust-toolchain-input: TOOLCHAIN=%q is not a toolchain name (letters, digits, ., _, -); nothing was changed\n' "$TOOLCHAIN" >&2
    exit 2
fi
TARGET="${1:?Usage: fix-rust-toolchain-input.sh <workflow.yml | repo-path>}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# One workflow-pin fixer per checkout at a time (see lib/pin-fixer-lock.sh):
# a single workflow file is locked by the checkout it belongs to — found from
# the file'"'"'s directory (`<checkout>/.github/workflows/<file>`, relative or
# absolute), so every fixer on that checkout contends for the same lock.
. "$SCRIPT_DIR/lib/pin-fixer-lock.sh"
TOOLCHAIN_YAML="$TOOLCHAIN"
if pin_fixer_yaml_nonstring "$TOOLCHAIN"; then TOOLCHAIN_YAML="'$TOOLCHAIN'"; fi
# anchor_defs/2: the anchors a line defines, outside quoted scalars and comments.
ANCHORS_AWK=$(cat "$SCRIPT_DIR/lib/yaml-anchors.awk")
if [ -d "$TARGET" ]; then
    pin_fixer_lock "$TARGET"
else
    target_dir=$(cd "$(dirname "$TARGET")" 2>/dev/null && pwd -P || dirname "$TARGET")
    case "$target_dir" in
        */.github/workflows) pin_fixer_lock "${target_dir%/.github/workflows}" ;;
        *) pin_fixer_lock "$target_dir" ;;
    esac
fi

# Add missing TOOLCHAIN inputs to SHA-pinned Rust toolchain steps in file $1.
# Return 0 for a repair or no-op; return 2 and leave the file untouched if
# parsing fails or a pinned reference uses an unsupported layout.
apply_to_file() {
    local f="$1" tmp rc
    [ -f "$f" ] || { echo "SKIP (not a file): $f" >&2; return 0; }
    # GitHub resolves `Dtolnay/Rust-Toolchain` to the same action: the name is
    # matched in either case here and in the parser below.
    grep -qiE 'dtolnay/rust-toolchain@[0-9a-fA-F]{40}' "$f" || return 0

    tmp=$(mktemp)
    set +e
    awk -v toolchain="$TOOLCHAIN_YAML" "$ANCHORS_AWK"'
        # Return the number of leading spaces in source line s.
        function indent_of(s,   t) { t = s; sub(/[^ ].*$/, "", t); return length(t) }
        # Column of a key, with a list dash counted as indentation: `  - with:` -> 4.
        function key_indent_of(s,   t) { t = s; sub(/[A-Za-z"'"'"'].*$/, "", t); gsub(/-/, " ", t); return t }
        # Return true when s is neither blank nor a full-line comment.
        function plain(s) { return (s !~ /^[[:space:]]*$/ && s !~ /^[[:space:]]*#/) }
        # Return a string of n spaces for an inserted YAML line.
        function spaces(n,   t) { t = ""; while (n-- > 0) t = t " "; return t }
        # Only text OUTSIDE quoted scalars and comments is code: a
        # `components: "}"` or a `# }` is text, not the end of the mapping.
        function code_only(s,   t) { t = s; gsub(/"([^"\\]|\\.)*"/, "\"\"", t); gsub(/'"'"'[^'"'"']*'"'"'/, "'"'"''"'"'", t); sub(/(^|[[:space:]])#.*$/, "", t); return t }
        # The inputs one line names at the `with:` flow mapping'"'"'s OWN level,
        # walked from brace depth `depth` — 0 on the `with: {` line, the depth
        # the previous line left on a continuation line. A flow mapping closes
        # when the depth returns to zero, not at the first `}`: an expression
        # `${{ matrix.x }}` opens and closes its own braces (its `$` stays as
        # the value'"'"'s text), and a nested mapping and its contents are dropped
        # with its braces. The brace that opens the mapping and the one that
        # closes it are kept. A quoted VALUE becomes the one-letter text `"v"`
        # (a `"clippy, toolchain: nightly"` is text, a quoted key is still a
        # key) while an empty one (`""`, `'"'"''"'"'`) stays empty, and a comment ends
        # the line. Text BEFORE a nested group closes is hidden; text AFTER it
        # on the same line (`channel: beta }, toolchain: nightly`) is read at
        # the mapping'"'"'s level like any other entry. The depth the line leaves
        # is left in lv_depth: the walk that reads the entries is the one that
        # counts the braces, so the two can never disagree.
        function level_view(s, depth,   out, i, n, c, j, body) {
            out = ""; n = length(s); i = 1
            while (i <= n) {
                c = substr(s, i, 1)
                if (c == "#" && (i == 1 || substr(s, i - 1, 1) ~ /[[:space:]]/)) break
                if (c == "\"" || c == "'"'"'") {
                    j = i + 1
                    if (c == "\"") { while (j <= n && substr(s, j, 1) != "\"") { if (substr(s, j, 1) == "\\") j++; j++ } }
                    else { while (j <= n && substr(s, j, 1) != "'"'"'") j++ }
                    body = substr(s, i + 1, j - i - 1)
                    if (depth == 1) {
                        if (substr(s, j + 1) ~ /^[[:space:]]*:/) out = out c body c
                        else out = out c (body == "" ? "" : "v") c
                    }
                    i = j + 1; continue
                }
                if (c == "{") { depth++; if (depth == 1) out = out c }
                else if (c == "}") { if (depth == 1) out = out c; depth-- }
                else if (depth == 1) out = out c
                i++
            }
            lv_depth = depth
            return out
        }
        # A 40-hex pin of the action on a non-comment line, trailing comment
        # stripped. `@stable` and `@1.85.0` are tags: the ref carries the
        # channel and the step is not ours.
        # The 40 hex characters must END the ref: `@<sha>-patched` is a tag
        # that merely begins with one, and carries its own channel. Either
        # case, for the commit ID (git resolves upper-case hex too) and for the
        # action name (GitHub resolves `Dtolnay/Rust-Toolchain` too).
        function sha_ref(s,   t, m, nxt) {
            if (!plain(s)) return 0
            t = tolower(s); sub(/[[:space:]]#.*$/, "", t)
            if (!match(t, /dtolnay\/rust-toolchain@[0-9a-f]+/)) return 0
            m = substr(t, RSTART + 23, RLENGTH - 23)
            nxt = substr(t, RSTART + RLENGTH, 1)
            return (length(m) == 40 && (nxt == "" || nxt ~ /["'"'"'[:space:],}\]]/))
        }
        # The line with its comment dropped and every quoted scalar that is
        # neither a key nor the value of a `uses` key (past any anchor or tag)
        # blanked to `""`.
        function site_view(s,   out, i, n, c, j, body) {
            out = ""; n = length(s); i = 1
            while (i <= n) {
                c = substr(s, i, 1)
                if (c == "#" && (i == 1 || substr(s, i - 1, 1) ~ /[[:space:]]/)) break
                if (c == "\"" || c == "'"'"'") {
                    j = i + 1
                    if (c == "\"") { while (j <= n && substr(s, j, 1) != "\"") { if (substr(s, j, 1) == "\\") j++; j++ } }
                    else { while (j <= n && substr(s, j, 1) != "'"'"'") j++ }
                    body = substr(s, i + 1, j - i - 1)
                    if (substr(s, j + 1) ~ /^[[:space:]]*:/ || out ~ /(^|[{,[:space:]])["'"'"']?uses["'"'"']?[[:space:]]*:[[:space:]]*([&!][^[:space:],}]*[[:space:]]+)*$/) out = out c body c
                    else out = out c c
                    i = j + 1; continue
                }
                out = out c; i++
            }
            return out
        }
        # A SHA pin that is the value of a `uses` key, block or flow: the
        # only pin the parser can be asked to place. The same text inside
        # another key'"'"'s string (`name: "fixture dtolnay/...@<sha>"`) is data.
        function uses_pin(s) { return sha_ref(s) && tolower(site_view(s)) ~ USES_SITE_RE }
        # Buffer line $0 of the current step as buf[n], with the verdict on
        # its `uses:` alias, if any, as the anchors read so far give it —
        # YAML resolves an alias to the latest anchor of that name BEFORE it,
        # so the verdict cannot wait for the end of the step, when a later
        # line may have redefined the name: "bad" when the anchor holds a
        # SHA-pinned toolchain reference or no anchor of that name has been
        # seen, else "ok". Anchor names are case-sensitive: the key is
        # matched in lower case, the name is taken as written.
        function buffer(   v, m) {
            buf[++n] = $0; lnum[n] = NR; alias_state[n] = ""
            v = site_view($0)
            if (match(tolower(v), USES_ALIAS_RE)) {
                m = substr(v, RSTART, RLENGTH); sub(/^.*\*/, "", m)
                alias_state[n] = ((m in tc_anchor) || !(m in any_anchor)) ? "bad" : "ok"
            }
        }
        # YAML context outside list items: the key at each indentation, so a
        # list item opens a step only when its parent key is `steps:`.
        function record_key(s,   ind, k, t) {
            ind = indent_of(s)
            t = s; sub(/^[[:space:]]*/, "", t); sub(/[[:space:]]*:.*$/, "", t); gsub(/["'"'"']/, "", t)
            for (k in keyat) if (k + 0 > ind) delete keyat[k]
            keyat[ind] = t
        }
        # Return true if the nearest recorded key at or above ind is steps.
        function parent_is_steps(ind,   k, best) {
            best = -1
            for (k in keyat) if (k + 0 <= ind && k + 0 > best) best = k + 0
            return (best >= 0 && keyat[best] == "steps")
        }
        # Return true if the nearest recorded key strictly above ind is steps:
        # a key line inside a flow-style step spread over several lines.
        function under_steps(ind,   k, best) {
            best = -1
            for (k in keyat) if (k + 0 < ind && k + 0 > best) best = k + 0
            return (best >= 0 && keyat[best] == "steps")
        }
        # Emit the buffered step, patched if it is a SHA-pinned dtolnay step
        # whose `with:` names no toolchain.
        function flush(   i, j, line, ind, uses_i, has_with, with_bad, with_i, with_flow, with_indent, in_with, in_flow, flow_depth, child_indent, has_tc, key_indent, skey, flow_item, tc_empty_i, tc_empty_flow, tc_empty_eol, tc_nested, tc_await, tc_flow_await, m, pre, post, delim, comment, v, w) {
            if (n == 0) return
            # The step'"'"'s key column: the opener'"'"'s key (`- uses:` -> the column of
            # `uses`), or for a bare `-` — alone, or followed only by a comment
            # (`- # install Rust`) — the indentation of the first plain line under it.
            flow_item = (buf[1] ~ /^[[:space:]]*-[[:space:]]*\{/)
            if (buf[1] ~ /^[[:space:]]*-[[:space:]]+[^[:space:]#]/) skey = length(key_indent_of(buf[1]))
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
                # The step'"'"'s own `uses:` with its value continued below the key
                # (`uses: >-` then `dtolnay/rust-toolchain@<sha>`): the key line
                # names no action and the value reads as nested text, so a SHA
                # pin there is reported, never passed over as a step that is fine.
                if (!flow_item && length(key_indent_of(buf[i])) == skey && tolower(site_view(buf[i])) ~ USES_OPEN_RE)
                    for (j = i + 1; j <= n && !(plain(buf[j]) && indent_of(buf[j]) <= skey); j++)
                        if (sha_ref(buf[j])) unrec[++nunrec] = lnum[j]
                # The step'"'"'s own `uses:` given as an alias (`uses: *rust`) of an
                # anchor that holds a SHA-pinned toolchain reference: the step
                # runs that action, but the reference is not where the parser
                # places a step'"'"'s action. Reported, never passed over.
                # An alias of no anchor read here could be that reference too.
                # The verdict was taken when the line was read (alias_state):
                # an alias names the anchor defined before it, not one a later
                # line of the step defines again under the same name.
                if ((flow_item || length(key_indent_of(buf[i])) == skey) && alias_state[i] == "bad") unrec[++nunrec] = lnum[i]
                if (!sha_ref(buf[i])) continue
                if (!flow_item && tolower(buf[i]) ~ USES_RE && length(key_indent_of(buf[i])) == skey) { if (!uses_i) uses_i = i; continue }
                if (!flow_item && i > 1 && indent_of(buf[i]) > skey) continue
                if (!uses_pin(buf[i])) continue
                unrec[++nunrec] = lnum[i]
            }
            if (!uses_i) { for (i = 1; i <= n; i++) print buf[i]; n = 0; return }
            has_with = 0; with_bad = 0; with_i = 0; with_flow = 0; with_indent = -1; in_with = 0; in_flow = 0; child_indent = -1; has_tc = 0
            tc_empty_i = 0; tc_empty_flow = 0; tc_empty_eol = 0; tc_nested = 0; tc_await = 0; tc_flow_await = 0
            for (i = 1; i <= n; i++) {
                line = buf[i]; ind = indent_of(line)
                if (in_flow) {
                    # A flow `with: {` spread over several lines: every entry up to
                    # the brace that closes it is an input. An EMPTY `toolchain:`
                    # entry (`toolchain: ,` / `toolchain: null`) is the key with no
                    # value: it is given one in place, never duplicated. Only text
                    # at the mapping'"'"'s own depth names inputs — outside quoted
                    # values and nested mappings, and including the rest of a line
                    # on which a nested mapping closes.
                    v = level_view(line, flow_depth); flow_depth = lv_depth
                    # A bare `toolchain:` that ended the previous line is empty
                    # only when the next token closes the mapping or moves on
                    # (`}`, `,`); anything else is its value, spread over lines,
                    # and not this fixer'"'"'s to place.
                    if (tc_flow_await) { w = code_only(line); sub(/^[[:space:]]*/, "", w); if (w != "") { tc_flow_await = 0; if (w !~ /^[,}]/) tc_nested = 1 } }
                    if (v ~ TC_FLOW_EMPTY_RE) { tc_empty_i = i; tc_empty_flow = 1 }
                    else if (v ~ TC_FLOW_EOL_RE) { tc_empty_i = i; tc_empty_eol = 1; if (v ~ TC_FLOW_BARE_EOL_RE) tc_flow_await = 1 }
                    else if (v ~ TC_RE || v ~ TC_FLOW_RE) has_tc = 1
                    if (flow_depth <= 0) in_flow = 0
                    continue
                }
                if (in_with) {
                    if (plain(line) && ind <= with_indent) in_with = 0
                    else if (plain(line)) {
                        # Only a DIRECT child of `with:` is an input: the contents of
                        # another input'"'"'s block scalar (`components: |`) sit deeper.
                        # The first must be a block mapping key: `with:` whose value
                        # is a flow mapping, an alias or anything else written on the
                        # next line (`{ components: rustfmt }`, `*inputs`) is a shape
                        # a block child cannot be added to, so the step is reported.
                        if (child_indent < 0) {
                            child_indent = ind
                            if (line !~ WITH_CHILD_KEY_RE) { with_bad = 1; unrec[++nunrec] = lnum[i] }
                        }
                        # An empty `toolchain:` whose next plain line (blank and
                        # comment lines between them notwithstanding) sits deeper is
                        # a nested mapping, not an empty value: nothing this fixer
                        # can repair.
                        if (tc_await) { tc_await = 0; if (ind > child_indent) tc_nested = 1 }
                        if (ind == child_indent && line ~ TC_EMPTY_RE) { tc_empty_i = i; tc_await = 1 }
                        else if (ind == child_indent && line ~ TC_RE) has_tc = 1
                    }
                }
                if (!in_with && !has_with && length(key_indent_of(line)) == skey) {
                    if (line ~ WITH_BLOCK_RE) {
                        has_with = 1; with_i = i; with_indent = skey; in_with = 1
                    } else if (line ~ WITH_FLOW_RE) {
                        has_with = 1; with_i = i; with_flow = 1
                        v = level_view(line, 0); flow_depth = lv_depth
                        if (v ~ TC_FLOW_EMPTY_RE) { tc_empty_i = i; tc_empty_flow = 1 }
                        else if (v ~ TC_FLOW_EOL_RE) { tc_empty_i = i; tc_empty_eol = 1; if (v ~ TC_FLOW_BARE_EOL_RE) tc_flow_await = 1 }
                        else if (v ~ TC_FLOW_RE) has_tc = 1
                        if (flow_depth > 0) in_flow = 1
                    } else if (line ~ WITH_ANY_RE) {
                        # `with: &anchor`, `with: *alias`, or any value shape this
                        # parser does not read: a second `with:` would be worse than
                        # none, so the step is reported, not rewritten.
                        has_with = 1; with_bad = 1; unrec[++nunrec] = lnum[i]
                    }
                }
            }
            if (tc_flow_await) tc_nested = 1
            if (tc_nested) { unrec[++nunrec] = lnum[tc_empty_i]; with_bad = 1 }
            if (tc_empty_i && tc_empty_flow && !match(buf[tc_empty_i], TC_FLOW_EMPTY_RE)) { unrec[++nunrec] = lnum[tc_empty_i]; with_bad = 1 }
            if (tc_empty_i && tc_empty_eol && !match(buf[tc_empty_i], TC_FLOW_EOL_RAW_RE)) { unrec[++nunrec] = lnum[tc_empty_i]; with_bad = 1 }
            if (has_tc || with_bad) { for (i = 1; i <= n; i++) print buf[i]; n = 0; return }
            for (i = 1; i <= n; i++) {
                line = buf[i]
                if (tc_empty_i && i == tc_empty_i) {
                    # The key is there with no value: fill it where it stands.
                    if (tc_empty_flow) {
                        match(line, TC_FLOW_EMPTY_RE)
                        pre = substr(line, 1, RSTART - 1); m = substr(line, RSTART, RLENGTH); post = substr(line, RSTART + RLENGTH)
                        delim = substr(m, length(m))
                        sub("[[:space:]]*" NULLV "[[:space:]]*[,}]$", "", m)
                        line = pre m " " toolchain (delim == "}" ? " }" : ",") post
                    } else {
                        # A block entry, or a flow entry whose delimiter is on
                        # the next line: the value ends the line either way.
                        comment = ""
                        if (match(line, /[[:space:]]+#.*$/)) comment = substr(line, RSTART)
                        sub(":[[:space:]]*" NULLV "[[:space:]]*(#.*)?$", "", line)
                        line = line ": " toolchain comment
                    }
                    print line; fixed++
                } else if (has_with && with_flow && i == with_i && !tc_empty_i) {
                    if (line ~ /\{[[:space:]]*\}/) sub(/\{[[:space:]]*\}/, "{ toolchain: " toolchain " }", line)
                    else if (line ~ /\{[[:space:]]*$/) sub(/\{[[:space:]]*$/, "{ toolchain: " toolchain ",", line)
                    else sub(/\{[[:space:]]*/, "{ toolchain: " toolchain ", ", line)
                    print line; fixed++
                } else if (has_with && !with_flow && i == with_i && !tc_empty_i) {
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
            n = 0; in_step = 0; in_scalar = 0; fixed = 0; nunrec = 0; anc_scalar = -1
            Q = "[\"'"'"']?"
            LIST_RE = "^[[:space:]]*-([[:space:]]|$)"
            KEY_RE = "^[[:space:]]*" Q "[A-Za-z0-9_.-]+" Q "[[:space:]]*:([[:space:]]|$)"
            SCALAR_RE = ":[[:space:]]*[|>][-+0-9]*[[:space:]]*(#.*)?$"
            # Matched against the line in lower case.
            USES_RE = "^[[:space:]]*(-[[:space:]]+)?" Q "uses" Q "[[:space:]]*:[[:space:]]*" Q "dtolnay/rust-toolchain@[0-9a-f]+"
            # The value may carry YAML node properties, an anchor or a tag
            # (`uses: &rust ...`, `uses: !!str ...`): still the step'"'"'s pin, but
            # not one USES_RE places, so such a step is reported.
            USES_SITE_RE = "(^|[-{,[:space:]])" Q "uses" Q "[[:space:]]*:[[:space:]]*([&!][^[:space:],}]*[[:space:]]+)*" Q "dtolnay/rust-toolchain@[0-9a-f]+"
            # A `uses:` whose value is an alias, `*name` (matched in lower case).
            USES_ALIAS_RE = "(^|[-{,[:space:]])" Q "uses" Q "[[:space:]]*:[[:space:]]*\\*[a-z0-9_.-]+"
            # A `uses:` key whose value is not on its line: a block scalar
            # (`uses: >-`) or nothing at all, the reference on the lines below.
            USES_OPEN_RE = "^[[:space:]]*(-[[:space:]]+)?" Q "uses" Q "[[:space:]]*:[[:space:]]*([&!][^[:space:],}]*[[:space:]]*)*([|>][-+0-9]*)?[[:space:]]*$"
            WITH_BLOCK_RE = "^[[:space:]]*(-[[:space:]]+)?" Q "with" Q "[[:space:]]*:[[:space:]]*(#.*)?$"
            WITH_FLOW_RE = "^[[:space:]]*(-[[:space:]]+)?" Q "with" Q "[[:space:]]*:[[:space:]]*\\{"
            WITH_ANY_RE = "^[[:space:]]*(-[[:space:]]+)?" Q "with" Q "[[:space:]]*:"
            WITH_CHILD_KEY_RE = "^[[:space:]]*" Q "[A-Za-z0-9_.-]+" Q "[[:space:]]*:([[:space:]]|$)"
            TC_RE = "^[[:space:]]*" Q "toolchain" Q "[[:space:]]*:[[:space:]]*[^[:space:]]"
            TC_FLOW_RE = "[{,][[:space:]]*" Q "toolchain" Q "[[:space:]]*:[[:space:]]*[^[:space:],}]"
            # The key present with no value — YAML null in the spellings the core
            # schema admits unquoted (`toolchain:`, `null`, `Null`, `NULL`, `~`)
            # or the empty string (`""`, `'"'"''"'"'`): not an input the action can
            # read, and not a key to write twice. `"null"` quoted is the string
            # null, a value.
            NULLV = "(null|Null|NULL|~|\"\"|'"'"''"'"')?"
            TC_EMPTY_RE = "^[[:space:]]*" Q "toolchain" Q "[[:space:]]*:[[:space:]]*" NULLV "[[:space:]]*(#.*)?$"
            TC_FLOW_EMPTY_RE = "(^|[{,])[[:space:]]*" Q "toolchain" Q "[[:space:]]*:[[:space:]]*" NULLV "[[:space:]]*[,}]"
            # The same entry with its delimiter on the next line of a multi-line
            # flow mapping (`toolchain: null` then `}`); a BARE key there waits
            # for the next token, which must be that delimiter.
            TC_FLOW_EOL_RE = "(^|[{,])[[:space:]]*" Q "toolchain" Q "[[:space:]]*:[[:space:]]*" NULLV "[[:space:]]*$"
            TC_FLOW_EOL_RAW_RE = "(^|[{,])[[:space:]]*" Q "toolchain" Q "[[:space:]]*:[[:space:]]*" NULLV "[[:space:]]*(#.*)?$"
            TC_FLOW_BARE_EOL_RE = "(^|[{,])[[:space:]]*" Q "toolchain" Q "[[:space:]]*:[[:space:]]*$"
        }
        {
            ind = indent_of($0)
            # Every anchor whose value is a SHA-pinned toolchain reference
            # (`ACTION: &rust dtolnay/rust-toolchain@<sha>`), wherever it is
            # defined: an alias of it is that reference. Anchors are read by
            # lib/yaml-anchors.awk (outside quoted scalars and comments) and
            # never from a block scalar'"'"'s content (`run: |` text); a later
            # anchor of the same name replaces an earlier one.
            if (anc_scalar >= 0 && (!plain($0) || ind > anc_scalar)) { }
            else {
                anc_scalar = -1
                if (plain($0)) {
                    anchor_defs($0, d)
                    for (a in d) { any_anchor[a] = 1; if (sha_ref(d[a])) tc_anchor[a] = 1; else delete tc_anchor[a] }
                    if ($0 ~ SCALAR_RE) anc_scalar = length(key_indent_of($0))
                }
            }
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
            if (in_step) { buffer(); next }
            if ($0 ~ LIST_RE) {
                if (parent_is_steps(ind)) { in_step = 1; step_indent = ind; n = 0; buffer(); next }
                # A list item whose parent is not `steps:` (a matrix, a fixture) is data.
                print; next
            }
            if (plain($0) && $0 ~ KEY_RE) {
                record_key($0)
                if ($0 ~ SCALAR_RE) { in_scalar = 1; scalar_indent = ind }
                # A key line that itself carries the pin (`steps: [ { uses: ... } ]`
                # on one line, a key of a flow-style step spread over lines) is a
                # step this parser cannot place. The pin inside some other value
                # (a workflow-level `env:` string naming the action) is data.
                if (uses_pin($0) && (parent_is_steps(ind) || under_steps(ind))) unrec[++nunrec] = NR
            } else if (uses_pin($0) && parent_is_steps(ind)) unrec[++nunrec] = NR
            print
        }
        END {
            flush()
            if (nunrec) {
                printf "UNSUPPORTED  %s: %d SHA-pinned dtolnay/rust-toolchain reference(s) the parser cannot place as a block-style step key (a flow-style `- { ... }` step, a `uses:` value continued below its key, or an alias of an anchored reference?), line(s):", FILENAME, nunrec > "/dev/stderr"
                for (j = 1; j <= nunrec; j++) printf " %d", unrec[j] > "/dev/stderr"
                printf ". Fix by hand; the file was not changed.\n" > "/dev/stderr"
                exit 2
            }
            if (fixed) printf "FIXED  %s  steps=%d  toolchain=%s\n", FILENAME, fixed, toolchain > "/dev/stderr"
        }
    ' "$f" > "$tmp"; rc=$?
    set -e
    if [ "$rc" -ne 0 ]; then rm -f "$tmp"; return 2; fi

    if ! cmp -s "$f" "$tmp"; then pin_fixer_replace "$tmp" "$f"; else rm -f "$tmp"; echo "OK (every SHA-pinned dtolnay step names a toolchain): $f"; fi
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
