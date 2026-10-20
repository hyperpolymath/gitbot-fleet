# SPDX-License-Identifier: MPL-2.0
# yaml-anchors.awk — the YAML anchors one line of a workflow defines.
#
# Shared by the workflow-pin fixers, which prepend this file to their own awk
# program (`awk "$(cat lib/yaml-anchors.awk)"'<program>'`); POSIX awk only, so
# mawk reads it as gawk does.
#
# anchor_defs(s, defs) clears defs and sets defs[name] = value for every
# `&name value` on line s, and returns how many there are. An anchor is
# `&name` where a YAML node starts, outside any quoted scalar and before any
# comment: at the line's start or after its list dashes, after `key: `, or
# inside a flow collection after `[`, `{` or `,`, past any tag (`!!map &x`).
# `&call` written inside `NOTE: "fixture &call a/b@<sha>"`, inside a plain
# scalar (`NOTE: fixture &call a/b@<sha>`) or after a `#` is text, not an
# anchor. Its value is the scalar after it: a quoted one
# read to its closing quote (`''` is a quote inside single quotes; inside
# double quotes each `\` escape is decoded, yaml_unescape/1), a plain one up
# to whitespace, `,`, `}`, `]` or a comment; "" when the anchor names a
# mapping or sequence that follows. The caller skips the content lines of
# block scalars (`run: |`), which are text throughout.
#
# The character a double-quoted escape stands for, `t` being the text after
# its backslash; esc_len is set to how much of `t` it spans. YAML resolves
# `"\x64tolnay/rust-toolchain@<sha>"` to the action itself, so an anchored
# reference is read decoded, never as the raw bytes that hide it. An ASCII
# character comes back as itself (`\x6f`, `\u006f`, `\"`, `\\`, `\/`, an
# escaped space or tab); anything else — a code point past ASCII, `\n`, an
# unknown or malformed escape — as "\001", which no action reference holds.
function yaml_unescape(t,   e, n, k, d, code) {
    e = substr(t, 1, 1); esc_len = 1
    n = (e == "x") ? 2 : (e == "u") ? 4 : (e == "U") ? 8 : 0
    if (n) {
        code = 0
        for (k = 2; k <= n + 1; k++) {
            d = index("0123456789abcdef", tolower(substr(t, k, 1))) - 1
            if (d < 0) return "\001"
            code = code * 16 + d
        }
        esc_len = n + 1
        return (code >= 32 && code < 127) ? sprintf("%c", code) : "\001"
    }
    if (e == "\"" || e == "\\" || e == "/" || e == " ") return e
    if (e == "t" || e == "\t") return "\t"
    return "\001"
}
# Line s as YAML reads its double-quoted scalars: each `\` escape decoded
# (yaml_unescape), every other byte kept as written, so a reference spelled
# with escapes (`uses: "\x64tolnay/rust-toolchain@<sha>"`) is matched as
# the reference it is. An escaped quote decodes to "\001", never to `"`: the
# quotes that delimit a scalar stay the only ones, and a reader of the line
# sees the same scalars. Single-quoted scalars have no escapes; a comment is
# copied as written.
function yaml_decode_dq(s,   out, i, len, c, q) {
    out = ""; i = 1; len = length(s); q = ""
    while (i <= len) {
        c = substr(s, i, 1)
        if (q == "\"" && c == "\\") {
            c = yaml_unescape(substr(s, i + 1, 9)); out = out (c == "\"" ? "\001" : c); i += 1 + esc_len; continue
        }
        if (q != "") { if (c == q) q = "" }
        else if (c == "#" && (i == 1 || substr(s, i - 1, 1) ~ /[[:space:]]/)) { out = out substr(s, i); break }
        else if (c == "\"" || c == "'") q = c
        out = out c; i++
    }
    return out
}
# Does line s end in an escaped line break inside a double-quoted scalar
# (`uses: "dtolnay/rust-\`)? YAML then joins the next line to it, the break
# and that line's leading whitespace removed (yaml_dq_join), so the scalar
# is read whole only across both lines. A comment ends the line.
function yaml_dq_continued(s,   i, len, c, q) {
    i = 1; len = length(s); q = ""
    while (i <= len) {
        c = substr(s, i, 1)
        if (q == "\"" && c == "\\") { if (i == len) return 1; i += 2; continue }
        if (q != "") { if (c == q) { if (q == "'" && substr(s, i + 1, 1) == "'") { i += 2; continue }; q = "" } }
        else if (c == "#" && (i == 1 || substr(s, i - 1, 1) ~ /[[:space:]]/)) return 0
        else if (c == "\"" || c == "'") q = c
        i++
    }
    return 0
}
# Line s, ending in an escaped line break (yaml_dq_continued), joined with
# the line t that continues its scalar.
function yaml_dq_join(s, t) { sub(/^[[:space:]]+/, "", t); return substr(s, 1, length(s) - 1) t }
function anchor_defs(s, defs,   n, i, len, c, q, name, v, seen, depth) {
    split("", defs); n = 0; i = 1; q = ""; len = length(s); seen = ""; depth = 0
    while (i <= len) {
        c = substr(s, i, 1)
        if (q != "") {
            if (q == "\"" && c == "\\") { i += 2; continue }
            if (c == q) {
                if (q == "'" && substr(s, i + 1, 1) == "'") { i += 2; continue }
                q = ""; seen = seen "x"
            }
            i++; continue
        }
        if (c == "#" && (i == 1 || substr(s, i - 1, 1) ~ /[[:space:]]/)) break
        if (c == "\"" || c == "'") { q = c; seen = seen "x"; i++; continue }
        if (c == "{" || c == "[") depth++
        else if ((c == "}" || c == "]") && depth > 0) depth--
        if (c == "&" && anchor_at(seen, depth) && match(substr(s, i + 1), /^[A-Za-z0-9_.-]+/)) {
            name = substr(s, i + 1, RLENGTH); i += RLENGTH + 1
            while (i <= len && substr(s, i, 1) ~ /[[:space:]]/) i++
            c = substr(s, i, 1); v = ""
            if (c == "\"" || c == "'") {
                q = c; i++
                while (i <= len) {
                    c = substr(s, i, 1)
                    if (q == "\"" && c == "\\") { v = v yaml_unescape(substr(s, i + 1, 9)); i += 1 + esc_len; continue }
                    if (c == q) {
                        if (q == "'" && substr(s, i + 1, 1) == "'") { v = v "'"; i += 2; continue }
                        break
                    }
                    v = v c; i++
                }
                q = ""; i++
            } else {
                while (i <= len && substr(s, i, 1) !~ /[],}[:space:]]/) { v = v substr(s, i, 1); i++ }
            }
            defs[name] = v; n++; seen = seen "x"
            continue
        }
        seen = seen c; i++
    }
    return n
}

# Can a node start after `seen` (the line so far, quoted scalars as `x`), at
# flow depth `depth`? At the line's start or after its list dashes, after
# `key: `, or inside a flow collection after `[`, `{` or `,`; past any tag.
function anchor_at(seen, depth) {
    if (seen ~ /(^[[:space:]]*(-[[:space:]]+)*|:[[:space:]]+)(![^[:space:]]*[[:space:]]+)*$/) return 1
    return depth > 0 && seen ~ /[{,[][[:space:]]*(![^[:space:]]*[[:space:]]+)*$/
}
