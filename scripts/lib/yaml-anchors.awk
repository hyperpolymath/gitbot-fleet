# SPDX-License-Identifier: MPL-2.0
# yaml-anchors.awk — the YAML anchors one line of a workflow defines.
#
# Shared by the workflow-pin fixers, which prepend this file to their own awk
# program (`awk "$(cat lib/yaml-anchors.awk)"'<program>'`); POSIX awk only, so
# mawk reads it as gawk does.
#
# anchor_defs(s, defs) clears defs and sets defs[name] = value for every
# `&name value` on line s, and returns how many there are. An anchor is
# `&name` at the start of a token outside any quoted scalar and before any
# comment: `&call` written inside `NOTE: "fixture &call a/b@<sha>"` or after a
# `#` is text, not an anchor. Its value is the scalar after it: a quoted one
# read to its closing quote (`\` escapes inside double quotes, `''` inside
# single ones), a plain one up to whitespace, `,`, `}`, `]` or a comment; ""
# when the anchor names a mapping or sequence that follows. The caller skips
# the content lines of block scalars (`run: |`), which are text throughout.
function anchor_defs(s, defs,   n, i, len, c, q, name, v) {
    split("", defs); n = 0; i = 1; q = ""; len = length(s)
    while (i <= len) {
        c = substr(s, i, 1)
        if (q != "") {
            if (q == "\"" && c == "\\") { i += 2; continue }
            if (c == q) {
                if (q == "'" && substr(s, i + 1, 1) == "'") { i += 2; continue }
                q = ""
            }
            i++; continue
        }
        if (c == "#" && (i == 1 || substr(s, i - 1, 1) ~ /[[:space:]]/)) break
        if (c == "\"" || c == "'") { q = c; i++; continue }
        if (c == "&" && (i == 1 || substr(s, i - 1, 1) ~ /[[:space:]:,{[-]/) && match(substr(s, i + 1), /^[A-Za-z0-9_.-]+/)) {
            name = substr(s, i + 1, RLENGTH); i += RLENGTH + 1
            while (i <= len && substr(s, i, 1) ~ /[[:space:]]/) i++
            c = substr(s, i, 1); v = ""
            if (c == "\"" || c == "'") {
                q = c; i++
                while (i <= len) {
                    c = substr(s, i, 1)
                    if (q == "\"" && c == "\\") { v = v substr(s, i, 2); i += 2; continue }
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
            defs[name] = v; n++
            continue
        }
        i++
    }
    return n
}
