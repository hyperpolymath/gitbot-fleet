// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// a2ml-census.js — the value census behind fix-a2ml-via-kyx.sh's "no
// information is lost" gate. Runs under Bun, zero dependencies.
//
//   bun a2ml-census.js check SOURCE.a2ml AST.json
//
// AST.json is `bun deed_lint.js --ast-json OUT.deed` (standards). Every
// non-empty scalar VALUE in the A2ML source (strings, numbers, booleans, dates)
// must appear as a value in the deed at least as many times as in the source
// (a multiset check, so a field collapsed into another is caught). Keys and
// table names are not values (a mapping spec renames them), and comments are not values (kyX's own
// drop report covers those). Prints one JSON object
//   {"source_values": N, "missing": [value, ...],
//    "short": [{"value", "source": count, "deed": count}, ...]}
// and exits 0 when nothing is missing, 1 when something is, 2 on a usage or
// parse error. It is a NECESSARY condition for losslessness, checked
// independently of kyX: it errs towards reporting loss (a value a spec
// legitimately rewrites shows as missing), never towards hiding it.
//
// This is a verifier, not a converter: it never writes a deed.

import { readFileSync } from "node:fs";

function fail(msg) {
  process.stderr.write(`a2ml-census: ${msg}\n`);
  process.exit(2);
}

// --- A2ML (the retired TOML-like record dialect) value lexer --------------

export function a2mlValues(text) {
  const s = text.replace(/\r\n/g, "\n");
  const out = [];
  let i = 0;
  let line = 1;
  const peek = (k = 0) => s[i + k];
  const err = (m) => fail(`A2ML line ${line}: ${m}`);
  const skipWs = (newlines) => {
    for (;;) {
      const c = peek();
      if (c === " " || c === "\t") i++;
      else if (newlines && c === "\n") { i++; line++; }
      else if (c === "#") { while (i < s.length && peek() !== "\n") i++; }
      else return;
    }
  };
  // One escape, after its backslash; the same rules in "..." and """...""".
  const escape = () => {
    const e = s[i++];
    const map = { n: "\n", t: "\t", r: "\r", '"': '"', "\\": "\\", b: "\b", f: "\f" };
    if (e in map) return map[e];
    if (e === "u" || e === "U") {
      const n = e === "u" ? 4 : 8;
      const hex = s.slice(i, i + n);
      if (!/^[0-9A-Fa-f]+$/.test(hex) || hex.length !== n) err("bad \\u escape");
      const cp = parseInt(hex, 16);
      if (cp > 0x10ffff || (cp >= 0xd800 && cp <= 0xdfff)) err("\\u escape is not a Unicode scalar value");
      i += n;
      return String.fromCodePoint(cp);
    }
    err(`bad escape \\${e}`);
  };
  const basic = () => {
    // after the opening quote
    let v = "";
    for (;;) {
      const c = s[i++];
      if (c === undefined || c === "\n") err("unterminated string");
      if (c === '"') return v;
      if (c === "\\") v += escape();
      else v += c;
    }
  };
  const multi = (q) => {
    // after the opening triple quote; a newline right after it is trimmed
    if (peek() === "\n") { i++; line++; }
    let v = "";
    for (;;) {
      if (i >= s.length) err("unterminated multi-line string");
      if (s.startsWith(q + q + q, i)) { i += 3; return v; }
      const c = s[i++];
      if (c === "\n") line++;
      if (q === '"' && c === "\\") {
        // A line-ending backslash is the last non-whitespace character on its
        // line (TOML 1.0), so spaces or tabs may follow it before the break.
        let j = i;
        while (s[j] === " " || s[j] === "\t") j++;
        if (s[j] === "\n") { // trim the padding, the break and the next indent
          i = j;
          while (peek() === "\n" || peek() === " " || peek() === "\t") { if (peek() === "\n") line++; i++; }
          continue;
        }
        v += escape();
      } else v += c;
    }
  };
  const value = () => {
    skipWs(false);
    const c = peek();
    if (s.startsWith('"""', i)) { i += 3; out.push(multi('"')); return; }
    if (s.startsWith("'''", i)) { i += 3; out.push(multi("'")); return; }
    if (c === '"') { i++; out.push(basic()); return; }
    if (c === "'") {
      i++;
      const j = s.indexOf("'", i);
      const nl = s.indexOf("\n", i);
      if (j < 0 || (nl >= 0 && nl < j)) err("unterminated literal string");
      out.push(s.slice(i, j));
      i = j + 1;
      return;
    }
    if (c === "[") {
      i++;
      for (;;) {
        skipWs(true);
        if (peek() === "]") { i++; return; }
        value();
        skipWs(true);
        if (peek() === ",") { i++; continue; }
        if (peek() === "]") { i++; return; }
        err("expected , or ] in an array");
      }
    }
    if (c === "{") {
      i++;
      for (;;) {
        skipWs(false);
        if (peek() === "}") { i++; return; }
        key();
        value();
        skipWs(false);
        if (peek() === ",") { i++; continue; }
        if (peek() === "}") { i++; return; }
        err("expected , or } in an inline table");
      }
    }
    // bare scalar: bool, number, date
    let j = i;
    while (j < s.length && !/[\s,\]}#]/.test(s[j])) j++;
    const tok = s.slice(i, j);
    if (tok === "") err("expected a value");
    out.push(tok);
    i = j;
  };
  const key = () => {
    // a bare, quoted or dotted key, then '='
    skipWs(false);
    while (i < s.length && peek() !== "=") {
      if (peek() === "\n") err("a key without '='");
      if (peek() === '"') { i++; basic(); continue; }
      if (peek() === "'") { const j = s.indexOf("'", i + 1); if (j < 0) err("bad key"); i = j + 1; continue; }
      i++;
    }
    if (peek() !== "=") err("expected '='");
    i++;
  };
  while (i < s.length) {
    skipWs(true);
    if (i >= s.length) break;
    if (peek() === "[" || peek() === "@") {
      // A table header is a name, not a value. An `@directive(...)` line is
      // kyX's drop doctrine's to report (spec §4), so the drop gate covers it.
      while (i < s.length && peek() !== "\n") i++;
      continue;
    }
    key();
    value();
    skipWs(false);
    if (i < s.length && peek() !== "\n") err("trailing content after a value");
  }
  return out;
}

// --- deed AST (deed_lint.js --ast-json) value collector -------------------

export function deedValues(ast) {
  const out = [];
  const node = (n) => {
    if (!Array.isArray(n) || n.length < 2) return;
    const [kind, v] = n;
    if (kind === "list") { for (const x of v) node(x); return; }
    if (kind === "boolean") { out.push(v === "#t" ? "true" : "false"); return; }
    if (typeof v === "string") out.push(v);
  };
  const items = (xs) => {
    for (const it of xs) {
      if (it[0] === "field") node(it[2]);
      else if (it[0] === "clause") items(it[2]);
    }
  };
  if (!ast || !Array.isArray(ast.items)) fail("AST has no items (is it deed_lint --ast-json output?)");
  items(ast.items);
  return out;
}

// A multiset count: how many times each value occurs.
function tally(values) {
  const n = new Map();
  for (const v of values) n.set(v, (n.get(v) ?? 0) + 1);
  return n;
}

export function census(sourceText, ast) {
  const want = a2mlValues(sourceText).filter((v) => v.trim() !== "");
  // Counted, not just present: two source fields holding the same value must
  // be two deed values, or a field collapsed into another passes unseen.
  const need = tally(want);
  const have = tally(deedValues(ast));
  const short = [];
  for (const [value, source] of need) {
    const deed = have.get(value) ?? 0;
    if (deed < source) short.push({ value, source, deed });
  }
  return { source_values: want.length, missing: short.map((s) => s.value), short };
}

if (import.meta.main) {
  const [cmd, src, astPath] = process.argv.slice(2);
  if (cmd !== "check" || !src || !astPath) fail("usage: a2ml-census.js check SOURCE.a2ml AST.json");
  let text, ast;
  try { text = readFileSync(src, "utf8"); } catch (e) { fail(`cannot read ${src}: ${e.message}`); }
  try { ast = JSON.parse(readFileSync(astPath, "utf8")); } catch (e) { fail(`cannot read ${astPath}: ${e.message}`); }
  const r = census(text, ast);
  process.stdout.write(JSON.stringify(r) + "\n");
  process.exit(r.missing.length === 0 ? 0 : 1);
}
