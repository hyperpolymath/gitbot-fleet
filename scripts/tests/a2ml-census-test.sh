#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Unit tests for the value census, independent of kyX and standards.
# Usage: bash scripts/tests/a2ml-census-test.sh (Bun only; normally < 5s).
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
command -v bun >/dev/null || { echo 'NOT RUN: needs bun' >&2; exit 2; }
CENSUS="$ROOT/scripts/lib/a2ml-census.js" bun run - <<'JS'
import assert from 'node:assert/strict';
import { mkdtempSync, writeFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { spawnSync } from 'node:child_process';
const { a2mlValues, deedValues, census } = await import(process.env.CENSUS);
let passed = 0;
function test(name, fn) { fn(); console.log(`PASS: ${name}`); passed++; }
const field = (key, kind, value) => ['field', key, [kind, value]];
const ast = (...items) => ({ items });
const strings = (...values) => ast(...values.map(v => field('renamed', 'string', v)));

const lexCases = [
  ['empty input and comments', '# ignored = "value"\r\n\t\r\n', []],
  ['headers, directives and keys are not values', '[project]\n[[related.projects]]\n@drop("ignored")\n"key=with equals".name = "kept" # ignored\n', ['kept']],
  ['literal strings preserve escapes and comment markers', "a = 'C:\\tmp#file'\nb = '''\nraw\\n\nline'''\n", ['C:\\tmp#file', 'raw\\n\nline']],
  ['CRLF normalizes before multiline parsing', 'a = """\r\nfirst\r\nsecond"""\r\n', ['first\nsecond']],
  ['nested containers collect only values in order', 'a = [{name = "x", nested = [false, 0]}, ["y",], {}]\n', ['x', 'false', '0', 'y']],
  ['bare scalars retain their spelling', 'a = [-1, 1.20, 2026-11-17, true, false]\n', ['-1', '1.20', '2026-11-17', 'true', 'false']],
  ['basic escapes decode', String.raw`a = "\b\t\n\f\r\"\\"`, ['\b\t\n\f\r"\\']],
  ['Unicode scalar boundaries decode', String.raw`a = "\u0000\uD7FF\uE000\U0010FFFF"`, ['\0\uD7FF\uE000\u{10FFFF}']],
  ['continuations trim blank lines and indentation', 'a = """\none \\\n \t\n  two"""', ['one two']],
];
for (const [name, source, expected] of lexCases) test(name, () => assert.deepEqual(a2mlValues(source), expected));

test('AST recursively collects clauses and lists, normalizing booleans', () => {
  assert.deepEqual(deedValues(ast(['clause', 'ignored-name', [
    field('ignored-key', 'list', [['string', 'x'], ['list', [['boolean', '#t'], ['boolean', '#f']]], ['integer', '0']]),
    ['clause', 'nested', [field('id', 'uuid', 'id-value')]],
  ]])), ['x', 'true', 'false', '0', 'id-value']);
});
test('AST names and unknown items cannot satisfy missing values', () => {
  assert.deepEqual(census('a = "needle"', ast(field('needle', 'string', 'other'), ['unknown', 'needle'])), {
    source_values: 1, missing: ['needle'], short: [{ value: 'needle', source: 1, deed: 0 }],
  });
});
test('only empty values are excluded; whitespace-only values are counted', () => {
  // A value that is only spaces or a tab is still a value: a deed that drops
  // it has lost something, so it must show as a shortfall.
  assert.deepEqual(census('a = ["", "  ", "\\t", "x"]', strings('x')), {
    source_values: 3, missing: ['  ', '\t'],
    short: [{value: '  ', source: 1, deed: 0}, {value: '\t', source: 1, deed: 0}],
  });
  assert.deepEqual(census('a = ["", "  ", "\\t", "x"]', strings('x', '  ', '\t')).missing, []);
});
test('nonempty values retain significant whitespace', () => {
  assert.deepEqual(census('a = " x "', strings('x')).missing, [' x ']);
});
test('duplicate deficits report exact counts and stable first-seen order', () => {
  assert.deepEqual(census('a = ["b", "a", "b", "a", "b"]', strings('b', 'a')), {
    source_values: 5, missing: ['b', 'a'], short: [{value: 'b', source: 3, deed: 1}, {value: 'a', source: 2, deed: 1}],
  });
});
test('surplus deed values and reordered fields are permitted', () => {
  assert.deepEqual(census('a = ["x", "x", true, 0]', ast(...strings('extra', 'x', 'x', 'x').items, field('n', 'integer', '0'), field('b', 'boolean', '#t'))), {source_values: 4, missing: [], short: []});
});
test('census does not mutate caller AST', () => {
  const input = strings('x'); const before = structuredClone(input);
  census('a = "x"', input); assert.deepEqual(input, before);
});

// Parse failures exit the process: exercise the public CLI in isolated children.
const tmp = mkdtempSync(join(tmpdir(), 'a2ml-census-test-'));
try {
  const source = join(tmp, 'source.a2ml'), deed = join(tmp, 'ast.json');
  const cli = (...args) => {
    const r = spawnSync(process.execPath, [process.env.CENSUS, ...args], {encoding: 'utf8', timeout: 3000});
    assert.equal(r.error, undefined); assert.equal(r.signal, null); return r;
  };
  writeFileSync(deed, JSON.stringify(strings('x')));
  const invalid = [
    ['missing assignment', 'oops\n', /key without '='/],
    ['missing value', 'a =', /expected a value/],
    ['unterminated basic string', 'a = "oops', /unterminated string/],
    ['newline in literal', "a = 'one\ntwo'", /unterminated literal/],
    ['unterminated multiline', 'a = """oops', /unterminated multi-line/],
    ['unknown escape', String.raw`a = "\q"`, /bad escape/],
    ['short Unicode escape', String.raw`a = "\u123"`, /bad \\u escape/],
    ['surrogate escape', String.raw`a = "\uD800"`, /not a Unicode scalar/],
    ['out of range escape', String.raw`a = "\U00110000"`, /not a Unicode scalar/],
    ['missing array comma', 'a = ["x" "y"]', /expected , or \]/],
    ['missing table comma', 'a = {b = "x" c = "y"}', /expected , or }/],
    ['trailing content', 'a = "x" junk', /trailing content/],
    ['line number after CRLF', '# comment\r\na = "oops\r\n', /A2ML line 2/],
  ];
  for (const [name, text, error] of invalid) test(`CLI rejects ${name}`, () => {
    writeFileSync(source, text); const r = cli('check', source, deed);
    assert.equal(r.status, 2); assert.equal(r.stdout, ''); assert.match(r.stderr, error);
  });
  for (const [name, args] of [['no arguments', []], ['wrong command', ['convert', source, deed]], ['missing AST argument', ['check', source]]]) {
    test(`CLI usage: ${name}`, () => { const r = cli(...args); assert.equal(r.status, 2); assert.match(r.stderr, /usage:/); });
  }
  test('CLI unreadable source', () => { const r = cli('check', join(tmp, 'absent'), deed); assert.equal(r.status, 2); assert.match(r.stderr, /cannot read/); });
  writeFileSync(source, 'a = "x"');
  test('CLI invalid JSON', () => { writeFileSync(deed, '{'); const r = cli('check', source, deed); assert.equal(r.status, 2); assert.match(r.stderr, /cannot read/); });
  test('CLI rejects AST without items', () => { writeFileSync(deed, '{}'); const r = cli('check', source, deed); assert.equal(r.status, 2); assert.match(r.stderr, /AST has no items/); });
  for (const [name, values, status, expected] of [
    ['matching values', ['x'], 0, {source_values: 1, missing: [], short: []}],
    ['missing values', [], 1, {source_values: 1, missing: ['x'], short: [{value: 'x', source: 1, deed: 0}]}],
  ]) test(`CLI ${name}: status and JSON`, () => {
    writeFileSync(deed, JSON.stringify(strings(...values))); const r = cli('check', source, deed);
    assert.equal(r.status, status); assert.equal(r.stderr, ''); assert.deepEqual(JSON.parse(r.stdout), expected);
  });
} finally { rmSync(tmp, { recursive: true, force: true }); }
console.log(`a2ml-census-test: ${passed} passed`);
JS
