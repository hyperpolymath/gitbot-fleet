<!-- SPDX-License-Identifier: MPL-2.0 -->
# ReScript → AffineScript Migration (issue #148)

This subtree was migrated from ReScript to AffineScript on 2026-05-24 by a
**hand-port under explicit policy override** of issue #148's
"do-not-hand-port-ahead-of-the-compiler" rule.

A follow-up static-review pass on 2026-05-25 reconciled the hand-port
against the canonical AffineScript grammar (`hyperpolymath/affinescript`
`lib/parser.mly`) and stdlib. See **"Static review changes (2026-05-25)"**
below.

## Scope of migration

- All `.res` / `.res.js` under `src/`, `src/tea/`, `bindings/`, and
  `lib/ocaml/` were deleted.
- The `rescript-runtime/` vendored Belt runtime was deleted.
- `rescript.json` was deleted.
- `package.json` and `deno.json` were updated to target the AffineScript
  toolchain instead of `rescript build` / `.res.js` outputs.
- The 11 in-scope `.res` files were re-expressed as `.affine`:
  `Types`, `Config`, `Webhook`, `Analysis`, `GitHubAPI`, `GitHubApp`,
  `Report`, `Router`, `Oikos`, `Main`, and `tea/ServerTea`.
- After the static-review pass, `tea/ServerTea.affine` was split into
  three siblings: `tea/Cmd.affine`, `tea/Sub.affine`, `tea/Runtime.affine`
  (the parser has no inline `module Name { ... }` form, so the original
  three-module file was structurally invalid).

## Static review changes (2026-05-25)

The hand-port was originally written against the README spec only,
without access to the parser. Reconciling it against
`lib/parser.mly` + the actual stdlib produced these edits:

| Change | Reason |
|---|---|
| `open Types` → `use Types::*;` | The lexer has no `open` keyword. Module-level imports use `use … ::*;` or `use … ::{Foo, Bar};`. |
| Each file now starts with `module <Name>;` | The parser requires a single `module Name;` decl at the head of every file. |
| `Dict[K, V]` field types → `[(K, V)]` | `stdlib/dict.affine` represents a dict as an assoc list of pairs (no `Dict` type symbol exists). |
| `Json` references kept (no `<…>` args) | `stdlib/json.affine` exports `pub type Json = JNull \| JBool(Bool) \| …` — Json is unparameterised. |
| `Json.foo(…)` → `json::foo(…)` | Module path uses `::`, and the canonical module is lowercase `json`. Similarly `Dict.foo` → `dict::foo`, `Option.foo` → `option::foo`. |
| `String.foo(…)` → `String::foo(…)` etc. | Capital-cased modules (Http, Crypto, Bytes, Env, Console, Time, Base64, String, Int, Float, Exn, Array, Config) still need `::` for module access; `.` is field access only. |
| Top-level `let x: T = …` → `const x: T = …;` | `let`-bindings only appear inside block/fn bodies. Module-level bindings use `const`. |
| Top-level `let _ = main()` removed | There is no top-level imperative slot; the program entry is the `pub fn main` itself. |
| `let rec … and …` (ServerTea Runtime) → individual `fn` decls with state passed explicitly, stubbed | The parser has no mutually-recursive `let rec … and …` form. The dispatch loop reduces to a TODO stub awaiting the mechanical migrator. |
| Inline `module Cmd { … }` → separate `tea/Cmd.affine` (plus `Sub`/`Runtime`) | The parser only accepts file-level `module Name;` — inline module bodies are not in the grammar. |
| `\|>` pipe operator → nested calls / let-binding chains | The lexer has no pipe operator. Each pipeline was rewritten as nested function calls or stepwise `let` bindings. |
| Variant decls with leading `\|` allowed (kept consistent) | Both `type X = A \| B` and `type X = \| A \| B` parse; we kept the no-leading-pipe form. |

The Json/Dict placeholder caveats from the original notes are gone:
`stdlib/json.affine` (the `Json` ADT, encoders/decoders, `stringify`) and
`stdlib/dict.affine` (the `[(String, V)]` assoc-list dict) have both
landed and are the canonical surfaces the rewritten files now point at.

**Compiler check not run.** The remote environment has no OCaml /
`dune` / `affinescript` toolchain available, so `affinescript check
src/` could not be executed. The edits above were verified against
`lib/parser.mly` by hand only.

## Known unbound surfaces

A list of stdlib surfaces these files call that are **not yet bound** in
canonical AffineScript stdlib lives in `MISSING-EXTERNS.md`. The
mechanical migrator (affinescript#57 Phase 3) must re-point those calls
against whatever names the eventual bindings land with.

## Re-port checklist (when affinescript#57 Phase 3 lands)

- [ ] Run the mechanical migrator over the original `.res` (via
      `git show <pre-migration-sha>:path/to/file.res`) and diff its
      output against the hand-port.
- [ ] Resolve every `TODO(mechanical-migrator):` annotation. The
      densest cluster is in `tea/Runtime.affine` (the dispatch loop
      stub) and `src/Main.affine` / `src/Router.affine` (the HTTP
      serve loop stubs).
- [ ] Re-bind every name listed in `MISSING-EXTERNS.md`.
- [ ] Re-narrow effect rows against the actual stdlib declarations
      (the hand-port uses a conservative `-{IO}->` everywhere).
- [ ] Run `affinescript check src/` (in an environment that has the
      compiler) and resolve any remaining errors that the hand-port
      couldn't anticipate.
