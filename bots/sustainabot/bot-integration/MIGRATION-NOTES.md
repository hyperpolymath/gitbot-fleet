<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
# ReScript → AffineScript Migration (issue #148)

This subtree was migrated from ReScript to AffineScript on 2026-05-24 by a
**hand-port under explicit policy override** of issue #148's
"do-not-hand-port-ahead-of-the-compiler" rule.

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

## Known caveats (must be verified by the real mechanical migrator)

The hand-port was done without an AffineScript compiler available to the
porter, against the canonical README spec only. Several pieces are
educated guesses that the upstream Phase-3 migration assistant
(affinescript#57 / PR #314) is expected to redo:

1. **`Json` and `Dict` are placeholder names** in many type signatures.
   The portable `Json` primitive is tracked by affinescript#161 (OPEN)
   and `Dict`/`Map` by #162 (OPEN). Once those land, the placeholders
   need to be re-pointed at the canonical stdlib names.
2. **Effect rows** use a conservative `-{IO + Http + Crypto + Exn[E]}->`
   shape. These need narrowing against the actual effect declarations
   in the stdlib.
3. **TEA runtime (`tea/ServerTea.affine`)** uses `mut` cells and
   mutually-recursive closures. The borrow checker may force this into
   a single-owner / handler-state shape — exact form to be settled by
   the actual checker.
4. **JWT crypto chain (`GitHubApp.affine`)** assumes Web Crypto-derived
   stdlib bindings (`Crypto.import_key_pkcs8`,
   `Crypto.sign_with_algorithm`). Real binding names will land with
   `#103` (Async-extern ABI — closed 2026-05-18) once the canonical
   stdlib surface is published.
5. **Bindings deleted** (`bindings/Deno.res`, `bindings/Fetch.res`) —
   their callers now reference target-agnostic stdlib names
   (`Http.fetch`, `Env.get`, `Console.log`, etc.). The compiler's
   target-binding layer is expected to lower these per backend.

## Re-port checklist (when affinescript#57 Phase 3 lands)

- [ ] Run the mechanical migrator over the original `.res` (via
      `git show <pre-migration-sha>:path/to/file.res`) and diff its
      output against the hand-port.
- [ ] Substitute placeholder `Json` / `Dict[String, V]` references with
      whatever canonical names #161 / #162 land with.
- [ ] Re-narrow effect rows against the actual stdlib declarations.
- [ ] Run `affinescript check src/` against every file and resolve any
      remaining errors that the hand-port couldn't anticipate.
