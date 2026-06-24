<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
<!-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell -->

# sustainabot ReScript→AffineScript hand-port validation

**Date**: 2026-05-26
**Agent**: Claude Opus 4.7 (1M context)
**Session**: Issue #148 — gitbot-fleet/bots/sustainabot/bot-integration/src `.affine` parse validation
**Status**: Validation complete; 6 PRs filed; gates on owner-merge

---

## Goal

Run `affinescript check` on the 13 hand-ported `.affine` files under
`bots/sustainabot/bot-integration/src/` and reduce every parse error to
either a successful type-check or to `Resolve.UndefinedModule` (the
expected residual when single-file `check` doesn't load the stdlib
graph — INT-02 loader-bridge territory, out of scope here).

The 13 files (a 4,939 LOC migration from the original `.res` set):

```
src/Analysis.affine          src/Main.affine          src/Router.affine          src/tea/Sub.affine
src/Config.affine            src/Oikos.affine         src/Types.affine
src/GitHubAPI.affine         src/Report.affine        src/Webhook.affine
src/GitHubApp.affine                                  src/tea/Cmd.affine
                                                      src/tea/Runtime.affine
```

## Approach

For each parse failure encountered, decide **parser-fix** (upstream
`hyperpolymath/affinescript`, `lib/parser.mly`) vs **hand-port-rewrite**
(this repo, the `.affine` source). Default:

* **parser-fix** when the failing syntax is documented as part of the
  AffineScript language surface (ADR-008/009 / SETTLED-DECISIONS in the
  affinescript repo) and dropping it would create a gap.
* **hand-port-rewrite** when the syntax is an OCaml/ReScript-ism the
  language never promised.

Constraint on every parser-fix: zero new LR conflicts. Baseline is
**21 shift/reduce + 1 reduce/reduce**; verified after every patch via
`menhir --explain`.

## Outcome

All 13 files now reach **Resolution** (parser layer fully clear). The
work bundled into 6 PRs across 2 repos:

### Parser PRs (hyperpolymath/affinescript)

| PR | Scope | Branch |
|---|---|---|
| [#370](https://github.com/hyperpolymath/affinescript/pull/370) | Trailing-comma in fn params + expr lists; effect-annotated lambda `fn() -{IO}-> M { … }` | `claude/parser-trailing-comma-148` |
| [#371](https://github.com/hyperpolymath/affinescript/pull/371) | fn-type with effect arrow `fn(A, B) -{E}-> R` in type position | `claude/parser-fn-type-eff-arrow-148` |
| [#372](https://github.com/hyperpolymath/affinescript/pull/372) | Builtin-type qualified paths (`Int::to_string`); lowercase-module qualified paths (`json::encode_object`); `total` as a record field name | `claude/parser-builtin-qualified-paths-148` |
| [#373](https://github.com/hyperpolymath/affinescript/pull/373) | Underscore-prefix idents `_key`, `_unused` lex as a single LOWER_IDENT (bare `_` still lexes as UNDERSCORE) | `claude/lexer-underscore-idents-148` |
| [#376](https://github.com/hyperpolymath/affinescript/pull/376) | Record-update spread at start `Record #{ ..base, override: x }` | `claude/parser-record-spread-148` |

### Hand-port PR (hyperpolymath/gitbot-fleet)

| PR | Scope | Branch |
|---|---|---|
| [#206](https://github.com/hyperpolymath/gitbot-fleet/pull/206) | OCaml-style float ops `/.`, `*.`, `+.`, `-.` → unified AffineScript `/`, `*`, `+`, `-`; `pub fn handle(...)` → `pub fn dispatch(...)` (HANDLE is a reserved keyword) | `claude/sustainabot-parse-fixes-148` |

## Validation oracle (post-merge)

```
$ for f in bots/sustainabot/bot-integration/src/*.affine \
           bots/sustainabot/bot-integration/src/tea/*.affine; do
    affinescript check "$f"
  done

Analysis.affine     Resolution error: (Resolve.UndefinedModule …
Config.affine       Resolution error: (Resolve.UndefinedModule …
GitHubAPI.affine    Resolution error: (Resolve.UndefinedModule …
GitHubApp.affine    Resolution error: (Resolve.UndefinedModule …
Main.affine         Resolution error: (Resolve.UndefinedModule …
Oikos.affine        Resolution error: (Resolve.UndefinedModule …
Report.affine       Resolution error: (Resolve.UndefinedModule …
Router.affine       Resolution error: (Resolve.UndefinedModule …
tea/Cmd.affine      Resolution error: (Resolve.UndefinedModule …
tea/Runtime.affine  Resolution error: (Resolve.UndefinedModule …
tea/Sub.affine      Resolution error: (Resolve.UndefinedModule …
Types.affine        Resolution error: (Resolve.UndefinedModule …
Webhook.affine      Resolution error: (Resolve.UndefinedModule …
```

`Resolve.UndefinedModule` is the expected residual: the cross-module
loader is tracked separately under INT-02 in
`hyperpolymath/affinescript` `docs/TECH-DEBT.adoc`. Closing that gap is
a different effort, not within #148.

## Gotchas discovered (captured for future hand-ports)

Two hand-port-rewrite rules surfaced that are worth documenting up-front
for the broader `.res → .affine` walk (affinescript#57):

1. **`handle` is a reserved keyword** in AffineScript (HANDLE token,
   used by the effect-handler expression form
   `handle body { handler_arms }`). It parses as a token, not an
   identifier, so `pub fn handle(...)` is a syntax error. The
   `field_name` rule allows `handle` contextually as a record field
   name (the surrounding COLON disambiguates), but no equivalent is
   safe in fn-decl name position without grammar conflict risk.
   *Workaround*: rename to `dispatch`, `handle_request`,
   `handle_event`, etc.

2. **No OCaml-style float operators.** AffineScript uses unified `+`,
   `-`, `*`, `/` for both Int and Float (per
   `examples/lessons/01_hello.affine`: `subtotal * 0.08`). `+.`, `-.`,
   `*.`, `/.` are never accepted and must be rewritten on port. This
   is a hand-port-rewrite, not a parser-fix candidate — adding the
   OCaml form would create operator overlap for no semantic benefit.

Both are recorded in the agent's persistent memory for the next session
and are also captured as `Refs gitbot-fleet#148` commits in #206.

## Out of scope (filed separately)

3 byte-identical `SafeDOMExample.affine` fixtures
(`bots/{hotchocolabot,echidnabot,finishingbot}/examples/`) parse-
fail with a different shape: they use a **pre-stabilization AffineScript
dialect** with 8+ grammar divergences from current. Tracked at
[gitbot-fleet#208](https://github.com/hyperpolymath/gitbot-fleet/issues/208).
Recommended disposition: a single dialect-migration PR (or deletion if
the examples are unreferenced — `grep -r SafeDOMExample` will say).

## Conflict-cost on the parser

All 5 upstream parser PRs together net **zero new LR conflicts**: the
parser builds at 21 S/R + 1 R/R, identical to the pre-patch baseline.
The conflict-neutrality discipline is required by ADR-012 (Grammar
Changes Are Correctness Assertions) in the affinescript repo.

## Status comment on the tracking issue

A consolidated status post is on [gitbot-fleet#148](https://github.com/hyperpolymath/gitbot-fleet/issues/148#issuecomment-4542225835)
linking each of the six PRs, showing the validation oracle output, and
noting INT-02 as the next gate. The issue is **not** auto-closed by
any `Refs` keyword — that's an owner-merge action per the
`ISSUE-CLOSURE` rule in the repo's CLAUDE.md.

---

*Generated 2026-05-26 by Claude Opus 4.7 (1M context).*
