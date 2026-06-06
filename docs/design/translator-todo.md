# Bash to Fish Translator - Active Backlog

Last refreshed: 2026-06-06

This file is the active backlog for Monk's translator work.

- `docs/design/translator-audit.md` is the fidelity source of truth.
- `docs/migration-guide.md` is the user-facing cleanup guide.
- Checkboxes here mean "actionable engineering work remains", not "semantic parity is solved everywhere".

Current local baseline:

- Focused local checks now cover the public diagnostics contract, translator state/warning ordering, shared status/conjunction lowering, bake-off tool preflight messaging, fixture sidecars, and Linux-gated process-substitution output fixtures.
- The current phase also landed explicit warning code/severity rendering, non-literal/missing `source` diagnostics, exact `set --` argv updates, redirected `shopt` lowering, expanded exact `read` handling, numeric/named `trap` hardening, shell-run timeouts, checked-in bake-off-compatible list coverage, and temp-file-backed status-preserving lowering for covered `>(...)` redirect targets.

## Current Position

Monk is a correctness-first conservative migrator, not a prove-exact transpiler. The translator should prefer:

- one consistent lowering policy per construct across all contexts
- explicit warnings for best-effort branches
- strict-mode failure for constructs we cannot lower safely
- runtime evidence before promoting a feature from documented best-effort to exact

## Active Backlog

### P0: Keep the remaining best-effort surfaces conservative and explicit

- [x] Expand evidence around residual `set -e` / `pipefail` compound-list edge cases.
- [x] Expand the exact helper-backed `read` surface and keep residual warnings narrow.
  - [x] no-variable reads via `REPLY`
  - [x] no-variable delimiter reads
  - [x] newline-delimited array and multi-variable reads
  - [x] non-numeric `-u` values
  - [x] unsupported flag clusters
- [ ] Record explicit Linux runtime evidence for covered `>(...)` redirect-target lowering before upgrading it beyond best-effort overall.
  - [x] Add fast-fail shell-run timeouts so deadlocks fail instead of hanging.
  - [x] Add local Linux runtime evidence for the `procsub-output*` fixtures, including producer-status, `set -e`, ignored-consumer-status, and compound-consumer behavior.
  - [x] Add a dedicated Ubuntu CI selector for the Linux-gated `procsub-output*` fixtures.
  - [ ] Close this item after that evidence step passes on Ubuntu.

### P1: Preserve documentation and diagnostics discipline

- [ ] Keep this file, `docs/design/translator-audit.md`, and `docs/migration-guide.md` in lockstep whenever a best-effort branch changes status.
- [ ] Keep warning codes, severities, and rendered CLI text aligned as one documented contract.
- [ ] Keep bake-off-compatible fixture selection current when new parity fixtures are added.

### P1: Leave these surfaces warning-driven unless exactness is proven

- [ ] non-literal `source`
- [ ] option-heavy `trap`
- [ ] `shopt`
- [ ] `coproc`

### P2: Optimization work only with measurement

- [x] Re-run `cabal bench` once the local Cabal benchmark setup is healthy again.
- [ ] Extend simplifier and helper cleanups only when the rewrite is semantics-preserving and benchmarked.

## Evidence Required Before Closing Items

- Add a focused runtime fixture or bake-off result for semantic changes, not only translation-shape tests.
- Add direct unit coverage when changing warning codes, warning severities, or helper registration behavior.
- Treat Linux as the acceptance source of truth for helper-backed `>(...)`; close that item only after the dedicated Ubuntu CI evidence step passes.

## Archive

The following milestones were completed in the correctness-first pass and are kept here as historical context rather than active backlog.

### Core translator architecture

- [x] `mtl`-based translation monad with source-range tracking, warnings, and strict-mode failures
- [x] Modular translator split across `Variables`, `Commands`, `Control`, `IO`, `Redirections`, and related helper modules
- [x] Public library split into `Monk.Translation`, `Monk.AST`, `Monk.Source`, and a thin `Monk` facade
- [x] `Monk.AST` now exposes the typed Fish DSL; raw constructors moved behind the explicit `Monk.AST.Raw` escape hatch
- [x] Translator handoff now emits a DSL `Script` and lowers through `Language.Fish.DSL.Lower` before rendering
- [x] Central pipeline helpers now require `NonEmpty` stages instead of accepting empty lists with a silent `true` fallback
- [x] Translator modules now route raw-shaped construction through `Language.Fish.Translator.DSL` instead of importing `Language.Fish.AST` directly
- [x] Repeated redirect-attachment helpers are centralized behind the translator DSL boundary
- [ ] Continue shrinking `Language.Fish.Translator.DSL` imports by moving command, expression, and runtime-helper islands onto typed DSL construction helpers
- [x] Bake-off moved into a separate private library/executable under `scripts/`

### Correctness fixes landed

- [x] Side-effecting parameter expansion now hoists correctly in command arguments, redirections, heredocs, and `case`
- [x] `${var:?err}` / `${var?err}` abort semantics now hoist correctly
- [x] `((expr))` status semantics and arithmetic side effects are modeled on the covered surface
- [x] `until` now negates the whole compound condition list
- [x] `case` pattern lowering preserves glob semantics on the covered surface
- [x] Recursive separate `source` output now supports output-rooted bundles and rewrites literal child paths relative to the emitted bundle
- [x] Bake-off selector files now resolve relative entries from the selector file directory instead of the process cwd
- [x] `trap` now clears Monk-generated handlers on covered reset forms and degrades pseudo-signals to explicit manual-review notes instead of invalid fish
- [x] Shared subshell policy now applies across statement, status, and command-substitution contexts
  - [x] normal mode emits the stable `BestEffortSubshell` warning and lowers to non-isolating `begin ... end`
  - [x] `--strict` fails on subshells in all covered contexts
- [x] Command-substitution subshells no longer silently collapse to `true`

### Diagnostics and API cleanup

- [x] Typed warnings now carry stable code, severity, detail, and range
- [x] CLI diagnostics render from structured warnings instead of string matching
- [x] `Monk.Translation` no longer exposes translator-state constructors publicly
- [x] Dead translator state (`preserveComments`, `inLoop`, unused source-map field) was removed from the translation state machine

### Helper management and simplification

- [x] Helper registration is now keyed by helper ID instead of per-helper booleans or preamble scans
- [x] Pipefail, background runtime, exact `read`, and `>(...)` helpers are deduplicated through the shared registry
- [x] Exact `read` assignment helpers were folded into one mode-driven helper
- [x] The simplifier now removes synthetic `else true` branches and other trivial wrapper patterns where semantics are unchanged

### Testing and evidence

- [x] Golden, unit, property, integration, real-world, and bake-off seam coverage are all in place
- [x] Focused parity fixtures cover side-effecting parameter expansion, generalized covered `read -d`, simple `<(...)`, recursive literal `source`, translated background `wait`, and simple `trap ... EXIT`
- [x] Focused parity fixtures now also cover the subshell-status regression surface:
  - [x] parent-variable access
  - [x] exact `read -d` helpers
  - [x] pipefail helpers
- [x] Focused parity and unit coverage now also cover exact `set --` argv updates, no-variable/default-newline `read` helpers, redirected `shopt`, numeric and named uncatchable `trap` signals, checked-in bake-off-compatible selectors, and Linux `procsub-output*` fixtures.
