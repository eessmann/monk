# Bash to Fish Translator - TODO Checklist

Audit note (refreshed 2026-04-16): checked boxes in this file mean Monk has some implementation for the construct. Exactness lives in `docs/design/translator-audit.md`, which classifies features as `exact`, `best-effort`, `unsupported`, or `unverified` and records the current test evidence. The current local baseline is a passing `MONK_INTEGRATION=1 cabal test` run with 220 tests, including the gated background-job fixtures, the generalized `read -d` integration surface (`read-delimiter`, `read-delimiter-null-array`, `read-delimiter-null-vars`, `read-delimiter-ifs`, `read-delimiter-flags`), the helper-backed `>(...)` fixtures, the reduced `realworld/neofetch-mini` generated-output slice, the curated real-world parity fixtures `argparse-mini`, `envfile-preview`, `path-filter`, and `semver-normalize`, the direct simplifier translation-shape coverage added in the cleanup pass, and the new public/source/harness/bake-off seam tests added in the architecture pass. The active execution focus is now cleanup and code quality; the remaining semantic-expansion items stay tracked below but are intentionally paused unless cleanup work exposes a concrete bug.

## 🔴 Critical (Blocking Issues)

### Foundation
- [x] Implement TranslateM monad with Polysemy for metadata tracking
  - [x] Define TranslateState with source locations, warnings, context
  - [x] Define TranslateError for unsupported constructs
  - [x] Add TranslateConfig for user options
- [x] Add source location preservation from ShellCheck AST
- [x] Implement error recovery and warning accumulation

### Polysemy Conversion (Architecture)
- [x] Add `polysemy` + `polysemy-plugin` to `monk.cabal`
- [x] Rewrite `Language.Fish.Translator.Monad` around Polysemy effects
- [x] Rework `runTranslate*` to interpret Reader/State/Error/Writer/Input
- [x] Port all translator modules to Polysemy (`Variables`, `Commands`, `Control`, `IO`, `Redirections`, `Builtins`, `ForArithmetic`)
- [x] Update top-level glue and tests to use the new runners
- [x] Remove legacy `StateT` helpers and imports

### Semantic Parity Gaps (must fix for correctness)
- [x] Preserve side effects of `${var:=...}` / `${var=...}` in all contexts
  - [x] Command arguments (hoisted out of command substitution)
  - [x] Redirection targets and heredocs
  - [x] Case switch expressions
- [x] Preserve abort semantics of `${var:?err}` / `${var?err}` in all contexts
  - [x] Command arguments (exit in outer scope)
  - [x] Redirection targets and heredocs
  - [x] Case switch expressions
- [x] Implement `((expr))` status semantics (`expr != 0`)
- [x] Extend arithmetic side effects beyond simple assignments/`++`/`--` (postfix and compound expressions)
- [x] Fix `until` negation for compound conditions (negate whole condition list, not just first pipeline)
- [x] Preserve `case` pattern semantics (no quoting; keep glob patterns)
- [x] Preserve compound condition lists in `if/while/until` (multiple commands / `;` / `&&` / `||` outside `[[ ]]`)
- [x] Subshell isolation: best-effort translation with warning; strict mode should fail on `(...)`

### Missing Core Constructs
- [x] Handle `T_DollarBraced` parameter expansions
  - [x] `${var:-default}` → `set -q var; or set var default`
  - [x] `${var:=default}` → Set and use default
  - [x] `${var:?error}` → Error if unset
  - [x] `${var:+alternate}` → Use alternate if set
  - [x] `${var-default}` → Use default if unset (no empty check)
  - [x] `${var=default}` → Assign default if unset
  - [x] `${var?error}` → Error if unset
  - [x] `${var+alternate}` → Use alternate if set
- [x] Handle `T_DollarArithmetic`: `$((expr))` → `math` command
- [x] Handle `T_ForArithmetic`: `for ((i=0; i<10; i++))` → `begin; set i 0; while ...; ...; end`
- [x] Handle `T_Condition`: `[[ ]]` double brackets
  - [x] String pattern matching: `[[ $x == pat* ]]`
  - [x] Regex matching: `[[ $x =~ regex ]]`
- [x] Handle arrays (`T_Array`, `T_IndexedElement`)
  - [x] Array literals: `arr=(a b c)` → `set arr a b c`
  - [x] Array indexing: `${arr[0]}` → `$arr[1]` (Fish is 1-indexed!)
  - [x] Dynamic index adjustment for non-literal indices (e.g., `${arr[$i]}`)

## 🟡 High Priority (Semantic Correctness)

### Variable Scoping
- [x] Track and translate variable scopes correctly
  - [x] `export VAR=value` → `set -gx VAR value`
  - [x] `local VAR=value` → `set -l VAR value`  
  - [x] Global variables → `set -g VAR value`
- [x] Handle `VAR=value command` syntax (temporary env vars)

### I/O & Redirections
- [x] File descriptor redirections (`T_FdRedirect`)
  - [x] `2>&1` (Fish accepts `2>&1`)
  - [x] `&>file` → `>file 2>&1`
  - [x] `exec 3< file` → Translate to `exec` with warning (may need manual check)
  - [x] Redirection attachment for non-command statements
- [x] Here documents (`T_HereDoc`)
  - [x] Basic: `cat <<EOF ... EOF`
  - [x] With variable expansion vs literal
- [x] Process substitution (`T_ProcSub`)
  - [x] `<(cmd)` → `(cmd | psub)`
  - [x] `>(cmd)` → FIFO + background pipeline workaround via temp dir + cleanup block
  - [x] FIFO cleanup/teardown after use

### Control Flow
- [x] Select loops (`T_SelectIn`) - emulated with read loop
- [x] Trap handling - lower simple `EXIT`/signal traps to Fish event handlers
- [x] Proper exit code propagation
- [x] Preserve `source file args...` in recursive inline mode (set `argv` around inlined body)

## 🟢 Medium Priority (Completeness)

### String Operations
- [x] Substring extraction: `${var:offset:length}`
- [x] Pattern removal: `${var#pattern}`, `${var##pattern}`
- [x] Pattern replacement: `${var/old/new}`
- [x] Case modification: `${var^^}`, `${var,,}`
  - [x] Document which expansions are lossy vs exact

### Advanced Features
- [x] Extended globs (`T_Extglob`): `?(pat)`, `*(pat)`
- [x] Glob patterns (`T_Glob`): Translate glob syntax differences
- [x] Command substitution variations
  - [x] Backticks: `` `cmd` `` → `(cmd)`
  - [x] Nested substitutions

### Bash Built-ins
- [x] Map bash built-ins to Fish equivalents
  - [x] `pushd`/`popd` → Fish has these
  - [x] `declare` → `set` with appropriate flags
  - [x] `readonly` → No direct equivalent
  - [x] `shift` → `set argv $argv[2..-1]`

## 🔵 Nice to Have (Polish)

### Optimizations (cleanup, not semantic blockers)
- [x] Add an explicit post-translation simplification pass
- [x] Flatten safe consecutive `set` prelude wrappers without merging `set` commands that change overwrite/list semantics
- [x] Simplify remaining redundant subshell-like wrappers where scope/status behavior is unchanged
- [x] Optimize pipeline constructs only when wrapper elision is provably a no-op

### Architecture cleanup
- [x] Split the public library surface into `Monk.Translation`, `Monk.AST`, `Monk.Source`, and a thin `Monk` convenience facade
- [x] Move shared fixture metadata and shell helpers into a private internal support library used by tests and bake-off
- [x] Split the bake-off into a separate private library and executable under `scripts/` so bake-off-only dependencies do not affect the main `monk` library
- [x] Add focused seam coverage for source-graph behavior, shared harness helpers, and bake-off selection/report planning

### User Experience  
- [x] Add `--strict` mode to fail on unsupported constructs
- [x] Generate comments explaining non-trivial translations (subshell/read flags/set -e/arith short-circuit)
- [x] Provide confidence scores for translations
- [x] Create migration guide for manual fixes
- [x] Emit warnings for best-effort subshell translation (non-isolating)

### Testing Improvements
- [x] Add golden tests with known good translations
- [x] Create corpus of bash scripts for testing
- [x] Differential testing: run both bash and fish, compare outputs
- [x] Property: translated script output ≈ original script output
- [x] Run semantic parity suites in CI with `fish` installed and `MONK_INTEGRATION=1`
- [x] Add real-world fixtures from external repos with manual fish translations for small scripts
- [x] Add fixture metadata via `.args` and `.stdin` sidecar files for safe non-interactive tests
- [x] Add semantic tests for `${var:=...}` / `${var:?err}` side effects and error propagation
- [x] Add semantic tests for `((i++))`, `((i+=n))`, and `((expr))` status behavior (partial coverage)
- [x] Add semantic tests for `case` pattern globs and fallthrough behavior
- [x] Add semantic tests for `source file args...` and `$argv` in sourced scripts
- [x] Add semantic tests for `until` with compound conditions (`&&`/`||`)
- [x] Add semantic tests for subshell isolation warnings / strict-mode failure
- [x] Add semantic tests for side-effecting expansions in redirections/heredocs/case switches
- [x] Add semantic tests for arithmetic short-circuit/ternary lowering
- [x] Add golden fixture for mixed expansion+glob case patterns
- [x] Add semantic tests for `read` flags (`-r`, `-n`, `-t`, `-u`, `-a`) and IFS splitting
- [x] Add Polysemy effect tests per `docs/design/polysemy_testing_strategy.md`
- [x] Add direct warning-path tests for `shopt`, delimiter/silent `read`, malformed `trap`/`shift`/`unset`/`declare`/`local`/`export`, and unsupported arithmetic `for ((...))` fallbacks
- [x] Expand generated differential cases beyond basic arithmetic/arrays/pipelines to cover args, stdin, env-prefix commands, here-strings, and case/glob behavior
- [x] Add runtime parity fixtures for side-effecting parameter expansion in command arguments, redirection targets, and `case` switch expressions
- [x] Add runtime parity coverage for simple `<(...)` process substitution
- [x] Add runtime parity coverage for recursive literal `source` with argv and environment effects
- [x] Extend fixture sidecars to cover test prerequisites, recursive-source mode, run mode, and platform gating

## 📋 Implementation Checklist

### Stage 0: Refactor & Setup
- [x] Split translator into modules (Monad, Variables, Commands, Control, IO, Redirections)
- [x] Rewire top-level `Language.Fish.Translator` to delegate to submodules
- [x] Update Cabal `other-modules`

### Stage 1: Foundation
- [x] Create `Language.Fish.Translator.Monad` module
- [x] Implement TranslateM scaffolding and error/warning types
- [x] Thread monad through translation entry points
- [x] Add source location tracking

### Stage 2: Variables & Expansion
- [x] Create `Language.Fish.Translator.Variables` module  
- [x] Implement all parameter expansion forms (substring/pattern/case mods)
- [x] Add variable scope tracking
- [x] Handle arrays properly (dynamic index adjustments)

### Stage 3: Control Flow
- [x] Create `Language.Fish.Translator.Control` module
- [x] Handle all loop types
- [x] Implement condition translations
- [x] Add select loop emulation

### Stage 4: I/O & Redirections
- [x] Create `Language.Fish.Translator.IO` module
- [x] Handle all redirection types (exec fd setup emits warnings for manual review)
- [x] Implement heredoc support
- [x] Add process substitution
  - [x] Track/emit cleanup for FIFO-based `>(cmd)` translation

### Stage 5: Testing & Polish
- [x] Add comprehensive test suite
- [x] Create benchmark scripts
- [x] Write documentation (README + design notes)
- [ ] Handle edge cases
  - [x] Add gated runtime coverage for side-effecting parameter expansions in args, redirections, and `case`
  - [x] Add gated runtime coverage for simple `<(...)`
  - [x] Add gated runtime coverage for recursive literal `source`
  - [x] Add Linux-gated runtime coverage for `>(...)`
  - [x] Promote the drafted background-job fixtures (`background-success-wait`, `background-fail-wait`, `background-pipefail`, `background-jobs`) into the gated suite once parity is fixed
  - [x] Add a reduced generated-output regression slice derived from `neofetch`
  - [x] Close the command-substitution `set -e` gap exposed by `realworld/echo-args`

## ✅ Added Tests Scope (current)

- Unit: translation shape plus warning-path coverage for arithmetic, `read`, `trap`, `source`, `set` options, and Polysemy effect behavior.
- Property: pretty-printing and translation invariants, plus bash-vs-fish output equivalence on generated script families.
- Integration: gated bash-vs-fish parity for focused semantic fixtures, including the generalized `read -d` surface and helper-backed `>(...)` fixtures, plus curated generated-output real-world fixtures such as `neofetch-mini`, `argparse-mini`, `envfile-preview`, `path-filter`, and `semver-normalize` under `MONK_INTEGRATION=1`.
- Translation-shape coverage: nested prelude flattening, nested trivial `begin` cleanup, pipeline-local wrapper elision, and scope-changing wrapper preservation now have focused unit coverage.
- Architecture/seam coverage: `Monk.Source`, the shared harness layer, and the bake-off selection/report/benchmark seams now have direct unit tests so refactors fail faster than the full integration pass.

## 🐛 Known Semantic Differences to Document

1. **Array indexing**: Bash is 0-indexed, Fish is 1-indexed
2. **Exit on error**: Bash `set -e` vs Fish default behavior  
3. **Word splitting**: Fish doesn't split variables by default; we only emulate with `string split -- $IFS` when list semantics would fail
4. **Glob expansion**: Different glob syntax and behavior
5. **Function scope**: Fish functions have different scoping rules
6. **Background jobs**: Different job control semantics
7. **Process substitution**: `>(cmd)` now uses a generated FIFO helper and still remains Linux-gated for runtime evidence
8. **Param expansions**: glob-to-regex conversion and `^`/`,` case mods are approximate
9. **`read` semantics**: flag parity and IFS splitting differ from bash; warnings emitted for lossy cases
10. **Arithmetic side effects**: short-circuit/ternary are emulated via temp vars; verify on edge cases
11. **`set -e`/`pipefail`**: emulated via `or exit $status` and `__monk_pipefail`; translated background jobs / `wait` now have focused parity coverage, but some compound-list edge cases still diverge
12. **`shopt`**: ignored with a warning; no fish emulation exists
13. **Residual `read` fallbacks**: the generalized helper path now covers covered `read -d` forms plus numeric `-u` helper-backed reads, but no-var delimiter reads, non-numeric fd values, and unsupported option clusters still remain warning-driven

## ✅ Quick Wins (Can do immediately)

- [x] Fix `translateExit` to handle non-numeric arguments
- [x] Add `T_Glob` basic handling

## Bake-off notes

- 2026-04-15: the Haskell `monk-bakeoff` runner is now the operative comparison surface. The current full-run summary lives in `docs/babelfish-comparison.md`, and the runner now respects fixture metadata instead of hanging on Linux-only cases during local macOS runs.
- 2026-04-16: a focused `monk-bakeoff` run over the new `read` and `>(...)` fixtures (`/private/tmp/monk-bakeoff-read-procsub`) showed Monk succeeding on all 5 runnable fixtures with no translation failures or runtime failures. The 3 helper-backed `>(...)` fixtures were skipped on local macOS because their metadata still treats Linux as the semantic source of truth. On the overlapping runnable surface, Babelfish mismatched 4 delimiter fixtures and failed translation on `read-delimiter-null-array`.
- 2026-04-16: the post-refactor full `monk-bakeoff` run (`/private/tmp/monk-bakeoff-architecture-full-final`) covered 62 fixtures, skipped 4 by metadata, and completed with Monk translating/running all 58 non-skipped fixtures successfully. Babelfish translated 35/58 non-skipped fixtures and the overlapping runtime surface differed on 13 fixtures.
- Case patterns with expansions now build the full pattern via `printf` in a command substitution to avoid empty-string failures and glob expansion surprises.
- Details recorded in `docs/babelfish-comparison.md`.
- [x] Implement `T_HereString`: `<<<` → echo piping
- [x] Handle `time` command prefix
- [x] Add `T_CoProcBody` with warning (not supported in Fish)
- [x] Improve error messages from generic "Skipped token"

## ▶ Next Up (Recommended Order)

1. [ ] **Keep the checklist, audit, and migration guide in lockstep**
   - [ ] Refresh this file, `docs/design/translator-audit.md`, and `docs/migration-guide.md` together whenever a best-effort branch changes status.
   - [ ] Treat `docs/design/translator-audit.md` as the fidelity source of truth and only check off TODO items once runtime evidence or explicit best-effort documentation exists.
2. [x] **Cleanup-first pass: finish the conservative simplifier and helper refactors**
   - [x] Turn each unchecked optimization box into a targeted simplifier rule with translation-shape coverage.
   - [x] Keep hard exclusions around redirections, background jobs, pipelines, conjunctions, control-flow blocks, and scope-changing wrappers.
   - [x] Refactor `Commands/Read.hs` into explicit exact-read phases without changing helper names, supported branches, or warning-driven fallbacks.
   - [x] Refactor `Variables/ProcessSubst.hs` to share FIFO setup/cleanup builders between the inline and helper-backed `>(...)` paths.
   - [x] Keep full `neofetch` bake-off-only, but gate a reduced automatable generated-output slice.
3. [ ] **Tracked semantic backlog: tighten the remaining `read` fallback surface**
   - [x] Replace the old narrow helper path with a generalized exact branch for covered `read -d` cases, including empty delimiters, arrays, multi-variable assignment, supported mixed flag clusters, and numeric `-u` helper-backed reads.
   - [ ] Keep no-var delimiter reads, non-numeric fd values, and unsupported flag clusters warning-driven unless they can be proven exact.
   - [ ] Add more focused parity fixtures only if they cover branches that are still warning-driven today.
4. [ ] **Tracked semantic backlog: record Linux-source-of-truth evidence for helper-backed `>(...)`**
   - [x] Replace the inline FIFO expansion with a generated helper so the runtime path has one implementation surface.
   - [x] Broaden the Linux-gated fixture surface beyond the current single output-process-substitution case.
   - [ ] Record explicit Linux runtime evidence before upgrading the audit status or claiming stronger cross-platform parity.
