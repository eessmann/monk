# Bash to Fish Translator - TODO Checklist

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
  - [x] `>(cmd)` → FIFO + background pipeline workaround (cleanup TBD)
  - [x] FIFO cleanup/teardown after use

### Control Flow
- [x] Select loops (`T_SelectIn`) - emulated with read loop
- [x] Trap handling - map to Fish trap syntax
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

### Optimizations
- [ ] Combine consecutive `set` statements
- [ ] Simplify redundant subshells: `(echo x)` → `echo x`
- [ ] Optimize pipeline constructs

### User Experience  
- [x] Add `--strict` mode to fail on unsupported constructs
- [x] Generate comments explaining non-trivial translations (subshell/read flags/set -e/arith short-circuit)
- [x] Provide confidence scores for translations
- [ ] Create migration guide for manual fixes
- [x] Emit warnings for best-effort subshell translation (non-isolating)

### Testing Improvements
- [x] Add golden tests with known good translations
- [x] Create corpus of bash scripts for testing
- [x] Differential testing: run both bash and fish, compare outputs
- [x] Property: translated script output ≈ original script output
- [x] Add real-world fixtures from external repos with manual fish translations for small scripts
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
- [ ] Write documentation
- [ ] Handle edge cases

## ✅ Added Tests Scope (planned)

- Unit: basic command translation (`echo`, `exit`), bracket test `[ ... ]`, pipelines, `if/then/else`, `for ... in`, function bodies, backgrounding.
- Property: generated word lists in `echo` preserve arguments after translation (quoted rendering), pipeline continuation counts preserved.

## 🐛 Known Semantic Differences to Document

1. **Array indexing**: Bash is 0-indexed, Fish is 1-indexed
2. **Exit on error**: Bash `set -e` vs Fish default behavior  
3. **Word splitting**: Fish doesn't split variables by default; we only emulate with `string split -- $IFS` when list semantics would fail
4. **Glob expansion**: Different glob syntax and behavior
5. **Function scope**: Fish functions have different scoping rules
6. **Background jobs**: Different job control semantics
7. **Process substitution**: `>(cmd)` uses FIFO workaround; cleanup via background block
8. **Param expansions**: glob-to-regex conversion and `^`/`,` case mods are approximate
9. **`read` semantics**: flag parity and IFS splitting differ from bash; warnings emitted for lossy cases
10. **Arithmetic side effects**: short-circuit/ternary are emulated via temp vars; verify on edge cases
11. **`set -e`/`pipefail`**: bash options have no fish equivalent; translation emits notes only

## ✅ Quick Wins (Can do immediately)

- [x] Fix `translateExit` to handle non-numeric arguments
- [x] Add `T_Glob` basic handling

## Bake-off notes

- 2026-02-07: ran Monk vs babelfish on corpus + benchmark + integration + golden + real-world fixtures. Babelfish failed on `medium` (unsupported UnaryArithm), `large` (unsupported ForClause), `extglob-basic` (unsupported ExtGlob), and the real-world pyramid fixtures (unsupported C-style for loop). Monk succeeded on all, with warnings for `set -e/-u/pipefail`, IFS splitting, and subshell best-effort in `time-prefix`.
- Case patterns with expansions now build the full pattern via `printf` in a command substitution to avoid empty-string failures and glob expansion surprises.
- Details recorded in `docs/babelfish-comparison.md`.
- [x] Implement `T_HereString`: `<<<` → echo piping
- [x] Handle `time` command prefix
- [x] Add `T_CoProcBody` with warning (not supported in Fish)
- [x] Improve error messages from generic "Skipped token"

## ▶ Next Up (Recommended Order)

1. [ ] **Triage remaining semantic gaps**
   - [ ] Validate short-circuit/ternary arithmetic semantics on edge cases
   - [ ] `read` flags (`-r`, `-n`, `-t`, `-u`, `-a`) and IFS splitting parity
   - [ ] `set -e` / `errexit` semantics and pipeline failure behavior
   - [ ] Non-literal `source`/`.` paths in recursive translation
2. [ ] **Generate inline translation notes**
   - [ ] Emit comments in Fish output for lossy translations and semantic gaps.
3. [x] **Create benchmark scripts**
   - [x] Add real-world translation inputs for perf and regression tracking.
4. [ ] **Handle edge cases**
   - [ ] Word splitting differences (`"$var"` vs `$var` list expansion), only when fish list semantics would fail.
   - [ ] `printf` vs `echo` portability and escape handling.
