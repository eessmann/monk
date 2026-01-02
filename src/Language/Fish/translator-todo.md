# Bash to Fish Translator - TODO Checklist

## 🔴 Critical (Blocking Issues)

-### Foundation
- [x] Implement TranslateM monad with StateT for metadata tracking
  - [x] Define TranslateState with source locations, warnings, context
  - [x] Define TranslateError for unsupported constructs
  - [x] Add TranslateConfig for user options
- [x] Add source location preservation from ShellCheck AST
- [ ] Implement error recovery and warning accumulation

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
- [ ] Command substitution variations
  - [x] Backticks: `` `cmd` `` → `(cmd)`
  - [ ] Nested substitutions

### Bash Built-ins
- [ ] Map bash built-ins to Fish equivalents
  - [ ] `pushd`/`popd` → Fish has these
  - [x] `declare` → `set` with appropriate flags
  - [x] `readonly` → No direct equivalent
  - [x] `shift` → `set argv $argv[2..-1]`

## 🔵 Nice to Have (Polish)

### Optimizations
- [ ] Combine consecutive `set` statements
- [ ] Simplify redundant subshells: `(echo x)` → `echo x`
- [ ] Optimize pipeline constructs

### User Experience  
- [ ] Add `--strict` mode to fail on unsupported constructs
- [ ] Generate comments explaining non-trivial translations
- [ ] Provide confidence scores for translations
- [ ] Create migration guide for manual fixes

### Testing Improvements
- [x] Add golden tests with known good translations
- [x] Create corpus of bash scripts for testing
- [x] Differential testing: run both bash and fish, compare outputs
- [ ] Property: translated script output ≈ original script output

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
- [ ] Create benchmark scripts
- [ ] Write documentation
- [ ] Handle edge cases

## ✅ Added Tests Scope (planned)

- Unit: basic command translation (`echo`, `exit`), bracket test `[ ... ]`, pipelines, `if/then/else`, `for ... in`, function bodies, backgrounding.
- Property: generated word lists in `echo` preserve arguments after translation (quoted rendering), pipeline continuation counts preserved.


## 🐛 Known Semantic Differences to Document

1. **Array indexing**: Bash is 0-indexed, Fish is 1-indexed
2. **Exit on error**: Bash `set -e` vs Fish default behavior  
3. **Word splitting**: Fish doesn't split variables by default
4. **Glob expansion**: Different glob syntax and behavior
5. **Function scope**: Fish functions have different scoping rules
6. **Background jobs**: Different job control semantics
7. **Process substitution**: `>(cmd)` uses FIFO workaround; cleanup via background block
8. **Param expansions**: glob-to-regex conversion and `^`/`,` case mods are approximate

## ✅ Quick Wins (Can do immediately)

- [x] Fix `translateExit` to handle non-numeric arguments
- [ ] Add `T_Glob` basic handling
- [x] Implement `T_HereString`: `<<<` → echo piping
- [ ] Handle `time` command prefix
- [ ] Add `T_CoProcBody` with warning (not supported in Fish)
- [ ] Improve error messages from generic "Skipped token"

## ▶ Next Up (Recommended Order)

1. **Error recovery + strict mode**
   - Accumulate warnings, surface `Unsupported` in strict mode.
2. **Built-in mapping**
   - `declare`, `readonly`, `shift`, and other bash built-ins.
3. **Nested command substitutions**
   - Ensure nested `$(...)` and backticks translate fully.
4. **Output-equivalence property tests**
   - Property: translated script output ≈ original script output.
