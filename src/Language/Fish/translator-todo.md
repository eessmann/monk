# Bash to Fish Translator - TODO Checklist

## 🔴 Critical (Blocking Issues)

-### Foundation
- [x] Implement TranslateM monad with StateT for metadata tracking
  - [x] Define TranslateState with source locations, warnings, context
  - [x] Define TranslateError for unsupported constructs
  - [x] Add TranslateConfig for user options
- [ ] Add source location preservation from ShellCheck AST
- [ ] Implement error recovery and warning accumulation

### Missing Core Constructs
- [ ] Handle `T_DollarBraced` parameter expansions
  - [ ] `${var:-default}` → `set -q var; or set var default`
  - [ ] `${var:=default}` → Set and use default
  - [ ] `${var:?error}` → Error if unset
  - [ ] `${var:+alternate}` → Use alternate if set
- [ ] Handle `T_DollarArithmetic`: `$((expr))` → `math` command
- [ ] Handle `T_ForArithmetic`: `for ((i=0; i<10; i++))` → `for i in (seq 0 9)`
- [ ] Handle `T_Condition`: `[[ ]]` double brackets
  - [ ] String pattern matching: `[[ $x == pat* ]]`
  - [ ] Regex matching: `[[ $x =~ regex ]]`
- [ ] Handle arrays (`T_Array`, `T_IndexedElement`)
  - [ ] Array literals: `arr=(a b c)` → `set arr a b c`
  - [ ] Array indexing: `${arr[0]}` → `$arr[1]` (Fish is 1-indexed!)

## 🟡 High Priority (Semantic Correctness)

### Variable Scoping
- [ ] Track and translate variable scopes correctly
  - [ ] `export VAR=value` → `set -gx VAR value`
  - [ ] `local VAR=value` → `set -l VAR value`  
  - [ ] Global variables → `set -g VAR value`
- [ ] Handle `VAR=value command` syntax (temporary env vars)

### I/O & Redirections
- [ ] File descriptor redirections (`T_FdRedirect`)
  - [ ] `2>&1` → `^&1` (Fish syntax)
  - [ ] `&>file` → `>file 2>&1`
  - [ ] `exec 3< file` → No direct equivalent (needs workaround)
- [ ] Here documents (`T_HereDoc`)
  - [ ] Basic: `cat <<EOF ... EOF`
  - [ ] With variable expansion vs literal
- [ ] Process substitution (`T_ProcSub`)
  - [ ] `<(cmd)` → `(cmd | psub)`
  - [ ] `>(cmd)` → Complex workaround needed

### Control Flow
- [ ] Select loops (`T_SelectIn`) - needs emulation with read loop
- [ ] Trap handling - map to Fish trap syntax
- [ ] Proper exit code propagation

## 🟢 Medium Priority (Completeness)

### String Operations
- [ ] Substring extraction: `${var:offset:length}`
- [ ] Pattern removal: `${var#pattern}`, `${var##pattern}`
- [ ] Pattern replacement: `${var/old/new}`
- [ ] Case modification: `${var^^}`, `${var,,}`

### Advanced Features
- [ ] Extended globs (`T_Extglob`): `?(pat)`, `*(pat)`
- [ ] Glob patterns (`T_Glob`): Translate glob syntax differences
- [ ] Command substitution variations
  - [ ] Backticks: `` `cmd` `` → `(cmd)`
  - [ ] Nested substitutions

### Bash Built-ins
- [ ] Map bash built-ins to Fish equivalents
  - [ ] `pushd`/`popd` → Fish has these
  - [ ] `declare` → `set` with appropriate flags
  - [ ] `readonly` → No direct equivalent
  - [ ] `shift` → `set argv $argv[2..-1]`

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
- [ ] Add golden tests with known good translations
- [ ] Create corpus of real-world bash scripts for testing
- [ ] Differential testing: run both bash and fish, compare outputs
- [ ] Property: translated script output ≈ original script output

## 📋 Implementation Checklist

### Week 0: Refactor & Setup
- [x] Split translator into modules (Monad, Variables, Commands, Control, IO, Redirections)
- [x] Rewire top-level `Language.Fish.Translator` to delegate to submodules
- [x] Update Cabal `other-modules`

### Week 1: Foundation
- [x] Create `Language.Fish.Translator.Monad` module
- [x] Implement TranslateM scaffolding and error/warning types
- [ ] Thread monad through translation entry points
- [ ] Add source location tracking

### Week 2: Variables & Expansion
- [ ] Create `Language.Fish.Translator.Variables` module  
- [ ] Implement all parameter expansion forms
- [ ] Add variable scope tracking
- [ ] Handle arrays properly

### Week 3: Control Flow
- [ ] Create `Language.Fish.Translator.Control` module
- [ ] Handle all loop types
- [ ] Implement condition translations
- [ ] Add select loop emulation

### Week 4: I/O & Redirections
- [ ] Create `Language.Fish.Translator.IO` module
- [ ] Handle all redirection types
- [ ] Implement heredoc support
- [ ] Add process substitution

### Week 5: Testing & Polish
- [ ] Add comprehensive test suite
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

## ✅ Quick Wins (Can do immediately)

- [ ] Fix `translateExit` to handle non-numeric arguments
- [ ] Add `T_Glob` basic handling
- [ ] Implement `T_HereString`: `<<<` → echo piping
- [ ] Handle `time` command prefix
- [ ] Add `T_CoProcBody` with warning (not supported in Fish)
- [ ] Improve error messages from generic "Skipped token"
