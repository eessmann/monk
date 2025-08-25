# Bash to Fish Translator - Code Review & Analysis

## Executive Summary

The current implementation provides a strong foundation with a well-designed Fish AST using GADTs for type safety. However, the translator lacks coverage for many Bash constructs and doesn't implement the planned state monad for metadata tracking. Approximately 40% of common Bash features are properly translated, with critical gaps in variable expansion, advanced redirections, and arithmetic operations.

## Strengths

### AST Design (`src/Language/Fish/AST.hs`)
✅ **Type-safe GADT design** - Excellent use of GADTs with phantom types for semantic correctness  
✅ **Comprehensive Fish coverage** - Most Fish shell constructs are well-represented  
✅ **NonEmpty lists** - Good use of NonEmpty for constructs that require at least one element  
✅ **Proper job control model** - FishJobPipeline and FishJobConjunction correctly model Fish's job semantics  

### Pretty Printer (`src/Language/Fish/Pretty.hs`)
✅ **Clean implementation** - Well-structured pretty printer using prettyprinter library  
✅ **Proper escaping** - Handles single/double quote escaping correctly  

### Testing (`test/Spec.hs`)
✅ **Good test coverage** - Both unit tests and property-based tests  
✅ **QuickCheck generators** - Well-designed generators for testing  

## Critical Issues

### 1. Missing State Monad
**Issue**: Despite the stated intention, no state monad is implemented for tracking:
- Source locations
- Comments
- Variable scoping context
- Translation warnings/errors

**Impact**: Cannot preserve source location information or provide meaningful error messages

### 2. Incomplete ShellCheck Token Coverage

#### Completely Missing Translations:
- `T_ForArithmetic` - C-style for loops: `for ((i=0; i<10; i++))`
- `T_Condition` - Double bracket conditions: `[[ $x == pattern ]]`
- `T_DollarBraced` - Parameter expansions: `${var:-default}`, `${var#pattern}`
- `T_DollarArithmetic` - Arithmetic expansion: `$((x + 1))`
- `T_Extglob` - Extended globs: `?(pattern)`, `*(pattern)`
- `T_ProcSub` - Process substitution: `<(command)`, `>(command)`
- `T_FdRedirect` - FD redirections: `2>&1`, `3>&-`
- `T_HereDoc` - Here documents: `cat <<EOF`
- `T_HereString` - Here strings: `cat <<<$var`
- `T_SelectIn` - Select loops: `select x in a b c`
- `T_Array` - Array literals: `(a b c)`
- `T_IndexedElement` - Array indexing: `${arr[0]}`
- `T_Glob` - Glob patterns: `*.txt`, `file?.log`

#### Partially Handled:
- **Variable assignments**: Only simple `VAR=value`, not arrays or exports
- **Redirections**: Only basic `>`, `>>`, `<`, missing FD manipulation
- **Arithmetic**: Only via `math` command, not inline expressions
- **Test conditions**: Only `[`, not `[[` with pattern matching

### 3. Semantic Gaps

#### Variable Scoping
```haskell
-- Current: all assignments are local
translateAssignment :: Token -> Maybe FishStatement
translateAssignment tok = 
  -- Always uses ScopeLocal, losing export/global semantics
```

**Fish equivalent needed**:
- `export VAR=value` → `set -gx VAR value`
- `local VAR=value` → `set -l VAR value`
- `VAR=value cmd` → Needs special handling in Fish

#### Pipeline Status
Bash's `PIPESTATUS` array vs Fish's `$pipestatus` - no translation handling

#### Exit Code Propagation
- `set -e` behavior not tracked
- `set -o pipefail` not handled

### 4. Lost Semantics Examples

#### Example 1: Parameter Expansion
```bash
# Bash
echo ${var:-default}     # Use default if var unset
echo ${var:=default}     # Set and use default
echo ${var:?error}       # Error if unset
echo ${var:+alternate}   # Use alternate if set
```
**Current**: Becomes generic comment "Skipped token"

#### Example 2: Arrays
```bash
# Bash
arr=(one two three)
echo ${arr[0]}
echo ${#arr[@]}
```
**Current**: Not recognized as array construct

#### Example 3: Complex Redirections
```bash
# Bash
cmd 2>&1 | tee log.txt
exec 3<&0
cmd >&3
```
**Current**: FD redirections ignored

## Architecture Issues

### 1. No Error Recovery
The translator silently converts unsupported constructs to comments, making it impossible to:
- Warn users about potential semantic changes
- Suggest manual intervention for complex cases
- Track translation confidence levels

### 2. No Context Propagation
Without state/reader monad:
- Cannot track whether we're in a function/loop/subshell
- Cannot maintain variable scope tables
- Cannot handle context-dependent translations

### 3. Monolithic Translation Functions
Large pattern matches make it hard to:
- Add new construct support incrementally
- Test individual translation rules
- Maintain and debug

## TODO List for Complete, Correct, and Robust Translator

### Phase 1: Foundation (High Priority)
- [ ] **Implement Translation Monad**
  ```haskell
  type TranslateM = StateT TranslateState (Either TranslateError)
  data TranslateState = TranslateState
    { sourceMap :: Map SourceRange FishStatement
    , varScopes :: [VariableScope]
    , warnings  :: [Warning]
    , context   :: TranslationContext
    }
  ```

- [ ] **Add Error Handling**
  - Define `TranslateError` type for unsupported constructs
  - Implement error recovery strategies
  - Add warning accumulation for semantic differences

- [ ] **Implement Source Tracking**
  - Preserve ShellCheck position info
  - Map to Fish AST nodes
  - Enable round-trip source mapping

### Phase 2: Core Constructs (High Priority)

- [ ] **Variable Expansions**
  - [ ] Default values: `${var:-default}`
  - [ ] Assignment: `${var:=default}`
  - [ ] Error on unset: `${var:?message}`
  - [ ] Alternate: `${var:+alt}`
  - [ ] Substring: `${var:offset:length}`
  - [ ] Pattern removal: `${var#pattern}`, `${var##pattern}`
  - [ ] Pattern replacement: `${var/old/new}`
  - [ ] Case modification: `${var^^}`, `${var,,}`

- [ ] **Arrays**
  - [ ] Array declaration: `arr=(a b c)`
  - [ ] Indexed access: `${arr[i]}`
  - [ ] Array length: `${#arr[@]}`
  - [ ] Array slicing: `${arr[@]:1:2}`
  - [ ] Associative arrays → Fish has no equivalent (warning needed)

- [ ] **Arithmetic**
  - [ ] Arithmetic expansion: `$((expr))`
  - [ ] C-style for loops: `for ((;;))`
  - [ ] Arithmetic conditions: `(( x > 5 ))`
  - [ ] Increment/decrement: `((i++))`, `((--j))`

- [ ] **Advanced Test Conditions**
  - [ ] Pattern matching in `[[`: `[[ $x == pat* ]]`
  - [ ] Regex matching: `[[ $x =~ regex ]]`
  - [ ] File comparison: `[[ file1 -nt file2 ]]`

### Phase 3: I/O and Redirections (Medium Priority)

- [ ] **File Descriptor Management**
  - [ ] FD duplication: `2>&1`
  - [ ] FD closing: `3>&-`
  - [ ] Exec redirections: `exec 3< file`
  - [ ] FD save/restore patterns

- [ ] **Here Documents**
  - [ ] Basic heredoc: `<<EOF`
  - [ ] Indented heredoc: `<<-EOF`
  - [ ] Quoted delimiter: `<<'EOF'`
  - [ ] Here strings: `<<<string`

- [ ] **Process Substitution**
  - [ ] Input: `<(command)` → Fish's `(command | psub)`
  - [ ] Output: `>(command)` → More complex in Fish

### Phase 4: Control Flow (Medium Priority)

- [ ] **Select Loops**
  - No direct Fish equivalent - needs emulation
  - Generate Fish function with read loop

- [ ] **Trap Handling**
  - Map signals to Fish's `trap` command
  - Handle EXIT, ERR, DEBUG, RETURN

- [ ] **Set Options**
  - [ ] `set -e` → Track and emulate
  - [ ] `set -u` → Fish default behavior
  - [ ] `set -o pipefail` → Needs manual handling
  - [ ] `set -x` → Fish's `set -x`

### Phase 5: Advanced Features (Lower Priority)

- [ ] **Globs and Patterns**
  - [ ] Extended globs: `?(pat)`, `*(pat)`, `+(pat)`
  - [ ] Brace expansion: `{a,b}{1,2}`
  - [ ] Tilde expansion: `~user`
  - [ ] Globstar: `**/*.txt`

- [ ] **Job Control**
  - [ ] Background jobs: `&`
  - [ ] Job control builtins: `jobs`, `fg`, `bg`
  - [ ] Disown: `disown`

- [ ] **Aliases and Functions**
  - [ ] Detect and expand aliases
  - [ ] Handle function export
  - [ ] Local variables in functions

### Phase 6: Optimization and Polish

- [ ] **Semantic Preserving Optimizations**
  - [ ] Combine consecutive `set` statements
  - [ ] Simplify redundant subshells
  - [ ] Optimize pipeline constructs

- [ ] **User Experience**
  - [ ] Add translation confidence scores
  - [ ] Generate migration guide for manual fixes
  - [ ] Provide alternative translations for ambiguous cases

- [ ] **Testing**
  - [ ] Differential testing against real bash/fish
  - [ ] Corpus of real-world scripts
  - [ ] Regression test suite

## Recommended Implementation Order

1. **Week 1-2**: Implement TranslateM monad and basic error handling
2. **Week 3-4**: Variable expansions and arrays
3. **Week 5-6**: Arithmetic and advanced conditions
4. **Week 7-8**: Redirections and heredocs
5. **Week 9-10**: Remaining control flow
6. **Week 11-12**: Testing and polish

## Code Quality Improvements

### Refactoring Suggestions

1. **Split translator into modules**:
   ```
   Language.Fish.Translator.Monad
   Language.Fish.Translator.Variables
   Language.Fish.Translator.Commands
   Language.Fish.Translator.Control
   Language.Fish.Translator.IO
   ```

2. **Add translation configuration**:
   ```haskell
   data TranslateConfig = TranslateConfig
     { strictMode :: Bool  -- Fail on unsupported constructs
     , preserveComments :: Bool
     , warningSeverity :: WarnLevel
     }
   ```

3. **Implement translation passes**:
   - Pass 1: Direct AST translation
   - Pass 2: Semantic analysis and warnings
   - Pass 3: Optimization
   - Pass 4: Pretty printing

## Testing Strategy

1. **Unit tests per construct**: Each Bash construct should have dedicated tests
2. **Property tests**: Ensure semantic preservation properties
3. **Golden tests**: Compare against known good translations
4. **Differential testing**: Run both bash and fish versions, compare outputs

## Conclusion

The current implementation is a good proof of concept but needs significant expansion to be production-ready. The type-safe AST is excellent, but the translator needs comprehensive coverage of Bash constructs and proper semantic preservation. Implementing the state monad and systematic handling of all ShellCheck tokens should be the immediate priority.