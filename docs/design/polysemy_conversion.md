# Polysemy Conversion Plan (Full)

## Summary
Converted the translation stack from `StateT`/`Either` to Polysemy across the entire codebase in one pass. The effect set in use is:
- `Input` for token ranges (position lookups)
- `Reader` for config
- `State` for context + warnings + source map + token ranges + range stack
- `Writer` for warnings (in addition to state, to preserve current shape)
- `Error` for `TranslateError`

`polysemy` and `polysemy-plugin` are enabled in `monk.cabal`.

## Status
Complete. See `docs/design/polysemy_testing_strategy.md` for validation guidance.

## Goals
- Preserve existing behavior and output.
- Keep public `TranslateConfig`, `TranslateState`, `TranslateError`, `Warning` types stable unless required.
- Improve testability by swapping interpreters and injecting effects.
- Full conversion in one pass (no parallel `TranslateM` + Polysemy shims).

## Non-Goals
- Redesigning translation semantics or behavior.
- Rewriting Pretty or AST types.
- Changing test fixtures other than what’s required for compilation or semantics parity.

## Effect Stack (Implemented)
```
type TranslateEffs =
  [ Input (Map Id SourceRange)
  , Reader TranslateConfig
  , State TranslateState
  , Writer [Warning]
  , Error TranslateError
  ]
```

Notes:
- `State` retains current fields (context/warnings/source map/token ranges/range stack).
- `Writer [Warning]` collects warnings in parallel; interpreter will merge writer output into `TranslateState.warnings` to preserve current return shape.
- `Input` provides token ranges. We can keep `tokenRanges` in state for compatibility, but use `Input` for lookups to make testing and isolation easier.

## Files to Touch
- `monk.cabal` (add `polysemy`, `polysemy-plugin`, and any required flags)
- `src/Language/Fish/Translator/Monad.hs` (rewrite around Polysemy)
- `src/Language/Fish/Translator.hs` (update usage)
- `src/Language/Fish/Translator/*.hs` (swap `TranslateM` usage to Polysemy)
- `src/Monk.hs` (if needed for new runner exports)
- Tests that depend on `TranslateState` shape or warnings (likely unchanged if we merge writer output into state)

## Migration Plan (Task Checklist)
1. **Dependencies & Compiler**
   - [x] Add `polysemy`, `polysemy-plugin` to `monk.cabal`.
   - [x] Enable the Polysemy plugin in the library/test stanzas.
   - [x] Confirm build with a no-op change.

2. **New Core Types & Runners**
   - [x] Redefine `TranslateM` as `Sem TranslateEffs`.
   - [x] Add `runTranslate*` helpers that interpret `Reader`, `State`, `Error`, `Writer`, `Input`.
   - [x] Ensure `runTranslate*` merges writer warnings into `TranslateState.warnings`.
   - [x] Update `withTokenRange`, `addWarning`, `unsupported` to use Polysemy effects.

3. **Port `Translator.Monad` API**
   - [x] Preserve exported API in `Language.Fish.Translator.Monad`.
   - [x] Keep `TranslateConfig`, `TranslateState`, `TranslateError`, `Warning`.
   - [x] Adapt `runTranslate`, `runTranslateWithPositions`, `evalTranslate*` to Polysemy.

4. **Module-by-Module Translation**
   - [x] `Language.Fish.Translator.Variables`
   - [x] `Language.Fish.Translator.Commands`
   - [x] `Language.Fish.Translator.Control`
   - [x] `Language.Fish.Translator.Redirections`
   - [x] `Language.Fish.Translator.IO`
   - [x] `Language.Fish.Translator.Builtins`
   - [x] `Language.Fish.Translator.ForArithmetic`
   - [x] `Language.Fish.Translator` top-level glue

5. **Adjust Call Sites**
   - [x] `Monk.translateParseResult` now returns a `TranslationResult` wrapper.
   - [x] Tests updated to use `translationState` / `renderTranslation`.

6. **Cleanup**
   - [x] Remove any leftover `StateT` imports and old helper shims.
   - [x] Ensure no duplicate warning accumulation paths.

7. **Verification**
   - [x] `cabal test` must pass.
   - [x] Spot-check translations with `cabal run monk -- <script>`.

## Interpreter Details
- `runTranslateWithPositions` should:
  1. Initialize `TranslateState` with the given config and positions.
  2. `runInputConst` with the token ranges map.
  3. `runReader` with config.
  4. `runState` to thread state.
  5. `runWriter` to collect warnings.
  6. `runError` for `TranslateError`.
  7. Merge writer warnings into `TranslateState.warnings`.

## Risks & Mitigations
- **Risk:** Double warning accumulation (state + writer).
  - **Mitigation:** Keep `addWarning` writer-only and merge once in the runner.
- **Risk:** Missing strictness or ordering issues in Polysemy stack.
  - **Mitigation:** Keep effect order consistent and avoid custom interpreters until stable.
- **Risk:** Test breakage due to state shape changes.
  - **Mitigation:** Keep `TranslateState` fields unchanged; only change internal wiring.

## Acceptance Criteria
- All existing tests pass.
- No public API changes beyond internal effect wiring.
- Translators produce identical output for all fixtures and properties.
