# Polysemy Testing Strategy (Translator)

## Purpose
Define how we will validate the Polysemy conversion for the Bash-to-Fish translator. This complements the conversion plan and focuses on test coverage for effect behavior, regression protection, and ease of authoring future tests.

## Goals
- Preserve current translation behavior after switching to Polysemy.
- Ensure each effect (Reader, State, Error, Writer, Input) is exercised in isolation and in composition.
- Keep tests fast and deterministic.
- Make it easy to add new tests without rewriting interpreters.

## Non-Goals
- Changing translation semantics.
- Rewriting existing golden fixtures unless behavior changes.
- Benchmarking performance (tracked separately).

## Effect Stack Under Test
```
TranslateEffs:
  Reader TranslateConfig
  State TranslateState
  Error TranslateError
  Writer [Warning]
  Input (Map Id SourceRange)
```

### Key behaviors to validate
- `Reader`: config flags influence translation decisions (`strict`, warnings, etc.).
- `State`: warnings, source map, context, and range stack are updated correctly.
- `Error`: unsupported constructs short-circuit with the right error payload.
- `Writer`: warnings are emitted and merged into state once.
- `Input`: token range lookups are stable and independent from state mutation.

## Test Layers

### 1) Unit Tests (Effect Behavior)
Focus: exercise the core Polysemy wiring and helpers in `Language.Fish.Translator.Monad`.
- `runTranslateWithPositions`:
  - merges `Writer` warnings into `TranslateState.warnings` exactly once.
  - returns `Left TranslateError` when `Error` is thrown.
  - carries `TranslateConfig` into `unsupported` and other reader-dependent behavior.
- `withTokenRange`:
  - pushes and pops ranges correctly for nested translations.
  - uses `Input` and does not depend on `TranslateState.tokenRanges`.
- `addWarning`:
  - adds to writer, and state merging occurs in the runner.

Suggested approach:
- Build a minimal `Sem TranslateEffs a` computation in tests and run it with `runTranslateWithPositions`.
- Use simple fake token range maps with 1-2 entries.

### 2) Unit Tests (Translator Modules)
Focus: each module still compiles and behaves as before, now via Polysemy.
- Maintain current unit tests for `Variables`, `Commands`, `Control`, `IO`, `Redirections`, `Builtins`.
- Add small tests where Polysemy-specific behavior could diverge (e.g., warning emission order, error propagation).

### 3) Property Tests
Focus: ensure the Polysemy conversion does not change existing properties.
- Keep current properties for translation output shape and behavior.
- Add property helpers to run `TranslateM` in pure mode with a deterministic `TranslateConfig`.

### 4) Golden Tests
Focus: regression protection for translation output.
- Reuse existing fixtures; verify output unchanged after Polysemy conversion.
- If any output changes are required, document them in the fixture commit message and in `CHANGELOG.md`.

### 5) Integration Tests
Focus: end-to-end behavior for CLI and script translation.
- Run `cabal test` and verify existing integration tests.
- When adding new Polysemy-driven features, add a minimal fixture to `test/fixtures/integration/`.

## Test Utilities to Add (If Needed)
- A helper to run `TranslateM` with:
  - default config
  - empty state
  - small input ranges
- A helper to extract `TranslateState` + `warnings` without needing to inspect the writer directly.

## Coverage Checklist
- [ ] Error path: unsupported construct triggers `TranslateError`.
- [ ] Warning path: `addWarning` only appears once in output state.
- [ ] Input map used for `withTokenRange` lookups.
- [ ] Range stack is restored after nested translation.
- [ ] Multiple warnings accumulate in order.
- [ ] Config changes in `TranslateConfig` impact translation behavior as before.

## CI / Local Execution
- `cabal test` must pass.
- Optional: `cabal test --enable-coverage` after major refactors.

## Open Questions
- Do we want a dedicated test for interpreter order (Reader/State/Writer/Error/Input) to prevent accidental changes?
- Should we enforce warning order by position to avoid accidental nondeterminism?

