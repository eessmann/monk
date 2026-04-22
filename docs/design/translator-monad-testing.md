# Translator Monad Testing

## Purpose

Validate that the translator-monad migration preserves translation behavior while
removing the `polysemy` dependency chain that blocked GHC 9.14.

## Coverage Focus

The translator-monad unit coverage should continue to exercise:

- strict-mode error propagation
- warning ordering and stable warning metadata
- helper registration deduplication
- function-scope and command-substitution scope restoration
- token-range attribution through `withTokenRange`
- nested range-stack restoration after inner warnings

The focused unit tests in `Unit.TranslatorMonad` cover the migration-sensitive
runner behaviors directly, while the existing translation, golden, integration,
property, and real-world tests remain the regression suite for user-visible
output.

## Compiler Verification

Run the full build and test suite on both supported compilers:

- `ghcup run --ghc 9.12.2 --cabal 3.16.1.0 -- cabal build all`
- `ghcup run --ghc 9.12.2 --cabal 3.16.1.0 -- cabal test all`
- `ghcup run --ghc 9.14.1 --cabal 3.16.1.0 -- cabal build all`
- `ghcup run --ghc 9.14.1 --cabal 3.16.1.0 -- cabal test all`

These commands are the source of truth for the migration because the original
failure mode was compiler-version compatibility, not translation semantics.
