# Translator Monad Migration

## Summary

Monk now uses a conventional `mtl`-style translator monad instead of `polysemy`.
The immediate driver was GHC 9.14 support: `polysemy-plugin` depends on
`ghc-tcplugins-extra`, which currently excludes `ghc >= 9.13`, so the old design
failed at dependency solving before compilation started.

## Runtime Shape

The translator runner is now:

```haskell
ReaderT TranslateEnv (StateT TranslateState (Either TranslateError))
```

`TranslateEnv` contains:

- `TranslateConfig`
- token-range positions used by `withTokenRange`

`TranslateState` contains only mutable translation state:

- accumulated warnings
- translation context and local-variable tracking
- range stack for nested warning attribution
- `errexit` / `pipefail` flags
- helper registration and warning deduplication
- preamble statements

This keeps configuration and parse metadata read-only while preserving the
previous translation behavior and public runner surface.

## Compatibility Goals

- Keep `runTranslate`, `runTranslateWithPositions`, `evalTranslate`, and
  `evalTranslateWithPositions` stable.
- Preserve strict-mode failure behavior and warning ordering.
- Keep uncaught translation errors state-discarding, matching the old runner.
- Remove `polysemy`, `polysemy-plugin`, and the `Polysemy.Plugin` GHC plugin.

## Verification

The migration is considered complete when all of the following hold:

- `ghcup run --ghc 9.12.2 --cabal 3.16.1.0 -- cabal build all`
- `ghcup run --ghc 9.12.2 --cabal 3.16.1.0 -- cabal test all`
- `ghcup run --ghc 9.14.1 --cabal 3.16.1.0 -- cabal build all`
- `ghcup run --ghc 9.14.1 --cabal 3.16.1.0 -- cabal test all`

The GitHub Actions matrix mirrors the local compiler targets so regressions stay
visible on both supported GHC versions.
