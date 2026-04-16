# Monk Architecture (2026-04-16)

This document describes the current module boundaries after the whole-repo refactor pass that separated the public library surface, shared harness support, and the bake-off executable.

## Public Library Surface

The public API is intentionally explicit:

- `Monk.Translation`
  - Parse, translate, and render entry points.
  - Owns `TranslationResult`, `TranslationFailure`, `Translation`, and translation-state access.
- `Monk.AST`
  - Public Fish AST surface for downstream code that needs to inspect or post-process generated Fish.
- `Monk.Source`
  - Recursive source-graph construction and source-path rewriting helpers used by the CLI and tests.
- `Monk.Diagnostics`
  - Warning rendering, note rendering, and confidence scoring.
- `Monk`
  - Thin convenience facade that re-exports `Monk.Translation` and `Monk.Diagnostics`.

`Monk` is no longer the catch-all public surface for the Fish AST or recursive source logic.

## Translator Internals

The translator is organized around a typed Fish IR and small focused subsystems:

- `Language.Fish.AST`
  - Re-export surface over the AST internals.
- `Language.Fish.AST.Common`
  - Shared leaf enums and source-position types.
- `Language.Fish.AST.Types`
  - Recursive statement, expression, job, and redirection structures.
- `Language.Fish.Translator`
  - Top-level statement dispatch and orchestration.
- `Language.Fish.Translator.Commands.Read`
  - Facade over read lowering.
- `Language.Fish.Translator.Commands.Read.Parse`
  - Read flag parsing and exact-path selection.
- `Language.Fish.Translator.Commands.Read.Types`
  - Shared read lowering data types.
- `Language.Fish.Translator.Commands.Read.Runtime`
  - Generated helper text and capture/runtime builders.
- `Language.Fish.Translator.Commands.Read.Exact`
  - Exact helper-backed read lowering.
- `Language.Fish.Translator.Variables.ParamExpansion`
  - Facade over parameter-expansion lowering.
- `Language.Fish.Translator.Variables.ParamExpansion.Parse`
  - Parsing and normalized modifier/operator classification.
- `Language.Fish.Translator.Variables.ParamExpansion.Types`
  - Parameter-expansion IR.
- `Language.Fish.Translator.Variables.ParamExpansion.Render`
  - Render and hoist-aware lowering.

The structural target for this pass was to eliminate mixed-responsibility 700-1000 line translator modules. The remaining top-level translator modules are orchestration modules rather than large monoliths carrying unrelated logic.

## Source Graph And CLI

Recursive source handling is now a library service rather than ad hoc CLI logic:

- `Monk.Source.translateSourceGraph`
  - Parses and translates a root script plus recursively discovered literal `source` / `.` edges.
- `Monk.Source.rewriteSources`
  - Rewrites recursive source targets for separate-output mode.
- `app/Main.hs`
  - Thin CLI that parses arguments, calls `Monk.Source`, and writes inline or separate output.

Literal source resolution now tries the Bash working-directory-relative path first and then falls back to the parent source file directory. Non-literal source paths remain warning-driven/manual-review territory.

## Shared Harness Support

Tests and bake-off now share one internal support layer:

- Private Cabal library: `monk-harness-support`
- Modules:
  - `Monk.Internal.Fixture`
  - `Monk.Internal.Shell`

Responsibilities:

- fixture sidecar loading (`.args`, `.stdin`, `.mode`, `.platforms`, `.prereqs`, `.recursive`)
- shell execution helpers
- environment capture and diffing

This layer uses typed `Path` values internally and is consumed by both `test/` and the bake-off code under `scripts/`.

## Bake-off Architecture

The bake-off is now a separate executable and private internal library:

- Private Cabal library: `monk-bakeoff-lib`
- Executable entrypoint: `scripts/app/Main.hs`
- Modules:
  - `Bakeoff.Selection`
  - `Bakeoff.Artifacts`
  - `Bakeoff.Process`
  - `Bakeoff.Report`
  - `Bakeoff.Benchmark`
  - `Bakeoff.Tools`
  - `Bakeoff.Runner`
  - `Bakeoff.Types`

Responsibilities are split as follows:

- `Bakeoff.Selection`
  - Fixture discovery, selector resolution, metadata-based skipping, and artifact-path derivation.
- `Bakeoff.Artifacts`
  - Output-directory preparation and per-fixture artifact paths.
- `Bakeoff.Process`
  - External process execution, JSON IO, stderr normalization, and diff artifact emission.
- `Bakeoff.Benchmark`
  - Hyperfine plan construction and benchmark execution/loading.
- `Bakeoff.Tools`
  - Tool resolution, version capture, and git metadata.
- `Bakeoff.Report`
  - Markdown summary rendering and hyperfine summary loading.
- `Bakeoff.Runner`
  - Shake orchestration over the bake-off pipeline.

This keeps `shake`, `aeson`, and other bake-off-only dependencies out of the main `monk` library and CLI targets.

## Verification Gates

The refactor is considered healthy when all of the following hold:

- `cabal build all`
- `MONK_INTEGRATION=1 cabal test`
- `cabal run monk-bakeoff -- --compatible --no-benchmark ...`
- one full local `monk-bakeoff` run with metadata-based skips respected

As of 2026-04-16, this architecture pass satisfies those gates on the current machine.
