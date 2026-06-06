# Monk Architecture (2026-04-16)

This document describes the current module boundaries after the whole-repo refactor pass that separated the public library surface, shared harness support, and the bake-off executable.

## Public Library Surface

The public API is intentionally explicit:

- `Monk.Translation`
  - Parse, translate, and render entry points.
  - Owns `TranslationResult`, `TranslationFailure`, `Translation`, and translation-state access. Successful results retain the typed DSL `Script`; `translationStatements` is the explicit compatibility lowering helper for raw backend consumers.
- `Monk.AST`
  - Convenience re-export of the public type-safe Fish construction DSL for downstream code that needs to build generated Fish.
- `Language.Fish.DSL`
  - Canonical public construction API over typed expressions, renderable arguments, command roles, non-empty blocks, stages, job conjunctions, control forms, and scripts.
- `Language.Fish.DSL.Lower`
  - Explicit lowering API from DSL values into the raw AST used by the existing pretty-printer backend.
- `Monk.AST.Raw`
  - Explicit raw Fish AST escape hatch for backend/rendering code and advanced consumers.
- `Monk.Source`
  - Recursive source-graph construction and source-path rewriting helpers used by the CLI and tests.
- `Monk.Diagnostics`
  - Warning rendering, note rendering, and confidence scoring.
- `Monk`
  - Thin convenience facade that re-exports `Monk.Translation` and `Monk.Diagnostics`.

`Monk` is no longer the catch-all public surface for the Fish AST or recursive source logic.

## Translator Internals

The translator is organized around a typed Fish DSL handoff and small focused subsystems:
- `Language.Fish.AST`
  - Raw renderer-backend AST surface used internally and re-exported explicitly as `Monk.AST.Raw`.
- `Language.Fish.AST.Common`
  - Shared leaf enums and source-position types.
- `Language.Fish.AST.Types`
  - Recursive statement, expression, job, and redirection structures.
- `Language.Fish.Translator`
  - Top-level statement dispatch and orchestration; the public translation handoff emits a DSL `Script` before `Monk.Translation` lowers it for rendering.
- `Language.Fish.Translator.Syntax`
  - Internal translator-only boundary for raw-shaped construction while remaining translator modules are migrated onto typed DSL helpers. Direct `Language.Fish.AST`, `Language.Fish.DSL.Internal`, and `Language.Fish.DSL.Lower` imports are forbidden in translator modules outside this boundary.
- `Language.Fish.Translator.Construction`
  - The single internal lowering boundary for translator-authored DSL values. Helper/runtime code can use public `Language.Fish.DSL` constructors and lower through this module without importing DSL internals directly.
- `Language.Fish.Translator.Types`
  - Narrow raw-type facade used by low-level helpers so fewer translator modules depend directly on the broader syntax bridge.
- `Language.Fish.Translator.Redirections`
  - Shared redirection token planning API that returns typed DSL `Arg` values for normal command and command-substitution lowering.
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
  - `Bakeoff.Execution`
  - `Bakeoff.Execution.Translation`
  - `Bakeoff.Execution.Runtime`
  - `Bakeoff.Execution.Diff`
  - `Bakeoff.Execution.Shared`
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
- `Bakeoff.Execution.*`
  - Shake orchestration split into translation, runtime, diff, and shared execution helpers.
- `Bakeoff.Tools`
  - Tool preflight, version capture, benchmark warnings, and git metadata.
- `Bakeoff.Report`
  - Markdown summary rendering and hyperfine summary loading.
- `Bakeoff.Runner`
  - Top-level bake-off setup, preflight messaging, and summary generation.

This keeps `shake`, `aeson`, and other bake-off-only dependencies out of the main `monk` library and CLI targets.

## Verification Gates

The refactor is considered healthy when all of the following hold:

- `cabal build all`
- `MONK_INTEGRATION=1 cabal test`
- `cabal run monk-bakeoff -- --compatible --no-benchmark ...`
- one full local `monk-bakeoff` run with metadata-based skips respected

As of 2026-04-20, this architecture pass satisfies the build/test/lint gates on the current machine; bake-off tool preflight is also verified for invalid explicit paths.
