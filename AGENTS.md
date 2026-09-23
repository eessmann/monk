# Repository Guidelines

## Project Structure & Module Organization

Monk is a Haskell translator from Bash to Fish.

- `src/Monk/`: thin public translation, source discovery, and output orchestration APIs.
- `foundation-src/`: shared source locations, contracts, diagnostics, integer specification, digest, and typed ABI inventory.
- `host-src/`: byte-preserving subprocess ownership, snapshots, and runtime capture/validation.
- `compiler-src/Language/Bash/`: pure ShellCheck integration and semantic normalization; `compiler-src/Language/Fish/`: structural DSL, materialization, and private renderer; `compiler-src/Monk/Compiler/`: shared indexed ownership boundaries.
- `publication-src/`: validated layouts, manifests, locking, durability, publication, and recovery.
- `app/Main.hs`: command-line interface; `runtime/`: Rust 2024 execution runtime.
- `protocol/abi2.tsv`: shared ABI metadata, generated into Haskell and Rust by `scripts/generate-abi-metadata.sh`.
- `test/`: unit, property, golden, integration, and real-world suites; `test/fixtures/` stores inputs and expectations; `test-support/` owns shared fixture metadata and execution observations.
- `benchmark/`: Criterion benchmarks; `tooling/src/`: verification, packaging, evidence, and Bakeoff domains.
- `docs/design/`: architecture, semantic audit, roadmap, and public API migration guidance.

## Build, Test, and Development Commands

Use the pinned devenv Rust nightly, GHC 9.12.2 or 9.14.1, and Cabal; CI uses Cabal 3.16.1. Runtime comparisons require Bash 5.3 and Fish 4.6 or newer.

- `devenv shell -- monk-build`: build the Cargo runtime and Haskell libraries/executables.
- `devenv shell -- monk-test`: run the normal Cargo and Cabal suites.
- `devenv shell -- monk-integration`: run Bash/Fish execution comparisons against the Cargo runtime.
- `devenv shell -- monk-quality`: check ABI generation, HLint, Ormolu, Cabal metadata, and tooling language policy.
- `devenv shell -- monk-sdist`: build and smoke-test the unpacked source archive.
- `cabal run monk -- script.sh --strict`: translate a script with strict diagnostics.
- `cabal bench monk-benchmark`: run Criterion benchmarks.
- `cabal haddock all`: generate API documentation.

Use `devenv shell -- monk-quality` for Haskell style checks; it filters generated build outputs and removed tracked files.

## Coding Style & Naming Conventions

Use Ormolu formatting and two-space indentation for Haskell, and rustfmt for Rust. Follow GHC2024 conventions and the project's `Relude` Prelude. Use `UpperCamelCase` for modules/types and `lowerCamelCase` for functions/values. Provide explicit export lists, top-level signatures, and deriving strategies. Keep builds warning-free: Cabal enables `-Wall` and `-Werror`. Register new Haskell modules in `monk.cabal`; keep Cargo.lock pinned.

## Testing Guidelines

Tests use Tasty, HUnit, and QuickCheck. Place focused cases in `test/Unit/`, properties in `test/Property/`, and register new groups in `test/Spec.hs`. Use descriptive behavioral test names and hyphenated fixture stems such as `read-prompt.bash`/`read-prompt.fish`.

For semantic changes, add regression coverage that compares Bash and generated Fish behavior, including exit status and output. Review golden changes manually. Report platform skips and missing prerequisites explicitly; they remain evidence gaps. No numeric coverage threshold is configured.

## Commit & Pull Request Guidelines

Follow the history's short, imperative subjects, such as `Correct translator roadmap evidence`. Keep commits focused. PR descriptions should explain the problem, behavior change, relevant issues, validation commands/results, and remaining limitations. Update semantic claims in `docs/design/translator-audit.md` and acceptance evidence in `docs/design/translator-todo.md` when affected.

## Architecture Boundaries

Preserve the structural Fish DSL and private renderer boundary. Keep output planning separate from filesystem writes. Consult `docs/design/architecture.md` before changing public APIs or translation passes.
