# Repository Guidelines

## Project Structure & Module Organization
Source code lives in `src/` as the `Monk` library and `Language.*` modules for parsing and translation. The CLI entry point is `app/Main.hs`, and the benchmark entry point is `benchmark/Main.hs`. Tests are under `test/` (notably `test/Spec.hs` and `test/Gen.hs`), with any extra fixtures stored alongside. Top-level project metadata and build configuration are in `monk.cabal`, `cabal.project.local`, `CHANGELOG.md`, and `LICENSE`.

## Build, Test, and Development Commands
- `cabal build` builds the library and executable.
- `cabal run monk -- path/to/script.sh` runs the CLI.
- `cabal test` runs the Tasty test suite.
- `cabal test --enable-coverage` runs tests with coverage.
- `cabal bench` runs the benchmark target.
- `hlint .` runs lint checks across the codebase.

## Coding Style & Naming Conventions
Use Ormolu formatting (standard 2-space indentation) and keep code aligned with the GHC2024 defaults in `monk.cabal`. Address `hlint` suggestions unless they reduce clarity. Follow existing naming patterns: modules in `PascalCase` file paths (for example, `Language/Fish/Translator.hs`), types and constructors in `UpperCamelCase`, and values in `lowerCamelCase`.

## Testing Guidelines
Tests use `tasty`, `tasty-hunit`, and `tasty-quickcheck` with `QuickCheck`. Unit tests live in `test/Unit/*.hs`, property tests in `test/Property/*.hs`, and fixtures under `test/fixtures/`. When extending translation behavior, add a focused unit test and a property-based test when practical.

## Commit & Pull Request Guidelines
Commit subjects in history are short, sentence-style summaries (for example, “AST Rework”). Keep subjects concise and descriptive; avoid extra prefixes unless they add clarity. For pull requests, describe behavioral changes, link related issues, and include tests or a short rationale if tests are not added. Provide sample input/output when changing translation logic.
