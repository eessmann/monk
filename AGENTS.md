# Repository Guidelines

## Project Structure & Module Organization
- `src/`: Core library. Key modules: `Monk` (public API), `Language/Fish/AST.hs` (Fish AST), `Language/Fish/Translator.hs` (Bash→Fish), `Language/Fish/Pretty.hs` (rendering).
- `app/`: CLI entrypoint (`Main.hs`) building the `monk` executable.
- `test/`: Test suite entry (`Spec.hs`). Add additional specs here and wire them via `Spec.hs`.
- `benchmark/`: Optional micro-bench entry (`Main.hs`).
- `dist-newstyle/`, `.hie/`: Build artifacts; do not commit.

## Build, Test, and Development Commands
- `cabal update`: Refresh package index.
- `cabal build all`: Build library, exe, tests, and benchmarks.
- `cabal run monk`: Run the CLI (reads `test.sh` by default in `app/Main.hs`).
- `cabal test all`: Run the test suite.
- `cabal repl monk`: Open a REPL for iterative development.
- `hlint .`: Lint the code (also run in CI via `.travis.yml`).

## Coding Style & Naming Conventions
- Haskell with Relude; warnings enabled (`-Wall`, extra `-W…` in `monk.cabal`). Keep code warning-free.
- Module names mirror paths (e.g., `Language.Fish.Translator` in `src/Language/Fish/Translator.hs`).
- Prefer explicit exports and total functions. Use descriptive identifiers; avoid one-letter names outside short scopes.
- Formatting: follow conventional Haskell style (aligned `let/in`, consistent imports). If in doubt, match existing modules.

## Testing Guidelines
- Framework: simple exitcode test suite (`test/Spec.hs`). Organize helper modules under `test/` and import into `Spec.hs`.
- Naming: test files use `*.hs` under `test/`; group by feature (e.g., `Fish/TranslatorSpec.hs`).
- Run tests with `cabal test`; add focused runners in `Spec.hs` as tests grow.

## Commit & Pull Request Guidelines
- Commits: present-tense, concise summaries (e.g., `Add Bash parser`, `Extend translation`). Group related changes.
- PRs: include purpose, summary of changes, and links to issues. Add before/after examples for translator or pretty-printer changes. Note any behavior differences vs. Bash.

## Architecture Overview
- Pipeline: Bash parsed via ShellCheck AST → translated to Fish AST → rendered to text.
- Edit flow: update `Language/Fish/AST` types first, then `Translator`, then `Pretty`; keep them in sync.
