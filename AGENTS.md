# Repository Guidelines

## Project Structure & Module Organization
- `src/`: Core library. Key modules: `Monk` (public API), `Language/Fish/AST.hs` (Fish AST), `Language/Fish/Translator.hs` (Bash→Fish), `Language/Fish/Pretty.hs` (rendering).
- `app/`: CLI entrypoint (`Main.hs`) for the `monk` executable.
- `test/`: Tests (`Spec.hs` driver, helpers under `test/`).
- `benchmark/`: Optional micro-bench (`Main.hs`).
- Generated artifacts: `dist-newstyle/`, `.hie/` — do not commit.

## Build, Test, and Development Commands
- `cabal update`: Refresh package index.
- `cabal build all`: Build library, executable, tests, and benchmarks.
- `cabal run monk`: Run the CLI (defaults to reading `test.sh` in `app/Main.hs`).
- `cabal test all`: Run the full test suite.
- `cabal repl monk`: REPL for iterative development.
- `hlint .`: Lint the code (also run in CI via `.travis.yml`).

## Coding Style & Naming Conventions
- Language: Haskell with Relude. Enable warnings (`-Wall` + extras in `monk.cabal`) and keep code warning-free.
- Modules mirror paths (e.g., `Language.Fish.Translator` → `src/Language/Fish/Translator.hs`).
- Prefer explicit export lists, total functions, and descriptive names; avoid single-letter identifiers beyond tiny scopes.
- Formatting: idiomatic Haskell (aligned `let/in`, grouped imports). Match existing style when unsure.

## Testing Guidelines
- Frameworks: `tasty`, `tasty-hunit`, `tasty-quickcheck`.
- Organization: group tests by feature under `test/`; wire them through `Spec.hs`.
- Run: `cabal test`. Keep property tests fast and deterministic.

## Commit & Pull Request Guidelines
- Commits: present tense, concise (e.g., `Add Bash parser`, `Extend Fish translator`). Group related changes.
- PRs: include purpose, summary, linked issues, and before/after examples for translator/pretty changes. Note behavior differences vs. Bash.

## Architecture Overview & Workflow
- Pipeline: ShellCheck AST → Fish AST (`Language.Fish.AST`) → text via pretty-printer (`Language.Fish.Pretty`).
- Edit flow: update AST types first, then `Translator`, then `Pretty`. Keep all three in sync.

## Security & Configuration Tips
- Avoid committing build outputs or local overrides (`dist-newstyle/`, `.hie/`, `cabal.project.local`).
- Prefer pure builds; avoid network I/O at runtime. Keep translations side-effect-free.
