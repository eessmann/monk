# Monk

Monk is a Haskell project that tries to translate Bash scripts into fish.
It started as a fun excuse to learn more about shell parsing, typed ASTs, and
all the weird corners where Bash and fish do not line up cleanly.

Monk is deliberately conservative: it
translates what it understands, emits warnings for the parts that need a human to look again, and can fail fast in `--strict` mode when it would rather stop than try its best.

## What It Does

Monk parses Bash with ShellCheck, hands translation output through a typed fish
DSL boundary, and renders fish source from the existing pretty-printer backend.

Today it handles a lot of ordinary shell code:

- control flow such as `if`, `while`, `for`, and `case`
- functions, arrays, variable assignments, and common special variables
- pipelines, background jobs, and command substitution
- redirections, here-strings, and a chunk of process substitution
- recursive `source` translation for literal source paths

It also has a long tail of best-effort behavior.

## What To Expect

If you run Monk on a script, the happy path is:

1. it produces fish output
2. it tells you where translation got lossy or approximate
3. you review the result like generated migration code, not handwritten code

The current source of truth for exact vs best-effort behavior is
`docs/design/translator-audit.md`.

Constructs that still deserve extra attention include:

- subshell-heavy scripts
- residual `read` edge cases outside the exact helper-backed surface
- `set -e` / `pipefail` interactions in compound shell logic
- non-literal `source`
- option-heavy `trap`, uncatchable trap signals, `shopt`, and `coproc`
- argument-position or broader `>(...)` forms outside the covered Linux redirect-target fixtures

## Quick Start

Build it from source:

```bash
git clone https://github.com/eessmann/monk.git
cd monk
cabal build
```

Translate a script:

```bash
monk script.sh > script.fish
monk script.sh --output script.fish
monk script.sh --strict
monk script.sh --recursive --sources separate
```

Useful flags:

- `--output FILE` writes to a file instead of stdout
- `--strict` turns best-effort warnings into failures where supported
- `--quiet-warnings` suppresses warning output
- `--recursive` follows literal `source` / `.`
- `--sources inline|separate` controls how recursive source translation is emitted

Warnings and notes go to stderr.

## Library Surface

The public modules are intentionally small:

- `Monk.Translation` for parse + translate entry points
- `Monk.Translation.Types` for the stable translation/diagnostics contract
- `Monk.AST` / `Language.Fish.DSL` for the public type-safe Fish construction DSL
- `Language.Fish.DSL.Lower` for explicit DSL-to-backend lowering
- `Monk.AST.Raw` for the explicit raw renderer-backend AST escape hatch
- `Monk.Source` for recursive source-graph helpers
- `Monk.Diagnostics` for warning rendering and confidence summaries
- `Monk` as a thin convenience re-export

`Monk.AST` now exposes smart constructors such as `script`, `stmt`,
`command`, `arg`, `redirect`, `begin`, `pipeline`, `if_`, `while`, `for`,
`switch`, and `function`. The DSL keeps block and pipeline bodies non-empty at
the type level and lowers through the existing pretty-printer backend. Code
that intentionally needs raw constructors should import `Monk.AST.Raw`; this is
a breaking change for callers that previously used `Monk.AST` as the raw
constructor surface.

Successful translations now retain the typed `Script` in `TranslationResult`.
Call `translationStatements` when compatibility code needs the lowered raw
renderer-backend statements.

Example:

```haskell
import Monk.Translation

main :: IO ()
main = do
  result <- translateBashFile defaultConfig "script.sh"
  case result of
    Left err -> print err
    Right translation -> do
      putStrLn (toString (renderTranslation translation))
      print (translationWarnings translation)
```

## Development

The normal local loop is:

```bash
cabal build
cabal test
MONK_INTEGRATION=1 cabal test
hlint .
```

There is also a bake-off runner for comparing Monk and Babelfish:

```bash
cabal run monk-bakeoff -- --compatible --no-benchmark --out-dir /tmp/monk-bakeoff
```

Bake-off prerequisites:

- `babelfish` and `fish` are required
- `hyperfine` is optional and only needed for benchmark runs
- the runner now validates tool paths up front and reports actionable preflight errors or benchmark-skip notes

## Docs

- `docs/design/translator-audit.md`: fidelity matrix and evidence backlog
- `docs/design/translator-todo.md`: active translator backlog
- `docs/design/architecture.md`: module layout and subsystem boundaries
- `docs/migration-guide.md`: manual cleanup patterns after translation
- `docs/babelfish-comparison.md`: current bake-off workflow and comparison notes
