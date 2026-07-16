# Monk

Monk is a Haskell project that tries to translate Bash scripts into fish.
It started as a fun excuse to learn more about shell parsing, typed ASTs, and
all the weird corners where Bash and fish do not line up cleanly.

Monk is deliberately conservative: it
translates what it understands, emits warnings for the parts that need a human to look again, and can fail fast in `--strict` mode when it would rather stop than try its best.

## What It Does

Monk parses Bash with ShellCheck, hands translation output through a typed fish
IR, applies typed source/output passes, and renders fish source.

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

## How It Works

Monk works like a small compiler:

```text
Bash source
  -> ShellCheck parser and Bash AST
  -> Monk translator
  -> structural fish DSL
  -> typed source graph and output bundle
  -> pretty-printed fish source
```

1. `Language.Bash.Parser` asks ShellCheck to parse Bash and retain source
   positions and parse diagnostics.
2. `Language.Fish.Translator` recursively translates ShellCheck tokens.
   Focused modules handle control flow, commands, variables, arithmetic,
   redirections, parameter expansion, process substitution, and other semantic
   areas.
3. The translator produces a typed `Language.Fish.DSL.Script`. Its types keep
   blocks and pipelines non-empty, distinguish expression types, and restrict
   pipeline stages to status-returning commands.
4. Typed simplification, renaming, source rewriting, inlining, and output
   planning operate on that same structural representation.
5. The private renderer boundary produces final fish source while the translation
   result retains structured diagnostics for the caller.

The translator also tracks context such as function scope, local variables,
command substitution, `errexit`, and `pipefail`. When fish has no direct
equivalent for required Bash behavior, Monk can emit a generated helper
preamble for supported cases such as background-job tracking, exact `read`
behavior, process substitution, and `pipefail` handling.

## Diagnostics And Strict Mode

Diagnostics are structured values with a stable code, phase, severity, message,
optional source range, and `ReviewRisk` (`Clean`, `Review`, or `Unsafe`). The CLI
prints them to stderr together with deduplicated runtime requirements. Numeric
confidence scores are not part of the 0.4 API.

Default mode keeps translating when a best-effort result is available.
`--strict` instead turns unsupported constructs into translation failures. This
makes normal mode useful for migrations and strict mode useful when approximate
output is unacceptable.

## Recursive Sources

With `--recursive`, Monk discovers literal `source` and `.` references and
builds a graph of the scripts it can resolve. `--sources inline` combines
translated files into one output, while `--sources separate` emits individual
`.fish` files and rewrites source paths to their translated targets. Separate
recursive bundles extract live generated helpers into at most one
`_monk_runtime.fish`, sourced through quoted relative paths.

Dynamic source expressions cannot be resolved statically and remain
warning-driven manual-review cases.

## Quick Start

Build it from source:

```bash
git clone https://github.com/eessmann/monk.git
cd monk
cabal build
```

Generated scripts target Fish 4.6 or newer. Python 3 is declared as an explicit
runtime requirement only when an exact hard-case fallback needs it.

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
- `Monk.Source` for recursive source-graph helpers
- `Monk.Output` for typed stdout, combined, and separate bundle planning
- `Monk.Diagnostics` for diagnostics, review-risk, and requirement rendering
- `Monk` as a thin convenience re-export

`Monk.AST` now exposes smart constructors such as `script`, `stmt`,
`command`, `arg`, `redirect`, `begin`, `pipeline`, `if_`, `while`, `for`,
`switch`, and `function`. The DSL keeps block and pipeline bodies non-empty at
the type level. Raw constructors and lowering internals are no longer public;
callers that depended on them must migrate to the structural DSL in 0.4.

Successful translations retain the structural `Script`, ordered diagnostics,
and declared requirements in `TranslationResult`.

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
      print (translationDiagnostics translation)
      print (translationRuntimeRequirements translation)
```

## Development

The normal local loop is:

```bash
cabal build
cabal test
MONK_INTEGRATION=1 cabal test
hlint .
```

### Repository Map

- `app/`: the `monk` CLI entry point
- `src/Monk/`: public translation, diagnostics, and source-graph APIs
- `src/Language/Bash/`: the ShellCheck parser boundary
- `src/Language/Fish/DSL*`: the structural fish IR and safe construction API
- `src/Language/Fish/Translator/`: translation orchestration and semantic
  subsystems
- `src/Language/Fish/Pretty/`: the private structural Fish renderer
- `test/`: unit, property, golden, integration, and real-world tests
- `scripts/Bakeoff/`: the Monk-versus-Babelfish comparison harness
- `docs/design/`: architecture, fidelity evidence, and active translator
  design notes

### Testing Strategy

The test suite checks both generated structure and runtime behavior:

- unit tests cover focused translator, DSL, renderer, diagnostics, source, and
  harness behavior
- property tests exercise rendering and translation invariants
- golden tests compare generated fish text with checked-in expected output
- integration and real-world tests run Bash and translated fish, then compare
  exit status, stdout, stderr, and environment changes
- the bake-off runner compares Monk with Babelfish, benchmarks both
  translators, and reports Bash-versus-generated-Fish runtime medians

Run `cabal test` for the normal suite. Set `MONK_INTEGRATION=1` to enable tests
that require Bash and fish execution.

### Bake-Off

The bake-off runner compares Monk and Babelfish:

```bash
cabal run monk-bakeoff -- --compatible --no-benchmark --out-dir /tmp/monk-bakeoff
```

Bake-off prerequisites:

- `babelfish` and `fish` are required
- `hyperfine` is optional and only needed for benchmark runs
- the runner now validates tool paths up front and reports actionable preflight errors or benchmark-skip notes
- runtime benchmark workers replay fixture arguments, stdin, and execution mode
  against the original Bash and Monk-generated Fish scripts

## Docs

- [`docs/design/translator-audit.md`](docs/design/translator-audit.md): fidelity
  matrix and evidence backlog
- [`docs/design/translator-todo.md`](docs/design/translator-todo.md): active
  translator backlog
- [`docs/design/architecture.md`](docs/design/architecture.md): module layout
  and subsystem boundaries
- [`docs/design/shellcheck-syntax-inventory.md`](docs/design/shellcheck-syntax-inventory.md):
  explicit parser-node support and scope decisions
- [`docs/migration-guide.md`](docs/migration-guide.md): manual cleanup patterns
  after translation
- [`docs/babelfish-comparison.md`](docs/babelfish-comparison.md): current
  bake-off workflow and comparison notes
