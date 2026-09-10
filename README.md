# Monk

Monk translates an admitted subset of Bash into Fish under a versioned
execution contract. Unsupported semantics
produce structured diagnostics and a failure in both normal and strict mode.
Named approximations require an explicit opt-in.

The [roadmap](docs/design/translator-todo.md) records verified coverage,
deliberate exclusions and remaining external CI evidence. The
[verification report](docs/design/translator-verification.md) gives reproducible
local candidate results.

## Build and translate

Use GHC 9.12.2 or 9.14.1 and Cabal 3.16.1:

```bash
cabal build all -fdevelopment
cabal run monk -- script.bash --strict --output script.fish
fish --no-config script.fish
```

The initial profile is Bash 5.3, signed 64-bit arithmetic, UTF-8 source and C
locale. Runtime evidence uses Bash 5.3.9 and Fish 4.6.0 on Linux x86-64. Generated
operations use native Fish and declare a compatible compiled `monk-runtime`
when bounded byte or integer operations need it. Install both executables with
`cabal install exe:monk exe:monk-runtime`, or select a provider with `--runtime FILE`.
`--managed --output script.fish` captures that provider in an immutable bundle.
Generated support requires no Python and never evaluates Bash expression strings.
See the [execution contract](docs/design/execution-profile.md)
for startup conditions and caller obligations.

```bash
# Combined output on stdout:
cabal run monk -- script.bash --strict

# Literal dependencies, using the declared execution cwd and PATH:
cabal run monk -- script.bash --strict --recursive --sources inline

# Explicitly permit the readonly enforcement approximation:
cabal run monk -- script.bash --allow-approximation readonly-unchecked

# Output intended to be sourced by a declared caller:
cabal run monk -- script.bash --strict --entry sourceable \
  --caller-contract caller.json --output script.fish
```

`--target-profile bash-5.3-fish-4.6` names the initial profile. `--strict` cannot be
combined with `--allow-approximation`. Warnings, notes and runtime requirements
go to stderr; `--quiet-warnings` suppresses them, not translation failures.

## Supported semantics

The implementation supports exact words, integer arithmetic, control
flow, definite function calls and literal sources. Quotation, empty arguments,
field splitting, lazy expansion effects and invocation order are part of the
contract. Child execution has an explicit isolation plan. Sourceable output has
an owned return/status/argv boundary and declared scalar and function effects.

Admission depends on context, not just syntax. Arrays, arbitrary `eval`, unknown
dynamic dispatch, recursion, computed sources, source cycles, callbacks and
unsupported binding or option states require diagnostics. A rejection test
establishes an exclusion; it does not establish implemented functionality.
Consult the [semantic audit](docs/design/translator-audit.md) and
[constructor policy](docs/design/shellcheck-syntax-inventory.md) for exact
boundaries. Old best-effort support claims do not apply to this translator.

## Library and architecture

```text
ShellCheck syntax and immutable source input
  -> private semantic plan
  -> admitted materialization plan, including helpers and execution boundaries
  -> structural Fish DSL
  -> rendering and publication
```

`Monk.Translation` owns parse/translate entry points. `Monk.Source` discovers
literal dependencies through the same semantic analysis. `Monk.Output` plans
output separately from filesystem writes. Translation results, source graphs
and output bundles are opaque, with inspection functions. General
`Language.Fish.DSL` / `Monk.AST` construction remains available without allowing
arbitrary constructed scripts to become certified translations.

```haskell
import Monk.Translation

translateFile = do
  result <- translateBashFile strictConfig "script.bash"
  case result of
    Left failure -> print failure
    Right translation -> do
      print (renderTranslation translation)
      print (translationDiagnostics translation)
      print (translationRuntimeRequirements translation)
```

Sourceable callers declare binding access, initial export attributes, lookup
and ambient effects in a versioned JSON contract. Runtime guards check
observable preconditions; equivalence of imported functions and absence of
relevant callbacks remain caller obligations. See the
[migration guide](docs/migration-guide.md) for the deliberate API/CLI changes.

Managed publication stages immutable generations on the destination filesystem
and replaces one entry loader atomically. Child references remain pinned to a
generation and prior generations remain available. This guarantee concerns
publication and reader consistency; executing scripts can still have their
declared effects. Use the output publisher, rather than manually writing the
files returned by inspection accessors.

## Development and evidence

```bash
cabal build all -fdevelopment
MONK_INTEGRATION=1 cabal test all -fdevelopment
hlint .
git ls-files '*.hs' -z | xargs -0 ormolu --mode check
cabal haddock all -fdevelopment
cabal check
```

The development flag retains warnings as errors without making release package
metadata reject unconditional `-Werror`. Tests compare output, status, argument
boundaries, filesystem effects and declared caller updates. Generated
compositions include shrinking and classify zero-diagnostic mismatches
separately. Compile-fail checks include positive controls. Publication tests
exercise failure recovery and concurrent readers and publishers. Skipped
runtime and platform checks remain explicit evidence gaps.

- [Architecture](docs/design/architecture.md)
- [Roadmap and acceptance evidence](docs/design/translator-todo.md)
- [Legacy test migration](docs/design/legacy-test-migration.md)
- [Bake-off workflow](docs/babelfish-comparison.md)

The current bake-off compares each generated script directly with Bash. It
records translation rejections separately and measures standalone stdout,
stderr and status; caller-state and filesystem equivalence need other tests.
