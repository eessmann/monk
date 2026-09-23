# Monk

Monk translates an admitted subset of Bash into Fish under a versioned
execution contract. Unsupported semantics
produce structured diagnostics and a failure in both normal and strict mode.
Named approximations require an explicit opt-in.

The [roadmap](docs/design/translator-todo.md) records verified coverage,
deliberate exclusions and remaining external CI evidence. The
[portable verification report](docs/design/portable-runtime-verification.md) separates
fresh local results from historical evidence and unverified target execution.

## Build and translate

Use the pinned devenv project (GHC 9.14.1 by default, with a
GHC 9.12.2 compatibility lane, Cabal 3.16.1, and the locked Rust nightly). See [dependency and packaging
commands](nix/README.md), including the devenv MCP launch configuration:

```bash
devenv shell -- bash -e <<'SCRIPT'
monk-build
monk-rust-build
runtime="$PWD/target/debug/monk-runtime"
cabal run monk -- script.bash --strict --runtime "$runtime" --output script.fish
"$runtime" --abi 2 launch script.fish
SCRIPT
```

The explicit runtime path works directly from the build tree; building alone
does not install `monk-runtime` on PATH.

The initial profile is Bash 5.3, signed 64-bit arithmetic, UTF-8 source and C
locale. The reference pair is Bash 5.3.9 and Fish 4.6.0; the locked newer Fish
is tested separately. Generated programs use native Fish directly where
equivalence is proved. External commands use a bounded replace-self dispatch
shim for Bash-compatible launch errors. Source output uses a bounded writer to
preserve Bash diagnostics and signal termination on failed writes, including
closed stdout and SIGPIPE. Scalar expressions and control flow stay in Fish.
All standalone output uses the ABI 2 native launcher so initially closed
streams are recorded before Fish starts. Byte operations use bounded helpers; supervised programs keep control flow in a private Fish
evaluator while the native owner manages user processes and descriptors. Install both executables with
`cabal install exe:monk` and `cargo install --path runtime --locked`, or select a
provider with `--runtime FILE`.
`--managed --output script.fish` captures that provider in an immutable bundle.
The captured `bin/monk-runtime` can launch the bundle entry without an installed
provider. Sourceable output retains its declared Fish caller interface.
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

Standalone coverage includes foreground pipelines and background jobs, `$!`
and `wait`, ordered file and descriptor redirections, here-documents and
here-strings, admitted `read` flags, dense indexed arrays, composed word
expansion, owned process substitution, compiled EXIT/ERR traps, and finitely
proved `eval` and immutable sources. These features have explicit context and
operand restrictions. Session effects do not extend the sourceable contract.

Admission depends on context, not just syntax. Sparse/associative arrays,
arbitrary runtime-generated `eval`, unknown dynamic dispatch, recursion,
mutable computed sources, source cycles, interactive job control and arbitrary
signal callbacks remain rejected. A rejection test
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
devenv shell -- monk-build
devenv shell -- monk-rust-build
devenv shell -- monk-rust-test
devenv shell -- monk-rust-quality
devenv shell -- monk-integration
devenv shell -- monk-quality
devenv shell -- monk-docs
devenv shell -- monk-sdist
devenv shell -- monk-benchmark
devenv shell -- monk-package
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

The frozen 95-fixture bake-off compares original Monk, the previous baseline,
the candidate and pinned Babelfish independently against Bash. Separate
filesystem, caller-state and process-lifetime cohorts supplement its unchanged
denominator. Rejections, mismatches and unavailable executions remain distinct.

Release definitions cover x86_64 Linux and arm64 Linux with static musl, plus
Apple Silicon macOS with static Haskell/third-party dependencies and Apple
system libraries. Native Linux execution remains unverified in this work: the
user requested local source and local testing only.
