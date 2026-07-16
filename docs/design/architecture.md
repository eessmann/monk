# Monk 0.4 Architecture

Monk is organized as a typed compiler pipeline. ShellCheck's Bash syntax tree
is an input format, not Monk's output representation, and raw Fish renderer
constructors are private implementation details.

```text
Bash text
  -> ShellCheck parse result
  -> structural Fish DSL + diagnostics + requirements
  -> typed source graph / passes / output bundle
  -> Fish renderer
  -> text or files
```

## Public Surface

- `Monk.Translation` parses and translates Bash. `TranslationResult` contains a
  structural `Script`, ordered `Diagnostic` values, and deduplicated
  `RuntimeRequirement` values. `TranslationFailure` contains a nonempty
  diagnostic collection.
- `Monk.Translation.Types` owns stable diagnostic codes, phases, severities,
  review risk, runtime programs, reasons, and source locations.
- `Monk.AST` and `Language.Fish.DSL` expose structural Fish types plus smart
  constructors and `renderScript`. Constructors preserve expression
  cardinality, command roles, and nonempty block/pipeline invariants.
- `Monk.Source` owns recursive literal-source discovery, typed per-source
  translations, dependency mappings, rewriting, and inlining.
- `Monk.Output` plans stdout/combined output and separate recursive bundles.
  `OutputBundle` separates generated user files from an optional shared runtime
  file; rendering remains separate from filesystem writing.
- `Monk.Diagnostics` renders stable diagnostics, review-risk summaries, and
  declared runtime requirements.

There is no public `Monk.AST.Raw`, public lowering module, translator state,
callback warning channel, or raw inline result in 0.4.

## Structural Fish IR

`Language.Fish.DSL.Types` defines the independent leaf types.
`Language.Fish.DSL.Internal` owns the recursive structural representation used
by translation and typed passes. `Language.Fish.DSL` exposes safe construction
views and smart constructors; internal constructors remain private to the
library.

The renderer consumes the same structural representation through a private
boundary. Compatibility modules under `Language.Fish.AST.*` are private and do
not form a second public AST.

## Translation

`Language.Fish.Translator` dispatches ShellCheck tokens and coordinates focused
subsystems under `Language.Fish.Translator.*`:

- commands, conditions, compound status plans, and control flow;
- variables, assignments, arithmetic, and parameter expansion;
- redirection and process-substitution plans;
- generated background, pipefail, read, and process-substitution runtimes;
- simplification and renaming passes.

Translator state is private policy state. It tracks context, source ranges,
helper liveness, option state, ordered warnings, and runtime requirements. The
public boundary converts private warnings into stable `Diagnostic` values.

Unsupported standalone statements are fail-closed: normal mode emits a stable
diagnostic, an explanatory Fish comment, and `false`; strict mode returns a
failure. Compatibility fallbacks are narrowly allowlisted and declare their
external runtime requirements.

## Source And Output Planning

ShellCheck sourced-file expansion is disabled. `Monk.Source` is authoritative:
it discovers literal edges, translates every source into a structural script,
and records source mappings and diagnostics.

Typed passes then choose one of two shapes:

- combined output inlines the source graph and structurally deduplicates helper
  definitions;
- separate output relocates source paths, rewrites literal `source` targets,
  extracts live generated preambles, and emits at most one
  `_monk_runtime.fish`. Dependent files use quoted paths resolved from
  `status current-filename`, so nested sources remain file-relative even when
  the bundle is launched from another working directory.

`Monk.Output` never writes files. The CLI renders the plan and performs the
requested stdout/filesystem effects.

## Evidence And Tooling

The test suite combines focused unit tests, DSL/rendering properties, golden
fixtures, Bash/Fish differential integrations, real-world fixtures, source
bundle checks, and static architecture boundaries. The bake-off measures Monk
against Babelfish and benchmarks original Bash versus Monk-generated Fish with
the same fixture arguments, stdin, and execution mode.

CI runs bounded Ubuntu matrices for GHC 9.12.2 and 9.14.1 with pinned Fish 4.6.0
and the moving Fish 4 PPA. It also generates a fixture parity manifest containing
translation and Fish-syntax success, rendered hash, Fish bytes, diagnostic
codes, helper count, and declared requirements.

`shellcheck-syntax-inventory.md` records the explicit support or scope decision
for ShellCheck nodes at the Bash input boundary.
