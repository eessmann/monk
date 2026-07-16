# Monk 0.4 Translator Roadmap

Last refreshed: 2026-07-16

This roadmap records evidence, blockers, and remaining work. A feature is only
marked verified when a reproducible local or CI command supports the claim.
`docs/design/translator-audit.md` remains the semantic fidelity matrix and
`docs/migration-guide.md` remains the user-facing cleanup guide.

## Verified

- The public Fish DSL is structural. Its core types live independently in
  `Language.Fish.DSL.Types` and `Language.Fish.DSL.Internal`; no public raw AST
  or lowering module is exposed.
- Translation, simplification, renaming, source rewriting, and inlining now
  exchange structural `Script` values. `Monk.AST.Raw` has been removed.
- `TranslationResult` exposes only a structural `Script`, ordered structured
  diagnostics, and deduplicated runtime requirements. Strict failures contain a
  nonempty diagnostic collection.
- Diagnostics use explicit stable codes, phases, severities, ranges, and
  `ReviewRisk = Clean | Review | Unsafe`; confidence percentages are retired.
- ShellCheck source expansion is disabled. Monk's typed `SourceGraph` owns
  recursive literal source discovery.
- Standalone `!`, compound commands in covered status positions, fail-closed
  unsupported statements, extglob compatibility requirements, and dedicated
  here-string diagnostics have focused tests.
- Structural `StatusPlan` lowering covers conditionals, cases, loops,
  selections, functions, and background constructs in conjunction/condition
  positions. Compound pipeline stages have a Bash/Fish differential fixture.
- Brace expansion, dollar-single-quoted strings, deprecated dollar-bracket
  arithmetic, source wrappers, standalone negation, and Bats scope are recorded
  in an explicit ShellCheck syntax inventory with executable coverage for the
  supported Bash forms.
- Separate recursive output is represented as an `OutputBundle`. Generated
  helper preambles are extracted, structurally deduplicated, and emitted as at
  most one `_monk_runtime.fish`. Source paths are quoted and resolved relative
  to `status current-filename`, so nested bundles do not depend on the caller's
  working directory. An execution test covers two sourced helper consumers.
  Duplicate user/runtime targets are rejected with
  `monk.output.duplicate-target` before any file is written.
- Recursive inlining traverses typed conditions, pipelines, conjunctions,
  wrappers, redirects, indexes, and command/process substitutions. The CLI's
  inline mode uses the combined bundle planner, including global helper
  deduplication and merged runtime requirements. Sources with argv and
  redirections restore argv and then reproduce the sourced script's exact exit
  status; a status-7 differential fixture covers `$status` and conditionals.
- Arbitrary-delimiter multi-variable and array reads use one Python process and
  no nested Fish process. A proven raw single-variable delimiter path uses Fish
  4.6 primitives and retains the Python exact fallback for harder cases.
- On the local `read-delimiter` fixture, generated output moved from 5,326 to
  936 bytes (82.4% smaller) and the 20-run Hyperfine mean moved from 73.5 ms to
  9.3 ms (87.3% faster) on the same machine. All six local delimiter/IFS
  differential fixtures pass.
- Full neofetch output is valid Fish and measures 665,372 bytes for 376,936
  input bytes, a 1.7652 expansion ratio. Bounded command-substitution indentation
  removed alignment-driven whitespace without changing structural nesting.
- The bake-off report records translated bytes, expansion ratio, helper bytes,
  helper invocations, declared external requirements, diagnostic counts, and
  review risk. Its Hyperfine runtime suites replay fixture args, stdin, and run
  mode against original Bash and generated Fish, then report medians, means,
  and standard deviations. Runtime plans admit only successful Monk
  translations, preflight Bash and Fish syntax before Hyperfine, and reject
  missing artifacts while continuing to allow intentional nonzero script exits.
- Deduplicated runtime requirements retain operation-specific reasons and every
  available source range, including repeated uses of the same shared helper.
- The local suite contains 339 passing tests. With integrations enabled, all
  locally runnable Bash/Fish differential fixtures pass; six Linux-only output
  process-substitution fixtures remain skipped on macOS.
- The parity manifest translates and Fish-syntax-checks all 76 Bash fixtures,
  recording hashes, bytes, diagnostic codes, helper counts, external
  requirements, translation success, and syntax success.
- CI defines bounded Ubuntu jobs for GHC 9.12.2 and 9.14.1 against pinned Fish
  4.6.0 and the moving official Fish 4 PPA. It gates HLint, Ormolu check mode,
  build, Haddock, the complete integration suite, the Linux `procsub-output`
  selector, Fish syntax validation, and parity-manifest upload.
- GitHub Actions is enabled for `eessmann/monk` with all actions allowed.

## Blocked

- Linux output-process-substitution evidence remains open until the new Ubuntu
  jobs run successfully. Local macOS skips for the six `>(...)` fixtures are not
  evidence.
- The workflow has not yet run for this unpublished worktree branch. Ubuntu
  evidence remains blocked until the branch is pushed and Actions completes.
- The moving PPA can lag the newest Fish release. Pinned 4.6.0 is the minimum
  runtime gate; the PPA job is the moving compatibility signal.

## Next

- Capture the first Ubuntu parity manifest and compare its rendered hashes,
  bytes, diagnostic codes, helper counts, requirements, and success flags with
  the local architecture checkpoint.

## Deferred

- Wider compatibility work: `shopt`, traps, coprocesses, nonliteral sources,
  subshell isolation, broader `set -e`/`pipefail`, and asynchronous output
  process substitution.
- External corpus ingestion and mutation/fuzz differential testing.
- Translator-throughput profiling (`-N1`, sequence-backed accumulation, source
  queue behavior, and parser configuration) after output-quality work.
- Comment and shebang preservation unless a concrete consumer requires them.
- Repository remote-alias cleanup; it is hygiene rather than a translator
  blocker.

## Release Gates

Release 0.4.0 only when:

1. raw constructors and lowering internals remain private and unused outside
   renderer boundaries;
2. the public API migration is documented in README, architecture, migration
   guide, audit, and changelog;
3. semantic and generated-runtime tests pass locally, including Fish syntax and
   Bash/Fish differential checks;
4. the parity manifest is captured in Ubuntu CI;
5. the pinned Fish 4.6.0 and moving Fish 4 jobs are green; and
6. the Linux `procsub-output` selector supplies current evidence.
