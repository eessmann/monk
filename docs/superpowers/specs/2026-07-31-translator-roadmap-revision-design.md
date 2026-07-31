# Translator Roadmap Revision Design

## Purpose

Revise `docs/design/translator-todo.md` so it answers two different questions
without mixing them together:

1. What evidence and decisions still block the Monk 0.4 release?
2. What design and implementation work should follow the 0.4 checkpoint?

The roadmap will remain the ordered engineering backlog. Detailed semantic
status stays in `docs/design/translator-audit.md`, parser-node policy stays in
`docs/design/shellcheck-syntax-inventory.md`, and user cleanup advice stays in
`docs/migration-guide.md`.

## Current Evidence

The revision will use the live checkout rather than the older roadmap snapshot:

- The local 0.4 architecture checkpoint is ahead of published `main` and has
  not run in GitHub Actions.
- The built test executable passes all 339 tests with integrations enabled on
  GHC 9.14.1 and Fish 4.8.1. Six Linux-only output-process-substitution fixtures
  are skipped on macOS, and the `taoc` fixture is skipped because `tac` is not
  installed locally.
- The local parity command translates and Fish-syntax-checks all 76 Bash
  fixtures successfully.
- The latest scheduled GitHub Actions run on published `main` failed while
  installing HLint under GHC 9.14.1, before build or test execution. The local
  workflow still installs HLint and Ormolu inside every compiler/Fish matrix
  job, so the unpublished workflow is not yet evidence that this failure is
  resolved.
- `cabal check` reports that Hackage would reject the package because `-Werror`
  is unconditional. It also reports missing upper bounds. These findings only
  block 0.4 if Hackage distribution is part of the release target.

## Roadmap Structure

### 1. Status Snapshot

Replace the long `Verified` narrative with a compact table containing:

- evidence area;
- current status;
- exact command or external run that supports the status;
- remaining limitation or platform gap.

The snapshot will record local unit/property/golden results, local differential
results, parity-manifest results, architecture-boundary checks, and remote CI
status. Historical performance measurements may remain as short baselines, but
they will not occupy the active backlog.

### 2. Horizon One: Ship 0.4

Order release work by dependency and give every item an acceptance condition.

1. **Restore trustworthy CI.** Separate or otherwise constrain quality-tool
   installation so HLint/Ormolu compatibility does not prevent the supported
   GHC builds from running. Avoid repeating compiler-independent quality checks
   across all Fish matrix entries unless the duplication provides evidence.
   Acceptance requires a run that reaches every intended gate.
2. **Publish and exercise the architecture checkpoint.** Push the unpublished
   checkpoint through the supported GHC 9.12.2/9.14.1 and pinned/moving Fish 4
   jobs. Capture the parity artifacts and the dedicated Linux
   `procsub-output` selector. Acceptance requires green jobs and downloadable
   manifests for the tested commit.
3. **Choose the release channel.** Record whether 0.4 is a GitHub/source release
   or a Hackage release. A Hackage release additionally requires a clean
   `cabal check`, including a development-only `-Werror` policy and deliberate
   dependency bounds.
4. **Finalize the public 0.4 contracts.** Resolve unused or ambiguous public
   variants before release. In particular, decide whether `PhaseRuntime` and
   `RequiresFishFeature` have defined consumers or should be removed, and give
   output-planning errors their correct phase instead of labeling them as
   source errors. Acceptance requires public API tests and matching
   documentation for the chosen taxonomy.
5. **Close documentation drift.** Make the architecture, audit, migration
   guide, comparison document, changelog, and test descriptions consistently
   describe one canonical structural Fish representation. Treat old
   Superpowers specs and plans as historical records rather than current
   architecture documentation.
6. **Cut the release checkpoint.** Re-run the local and remote gates, archive
   the final evidence, move the changelog entry from `Unreleased` to 0.4.0, and
   tag or publish according to the chosen release channel.

No broader semantic expansion belongs in this horizon unless a release gate
reveals a correctness regression.

### 3. Horizon Two: After 0.4

Prioritize post-release work in this order:

1. **Correctness and output robustness.** Define and test failure semantics for
   writing multi-file bundles. Planning currently rejects duplicate targets,
   but the CLI writes files sequentially and can leave a partial bundle after a
   filesystem failure. Consider staging and atomic replacement as a separate
   design task.
2. **Evidence expansion.** Add an external corpus and mutation or fuzz-driven
   Bash/Fish differential tests. Use the resulting mismatch clusters to rank
   semantic work rather than expanding syntax coverage speculatively.
3. **Conservative semantic closure.** Revisit `set -e`/`pipefail`, asynchronous
   or argument-position output process substitution, subshell isolation,
   nonliteral sources, option-heavy traps, `shopt`, and coprocesses only when
   evidence justifies the complexity. Keep unsupported forms fail-closed and
   diagnostic-driven in the meantime.
4. **Architecture cleanup.** Evaluate making source inlining a pure typed pass,
   retiring identity-lowering and compatibility vocabulary, and reducing
   legacy `Fish*`/raw-shaped internal naming. Do not combine this cleanup with a
   semantic change.
5. **Measured performance work.** Profile translator throughput before changing
   accumulation structures, parser configuration, concurrency, or source-queue
   behavior. Preserve translation fidelity and generated-output quality as hard
   constraints.
6. **Lower-priority preservation work.** Reconsider comments and shebangs only
   after a concrete consumer or corpus demonstrates their value.

## Documentation Contract

The revised roadmap will enforce these ownership rules:

- `translator-todo.md` owns priority, order, blockers, and acceptance evidence.
- `translator-audit.md` owns the semantic capability matrix.
- `shellcheck-syntax-inventory.md` owns explicit parser-node support decisions.
- `migration-guide.md` owns user-facing remediation for best-effort and
  unsupported translations.
- `architecture.md` owns current module and data-flow boundaries.
- `babelfish-comparison.md` owns dated comparative measurements and must label
  snapshots as historical when the fixture inventory has changed.

When a semantic item changes status, update the roadmap, audit, syntax
inventory when applicable, and migration guide together. Pure release-evidence
changes normally update only the roadmap and changelog.

## Revision Acceptance Criteria

The roadmap revision is complete when:

- the active work is visibly split into `Ship 0.4` and `After 0.4`;
- every release item has a reproducible acceptance condition;
- live local evidence is distinguished from missing Linux or GitHub evidence;
- the current CI bootstrap failure and conditional Hackage work are explicit;
- public API, output robustness, evidence, semantic, architecture, and
  performance gaps are all represented once, at the appropriate horizon;
- detailed fidelity claims are linked to the audit instead of duplicated;
- completed history is compressed into a status snapshot;
- no item claims verification from a workflow that has not run on the current
  commit; and
- the revision changes documentation only, not implementation or CI behavior.

## Out of Scope

This revision does not fix CI, change public types, alter output writes, expand
translation semantics, publish commits, or release 0.4. Those actions require
separate implementation plans after the roadmap itself is accepted.
