# Monk Translator Roadmap

Last refreshed: 2026-07-31

This is Monk's ordered engineering backlog. It separates the evidence and
decisions required to ship 0.4 from work that should follow the release.

## Evidence Policy

- Close an item only when a reproducible local command or a linked CI run on the
  relevant commit supports it.
- Treat platform-specific skips as open evidence gaps, not successful coverage.
- Keep detailed semantic status in [translator-audit.md](translator-audit.md),
  ShellCheck node policy in
  [shellcheck-syntax-inventory.md](shellcheck-syntax-inventory.md), and user
  remediation in [migration-guide.md](../migration-guide.md).
- Keep dated measurements as baselines, not as permanent claims about the
  current tree.

## Status Snapshot

| Area | Status on 2026-07-31 | Evidence | Remaining gap |
| --- | --- | --- | --- |
| Structural 0.4 API and module boundaries | Verified locally | Commit `53029d9`; focused API and refactor-seam coverage passes in the local suite. Public raw modules and lowering internals are absent from Cabal's exposed modules. | The unpublished checkpoint has not run in GitHub Actions. |
| Local behavioral suite | Verified on macOS | Commit `53029d9`; `MONK_INTEGRATION=1 "$(cabal list-bin test:monk-test)" --hide-successes` reports all 339 tests passed on GHC 9.14.1 and Fish 4.8.1. | Six Linux-only `procsub-output` fixtures are skipped on macOS; `taoc` is skipped locally because `tac` is unavailable. |
| Fixture parity manifest | Verified locally | Commit `53029d9`; `scripts/generate-parity-manifest.sh "$(cabal list-bin exe:monk)" /tmp/monk-parity-manifest.tsv` translates and Fish-syntax-checks all 76 Bash fixtures. | No Ubuntu manifest has been captured for the architecture checkpoint. |
| CI definition | Present, not verified | The local workflow defines GHC 9.12.2/9.14.1 and pinned/moving Fish 4 jobs, full integrations, the Linux selector, HLint, Ormolu, Haddock, and parity artifacts. | The workflow has not run on the unpublished checkpoint. |
| Published CI | Blocked | At the 2026-07-31 review, [run 30183469449](https://github.com/eessmann/monk/actions/runs/30183469449) on `095f247ef78df9b1386922a15dc3828d4dbb2df1` failed while installing HLint under GHC 9.14.1, before build or test execution. | Make quality-tool installation compatible and obtain a current green run. |
| Hackage readiness | Conditional blocker | `cabal check` rejects unconditional `-Werror` and reports missing upper bounds. | Decide whether 0.4 targets Hackage; fix these findings if it does. |

## Ship 0.4

Work in this order. Do not expand the semantic surface unless a release gate
finds a correctness regression.

### 1. Restore Trustworthy CI

- [ ] Move HLint and Ormolu onto a compatible, deliberately pinned toolchain or
  a compiler-independent quality job.
- [ ] Avoid reinstalling and rerunning compiler-independent quality gates for
  every Fish matrix entry unless the duplication supplies distinct evidence.

Acceptance:

- A bootstrap-validation run reaches HLint, Ormolu check mode, build, tests,
  Haddock, and parity generation without an installation or timeout failure.
- Record that run's URL and tested commit SHA in this snapshot before
  publishing the architecture checkpoint.

### 2. Publish And Exercise The Architecture Checkpoint

- [ ] Publish the checkpoint after the CI bootstrap is credible.
- [ ] Run the supported GHC 9.12.2 and 9.14.1 jobs against pinned Fish 4.6.0
  and the moving Fish 4 signal.
- [ ] Download the Ubuntu parity manifests and compare translation success,
  Fish syntax, hashes, byte counts, diagnostic codes, helper counts, and
  external requirements with the local checkpoint.
- [ ] Capture current Linux evidence for all six `procsub-output` fixtures.

Acceptance:

- All supported matrix jobs are green on one commit.
- Every manifest has 76 successful translations and 76 successful Fish syntax
  checks, or an investigated platform-specific difference is recorded.
- The dedicated Linux selector runs rather than skips all six fixtures.

### 3. Choose The 0.4 Release Channel

- [ ] Record whether 0.4 is a GitHub/source release or a Hackage release.

Acceptance for a GitHub/source release:

- Record the GitHub/source release channel and its documentation and artifact
  contract.
- Hackage-only warnings are explicitly classified as post-release packaging
  work rather than silently ignored.

Additional acceptance for a Hackage release decision:

- Make `-Werror` a development/CI policy rather than an unconditional package
  option.
- Add deliberate PVP-compatible dependency bounds.
- `cabal check` exits successfully with no Hackage rejection.

### 4. Finalize Public 0.4 Contracts

- [ ] Decide whether `PhaseRuntime` has a defined producer and consumer; remove
  it before release if it does not.
- [ ] Decide whether `RequiresFishFeature` is part of the supported requirement
  model; remove or document and test it before release.
- [ ] Give output-planning diagnostics an explicit output phase, or document a
  deliberate alternative; do not label output collisions and missing output
  roots as source failures by accident.
- [ ] Add focused public API tests for the chosen diagnostic and requirement
  taxonomy.

Acceptance:

- Every exported constructor in `Monk.Translation.Types` has a documented
  meaning and at least one intended producer or an explicit extension role.
- Output-planning errors render with the chosen stable phase and code.
- README, architecture, migration guide, Haddocks, and tests agree with the
  final public surface.

### 5. Close Documentation Drift

- [ ] Make `architecture.md`, `translator-audit.md`, `migration-guide.md`,
  `babelfish-comparison.md`, the changelog, and current test descriptions use
  one canonical structural Fish representation.
- [ ] Remove current-tense claims that the translator lowers through a distinct
  raw AST when the compatibility lowering is an identity boundary.
- [ ] Leave old Superpowers specs and plans unchanged as dated historical
  records.

Acceptance:

- Current documentation distinguishes the canonical structural DSL from
  private compatibility module names without describing two live ASTs.
- Documentation links resolve and `git diff --check` reports no errors.

### 6. Cut The Release Checkpoint

- [ ] Re-run local build, tests with integrations, HLint, Ormolu check mode,
  Haddock, and the parity manifest.
- [ ] Confirm the green remote matrix and archived Linux/parity artifacts refer
  to the release commit.
- [ ] Move the changelog entry from `Unreleased` to `0.4.0`.
- [ ] Tag or publish 0.4 according to the selected release channel.

Acceptance:

- All previous `Ship 0.4` acceptance conditions are closed on the release
  commit.
- The release tag, changelog, package version, documentation, and CI evidence
  identify the same commit and contract.

## After 0.4

### P1: Correctness And Output Robustness

- [ ] Design staged, atomic replacement for multi-file output bundles so a
  filesystem failure cannot leave a partially updated bundle.
- [ ] Add failure-injection coverage for directory creation, staging, rename,
  and cleanup behavior before changing the CLI writer.

Success condition: a separately approved design defines rollback and recovery,
and automated tests prove that an interrupted write preserves the prior bundle
or leaves no published bundle.

### P1: Evidence Expansion

- [ ] Add a curated external Bash corpus with provenance and stable selection
  rules.
- [ ] Add mutation or fuzz-driven Bash/Fish differential testing for constructs
  already represented in the typed translator.
- [ ] Cluster mismatches by diagnostic code and translator subsystem, then use
  those clusters to rank semantic work.

Success condition: the roadmap can cite reproducible mismatch counts and
fixtures rather than intuition when promoting a semantic item.

### P2: Conservative Semantic Closure

Candidates, in evidence order rather than assumed priority:

- broader `set -e` and `pipefail` exception boundaries;
- asynchronous and argument-position output process substitution;
- subshell environment isolation;
- nonliteral source discovery and rewrite behavior;
- option-heavy traps, `shopt`, and coprocesses;
- residual warning-driven `read` combinations.

Success condition for promoting any candidate: a focused failing differential
fixture, an exact or explicitly bounded strategy, stable diagnostics, and a
measurable reduction in the external-corpus mismatch cluster.

### P2: Architecture Cleanup

- [ ] Evaluate replacing `inlineSourceGraph`'s IORef callback collection with a
  pure typed result.
- [ ] Evaluate retiring identity-lowering and legacy raw/`Fish*` vocabulary
  where it no longer protects a real boundary.
- [ ] Keep architecture cleanup separate from semantic changes so the existing
  differential suite remains a useful behavior gate.

Success condition: each cleanup has an approved narrow design, preserves the
public 0.4 contract, and passes the complete local/CI evidence suite unchanged.

### P3: Measured Performance Work

- [ ] Profile translator throughput with `-N1`, accumulation hot spots, source
  queue behavior, and parser configuration before selecting an optimization.
- [ ] Preserve translation fidelity, diagnostic ordering, and generated-output
  quality as hard constraints.

Success condition: every optimization names a reproducible corpus, baseline,
target metric, and regression gate.

### P3: Preservation And Repository Hygiene

- [ ] Reconsider comments and shebang preservation only when a concrete
  consumer or corpus demonstrates the need.

## Documentation Maintenance

- A semantic status change updates this roadmap,
  [translator-audit.md](translator-audit.md), the syntax inventory when
  applicable, and [migration-guide.md](../migration-guide.md) together.
- A pure release-evidence change normally updates this roadmap and the
  changelog only.
- Architecture boundary changes update [architecture.md](architecture.md);
  dated comparison results stay in
  [babelfish-comparison.md](../babelfish-comparison.md).
- Do not expand the status snapshot back into a completed-work archive; Git and
  the changelog already preserve that history.
