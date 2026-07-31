# Translator Roadmap Revision Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the evidence-heavy translator roadmap with an actionable two-horizon backlog for shipping Monk 0.4 and directing post-0.4 engineering.

**Architecture:** Keep `docs/design/translator-todo.md` as the single ordered backlog, with a compact dated evidence snapshot followed by `Ship 0.4` and `After 0.4`. Link detailed fidelity claims to the audit and preserve the existing ownership boundaries among the architecture, syntax inventory, migration guide, comparison, and changelog documents.

**Tech Stack:** Markdown, Git, Cabal, Tasty, Bash/Fish differential fixtures, GitHub Actions evidence.

## Global Constraints

- The revision changes documentation only, not implementation or CI behavior.
- The active work must be visibly split into `Ship 0.4` and `After 0.4`.
- No broader semantic expansion belongs in the release horizon unless a release gate reveals a correctness regression.
- Every release item must include a reproducible acceptance condition.
- Local evidence must remain distinct from missing Linux or GitHub evidence.
- No item may claim verification from a workflow that has not run on the current commit.
- `translator-todo.md` owns priority, order, blockers, and acceptance evidence.
- `translator-audit.md` owns the semantic capability matrix.
- `shellcheck-syntax-inventory.md` owns explicit parser-node support decisions.
- `migration-guide.md` owns user-facing remediation for best-effort and unsupported translations.
- `architecture.md` owns current module and data-flow boundaries.
- `babelfish-comparison.md` owns dated comparative measurements.

---

## File Structure

- Modify `docs/design/translator-todo.md`: replace the historical verified/blocker narrative with the current evidence snapshot and the two ordered backlog horizons.
- Read `docs/superpowers/specs/2026-07-31-translator-roadmap-revision-design.md`: authoritative requirements for this documentation change.
- Read only for consistency: `docs/design/translator-audit.md`, `docs/design/shellcheck-syntax-inventory.md`, `docs/design/architecture.md`, `docs/migration-guide.md`, `docs/babelfish-comparison.md`, `CHANGELOG.md`, `.github/workflows/ci.yml`, and `monk.cabal`.
- Do not modify implementation, tests, workflow files, package metadata, or the adjacent design documents in this task.

### Task 1: Rewrite the Translator Roadmap

**Files:**
- Modify: `docs/design/translator-todo.md`
- Reference: `docs/superpowers/specs/2026-07-31-translator-roadmap-revision-design.md`
- Test: shell assertions against `docs/design/translator-todo.md`

**Interfaces:**
- Consumes: the approved design spec, the audit's semantic capability matrix, the syntax inventory's parser policy, and the live verification evidence recorded on 2026-07-31.
- Produces: the repository's ordered translator backlog with exact release gates, post-release priorities, and cross-document maintenance rules.

- [ ] **Step 1: Prove that the existing roadmap has the obsolete shape**

Run:

```bash
rg -n '^## (Verified|Blocked|Next|Deferred)$' docs/design/translator-todo.md
rg -n '^## (Ship 0\.4|After 0\.4)$' docs/design/translator-todo.md
```

Expected:

- The first command finds `Verified`, `Blocked`, `Next`, and `Deferred`.
- The second command exits with status 1 and prints no matches.

- [ ] **Step 2: Refresh the evidence that the roadmap will summarize**

Run these commands separately:

```bash
git status --short --branch
```

```bash
MONK_INTEGRATION=1 "$(cabal list-bin test:monk-test)" --hide-successes
```

```bash
test -x "$(cabal list-bin exe:monk)"
```

```bash
scripts/generate-parity-manifest.sh "$(cabal list-bin exe:monk)" /tmp/monk-parity-manifest.tsv
```

```bash
wc -l /tmp/monk-parity-manifest.tsv
```

```bash
gh run list -R eessmann/monk --workflow CI --limit 3 --json headSha,status,conclusion,createdAt,url
```

```bash
cabal check
```

Expected at this checkpoint:

- The direct test executable reports `All 339 tests passed`; six Linux-only output-process-substitution fixtures and the missing-`tac` fixture remain skipped on macOS.
- The existing CLI path reported by `cabal list-bin` is executable. Rebuilding
  is unnecessary for this documentation-only task and can produce a sandboxed
  Cabal build-log error after a successful link.
- The parity generator exits 0 and `wc -l` reports 77 lines: one header plus 76 fixtures.
- No GitHub Actions run exists for the unpublished architecture checkpoint; the dated published-main snapshot remains red before project tests.
- `cabal check` reports the unconditional `-Werror` rejection plus missing-upper-bound warnings.

If any substantive result differs, stop before replacing the roadmap and revise the dated evidence rows to match the new result. Do not preserve a stale count or CI claim.

- [ ] **Step 3: Replace the roadmap with the approved two-horizon document**

Replace the complete contents of `docs/design/translator-todo.md` with:

```markdown
# Monk Translator Roadmap

Last refreshed: 2026-07-31

This is Monk's ordered engineering backlog. It separates the evidence and
decisions required to ship 0.4 from work that should follow the release.

## Evidence Policy

- Close an item only when a reproducible local command or a linked CI run on the
  relevant commit supports it.
- Treat platform-specific skips as open evidence gaps, not successful coverage.
- Keep detailed semantic status in `translator-audit.md`, ShellCheck node policy
  in `shellcheck-syntax-inventory.md`, and user remediation in
  `../migration-guide.md`.
- Keep dated measurements as baselines, not as permanent claims about the
  current tree.

## Status Snapshot

| Area | Status on 2026-07-31 | Evidence | Remaining gap |
| --- | --- | --- | --- |
| Structural 0.4 API and module boundaries | Verified locally | The focused API and refactor-seam tests pass within the 339-test suite. Public raw modules and lowering internals are absent from Cabal's exposed modules. | The unpublished checkpoint has not run in GitHub Actions. |
| Local behavioral suite | Verified on macOS | The built test executable reports all 339 tests passed with `MONK_INTEGRATION=1` on GHC 9.14.1 and Fish 4.8.1. | Six Linux-only `procsub-output` fixtures are skipped on macOS; `taoc` is skipped locally because `tac` is unavailable. |
| Fixture parity manifest | Verified locally | `scripts/generate-parity-manifest.sh` translates and Fish-syntax-checks all 76 Bash fixtures. | No Ubuntu manifest has been captured for the architecture checkpoint. |
| CI definition | Present, not verified | The local workflow defines GHC 9.12.2/9.14.1 and pinned/moving Fish 4 jobs, full integrations, the Linux selector, HLint, Ormolu, Haddock, and parity artifacts. | The workflow has not run on the unpublished checkpoint. |
| Published CI | Blocked | At the 2026-07-31 review, the newest scheduled run on published `main` failed while installing HLint under GHC 9.14.1, before build or test execution. | Make quality-tool installation compatible and obtain a current green run. |
| Hackage readiness | Conditional blocker | `cabal check` rejects unconditional `-Werror` and reports missing upper bounds. | Decide whether 0.4 targets Hackage; fix these findings if it does. |

Historical output baselines, not release gates:

- The `read-delimiter` fixture previously measured 936 generated bytes and a
  9.3 ms 20-run Hyperfine mean after the exact-read rewrite.
- Full neofetch previously measured a 1.7652 generated/input byte ratio.

## Ship 0.4

Work in this order. Do not expand the semantic surface unless a release gate
finds a correctness regression.

### 1. Restore Trustworthy CI

- [ ] Move HLint and Ormolu onto a compatible, deliberately pinned toolchain or
  a compiler-independent quality job.
- [ ] Avoid reinstalling and rerunning compiler-independent quality gates for
  every Fish matrix entry unless the duplication supplies distinct evidence.

Acceptance:

- A run for the architecture checkpoint reaches HLint, Ormolu check mode,
  build, tests, Haddock, parity generation, artifact upload, and the dedicated
  Linux selector.
- Every intended job completes without an installation or timeout failure.
- Record the run URL and tested commit SHA in this snapshot.

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

- README installation instructions, changelog, tag, and release artifacts agree
  on the source-release contract.
- Hackage-only warnings are explicitly classified as post-release packaging
  work rather than silently ignored.

Additional acceptance for a Hackage release:

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
- [ ] Normalize the local `origin` URL from the historical `clam` alias to the
  canonical `eessmann/monk` URL when repository hygiene is next in scope.

## Documentation Maintenance

- A semantic status change updates this roadmap, `translator-audit.md`, the
  syntax inventory when applicable, and `migration-guide.md` together.
- A pure release-evidence change normally updates this roadmap and the
  changelog only.
- Architecture boundary changes update `architecture.md`; dated comparison
  results stay in `babelfish-comparison.md`.
- Do not expand the status snapshot back into a completed-work archive; Git and
  the changelog already preserve that history.
```

- [ ] **Step 4: Verify the new structure and required gap coverage**

Run:

```bash
rg -n '^## (Evidence Policy|Status Snapshot|Ship 0\.4|After 0\.4|Documentation Maintenance)$' docs/design/translator-todo.md
```

Expected: exactly one match for each of the five headings.

Run:

```bash
rg -n '^## (Verified|Blocked|Next|Deferred)$' docs/design/translator-todo.md
```

Expected: exit status 1 with no output.

Run:

```bash
rg -n '339 tests|76 Bash|HLint|GHC 9\.14\.1|PhaseRuntime|RequiresFishFeature|atomic replacement|mutation or fuzz|set -e|identity-lowering|comments and shebang' docs/design/translator-todo.md
```

Expected: every required evidence or gap phrase is present.

Run these link-target checks separately:

```bash
test -f docs/design/translator-audit.md
test -f docs/design/shellcheck-syntax-inventory.md
test -f docs/design/architecture.md
test -f docs/migration-guide.md
test -f docs/babelfish-comparison.md
```

Expected: every command exits 0.

- [ ] **Step 5: Review the documentation-only diff against the design spec**

Run:

```bash
git diff --check
```

```bash
git diff -- docs/design/translator-todo.md
```

```bash
git status --short
```

Expected:

- `git diff --check` prints nothing and exits 0.
- The diff replaces only the roadmap's structure and content described above.
- The status lists `docs/design/translator-todo.md` and this implementation
  plan only; there are no implementation, test, workflow, or package changes.

- [ ] **Step 6: Commit the roadmap revision**

Run:

```bash
git add docs/design/translator-todo.md docs/superpowers/plans/2026-07-31-translator-roadmap-revision.md
git diff --cached --check
git diff --cached --name-only
git commit -m "Revise translator roadmap"
```

Expected:

- The staged name list contains only the roadmap and this plan.
- The commit succeeds with subject `Revise translator roadmap`.
