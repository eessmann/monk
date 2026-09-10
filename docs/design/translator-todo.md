# Translator redesign and release evidence

Last refreshed: 2026-09-10. Implementation branch: `codex/principled-translator`.
The 2026-09-09 conditional core is the frozen baseline for the approved
[native runtime and coverage extension](../superpowers/plans/2026-09-10-native-runtime-coverage.md).
Its historical [verification report](translator-verification.md) remains available;
the new final-tree evidence is recorded separately in
[native runtime verification](native-runtime-verification.md).

The [approved design](principled-translator-design.md) supersedes the earlier
release-only backlog. Its [implementation plan](../superpowers/plans/2026-09-09-principled-translator.md)
requires a working exact core, truthful exclusions, and safe publication before
release readiness. Tagging and publishing are separate actions.

## Evidence policy

Close a gate only with reproducible evidence on the final tree. A passing
rejection proves classification, not implemented functionality. Keep platform
skips, missing toolchains, unrun remote jobs, and unexplained failures visible.
Track zero-diagnostic semantic mismatches separately from translation failures.

Main `1a2c3826d5265b9be997c5365cd8c138f6cf015f` passed all 339 existing tests
with integrations enabled on Linux, Bash 5.3.9 and Fish 4.6.0. All six Linux
process-substitution fixtures executed. Nevertheless, eleven newly reproduced
strict translations had no diagnostics and disagreed with Bash. Their source
programs now live in `test/fixtures/semantic/`; the new suite initially failed
11 of 14 tests, with three successful exact controls. This supersedes the old
inference that the passing 339-test baseline established semantic readiness.

Both earlier branches are prototypes. The typed checkpoint `db0aed4` recorded
332 failures out of 1093 and did not certify the final materialized artifacts.
Its source identity and execution-boundary ideas inform this redesign, but its
failure backlog and overlapping analyses are not acceptance evidence.

## Implementation gates

| Stage | Required outcome | Current evidence |
| --- | --- | --- |
| 1. Evidence/admission | Every parser constructor and semantic context has an exact envelope, named approximation, or rejection; useful positive suite is mandatory. | Exhaustive ShellCheck 0.11 constructor match and context policy; all 11 original counterexamples retained; no legacy translation fallback. |
| 2. Words/evaluation | Scalar/list boundaries, zero/one/many fields, empty quoted substitutions, IFS, admitted globs, lazy case and all case terminators. | Owned scalar/field and pattern plans; mandatory differentials for quoted argv, splitting, pathname patterns, lazy effects and all case outcomes. |
| 3. Arithmetic/state | Signed 64-bit operator-tree execution; intermediate truncation, wrapping, side effects, lazy errors; actual-path option state. | Structured integer primitives and framed operands; one runtime binding owner; finite flow joins; executed option transitions with short-circuit/error regressions. |
| 4. Functions/dispatch | Definite identity before builtin interception; definition order; compatible finite call contexts; deferred redirects. | Definition identities include absence/builtin lookup dependencies; compatible body-local contexts, ordered redefinition, imported calls and invocation-time standard redirects. |
| 5. Sourceable/sources | Owned return/status/argv boundary, declared caller changes, immutable literal dependencies and repeated acyclic execution. | Versioned caller contract and guards; normalization-driven discovery; owned snapshots and occurrences; explicit argv/status/return frame and persistent function helpers. |
| 6. Residual effects | Shared/child ownership and callback relevance; useful exact cases retained, other cases rejected with explanations. | Isolated substitutions, subshells and bounded pipelines, with large/invalid-byte and transitive closure tests. Background jobs, process substitution and traps explicitly reject. Superseded semantic walkers removed. |
| 7. Publication/release | Immutable generations, atomic entry replacement, durable failure recovery, package/CI/docs consistency. | Managed pinned generations, typed recovery and retry durability; private failure injection and concurrent reader/publisher coverage. Final compiler/runtime/package gates recorded below. |

## Native runtime and coverage acceptance, 2026-09-10

The approved extension is implemented in the existing worktree. Its accepted
304-file build-input manifest is
`7bb370a18da2c3f90a3abe190b423cd4cb9ee35e7ad3f36bb03b5cde53c70043`.
The [verification report](native-runtime-verification.md) records the compiler,
runtime, publication, abstraction and packaging evidence; the
[comparison report](../babelfish-comparison.md) records frozen inputs and raw
measurements. Earlier checkpoints remain historical evidence.

- [x] Compiled Haskell runtime replaces all generated Python support; bounded
  operations use a versioned byte protocol and preserve child I/O ownership.
- [x] Runtime selection and compatibility checks precede body effects; managed
  publication captures provider bytes and pins executable generations, including
  deferred exported functions. Binary roles, modes and bytes enter identity.
- [x] One optimization stage precedes admission: shared integer folding, bounded
  pure arithmetic batching, helper interning, transitive child requirements and
  structural statistics. Simple literal output needs no native helper.
- [x] Arithmetic loops and dollar-bracket arithmetic, ANSI-C words, brace
  products, bounded parameter operators, scalar append, shift and fixed-arity
  conditions have positive differential and explicit rejection coverage.
- [x] The stable directory contract and caller-contract version 2 cover the
  admitted directory operations, distinct binding/stack state and failure flow.
- [x] Frozen coverage reaches 45/95 by default and 48/95 under the stable
  directory contract, preserving all 38 baseline matches with zero admitted
  mismatches. All seven common and three directory target fixtures match.
- [x] Serial, alternating measurements with three warmups and twenty samples
  pass all optimization gates: arithmetic aggregate medians improve 6.52 times,
  the original 16-fixture common subset improves 2.66 times, and `large-exact`
  Fish is 170,987 bytes, 10.4% of its 1,641,887-byte baseline.
- [x] The 725-test suite passes with integrations enabled on both GHC 9.12.2
  and 9.14.1 and both pinned Fish 4.6.0 and moving Fish 4.8.1; Bash evidence
  uses 5.3.9.
- [x] Native byte/descriptor/signal tests, 38 private publication tests, nine
  native publication blackboxes, twelve public boundary controls, Linux
  selectors, parity manifests, HLint, Ormolu, Haddock and `cabal check` pass.
- [x] Both supported compilers build and install both executables from unpacked
  source, with combined/managed smoke comparisons and Python-free execution.
- [x] Independent native and semantic reviews are closed; superseded candidate
  measurements and failed sandboxed installation attempts remain identified.
- [ ] Remote CI acceptance remains unrun on this unpushed tree. Local results
  establish the verified candidate, not execution of the configured remote jobs.

Bundled executables remain specific to the declared Linux platform and recorded
dynamic libraries. Publishing, tagging and generation garbage collection are
outside this implementation.

## Historical 2026-09-09 acceptance record

- [x] Original eleven counterexamples: nine exact differential successes and two explicit rejections.
- [x] Zero/one/many fields, quoted empties, IFS, glob/no-match and evaluation-region interactions.
- [x] Arithmetic intermediate truncation, signed overflow, short-circuiting, context-specific errors.
- [x] Branch/call option changes, redefinitions, dynamic heads, invocation-time redirects.
- [x] Both entry modes: return, incoming/final status, argv, repeated sources, exported and caller-local changes.
- [x] Namespace collisions, relevant callback uncertainty, shared and child effects.
- [x] Bounded generated compositions with shrinking; no zero-diagnostic mismatches in admitted cases.
- [x] Opaque product compile-fail checks with positive controls and private record-label checks.
- [x] Publication failures during staging, flushing, replacement, recovery; concurrent readers/publishers.
- [x] Full tests with integrations, explicitly accounted changes to old expectations, no unexplained failures.
- [x] GHC 9.12.2 and 9.14.1; pinned Bash/Fish evidence; moving Fish 4.8.1 compatibility on both compilers.
- [x] Linux selectors, parity manifests, HLint, Ormolu, Haddock, `cabal check`.
- [x] Unpacked source build/install smoke tests with both compilers, including UTF-8 combined and managed output.
- [x] Current remote CI acceptance is explicitly recorded as an external evidence gap.

The passing main-suite count is 438 on each supported GHC, with 36 additional
publication tests. One oversized manual neofetch baseline is explicitly skipped;
it contributes no translator acceptance evidence. No unexplained failures remain.
The remote matrix is configured but has not run on this unpushed candidate.

The subsequent [Babelfish comparison](../babelfish-comparison.md) reruns 95
fixtures with independent Bash byte/status checks and serial timings. Monk
matches all 38 admitted translations, rejects 57, and has no admitted mismatch
in that run. Runtime overhead remains substantial; the report records both
the full corpus and the 16-fixture shared matching subset.

Keep [semantic audit](translator-audit.md), [syntax policy](shellcheck-syntax-inventory.md),
[architecture](architecture.md), and [migration guidance](../migration-guide.md)
aligned with the actual admitted surface. Historical Superpowers plans remain
historical records and are not rewritten to imply completed work.
