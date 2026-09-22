# Translator redesign and release evidence

Last refreshed: 2026-09-22. The current implementation follows the approved
[portable exact-coverage plan](../superpowers/plans/2026-09-22-portable-exact.md)
from base `c2bd371`. It extends the existing semantic architecture; historical
checkpoints below retain their original evidence and dates.

## Portable coverage implementation, 2026-09-22

| Area | Implemented behavior and boundary |
| --- | --- |
| Dependencies | Pinned IOHK haskell.nix; one Cabal project for devenv, CI and release outputs; GHC 9.14.1 default and 9.12.2 compatibility; Bash 5.3.9/Fish 4.6.0 plus locked newer Fish. |
| Runtime ABI 2 | Native standalone entry captures streams before Fish startup; portable descriptor ownership, parent-prepared POSIX spawning, private versioned control transport, native target metadata and immutable provider capture. Linux musl and Apple Silicon macOS package definitions are separate from execution evidence. |
| Selective supervision | Direct native Fish for proven scalar/control/function/external-pipeline programs. Standalone session owner for real process identities, waiting, descriptor tables, pipelines and background lifetime; control and shared variables stay in generated Fish. |
| Input and arrays | Ordered opens, append, duplication/closure, compound scopes, here input, finite prefix assignments, byte `read`, dense indexed arrays and child snapshots. Unknown inherited descriptors, sparse arrays and session-only sourceable effects reject. |
| Expansion | Composed quote-aware splitting/pathname expansion and parameter trimming, C-locale bracket classes, effect-preserving snapshots, safe literal rendering and single-operation pure arithmetic batching. |
| Process substitution | Concurrent owned pipe endpoints, multiple substitutions, early close, explicit wait and independent status. Endpoints may not escape into arbitrary stored values or unowned consumers. |
| Callbacks and finite code | Compiled standalone EXIT/ERR bodies with live reads and status/suppression rules; finite `eval`; immutable source bodies inside compatible owned functions and children. Arbitrary callbacks and runtime-generated programs remain excluded. Directory operations combined with EXIT/ERR traps reject until shared stdio error state across signal callbacks is modeled. |
| Readability | Structural DSL and one semantic pipeline; native scalar storage/argv, status provenance, fewer snapshots, flattened standalone guards, concise literals and demand-driven support. External dispatch uses a replace-self native shim to preserve Bash launch diagnostics. Source output uses one shared native writer boundary to preserve write errors and SIGPIPE. |

Fresh validation and outstanding gates are recorded in the
[portable verification report](portable-runtime-verification.md). The
[comparison report](../babelfish-comparison.md) preserves the 95-fixture
denominator and reports filesystem, caller-state and process cohorts separately.
The original Monk `58/58` translation-success figure is not an exactness target.
The previous 45/95 (48 with stable directory contract) and Babelfish 26-match /
32-mismatch counts below are historical Linux executions; matching all 304
historical build-input hashes at the initial checkout did not rerun those tests.

The planned helper-free greeting gate conflicts with an observed Fish/Bash
semantic difference: with stdout initially closed, Bash `printf x` returns 1
and diagnoses the failed write, while Fish returns 0 silently. With a pipe
whose reader has closed, Bash terminates from SIGPIPE while Fish returns 1.
Exact output therefore requires a shared native writer even for a greeting.
The user-approved `monk-runtime --abi 2 launch FILE [ARGS...]` entry captures
initial descriptor state before Fish can replace missing stdout with `/dev/null`;
sourceable output retains its existing caller interface. Launcher-only runtime
requirements capture a provider for managed silent programs without adding
primitive calls or helpers to their Fish body.
The compiler retains direct scalar/control expressions and avoids per-command
operand/status snapshots; the writer owns one necessary status slot and actual
signal termination. The original helper-free output criterion is not claimed
as achieved, and the execution contract has not been narrowed to hide these
counterexamples.

Native Linux execution is explicitly deferred by the user. Source stays local;
no Monk build or test ran on ccs-ci. This is an open platform acceptance gate,
not a semantic mismatch or a substitute for native testing. Packaging, timing,
compiler and quality claims require their own final-input receipts. Publishing,
tagging and remote Git changes remain outside this implementation.

## Fresh local acceptance evidence, 2026-09-22

The final compiler/runtime bytes were checked with the pinned Nix reference
on Apple Silicon macOS. The [verification report](portable-runtime-verification.md)
and [comparison report](../babelfish-comparison.md) retain source identities,
checksums, exact commands and the distinctions between successful snapshots.

| Gate | Fresh local result |
| --- | --- |
| Frozen historic95, default contract | 74 exact matches, 21 explicit rejections, zero admitted mismatches. |
| Frozen historic95, stable-directory contract | 77 exact matches, 18 explicit rejections, zero admitted mismatches. |
| Independent providers, default contract | Original Monk: 71 matches/24 mismatches. Babelfish: 26 matches/32 mismatches/37 rejections. Current baseline: 23 matches/50 rejections/22 Darwin-unavailable cases; all executable baseline positives retained. |
| Separate observational cohorts | Filesystem 1/1, sourceable caller 1/1, process lifetime 2/2; strengthened read/array controls 4 matches plus one explicit timing rejection. These do not enlarge historic95. |
| Main integration suites | 980 tests pass on GHC 9.14.1 and GHC 9.12.2 with Bash 5.3.9/Fish 4.6.0. Fish 4.9.3 passes the prior full 978-test suite and the two subsequently added harness regressions. |
| Native and release checks | Protocol, descriptors, streams, sessions, callbacks, managed launch, publication, public API boundaries, HLint, Ormolu, Haddock and unpacked source build/install checks pass. Receipts identify the exact executable bytes and distinguish test-only corrections from runtime changes. |
| Apple Silicon package | Copied ABI 2 artifact executes outside the build shell and has only Apple system-library dependencies. Archive, binary checksums and macOS deployment target are recorded. |
| Performance, runnable common14 | Final baseline/candidate aggregate medians: 171.476 ms / 621.447 ms, a 3.624 times regression. Candidate time is 38.7% below the pre-optimization run, but the 10% limit is not met. The other two common cases cannot run against the baseline on Darwin. |

The environment snapshot harness now uses NUL framing: an exported multiline
`DEVENV_CMDLINE` previously produced two false caller-state failures. Bash and
Fish regressions reproduce that defect, and the full corrected suite passes
with the original failing environment injected. The production-input scanner
also includes this private harness module; its changed fingerprint is not
misrepresented as a translator or runtime executable change.

A measured runtime shutdown cost led to a narrow optimization: completed raw
writers exit immediately; metadata output flushes before exiting; and native
launch exits after its wait and all cleanup finish. Session and generic primitive
shutdown paths retain their ownership behavior. Full native checks, the 980-test
canonical suite, compatibility/moving-Fish checks and the rebuilt copied macOS
package pass after this change. Both frozen95 comparison counts remain unchanged.

The performance regression is an observed failure, not merely missing evidence.
Three warmups and twenty alternating samples preserve Bash output/status for every
measured sample. The full common16 cohort remains incomplete, all three original
arithmetic inputs are unavailable, and targeted elapsed-time/launch-count gates
remain unverified. The reduced teardown overhead does not establish overall
performance acceptance.

Remaining gates are explicit: native execution and static inspection of both
Linux packages; execution at the declared minimum platform versions; the full
common/arithmetic performance and measured-launch criteria; and the helper-free
greeting criterion discussed above. Missing original arithmetic inputs and a
Linux-only historical runtime prevent full local performance acceptance.
Configured remote CI is not executed CI evidence. No publishing or remote Git
operation is implied by these local results.

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

## Historical architecture gates, 2026-09-10

| Stage | Required outcome | Current evidence |
| --- | --- | --- |
| 1. Evidence/admission | Every parser constructor and semantic context has an exact envelope, named approximation, or rejection; useful positive suite is mandatory. | Exhaustive ShellCheck 0.11 constructor match and context policy; all 11 original counterexamples retained; no legacy translation fallback. |
| 2. Words/evaluation | Scalar/list boundaries, zero/one/many fields, empty quoted substitutions, IFS, admitted globs, lazy case and all case terminators. | Owned scalar/field and pattern plans; mandatory differentials for quoted argv, splitting, pathname patterns, lazy effects and all case outcomes. |
| 3. Arithmetic/state | Signed 64-bit operator-tree execution; intermediate truncation, wrapping, side effects, lazy errors; actual-path option state. | Structured integer primitives and framed operands; one runtime binding owner; finite flow joins; executed option transitions with short-circuit/error regressions. |
| 4. Functions/dispatch | Definite identity before builtin interception; definition order; compatible finite call contexts; deferred redirects. | Definition identities include absence/builtin lookup dependencies; compatible body-local contexts, ordered redefinition, imported calls and invocation-time standard redirects. |
| 5. Sourceable/sources | Owned return/status/argv boundary, declared caller changes, immutable literal dependencies and repeated acyclic execution. | Versioned caller contract and guards; normalization-driven discovery; owned snapshots and occurrences; explicit argv/status/return frame and persistent function helpers. |
| 6. Residual effects | Shared/child ownership and callback relevance; useful exact cases retained, other cases rejected with explanations. | Isolated substitutions, subshells and bounded pipelines, with large/invalid-byte and transitive closure tests. Background jobs, process substitution and traps explicitly reject. Superseded semantic walkers removed. |
| 7. Publication/release | Immutable generations, atomic entry replacement, durable failure recovery, package/CI/docs consistency. | Managed pinned generations, typed recovery and retry durability; private failure injection and concurrent reader/publisher coverage. Final compiler/runtime/package gates recorded below. |

## Historical native runtime and coverage acceptance, 2026-09-10

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

At that checkpoint, bundled executables were specific to the declared Linux
platform and recorded dynamic libraries. Publishing, tagging and generation garbage collection are
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
