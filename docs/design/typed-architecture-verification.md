# Typed architecture verification

This pass starts from `43d12c81264ce30986c7e9ba80a1bceedd01f77d` and preserves
CLI behavior and ABI 2. Implementation lives on `codex/typed-architecture` in an
isolated checkout. During the measurements below, the original checkout's
uncommitted `devenv.lock` patch was retained unchanged and the isolated build
used the committed lock. The later commit/merge request also incorporates that
dependency update, retaining the validated reference-shell pin and propagating
devenv's package policy into explicit Haskell imports. Its shell and integration
checks are recorded separately in
[the merge follow-up](../evidence/typed-architecture-merge-2026-09-23.json).
The measurements and source archives below retain their original lock identity.

## Enforced boundaries

The compiler now owns phase transitions and complete artifact materialization.
Entry bodies retain compilation, target, provider and entry identities; lexical
control roots, dense-array flow evidence, scalar/list regions and complete
runtime requests carry their relevant indices. Public Fish builders use closed
grammar/redirect constraints, validated identifiers and executable tokens.
Raw publication layouts remain private, and expiring synchronized leases protect
lock and staging operations even when callback actions escape.

These guarantees have specific scope: nested syntax carries control scope and
root kind, while compilation ownership is retained by entry bodies and artifacts.
Native specialization has statement-bound eligibility proof. Finite effect and
runtime-requirement sets are checked by the authoritative fold and admission;
there is no universal type-level effect row on every statement. See the
[migration guide](typed-architecture-migration.md) and
[architecture](architecture.md).

The single package now separates foundation, host, compiler, publication and
test-support components. Normalization, materialization, publication, native
execution and session transport have smaller responsibility-specific modules.
The compiler consumes immutable source documents; host execution and filesystem
discovery remain outside it.

## Local verification

The local execution profile is Apple Silicon Darwin, Bash 5.3.9, Fish 4.6.0,
Cabal 3.16.1.0 and pinned Rust nightly 2026-09-23. Both GHC 9.14.1 and 9.12.2 were
built and exercised.

| Check | Result |
| --- | --- |
| Main compiler, property, golden and integration suite | 1,021 passing tests on each GHC lane, with `MONK_INTEGRATION=1` |
| Compiled publication library | 47 passing tests on each lane, including fault recovery and escaped/concurrent lease actions |
| Host and tooling | 76 passing tests on each lane; non-threaded host suite also passes |
| Haskell construction boundaries | 70 cases pass on each compiled package database: three positive consumers and 67 intended compilation failures |
| Admission manifest | 90/90 reviewed classifications verified; all 71 admitted outputs pass Fish syntax checks |
| Native runtime suites | All 16 execution suites and the separate digest suite pass against the final translator/runtime |
| Rust | Cargo suites, 14 downstream failure probes with passing control, strict Clippy, rustfmt and 11 Miri checks pass |
| Quality | ABI generation, HLint, Ormolu, Cabal metadata and tooling-language policy pass |
| Documentation | Haddock builds for all components; unavailable dependency documentation leaves external-link warnings |
| Source distribution | Normal pinned devenv builds and installs the unpacked Cargo/Cabal products; combined and managed Unicode smoke outputs match Bash |

The full test suite retains one explicit manual-baseline skip for `neofetch`:
its hand translation is too large for that comparison. Tests reporting
rejection are admission evidence, not execution support for those inputs.
Native pathname tests also report two platform gaps: the local filesystem
rejects the invalid-byte names `\377` and `\376.txt` before execution.

Final packaging verification includes versioned JSON fixture sidecars. The
existing non-default platform assertion and six related execution comparisons
pass on both GHC lanes and in the freshly built unpacked archive. That follow-up
changes only the source-list glob, equivalent fixture metadata and documentation;
the measured Haskell/Rust sources remain identical. A final documentation archive
is checked against the tested archive for identical non-documentation members,
with a separate receipt to avoid a self-referential archive hash.

Baseline and candidate classifications are identical across the 90 reviewed
rows. The accepted generated outputs total 4,340,776 bytes before and 4,339,856
after this pass. This small aggregate reduction is an output-size observation,
not a runtime performance claim.

The binary-matched frozen corpus comparison covers 104 inputs in each contract
lane. Both compilers produce 80 exact matches and 24 rejections in the default
lane, and 82 matches and 22 rejections in the stable lane. The historical
95-input subsets retain 74 and 77 matches respectively. All 162 jointly admitted
lane rows match Bash and each other in raw stdout, stderr, status and the
applicable filesystem/caller observations. All 324 generated provider outputs
pass Fish syntax checks. The stable lane's caller probe requires a newer
directory-permission contract and is rejected by both providers.

## Measurement protocol

Baseline and candidate standalone executions both use their own ABI-2 runtime
launcher. The frozen baseline already requires that entry contract. An initial
asymmetric count recipe (direct Fish for baseline, launcher for candidate) is
retained as a diagnostic and excluded from performance conclusions.

Calibrated Darwin image tracing records identical launches for every accepted
fixture with symmetric entry: common16 totals 52 runtime and 16 Fish images per
provider; mixed3 totals 246 and 3; the three admitted targeted-native fixtures
total 11 and 3. Every counted run matches its uninstrumented control and Bash.
The fourth targeted-native fixture, `checked-printf`, is rejected by both
compilers. There is no measured launch reduction or regression. Instrumented
runs are never used for timing.

Translation profiling uses the identical probe linked against each frozen
library, three warmup pairs and 20 alternating serial sample pairs. Source-file
loading is excluded. Relative changes below are geometric means of per-fixture
candidate/baseline ratios; live bytes are the process RTS high-water mark.

| Translation metric | Common16 | Mixed3 |
| --- | --- | --- |
| Time | +11.0% | -1.6% |
| Allocation | +10.1% | -2.1% |
| Maximum live bytes | -0.8% | +38.6% |

These costs accompany a concrete output improvement: mixed3 generated bytes
fall from 74,563 to 52,205 (30.0%), status captures from 244 to 6, and temporary
bindings from 425 to 27. Runtime helper calls and helper definitions are unchanged.
The largest mixed-case live heap rises from 387,168 to 545,248 bytes. The
small-script translation overhead remains a measured cost of this pass.

Runtime timing uses three warmup pairs and 20 alternating serial pairs, checking
stdout, stderr and status against Bash on every run. Aggregates take the median
of the 20 per-index sums across a fixed cohort. Common16 is 0.9999 times baseline
(380.09 to 380.06 ms aggregate); mixed3 is 0.9994 (828.16 to 827.68 ms). Both meet
the 10% ceiling, but these tiny changes are best described as unchanged runtime
performance. The three admitted targeted-native cases measure 1.0065 times
baseline; that subset does not complete the four-case cohort. Missing arithmetic
inputs and the rejected targeted case keep full historical performance acceptance
open.

Build-cost observations use GHC 9.14.1, the frozen baseline source and the tested
candidate source archive. Both build all Haskell components, tests and benchmarks
with a warm shared dependency cache and fresh local build directories. Each row
is one serial paired observation, not a statistical benchmark. The incremental
case changes only a documentation comment in `app/Main.hs` in disposable copies.

| Build metric | Baseline | Candidate |
| --- | --- | --- |
| Clean wall time | 142.04 s | 163.06 s |
| Clean peak resident memory | 689.8 MiB | 704.7 MiB |
| No-op wall time | 0.19 s | 0.24 s |
| Incremental CLI wall time | 3.04 s | 3.13 s |

The clean build is 14.8% slower and uses 2.2% more peak resident memory in this
observation. These are additional implementation costs, separate from runtime
execution performance.

Session measurements retain the existing cancellation heartbeat. Idle and
three-blocked-job observations each span approximately five seconds, with a Fish
evaluator blocked on its builtin `read`. The new shared guardian adds one process
per session; each provider adds three processes for three jobs.

| Session observation | Baseline | Candidate |
| --- | --- | --- |
| Idle processes, including owner and evaluator | 2 | 3 |
| Idle summed resident memory | 12,224 KiB | 15,888 KiB |
| Idle CPU, percentage of one core | 1.97% | 2.56% |
| Three blocked jobs: processes | 5 | 6 |
| Three blocked jobs: summed resident memory | 29,184 KiB | 30,400 KiB |
| Three blocked jobs: CPU, percentage of one core | 2.56% | 2.96% |
| Median request latency | 3.015 ms | 3.083 ms |

Resident memory is the sum of `ps` observations, including shared pages counted
more than once; CPU uses coarse cumulative counters over a single short window.
Neither supports a precise scalability claim. Request latency uses three warmup
pairs and 20 alternating pairs of 50 sequential requests in ready sessions,
excluding startup/shutdown; it rises 2.3%. Output bytes, replies, repeated waits,
normal exit and workspace reclamation are checked for both providers.

## Correctness fixes discovered during migration

- Prefix assignments now install each value before the next operand's command
  substitution. The regression preserves Bash's `x=after y=$(printf '%s' "$x")`
  evaluation order while restoring the enclosing binding afterward.
- Fish control-word function targets produce located semantic diagnostics
  before materialization. Invalid target construction no longer raises an
  uncaught exception; ordinary functions remain admitted. Two additional legal
  Bash counterexamples, functions named `and` and `time` called with quoted
  command names, were admitted with mismatching output by the baseline and are
  now rejected with `monk.semantic.function-name`. These demonstrate correctness
  fixes outside the unchanged reviewed corpus.
- Nested background syntax and downstream extensions of pipeline/redirect
  constraints cannot bypass the canonical grammar restrictions. Backgrounding
  a sequence groups the whole sequence.
- Continuous capture checks cancellation between buffered chunks. Bootstrap
  validates descriptor roles before relocation, and an incomplete capsule
  connection cannot monopolize guardian takeover.
- Early child stdin closure preserves its output and exit status, including
  an `EPIPE` raised while closing the parent's buffered input handle.
- Publication leases reject deferred sealing or committing after callback
  expiry and wait for already claimed operations before cleanup.

## Dependency decisions

`singletons` core, `cryptohash-sha256`, `base64-bytestring` and Rust `trybuild`
replace local machinery or support bounded witnesses. Both GHC dependency lanes
were built before migrating consumers. Base64 decoding requires canonical
padded re-encoding; SHA-256 preserves lowercase hexadecimal identities.

`typed-process` was evaluated but not adopted. Its asynchronous waiter can reap
a fast-exiting leader before the caller captures the process-group identity.
The shared runner therefore captures that identity directly before starting
its waiter, with bracketed `async` stream ownership. The
[implementation decision](../superpowers/plans/2026-09-23-typed-architecture.md#feasibility-ruling-during-implementation)
records the upstream API limitation and regression evidence.

## Evidence and open gates

Raw commands, streams, generated scripts, compiler diagnostics and binary/input
hashes are retained locally under `artifacts/typed-architecture/`. The final
[compact evidence record](../evidence/typed-architecture-2026-09-23.json) identifies
completed measurements separately from unavailable cohorts.

Linux execution, minimum-OS execution and the remote CI matrix are not established
by these Darwin checks. No `ccs-ci` execution was used. The three original frozen
arithmetic performance inputs are absent; their hashes remain recorded and no
replacement workloads may be presented as that cohort.

The two historical process-effect probes remain separate evidence gaps: their
command heads are rejected by both compilers, and the Bash handshake reference
times out because its helper sees EOF. Passing native process-substitution and
lifecycle suites do not replace those probe results. Actual runtime/Fish image
counts on Darwin cover unprotected dynamically linked images retaining the
instrumentation environment, not all operating-system process launches.
