# SDD ledger — plan: docs/superpowers/plans/2026-09-22-portable-exact.md

Base: c2bd371. Isolated managed worktree, detached HEAD. User's original monk.code-workspace and .devenv caches untouched; requested devenv config copied into worktree.

## Preflight

| Tasks | Shared interface / finding |
|---|---|
| 1/2 | haskell.nix project produces native/static executable derivations; runtime agent owns source, environment agent owns Nix. |
| 2/4 | ABI2/platform/descriptor primitives feed supervisor; supervisor follows portable foundation. |
| 3/5-10 | Native optimization shares Plan/Normalize; land isolated renderer/arithmetic work first, serialize semantic integration. |
| 4/5-9 | Supervisor protocol is common ownership boundary; real PID and descriptor semantics mandatory. |
| 6/7 | read -a depends on dense storage; coordinate before admission. |
| 8/4 | Endpoint lifetime must remain asynchronous, no synchronous temp-file fallback. |
| 9/3 | Callback effects constrain status/native eligibility. |
| 10/normalization | Only statically known finite Bash bodies; never send Bash text to Fish eval. |
| 11/12 | Historic frozen95 denominator preserved; additional cohorts separate; platform gaps explicit. |
| 1-12 | All task descriptions consistent with exact admission; native/static packaging does not imply runtime proof. |

Ruling: use parallel implementers only on disjoint owned paths, with shared interface integration serialized — developer delegation preference and task scale justify parallel independent work — conflicts require controller resolution.
Ruling: approved conversation plan is authoritative; no additional design approval gate — user explicitly requested implementation — material scope changes remain visible.

## Status

- 1: pinned haskell.nix/devenv, shared Cabal project and correct MCP launch configuration implemented; Nix provides all dependencies and test programs. Both supported compiler lanes validated locally.
- 2: ABI 2, portable descriptor/process ownership and target identity implemented. Copied Apple Silicon package executes with only Apple system dylibs; musl release definitions cover both Linux architectures. Linux execution remains user-deferred.
- 3: native scalar/control/function/external-pipeline lowering, status provenance, concise literals, flat standalone guards, child deduplication and integer batching implemented. Shared exact writer is required for source output; helper-free greeting and complete performance acceptance remain unmet.
- 4/5: native owner and private Fish evaluator implement pipelines, real background PIDs, waiting, signal status and normal-parent-completion lifetime; handshake and stream regressions pass.
- 6: ordered descriptor scopes, here input, byte read and finite prefix assignments implemented; scoped stream, diagnostic and source-origin regressions pass.
- 7: dense arrays/snapshots, composed splitting/pathnames and quote-aware parameter trimming implemented; precise sparse/associative and unsupported-context exclusions retained.
- 8/9: owned streaming process substitution and compiled standalone EXIT/ERR implemented, including independent status, live reads, scope and failure-site diagnostic metadata. Unsupported callback/endpoint contexts reject.
- 10: finite eval and immutable source contexts implemented; occurrence diagnostic spelling stays separate from canonical source identity.
- 11: final frozen95 comparisons yield default74/stable77 exact matches, zero admitted mismatches, with separate filesystem/caller/process/strengthened cohorts. Historical results and unavailable platform cases retain distinct identities. The runnable common14 performance limit fails; incomplete full cohorts and launch-count gates remain unverified.
- 12: local GHC/Fish, native protocol/publication/API, quality, Haddock, source-distribution and Darwin packaging evidence recorded. Final documentation closure binds the reviewed tree to the tested binaries; Linux/minimum-platform/performance/helper-free gates remain open.

User constraint: all source stays local. Do not test on ccs-ci or transfer source to any other host. Linux native execution is explicitly deferred/unverified. Earlier ccs-ci public tool bootstrap did not receive Monk source and did not run Monk tests.

Reference note: dependencies and test programs now come from locked upstream Nix packages. The canonical oracle is nixpkgs-reference.bashNonInteractive 5.3p9, reporting Bash 5.3.9. The removed private Bash derivation had different Darwin iconv behavior; its runs remain historical. Binary hashes and raw behavior probes identify the current reference.


## Final review corrections after first full run

The Nix reference main suite passed 955/955 before new nested closed-stderr
regressions exposed an evaluator invocation bug. Direct script launch now
preserves the child snapshot argv scope and applies each descriptor closure
once. Shared-shell builtin diagnostics retain original stderr; subshells and
asynchronous/pipeline stages begin their own diagnostic ownership.

Independent native output probes then disproved Fish builtin writer eligibility
for the full I/O contract: closed stdout returned silent success instead of
Bash status1, and broken pipes returned1 instead of actual SIGPIPE. Exactness
remains the default. A shared bounded writer and signal termination path are
being verified; the requested helper-free greeting criterion remains an explicit
plan conflict rather than a false pass. No successful-output-only contract or
approximate backend was introduced. Standalone namespace guards are now flat
as well as binding guards; sourceable continuation ownership is preserved.


## Native entry decision

The user explicitly approved native standalone launch for full stream fidelity.
Canonical execution is `monk-runtime --abi 2 launch FILE [ARGS...]`; sourceable
output keeps its declared Fish caller boundary. The launcher captures stdio
before Fish startup, stages a private compiled wrapper only for missing streams,
and keeps wrapper-relative managed paths bound to the original artifact.
All standalone plans declare `NativeLaunch`, including silent native bodies,
so managed publication captures the necessary immutable provider. No body helper
or runtime preflight is emitted solely for this entry requirement.

Final validation must replace pre-launcher snapshot receipts. The GHC 9.12.2
snapshot built successfully and reproduced only the same four main-suite failures
as 9.14.1: three writer-statistic expectations and low Fish capture-limit setup.
Those fixes and additional scope/metadata regression cases are now implemented.


## Final corrective checks and evidence closure

The canonical suites pass980/980 on GHC9.14.1 and9.12.2. Locked Fish4.9.3
passes the full prior978 and both later environment-framing regressions. Native
launcher/writer/callback suites, publication and public API boundary checks pass.
The final harness correction replaces newline-delimited environment parsing with
NUL framing, preserving multiline/equals/empty values and exact stdout. Both new
regressions failed before the fix; reinjecting the original DEVENV_CMDLINE carrier
now passes the complete suite. Translator/runtime bytes did not change.

The old publication assertion that literal output needs no runtime was stale
against the closed-stdout/SIGPIPE correction. It now checks the exact writer
requirement and native entry; a separate silent assignment requires no body
helper. This does not close or remove the approved helper-free greeting gate.
Final source/distribution and fingerprint bridges explicitly identify test-only
changes rather than relabeling earlier whole-input receipts.

Fresh default/stable comparisons and package evidence are linked from the
roadmap and portable verification report. The original checkout still has only
its five initial modified/untracked paths. No source transfer, ccs-ci testing,
commit, merge, publishing or remote Git operation was performed by this work.


## Completed-operation shutdown optimization

Serial final benchmarking exposed a 6.545 times common14 regression before
teardown optimization. Bounded diagnostic timings identified roughly14ms of
ordinary runtime shutdown cost at leaf boundaries. DirectOutput now exits only
after complete raw writes/diagnostics, describe only after explicit successful
stdout flush, and Launch only after child wait and cleanup brackets complete.
Generic/session ownership and signal termination are unchanged. Partial-reader,
closed-stream, SIGPIPE, allstdio-mask, SIGTERM cleanup and background-survival
checks pass; the canonical980 suite and fresh native/compiler/Fish/package checks
also pass. The translator executable is unchanged.

The final serial frozen run records baseline171.476ms versus candidate621.447ms
aggregate medians on the same14 runnable common fixtures:3.624times slower than
baseline. Candidate time decreased38.7% from the first run's1013.566ms, but the
10% limit remains FAILED on the runnable subset. Two common baseline programs
cannot execute on Darwin; original arithmetic3 inputs are missing; targeted
cohorts and process-launch reductions are unverified. Every measured sample still
matches Bash. Both comparison contracts remain74/95 and77/95 with zero admitted
mismatches. The first benchmark and package are retained as separate snapshots.

This implementation does not claim full plan acceptance: Linux/minimum-platform
execution, performance and helper-free greeting requirements remain open. Final
archive byte verification and documentation closure are recorded separately from
prior full source-distribution build/install and fresh optimized runtime builds.
