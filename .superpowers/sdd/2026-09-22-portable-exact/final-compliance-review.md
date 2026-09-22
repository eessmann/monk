# Bounded final plan-compliance review

Read-only review on 2026-09-22 while final receipts run. No tracked files were
modified. Reference: docs/superpowers/plans/2026-09-22-portable-exact.md.
This is a scope/claims review, not another exhaustive semantic proof.

## Findings

No new concrete admitted semantic counterexample was established. The reviewed
implementation preserves the planned architectural division: public translation,
source-graph and generated-output APIs expose execution strategy and structural
statistics; DirectExecution permits bounded primitives, while NativeSession
selects SupervisedExecution. Native lowering is a proof-based specialization of
the typed plan and falls back to the general materializer. Session lowering
execs the owner, preserves generated Fish control flow, and separates framed
control from user streams. Finite eval, source bodies and trap handlers are
parsed during translation rather than interpreted as Bash at runtime.

One minor readability/completion qualification remains. Plan Task3 requests flat
standalone guards. Binding guards are flat (Translator/Plan.hs1462–1477), but
privateGuardsWithCaptured (1423–1435), used by standaloneGuards (1450–1452),
still nests two namespace checks around the full body. The assignments golden
shows that concrete shape. The roadmap's line19 phrase “flattened standalone
guards” is broader than the implementation; “flat binding guards” is precise.
This is not a demonstrated semantic defect and does not justify disrupting the
frozen final receipt. Any documentation correction should follow the receipt.

## Explicit implementation boundaries, not failed validation

The audit/profile enumerate restrictions beyond broad feature names: owned dense
arrays and statically safe writes; no preexisting ambient array-name binding;
standalone-only session features; literal read flags and owned extra descriptors;
IFS-only read prefixes and restricted ordinary-command prefixes; nonescaping
process endpoints; pure/literal EXIT/ERR handlers with no directory-operation
combination; finite eval excluding unmapped nested diagnostic forms; immutable
function sources requiring compatible call-time facts. These are represented
by explicit normalization rejection paths, not silently accepted approximations.
They should continue to be described as initial admission envelopes rather than
complete Bash feature support.

Arbitrary runtime Bash, ENOEXEC implicit shell interpretation, interactive jobs,
sparse/associative arrays and shell-depth/private-process introspection remain
excluded. The missing/permission-change executable cases are correctly retained
as exact obligations, not retroactively removed by a dependency promise. The
SHLVL mismatch is explicitly identified as an unsupported observation. Ordinary
exported data, real $!/wait PIDs and user descriptor effects remain obligations.

## Acceptance gates still needing evidence

The approved plan lines9 and34 require packaged artifacts executed on all three
native targets, static Linux inspection and Apple-only Darwin dependencies.
Release definitions and an inspection tool exist; definitions are not execution
evidence. Linux execution is user-deferred, and minimum-OS/kernel floors remain
unverified. The final Darwin package, GHC9.12, moving-Fish, sdist/publication/API
and whole-input receipt outcomes must be recorded from the independent agent's
actual final run, not inferred from the 955-test GHC9.14/Fish4.6 pass.

Plan lines23 and34 also require final frozen95 comparison and performance gates.
The cohort manifest preserves the 95 denominator and separately records stronger
filesystem/caller/process cases. Final candidate comparison is still pending in
the current documentation. The original arithmetic3 source inputs are absent,
the historical native baseline cannot run on Darwin, and process-launch tracing
is unavailable. Therefore the full common/arithmetic <=10% regression and
fewer-launches acceptance requirements are not established; surviving subsets,
static call counts or historical Linux timings cannot close those gates.

These limitations are already stated in portable-runtime-verification.md40–79
and the current comparison appendix. Final reporting must preserve those labels
and refresh pending outcomes only after receipt completion. Historical counts
are dated and distinguished from fresh candidate results. I found no additional
unqualified “all targets verified” or “performance gates passed” claim in the
reviewed current sections.

## Evidence limits

The prior integrated pass is 955/955 with pinned Nixpkgs Bash5.3p9, Fish4.6 and
GHC9.14.1 on aarch64 Darwin. This review did not rerun programs or infer Linux
behavior from that pass. The precise runtime counterexamples found earlier
(read IO errors, mutable executable failure, removed cwd and inherited ignored
signals) have dedicated regressions and were corrected before this review;
no passing path was reopened without a new concrete witness.


## Subsequent corrective window

After the read-only review, root completed namespace guard flattening using
shared predicates and standalone exit125 checks; sourceable continuations stay
nested. Structural review found no issue, and refreshed complete-file goldens
pass. This closes the minor readability qualification above.

A later requested initial-stream probe demonstrated an additional admitted
direct-native writer mismatch, recorded in native-stdio-review.md. Separately,
new nested closed-stderr integration witnesses exposed a child-launch125 result.
Build5 is green and broad3 passes956/959; the three newly added nested read/wait/
external cases fail, while shared-group fallback and all existing tests pass.
These concrete failures are active correction work and supersede any inference
that the prior955-test pass established complete final correctness.

## Launcher integration independent review

Reviewed Launch ownership and cleanup, NativeRuntime.entryDirectory mapping,
NativeLaunch requirement emission, sourceable deferred wrappers, managed provider
reuse and the updated owned documentation. No additional actionable production
finding remains after the following concrete issue was closed:

- Sourceable external execution stripped ambient MONK_LAUNCH_ORIGINAL/WRAPPER
  without a matching entry guard. Both entry and deferred calls now reject these
  reserved markers before user effects. Four explicit caller-contract tests pass.

NativeLaunch is required for all standalone products, including silent bodies,
so managed publication captures the provider even when no primitive helper is
emitted. Launch-only metadata does not add body runtime setup. Sourceable products
do not acquire standalone launch semantics. Entry-directory substitution only
applies when the current filename equals the private wrapper filename; nested
source members retain their own immutable artifact base. Launcher markers are
removed from user external command environments, and caller-provided markers are
rejected at the relevant boundaries instead of silently discarded.

The temporary wrapper uses begin/end rather than a Fish function or source call,
which preserves global argv snapshot restoration. Runtime owner tests cover all
stdio masks, signal termination/cleanup and background survival. Independent
managed publication with original provider removed and PATH withdrawn passes all
eight masks via the captured launcher. The final broad main suite passes968/968.

Documentation distinguishes canonical native entry from raw Fish invocation and
records the helper-free greeting plan deviation explicitly; it does not silently
remove closed streams from the admitted contract. The historical approved plan
is unchanged. Final matrix/package/receipt and Linux validation remain separate
acceptance evidence; this bounded review does not assert those gates complete.

## Source occurrence diagnostic spelling correction

The durable managed launcher test exposed a concrete mismatch hidden by ordinary
successful output: a literal `./module.bash` diagnostic used its canonical
absolute filename. The fix keeps canonical parser positions, snapshot/cache keys,
source stacks, cycle checks and function definition identities unchanged. A
separate occurrence origin is supplied with each immutable SourceDocument and
used only for planned runtime ranges and arithmetic error sites. Root invocation
spelling is also retained independently of its canonical snapshot.

The internal resolver returns canonical identity plus lexical selected spelling;
the existing public resolver still returns only canonical identity. Explicit
operands preserve dots/symlinks; PATH preserves selected directory spelling;
empty PATH components produce `./name`, while failed-search cwd fallback produces
`name`. Root Nix Bash probes independently established these rules.

Three red Bash comparisons established mismatches for repeated aliases, a later
function call and arithmetic errors. The corrected 24-test source graph group
passes, including ten new tests for alias redefinition without duplicated
snapshots, relative PATH, empty PATH, cwd fallback, source/root symlinks, and
function/arithmetic origins. Build-all, HLint, Ormolu and diff checks pass.
Logs: `/tmp/monk-source-origin-red2.log`,
`/tmp/monk-source-origin-green2.log`,
`/tmp/monk-source-origin-green-build.log`.

The source test helper now supplies its declared PATH prefix to both reference
and generated executions, keeping resolver and execution environments aligned.
No runtime source changed in this correction. Root separately identified and
owns an ERR-handler diagnostic line correction; broad snapshot results before
that change are not final whole-worktree acceptance evidence.

## Callback diagnostic context independent review

Reviewed the subsequent Plan/Traps failure-site diagnostic context correction.
No actionable status, positional-argument or closure-scope finding. Callback
origin/line bindings are local in the dynamic wrapper; exactly two metadata
arguments are removed before forwarding the unchanged user argv. ERR saves and
restores source status, and its scoped active guard prevents recursive callback
execution. Nested handler compilation restores its prior materialCallback flag.
All admitted trap output takes the supervised writer branch that consumes the
new diagnostic arguments; explicit exit receives the active source origin while
implicit EOF uses the owning entry origin.

Six independent pinned Nix Bash/native-launcher comparisons pass for multiline
ERR/EXIT closed-stdout error lines, empty positional arguments and status,
explicit exit in a function, failed-function caller ERR location, and replacing
EXIT from ERR. Script `/tmp/monk-callback-independent.py`; output
`/tmp/monk-callback-independent.log`. No tracked files were edited by this review.
