# Direct native writer initial-stream counterexample

Requested bounded probe, 2026-09-22. No production edits. Exact environment is
`/tmp/monk-nixpkgs-environment`, pinned upstream Nix Bash5.3p9 and Fish4.6.
Probe script `/tmp/monk-direct-stdio-probe.py`, raw log
`/tmp/monk-direct-stdio-probe.log`. The existing built monk translates source
files in a fresh physical temporary directory; each reference/generated shell
receives identical descriptors. No runtime-generated Bash was involved.

`printf x` emits `builtin printf x`; `echo x` emits `builtin printf '%s'\\n x`.
With stdout closed immediately before shell exec, Bash exits1 with
`input.bash: line 1: printf: write error: Bad file descriptor` (respectively
`echo`) while Fish exits0 with no stderr. With stdout attached to a pipe whose
read end is already closed before shell launch, Bash terminates from SIGPIPE
(subprocess returncode -13); Fish exits1, both with empty stderr.

These are concrete admitted-profile observations: initial closed stdio and
ordinary pipe closure are allowed. They are not implementation introspection
and must not be retroactively excluded merely to preserve a no-helper metric.
Root owns the bounded strategy decision; no speculative redesign was made.
Successful standard-stream greeting tests do not close this counterexample.


## Bounded writer implementation and startup limitation

DirectOutput.hs now implements write-builtin from fully read NUL frames using
existing finite echoBytes/printfBytes, plus one-payload echo-bytes and fixed
raise-signal13. Raw fd writes avoid shutdown buffer retries; constructor-recorded
fd1/2 presence prevents writing reused RTS descriptors. Source errno diagnostics
return1; empty output remains successful; actual SIGPIPE is retained. New
runtime-test/direct-output.py was red at unsupportedoperation125, then passed
all primitive byte/1MiB/invalid-byte/metadata/closed-stream/signal cases with the
compiled implementation. Warning-clean typecheck and HLint pass.

The integrated helper now matches actual SIGPIPE for a readerless pipe. However,
a fresh Fish first-operation probe (`exec runtime --abi2 descriptor-state`) sees
mask7 after parent closes stdout, where the native process directly would see5.
A first-operation Python fstat probe identifies Fish's replacement as O_RDWR
/dev/null; Bash retains EBADF. No generated setup or substitution precedes the
probe. Therefore generated Fish cannot recover initial closed stdout from a
snapshot taken after Fish startup or distinguish it from an intentionally
supplied /dev/null. Root owns the pre-Fish entry decision; the compiled oracle
continues to fail this case rather than suppressing it.

Build6 passed. Broad4 passed955/959, with three stale capability/statistics
assertions and a newly exposed low fish_read_limit provider-lookup regression.
Those were reported to root. All four added nested/shared stderr cases passed
after runtime owner corrected duplicate closure and direct script argv restore.
No final full-suite success is claimed at this checkpoint.

## Native entry closure — 2026-09-22 final local revision

The user selected native standalone entry rather than excluding initially closed
streams. `monk-runtime --abi 2 launch FILE [ARGS...]` captures descriptor presence
before Fish startup. Its private wrapper is used only when descriptors are absent;
ordinary execution remains direct Fish lowering with bounded writer primitives.
Sourceable output retains caller scope and now rejects either reserved launcher
marker at entry and deferred function calls before source effects.

Canonical GHC9.14.1 build-all and the incremental Plan relink passed. Six golden
files were regenerated and reviewed: assignments unchanged; the other five gained
only the local `fish_read_limit 0` before the first runtime pathname capture.
The final fresh main suite passed **968/968 in 71.38s**, including 100 recursive
composition trials and the four sourceable marker regressions. Log:
`/tmp/monk-nixpkgs-broad7.log`. The initial marker regressions needed explicit
`command env` dispatch and a declared function export; their corrected contracts
passed the focused four tests before the final broad run.

`runtime-test/direct-output.py RUNTIME MONK` passes runtime primitive bytes,
empty outputs, source errno diagnostics and actual SIGPIPE, followed by compiled
writer comparisons through the launcher for ordinary, closed stdout, closed
stdout+stderr and no-reader pipe cases. Log:
`/tmp/monk-launch-direct-output-final.log`.

An independent managed-publication probe passes all eight stdio masks after
unlinking the original provider and removing its bin directory from PATH (with
an explicit assertion that monk-runtime is no longer discoverable). It launches
the published loader using the captured runtime under a directory ending in a
newline, comparing exact status/stdout/stderr for a function and subshell with
arguments. Script `/tmp/monk-managed-launch-removal.py`; log
`/tmp/monk-managed-launch-removal.log`. The earlier optional launcher test lacked
`--managed`, so its previous relative-artifact claim was overstated; the runtime
owner is correcting that durable test to include both loader and generation entry.

These checks close the concrete initially-closed stdout and SIGPIPE mismatches.
They are local aarch64-darwin/Nix Bash5.3p9/Fish4.6.0 evidence, not Linux execution
or final packaging/performance acceptance evidence.
