# Bounded performance diagnosis

Read-only review of `performance-final/report.json` and its immutable generated
files/providers. No source edits, broad retests, or remote activity. Formal
process-launch measurements remain unavailable; static invocation structure is
not reported as traced launch counts.

The fourteen locally comparable common workloads regress from aggregate sample
median154.864ms to1013.566ms (6.545x). Their generated programs each contain one
runtime --describe check and no Session owner. Typical one-output programs take
68–71ms vs baseline10–11ms; untaken output in double-bracket-eq takes48.89ms;
echo-echo with two executed outputs takes90.36ms. Thus repeated per-command
preflight or session supervision is not the main cause in this cohort. Fixed
native entry, one compatibility probe, and per-output runtime invocation dominate.

A short serial diagnostic batch was explicitly coordinated with root while all
other lanes were idle. Seven measured samples after three warmups, alternating
operation order, use the frozen report runtime (same verified product hash).
Script/data: `/tmp/monk-runtime-latency.py`, `/tmp/monk-runtime-latency.json`.
These are diagnostic medians, not a replacement acceptance timing protocol:

| Existing operation | Median ms |
| --- | ---: |
| runtime --describe | 24.10 |
| runtime descriptor-state | 23.99 |
| runtime write-builtin, empty output | 23.75 |
| runtime write-builtin, one byte | 23.94 |
| runtime raise-signal13 | 9.86 |
| runtime exec-site true | 13.73 |
| Fish builtin true | 9.04 |
| runtime launch Fish builtin true | 23.89 |

The identical native image with signal termination or tail-exec is much faster
than ordinary completion, strongly implicating ordinary GHC runtime teardown.
A ticker/shutdown floor is consistent with launch-true taking the same time as
--describe despite executing an additional Fish child. This is inference from
exit-path controls, not a sampled RTS stack trace.

## Recommendation

First consider explicit immediate process termination only at fully discharged
boundaries:

- DirectOutput: success after strict input validation and completed raw fd writes;
  error after its raw diagnostic write. No output Handle buffer, child or workspace
  remains. Preserve the existing real SIGPIPE disposition and signal path.
- --describe: after explicitly flushing the complete stdout description. A flush
  error must retain the existing failure behavior; do not discard partial writes.
- Launch: ordinary status only after child wait, signal-handler restoration and
  any wrapper-directory bracket cleanup have returned. Never move termination
  inside those brackets or bypass asynchronous child lifetime rules.

These are narrower and better evidenced than compatibility-probe caching or a
new persistent writer protocol. They can plausibly remove substantial observed
exit overhead, but require implementation, semantic regression checks and a new
serial measurement before claiming a speedup. Do not use a blanket Main-level
immediate-exit path for Session/Child/capsule ownership or error unwinding.

A subsequent all-stdio-open launch tail-exec could remove an owner process, but
needs independent signal/entry-lifetime proof. Avoid skipping --describe merely
because launch ran: selected runtime providers can differ, and sourceable/deferred
boundaries need their own capability checks. Authenticated same-provider reuse
would be a separate design and does not address most per-writer cost.

Even successful teardown optimization leaves native process startup and exact
per-output boundaries. These data do not justify predicting the common-cohort
+10% target will pass. Persistent primitive transport or more batching would be
larger changes, and batching cannot cross observable status/control/error/output
boundaries without new proof. Neither closed-stream fidelity nor real SIGPIPE
should be waived for timing. All diagnostic processes have completed.
