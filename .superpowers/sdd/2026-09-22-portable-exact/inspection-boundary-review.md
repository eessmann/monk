# SHLVL inspection boundary and static review

The external SHLVL probe is a concrete raw-output counterexample, classified as **unsupported shell-depth inspection**, not an exact match. This follows the approved plan's exclusion of implementation-process introspection and the existing execution profile's shell-introspection exclusion. Normalize also rejects direct source use of SHLVL. The final execution-profile documentation should explicitly say that the exclusion includes external programs inspecting SHLVL or other implementation shell identity/depth state; hiding the access inside `printenv`, `env` or another program does not make it an ordinary scalar input.

This boundary does not waive normal exported-variable preservation, explicit job PID/$!, wait/status behavior, user descriptor effects or process-lifetime requirements. In particular, stripping an ordinary user-provided private-name environment variable without a pre-effect guard was a separate environment bug. Root has added that guard in source; it still requires final executable validation.

## Concrete probe

With initial environment `SHLVL=0`, exact Bash5.3.9/Fish4.6.0, C locale and the same PATH, translate/run:

```bash
/usr/bin/true &
wait
python3 -c 'import os; print(os.environ.get("SHLVL"))'
```

Bash invoked with a source filename exposes `1\n` to the external program; the captured candidate exposed `0\n`, both exit0. The review driver is `/tmp/monk-review-probes.py`; it additionally demonstrated the separately reported private ambient-name issue and inherited-fd3 issue. Its first sandboxed run failed because Unix sockets were disallowed; the authorized local escalated rerun produced the semantic observations.

Independent canonical Bash checks distinguish launch shapes:

| Source shape | `bash script-file` | `bash -c source` |
|---|---|---|
| One external SHLVL observer | 1 | 0 |
| Background true, wait, final observer | 1 | 0 |
| EXIT trap, final observer | 1 | 1 |

Thus the earlier statement that background+wait alone necessarily disables Bash's final-exec optimization was too broad. Incrementing the evaluator environment might fix this file-launch example but would not prove the general process/exec-sensitive observation exact. No broad SHLVL projection fix was made. Keep the counterexample separate from the frozen95 exactness totals and from ordinary environment preservation tests.

## Static checks

Read-only Ormolu0.8.0.2 check: two files need formatting, `scripts/Bakeoff/Runner.hs` and `src/Language/Fish/Translator/ArithmeticDiagnostic.hs`. None of the144 checked files changed during that run.

Read-only HLint3.10 check across145 Haskell files: unused LambdaCase in `src/Language/Fish/Pretty/Pattern.hs`; use-isDigit in `src/Language/Fish/Pretty/Expr.hs` and two places in `src/Language/Bash/Plan/Normalize.hs`. Normalize and PlannedInput changed during the check, so their final static evidence remains pending. No fixes were applied to another owner's files. Logs: `/tmp/monk-review-ormolu.log` and `/tmp/monk-review-hlint.log`.

The approved Child/Capsule POSIX-spawn conversion was then built by the runtime owner. An independent copied-runtime run of `test/native/child-transport.sh` passed all six groups under the exact reference shells: ordinary/extended transport, signals/descriptors, initial descriptor snapshots, streaming warning, and missing original stdin. Log: `/tmp/monk-review-spawn-transport.log`. The runtime owner separately reported portable/session/descriptor suites passing. Full final-tree and packaged execution remain separate gates.
