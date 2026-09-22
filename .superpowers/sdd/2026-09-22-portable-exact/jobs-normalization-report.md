# Owned job normalization

## Implemented

Standalone background commands normalize into owned child regions. Wait normalizes typed word operands and last-background PID becomes a scalar with an empty prelaunch value. Sourceable session operations reject. The original draining pipeline admission stays native; standalone pipelines outside that envelope use the supervised pipeline node.

Wait accepts only provably numeric scalar forms, including stored PID facts, with optional literal leading `--`. Unproved strings, job specifications and options reject. Wait inside finite eval rejects until nested source diagnostics can be represented exactly. Runtime owns actual PID validation, wait status and operand diagnostics; renderer owns session materialization.

## Evidence

Before implementation a direct regression driver against the prior CLI demonstrated four admission failures for background, wait, last PID and a nondraining builtin pipeline. After normalization and root/runtime integration, canonical `cabal build all --builddir=dist-ghc9141` passes under GHC 9.14.1.

Canonical focused integration: **164/164 pass**, using Bash 5.3.9 and Fish 4.6. Owned background cases verify selected exit status, operandless and multiple-PID wait status, scalar isolation, caller-local snapshots, launch status, prelaunch empty PID, and null stdin for asynchronous noninteractive commands. Sourceable jobs reject. A 200KB builtin writer into `head -c 1` verifies early-closing pipeline byte/status/stderr behavior and the producer continuation boundary. Existing child argv/export/invalid-byte/large-frame isolation regressions also pass. Log `/tmp/monk-canonical-focused.log`.

Broad canonical main integration is running. Runtime agent is adding exact invalid-PID diagnostics; those edge cases need dedicated follow-up tests before claiming that diagnostic envelope. No commits. Root owns audit/todo integration and plan/materialization; runtime agent owns supervisor implementations.

Canonical broad snapshot: **773/780 pass**. Six stale fixture-policy exclusions were promoted without changing source bytes; their focused canonical rerun passes **6/6** (five background integrations plus structural pipeline golden). The remaining failure is the independent expected Bash stdout for an out-of-range Unicode echo escape; canonical custom reference Bash and nixpkgs Bash have different behavior despite reporting version 5.3.9. Root/build agent own that reference-runtime investigation. Logs `/tmp/monk-canonical-broad.log` and `/tmp/monk-canonical-promoted.log`.

Later canonical build-all passes with the input/trap/word/prefix frontend integrated. Focused **98/98** arrays/input/trap tests pass; array overflow rejection and additional vector snapshot checks are green. Root resolved the Unicode echo oracle by selecting the pinned custom Bash build configuration. The comprehensive final run is still pending recursive-composition/runtime process-owner stabilization. See `input-traps-expansion-report.md`.
