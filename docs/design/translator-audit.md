# Translator Audit (2026-04-30)

This audit evaluates Monk as a conservative Bash-to-Fish migrator.

Status labels used here:

- `exact`: direct lowering plus focused runtime evidence on the current suite
- `best-effort`: lowering exists, but semantics are intentionally approximate or warning-driven
- `unsupported`: warning-only or strict-mode failure with no meaningful lowering
- `unverified`: implementation exists, but evidence is still too thin to promote confidence

Current evidence used for this audit:

- the translator implementation under `src/Language/Fish/Translator/`
- the public API and diagnostics layers under `src/Monk/`
- the current unit, golden, property, integration, and real-world tests
- the bake-off runner under `scripts/`
- passing local `cabal test` and `MONK_INTEGRATION=1 cabal test` runs with 257 tests
- a dedicated Ubuntu CI step for the Linux-gated helper-backed `>(...)` fixtures
- a successful local `cabal bench` run
- the current bake-off workflow and documentation

## Current Assessment

Monk is now in a better place to be judged as a correctness-first migrator rather than a sprawling feature checklist.

The biggest correctness issue in the previous phase was subshell drift across contexts. Statement lowering already used a best-effort `begin ... end` strategy, but status-context subshells were still being rendered through `fish -c`, which lost parent variables and generated helpers. That bug is now fixed.

The current subshell policy is consistent:

- normal mode emits the stable `BestEffortSubshell` warning and lowers to non-isolating `begin ... end`
- `--strict` fails on subshells in statement, status, and command-substitution contexts
- focused runtime fixtures now cover parent-variable access, exact `read -d`, and pipefail inside status-context subshells

The diagnostics model is also materially stronger than before:

- warnings are typed (`WarningCode`, `WarningSeverity`, optional detail, optional range)
- `Monk.Translation.Types` is now the single public owner of translation config, warning metadata, and warning text/severity mapping
- CLI and reporting layers render user-facing text from those typed diagnostics
- tests now assert stable warning codes/severities directly instead of relying on string heuristics
- warning severity and default-message metadata now share one descriptor table, with `allWarningCodes` exposing the public code list for contract checks

Helper emission is likewise in better shape:

- pipefail, background runtime, exact `read`, and helper-backed `>(...)` now go through one helper registry keyed by helper ID
- helper deduplication has direct unit coverage

## Capability Matrix

| Area | Status | Diagnostics | Current Evidence | Notes |
| --- | --- | --- | --- | --- |
| `set -e` / `pipefail` | best-effort | warnings on unsupported option surfaces; no warning on covered helper paths | focused runtime integration, background wait fixtures, properties, and real-world `echo-args` coverage | grouped `&&` / `||`, negated pipelines, and conditional enable/disable boundaries now have focused fixtures, but the area is still conservative overall |
| Subshells `(...)` | best-effort / unsupported in `--strict` | stable `BestEffortSubshell` warning; strict-mode failure | unit plus focused runtime integration on status-context regressions | environment isolation is still not preserved |
| Command-substitution subshells | best-effort / unsupported in `--strict` | stable `BestEffortSubshell` warning; strict-mode failure | unit coverage | no longer silently collapse to `true` |
| Side-effecting parameter expansion in args / redirections / `case` | exact on covered forms | no warning on covered forms | focused runtime integration plus unit coverage | one of the strongest semantic areas now |
| Arrays and 0-based to 1-based indexing | exact on covered forms | no warning | unit, property, and runtime evidence | stable area |
| `read -d` covered helper path | exact on covered forms | no warning on covered helper-backed forms | focused runtime integration plus unit coverage | covered surface includes empty delimiters, arrays, multi-variable assignment, supported mixed flag clusters, and numeric `-u` helper-backed reads |
| Residual `read` fallback surface | best-effort | `ReadIssue` warnings | direct unit coverage | no-var delimiter reads, non-numeric `-u`, and unsupported clusters remain warning-driven |
| Here-strings `<<<` | best-effort | no dedicated warning | focused runtime integration | simple cases are covered directly |
| Process substitution `<(...)` | exact on covered forms | no dedicated warning | focused runtime integration | current simple surface is in good shape |
| Process substitution `>(...)` | exact on covered Linux fixtures / best-effort overall | no dedicated warning | helper-backed lowering, unit coverage, Linux-gated fixtures, dedicated Ubuntu CI selector | the covered fixture set is the only exact surface once the Ubuntu evidence step passes; broader async/FIFO cases remain conservative |
| Recursive literal `source` | exact on covered forms | warnings on unsupported/non-literal variants | source-graph unit coverage, relocated bundle coverage, plus runtime integration | cwd-relative and parent-relative resolution are both exercised, and `--recursive --sources separate --output FILE` now rewrites literal children relative to the emitted bundle |
| Background jobs / translated `wait` | exact on covered translated-wait surface / best-effort otherwise | warning on PID-specific `$!` follow-ons | focused runtime integration | `kill $!`-style PID assumptions remain manual-review territory |
| `trap` | best-effort overall / exact on covered `EXIT`, reset, and normalized real-signal forms | typed warnings for pseudo-signals and unsupported option surfaces | unit diagnostics plus simple `EXIT` runtime integration | option-heavy behavior is still conservative, and pseudo-signals no longer lower to invalid fish handlers |
| `readonly` / `declare -r` | best-effort | stable readonly warning | unit coverage plus incidental runtime evidence | fish has no readonly enforcement |
| `shopt` | unsupported | stable warning | direct unit coverage | lowered to `true` |
| `coproc` | unsupported | stable warning / strict failure | direct unit coverage | intentionally unsupported |

## Highest-Priority Remaining Gaps

1. Residual warning-driven `read` branches should stay narrow and explicit until they gain exact evidence.
2. Helper-backed `>(...)` still needs the dedicated Ubuntu evidence step to pass before its covered fixtures should be marked closed in the active backlog.
3. `set -e` / `pipefail` should continue to be treated as conservative overall, even though grouped conjunctions, negated pipelines, and toggle boundaries now have focused runtime evidence.
4. Non-literal `source`, option-heavy `trap`, `shopt`, and `coproc` should remain warning-driven unless a clearly exact strategy is worth the complexity.

## Documentation Contract

The project now has a cleaner documentation split:

- `docs/design/translator-todo.md`: active engineering backlog
- `docs/design/translator-audit.md`: fidelity source of truth
- `docs/migration-guide.md`: user-facing cleanup guide for warning-driven areas

Those three documents should move together whenever a best-effort branch changes status.
