# Translator Audit (2026-03-22)

This audit reviews Monk against the goal of a principled Bash to Fish transpiler. The standard used here is strict:

- `exact`: direct lowering plus runtime evidence on the current suite
- `best-effort`: warnings, shims, raw pass-through, or documented semantic approximation
- `unsupported`: warning-only or strict-mode failure with no semantic lowering
- `unverified`: implementation exists, but current evidence is mostly syntax-level or too narrow

The sources for this audit were:

- `docs/design/translator-todo.md`
- `README.md`
- the translator modules under `src/Language/Fish/Translator/`
- the current unit, golden, property, integration, and real-world tests
- the GitHub Actions workflow in `.github/workflows/ci.yml`

## Prioritized Findings

### P0: Semantic parity was not previously exercised in CI

Before this audit, CI ran `cabal test all` without `fish` installation or `MONK_INTEGRATION=1`, so the bash-vs-fish integration fixtures, output-equivalence properties, and generated real-world parity checks were skipped. That meant the most important semantic evidence existed only in local runs.

This is now fixed in `.github/workflows/ci.yml`.

### P1: Warning-only branches had little or no direct test coverage

The following paths existed in the translator but were not directly asserted before this audit:

- `shopt` ignored with a note
- delimiter/silent `read` handling and its diagnostics
- dynamic `set -o` / expanded `set` options
- malformed or unsupported `trap`, `shift`, `unset`, `declare`, `local`, and `export`
- unsupported arithmetic `for ((...))` init and increment fallbacks

These are now covered by direct unit tests for diagnostics and output shape.

### P1: Best-effort I/O shims had thin runtime evidence

Here-strings are non-trivial lowerings. Before this audit they were largely covered by translation-shape tests or incidental use in large fixtures. The suite now includes a focused runtime fixture for:

- simple here-string behavior

This follow-up pass also added runtime integration coverage for:

- side-effecting parameter expansion in command arguments
- side-effecting parameter expansion in redirection targets
- side-effecting parameter expansion in `case` switch expressions
- simple `<(...)` process substitution
- recursive literal `source` with argv and environment effects

The same pass exposed a real lowering bug in `<(...)`: process-substitution bodies were being rendered as empty commands and simple proc-sub words were incorrectly fed through IFS splitting. That is now fixed.

`read -d` remains best-effort because bash and fish diverge on some stdin-driven delimiter cases. `>(cmd)` now has a Linux-gated runtime fixture in CI; local macOS runs still skip that fixture because bash output redirection through `/dev/fd/*` is sandbox-restricted here.

### P2: Differential properties were too narrow

The old output-equivalence property generator only covered four script families:

- simple variable echo
- simple arithmetic
- array indexing
- uppercasing through a pipeline

The generator now also covers:

- argv round-tripping
- stdin-driven `read`
- temporary environment prefixes
- case/glob matching
- here-strings

This materially broadens the semantic surface exercised on every parity run.

### P2: Manual fish baselines are useful, but not evidence for Monk

The `test/fixtures/realworld/*.fish` files are hand-written comparison baselines. They remain useful, but they should not be cited as proof that Monk's generated output is correct unless the generated output is also executed. The README and test naming now make that distinction more explicit.

`neofetch` remains bake-off-only for Monk-generated output. It is too large and warning-heavy to count as normal automated evidence today.

`echo-args` is back in the generated-output integration set after making errexit lowering command-substitution aware by default. Monk now aims at Bash's default non-`inherit_errexit` behavior rather than exposing the old function-wide workaround.

## Capability Matrix

| Area | Status | Diagnostics | Current Evidence | Notes |
| --- | --- | --- | --- | --- |
| `set -e` / `pipefail` | best-effort | documented caveats; `nounset` warns | runtime integration on focused fixtures plus `realworld/echo-args` | default non-`inherit_errexit` command-substitution behavior is covered; background jobs and some compound lists still diverge |
| Subshells `(...)` | best-effort / unsupported | note in normal mode, failure in `--strict` | unit coverage only | environment isolation is not preserved |
| Side-effecting parameter expansion in args / redirections / case | exact on covered cases | no warning on supported forms | focused runtime integration fixtures | command arguments, redirection targets, and `case` switch expressions now have parity coverage |
| Arithmetic short-circuit / ternary side effects | exact on covered cases | no warning on supported forms | focused runtime integration fixture | edge cases still deserve expansion |
| Arithmetic `for ((...))` fallback paths | best-effort | warning comments | direct unit warning coverage | unsupported init/increment forms still degrade to comments |
| Arrays and 0-based to 1-based indexing | exact on covered cases | no warning | unit, property, and runtime evidence | one of the best-covered areas |
| `read -n/-t/-u/-a` | best-effort | IFS note for lossy cases | unit plus runtime integration | typed lowering exists, but splitting semantics still differ |
| `read -s` | exact on covered cases | no warning on supported forms | unit coverage | direct `--silent` lowering is in place |
| `read -d` | best-effort | delimiter warning on lossy cases | unit and translation-shape coverage | typed lowering exists, but runtime parity still diverges on some delimiter cases |
| Here-strings `<<<` | best-effort | no dedicated warning | focused runtime integration plus real-world incidental use | simple cases are now exercised directly |
| Process substitution `<(...)` | exact on covered cases | no dedicated warning | focused runtime integration fixture | a real body-lowering and no-split bug was fixed while adding coverage |
| Process substitution `>(...)` | best-effort | no dedicated warning | Linux-gated runtime integration plus translation-shape assertions | FIFO workaround is exercised in CI; local macOS runs skip the fixture because of sandbox restrictions |
| `readonly` / `declare -r` | best-effort | warning for missing readonly enforcement | unit diagnostics plus generated real-world parity | behavior is intentionally lossy |
| `shopt` | unsupported | warning note | direct unit diagnostics | lowered to `true`; no semantic emulation |
| `source` recursion with literal paths | exact on covered cases | warnings for recursive/non-literal variants | inline argv unit coverage plus runtime integration | simple literal recursive sourcing with argv/env effects is now exercised end to end |
| Background jobs / `wait` under `set -e` / `pipefail` | best-effort | no dedicated warning | failing fixture drafted, not yet gated | current fish behavior still diverges from bash on the drafted parity case |
| Non-literal `source` paths | unsupported | warning | no dedicated runtime evidence | intentionally left for manual review |
| `trap` | best-effort | warnings on unsupported option forms | unit diagnostics plus simple `EXIT` runtime integration | simple `EXIT` lowering now has end-to-end coverage; option-heavy behavior is not modeled |
| `coproc` | unsupported | warning / strict failure | unit diagnostics | correctly treated as unsupported |

## Concrete Test Backlog

The following gaps remain after the changes in this audit:

- Fix background-job parity under `wait`, `set -e`, and `pipefail`, then promote the drafted fixture into the gated suite.
- Expand exact-case runtime coverage for delimiter-heavy `read -d` combinations, or keep the remaining cases explicitly best-effort.
- Decide whether `>(...)` needs broader cross-platform evidence beyond the current Linux-gated fixture.
- Split out a reduced, automatable slice of `neofetch` if we want a generated-output regression target instead of bake-off-only coverage.

## Documentation Outcome

After this audit:

- `translator-todo.md` is still the implementation checklist
- this file is the fidelity matrix
- the README links to this audit and now describes the CI parity setup and the status of manual fish baselines more precisely
