# Translator Audit (2026-04-16)

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
- the current local 220-test `MONK_INTEGRATION=1 cabal test` baseline, which includes the curated real-world fixtures `argparse-mini`, `envfile-preview`, `path-filter`, and `semver-normalize`, plus direct source/harness/bake-off seam coverage

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

This follow-up pass also added runtime integration coverage for:

- translated background jobs / `$!` / `wait` under `set -e` and `pipefail`
- generalized covered `read -d` forms, including empty-delimiter, array, multi-variable, non-whitespace-IFS, and mixed-flag slices

The old narrow `read -d` helper has now been replaced with a generalized helper-backed exact path for covered delimiter-driven reads and numeric `-u` helper-backed reads. Remaining no-var delimiter reads, non-numeric fd values, and unsupported flag clusters stay best-effort. `>(cmd)` now also uses a generated helper and has a broadened Linux-gated fixture surface, but local macOS bake-off runs still skip those fixtures because Linux remains the semantic source of truth for acceptance.

The architecture pass that followed also exposed a real recursive-source resolution bug in the new public `Monk.Source` layer: literal source discovery only tried the parent source file directory and missed working-directory-relative paths such as `source test/fixtures/...`. The source graph now tries cwd-relative resolution first and then falls back to the parent source file directory, with direct unit coverage and the existing runtime integration fixture keeping that path exercised.

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

`neofetch` itself remains bake-off-only for Monk-generated output. It is still too large and warning-heavy to count as normal automated evidence today. A reduced `neofetch-mini` slice derived from `get_args()` now lives in the generated-output integration suite.

`echo-args` is back in the generated-output integration set after making errexit lowering command-substitution aware by default. Monk now aims at Bash's default non-`inherit_errexit` behavior rather than exposing the old function-wide workaround.

## Capability Matrix

| Area | Status | Diagnostics | Current Evidence | Notes |
| --- | --- | --- | --- | --- |
| `set -e` / `pipefail` | best-effort | documented caveats; `nounset` warns | runtime integration on focused fixtures, background wait fixtures, and `realworld/echo-args` | default non-`inherit_errexit` command-substitution behavior and translated background wait paths are covered; some compound-list edge cases still diverge |
| Subshells `(...)` | best-effort / unsupported | note in normal mode, failure in `--strict` | unit coverage only | environment isolation is not preserved |
| Side-effecting parameter expansion in args / redirections / case | exact on covered cases | no warning on supported forms | focused runtime integration fixtures | command arguments, redirection targets, and `case` switch expressions now have parity coverage |
| Arithmetic short-circuit / ternary side effects | exact on covered cases | no warning on supported forms | focused runtime integration fixture | edge cases still deserve expansion |
| Arithmetic `for ((...))` fallback paths | best-effort | warning comments | direct unit warning coverage | unsupported init/increment forms still degrade to comments |
| Arrays and 0-based to 1-based indexing | exact on covered cases | no warning | unit, property, and runtime evidence | one of the best-covered areas |
| `read -n/-t/-u/-a` | best-effort overall | IFS note for lossy cases | unit plus runtime integration | plain lowering still exists for lossy branches, but numeric `-u` reads on the helper-backed exact path now have focused parity coverage |
| `read -s` | exact on covered cases | no warning on supported forms | unit coverage | direct `--silent` lowering is in place |
| `read -d` | exact on the covered helper path / best-effort otherwise | no warning on covered helper-backed forms; delimiter warning on lossy cases | unit plus focused runtime integration | covered forms now include empty delimiters, arrays, multi-variable assignment, non-whitespace IFS cases, and supported mixed flag clusters; no-var delimiter reads remain best-effort |
| Here-strings `<<<` | best-effort | no dedicated warning | focused runtime integration plus real-world incidental use | simple cases are now exercised directly |
| Process substitution `<(...)` | exact on covered cases | no dedicated warning | focused runtime integration fixture | a real body-lowering and no-split bug was fixed while adding coverage |
| Process substitution `>(...)` | best-effort | no dedicated warning | generated helper plus broadened Linux-gated runtime surface and translation-shape assertions | the helper-backed FIFO path is landed, but Linux runtime evidence is still the source of truth before claiming stronger parity |
| `readonly` / `declare -r` | best-effort | warning for missing readonly enforcement | unit diagnostics plus generated real-world parity | behavior is intentionally lossy |
| `shopt` | unsupported | warning note | direct unit diagnostics | lowered to `true`; no semantic emulation |
| `source` recursion with literal paths | exact on covered cases | warnings for recursive/non-literal variants | source-graph unit coverage plus runtime integration | simple literal recursive sourcing with argv/env effects is exercised end to end; resolution now tries cwd-relative paths first, then parent-source-directory fallback |
| Background jobs / `wait` under `set -e` / `pipefail` | exact on covered translated wait cases / best-effort otherwise | warning on PID-specific `$!` follow-on uses | focused runtime integration fixtures | Monk-managed job tokens now back translated `$!` and `wait`; PID-specific uses such as `kill $!` remain manual-review territory |
| Non-literal `source` paths | unsupported | warning | no dedicated runtime evidence | intentionally left for manual review |
| `trap` | best-effort | warnings on unsupported option forms | unit diagnostics plus simple `EXIT` runtime integration | simple `EXIT` lowering now has end-to-end coverage; option-heavy behavior is not modeled |
| `coproc` | unsupported | warning / strict failure | unit diagnostics | correctly treated as unsupported |

## Concrete Test Backlog

The following gaps remain after the changes in this audit:

- Keep the residual warning-driven `read` branches explicit: no-var delimiter reads, non-numeric fd values, and unsupported option clusters should either gain evidence or stay clearly best-effort.
- Record explicit Linux runtime evidence for the helper-backed `>(...)` surface before upgrading it beyond best-effort in this audit.
- Keep option-heavy `trap` forms and non-literal `source` warning-driven unless a precise exact strategy is worth the complexity.
- Keep the new public-source and bake-off seam tests in lockstep with future refactors so architecture cleanup cannot silently regress recursive sourcing or bake-off selection/report behavior.

## Documentation Outcome

After this audit:

- `translator-todo.md` is still the implementation checklist
- this file is the fidelity matrix
- the README links to this audit and now describes the CI parity setup, public module layout, and the status of manual fish baselines more precisely
- `docs/design/architecture.md` now records the module and Cabal-component boundaries behind the current layout
