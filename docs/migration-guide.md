# Monk Migration Guide

This guide covers the warning classes and best-effort areas that most often need manual cleanup after translating Bash to Fish.

## Reading Diagnostics

- Monk warnings are now structured. Treat the warning code and severity as the stable contract; rendered message text is for humans.
- Rendered diagnostics include that contract, for example `warning[ReadIssue][medium]`.
- High-severity warnings mean the generated Fish should be reviewed before use.
- If the translator is run with `--strict`, unsupported best-effort branches fail instead of emitting output.

## `set -e` / `pipefail`

- Treat Monk's `set -e` and `pipefail` lowering as conservative, especially around compound-list edge cases.
- The focused runtime suite now covers grouped `&&` / `||` bodies, negated pipeline status, and conditional enable/disable boundaries, so those specific branches are less speculative than before.
- Translated background jobs / `$!` / `wait` now use Monk-managed job tokens and have focused parity coverage, but PID-specific follow-ons such as `kill $!` still deserve manual review.
- Monk targets Bash's default non-`inherit_errexit` behavior inside command substitutions. If a translated substitution still needs bespoke control flow, rewrite it as explicit Fish `if` / `begin ... end` logic.

## `read`

- The exact helper-backed path covers the currently accepted `read` surface:
  - empty delimiters
  - arrays
  - multiple destination variables
  - supported mixed flag clusters
  - numeric `-u` helper-backed reads
  - no-variable reads through Bash-compatible `REPLY`
  - newline-delimited array and multi-variable reads
- Remaining warning-driven `read` cases still need manual review:
  - non-numeric fd values
  - unsupported flag clusters
  - unsupported option combinations
- If Monk still emits a `ReadIssue` warning, validate the translated parser against real Bash input instead of trusting the generated Fish blindly.

## Process Substitution

- `<(...)` has direct runtime coverage for simple cases, but larger pipelines should still be exercised in Fish.
- Covered stdout redirect-target `>(...)` forms now lower through a temp-file-backed block that preserves the producer status, honors parent `set -e`, and ignores the consumer status, matching the covered Bash behavior more closely than a plain pipeline.
- The current simple, pipeline, variable-sink, status-sensitive, `set -e`, and compound-consumer fixtures are the covered Linux evidence surface once the dedicated Ubuntu CI step passes.
- Argument-position output process substitutions emit `ProcessSubstitutionIssue` and remain manual-review surfaces until they have their own focused runtime evidence.
- If the translated output depends on streaming or asynchronous timing, prefer rewriting it as explicit `mktemp` / producer / consumer steps in hand-edited Fish.

## `trap`

- Covered `trap '...' EXIT`, named real-signal handlers such as `SIGINT`, numeric signal handlers such as `2`, and `trap - SIGNAL...` reset forms now lower without generating invalid fish.
- Numeric trap signals stay numeric to avoid assuming Linux signal-number mappings on other platforms.
- Bash pseudo-signals such as `ERR`, `DEBUG`, and `RETURN`, uncatchable signals such as `KILL` and `STOP`, and option-heavy forms still warn for manual review.
- When cleanup ordering matters, prefer an explicit helper function and `--on-process-exit %self` in hand-edited Fish.

## Subshell Isolation

- Bash subshells isolate variable, directory, and function-local side effects. Fish `begin ... end` does not.
- Monk now applies the same best-effort subshell policy in statement, status, and command-substitution contexts:
  - normal mode emits `BestEffortSubshell`
  - `--strict` fails
- If isolation matters, rewrite the block as an explicit helper function, separate script, or another structure that restores the required boundary.

## `shopt`

- Monk lowers `shopt` to a warning plus `true`; no Fish semantic equivalent is applied.
- Replace `shopt`-dependent logic manually with explicit Fish behavior or Bash-only compatibility guards.

## `readonly` / `declare -r`

- Fish has no direct readonly variable enforcement.
- If immutability matters, keep the value local to a narrow scope or move it into a function argument instead of relying on the translated `set`.

## Non-literal `source`

- Recursive translation only inlines literal source paths.
- Literal recursive source resolution now tries the working-directory-relative path first and then falls back to the parent source file directory.
- `--recursive --sources separate --output FILE` now emits a self-contained bundle rooted at the output path and rewrites literal child sources relative to that bundle.
- Dynamic source paths remain manual-review territory.

## Cleanup Workflow

- Run Monk and review the typed warnings, not only the rendered script.
- Re-run the translated script against representative Bash inputs.
- Use `hlint .`, `cabal test`, and `MONK_INTEGRATION=1 cabal test` as the current project-level regression gates.
- Treat `docs/design/translator-audit.md` as the fidelity source of truth when deciding whether a warning can be ignored.
