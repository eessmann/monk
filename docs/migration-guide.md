# Monk Migration Guide

This guide covers the warning classes and best-effort areas that most often need manual cleanup after translating Bash to Fish.

## `set -e` / `pipefail`

- Treat Monk's `set -e` lowering as best-effort, especially around nested pipelines and compound-list edge cases.
- Translated background jobs / `$!` / `wait` now use Monk-managed job tokens and have focused parity coverage, but PID-specific follow-ons such as `kill $!` still deserve manual review.
- If a translated script depends on Bash's exact errexit exceptions, replace `cmd; or exit $status` regions with hand-written Fish control flow.
- Monk now targets Bash's default non-`inherit_errexit` behavior inside command substitutions, but if a translated substitution still needs bespoke control flow, prefer assigning through an explicit `if` or `begin ... end` block in Fish.

## `read`

- Recheck scripts using `read -a`, multiple destination variables, or custom `IFS`.
- Simple single-variable non-empty `read -d` now has an exact helper path and focused runtime coverage.
- Recheck delimiter-driven `read -d` flows against real stdin when they use empty delimiters, multiple destination variables, arrays, or mixed option clusters, because those cases still differ from Bash.
- Prefer explicit `string split`, `string collect`, and `read --delimiter` logic in hand-edited Fish for non-trivial stdin parsing.

## Process Substitution

- `<(...)` now has direct runtime coverage for simple cases, but larger pipelines should still be tested in Fish.
- `>(...)` now has Linux-gated runtime coverage for a simple case, but it remains a manual-review area for non-trivial flows and for local macOS runs that skip the fixture.
- If the translated output feeds another command asynchronously, prefer rewriting it as `mktemp` plus explicit producer/consumer steps.

## `trap`

- Simple `trap '...' EXIT` lowers to a Fish process-exit handler and now has runtime coverage.
- Unsupported trap options and more complex signal forms still warn for manual review.
- When cleanup ordering matters, prefer an explicit helper function and `--on-process-exit %self` in hand-edited Fish.

## Subshell Isolation

- Bash subshells isolate variable and directory changes. Fish `begin ... end` does not.
- When Monk warns about subshell best-effort translation, manually convert the block to an explicit helper function or separate script if isolation matters.

## `shopt`

- Monk lowers `shopt` to a warning plus `true`; no Fish semantic equivalent is applied.
- Replace `shopt`-dependent logic manually with explicit Fish behavior or Bash-only compatibility guards.

## `readonly` / `declare -r`

- Fish has no direct readonly variable enforcement.
- If immutability matters, keep the value local to a small scope or move it into a function argument instead of relying on the translated `set`.

## Non-literal `source`

- Recursive translation only inlines literal source paths.
- For dynamic source paths, keep them as manual review points and convert them to explicit branching or path resolution in Fish.

## Warning-Driven Cleanup Workflow

- Run Monk with warnings enabled.
- Use `MONK_INTEGRATION=1 cabal test` and compare the translated script against Bash on representative inputs.
- Treat warnings in `translator-audit.md` as semantic categories, not cosmetic notes.
