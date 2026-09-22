# Finite eval and immutable source contexts

## Implemented envelope

- Private `parseBashFragment` uses ShellCheck through Identity with filesystem/config callbacks disabled. The existing public IO parser signature is preserved.
- Eval operands must normalize to pure single fields whose complete text is a literal, an existing proven scalar constant, or a concatenation of those forms. They are joined with ASCII spaces exactly once, parsed during translation, and passed through the existing normalizer in the current binding/control flow. No runtime Bash parser or Fish eval is emitted.
- Empty/operandless eval explicitly succeeds. Nonempty eval retains incoming status and normal statement effects, including function return, local declarations in the actual owning function, and finite definitions. Explicit builtin/command eval dispatch is recognized.
- Recursive compile-time eval cycles and nesting beyond 64 reject. Unknown/effectful operands, eval option errors, potentially failing arithmetic inside eval, command substitutions inside eval, and wait operations inside eval reject. The latter two require exact nested diagnostic/warning source mapping before widening admission.
- Immutable literal sources now normalize in owned subshells and command substitutions. Functions admit absolute immutable source targets so compilation cannot assume definition-time cwd at later calls. Writes to existing caller locals and source return use existing visible storage and source frames.
- Source-local declarations still reject because a declaration inside the generated source wrapper would have the wrong function scope. Relative function sources, computed targets, cycles, mutable source text, and inherited argv mutation remain rejected.
- Source parse input is cached once, but every source occurrence is normalized independently against its current entry facts. Removed the unnecessary whole-entry equality gate. Return edges now join inside active functions as well.

## Tests and evidence

- New `Unit.PlannedEval` plus extended `Unit.Source`.
- Initial red: 16/30 tests fail for prior eval/source admission exclusions.
- Green: **31/31** focused eval/source tests pass with real Bash and Fish (`MONK_INTEGRATION=1`). Includes argument joining, empty/incoming status, nested eval, caller-local storage, return boundaries, child source isolation, absolute function source writes/return and repeated source contexts.
- Added three subsequent regressions for eval-defined function dispatch, redefinition identity rejection and successful constant arithmetic. These need the final shared rebuild described below.
- Broad integration snapshot: **762/767 pass**. The five failures are stale rejection expectations: two API04 literal eval inputs (root has changed to unknown inputs), semantic eval rejected grouping (moved to exact), pipeline-source nongraph diagnostic (updated source-environment), and source-recursive admission policy (now exact). No behavioral stream/status mismatch was observed.
- Promoted unchanged `semantic/eval-bash-syntax.bash` and `integration/source-recursive.bash` input fixtures to exact policy. Original corpus bytes were not edited.
- Latest final whole-suite rebuild was blocked by the concurrent session integration: `Plan.hs:938` constructs `ChildRuntime` without its newly added Bool field. Eval/source modules compiled cleanly. Root notified; final rebuild/retest must include these changes and the three additional eval cases.
- Ormolu checks and `git diff --check` pass on the owned implementation/test files.

All evidence above is the temporary local GHC9.12.3/Fish4.9.3 bootstrap, not canonical pinned-toolchain or multi-platform release evidence. Logs: `/tmp/monk-eval-red.log`, `/tmp/monk-eval-green.log`, `/tmp/monk-eval-broad.log`, `/tmp/monk-eval-final-build.log`.

## Integration ownership

Root owns final audit/todo documentation updates. Public Translation APIs and translator materialization were not changed for eval. No commits were made. Continuing with the separately delegated background/wait normalization cases after this report.

## Canonical toolchain confirmation

On 2026-09-22, `devenv ... shell -- cabal build all --builddir=dist-ghc9141` passed with GHC 9.14.1. A direct main test run with `MONK_INTEGRATION=1`, Bash 5.3.9 and Fish 4.6 passed **164/164** selected eval/source/child-isolation tests, including the three later eval regressions and the explicit eval-wait diagnostic rejection. Log: `/tmp/monk-canonical-focused.log`. This supersedes the bootstrap-only final-rebuild gap above; broad canonical integration is running separately.

Canonical broad snapshot: **773/780 pass**. Six stale fixture-policy exclusions were promoted without changing source bytes; their focused canonical rerun passes **6/6** (five background integrations plus structural pipeline golden). The remaining failure is the independent expected Bash stdout for an out-of-range Unicode echo escape; canonical custom reference Bash and nixpkgs Bash have different behavior despite reporting version 5.3.9. Root/build agent own that reference-runtime investigation. Logs `/tmp/monk-canonical-broad.log` and `/tmp/monk-canonical-promoted.log`.

Later canonical build-all passes with the input/trap/word/prefix frontend integrated. Focused **98/98** arrays/input/trap tests pass; array overflow rejection and additional vector snapshot checks are green. Root resolved the Unicode echo oracle by selecting the pinned custom Bash build configuration. The comprehensive final run is still pending recursive-composition/runtime process-owner stabilization. See `input-traps-expansion-report.md`.
