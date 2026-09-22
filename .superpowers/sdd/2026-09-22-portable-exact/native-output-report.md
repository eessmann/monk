# Native output/readability bounded task report

Completed the initial renderer/arithmetic portion of approved Task 3.

## Changes

- `Pretty/Expr.hs` structurally flattens scalar concatenations, merges adjacent literal runs, and removes empty literal fragments while retaining one empty word for wholly empty concatenations. Mixed literal/dynamic words retain quote boundaries so literal suffixes cannot extend variable identifiers. Dynamic evaluation order is unchanged.
- Complete literal words use a conservative ASCII whitelist; Fish keywords, assignment-looking text, expansions, metacharacters and control bytes retain quoting. Newline escaping retains the existing byte-safe layout behavior.
- `Pretty/Pattern.hs` delegates to the same expression renderer, avoiding a duplicate concatenation renderer. Literal wildcard fragments remain inactive.
- `ArithmeticPlan.hs` batches a single pure integer operation, removing redundant operand-read helper launches. The existing exclusions for division, remainder, exponentiation, lazy logic, updates and assignment boundaries remain. Arithmetic and pattern helper invocations now request ABI 2.
- Added focused unit and real-Fish property regressions for literal runs, empty arguments, identifier boundaries, keywords, wildcard literals, quotes, controls, metacharacters, numeric wrapping and single-operation batching. Updated existing Pretty/DSL expectations and four admitted complete-file goldens.

## Verification

Bootstrap environment: existing main checkout `devenv --no-tui shell`, GHC 9.12.3, Bash/Fish from that shell (Fish 4.9.3), `--builddir=dist-bootstrap`. This is temporary local evidence, not the approved GHC9.14.1/Fish4.6 or packaged multi-platform release evidence.

- Initial tests-first run: six expected failures across new renderer and batching checks. Parent native structural tests separately failed as intended.
- Corrected the arithmetic test after noticing its original literal assignment was constant-folded. The replacement uses a branch-assigned numeric value. It fails with the original `pureCost >= 2` threshold and passes with `>= 1`; manually confirmed the old artifact contains read + add calls while the new artifact has one batch call.
- `cabal build exe:monk test:monk-test --builddir=dist-bootstrap`: passes without compiler warnings.
- Focused integration selector for Pretty printing, Pretty properties, Fish DSL, Planned integer arithmetic, Planned word and builtin primitives: **100/100 pass**, including parent native tests. Literal and mixed-concatenation round-trip properties each run 50 generated examples against real Fish. Explicit control/metacharacter vector runs once.
- Full `MONK_INTEGRATION=1` main test executable: **732/741 pass**, nine branch-level failures listed below. Raw log: `/tmp/monk-native-broad-final.log`.
- Manually reviewed four admitted golden diffs (echo-echo, echo-exit, double-bracket-eq, glob-basic); they include parent native/flat-guard and ABI2 changes in addition to concise quoting. Golden tests pass.
- Ormolu 0.8.0.2 checks pass for all owned Haskell files and Unit.DSL; `git diff --check` passes. Bootstrap shell omitted Ormolu from PATH, so used installed `/nix/store/07nd3bh1pwnj40hk0967562fvssdqkw8-ormolu-0.8.0.2-bin/bin/ormolu`.

## Remaining branch failures handed to root

1. Unit.Translation: `set argv preserves argument boundaries`: expected `<2:one:two three>`, got `<0::>` from new native path.
2. Unit.OutputBundle: runtime image ABI expected 1, got 2.
3. Unit.API04: exact primitive native requirement test uses an echo now optimized away.
4. Unit.API04: deduplicated native requirements test likewise uses optimized echo.
5. Unit.API04: owned child descriptor platform assertion expects retired Linux-only marker.
6. Unit.PlannedDirectory resolved logical path bound, Standalone: Darwin raises ENAMETOOLONG during test directory creation.
7. Same Sourceable case.
8. Unit.PlannedEnvironment private function collision: pure native output no longer occupies private namespace, expected 125 vs actual success.
9. Same private variable collision.

No renderer/arithmetic failure remains in the full run. Root owns resolution of these branch-level items. No commits were created.

## Canonical compilation

The complete `cabal build all --builddir=dist-ghc9141` passed under canonical GHC 9.14.1 on 2026-09-22. Canonical broad integration is pending in `/tmp/monk-canonical-broad.log`; the earlier renderer-specific runtime evidence remains explicitly the bootstrap toolchain until that run finishes.

Canonical broad snapshot: **773/780 pass**. Six stale fixture-policy exclusions were promoted without changing source bytes; their focused canonical rerun passes **6/6** (five background integrations plus structural pipeline golden). The remaining failure is the independent expected Bash stdout for an out-of-range Unicode echo escape; canonical custom reference Bash and nixpkgs Bash have different behavior despite reporting version 5.3.9. Root/build agent own that reference-runtime investigation. Logs `/tmp/monk-canonical-broad.log` and `/tmp/monk-canonical-promoted.log`.

Later canonical build-all passes with the input/trap/word/prefix frontend integrated. Focused **98/98** arrays/input/trap tests pass; array overflow rejection and additional vector snapshot checks are green. Root resolved the Unicode echo oracle by selecting the pinned custom Bash build configuration. The comprehensive final run is still pending recursive-composition/runtime process-owner stabilization. See `input-traps-expansion-report.md`.
