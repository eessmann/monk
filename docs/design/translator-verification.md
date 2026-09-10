# Principled translator candidate verification

Local Linux verification on 2026-09-09, branch `codex/principled-translator`,
based on main `1a2c3826d5265b9be997c5365cd8c138f6cf015f`. These results describe
the conditional core in [the semantic audit](translator-audit.md) and
[execution profile](execution-profile.md), not arbitrary Bash compatibility.
No commit, push, merge, tag or release publication was performed.

## Candidate evidence

| Gate | Result |
| --- | --- |
| GHC 9.12.2 / Cabal 3.16.1 | Strict build of all libraries, executables, tests and benchmarks passed; 438 main tests and 36 publication tests passed. |
| GHC 9.14.1 / Cabal 3.16.1 | Same strict build and 438 main / 36 publication tests passed. |
| Pinned shell profile | Bash 5.3.9, Fish 4.6.0, Python 3.14.7, Linux x86-64; integrations enabled, C locale. Runtime downloads were checked against recorded SHA-256 values before extraction. |
| Moving Fish | Fish 4.8.1 passes all 438 main tests on each supported GHC, plus the 90-fixture admission/syntax manifest. The official standalone asset SHA-256 is `39cab35242ab77bfdbce73b473000c3b045aaf2fe0951b042199bb7fdba3df78`. |
| Original counterexamples | All 14 semantic admission tests pass: nine original mismatches now compare exactly, two excluded forms reject, and three positive controls execute. |
| Public abstraction | Both compilers pass one public consumer and eight specific constructor, record-update and private-module rejection checks. |
| Linux selectors | All 45 selected tests pass: owned child isolation executes; the six former output-process-substitution fixtures now verify explicit rejection. This is not a claim of process-substitution execution support. |
| Fixture policy / syntax | All 90 reviewed fixtures match policy: 35 exact and 55 rejected. Exact emitted output passes Fish syntax checks. This manifest does not replace behavioral comparisons. |
| Generated comparisons | The full suite includes 100 bounded compositions with shrinking and independent output-equivalence properties. Admission failures are failures, not discarded samples; zero-diagnostic mismatches are identified separately. No admitted mismatch remains in these runs. |
| Publication | 36 private tests cover staging/flush/rename/recovery injection, verified-generation retry durability, ownership/symlink conflicts and concurrent readers/publishers. Public bundle tests cover pinned children, repeated sources, old loaders, planning purity and sourceable status/scope/argv. |
| Quality | HLint 3.10: no hints. Ormolu 0.9.0.0: all 107 Haskell files pass, including new and compile-fail sources. `cabal check` and `git diff --check` pass. |
| Haddock | Project documentation generated for all three libraries on both GHC versions. Installed dependency documentation is absent locally, so external links and some harness reexports produce documentation warnings. Compiler builds remain warning-free. |
| Unpacked source | Both GHC versions pass tarball `cabal check`, strict unpacked build of all components, executable installation, and Bash/Fish byte comparisons for UTF-8 source/argv/filenames through combined output and a managed loader under C locale. |

The main-suite count includes one explicit non-executed manual baseline:
the oversized hand-written neofetch comparison remains bake-off-only. This is
not translator acceptance coverage. There are no other runtime prerequisite or
platform skips in these local runs. Rejected fixtures are counted as rejection
tests, never as successful Bash/Fish execution comparisons.

Four exact golden outputs were regenerated and reviewed. They now contain
owned contract guards, runtime state and bounded helper calls. The unused
`pipeline.bash` golden was registered and classified as a rejection because
the initial builtin-writer lifetime envelope does not prove downstream `wc`.
`cat` and literal `tr a-z A-Z` pipelines retain positive differential coverage.
[The migration ledger](legacy-test-migration.md) accounts for old expectations.

Final review also closed concrete gaps in function suffix redirects, loop
status, export fallback storage, large operand/child transport, fresh scalar
names ending in `PATH`, optional-caller guard size, and programmatic contract
validation. Guard output grew from 154,341 to 6,529,477 bytes for four to eight
bindings before the fix; it now grows from about 9 KB to 18 KB. Both entry modes
have growth regressions as well as behavioral guard tests. JSON and programmatic
contracts share validation before imported facts can affect admission; negative
tests exercise script, parse-result and source-graph APIs.

## Reproduction

Use a supported GHC and install the pinned runtime with
`bash scripts/install-ci-runtimes.sh "$PWD/artifacts/runtime" pinned`.
Place its `bin` directory first in PATH, then run:

```bash
export PATH="$PWD/artifacts/runtime/bin:$PATH"
export MONK_INTEGRATION=1 LC_ALL=C LANG=C
cabal build all -fdevelopment --enable-tests --enable-benchmarks
cabal test all -fdevelopment --test-show-details=direct
cabal exec -- python3 scripts/check-public-boundaries.py --report artifacts/boundaries.json
cabal test monk-test -fdevelopment --test-options="-p '/procsub-output/ || /Planned child isolation/'"
bash scripts/generate-parity-manifest.sh "$(cabal list-bin exe:monk)" artifacts/parity.tsv
hlint .
git ls-files '*.hs' -z | xargs -0 ormolu --mode check
cabal check
cabal haddock all -fdevelopment --disable-documentation
bash scripts/check-source-distribution.sh artifacts/sdist
```

The explicit Haddock command generates project API documentation without
rebuilding documentation for every external dependency. For an uncommitted
checkout, the Ormolu input must also include new files and omit deleted paths;
the local run checked all active Haskell files.

Local logs and runtime identities are retained under
`.superpowers/sdd/2026-09-09-principled-translator/`, particularly
`candidate-ghc-9.14-tests.log`, `candidate-ghc-9.12-tests.log`, the corresponding
build/boundary logs, `candidate-quality.json`, `candidate-parity.tsv`, and
`candidate-source-manifest.json`. The latter fingerprints 270 active source,
test, fixture, script, CI and package files; documentation and local artifacts
are excluded. Its aggregate SHA-256 is
`6090e328a69cb06bc8c70b2cbbf6fc3005c9219fb4acaceff30fde8decf7d655`.
Moving runtime results are in `moving-fish-4.8.1-ghc-9.12-tests.log`,
`moving-fish-4.8.1-ghc-9.14-tests.log` and `moving-fish-4.8.1-parity.tsv`.
Unpacked install evidence is retained in `sdist-ghc-9.12-escalated.log` and
`sdist-ghc-9.14.log`; each corresponding artifact directory records its unpacked
verification path. The GHC 9.12 install needed approved writes to the local
Cabal store; its initial sandbox denial was rerun successfully, not skipped.

## Remaining external evidence

The subsequent 2026-09-09 [Babelfish bake-off](../babelfish-comparison.md) changes
only comparison tooling, its tests/selectors, documentation and packaged
evidence. The earlier candidate manifest and source archive above identify
the pre-comparison snapshot; they do not identify this later tree. The bake-off
has its own source manifest, binary/tool hashes and raw reports. Its focused
validation comprises ten Haskell bake-off tests, four Python comparator tests,
38/38 admitted Bash comparisons and a repeated 16/16 match for each translator
on the shared subset, plus formatting, HLint and package checks. The full
compiler/runtime release matrix was not rerun for this tooling follow-up.

The updated remote CI matrix has not run on this unpushed candidate. It covers
both supported GHC versions and pinned/moving Fish, with independent pinned
quality-tool bootstrap. The local moving-version run does not establish future
Fish compatibility. Other operating systems, Bash profiles and locales remain
outside the initial verified contract. Publishing and tagging require a separate
action; this record does not claim a released version.
