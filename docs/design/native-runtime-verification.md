# Native runtime and broader coverage verification

Local Linux candidate on 2026-09-10, in the existing uncommitted
`codex/principled-translator` worktree. This extends the frozen 2026-09-09
principled translator; the prototype branches remain evidence, not production
fallbacks. No commit, merge, push, tag or release publication was performed.

The accepted build-input manifest contains 304 files and has SHA-256
`7bb370a18da2c3f90a3abe190b423cd4cb9ee35e7ad3f36bb03b5cde53c70043`.
Source and exact binary identities, raw logs, source archives and superseded
candidate evidence are retained in
`.superpowers/sdd/2026-09-10-native-runtime-coverage/`.

## Correctness and coverage

The independent Bash comparison preserves the original 95-fixture denominator:
45 default-contract translations match, and 48 match with the stable directory
contract. Every admitted fixture matches stdout bytes, stderr bytes and status;
all 38 frozen baseline successes remain. Rejected fixtures are not counted as
execution matches. New interaction cases and three frozen runtime-dependent
arithmetic workloads are reported separately.

The seven common-script gains and three explicit-directory gains are all
accounted for in [the comparison](../babelfish-comparison.md). Babelfish 1.2.1
matches 26/95, with 32 admitted mismatches and 37 translation rejections, in
both comparison scopes. Its unchanged directory behavior does not receive
Monk's caller obligations implicitly.

The expanded suites cover both entry modes, byte/empty field cardinality,
brace-duplicated effects, arithmetic errors and loop exits, fixed-arity file
predicates, source argument inheritance, sourceable caller state and exported
functions, directory success/failure edges, descriptor ownership, signals and
runtime generation replacement. Independent review reproduced and corrected
runtime pathname spelling, modified-generation execution during reuse,
provider trailing-newline loss, loop continuation/break facts, arithmetic
failure facts, bracket semantics, and effective source argv inheritance.

## Local acceptance

| Check | Evidence |
| --- | --- |
| GHC 9.12.2 and 9.14.1 | Development builds of libraries, executables, tests and benchmarks are warning-free. |
| Full suite with integrations enabled | 725 tests pass on each supported compiler with pinned Fish 4.6.0 and moving Fish 4.8.1. |
| Native core and transport | Pure protocol/integer tests and independent Bash byte comparisons pass; child tests cover signals, SIGPIPE, closed descriptors, private descriptor closure, large/invalid-byte transport, streaming NUL warnings and unchanged stdin. |
| Publication | 38 private tests pass, including fault injection and concurrent readers/publishers. Nine native blackboxes pass on both runtimes: missing/incompatible ABI, mode/byte corruption, generation retention, provider path bytes, ignored ambient RTS flags and execution without Python/installed runtime on PATH. |
| Public boundaries | Both compilers pass one public consumer and eleven targeted constructor, record-update and private-module rejection checks. |
| Linux and fixture policy | 45 Linux selector tests and the 90-fixture admission/syntax manifest pass. These are different inventories from the 95-fixture coverage comparison. |
| Formatting and packaging | HLint has no hints; all 128 active Haskell files pass Ormolu; `cabal check` and `git diff --check` pass. |
| Haddock | Project documentation passes on both compilers; absent external dependency documentation produces link warnings. |
| Unpacked source | Both compilers build all components, install both executables and pass UTF-8 filename/data/argv comparisons through combined and managed output. All 303 packaged build-input files match the accepted manifest; only `.github/dependabot.yml` is not packaged. |

The main suite retains one explicitly unexecuted oversized hand-written
neofetch baseline; it is a bakeoff-only comparison and contributes no
translator acceptance evidence. There are no missing runtime prerequisites or
platform skips in these local Linux execution runs.

Earlier sandboxed Cabal installation attempts reached the build stage but
could not write its package store. The accepted archive checks completed with
authorized store access; those earlier attempts remain marked as failed or
superseded rather than being counted as successful uninterrupted runs.

After documentation and comparison evidence were finalized, a final source
archive was assembled under the same artifact root's `final-distribution/`.
Its packaged build inputs are checked against the accepted manifest and both
successfully installed archives. Documentation-only additions do not change
those compiled inputs; the archive comparison records that identity without
claiming another build or installation run.

## Optimization evidence

All three acceptance gates pass on the accepted source:

| Frozen cohort / artifact | Baseline | Candidate | Result |
| --- | ---: | ---: | --- |
| Arithmetic group, median of sample totals | 989.049 ms | 151.680 ms | 6.52 times faster; required at least 2 times. |
| Original common 16, median of sample totals | 168.003 ms | 63.130 ms | 2.66 times faster; allowed at most 10% regression. |
| `large-exact`, installed-runtime Fish | 1,641,887 bytes | 170,987 bytes | 10.4% of baseline; allowed at most 25%. |

The native executable is 1,980,416 bytes. The measured managed `large-exact`
bundle contains 171,172 Fish bytes, including its entry loader, the native
executable, and 324 bytes of ownership/manifest data: 2,151,912 logical file
bytes in total. These disk-size costs are separate from generated Fish size.

Actual syscall traces over the frozen nineteen cases record 128 candidate child
process creations (127 native helpers and one `sh`) versus 177 baseline children
(176 Python helpers and one `sh`). Each also starts nineteen Fish entry
processes. Candidate traces contain no Python execution. Trace observations
match untraced stdout, stderr and status; these counts are distinct from static
helper definitions and call sites.

The exact measurement protocol and numeric results belong to
[the updated comparison](../babelfish-comparison.md). It retains three warmups,
twenty serial alternating baseline/candidate samples, independent first-run
observations, original shared subsets, native linkage, Fish/binary/bundle bytes
and real process traces separately from structural static counts. Superseded
candidate measurements are excluded from acceptance.

## Reproduction and limits

Install the declared Bash/Fish profile, select a supported GHC, and run:

```bash
cabal build all -fdevelopment --enable-tests --enable-benchmarks
export PATH="$(dirname "$(cabal list-bin exe:monk-runtime)"):$PATH"
MONK_INTEGRATION=1 LC_ALL=C LANG=C cabal test all -fdevelopment --test-options=-j1
python3 runtime-test/protocol.py "$(cabal list-bin exe:monk-runtime)"
bash test/native/child-transport.sh "$(cabal list-bin exe:monk-runtime)"
MONK_NATIVE_TEST_BINARY="$(cabal list-bin exe:monk)" MONK_NATIVE_RUNTIME="$(cabal list-bin exe:monk-runtime)" python3 scripts/test_native_publication.py
cabal exec -- python3 scripts/check-public-boundaries.py
cabal haddock all -fdevelopment --disable-documentation
MONK_GHC=ghc-9.12.2 bash scripts/check-source-distribution.sh artifacts/sdist-9.12
```

The same source-distribution script accepts `MONK_GHC=ghc-9.14.1`.
Use the frozen-cohort commands and identities in the comparison report to
reproduce performance rather than timing a changing fixture inventory.

The remote CI matrix is configured but remains unrun on this unpushed tree.
Local moving-Fish results are compatibility evidence for 4.8.1, not future
versions. Other Bash profiles, locales and platforms remain outside the
verified contract. Bundled executables retain their recorded dynamic-library
requirements. Publishing, tagging and automatic generation cleanup remain
separate actions.
