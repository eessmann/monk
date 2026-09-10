# Monk and Babelfish bake-off

The 2026-09-09 run compares the redesigned Monk translator with Babelfish 1.2.1.
Monk matched Bash on all 38 translations it admitted and rejected 57 fixtures.
Babelfish translated 58 fixtures: 26 matched Bash and 32 differed. Babelfish
also matched 10 fixtures that Monk rejects. This corpus shows the tradeoff
between Monk's narrower admission contract and Babelfish's broader output.

A match here means identical **stdout bytes, stderr bytes and exit status** for
one standalone execution with the recorded inputs. It does not establish
filesystem, caller-state or general semantic equivalence. Read the
[semantic audit](design/translator-audit.md) for Monk's supported contract.

## Results

All 95 selected fixtures were classified; there were no prerequisite/platform
skips or translation/runtime timeouts. Bash execution was needed for the 66
fixtures where at least one translator succeeded. The other 29 failed
translation in both tools and contribute no runtime evidence. In particular,
full `neofetch.bash` failed translation in both tools and was not executed.

| Outcome | Monk | Babelfish |
| --- | ---: | ---: |
| Translation succeeded | 38 | 58 |
| Translation rejected/failed | 57 | 37 |
| Matched Bash | 38 | 26 |
| Differed from Bash | 0 | 32 |
| Zero-diagnostic mismatches | 0 | 32 |

All successful Babelfish translations had empty translation stderr. Its 32
mismatches include 27 with different stdout and five with additional runtime
stderr; five also returned a different status. No output normalization was
applied. The stderr-only differences contain real Fish errors, not merely
different script filenames. Monk's successful results have no errors or
warnings; informational notes are retained in the raw reports.

| Fixture group | Selected | Monk matches / rejected | Babelfish matches / mismatches / failed |
| --- | ---: | ---: | ---: |
| Corpus | 2 | 2 / 0 | 2 / 0 / 0 |
| Benchmark | 5 | 3 / 2 | 1 / 0 / 4 |
| Golden | 9 | 4 / 5 | 7 / 1 / 1 |
| Integration | 51 | 16 / 35 | 9 / 24 / 18 |
| Real-world | 14 | 1 / 13 | 1 / 3 / 10 |
| Semantic regressions and controls | 14 | 12 / 2 | 6 / 4 / 4 |

The original eleven counterexamples now give Monk nine matches and two explicit
rejections (`array-mixed`, `eval-bash-syntax`). Babelfish gives three matches,
four mismatches and four translation failures on those same eleven. Both tools
match the three additional positive controls.

Babelfish's semantic mismatches include dynamic local binding, nondefault IFS,
embedded quoted `$@`, and `eval`. Across the wider corpus, option handling,
subshell state, delimiter reads and parameter expansion account for many of
its differences. Per-fixture classifications are in the
[machine-readable evidence](evidence/bakeoff-2026-09-09.json).

Babelfish matches these ten fixtures that Monk rejects: `assignments`,
`case-pattern-expansion-glob`, `pipeline`, `read-prompt`, `cd-tmp`, `pushd-popd`,
`pwd-cd`, `time-prefix`, `trap-exit-expansion` and `trap-exit`. These observations
do not establish support for every interaction of those constructs. Monk's
exclusions remain described in the audit; this comparison changed no admission
policy or translator production code.

## Performance

Hyperfine 1.20.0 ran ten measurements after one warmup, with Shake `--jobs 1`
to keep benchmark suites from competing with one another. Values below are
batch means ± sample standard deviations. No CPU affinity or exclusive host
reservation was used. The April macOS timings are not a comparable baseline.

| Translation workload | Monk | Babelfish |
| --- | ---: | ---: |
| All 95 fixtures, including failures | 1,174 ± 29 ms | 158.1 ± 1.8 ms |
| All five benchmark fixtures, including failures | 244 ± 21 ms | 23.60 ± 0.60 ms |
| Shared 16-fixture matching subset | 21.23 ± 0.90 ms | 24.62 ± 0.55 ms |
| Shared benchmark (`small.bash`) | 8.74 ± 0.39 ms | 8.28 ± 0.31 ms |

The first two batches return status 1 for **both** translators because the
worker aggregates translation failures; Hyperfine deliberately records those
runs with `--ignore-failure`. They measure processing the corpus, not throughput
of successful equivalent translations. The shared batches return status 0 in
every sample. The worker calls Monk's library in process and launches Babelfish
once per input, so these are harness throughput measurements, not equivalent
per-file CLI startup or isolated compiler timings.

| Generated-script runtime workload | Bash | Monk-generated Fish | Babelfish-generated Fish |
| --- | ---: | ---: | ---: |
| Shared 16 matching fixtures, three-way run | 53.5 ± 3.0 ms | 206.0 ± 3.8 ms | 60.0 ± 3.6 ms |
| All 38 Monk-admitted fixtures | 47.7 ± 1.0 ms | 4,805.5 ± 47.2 ms | — |
| Three Monk-admitted benchmark fixtures | 12.90 ± 0.23 ms | 3,806.6 ± 50.8 ms | — |

Each row is a separate measurement; compare shells within a row. The runtime
worker includes process startup and accepts a fixture's intentional nonzero
status. Direct Bash comparisons independently verified all admitted entries.
The three-way shared run includes both translators; the existing full runtime
benchmark compares only Bash with Monk-generated Fish.

Monk's runtime overhead is substantial, especially on the arithmetic-heavy
benchmark fixtures. On the shared subset, Monk emits 69,116 bytes versus
Babelfish's 765 bytes. Contract guards, generated control/storage code and
bounded helpers contribute to Monk's cost, but this run does not isolate their
individual contributions. Output size alone says nothing about correctness.
The raw JSON's legacy helper-byte/invocation heuristic does not recognize the
new planner's helpers; its zero estimates are not measurements and are unused
here.

## Reproduction and evidence

Environment: Linux x86-64, GHC 9.14.1, Cabal 3.16.1.0, Bash 5.3.9, Fish 4.6.0,
Python 3.14.7, Babelfish 1.2.1 and
[Hyperfine 1.20.0](https://github.com/sharkdp/hyperfine/releases/tag/v1.20.0).
Shell runs use C locale and UTF-8 source. The direct comparator clears Bash
startup injection/options and uses a separate Fish configuration directory.
It preserves fixture argv and raw stdin, runs from the repository root, and
captures bytes without decoding or normalization. It runs trusted fixtures
sequentially; it is not a filesystem sandbox or a filesystem-effects audit.

Build the current tree, put the pinned Bash/Fish on `PATH`, and run:

```bash
cabal build exe:monk-bakeoff -fdevelopment
BAKEOFF_BIN=$(cabal list-bin exe:monk-bakeoff)
"$BAKEOFF_BIN" --group all --file-list scripts/bakeoff-semantic.txt \
  --jobs 1 --babelfish-version 1.2.1 --out-dir artifacts/bakeoff-full
python3 scripts/compare-bakeoff-bash.py artifacts/bakeoff-full
"$BAKEOFF_BIN" --compatible --jobs 1 --babelfish-version 1.2.1 \
  --out-dir artifacts/bakeoff-compatible
python3 scripts/compare-bakeoff-bash.py artifacts/bakeoff-compatible
```

Babelfish and Fish are required. Hyperfine is optional for the runner, but is
required to reproduce the timing tables; missing timing tools are reported.
Use `--babelfish`, `--fish` and `--hyperfine` for explicit paths. Pass matching
`--bash` and `--fish` paths to the companion. Use a fresh output directory for
each run. The checked-in `--compatible` list now contains the 16 observed
matches in both tools; rerun the Bash comparison before relying on membership.

The runner now records and uses standalone execution. Legacy `.mode` sourcing
sidecars cannot change the translator's entry contract. Sourceable behavior
requires a caller contract and remains covered by the dedicated sourceable
suite. Executing a source-child fixture standalone does not test source calls.

The runner's `report.json` retains translation and execution records;
`summary.md` retains its historical pairwise Monk/Babelfish comparison. Its
runtime `succeeded` means the process completed, including intentional nonzero
exits. The companion's `bash-comparison/report.json` supplies the independent
Bash classifications and raw byte evidence. Its four regression checks cover
two translators agreeing on a wrong answer, nonzero status, unavailable comparisons,
empty/spaced arguments, binary streams and timeout cleanup:

```bash
PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover -s scripts \
  -p test_compare_bakeoff_bash.py
```

The [evidence snapshot](evidence/bakeoff-2026-09-09.json) records all 95 fixture
classifications, input/generated hashes, all Hyperfine samples, tool hashes,
source manifest and provenance. This is the uncommitted
`codex/principled-translator` tree based on
`1a2c3826d5265b9be997c5365cd8c138f6cf015f`, not a published version. Raw generated
scripts, streams, plans and reports are retained locally under
`.superpowers/sdd/2026-09-09-principled-translator/bakeoff-full/` and
`bakeoff-compatible/`. The latter also retains the alternate Babelfish runtime
plan, `three-way-command.json` and `hyperfine-runtime-three-way.json` for the
three-way measurement. These local artifacts are not included in the package. After measurement,
`extra-doc-files` was extended to package the evidence JSON; the measured
`monk.cabal` is retained beside the artifacts as `bakeoff-build-monk.cabal`.
No compiler options or dependencies changed.

This report supersedes the pre-redesign 2026-04-16 comparison. Its former
58/58 Monk translation count and broad feature claims do not describe current
admission. Release verification remains a separate
[dated evidence record](design/translator-verification.md).

## Native runtime follow-up (2026-09-10)

The [accepted native-runtime evidence](evidence/bakeoff-native-2026-09-10.json)
preserves the original 95 fixtures and common 16. Three frozen, baseline-admitted
arithmetic programs form a separate performance cohort and do not increase the
historic denominator. Results use standalone entry points and compare raw
stdout, stderr and exit status independently with Bash 5.3.9 on Fish 4.6.0.

| Translator / contract | Admitted | Matched Bash | Admitted mismatches |
|---|---:|---:|---:|
| Frozen Python-runtime Monk baseline | 38/95 | 38/95 | 0 |
| Native Monk, default | 45/95 | 45/95 | 0 |
| Native Monk, stable directories | 48/95 | 48/95 | 0 |
| Babelfish 1.2.1, fresh default comparison | 58/95 | 26/95 | 32 |
| Babelfish 1.2.1, fresh stable comparison | 58/95 | 26/95 | 32 |

All 38 baseline exact cases remain matched under both Monk contracts. The seven
required common-syntax gains and three additional stable-directory gains are
present. Babelfish has no directory-contract selector; its rows are fresh
independent reruns in the two comparison scopes with empty CDPATH. Its binary
SHA256 and Homebrew version receipt are recorded; `--version` is unsupported.

After build jobs became idle, each baseline/candidate fixture received three
warmups and twenty serial samples, alternating baseline/candidate order. Group
values are medians of the per-sample summed fixture times.

| Cohort | Baseline median | Native median | Speedup |
|---|---:|---:|---:|
| Original common 16 | 168.003 ms | 63.130 ms | 2.66× |
| Additional arithmetic 3 | 989.049 ms | 151.680 ms | 6.52× |

Both performance gates pass: arithmetic exceeds 2× speedup and the common 16
have no regression. A separate serial Babelfish follow-up, also using three
warmups and twenty samples, measures 24.523 ms for the common-16 aggregate.
Babelfish is faster on that shared subset; it is a separate measurement pass,
not part of the alternating acceptance comparison.

Initial observations before the warmups are retained separately for every
fixture. Their group totals are 168.738/63.752 ms for common 16 and
991.231/153.032 ms for arithmetic 3 (baseline/native). These follow preliminary
coverage executions and do not represent cold filesystem caches.

Large-exact installed Fish falls from 1,641,887 to 170,987 bytes, **10.414%** of
baseline, passing the <=25% gate. Native binary bytes are separate: the helper
is 1,980,416 bytes. A genuinely published managed large-exact bundle occupies
2,151,912 logical file bytes: 171,172 Fish bytes including its loader, 1,980,416
native bytes, and 324 ownership/manifest bytes. This excludes filesystem block
allocation; native members use mode 0700. The managed entry matches Bash.

Actual `strace` observations over common 16 plus arithmetic 3, collected
separately from timings through an approved tracing escalation, record:

| Observed events | Baseline | Native candidate |
|---|---:|---:|
| Child process creations, excluding threads and entry shells | 177 | 128 |
| Successful exec events, including 19 entry Fish shells | 196 | 147 |
| Python execs | 176 | 0 |
| Native helper execs, including ABI validation | 0 | 127 |

Both also execute one `sh`. All traced streams and statuses match untraced
observations. These are measured process events. The separately reported
`translationStatistics` counts structural helper definitions/references and
native operation call sites; those static sites are not execution counts.
Owned child programs remain structural subtrees until literal rendering,
including when nested definitions are copied. Simple literal output has zero
helper definitions, helper references and native call sites. The old regex
helper-byte estimate is unavailable.

`monk-bakeoff --runtime FILE --directory-contract stable` carries provider,
contract and cwd through serialized workers. The accepted compiled-source
manifest is `7bb370a18da2c3f90a3abe190b423cd4cb9ee35e7ad3f36bb03b5cde53c70043`;
collector hashes are recorded separately. Native ABI 1 targets
`bash53-i64-linux64`; linked dependencies and binary digests are in the snapshot.
The native image SHA256 is
`71b8a1dbe73e5f8fe006a9411bbde46e90c2cbfdb642af01c574485a04b81e0c`.

Raw generated files, stream bytes, all samples, static reports, traces and
managed bundle are retained under
`.superpowers/sdd/2026-09-10-native-runtime-coverage/evidence/`.
The collectors are `scripts/native-runtime-evidence.py`,
`scripts/babelfish-runtime-evidence.py` and `scripts/trace-runtime-evidence.py`.

### Replaying the frozen experiment

Run from the repository root with the retained local artifacts restored. These
commands require `monk-baseline`, the `candidate-accepted` binaries, the pinned
Bash/Fish bootstrap, `evidence/frozen-arithmetic.json`, and its three referenced
`arithmetic-*.bash` inputs at their recorded paths. They also require the
unchanged original fixture files and `docs/evidence/bakeoff-2026-09-09.json`.
The local baseline binary, baseline source archive and experiment inputs are
**not packaged binaries or fixtures supplied by a normal source install**.
Restore them from the retained experiment to reproduce the recorded identities.
A rebuild of `baseline-source.tar.gz` is a newly identified baseline; never
substitute the current translator for the baseline.

Check the executable and collector hashes against the evidence snapshot before
running. The exact collector copies and hashes are retained in
`evidence/frozen-collectors/`. The commands below use the corresponding scripts
in this checkout. Use the recorded repository cwd for an exact replay; a
relocated experiment needs its own provenance. The collectors check input and
baseline hashes and refuse to overwrite their output directories.

```bash
task_root=.superpowers/sdd/2026-09-10-native-runtime-coverage
runtime_tools=.superpowers/sdd/2026-09-09-principled-translator/runtime-final-bootstrap/bin
baseline_monk="$task_root/monk-baseline"
candidate_monk="$task_root/candidate-accepted/monk"
candidate_runtime="$task_root/candidate-accepted/monk-runtime"
babelfish_binary=/home/linuxbrew/.linuxbrew/bin/babelfish
candidate_fingerprint=7bb370a18da2c3f90a3abe190b423cd4cb9ee35e7ad3f36bb03b5cde53c70043

# A fresh directory preserves every previous measurement.
replay_dir=$(mktemp -d "$task_root/evidence/replay-XXXXXXXX")
cp "$task_root/evidence/frozen-arithmetic.json" "$replay_dir/"

# Freeze baseline outputs and the original 95 plus three extra arithmetic inputs.
python3 scripts/native-runtime-evidence.py freeze \
  --out "$replay_dir" --baseline "$baseline_monk" \
  --bash "$runtime_tools/bash" --fish "$runtime_tools/fish"

# Wait until build jobs are idle. This collects both coverage contracts,
# separate initial observations, three warmups and twenty alternating samples.
python3 scripts/native-runtime-evidence.py measure \
  --out "$replay_dir" --baseline "$baseline_monk" \
  --candidate "$candidate_monk" --runtime "$candidate_runtime" \
  --bash "$runtime_tools/bash" --fish "$runtime_tools/fish" \
  --source-fingerprint "$candidate_fingerprint"

# Run only after the preceding command finishes: fresh Babelfish comparisons
# in both scopes, with its common-16 timing collected in a separate serial pass.
python3 scripts/babelfish-runtime-evidence.py \
  --out "$replay_dir" --babelfish "$babelfish_binary" \
  --bash "$runtime_tools/bash" --fish "$runtime_tools/fish" \
  --time-common16 --run-name babelfish-final

# Untimed tracing follows the performance runs; these are observed syscalls.
python3 scripts/trace-runtime-evidence.py \
  --out "$replay_dir" --fish "$runtime_tools/fish" --variant baseline
python3 scripts/trace-runtime-evidence.py \
  --out "$replay_dir" --fish "$runtime_tools/fish" --variant candidate \
  --candidate-dir "$replay_dir/measurement"
```

The fingerprint above describes the accepted compiled inputs. Use a newly
verified fingerprint for any changed candidate. Babelfish's recorded identity
comes from its executable digest and Homebrew receipt, not an unsupported
`--version` flag. Tracing requires permission to trace child processes: the
recorded sandbox attempt was denied, and the successful run used an approved
tool escalation. A denied trace remains unavailable evidence; static call-site
counts cannot replace it.

The replay writes `baseline-cohort.json`, `measurement/report.json`,
`babelfish-final/report.json` and `process-traces-{baseline,candidate}/report.json`
under the new directory, along with generated files and raw process traces.
Runtime timing assertions independently check stdout, stderr and exit status on
every warmup and measured run. To repeat the experiment, start another fresh
replay directory rather than overwriting one of these reports.

Two earlier candidate observations are explicitly superseded and excluded from
these figures. Documentation/evidence additions follow the compiled-source
freeze; no commit or publication is implied. Broader release validation remains
in the [verification record](design/native-runtime-verification.md).
