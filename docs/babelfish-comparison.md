# Monk vs Babelfish (Bash -> Fish)

This document compares Monk with the Go-based babelfish translator and outlines a reproducible bake-off to evaluate real-world behavior.

## Goals

- **Translation fidelity**: does the output preserve runtime behavior?
- **Safety/visibility**: are lossy translations surfaced clearly?
- **Coverage**: what Bash constructs are translated vs left as-is?
- **Ergonomics**: how easy is it to run on a corpus and review results?

## High-level differences

- **Architecture**: Monk now exposes a typed Fish DSL and lowers the translation handoff through it before rendering with the raw pretty-printer backend; remaining raw-shaped translator internals are isolated behind an internal syntax boundary. Babelfish writes Fish text directly from a Bash AST.
- **Diagnostics**: Monk emits warnings and inline notes for lossy translations and offers `--strict`; babelfish tends to emit a best-effort script with fewer diagnostics.
- **Translation strategy**: Monk hoists side effects and emulates short-circuit arithmetic to preserve semantics; babelfish focuses on pragmatic, readable output.

## Translation approaches (worked example)

Bash input:

```bash
#!/usr/bin/env bash
name=${NAME:-world}
echo "hi $name"
if [[ $name == w* ]]; then
  echo "starts with w"
fi
```

Monk output (current CLI):

```fish
set --global name (if set '-q' 'NAME'
                      and test '-n' (string join ' ' $NAME ; or printf '')
                     if test (count $NAME) '-gt' '0'
                       printf '%s\n' $NAME
                     end
                   else
                     if test (count 'world') '-gt' '0'
                       printf '%s\n' 'world'
                     end
                   end)
echo 'hi '(string join ' ' $name ; or printf '')
if string 'match' '-q' '--' (string join ' ' w* ; or printf '') (string join ' ' $name ; or printf '')
  echo 'starts with w'
else
  begin
    true
  end
end
```

Babelfish output:

```fish
#!/usr/bin/env bash
set name (test -n "$NAME" && echo "$NAME" || echo 'world')
echo 'hi '"$name"
if test "$name" = 'w*'
  echo 'starts with w'
end
```

Notes:

- Monk expands `${NAME:-world}` using `set -q` plus list-safe handling. It's more verbose, but aims to preserve empty-list behavior and word splitting semantics.
- Monk lowers `[[ $name == w* ]]` to `string match -q -- 'w*' $name`, which preserves bash-style pattern matching. Babelfish emits `test "$name" = 'w*'`, which is a literal string comparison in fish (no glob matching), so semantics differ for patterns.
- Babelfish preserves the original shebang and keeps the output compact; Monk emits notes/warnings to stderr for review (not shown above).

## Bake-off protocol

### Inputs

Use a mix of synthetic and real-world inputs:

- **Monk corpus**: `test/fixtures/corpus/*.bash`
- **Monk benchmarks**: `benchmark/fixtures/*.bash`
- **Integration fixtures**: `test/fixtures/integration/*.bash`
- **Golden fixtures**: `test/fixtures/golden/*.bash`
- **Real-world fixtures**: `test/fixtures/realworld/*.bash`
- **Your scripts**: add representative Bash scripts under `benchmark/fixtures/` or a separate folder.

### Baseline commands

Translate each script with both tools:

```bash
# Monk (via cabal)
cabal run monk -- path/to/script.bash > /tmp/monk.fish

# Babelfish (reads stdin)
babelfish < path/to/script.bash > /tmp/babelfish.fish
```

If you want recursive source translation in Monk:

```bash
cabal run monk -- path/to/script.bash --recursive --sources inline > /tmp/monk.fish
```

### Normalization (for diffing)

Strip trailing whitespace and normalize line endings before diffing:

```bash
normalize() {
  sed -e 's/[[:space:]]\\+$//' "$1" | sed -e 's/\\r$//'
}
normalize /tmp/monk.fish > /tmp/monk.norm
normalize /tmp/babelfish.fish > /tmp/babelfish.norm

diff -u /tmp/babelfish.norm /tmp/monk.norm
```

### Behavioral checks

For each script, run both outputs in fish and compare stdout, stderr, and exit code.

```bash
fish /tmp/monk.fish > /tmp/monk.out 2>/tmp/monk.err; echo $? > /tmp/monk.rc
fish /tmp/babelfish.fish > /tmp/babelfish.out 2>/tmp/babelfish.err; echo $? > /tmp/babelfish.rc

diff -u /tmp/babelfish.out /tmp/monk.out
```

### Recording results

Capture the following for each script:

- **Translation errors** (if any)
- **Warnings/notes** (Monk)
- **Diff summary** (line count, key differences)
- **Runtime deltas** (stdout, stderr, exit code)
- **Tooling context** (monk git SHA, babelfish version if available)

A simple result template:

```
Script: <path>
Monk warnings: <count> (<high/medium/low>)
Babelfish notes: <count if any>
Output diff: <none | summary>
Runtime diff: <none | summary>
Notes: <interesting observations>
```

## Bake-off helper (optional)

Use the dedicated bake-off executable:

```bash
cabal run monk-bakeoff -- --out-dir /tmp/monk-babelfish
```

The bake-off code lives under `scripts/` and builds as a separate Cabal target so Shake/Aeson/process tooling does not affect the main `monk` library or executable.

Tool prerequisites:

- `babelfish` and `fish` are required for all bake-off runs
- `hyperfine` is optional and only needed when benchmark runs are enabled
- `--babelfish`, `--fish`, and `--hyperfine` accept explicit tool paths
- the runner performs tool preflight before Shake starts and reports actionable path/install guidance if a required tool is missing

The runner writes:

- `meta.json`
- `report.json`
- `summary.md`
- per-fixture artifact directories under `fixtures/`

Useful selectors and options:

```bash
cabal run monk-bakeoff -- --compatible --out-dir /tmp/monk-babelfish-compatible
cabal run monk-bakeoff -- --group integration --group realworld --no-benchmark
cabal run monk-bakeoff -- --file test/fixtures/realworld/version-compare.bash
cabal run monk-bakeoff -- --babelfish-version 1.2.1
```

The runner normalizes runtime stderr by stripping the output directory and tool-specific `.monk/.babelfish` suffixes to avoid path-only diffs.
It respects fixture sidecar files: `<name>.args`, `<name>.stdin`, `<name>.mode`, `<name>.platforms`, `<name>.prereqs`, and `<name>.recursive`.
If `hyperfine` is installed, the runner also records CLI timing runs. If it is missing, the runner now emits an explicit preflight note and skips benchmark targets. Configure benchmark runs with:

```bash
cabal run monk-bakeoff -- --no-benchmark
cabal run monk-bakeoff -- --hyperfine-runs 10 --hyperfine-warmup 1
```

Hyperfine outputs are written to `hyperfine-all.md/json` and `hyperfine-benchmark.md/json` in the output directory when enabled.

## Bake-off results (2026-04-16)

This section is a dated snapshot from the run below; fixture counts may differ from the current test inventory.

Environment:

- Monk: `monk-bakeoff` built from the current local tree
- Babelfish: 1.2.1
- Fish: 4.6.0
- Hyperfine: 1.20.0 (runs=10, warmup=1)
- Host: `darwin/aarch64`
- Report output: `/private/tmp/monk-bakeoff-architecture-full-final`

Summary (corpus + benchmarks + integration + golden + real-world fixtures):

- Total fixtures: 62
- Skipped by metadata: 4 (`test/fixtures/integration/procsub-output.bash`, `procsub-output-pipeline.bash`, and `procsub-output-variable.bash` are Linux-only; `test/fixtures/realworld/taoc.bash` requires `tac`)
- Monk translation success: 58/58 non-skipped fixtures
- Babelfish translation success: 35/58 non-skipped fixtures
- Runtime diffs where both translated and ran: 13/35

Coverage breakdown by fixture group:

- Benchmark: Monk 3/3, Babelfish 1/3
- Corpus: Monk 2/2, Babelfish 2/2
- Golden: Monk 9/9, Babelfish 8/9
- Integration: Monk 31/31 non-skipped, Babelfish 20/31 non-skipped
- Real-world: Monk 13/13 non-skipped, Babelfish 4/13 non-skipped

Babelfish translation failures (23):

- `benchmark/fixtures/large.bash`, `benchmark/fixtures/medium.bash`, `test/fixtures/realworld/pyramid-left.bash`, `test/fixtures/realworld/pyramid-right.bash`, `test/fixtures/realworld/echo-args.bash`, and `test/fixtures/realworld/argparse-mini.bash`: C-style loops or postfix arithmetic remain a recurring failure mode.
- `test/fixtures/integration/background-fail-wait.bash`, `background-jobs.bash`, `background-local-scope.bash`, `background-pipefail.bash`, and `background-success-wait.bash`: background jobs and translated `wait` remain outside Babelfish's working surface here.
- `test/fixtures/integration/param-expansion-args.bash`, `param-expansion-case.bash`, and `param-expansion-redirection.bash`: side-effecting parameter expansion still fails.
- `test/fixtures/integration/read-flags.bash`, `read-delimiter-null-array.bash`, and `test/fixtures/golden/extglob-basic.bash`: `read` flag handling, the newer null-delimited read surface, and extglob remain unsupported in practice on this corpus.
- `test/fixtures/realworld/a2l.bash`, `coat.bash`, `envfile-preview.bash`, `path-filter.bash`, and `neofetch.bash`: real-world readonly/parameter-expansion-heavy scripts still fail to translate.
- `test/fixtures/integration/arith-short-circuit.bash`: still fails translation outright.

Runtime diffs where both translated:

- `errexit-basic`, `errexit-andor`, and `errexit-conditionals`
- `pipefail-basic` and `pipefail-toggle`
- `read-delimiter`, `read-delimiter-flags`, `read-delimiter-ifs`, and `read-delimiter-null-vars`
- `source-recursive`
- `neofetch-mini`
- `semver-normalize`
- `version-compare`

Notes:

- These are Monk-vs-Babelfish diffs, not automatically Monk-vs-Bash failures.
- Representative spot checks against Bash still favor Monk on the previously investigated fixtures `errexit-basic`, `pipefail-basic`, `read-delimiter`, `source-recursive`, `version-compare`, and `neofetch-mini`.
- The new full run adds `semver-normalize` to the Monk-vs-Babelfish runtime-diff set; that fixture should get a direct Bash spot check before drawing stronger parity conclusions from the bake-off alone.
- In those spot checks, Babelfish typically emitted invalid Fish (`set -e`, `set -o pipefail`, `read -rd:`, `test ... ==`) or failed to preserve behavior, while Monk matched the Bash fixture.
- Monk still needs caveats of its own: the current audit still treats `set -e` / `pipefail` as best-effort overall, with remaining compound-list edge cases outside the focused fixtures.
- Monk emitted 57 warnings and 85 notes across the full run, and `test/fixtures/realworld/neofetch.bash` alone still accounts for 31 of those warnings. Outside full `neofetch`, Monk emitted 26 warnings across the whole corpus.

Performance (hyperfine translation batches):

- All fixtures: Monk `1.330 ± 0.011 s`, Babelfish `0.288 ± 0.011 s`
- Benchmark fixtures: Monk `0.032 ± 0.002 s`, Babelfish `0.041 ± 0.004 s`
- Throughput still favors Babelfish on the full corpus, but coverage matters more than raw speed because Babelfish fails translation on much of the difficult surface.

## Where Monk is currently stronger

- **Broader semantic coverage** on the current corpus, especially on integration and real-world fixtures where Babelfish often fails to translate at all
- **Diagnostics** via warnings, notes, confidence scoring, and `--strict`, which makes approximation visible instead of silent
- **Focused runtime evidence** for side-effecting expansions, translated background jobs / `wait`, generalized covered `read -d`, and recursive literal `source`
- **Stronger parity on the current mismatches**: the latest spot checks against Bash favored Monk on the major runtime-diff fixtures
- **Better testing depth** through property, golden, integration, and bake-off coverage tied back to the translator audit

## Where Monk is currently weaker / tradeoffs

- **Performance**: translation throughput is much slower than babelfish in the bake-off.
- **Verbosity**: output is more scaffolding-heavy (`string join`, `begin` blocks) to preserve list and expansion semantics.
- **Errexit/pipefail emulation**: still best-effort overall, especially around bash exceptions, compound lists, and subtle command-substitution behavior.
- **Large warning-heavy scripts**: translation success does not automatically mean drop-in parity; full `neofetch` remains the clearest example.
- **Semantic gaps remain**: word splitting, subshell isolation, non-literal `source`, option-heavy `trap`, and fish-specific behaviors still require manual review.

## Where babelfish is stronger

- **Simplicity**: quick to run on small scripts with low setup cost
- **Readable output**: often shorter and easier to edit by hand than Monk's semantic scaffolding
- **Throughput on the subset it handles**: still materially faster on the full-corpus translation benchmark

## Open questions

- How much further should Monk push `set -e` / `pipefail` fidelity beyond the current focused fixtures?
- Should Monk keep reducing the residual warning-driven `read` fallback surface, or is the current helper-backed covered slice enough?
- Are non-literal `source`, option-heavy `trap`, and broader `>(...)` coverage worth the extra implementation complexity?
- Are there external corpora where Babelfish still preserves semantics better than Monk, or is its remaining value mostly simplicity and speed?

---

If you run the bake-off, please capture results in this document or link a separate report.
