# Monk vs Babelfish (Bash -> Fish)

This document compares Monk with the Go-based babelfish translator and outlines a reproducible bake-off to evaluate real-world behavior.

## Goals

- **Translation fidelity**: does the output preserve runtime behavior?
- **Safety/visibility**: are lossy translations surfaced clearly?
- **Coverage**: what Bash constructs are translated vs left as-is?
- **Ergonomics**: how easy is it to run on a corpus and review results?

## High-level differences

- **Architecture**: Monk lowers into a typed Fish IR (GADT-based) and renders from there; babelfish writes Fish text directly from a Bash AST.
- **Diagnostics**: Monk emits warnings and inline notes for lossy translations and offers `--strict`; babelfish tends to emit a best-effort script with fewer diagnostics.
- **Translation strategy**: Monk hoists side effects and emulates short-circuit arithmetic to preserve semantics; babelfish focuses on pragmatic, readable output.

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

Use the repo script:

```bash
scripts/bakeoff.sh /tmp/monk-babelfish
```

It writes `report.tsv` and `meta.txt` in the output directory. If you want to pin the babelfish version in the report, set:

```bash
BABELFISH_VERSION_OVERRIDE=1.2.1 scripts/bakeoff.sh /tmp/monk-babelfish
```

The script normalizes runtime stderr by stripping the output directory and tool-specific `.monk/.babelfish` suffixes to avoid path-only diffs.
It also respects optional fixture sidecar files: `<name>.args` (whitespace-separated args) and `<name>.stdin` (stdin content).
If `hyperfine` is installed, the script also records CLI timing runs. You can configure:

```bash
BAKEOFF_HYPERFINE=0   # disable timing runs
HYPERFINE_RUNS=10     # default
HYPERFINE_WARMUP=1    # default
```

Hyperfine outputs are written to `hyperfine-all.md/json` and `hyperfine-benchmark.md/json` in the output directory.

## Bake-off results (2026-02-08)

Environment:

- Monk: git ecd1435 (dirty working tree)
- Babelfish: 1.2.1 (stdin-only CLI)
- Fish: 4.2.0
- Hyperfine: 1.20.0 (runs=10, warmup=1, disabled for this run)
- Report output: `/tmp/monk-babelfish-20260208094732`

Summary (corpus + benchmarks + integration + golden + real-world fixtures):

- Total scripts: 28
- Monk success: 28/28 (warnings on `large`, `medium`, `time-prefix`)
- Babelfish success: 18/28 (fails on `large`, `medium`, `extglob-basic`, `a2l`, `coat`, `echo-args`, `taoc`, `neofetch`, `pyramid-left`, `pyramid-right`)
- Runtime diffs where both succeeded: 1/18

Key deltas:

- `benchmark/fixtures/medium.bash`: Monk succeeds with set -e/-u/pipefail warnings; babelfish exits 1 with a `UnaryArithm` AST dump on stderr
- `benchmark/fixtures/large.bash`: Monk succeeds with set -e/-u/pipefail and read IFS-splitting warnings; babelfish exits 1 with a `ForClause` AST dump on stderr
- `test/fixtures/golden/extglob-basic.bash`: Monk succeeds; babelfish exits 1 with `unsupported: &syntax.ExtGlob`
- `test/fixtures/realworld/pyramid-left.bash` and `test/fixtures/realworld/pyramid-right.bash`: Monk succeeds; babelfish exits 1 (unsupported C-style for loop)
- `test/fixtures/realworld/a2l.bash`, `test/fixtures/realworld/coat.bash`, `test/fixtures/realworld/taoc.bash`: Monk succeeds; babelfish exits 1 (unsupported `DeclClause` / readonly parsing)
- `test/fixtures/realworld/echo-args.bash`: Monk succeeds; babelfish exits 1 (unsupported `UnaryArithm`)
- `test/fixtures/realworld/neofetch.bash`: Monk succeeds; babelfish exits 1 (unsupported `ParamExp` with indexed/default expansion)
- `test/fixtures/realworld/version-compare.bash`: outputs and stderr differ (both translations trigger `test` argument errors under fish)

Notes:

- Babelfish expects input on stdin; passing a file path yields no translation output.
- For scripts where both tools succeeded (excluding the diffs above), stdout, stderr, and exit codes matched.
- Hyperfine results are omitted for this full run because babelfish fails on multiple fixtures; including those timings would bias comparisons.

Compatible subset timing (both tools succeed):

- Compatible file list: `scripts/bakeoff-compatible.txt`
- Report output: `/tmp/monk-babelfish-compatible-20260208094832`
- Scripts: 18 (Monk 18/18, Babelfish 18/18, diffs: `test/fixtures/realworld/version-compare.bash`)

Hyperfine (CLI translation batches, compatible subset):

- All fixtures: Monk `2.470 ± 0.080 s` (min 2.321, max 2.539), Babelfish `22.9 ± 0.8 ms` (min 22.1, max 24.8)
- Benchmark fixtures: omitted (babelfish fails on medium/large; ignore-failure timings would bias results)

Criterion (monk-benchmark):

- `small`: mean 506.4 μs
- `medium`: mean 2.750 ms
- `large`: mean 3.414 ms

## Where Monk is currently stronger

- **Side-effect preservation** in expansions and arithmetic (`&&`, `||`, `?:`)
- **Warnings and inline notes** for lossy translations
- **Recursive `source` inlining** with argv preservation
- **Property tests, golden tests, and integration checks** baked into CI

## Where babelfish may be stronger

- **Simplicity**: quick install and low friction for small scripts
- **Mature ecosystem**: long-standing tool with broad user exposure
- **Readable output**: often short and direct translations

## Open questions

- Are there categories where babelfish consistently preserves semantics better?
- For which Bash features does Monk's "notes + best effort" still fall short?
- What gaps are most impactful for our user base?

---

If you run the bake-off, please capture results in this document or link a separate report.
