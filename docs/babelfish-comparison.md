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

- Monk: git 55c207e (dirty working tree)
- Babelfish: 1.2.1 (stdin-only CLI)
- Fish: 4.2.0
- Hyperfine: 1.20.0 (runs=10, warmup=1)
- Report output: `/tmp/monk-babelfish-20260208200358`

Summary (corpus + benchmarks + integration + golden + real-world fixtures):

- Total scripts: 35
- Monk translation success: 35/35 (warnings on 11 scripts; `neofetch` produced many)
- Babelfish translation success: 23/35
- Runtime diffs where both succeeded: 6/23

Babelfish translation failures (exit 1):

- `benchmark/fixtures/medium.bash`: `UnaryArithm` AST dump (postfix arithmetic)
- `benchmark/fixtures/large.bash`: `ForClause`/C-style loop AST dump
- `test/fixtures/integration/arith-short-circuit.bash`: non-zero exit (no stderr)
- `test/fixtures/integration/read-flags.bash`: indexed parameter expansion (`${arr[0]}`) AST dump
- `test/fixtures/golden/extglob-basic.bash`: `ExtGlob` AST dump
- `test/fixtures/realworld/a2l.bash`, `coat.bash`, `taoc.bash`: `DeclClause` / `readonly` parsing
- `test/fixtures/realworld/echo-args.bash`: `UnaryArithm` AST dump
- `test/fixtures/realworld/neofetch.bash`: `ParamExp` with index/default expansion
- `test/fixtures/realworld/pyramid-left.bash`, `pyramid-right.bash`: C-style `for ((...))` loop

Runtime diffs (both translated):

- `errexit-basic` / `errexit-andor` / `errexit-conditionals`: babelfish emits `set -e`, which fish treats as `set --erase` and errors at runtime; Monk uses `cmd; or exit $status` best-effort emulation.
- `pipefail-basic` / `pipefail-toggle`: babelfish emits `set -o pipefail` (fish error). Monk uses a helper (`__monk_pipefail`) and runs clean on these fixtures.
- `version-compare`: babelfish emits `test ... ==` which fish rejects; Monk emits a different comparison, so stdout differs.

Notes:

- Babelfish expects input on stdin; passing a file path yields no translation output.
- For scripts where both tools succeeded (excluding the diffs above), stdout, stderr, and exit codes matched.

Performance (hyperfine, full corpus; babelfish run ignores non-zero exits):

- All fixtures: Monk `5.947 ± 0.166 s` (min 5.690, max 6.135), Babelfish `0.008 ± 0.000 s` (min 0.007, max 0.009)
- Benchmark fixtures: Monk `431.4 ± 15.6 ms` (min 420.3, max 462.8), Babelfish `5.0 ± 0.1 ms` (min 4.9, max 5.1). The benchmark set includes `medium/large`, which babelfish fails; treat these timings as throughput only.

Compatible subset timing (both tools translate):

- Compatible file list: `scripts/bakeoff-compatible.txt`
- Report output: `/tmp/monk-babelfish-compatible-20260208200358`
- Scripts: 18 (Monk 18/18, Babelfish 18/18)
- Runtime diff: `test/fixtures/realworld/version-compare.bash` (stdout/stderr differ)

Hyperfine (CLI translation batches, compatible subset):

- All fixtures: Monk `2.519 ± 0.074 s` (min 2.419, max 2.606), Babelfish `22.9 ± 0.6 ms` (min 22.0, max 23.9)
- Benchmark fixtures: Monk `435.6 ± 18.6 ms` (min 410.2, max 463.8), Babelfish `5.0 ± 0.1 ms` (min 4.9, max 5.1). Benchmark set still includes `medium/large`; interpret with caution.

## Where Monk is currently stronger

- **Side-effect preservation** in expansions and arithmetic (`&&`, `||`, `?:`) on the current fixtures
- **Broader construct coverage** in this corpus (C-style loops, extglobs, indexed expansions), with warnings when behavior is approximate
- **Warnings and inline notes** for lossy translations, plus `--strict` to fail fast
- **Recursive `source` inlining** with argv preservation
- **Property tests, golden tests, and integration checks** baked into CI

## Where Monk is currently weaker / tradeoffs

- **Performance**: translation throughput is much slower than babelfish in the bake-off.
- **Verbosity**: output is more scaffolding-heavy (`string join`, `begin` blocks) to preserve list and expansion semantics.
- **Errexit/pipefail emulation**: best-effort and still imperfect, especially around bash exceptions and subtle command substitution rules.
- **Semantic gaps remain**: word splitting, glob differences, and fish-only behaviors still require manual review.

## Where babelfish is be stronger

- **Simplicity**: quick install and low friction for small scripts
- **Performance**: very fast translation throughput
- **Readable output**: often short and direct translations that are easy to edit by hand
- **Mature ecosystem**: long-standing tool with broad user exposure

## Open questions

- Are there categories where babelfish consistently preserves semantics better?
- For which Bash features does Monk's "notes + best effort" still fall short?
- What gaps are most impactful for our user base?
- Do we need more precise `set -e`/`pipefail` parity for condition lists and command substitutions?

---

If you run the bake-off, please capture results in this document or link a separate report.
