# Monk and Babelfish

Monk and Babelfish translate Bash into Fish, but they make different promises.
Monk admits programs within an explicit execution contract and rejects constructs
it cannot preserve. Its current implementation combines a Haskell compiler with
a Rust runtime. Babelfish is a syntax-directed translator that parses Bash
with `mvdan.cc/sh` and emits Fish by walking the syntax tree. Its upstream README
acknowledges incomplete handling of variables and arithmetic.
See [Babelfish upstream](https://github.com/bouk/babelfish).

This comparison describes the project as of 2026-09-23. It separates observed
agreement with Bash, admission restrictions and performance. A successful
translation alone is not a correctness result. The tested Babelfish version is
1.2.1; the Monk executables are built from the current Rust implementation.

## What Monk preserves

Monk uses ShellCheck's Bash parser, a private semantic plan and a structural
Fish DSL. Admission checks precede rendering: an unsupported program produces
structured diagnostics and no translated program. Both normal and strict mode
reject unsupported behavior. Named approximations require explicit opt-in;
strict mode forbids them.

The execution profile fixes UTF-8 source, C locale, Bash 5.3 signed-64-bit
arithmetic and Fish 4.6 or newer. Runtime values are NUL-free byte strings rather
than necessarily UTF-8 text. Arithmetic folding retains an independent Haskell
specification. Generated Fish owns program control flow and variables; the
runtime supplies exact byte operations and supervises resources when required.

| Area | Current Monk contract |
| --- | --- |
| Words and arithmetic | Quote-sensitive field cardinality, IFS splitting, pathname and pattern expansion, wrapping signed-64 arithmetic and ordered effects, within the admitted contexts. |
| State and control flow | Dense indexed arrays, finite function dispatch, compatible dynamic local storage, immutable acyclic sources and finite compile-time-parsed `eval`. |
| Processes and I/O | Ordered descriptor operations, byte reads, concurrent pipelines, owned background jobs, cached waits and bounded process-substitution lifetimes. |
| Callbacks and directories | Compiled standalone EXIT/ERR handlers; directory operations under an explicit stable-directory contract where required. Restrictions on their combinations still apply. |
| Sourceable programs | A separate versioned caller contract declares permitted scalar, function and directory effects. Standalone results do not establish sourceable behavior. |

These are conditional capabilities, not blanket syntax support. Sparse or
associative arrays, arbitrary runtime `eval`, unknown command dispatch,
recursive call graphs, interactive job control, arbitrary signal callbacks and
escaping process-substitution paths remain outside the admitted surface.
The [semantic audit](design/translator-audit.md) and
[execution profile](design/execution-profile.md) define the precise boundaries.

The Rust 2024 runtime preserves ABI 2 and the `bash53-i64` profile. It uses pinned
nightly Rust, private bounded pattern types, consuming state transitions and
RAII ownership for descriptors, cwd capabilities, children and endpoint leases.
Rustix, nix and private libc bindings provide native operations. Those types
prevent classes of resource misuse; byte-level comparisons and process tests
remain necessary to establish shell behavior.

Standalone generated programs enter through
`monk-runtime --abi 2 launch SCRIPT [ARGS...]`. The launcher observes missing
standard streams before Fish can reserve them. Exact output may therefore
require the native writer even for a short greeting. Running generated output
directly with Fish is not an equivalent test of the descriptor contract.
Managed publication also captures an immutable runtime provider, so a later
provider replacement cannot silently change an installed program.

## Current agreement with Bash

The current comparison uses the frozen 95-fixture corpus defined in
[`test/evidence/comparison-corpus.json`](../test/evidence/comparison-corpus.json).
It includes benchmark, golden, integration, real-world and semantic regression
inputs. The denominator stays fixed; separate effect probes do not enlarge it.

Fresh runs on aarch64 Darwin used Bash 5.3.9, Fish 4.6.0, Babelfish 1.2.1 and
Monk's final Clap-enabled Rust release. The newly built runtime has the same
SHA-256 as the retained native verification and performance binary. The
[current comparison receipt](evidence/babelfish-comparison-2026-09-23.json)
records the compiler, runtime, collector, corpus and build identities, with
per-fixture results for both lanes.

| Provider / contract | Translation succeeded | Matched Bash | Admitted mismatches | Rejected / failed translation |
| --- | ---: | ---: | ---: | ---: |
| Monk, default | 74 | 74 | 0 | 21 |
| Monk, stable directories | 77 | 77 | 0 | 18 |
| Babelfish, either lane | 58 | 26 | 32 | 37 |

Every Bash reference execution in historic95 completed. There were no
translation timeouts or unavailable admitted executions for Monk or Babelfish
in those headline rows. A completed program can intentionally return a nonzero
status; matching that status is part of the comparison.

A match means identical **stdout bytes, stderr bytes and exit status** against
an independently executed Bash reference for the recorded source, arguments,
stdin and environment. Streams are neither decoded nor normalized. This does
not prove equivalence for other inputs or all filesystem, caller or process
state. Nor is the fraction of this selected corpus a percentage of Bash
language compatibility.

The default and stable-directory rows are different contracts. Stable mode
requires caller guarantees including empty `CDPATH` and stable logical cwd
ancestry; it does not discover or prove those conditions at runtime. Babelfish
has no corresponding selector and is rerun in each lane under the same
controlled environment.

The default lane has 20 matches shared by both tools, 54 matched only by Monk
and six matched only by Babelfish. Under the stable-directory contract the
shared set grows to 23, leaving three Babelfish-only matches:
`case-pattern-expansion-glob`, `read-prompt` and `time-prefix`. The three extra
default-lane Babelfish matches are `cd-tmp`, `pwd-cd` and `pushd-popd`.
These are observed fixture results, not broader feature guarantees for either
tool.

Examples where Monk matches and Babelfish differs include nondefault IFS,
adjacent quoted `$@`, delimiter-read flags and `pipefail`. Monk also matches
`arith-short-circuit` and `background-fail-wait`, which Babelfish rejects.
This makes the comparison more informative than counting emitted programs:
broader translation admission can include behavior that differs from Bash,
while conservative rejection can exclude individual programs another tool
handles correctly.

Separate default-lane probes match filesystem bytes/modes and sourceable caller
state, one case each. Four strengthened cases match populated `read -a`, EOF
without newline, NUL/invalid-byte input and dense-array append; visible timing
is explicitly rejected. These results do not change the denominator of 95.
The version-1 caller probe is incompatible with the stable-directory option,
so its stable-lane rejection supplies no caller-state acceptance evidence.

The two additional process fixtures provide no acceptance evidence: both use
command heads outside Monk's proved dispatch envelope, and the handshake
reference additionally fails in its test worker. Native process and descriptor
suites remain separate evidence. Matching helper errors or translation
rejections do not establish successful process execution.

## Performance after the Rust replacement

The retained performance experiment compares **Monk's Rust runtime with its
previous Haskell runtime**, using the same frozen compiler for both. It is
separate from the Babelfish correctness comparison above.

| Workload | Haskell median (ms) | Rust median (ms) | Rust / Haskell |
| --- | ---: | ---: | ---: |
| Runtime startup, `--describe` | 13.651 | 7.265 | 0.532 |
| Arithmetic loop, 32 iterations | 3957.126 | 582.517 | 0.147 |
| Byte echo/printf, eight iterations | 679.641 | 133.409 | 0.196 |
| Supervised read/pipeline | 696.287 | 149.613 | 0.215 |
| Process substitution | 597.114 | 130.474 | 0.219 |

The Rust executable is 1,290,640 bytes, compared with 27,848,096 bytes for the
Haskell baseline: 95.4% smaller. Each workload used three warmups and 20
alternating paired samples on one Apple M1 Pro running macOS 27.0. All 200
measured invocations agreed on streams and status; the four script workloads
also matched independent Bash runs during preparation. Times include process
launch and stream capture. See the
[performance receipt](evidence/rust-runtime-performance-2026-09-23.json)
for identities, every sample and measurement limits.

These warmed local measurements establish neither a current speed ranking
against Babelfish nor Linux performance. There is no current comparable
head-to-head timing, translation-throughput, generated-size or memory result.
Aggregate performance and process-launch acceptance gates remain open; smaller
binaries and faster representative operations do not close them.

## Verification and remaining gaps

The [Rust verification report](design/rust-runtime-verification.md) distinguishes
source snapshots and executable identities. Its retained evidence includes:

- All 15 ABI suites on the final Cargo release, plus native lifecycle and failure
  tests for descriptors, signals, interrupted operations and cleanup.
- 1,262 frozen-Haskell semantic comparisons, 224 CLI comparisons and independent
  pinned-Bash checks; 11 suitable Miri tests and 14 negative type cases with
  successful positive controls.
- Local Haskell integration: 980 translator tests, 38 publication tests and
  63 tooling tests, with compiler-support and public-boundary checks.
- An arm64 Darwin package passing the ABI suites and child transport, permitted
  Apple linkage inspection, and an unpacked source build/install with combined
  and managed-provider smoke checks.

This is native aarch64 Darwin evidence. Local Cargo checks and Nix evaluation
for both Linux musl targets do not establish linked static release binaries or
native Linux behavior. Minimum-supported-macOS execution and the remote CI
matrix also remain unverified. Invalid-byte pathname cases rejected by the local
filesystem remain platform gaps. The [roadmap](design/translator-todo.md)
tracks these acceptance limits.

## Reproducing the comparison

Use the repository's pinned devenv environment and build both products with
`devenv shell -- monk-build`. Cargo is the sole runtime builder; Cabal builds the
compiler and Haskell tooling. Use the pinned Bash and Fish binaries recorded in
the evidence, and identify Babelfish by its binary digest and package receipt;
the captured provider is byte-identical to the Nix `babelfish-1.2.1` package.
Its CLI does not provide a `--version` interface.

The exact collector is `monk-tool evidence portable-comparison`. First freeze a
fresh copy of the corpus, then measure it against independent Bash executions.
Run each directory contract separately. Its output directories are immutable:
choose a new path for every run.

```bash
# Run inside devenv; monk-build does not install the Cabal executables.
TOOL="$(cabal list-bin exe:monk-tool)"
"$TOOL" evidence portable-comparison freeze \
  --repo "$PWD" --output "$FROZEN" --directory-contract default

"$TOOL" evidence portable-comparison measure \
  --frozen "$FROZEN" --output "$RUN" --directory-contract default \
  --original "$ORIGINAL_MONK" --current "$BASELINE_MONK" \
  --current-runtime "$BASELINE_RUNTIME" \
  --candidate "$MONK" --runtime "$RUST_RUNTIME" \
  --babelfish "$BABELFISH" --bash "$REFERENCE_BASH" --fish "$REFERENCE_FISH" \
  --stage final --build-receipt "$BUILD_RECEIPT" \
  --source-fingerprint "$PRODUCTION_FINGERPRINT"
```

The variables denote explicit paths to verified executables and fresh output
locations. The collector also retains original and pre-Rust baseline rows for
migration checks, so their genuine frozen binaries are required; replacing them
with the candidate would invalidate that experiment. They are local verification
artifacts, not binaries bundled with a source installation.

Capture `BUILD_RECEIPT` with `"$TOOL" evidence verification --output DIR
--binary MONK --binary RUST_RUNTIME -- BUILD_COMMAND`; use its
`production_after.sha256` as `PRODUCTION_FINGERPRINT`. The command must actually
build the candidate, and the wrapper checks that inputs remain unchanged and
records executable hashes. The comparison copies providers, hashes fixtures,
records generated programs and raw observations, and uses the ABI 2 launcher
for standalone candidate output. Older providers retain their original Fish entry
paths; sourceable probes retain a dedicated caller wrapper.

The frozen corpus preserves one historical background-job probe verbatim;
`monk-tool` supplies a narrowly scoped Haskell adapter for its exact `python3 -c`
invocation and records the adapter hash. Current process-effect probes use native
Haskell workers. No Python runtime is required. The original three extra
arithmetic benchmark inputs are unavailable; their identities remain marked
missing in the corpus and do not contribute to the 95-fixture result.

Superseded reports and raw attempts have been purged. The current comparison,
the four Rust verification receipts and their supporting local artifacts remain;
canonical test inputs live under `test/evidence/`. Historical corpus membership
is test data, not a retained performance claim. Run `cabal test monk-tool-test`
for collector validation, including rejection of altered fixture hashes,
metadata, stdin, ordering and cohort membership.
