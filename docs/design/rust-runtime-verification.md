# Rust runtime migration verification

The 2026-09-23 migration replaces the runtime in Cargo while retaining the
Haskell compiler and its pure integer specification. ABI 2, `bash53-i64`, operation
spellings and admission boundaries remain unchanged. The isolated implementation
starts from `9d0cf0d`; no publication, merge or remote Git operation is implied.

## Baseline and native conformance

The pre-migration Haskell executables were copied and hashed before the cutover.
All 15 runtime suites passed with the frozen runtime, Bash 5.3.9 and Fish 4.6.0.
The first direct-output attempt lacked the baseline runtime on PATH; the unchanged
baseline passed after correcting that environment. Raw logs remain under
`artifacts/rust-runtime-baseline/` in the implementation checkout.

The optimized Cargo release binary also passes all 15 suites: protocol,
descriptors, direct-output, exec, expansion, native-launcher, pattern-parts,
portable, printf, process-substitution, read, session, signals,
directory-signals and callback-diagnostics. The executable and its hash are
frozen alongside logs under `artifacts/rust-runtime-clap/` (SHA-256
`5174a6e287204bf63c71e14b7d6d97ef0017ac467a2e3dfe7cacf3b8529a27a0`).
Earlier `rust-runtime-final/`, `rust-runtime-release/`,
`rust-runtime-accepted/` and `rust-runtime-complete/` directories retain separately
identified intermediate verification snapshots. These checks include
all eight standard-stream masks, shared descriptor offsets, high descriptors,
removed/renamed cwd, repeated waits, background survival, real SIGPIPE and
managed provider capture. The separate Haskell digest specification is retained
in `compiler-support-test`; digest is not a runtime ABI operation.

## Type, byte and lifecycle checks

Rust tests cover checked pattern-type bounds, arbitrary bytes, framing empties,
wrapping arithmetic, escaped read separators and interrupt preservation. Fourteen
negative compilation cases have successful positive controls: lifetime escape,
owned-descriptor reuse, ownership forgery, private scalar construction, repeated
launch/endpoint transfer, repeated child completion and waiting on a completed
child, unvalidated cwd descriptors and cwd borrows outliving ownership.
The native module and raw descriptor adoption/signal helpers are private.
A small capability facade exposes checked ownership types; negative controls
also prevent accessing raw adoption and signal authority through that facade.
The test builds real Cargo consumers in isolated directories; a positive control
prevents missing or incompatible metadata from masquerading as type rejection.
Miri runs isolated pure/type modules without executing native constructors.

The Clap CLI has two ordinary black-box regression tests plus an explicit
224-case argv/input comparison against the frozen Haskell executable. These
cover malformed headers, raw invalid-UTF8 arguments, literal `--`, flag spellings,
empty arguments and diagnostic order. That comparison also corrected a missing
`launch` operand diagnostic without changing native launch failure handling.

Differential runs compared 1,413 initial and 1,262 durable replay cases against
the frozen Haskell runtime, with independently executed pinned Bash checks.
Ambient Bash 5.3.15 has different out-of-range Unicode echo behavior on this
machine; acceptance uses the project's exact Bash 5.3.9 reference.

Additional subprocess/failure tests check evaluator death during blocking reads,
FIFO opens, captures and partial requests; a signal arriving before a blocking
call; inherited blocked SIGCHLD; malformed descriptor manifests; and cleanup
after an EMFILE failure halfway through SCM_RIGHTS duplication. Child status
observation does not reap: the job owner retains that authority.

Independent review found and corrected an authentication deadline that could be
extended by trickling bytes, and an enum-based wait decoder that could lose Linux
real-time signal statuses. A 63-second comparison against the frozen Haskell
baseline and earlier Rust snapshot `37ba79f…` confirmed capsule cleanup despite
a byte arriving at 50 seconds. The raw wait
decoder preserves integer signal numbers; Linux execution is still a separate
open gate. A malformed ancillary test also exposed rustix 1.1.5's Darwin handling
of truncated control buffers. The receiver now allocates the supported kernels'
full descriptor-message bound before enforcing Monk's four-descriptor limit,
and tests that every rejected alias is closed.

## Build, package and outstanding evidence

The exact nightly is `2026-09-23`, selected from the complete rust-overlay
manifest and fetched through devenv. Cargo, devenv, Nix and CI consume the same
pin; rustix 1.1.5, nix 0.31.3 and Clap 4.6.7 are locked. Production/build
fingerprints include Rust sources, tests, Cargo files, the pin and generated
metadata, while excluding build artifacts. The ABI generator has a committed-output
drift check. It generates 90 namespaced CLI, session, integer, pattern and
compiled-body opcodes for Haskell and Rust; Rust dispatch consumes those constants. Ten malformed-input/drift controls verify
the generator. Devenv uses the documented
[`languages.rust.toolchainFile`](https://devenv.sh/languages/rust/) integration;
Cabal remains a Simple build and the devenv commands orchestrate both builders.

The final Darwin Nix package passes all 15 ABI suites plus child transport, and
its attestation verifies execution, check receipts and permitted Apple linkage.
Its packaged executable SHA-256 is
`0d197973592bacbaa20c3291416c25c48375ef091f0d18dfac08f6b42ebc639c`;
the Nix-built package has its own executable identity. Packaging adjusts Darwin
linkage and signs the copied executable.

The final Clap source also passes `devenv shell -- monk-integration`: 980 main
Haskell tests, 38 publication tests, 63 tooling tests, compiler-support checks,
all 15 ABI suites, digest, child transport and public boundaries. Cargo formatting,
strict Clippy, HLint, Ormolu, metadata drift and package checks pass. Haddock built
successfully on the earlier Haskell snapshot, before the generated opcode tables
expanded; its nonfatal documentation/link-target warnings are recorded separately.

The final source archive
`0f9784c8c9ac57be00786724cde33a537b6e39baaf5590b620002e7dc8ca6c8f`
contains the Clap lock, CLI tests, generated metadata and final performance JSON.
Unpacked locked Cargo tests/release build, Cabal build/install and strict combined
and managed-provider Unicode Bash/Fish smoke checks pass. The
[build receipt](../evidence/rust-runtime-build-2026-09-23.json) identifies the
archive snapshot; later evidence annotations are outside that tarball.

Native validation is on aarch64 Darwin. Both Linux musl targets pass local
all-target `cargo check`; Linux linking, release binaries and native execution
remain unverified. Nix derivation evaluation is not native execution. Native
x86_64/aarch64 Linux and minimum-supported-macOS execution remain incomplete
acceptance gates. The user prohibits use of ccs-ci; no Monk build, test or source
transfer ran there. Historical translator/performance evidence remains historical and is
not silently attributed to this runtime.

The frozen historic 95-fixture comparison retains 74 default and 77 stable
matches, with zero admitted mismatches or changed admission results. Its provider
hash identifies the earlier release snapshot used for that comparison; the final
cwd capability refinement, private native boundary, generated opcode use and
CLI diagnostic correction have their own native conformance checks. The legacy
collector's separate process-effect fixtures use an incompatible helper protocol,
so neither their timeout nor their raw matching helper failures count as acceptance evidence. Current native process-substitution
checks do exercise producer/consumer bytes, FIFO-controlled waits and a
300,000-byte producer terminating with SIGPIPE. They do not replace an independent
full-stream replay of those exact legacy fixtures.

Final build/package, source installation, coverage and performance receipts are
recorded in `docs/evidence/rust-runtime-*-2026-09-23.json`. Performance compares
the frozen Haskell and Rust binaries separately from semantic acceptance.

## Local performance measurements

The final Clap runtime is 1,290,640 bytes, compared with 27,848,096 bytes for the
frozen Haskell executable: a 95.4% reduction in executable size. With the same
frozen compiler for both providers, representative elapsed times were:

| Workload | Haskell median (ms) | Rust median (ms) | Rust / Haskell |
|---|---:|---:|---:|
| Startup `--describe` | 13.651 | 7.265 | 0.532 |
| Arithmetic loop, 32 iterations | 3957.126 | 582.517 | 0.147 |
| Byte echo/printf, eight iterations | 679.641 | 133.409 | 0.196 |
| Supervised read/pipeline | 696.287 | 149.613 | 0.215 |
| Process substitution | 597.114 | 130.474 | 0.219 |

Each workload used three warmups and 20 alternating paired samples with competing
builds paused. All 200 measured invocations matched stdout, stderr and status;
the four script fixtures also matched independently executed pinned Bash during
preparation. These are warmed measurements on one Apple M1 Pro running macOS 27.0,
including process launch and stream capture. They do not establish Linux
performance or replace the historical aggregate performance gate.

The [performance receipt](../evidence/rust-runtime-performance-2026-09-23.json)
records every sample and all provider/input hashes. The
[verification receipt](../evidence/rust-runtime-verification-2026-09-23.json)
and [coverage receipt](../evidence/rust-runtime-coverage-2026-09-23.json)
retain the exact scope of each semantic claim.
