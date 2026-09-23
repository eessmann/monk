# Rust runtime implementation ledger

The user-approved plan is `docs/superpowers/plans/2026-09-23-rust-runtime.md`.
The implementation is isolated on `codex/rust-runtime`, based on `9d0cf0d`.
The user subsequently authorized a signed commit and local merge into `main`.
Verification snapshots below retain their original pre-commit source identities.
Push and publication are outside this integration request.

## Implementation

1. Foundation complete: captured Haskell executable baseline, pinned complete nightly
   2026-09-23, locked Cargo workspace, generated Haskell/Rust ABI metadata and drift check.
2. Byte semantics complete: byte-preserving Rust operations, bounded pattern types,
   retained independent Haskell Integer specification, frozen differential corpus.
3. Resource runtime complete: owned and borrowed descriptors, explicit cwd capabilities,
   consuming launches and endpoint transfer, single reaping authority, scoped tables,
   signal-preserving startup/exec, sessions, jobs, pipelines and capsule guardians.
4. Production cutover complete: Cargo provides the executable; Cabal Simple builds
   the compiler and small support library. Superseded Haskell execution and C files removed.
5. Clap 4.6.7 added at the user's request with raw argument preservation and
   224 exact CLI comparisons against the frozen Haskell executable.
6. Local verification and distribution complete, with final Cargo, Darwin package,
   source-installation, integration, coverage and performance receipts.
   Native Linux and minimum-macOS execution remain incomplete gates.

## Verification snapshots

- The baseline Haskell executable passed all 15 ABI suites on aarch64 Darwin using
  pinned Bash 5.3.9 and Fish 4.6.0. The initial direct-output run omitted its runtime
  from PATH; the unchanged binary passed with the corrected environment.
- Final optimized Rust SHA-256
  `5174a6e287204bf63c71e14b7d6d97ef0017ac467a2e3dfe7cacf3b8529a27a0`
  is frozen in `artifacts/rust-runtime-clap/`. All 15 ABI suites pass.
- Explicit frozen-Haskell differential replay passes; pure/type Miri passes 11 tests.
  Cargo library and isolated semantic tests, four lifecycle integration tests plus
  the native-unit lifecycle regression, compile-fail positive controls and strict
  Clippy are exercised separately; exact final counts are in the report.
- Full final-Clap Cargo/Cabal integration passed: 980 main Haskell tests,
  38 publication tests, 63 tooling tests, compiler support, all 15 ABI suites,
  digest, child transport and public boundary checks.
- Historic coverage remains 74/95 default and 77/95 stable, with zero admitted
  mismatches or admission regressions. Raw legacy process-effect rows have an
  incompatible helper and are explicitly excluded from that acceptance claim.
- Independent review corrected a trickle-extended guardian timeout, Linux realtime
  signal decoding after wait, and a compile-fail harness choosing empty Clippy metadata.
  Review confirmed the final cwd capability constructors, lifetime and launch ownership.
  A final contract audit added the missing shared opcode tables and made the native
  seam private; compile-fail tests reject access to raw adoption/signal authority.
  The follow-up review closed both gaps without further findings.
- Failure tests cover evaluator death during blocking operations, signals before a
  blocking call, blocked inherited SIGCHLD, malformed inherited descriptor manifests,
  ancillary overflow and EMFILE midway through descriptor duplication.
- Final Darwin release package passes all 15 ABI suites plus child transport.
  Unpacked source build/install and combined/managed Unicode smoke checks pass.
  Linux musl targets pass local all-target `cargo check`;
  Linux release linking and native execution remain unverified.
- Final Clap performance measurements complete: all 200 measured invocations match
  bytes/status. Median runtime ratios are 0.532 for startup and 0.147–0.219 for
  four representative scripts; executable size is 4.635% of the Haskell baseline.
  Measurements do not alter semantic acceptance.

The user explicitly prohibits ccs-ci. No Monk source transfer, build or test ran there;
a prerequisite SSH connection timed out before any remote command. Do not retry.
