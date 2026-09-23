# Replace monk-runtime with typed Rust 2024

User-approved implementation plan, 2026-09-23. Base: 9d0cf0d.

## Contract

Replace the complete Haskell/C runtime, retaining executable name, ABI 2,
`bash53-i64`, all admitted operations, raw byte transport and generated Fish
behavior. Keep translation, structural Fish DSL, admission and publication in
Haskell. Preserve the pure integer specification for folding and differential
verification. Correctness and ownership determine acceptance; timings are a
separate measurement.

## Implementation sequence

1. Freeze the existing executable and baseline tests. Add a Cargo resolver 3
   workspace, edition 2024 crate, locked rustix 1.1.5/nix 0.31.3/libc dependencies,
   and newest complete dated nightly. Generate shared committed ABI tables from
   one declarative source with a drift check.
2. Port integer, bytes, fields, printf, pattern, glob and expansion. Preserve
   arbitrary bytes, NUL framing, empty values, wrapping and evaluation boundaries.
3. Port launches, exec replacement, child capture, descriptor scopes, read/write,
   sessions, jobs/pipelines, substitutions and guardians using typed owned/borrowed
   descriptors and consuming transitions. Keep unsafe at private native boundaries
   and checked pattern conversions. Pre-main capture preserves original streams
   and signal dispositions; bounded fork children never allocate or unwind.
4. Make Cargo the sole production runtime. Remove Haskell execution and C sources
   after conformance; retain small compiler support for Integer, Digest, metadata
   and provider validation. Explicitly orchestrate Cargo+Cabal in devenv and CI.
5. Update source archives, static-musl/Apple packaging, docs and fingerprints.
   Compare captured Haskell runtime with final release binaries for size, startup
   and representative script execution.

## Acceptance

Run positive-controlled compile-fail tests, isolated Miri, frozen Haskell and
independent Bash parity, all retained native suites, failure injection, all
stdio masks, ignored INT/QUIT, SIGPIPE, high descriptors, removed cwd, repeated
waits, background survival and guardian cleanup. Check format, Clippy, Haskell
quality, source installation and managed provider capture. Preserve accepted
translation coverage. Native Linux and minimum-macOS evidence must remain
explicitly incomplete if unavailable; never promote cross compilation to
execution evidence. The user prohibits ccs-ci.

## Outcome and receipts

The implementation ledger is `.superpowers/sdd/rust-runtime/progress.md`.
The architecture and current evidence are documented in
`docs/design/rust-runtime-verification.md`; durable receipts are under
`docs/evidence/rust-runtime-*-2026-09-23.json`.
