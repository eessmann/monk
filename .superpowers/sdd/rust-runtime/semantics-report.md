# Rust runtime semantics implementation

Implemented complete byte-preserving ports of Integer, Fields, Printf, Pattern, and Expansion under `runtime/src/semantics/`, with the agreed Output/dispatch API. Patterns retain cross-fragment quoting, C-locale classes, glob separator spelling, symlink handling, and shortest/longest removal. Integer arithmetic wraps explicitly, including parse accumulation, MIN/-1 division, and modular exponentiation. Shift counts use a private non-generic pattern type constrained to 0..=63; the conversion boundary is tested under Miri. No old-runtime fallback or interpreter exists.

Also implemented delegated `read.rs` and `directory.rs`. Reads retain per-byte escape protection, avoid read-ahead, preserve shared offsets, and provide `descriptor_interruptible` so owner lifecycle checks run after EINTR without losing a partial record. Directory APIs use byte paths, preserve diagnostic/error text, and leave cwd unchanged. `physical()` includes its trailing newline, matching the previous operation.

## Current verification

The final integrated executable and exact check scope are recorded in
`docs/evidence/rust-runtime-verification-2026-09-23.json`.

- The durable `runtime/tests/semantic_parity.rs` replay passed 1,262 comparisons
  against the frozen Haskell runtime. It is explicitly opt-in because the
  baseline executable is a local verification artifact.
- Eleven Miri tests passed on nightly 2026-09-23, including bounded SourceFd,
  DescriptorMask and ShiftCount conversions. The target reincludes pure modules
  and avoids native pre-main constructors. Independent Bash comparisons run in
  ordinary tests, outside Miri.
- Fourteen downstream negative type cases passed with successful positive
  controls. They use actual library metadata and API types to check borrowing,
  consuming transitions, private scalar construction and private native access.
- The final library run passed 45 tests; two ignored subprocess probe entrypoints
  were exercised by their parent tests. Separate semantic, lifecycle, CLI and
  all 15 ABI suites are identified in the retained receipt.
- Cargo formatting and all-target Clippy with warnings denied passed.

Superseded intermediate counts and test-attempt logs are not current acceptance
claims. The frozen pre-migration runtime remains available for differential
replay.

## Findings and limits

The first independent probe used ambient Bash 5.3.15, which drops out-of-range Unicode echo escapes on Darwin. Project-pinned Bash 5.3.9 retains the spelling and agrees with frozen Haskell/Rust. The restored regression includes that case and uses `MONK_REFERENCE_BASH`; no compatibility behavior was weakened to accommodate the ambient version.

Printf's Haskell `Read Int64` accepts base prefixes, parentheses, Latin-1 NBSP whitespace, and wrapping oversized integers before rejecting noncanonical decimal spelling. Rust preserves the exact distinction between malformed and noncanonical errors using bytes, with dedicated regressions.

Native execution evidence is aarch64 Darwin. Linux pathname/runtime execution remains an open gate. Darwin rejected creation of an invalid-UTF8 filename in the temporary fixture; glob fixture verification used valid UTF8 bytes, while byte matching/splitting/output tests include 0xff. Miri validates pure/type boundaries, not native fork/exec or filesystem syscalls.
