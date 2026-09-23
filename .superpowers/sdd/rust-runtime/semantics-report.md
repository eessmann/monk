# Rust runtime semantics implementation

Implemented complete byte-preserving ports of Integer, Fields, Printf, Pattern, and Expansion under `runtime/src/semantics/`, with the agreed Output/dispatch API. Patterns retain cross-fragment quoting, C-locale classes, glob separator spelling, symlink handling, and shortest/longest removal. Integer arithmetic wraps explicitly, including parse accumulation, MIN/-1 division, and modular exponentiation. Shift counts use a private non-generic pattern type constrained to 0..=63; the conversion boundary is tested under Miri. No old-runtime fallback or interpreter exists.

Also implemented delegated `read.rs` and `directory.rs`. Reads retain per-byte escape protection, avoid read-ahead, preserve shared offsets, and provide `descriptor_interruptible` so owner lifecycle checks run after EINTR without losing a partial record. Directory APIs use byte paths, preserve diagnostic/error text, and leave cwd unchanged. `physical()` includes its trailing newline, matching the previous operation.

## Verification evidence

- Recorded initial test-first missing-module/function failures; corrected runtime assertion failure for printf numeric diagnostics before rerunning. Recorded separate failing regressions before interrupt callback and ShiftCount implementation.
- Nine initial semantic groups passed, including independent arithmetic/echo/printf/IFS comparisons with exact project reference Bash 5.3.9. ShiftCount adds a tenth semantic group.
- Eight focused read/directory groups pass, including escaped separators, shared file offset, NUL/continuation/limit, final scalar remainder, interrupt continuation/termination, directory diagnostics and lexical bounds.
- Preliminary differential replay: 1,413 successful frozen Haskell comparisons (integer 250, split 250, pattern match 250, pattern trim 250, echo 150, glob 13, expansion 250).
- Durable `runtime/tests/semantic_parity.rs`: 1,262 additional comparisons passed via `devenv shell -- env MONK_BASELINE_RUNTIME=/Users/erich/.codex/worktrees/monk-rust-runtime/monk/artifacts/rust-runtime-baseline/monk-runtime cargo test -p monk-runtime --test semantic_parity -- --ignored --nocapture` (28.02 seconds). This suite is explicitly opt-in because it requires the frozen pre-migration executable.
- `devenv shell -- cargo miri test -p monk-runtime --test semantic_miri`: 11/11 passed using final nightly 2026-09-23. Includes actual bounded SourceFd, DescriptorMask, and ShiftCount conversion boundaries. Test target reincludes pure modules and avoids the native pre-main constructor; external Bash process checks are excluded only under Miri and run in ordinary tests.
- `devenv shell -- cargo test -p monk-runtime --test type_guarantees`: passed. Successful downstream positive controls precede nine compiler-error-checked negative cases: SourceFd/DescriptorMask forgery, transferred OwnedFd reuse, borrowed fd outliving owner, borrowed-to-owned conversion, prepared-launch reuse, running-child reuse after completion, completed-child wait, and endpoint-lease reuse after transfer. Uses actual library metadata and real API types, without surrogate types.
- Final `devenv shell -- cargo test -p monk-runtime --lib`: 39 passed, 0 failed, 1 ignored native subprocess probe (the parent native test exercises that probe). No compile warnings.
- Owned Rust files formatted; targeted `git diff --check` passed.

## Findings and limits

The first independent probe used ambient Bash 5.3.15, which drops out-of-range Unicode echo escapes on Darwin. Project-pinned Bash 5.3.9 retains the spelling and agrees with frozen Haskell/Rust. The restored regression includes that case and uses `MONK_REFERENCE_BASH`; no compatibility behavior was weakened to accommodate the ambient version.

Printf's Haskell `Read Int64` accepts base prefixes, parentheses, Latin-1 NBSP whitespace, and wrapping oversized integers before rejecting noncanonical decimal spelling. Rust preserves the exact distinction between malformed and noncanonical errors using bytes, with dedicated regressions.

This agent's execution evidence is aarch64 Darwin. Linux pathname/runtime execution remains for root validation. Darwin rejected creation of an invalid-UTF8 filename in the temporary fixture; glob fixture verification used valid UTF8 bytes, while byte matching/splitting/output tests include 0xff. Miri validates pure/type boundaries, not native fork/exec or filesystem syscalls. No commits made.
