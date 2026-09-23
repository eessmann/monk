# Rust runtime migration - approved implementation contract
Workspace: /Users/erich/.codex/worktrees/monk-rust-runtime/monk
Base: 9d0cf0d. User approved complete Haskell/C runtime replacement. Preserve every ABI2 operation and bash53-i64 behavior, raw byte framing, statuses/signals, typed Haskell compiler and publication. Rust2024 pinned nightly, rustix1.1.5, nix0.31.3, libc crate bindings. All project runtime code Rust. Private non-generic pattern_types for bounded scalars + typestate/RAII. Pure Haskell Integer remains compiler spec, three-way parity. No source interpreter. Existing tests are authoritative regressions; record a failing test before each behavior implementation, rerun relevant suites. No weakened tests or capability claims. No C retained. Native seam preserves pre-main descriptor/signal capture and std reservations, no postfork allocation/unwinding, direct spawn/exec with explicit PATH (never spawnp/execvp shell fallback), positive errno spawn errors, reversible failed exec. All descriptors immediately owned on acquisition and malformed SCM_RIGHTS rollback.
Use explicit workspace paths. It lies outside initial writable root; mutation commands may need require_escalated for already authorized worktree writes. Do not edit main checkout. No commits unless root requests; independent files avoid shared index contention. Read AGENTS.md and source modules for exact behavior. Current executables are available under main checkout dist-newstyle; root freezes baseline.

## File ownership
- root: runtime/src/{lib,main,protocol,session,transport,capsule,read,directory}.rs; ABI metadata/generator unless build agent takes it by message; plan/ledger, integrated tests/docs.
- semantics implementer: runtime/src/semantics/** only (integer, fields, printf, pattern, expansion and dispatch). Standalone tests there. Do not modify manifest/lib.rs except message root.
- native implementer: runtime/src/{native,types,launch,child,exec}.rs (or matching subdirectories) and native tests; coordinate interfaces below before full implementation.
- build implementer: Cargo manifests/lock/toolchain/config, devenv/Nix/CI, Cabal support split/sdist/evidence scanning. Do not delete old runtime until root confirms conformance; stage removal last.

## Common interfaces
Bytes=Vec<u8>; Frames=Vec<Bytes> (root protocol module). semantic errors Result<T,Vec<u8>>; syscall errors std::io::Error, no generic error panics.
semantics::Output { Bytes(Vec<u8>), Frames(Vec<Vec<u8>>), Status(i32) }
semantics::dispatch(op:&str, frames:&[Vec<u8>]) -> Result<Option<Output>,Vec<u8>>; None if not handled. Export fields::split_fields, fields::echo_bytes, printf::printf_bytes for read/session builtin paths (coordinate exact signatures).
protocol::decode(&[u8])->Result<Vec<Vec<u8>>,Vec<u8>>; encode(&[Vec<u8>])->Vec<u8>.
Native implementer must promptly publish actual API contract (Streams, typed SourceFd, RunningChild/ProcessOutcome, spawn, initial streams, signals). Preserve owned-vs-borrowed fds; root adapts session to it. Native implementer owns launch/exec/child, root owns session/transport/capsule/read/directory. Coordinate child-in-session callback before binding.

## Verification
Use project's existing monk-tool runtime check suites against explicitly selected runtime, fresh Cargo unit/type tests, Haskell tests, package/source-archive tests. Baseline must stay unchanged. Report platform gaps. Do not run benchmarks in parallel with builds. No global toolchains: use devenv languages.rust toolchainFile and pinned rust-overlay as documented by https://devenv.sh/languages/rust/.
