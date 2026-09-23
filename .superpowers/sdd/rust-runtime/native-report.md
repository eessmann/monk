# Native Rust runtime implementation

Owned files: `runtime/src/{native,types,launch,exec,child,main}.rs`, `runtime/src/native/tests.rs`.

Implemented native process execution using maintained libc ABI types/constants with private RAII action/attribute guards, positive POSIX spawn errno handling, explicit byte PATH search, cwd descriptor actions, and no ENOEXEC source interpretation. Source descriptor maps own `OwnedFd` capabilities and duplicate above every destination before setup. Parent descriptor, pipe, I/O and file operations use rustix; parent child reaping uses rustix raw wait status and fixed-signal operations use nix. The ignored-INT/QUIT asynchronous path prepares all strings, descriptor moves, signal actions, masks and acknowledgement pipe before fork. Its child branch performs direct libc operations, then execve or _exit; it never returns through Rust destructors.

A pre-main Rust initialization section snapshots fd0..5 and inherited ignored INT/QUIT, then retains OwnedFd null reservations for absent fd0..2. The pinned build uses `-Zon-broken-pipe=inherit`. Failed direct exec restores standard reservations, original descriptor flags, signal dispositions and mask before trying the next explicit PATH candidate or reporting the source diagnostic. Signal ownership uses atomic notification and non-restarting handlers; SIGCHLD has a separate notification consumed by the session owner. A descriptor-free heartbeat thread targets only the owning pthread every 10ms to close lost-wakeup races. The guard temporarily unblocks SIGCHLD, stops and joins the thread before restoring handlers and the original mask. EvaluatorWatch queries rustix waitid with NOWAIT; it never reaps and treats ECHILD as cancellation. Blocking native writes and acknowledgement reads stop retrying when the owner is cancelled.

SourceFd and DescriptorMask wrap private non-generic nightly pattern types. Constructors range-check before representation-preserving transmute; safe callers cannot construct invalid values. SourceFd accepts 0 through i32::MAX - 1; session manifest limits belong at adoption. Owned and borrowed OS descriptors remain distinct.

Child transport preserves raw bytes, strips captured NUL with one immediate warning, removes final newlines only after EOF, restores closed source stdio inside Fish, and removes private files before publishing a result packet. Launch propagates the actual terminating signal after owned evaluator cleanup. Session child execution is a typed callback to the session module.

CLI dispatch covers all ABI2 operations including direct-output, fixed SIGPIPE reproduction, platform byte hex validation and bounded real-pipe alias probing. Execution modules remain private behind the CLI and safe capability facade. Session-prepare decodes exactly one NUL-terminated script frame before capsule preparation.

PreparedLaunch owns every prepared byte string, cwd and descriptor; launch consumes the plan. RunningChild::complete consumes the running child and produces CompletedChild with only pid/outcome observations. Launch and child final waits use this consuming path. EndpointLease consumes its descriptor in transfer; root uses it for producer endpoint adoption into the semantic table.

Unsafe resource operations are confined to native.rs and its private native submodules. The scalar pattern wrappers separately use checked representation conversions. Descriptor inventory uses raw fcntl because absent slots cannot validly be wrapped as BorrowedFd; ordinary owned and borrowed descriptor operations use rustix.

## Verification

The final integrated Clap runtime passed all 15 ABI suites, including exec,
direct-output, signals, portable execution and the native launcher. The retained
`docs/evidence/rust-runtime-verification-2026-09-23.json` binds those checks to
release SHA-256 `5174a6e287204bf63c71e14b7d6d97ef0017ac467a2e3dfe7cacf3b8529a27a0`
and logs under `artifacts/rust-runtime-clap/`. Superseded native-only attempts
and intermediate candidate receipts have been purged.

Tests cover owned CLOEXEC duplication, literal PATH/empty components, EACCES
precedence, no ENOEXEC fallback, exit/signal identity, cwd identity across rename,
async INT/QUIT ignore preservation, descriptor snapshots before main, retained
null reservations, reversible failed exec, raw child frames and descriptor masks.
Native-launcher publication checks use `TMPDIR=/private/tmp` to avoid the default
`/var` symlink; the publication guard remains unchanged.

Only aarch64 Darwin native behavior has been executed. Local Cargo checks for
both Linux musl targets do not prove linking or execution; Linux and minimum
macOS execution remain open gates.

## Final directory capability boundary

`WorkingDirectory` owns only a descriptor validated with fstat as a directory. `BorrowedDirectory` can be obtained from that owner or by checking a borrowed descriptor. Spawn preparation, native spawn, execution diagnostics, supervised jobs, and source opens accept this distinct capability; SCM transport explicitly consumes it back into OwnedFd and validates it again on receive. Pipe/file runtime rejection and positive/negative downstream type controls pass, including rejection of generic BorrowedFd as cwd and a directory borrow outliving its owner. Native suite is now 13 passing tests plus two independently invoked subprocess probe entrypoints.

## Final private boundary correction

The production `native` module, bounded scalar implementation, CLI implementation, and execution modules are private. The binary calls only `monk_runtime::run_cli()`. A small public `capabilities` facade re-exports safe typed directory/launch/child/endpoint/scalar capabilities for downstream ownership controls; it exposes no raw adoption or signal authority. `adopt_inherited` is crate-private with an explicit exclusive inherited-descriptor ownership precondition, documented at the guardian caller. The package has `publish = false`.

The direct heartbeat regression moved from the integration suite into private native unit tests while retaining a bounded isolated subprocess. The integration manifest regression duplicates through rustix. New compile-fail consumers verify that the native module, raw adoption through the facade, and signal authority through the facade are inaccessible; the positive typed lifecycle control still compiles.

After the privacy move, `cargo fmt --all`, all-target Clippy with `-D warnings`, and full Cargo tests passed against the exact Bash oracle `/nix/store/s0psayl7zvkvwdcqc8fy1sbv8rlf1yq8-bash-5.3p9/bin/bash`: 45 unit tests, four lifecycle integration tests, 12 pure semantic tests, the downstream compile-control harness, and the compile-fail doctest. Two explicit subprocess probe entrypoints and the separately invoked frozen-Haskell semantic replay remain marked ignored by the default suite. The retained performance receipt measures the final Clap binary after these changes.

## User-requested Clap CLI

The private CLI now uses exact `clap = 4.6.7` with only `std` and `derive`, default features disabled. `ParsedCli` owns positional cardinality and OsString extraction. An inserted parser-only end-of-options marker prevents Clap from interpreting any original argument as its own option; the checked `Invocation` conversion accepts only standalone `--describe` or ordered `--abi 2 OPERATION [opaque argv...]`. Conversion to bytes follows OsString parsing, preserving invalid UTF-8. Help/version handling is disabled, parser errors map to the existing status-125 diagnostic, target-description validation still precedes parsing, and operation dispatch/input/error ordering is unchanged.

After this change, formatting, all-target Clippy with warnings denied, and full Cargo tests passed using the exact pinned Bash and Fish. The final 224-case CLI differential, performance and packaging runs identify the Clap-enabled binary separately in their retained receipts.


Final Clap performance refresh: `docs/evidence/rust-runtime-performance-2026-09-23.json` and `performance-report.md` now measure final release SHA `5174a6e287204bf63c71e14b7d6d97ef0017ac467a2e3dfe7cacf3b8529a27a0`. The fresh quiet-window run completed 20 paired samples for each of five workloads with exact byte/status equality. Superseded measurements and intermediate preparations have been purged.
