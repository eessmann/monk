# Translator roadmap and acceptance

Last refreshed: 2026-09-23. Monk keeps semantic admission, the structural Fish
DSL and publication tooling in Haskell, with a Rust execution runtime. This
roadmap separates implemented behavior from verified acceptance. The
[semantic audit](translator-audit.md) defines the supported envelope; the
[Rust verification report](rust-runtime-verification.md) records executable
identities, commands and evidence limitations.

## Implemented architecture

| Area | Behavior and boundary |
| --- | --- |
| Build | Cargo builds the Rust 2024 runtime on pinned nightly 2026-09-23. Devenv orchestrates Cargo and the Simple Cabal build explicitly. GHC 9.14.1 is the default compiler; GHC 9.12.2 remains a declared compatibility lane. |
| Semantic admission | One private semantic plan classifies context-dependent exact behavior, named opt-in approximations and explicit rejections. No general Bash interpreter or legacy translation fallback is available. |
| Runtime ABI 2 | `bash53-i64`, shared generated ABI metadata, raw byte framing, original-stream capture, typed descriptor ownership, prepared process launch and immutable provider capture. Haskell retains the pure integer specification for folding and differential tests. |
| Selective supervision | Proven scalar/control/function programs use native Fish directly. A session owner handles user descriptors, real child identities, pipelines, background lifetime and waiting when required; generated Fish retains program control flow and variables. |
| Words and arithmetic | Quote-aware field cardinality, IFS/pathname expansion, lazy effects and pattern regions; signed-64 operator trees, wrapping, ordered updates and context-specific errors. Optimizations preserve failure and effect boundaries. |
| Functions and sources | Definite finite dispatch, compatible dynamic local storage, invocation-time redirects, immutable acyclic sources and finite compile-time-parsed eval. Unknown dispatch, recursion, computed mutable sources and arbitrary runtime program text reject. |
| Input and arrays | Ordered opens, duplication/closure, scoped descriptors, here input, admitted byte-read flags, dense indexed arrays and child snapshots. Unknown inherited descriptors, sparse arrays and unsupported sourceable effects reject. |
| Jobs and process substitution | Owned background PIDs and cached wait status; concurrent pipe endpoints with explicit leases. Endpoint paths cannot escape into arbitrary stored values or unproved consumers. |
| Callbacks | Compiled standalone EXIT/ERR handlers with live scalar reads and status/suppression rules. Arbitrary signal handlers and directory operations combined with EXIT/ERR traps remain excluded. |
| Caller contract | Sourceable output owns return/status/argv boundaries and declares scalar, function and directory permissions. Ambient behavior that cannot be checked remains an explicit caller obligation. |
| Publication | Captured provider images, verified immutable generations, one atomic entry replacement, retained generations and observed-entry recovery. Planning remains separate from filesystem writes. |

The stable-directory contract, constructor-specific restrictions and process
inspection exclusions are detailed in the [execution profile](execution-profile.md)
and [constructor policy](shellcheck-syntax-inventory.md). An implemented syntax
form is not an unconditional promise about every operand or context.

## Current verification

The retained Rust records identify their measured binaries and source snapshots;
this table summarizes them without treating every check as a run of the same
artifact.

| Check | Retained evidence |
| --- | --- |
| Local Haskell integration | GHC 9.14.1: 980 main tests, 38 publication tests and 63 tooling tests passed, with compiler-support, runtime, digest, transport and boundary checks. |
| Final Cargo runtime | All 15 ABI suites passed. The record also contains Cargo tests, formatting/Clippy, 1,262 frozen-Haskell semantic comparisons, 224 CLI comparisons and 11 pure/type-boundary Miri checks. |
| Ownership and scalar boundaries | Fourteen downstream negative type cases and a successful positive control exercise borrowing, consuming transitions and private bounded construction. These checks supplement semantic comparisons. |
| Darwin distribution | The packaged arm64 runtime passed 15 ABI suites plus child transport, signature and permitted-library inspection. Unpacked source built and installed Cargo/Cabal products and passed combined/managed Unicode smoke checks. |
| Coverage preservation | A fresh comparison binds the final Rust runtime to a rebuilt current compiler: 74 default and 77 stable matches out of 95, with zero admitted mismatches. The retained migration receipt separately identifies its earlier snapshot. See the [Babelfish comparison](../babelfish-comparison.md) for scope and provider identities. |
| Runtime performance | Final Rust and retained Haskell runtime timings use the same frozen compiler and small representative workloads, with streams/status checked for every sample. These observations do not establish the historical aggregate performance gates or current performance against Babelfish. |

See the [build record](../evidence/rust-runtime-build-2026-09-23.json),
[runtime record](../evidence/rust-runtime-verification-2026-09-23.json),
[coverage record](../evidence/rust-runtime-coverage-2026-09-23.json) and
[performance record](../evidence/rust-runtime-performance-2026-09-23.json).
The current Rust records do not renew earlier whole-suite claims for GHC 9.12.2
or moving Fish. Configured compatibility lanes need their own execution evidence.

## Open acceptance gates

- Native execution of the x86_64 and aarch64 Linux musl release binaries.
  Local Cargo checks and Nix evaluation do not establish successful linking,
  static release inspection or process behavior on those hosts.
- Execution at the declared minimum operating-system versions. Inspecting a
  Mach-O deployment target does not prove execution on that macOS version.
- Execution of the configured remote CI matrix and renewed compatibility
  evidence for compiler and Fish variants outside the retained local run.
- Complete aggregate performance and measured process-launch evidence. Missing
  historical inputs and unavailable baseline targets are incomplete gates,
  not successful or failed observations of the current runtime.
- The extra process-effect comparison remains incomplete. Earlier fixtures used
  incompatible helpers; fresh fixtures use command heads outside proved dispatch,
  and their Bash handshake worker fails. Dedicated native process-substitution
  tests provide separate evidence, not replacement results for these probes.
- Invalid-byte pathname cases rejected by the local Darwin filesystem remain
  platform skips; invalid-byte protocol tests are separate executed checks.

The helper-free greeting criterion is not achieved. With initially closed
stdout, Bash `printf x` diagnoses a write failure while Fish can return success
silently; a readerless pipe also requires actual SIGPIPE termination. Exact
output therefore uses a shared native writer. The native standalone launcher
captures descriptor presence before Fish opens missing streams as `/dev/null`.
These states remain in the contract. Silent scalar/control bodies can remain
free of primitive helpers while using the same native entry.

## Evidence policy

Close a gate only with reproducible evidence for the relevant source, binary,
platform and contract. Report translation rejections, admitted mismatches,
missing prerequisites and unavailable executions separately. A rejection test
proves classification, not implemented functionality. Keep independent Bash
comparisons alongside specification replay and type tests.

Counterexample fixtures remain in the test suite when historical reports are
retired. The [SHLVL inspection example](execution-profile.md#shell-identity-inspection)
preserves a deliberate exclusion; the closed-stream examples above preserve
supported behavior that requires native execution.

Keep the semantic audit, syntax policy, [architecture](architecture.md) and
[migration guide](../migration-guide.md) aligned with the admitted surface.
Approved plans remain records of design intent, not verification results.
