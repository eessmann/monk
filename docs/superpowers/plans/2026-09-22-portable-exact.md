# Expand Monk's exact Bash coverage and produce portable, readable Fish

Approved by the author on 2026-09-22. This document records the implementation contract from the task conversation.

## Goal and invariants

Extend the existing exact semantic pipeline, structural Fish DSL and private renderer. Simple programs lower aggressively to native Fish where equivalence is proved; complex standalone programs use a native session owner with generated Fish control flow. No runtime Bash parser/interpreter or implicit Bash fallback. Preserve existing sourceable semantics, immutable managed generations and every exact positive.

Required targets: x86_64-linux, aarch64-linux and aarch64-darwin. Linux runtime binaries are fully static musl; Darwin statically links Haskell/third-party libraries with only Apple system dependencies. Fish and explicit source commands remain deployment dependencies.

## Tasks

1. Reproducible devenv/haskell.nix dependencies: pinned IOHK haskell.nix, shared Cabal project, GHC 9.14.1 default and 9.12.2 compatibility, Bash 5.3.9/Fish 4.6.0 reference plus locked moving Fish; build/test/quality/benchmark/package tasks; correct devenv MCP configuration.
2. Portable runtime ABI 2: explicit descriptor/process ownership, offsets, inheritance and signals; no /proc dependency or Linux /dev/fd reopen assumption; target metadata; static release derivations and artifact verification.
3. Native lowering/readability: pure scalar/argv expressions, ordinary scalar storage, admitted builtins, commands, Boolean control, simple owned functions and external-only pipelines. Explicit status ownership; retain effectful snapshots. Single-operation integer batching; shared echo fast path under redirects; concise safe literals and deterministic names; flat standalone guards and demand-driven support.
4. Selective supervisor: standalone launcher execs native session owner; private generated Fish evaluator retains control/bindings. Private versioned duplex protocol selects compiled operations/body IDs. Owner controls real PIDs, wait statuses, descriptor tables, pipelines and closure; user streams carry no protocol. Background jobs survive normal parent completion. No implementation-process introspection.
5. Pipelines/jobs: general foreground pipelines, builtin writers, noninteractive &, $!, wait with zero/multiple PID operands, pipefail/errexit, signals and completion-before-wait.
6. Input/descriptors: ordered opens/append/dup/close, scoped compound redirects, here-documents/here-strings, prefix assignments, byte-correct read -r/-d/-n/-u/-a.
7. Arrays/words: dense indexed arrays, access/append/quoted expansion/snapshots, composed splitting/pathname expansion, parameter patterns preserving quote activity.
8. Process substitution: concurrent <(...) and >(...), multiple owned endpoints, streaming/early closure, independent status/lifetime.
9. Traps: compiled standalone EXIT/ERR handlers, replacement/reset, live reads, status and suppression/inheritance rules.
10. Finite dynamic execution: literal/finitely proved eval, immutable literal sources inside owned functions/children, compatible finite source contexts.
11. Comparative evidence: frozen95 unchanged, original 2bc0e72bcfaa6d90615946667a573093aed2262e/current/candidate/pinned Babelfish independently compared to Bash; separate filesystem/caller/process cohorts and strengthened weak fixtures. Package reproducible inputs.
12. Final validation/documentation: complete gates below and align roadmap/audit/architecture/migration/comparison with final-tree evidence and explicit gaps.

## Interfaces and compatibility

Runtime ABI 2 separates semantic profile from native OS/architecture metadata; existing CLI semantic-profile spelling remains. Extend typed plans/capabilities for jobs, descriptors, arrays, callbacks and compiled dynamic bodies; expose execution strategy and structural statistics. Old managed generations retain captured providers; incompatible installed support fails before source effects. Session-only features initially require standalone mode.

## Acceptance

Zero unexplained admitted mismatches; retain every exact positive and original counterexample. Positive coverage for existing background, pipeline, process-substitution, EXIT, literal-eval and applicable read/array fixtures. Compare raw streams/status, filesystem bytes/modes, caller state and process events. Handshakes, not sleeps, establish async ordering. Include large streams, early consumers, SIGPIPE, descriptor leaks/reuse, interrupted wait, failed opens, EOF, NUL and invalid UTF-8, callback/job interactions. Extend generated nested/shrinking compositions and complete-file goldens. Simple scalar greeting/conditional: no native calls/helpers/redundant operand or status temporaries.

Freeze runtime cohorts: three warmups, twenty alternating samples; fewer launches and lower time on targeted native paths, <=10% median regression on existing common/arithmetic cohorts. Run integrations, native protocol/transport, publication, API boundaries, HLint, Ormolu, Haddock and source distribution through devenv. Execute packaged artifacts on all three native targets outside build environment; inspect Linux ELF and Darwin dylib dependencies. Preserve local/remote/skipped evidence distinctions.

## Exclusions

Arbitrary runtime-generated Bash, mutable computed sources, interactive jobs, DEBUG/RETURN traps, arbitrary signal handlers, sparse/associative arrays and implementation-process introspection remain rejected. Preserve unrelated changes. No publishing/tagging/remote Git changes are authorized by implementation.
