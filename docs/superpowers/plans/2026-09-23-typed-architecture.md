# Monk pervasive typed architecture implementation

Authority: the author's approved plan in the current task (2026-09-23).

## Binding constraints

Preserve the admitted Bash envelope, CLI behavior, ABI-2 bytes, the private renderer, one semantic pipeline, complete-artifact admission, and both GHC 9.12.2/9.14.1 lanes. Haskell public API breaks are authorized. Prefer readable native Fish; extra machinery requires exact semantics or measured benefit. Preserve the original checkout's devenv.lock change. Do not use ccs-ci. Keep unexecuted platform gates explicit.

## Workstreams

1. Freeze baseline identities, tests, generated output, and performance observations. Resolve and compile singletons, typed-process, cryptohash-sha256, and base64-bytestring on both GHC lanes before consumer migration.
2. Foundations/package boundaries: foundation-src, host-src, compiler-src, publication-src, public src, test-support, tooling/src; move locations below language facades; component-owned dependencies.
3. Compiler: phase/owner/target/entry/provider/control/region/world/domain/cardinality/effects/capability indices; nominal roles; finite dynamic facts; owned transitions; indexed semantic categories and control witnesses; canonical grammar-safe DSL; region builder; native specialization; deterministic helpers; whole-artifact admission.
4. Runtime: typed request decode before effects, centralized ABI-preserving codecs, native/session module splits, nonblocking multiplexed control receive/reply, job/guardian workspace ownership, borrowed frames, shared capture and matcher buffers, cached job completion, trybuild.
5. Host/publication/tooling: typed-process runner and cancellation, standard digest/Base64, validated publication layouts/stages and module splits, direct library tests, versioned strict fixtures and exact argv, linear source discovery, typed test suite inventory.
6. Verification/documentation: compile-fail and positive controls, parity and Fish syntax/goldens, lifecycle/fault tests, paired performance with existing 10% ceiling, quality, both GHCs, Cargo/Miri, Haddock, sdist, honest platform evidence, architecture/audit/roadmap/migration docs.

## Dependency graph

foundation -> host; foundation -> compiler; foundation+host -> publication; compiler+host+publication -> public monk; foundation+host -> test-support; foundation+host+test-support+monk -> tooling. No production import from tooling. Shared effect algebra is authoritative; dynamic environments never become type-level maps. No unsafeCoerce proof construction. Post-admission rewrites must return to draft.

## Migration decisions

Introduce validated leaves and executable proof slice first, migrate subsystem owners incrementally, remove transitional implementations rather than leave parallel pipelines. Singletons core only, explicit bounded closed families. typed-process retains explicit process-group termination. SHA-256 retains lowercase identity bytes; Base64 accepts canonical padded encoding. trybuild is test-only. Existing rustix/nix/tempfile support transport; retain cancellation heartbeat. memchr/general async runtime/full event-driven jobs are deferred.

## Acceptance

All baseline accepted/rejected classifications preserved except independently demonstrated correctness fixes; zero admitted output/status/effect mismatches. Type failures must fail for intended reasons with passing controls. Track compilation cost and memory as well as runtime and output statistics. No release/platform claim without execution evidence. No arbitrary code-size/type-parameter target.

## Feasibility ruling during implementation

Both GHC dependency lanes resolved and compiled the proposed libraries. The
`typed-process` adoption failed the process ownership requirement: its
`startProcess` starts an asynchronous `waitForProcess` before returning, while
`getPid` reads the mutable process handle. An immediately exiting leader can
therefore be reaped before the host captures the group identifier; surviving
pipe-holding descendants would escape cancellation. The public API has no
pre-wait identity hook or adoption of a precreated process. See
[upstream implementation](https://github.com/fpco/typed-process/blob/master/src/System/Process/Typed.hs).

The shared host runner instead retains a narrow `createProcess` acquisition
boundary and captures its group before any waiter starts, with bracketed
`async` stream ownership. No shell handshake or wrapper process is introduced.
This is an evidence-based dependency exception to preserve the plan's mandatory
cancellation guarantee; `typed-process` is not retained as an unused dependency.
