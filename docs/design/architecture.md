# Translator architecture

The approved redesign has one authoritative semantic pipeline:

```text
ShellCheck syntax + owned source text
  -> indexed semantic plan and source occurrence identities
  -> optimized draft structural Fish with bound runtime references
  -> whole-artifact admission
  -> rendering and publication
```

The public translation facade now enters the private planned translator. There
is no fallback to the legacy translator after rejection. The superseded
translator, hoisting, renaming, inlining and semantic walkers have been removed;
prototype branches preserve their history.

## Semantic ownership

Normalization owns quotation, field cardinality, evaluation regions, command
identity, finite flow facts, storage decisions, and control/execution boundaries.
It produces a private plan without executable ShellCheck tokens. Literal or
flow-proved eval text is parsed during translation and re-enters this normalizer;
no generated program invokes Bash eval. Literal source
discovery resumes normalization with immutable dependency input because a
source can change later variable and function resolution. A separate syntax
walker or a spelling-keyed output rewrite cannot establish those effects.

The materializer owns every helper call, temporary, wrapper, redirect and
cleanup operation. Admission applies to the complete materialized artifact.
Changing a plan invalidates admission: there are no detached receipts that
survive arbitrary rewrites, and equal commands do not imply equal occurrences.

Optimization belongs to this materialization stage. Successful constant
arithmetic uses the runtime's signed-64 specification; bounded pure islands
batch before their result is consumed. Writes, lazy branches and errors retain
their original evaluation regions. Helper interning and dead-state removal use
the existing semantic effect fold. There is no rendered-text optimizer or
second semantic analyzer. Embedded child scripts remain structural nodes even
when transported as a quoted byte value, so copied function closures remain
visible to helper/call statistics and recursive inspection.

Use type indices when a consumer enforces grammar, cardinality, control scope
or ownership. Finite sets of definition identities, effect flags and explicit
unknowns bound analysis. Changing environmental facts widen to unknown; there
is no recursive specialization or unbounded symbolic solver.

## Public boundary

`TranslateConfig` explicitly selects policy, execution profile, entry mode and
caller contract. Strict mode permits exact behavior only. Normal mode also
requires exact behavior unless a named approximation is selected. Unsupported
input returns diagnostics without executable output.

`TranslationResult` is opaque, with ordinary inspection functions
`translationScript`, `translationDiagnostics`, and
`translationRuntimeRequirements`, `translationExecutionStrategy`, and
`translationStatistics`. Private record labels prevent record-update
forgery. The same ownership rule applies to the source graph and output bundle. General Fish DSL construction remains available;
a user-built `Script` is not a certified translation.

Diagnostics distinguish parse, translate, source and output phases. Runtime
requirements use command names, typed Fish capabilities, typed platform capabilities,
or native ABI/profile/operation requirements; each capability
needs an actual producer, consumer and test. There is no unused runtime phase.

## Source and caller boundary

Standalone execution and sourceable execution have different contracts. A
versioned [execution profile](execution-profile.md) specifies startup options,
locale, platform, runtime checks and caller obligations. A
sourceable entry requires explicit caller declarations for visible/global
scalar bindings, imported functions and relevant ambient effects. Runtime
checks can verify scalar shape and lookup facts; they cannot prove that an
arbitrary caller function has its promised cross-language behavior.

An owned source-body function isolates `return` while preserving incoming and
final status, explicit arguments, exports and admitted caller-local updates.
Internal Bash source calls forward inherited argv explicitly. Repeated source
occurrences execute repeatedly even when their immutable parse input is reused.
Each occurrence also retains Bash's diagnostic filename spelling independently
of canonical graph identity. Explicit relative and symlink operands stay lexical;
PATH lookup retains the selected entry spelling. Runtime statement and arithmetic
locations use that origin, while snapshots, cycle checks and function identities
continue to use canonical parser positions.
Discovery uses declared cwd/PATH/search state, independently of output paths.

## Native and supervised execution

Direct printf/echo output has one shared framed native writer boundary to
preserve initially closed stdout, errno diagnostics and actual SIGPIPE. Scalar
operands and control remain native Fish; one helper-local result slot owns
writer status. Silent scalar/control bodies can still require no primitive helper.
The originally planned helper-free greeting conflicts with these demonstrated
I/O semantics; exactness takes precedence without narrowing the input contract.

The canonical standalone entry is `monk-runtime --abi 2 launch FILE [ARGS...]`.
It records descriptor presence before Fish startup and restores closed streams
around the compiled body. This entry is distinct from semantic supervision:
DirectExecution still has native Fish control and no session owner. Private
launch markers preserve the original file identity and do not enter user
external environments. Sourceable entry retains its explicit caller contract.

The same semantic plan selects `DirectExecution` or `SupervisedExecution` from
its materialized runtime requirements. Direct output uses native Fish for
proved scalar commands and control flow, with bounded byte/integer primitives
where needed. Supervised output gives the native owner responsibility for
background jobs, wait/status, streaming process endpoints, native read and
ordered user descriptor scopes. This is one Fish code generator with explicit
runtime operations, not a second Bash interpreter.

`Session` materializes framed requests and private replies. The runtime owns
virtual user descriptors, launches executable or already generated Fish child
regions, and observes process termination. The Rust runtime prepares owned launch
actions before creating a child; ordinary foreground launches use POSIX spawn,
while ignored INT/QUIT dispositions require a bounded native fork/exec path.
That child performs only audited async-signal-safe libc calls over precomputed
pointers, without allocation, unwinding or general destructors. Separate
control transport carries scalars, dense array vectors, argv and definition
closures without reusing script stdin. Ordinary child script/state transport
uses an owned private capsule with independent file offsets. Process
substitution instead uses real streaming pipes and a checked pipe-descriptor
pathname capability; it never buffers producer data into a temporary file.

Dense indexed arrays have typed assignment, append, index-zero/scalar update,
read/count and quoted-splice operations. Normalization keeps array identity
separate from known length. A runtime-sized dense read result retains unknown
length through append and index-zero writes; it cannot prove arbitrary indexed
writes safe. Child snapshots frame presence, count and every element separately.

`Traps` materializes deferred EXIT/ERR bodies from compile-time parsed literal
handlers. Registration does not freeze scalar values. ERR effects invalidate
subsequent flow facts, and handler execution preserves the owning status and
redirection region. Unsupported asynchronous handlers and unproved callback
control transfers remain outside admission. See the [semantic audit](translator-audit.md)
for the exact frontend restrictions.

ABI 2 separates the `bash53-i64` shell profile from the native target and typed
capabilities. The declared native targets are x86_64 Linux, aarch64 Linux and
Apple Silicon Darwin. Native image selection checks the actual target;
publication does not make an executable portable across operating systems.
Local Darwin tests and unexecuted Linux matrix jobs remain distinct evidence.

## Structural Fish and publication

`Language.Fish.DSL.Internal` is the canonical recursive output representation;
public smart constructors and inspection views preserve structural invariants.
The private renderer consumes that same representation. Compatibility names
under `Language.Fish.AST.*` do not form a second live IR.

Output planning is separate from filesystem effects. Managed bundles pin child
and runtime references to one immutable generation. The publisher stages and
flushes on the destination filesystem, serializes competing writers and
atomically replaces one small entry loader. Prior generations remain. Recovery
inspects the entry after an ambiguous post-rename failure. This protects bundle
publication and reader consistency, not the effects of executing a script.

`Monk.Output.Runtime` captures an opaque validated native image before output
planning. The captured bytes, role, path and mode participate in SHA-256
generation identity. The publisher writes native members at 0700, validates
the staged executable, flushes it, and only then replaces the entry. Reuse
checks bytes and mode as well as the manifest. Installed and bundled runtime
references are resolved before admission; changing a provider binding requires
fresh materialization and admission. Previously installed functions capture
their resolved absolute runtime path and retain the earlier generation.

`TranslationStatistics` reports structural helper definitions, static helper references
and runtime calls, and rendered Fish bytes, including embedded child bodies.
Result, source graph and generated-file accessors expose these values. They
describe emitted code; measured process launches are independent evidence.

The current implementation and evidence status is tracked in
[the roadmap](translator-todo.md); the architecture is not a release-completion
claim.

## Package dependencies and host ownership

One Cabal package contains the private `monk-foundation`, `monk-host`,
`monk-compiler`, `monk-publication` and `monk-test-support` libraries. Foundation
contains no language-specific imports. The compiler consumes immutable text and
source documents; it cannot execute providers or discover files. Public `monk`
coordinates discovery, materialization, provider capture, and output planning.
Publication consumes validated bytes and filesystem identities without importing
the compiler. Shared tooling and Bakeoff use the same host process runner and
fixture metadata readers as production tests.

`Monk.Host.Process` acquires a process group before starting a waiter and brackets
stdin, stdout, stderr, cancellation, and descendant cleanup together. Observations
retain raw bytes; decoding is an explicit operation for text consumers. A threaded
RTS uses an asynchronous process waiter. A nonthreaded RTS polls process status so
blocked waits cannot prevent stream workers or deadline handling from running.
The chosen dependency exception and public API changes are described in the
[typed architecture migration guide](typed-architecture-migration.md).

## Concrete implementation owners

`Language.Bash.Plan.Normalize` owns syntax/context admission and finite flow
joins. Its `Context`, `Flow`, `Words`, `Commands`, `Control` and `Sources`
modules separate immutable inputs, finite facts and source suspension from
syntax orchestration. Scope-indexed statements retain loop, return and argv
witnesses; body kinds distinguish functions, sources, children and callbacks.
Entry bodies additionally retain the exact compilation owner and entry contract.
Dense-array updates prove and apply their shape transition in the current flow
world after normalizing the operand.

`Language.Bash.Plan.Effects` folds that same semantic plan to close child
captures and finite function dependencies; it does not walk ShellCheck again.
The arithmetic source module correlates diagnostic spelling with the owned
operator tree at normalization time.

`Language.Fish.Translator.Plan` is the existential compilation facade.
`Monk.Compiler.Artifact` owns complete materialization and its private draft
constructor, so a caller cannot pair unrelated Fish or requirements with a
normalized-plan token. `Materialize`, `Words`, `Invocation` and `Context` own the
statement walker, ordered operands, invocation and scope state respectively.
`Region` and opaque `Emission` keep operand definitions attached to their
consumer. `Binding` owns scalar presence, logical export attributes and the
inherited exported environment of unset locals. Ordinary assignments,
declarations, arithmetic updates, external dispatch and child snapshots consume
that ownership. These are runtime state values, not analysis receipts.
`Child`, `Session`, `Traps`, `ArithmeticPlan` and `Pattern` implement bounded
operations. Opcode-indexed session and primitive requests retain mandatory
operands and structural child bodies. All helpers consume structured operands;
source expression strings are never interpreted. A deterministic registry
interns equal helper definitions and rejects conflicting definitions of one name.

`compileSourceBundle` collects source-wrapper definitions into immutable member
scripts before admission. Each source occurrence still invokes its owned body
at the original execution point. The generation entry loads definitions from
its own directory, and `compileBundleLoader` constructs the entire single
source command that preserves the incoming status, scope and explicit argv.
`Monk.Output` selects content-derived generation paths and publication bytes;
it cannot rewrite admitted commands or reconstruct occurrences by equality.

## Stable directory state

`Language.Bash.Plan.Directory` owns proved directory operations, separate
cwd/PWD/OLDPWD/stack access requirements, and success/failure directory facts.
The normalizer admits ordinary literal or constant-proved paths; interior
`name/..` cancellation is rejected because Fish can erase a missing name that
Bash must resolve. Short-circuit and conditional edges select the appropriate
facts. An unguarded `cd` joins success and failure; it never proves success by
checking the translation host's filesystem. Source requests carry an execution
cwd override only where that control edge proves it. A directory-changing loop
must retain an invariant absolute cwd for relative source discovery.

`Language.Fish.Translator.Directory` changes the parent process with Fish's
actual builtin `cd`. A structural `PipeErrorTo` continuation renders `2>|`:
source metadata and the actual builtin diagnostic reach the bounded native
converter, and the materializer immediately captures both pipeline statuses.
The converter does not replace the attempted operation with a filesystem
precheck. Native `getcwd` implements physical `pwd`; Bash stack display and
failed push/pop behavior are owned explicitly. `Binding` remains the sole owner
of OLDPWD's logical export state, including Bash's standalone startup
export-marked but unset OLDPWD and an explicit later `unset`.

Sourceable version 2 contracts grant directory permissions separately. The
caller supplies ordinary global directory state and guarantees empty CDPATH,
a valid exported scalar PWD, and stable logical cwd ancestry across imported
functions and external effects. These behavioral obligations are conditional
assumptions, not claims that a runtime check can prevent ancestor renames.
Owned children inherit cwd and serialize their own stack through the existing
snapshot boundary; child changes do not alter the parent's directory state.

The lexical directory envelope limits each UTF-8 path component to 255
bytes and the operand to 4095 bytes. These are lexical admission limits, not
filesystem existence checks. Longer operands reject because Fish can emit its
ENAMETOOLONG diagnostic outside the builtin stderr stream that the parent
operation captures. Control-byte and non-ASCII operands within the envelope
use Bash ANSI-C diagnostic quoting.

The resolved logical directory path must also fit the runtime target: at most
1023 bytes on Darwin and 4095 bytes on Linux.
A pure lexical runtime check enforces that obligation before parent cd, using
the actual PWD and operand; a violation returns/exits with status 125 before
the attempted directory operation. This guard performs no filesystem target
precheck.

## Rust runtime ownership

Cargo is the sole producer of `monk-runtime`. The compiler and publication
remain Haskell. The private `monk-foundation` library owns the integer
specification, SHA-256 interface and generated ABI metadata; `monk-host` owns
provider capture and validation.
`protocol/abi2.tsv` generates committed Haskell and Rust tables; the drift check
runs before builds. It preserves ABI 2 spellings and capability order.

The runtime uses edition 2024, resolver 3 and nightly 2026-09-23 through the same
`rust-toolchain.toml` in Cargo, devenv and CI. `rustix` owns descriptor, byte-I/O,
filesystem, socket and polling boundaries; `nix` provides suitable process and
signal operations. Remaining native calls use maintained `libc` bindings in
`runtime/src/native*`. There are no project C sources.

Source descriptors, OS descriptor owners, borrowed descriptors, prepared
launches, running/completed children and endpoint leases are distinct types.
Private pattern types bound descriptor numbers, stream masks and shift counts;
checked scalar conversions are documented and tested under Miri. Consuming
launch/completion/transfer methods prohibit reuse. One owner reaps and caches
jobs; a completed background job remains explicitly waitable, and dropping
an asynchronous job does not kill it. Guardian leases own cross-process cleanup.

Request decoding validates the complete borrowed frame before execution.
Session transport and initial capsule takeover multiplex incomplete peers and
partially written replies without changing serialized semantic read, wait or
foreground execution. A guardian owns one session root; job workspaces retain
private liveness leases across generated children and are reclaimed after their
last owner exits. Bootstrap validates all inherited descriptor roles before
adopting or relocating any descriptor. Capture checks cancellation between
buffered chunks as well as during blocking reads.

Pre-main constructors retain original stream presence and ignored INT/QUIT.
Rust's reserved standard descriptors stay valid internally, while child execution
restores actual source-stream absence. Failed exec attempts restore descriptor
reservations and signal state. `-Zon-broken-pipe=inherit` plus explicit operation
policy preserves actual SIGPIPE termination. Owner handlers only notify; a
bounded wake thread signals the owner thread so a signal just before a blocking
syscall cannot strand it. That thread never launches, reaps or owns descriptors.
All process-global state changes and lifecycle decisions remain with the owner.
