# Translator architecture

The approved redesign has one authoritative semantic pipeline:

```text
ShellCheck syntax + owned source text
  -> private semantic plan and source occurrence identities
  -> complete materialization with bound runtime references
  -> optimization and admission
  -> structural Fish DSL
  -> rendering and publication
```

The public translation facade now enters the private planned translator. There
is no fallback to the legacy translator after rejection. The superseded
translator, hoisting, renaming, inlining and semantic walkers have been removed;
prototype branches preserve their history.

## Semantic ownership

Normalization owns quotation, field cardinality, evaluation regions, command
identity, finite flow facts, storage decisions, and control/execution boundaries.
It produces a private plan without executable ShellCheck tokens. Literal source
discovery must resume normalization with immutable dependency input because a
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
`translationRuntimeRequirements`. Private record labels prevent record-update
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
Discovery uses declared cwd/PATH/search state, independently of output paths.

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

## Concrete implementation owners

`Language.Bash.Plan.Normalize` owns syntax/context admission and finite flow
joins. `Language.Bash.Plan.Effects` folds that same semantic plan to close child
captures and finite function dependencies; it does not walk ShellCheck again.
The arithmetic source module correlates diagnostic spelling with the owned
operator tree at normalization time.

`Language.Fish.Translator.Plan` materializes the complete core and source
boundary. `Binding` owns scalar presence, logical export attributes and the
inherited exported environment of unset locals. Ordinary assignments,
declarations, arithmetic updates, external dispatch and child snapshots consume
that ownership. These are runtime state values, not analysis receipts.
`Child`, `ArithmeticPlan` and `Pattern` implement bounded operations. All helpers
consume structured operands; source expression strings are never interpreted.

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

The bounded Linux directory envelope limits each UTF-8 path component to 255
bytes and the operand to 4095 bytes. These are lexical admission limits, not
filesystem existence checks. Longer operands reject because Fish can emit its
ENAMETOOLONG diagnostic outside the builtin stderr stream that the parent
operation captures. Control-byte and non-ASCII operands within the envelope
use Bash ANSI-C diagnostic quoting.

The resolved logical directory path must also remain shorter than 4096 bytes.
A pure lexical runtime check enforces that obligation before parent cd, using
the actual PWD and operand; a violation returns/exits with status 125 before
the attempted directory operation. This guard performs no filesystem target
precheck.
