# Typed architecture migration

Monk retains its command-line behavior and ABI-2 wire spelling. The Haskell DSL
now rejects combinations that Fish cannot execute as written. Public translation
and publication results remain opaque: their accessors inspect the admitted
artifact and cannot replace its provider, script or requirements.

## Public DSL changes

`Command` has a grammar index in addition to its result role. Ordinary invocations
have type `Command Atomic ReturnsStatus`; blocks have
`Command BlockGrammar ReturnsStatus`. Pipelines and asynchronous commands have
their own grammar categories. The `stage` builder accepts atomic commands and
blocks, so a background command or compound job cannot accidentally become a
pipeline stage. Put a compound job inside an explicit block when that scope is
intended.

Control commands (`return`, `break`, `continue` and `exec`) use `ControlGrammar`
and cannot form a pipeline stage. `exec` has its own constructor; the former
`DecExec` decoration is removed. Executable names use checked `CommandName`
values, excluding empty names, NUL and Fish's reserved/control words. Quoting
those words does not prevent Fish from interpreting their grammar role.
Dynamic executable positions use an `Executable` containing a literal command
name or a quoted variable identifier. A command substitution must first be
captured in a variable before that variable is used as the executable.

`redirect` returns `Redirect`. Use `redirectArg` when adding it to a simple
command's argument list. Block suffix builders accept `[Redirect]` directly:

```haskell
redirectedBlock :: Command BlockGrammar ReturnsStatus
redirectedBlock =
  beginWithRedirects
    (stmt (command "true" []) :| [])
    [redirect stdout overwrite (fileTarget (str "output"))]
```

Stream, mode and target witnesses constrain legal redirects. Append/clobber
require file targets; descriptor duplication and closure use input/overwrite.
Combined stdout/stderr redirection only supports output files. Descriptor
numbers are `Natural`. Fish has no Bash-style `<>` redirect, so the old
`readWrite` DSL constructor has been removed.

Shared source positions now live in `Monk.Source.Location` within the foundation
component. The public DSL continues to re-export them. Filesystem parsing lives
in the public orchestration layer; the compiler parser consumes immutable text.

`var` and single-element `varIndex` are quoted scalar fields. `vars` and range
indexing expose list expansion. Internally, the closed `UnquotedResult` family
makes an unquoted scalar variable a field sequence, so it cannot occupy a scalar
executable position. Scalar command substitutions retain quoting as well.

Variable references, assignments, loop binders and function parameters now use
opaque `Identifier` values. Static names remain convenient with
`OverloadedStrings`; dynamic names use `identifier :: Text -> Either Text Identifier`.
`identifierText` inspects a validated name. Invalid literals fail at construction;
applications handling input should use the checked function and report its error.

## Compiler ownership

Normalization starts with a generative compilation context. Parsed input,
normalized plans and materialized artifacts retain its owner, target, entry and
provider indices. Provider rebinding creates a new owner and requires fresh
materialization and admission. Callers receive existential wrappers rather than
program-sized type signatures.

Finite binding environments remain maps. Dense-array proofs carry the current
flow world; an update is proved and applied within one transition after operand
normalization. A proof or pending update from an earlier world cannot be reused
after mutation or a join. Definition, source and occurrence identities are
explicit values rather than textual or integer sentinels.

Statements retain their control scope. Loop, return and argv operations require
witnesses from that scope. Nested loop, function, source, child and handler roots
establish fresh scopes; existential bodies retain their root kind. The entry body
also retains its compilation owner and entry contract. A materializer loop frame
owns status storage, continue actions and descriptor restoration together.

Nested bodies retain their scope and root kind, rather than repeating the
compilation-owner index on every syntax node. Effect analysis uses finite sets
and the authoritative semantic fold. Native specialization consumes an indexed
proof tied to the exact statement. Platform, Fish and runtime requirements remain
finite values checked during whole-artifact admission; statements do not carry a
universal type-level effect row.

`Region` owns scalar and field-sequence values during lowering. Closing a region
produces an opaque `Emission`, which preserves the defining statements until a
complete consumer is rendered. There is no tuple projection that can discard a
prelude. Prefix assignments install each value before evaluating the next
assignment, including command substitutions.

Session requests and bounded primitives use opcode-indexed constructors with
mandatory fields. `ReplyShape` determines scalar versus field-sequence replies.
Pipeline preparation captures each stage's fields after its own operand effects
and before evaluating the next stage. Embedded child bodies remain structural.

Artifact construction owns materialization of the entire normalized plan. Its
constructor accepts no caller-supplied Fish, helper map or requirement list.
Admission covers the entry and publication members together; only admitted
artifacts provide executable inspection views. A publication loader is a fixed
operation producing a new draft, which must pass admission again.

Bash functions whose names have a Fish control-grammar role now produce a
located `monk.semantic.function-name` diagnostic before materialization. This
also applies to invoked imported targets. Such definitions previously could
produce invalid Fish; the stricter representation must not turn them into an
uncaught construction exception.

Publication plans expose checked construction and inspection, while raw layouts
remain private. Lock and staging callbacks use revocable synchronized leases:
an escaped deferred action cannot publish after its callback closes, and cleanup
waits for already claimed operations before releasing the lock or directory.

These are internal compiler interfaces. Ordinary public callers still use
`translate` and the opaque translation/source/output results. A general DSL
`Script` never confers translation certification.

## Package layout

All components remain in one Cabal package. Private libraries establish the
production dependency graph:

| Directory | Component responsibility |
| --- | --- |
| `foundation-src/` | Source locations, contracts, diagnostics, integer specification, digest and generated ABI inventory |
| `host-src/` | Byte-preserving subprocess ownership, snapshots and runtime capture/validation |
| `compiler-src/` | Normalization, semantic plan, materialization, admission, structural Fish and private renderer |
| `publication-src/` | Validated layouts, locking, staged durability, publication and recovery |
| `src/` | Public facades and source/output orchestration |
| `test-support/` | Shared fixture schema and shell observations |
| `tooling/src/` | Verification, evidence, packaging and Bakeoff domains |

Register new modules in `monk.cabal`. Common warning settings are independent
of component dependencies. Tests link the compiled publication library.

## Fixture metadata

New fixtures can use `<stem>.fixture.json` with `version: 1`. For example:

```json
{"version":1,"args":["one argument","","three"],"mode":"exec","stdin":"input\n","recursive":false}
```

Argument boundaries, including empty arguments, are exact. Supported keys are
`version`, `args`, `mode`, `platforms`, `stdin`, `prerequisites` and `recursive`.
Unknown versions/keys/modes and NUL arguments fail. A JSON sidecar cannot coexist
with legacy sidecars. Existing sidecars remain readable, with strict `exec` or
`source` modes. Missing required execution prerequisites fail; unsupported
platforms remain explicit structured skips. Cabal source archives include these
JSON sidecars; `procsub-output.fixture.json` preserves the existing fixture's
Linux/Darwin platform restriction.

`monk-tool runtime check --list` lists native-runtime suites from the shared typed
inventory. `--all --runtime FILE --monk FILE` runs those suites. `--suite` retains
existing names, including `digest`, which uses the separate compiler-support
checker and is excluded from `--all`.

## Dependencies and identity

`singletons` core supplies bounded witnesses; project-specific families remain
explicit. `cryptohash-sha256` owns hashing, with the existing lowercase 64-byte
hexadecimal interface. `base64-bytestring` replaces the codec; canonical padded
spelling is enforced by re-encoding. `trybuild` owns Rust downstream compilation
checks and remains a development dependency.

`typed-process` resolved on both supported GHCs but failed the group's acquisition
requirement: its waiter can reap a fast leader before its public `getPid` call.
The shared runner therefore uses a small `createProcess` boundary and scoped
`async` tasks, capturing the group identity before any waiter starts. This
preserves cancellation without a wrapper process or forked dependency. See the
[implementation decision](../superpowers/plans/2026-09-23-typed-architecture.md#feasibility-ruling-during-implementation).
