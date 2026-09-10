# Bash 5.3 / Fish 4.6 execution profile, version 1

`Bash53Signed64Fish46` (`--target-profile bash-5.3-fish-4.6`) is the initial
execution contract. Exact admission is conditional on this contract. The
[roadmap](translator-todo.md) records evidence, including unverified platforms
and remote jobs; a profile declaration is not a runtime version detector.

## Execution environment

The reference is noninteractive Bash 5.3.9, using signed 64-bit two's-complement
arithmetic, and Fish 4.6.0 on 64-bit Linux. Source files are UTF-8 without NUL.
Shell data is NUL-free byte strings; child capture preserves non-UTF-8 bytes and
implements Bash's removal and diagnostic for captured NUL bytes. `LC_ALL=C` and
`LANG=C` apply to both executions. Other locales and Bash profiles are outside
this version. Moving Fish builds provide compatibility evidence only.

The CLI and verification harness explicitly use UTF-8 for Haskell text I/O,
including when their own process locale is C. This keeps source, generated code
and diagnostic encoding independent of the locale used for shell semantics.

Start Bash without profiles, rc files, `BASH_ENV`, imported shell functions, or
environmental `SHELLOPTS`/`BASHOPTS` overrides. Start Fish with `--no-config`.
Both executions have the same cwd, argv, stdin, environment scalars, filesystem
and external command lookup. Bash starts with default noninteractive options:
`errexit`, `pipefail`, `nounset`, `noglob`, POSIX mode, `lastpipe`,
`inherit_errexit`, `extglob`, `nullglob`, `failglob`, `dotglob`, and `nocaseglob`
are off; `sourcepath` is on and IFS is space, tab, newline. The translator models
executed `errexit` and `pipefail` changes and admitted IFS assignments. It rejects
other option changes. It does not change the caller shell to erase callbacks or
to manufacture this initial contract.

Standard descriptors 0, 1 and 2 initially exist with their usual readable stdin
and writable stdout/stderr roles. Admitted redirections track their subsequent
ordering and access. General file opens, extra user descriptors and unowned
descriptor effects reject. External commands have the same executable behavior
and consume ordinary arguments, environment and standard streams. They must not
observe private helper functions, implementation processes, or transport
descriptors. Shell introspection and job-control builtins are excluded.
No relevant signal handler, trap, event callback or asynchronous mutation may
intervene, including through transitive calls. The contract does not promise
equivalence under exhaustion of resources needed by generated helpers.

Requirements enumerate commands, typed Fish/platform capabilities, and native
runtime ABI, profile and operation sets. Generated support uses Fish plus the
Haskell `monk-runtime`; Python is only a development/evidence tool. ABI 1 accepts
fixed operations over NUL-terminated byte frames. Zero frames and one empty
frame are distinct; NUL cannot occur inside a shell value. Child stdout/stderr
and original stdin are separate from framed control input. Captures remove
trailing newlines and Bash-disallowed NUL bytes, with the required diagnostic.

Combined/stdout output resolves `monk-runtime` on PATH, or the `--runtime FILE`
provider, once at each owning entry. It validates ABI/profile/operations before
body effects; unavailable or incompatible support reports `monk.runtime` and
returns 125. Installed providers must remain immutable and compatible during
execution. Managed output captures and validates provider bytes while planning,
then publishes and selects that exact executable at mode 0700. Children and
exported functions retain the selected generation across later publications.

The runtime uses raw bytes, explicit processes and POSIX descriptors. A minimal
C startup constructor records descriptors before GHC can reuse closed streams;
child launch restores signal dispositions and closes private transport
descriptors. The executable ignores ambient GHC RTS options, so `GHCRTS` cannot
add output or alter its configured behavior. Linux anonymous descriptor support
is required and descriptor ownership is checked. Bundling covers the captured
Linux executable, not its dynamic libraries; deployments must supply the ABI
compatible loader/libraries recorded by the verification report. See the
[native runtime specification](native-runtime.md).

## Runtime checks and caller obligations

Standalone guards reject an occupied private namespace and relevant variables
with unrepresented local, universal, path-list or non-environment attributes
before executing source effects. Ordinary exported scalar inputs are admitted.
Unrelated Fish state does not invalidate translation. Reserved target bindings
that would change helper behavior, including `fish_read_limit`, reject during
normalization; capture helpers own their own read limit.

Sourceable output requires the versioned JSON caller contract described in
[migration guidance](../migration-guide.md). Its variable entries declare
access, visible/global scope, and initial export attributes. The runtime checks
presence, scalar cardinality, scope conflicts, universal/path attributes and
export shape; it also checks imported function existence and private namespace
availability. A global unexported output may be absent on entry. Owned writes
create ordinary scalar storage, including for names ending in `PATH`.

The caller promises the same initial Bash option/IFS profile, equivalent imported
functions with only their declared effects, and absence of relevant callbacks.
These behavioral obligations cannot be established by checking names or scalar
shapes. The JSON `ambientEffects: "none"` expresses that promise; unknown effects
do not authorize execution. Sourceable option/IFS mutations, unsupported attributes and argv mutation reject.
Unqualified names must be admitted builtins, definite translated functions, or
declared imports. `command` bypasses function lookup: admitted builtin targets
retain builtin semantics, and external targets record a runtime requirement.
Imported functions are excluded while an uninitialized owned local may carry inherited
environment fallback state, because the import contract does not model that
representation. An admitted `exit` terminates the process; `return` stays inside
its owned function or source boundary. Escaping source-file `break`/`continue`
and loop-depth operands are excluded. Explicit exit/return statuses must be
canonical signed-64-bit decimal literals, definite literal-valued variables, or
`$?`; literal values are reduced modulo 256.

Entrypoint arguments are explicit. Internal literal source occurrences forward
inherited argv when no arguments are supplied, and use their explicit arguments
otherwise. An owned source body preserves incoming/final
status and contains `return`; it does not return from the caller's Fish function.
Declared installed functions recreate and clean their private helpers on later
calls; helper definitions are not left installed after the source entry returns.
Repeated literal dependencies execute each occurrence under one compatible
entry context. Sources inside function bodies or child regions remain excluded.
Source lookup is fixed by discovery cwd/PATH/sourcepath and
immutable input snapshots, independently of publication destinations.

## Observable result and publication

The comparison covers stdout/stderr bytes, status and control flow, argument
boundaries, external filesystem effects, and declared caller-state changes.
Evaluation and child execution ordering are included when observable. The
bounded pipeline envelope excludes unproved writer/SIGPIPE lifetimes; background
jobs, process substitution and traps remain explicit exclusions.

Publication has a separate failure contract: all generation members are staged
and flushed before one atomic entry replacement. Retries reflush an existing
verified generation before replacing the entry. Prior generations are retained,
so a reader pinned to an old loader remains consistent. A post-rename failure
reports the observed entry and possible durability uncertainty. This protocol
does not roll back effects of executing a translated program.
