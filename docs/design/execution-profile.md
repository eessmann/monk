# Bash 5.3 / Fish 4.6 execution profile, version 1

`Bash53Signed64Fish46` (`--target-profile bash-5.3-fish-4.6`) is the initial
execution contract. Exact admission is conditional on this contract. The
[roadmap](translator-todo.md) records evidence, including unverified platforms
and remote jobs; a profile declaration is not a runtime version detector.

## Execution environment

The reference is the pinned Nixpkgs Bash 5.3p9 package (reporting Bash 5.3.9),
using noninteractive execution and signed 64-bit two's-complement
arithmetic, and Fish 4.6.0. ABI 2 declares native targets x86_64-linux,
aarch64-linux and aarch64-darwin. Current local runtime evidence is Apple
Silicon Darwin; Linux execution and moving-Fish compatibility are separate
verification gates. Source files are UTF-8 without NUL.
Shell data is NUL-free byte strings; child capture preserves non-UTF-8 bytes and
implements Bash's removal and diagnostic for captured NUL bytes. `LC_ALL=C` and
`LANG=C` apply to both executions. Other locales and Bash profiles are outside
this version. Moving Fish builds provide compatibility evidence only.
For ANSI-C quoted Unicode escapes above the signed range, the pinned Darwin
and Linux Bash builds differ because of their conversion support. Translation
retains both byte results and the native runtime selects its target's result;
this choice is not folded using the translator host. Ordinary equal byte
literals remain compile-time constants.

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

Reference invocations supply the same standard input/output/error streams.
Standalone generated output uses `monk-runtime --abi 2 launch FILE [ARGS...]`.
This entry observes original descriptors before Fish replaces missing stdio
with `/dev/null`; invoking raw `fish FILE` cannot preserve that earlier state.
The launcher restores missing streams around the compiled body and preserves
source-entry identity. It executes Fish output, not runtime Bash text.
The reserved `MONK_LAUNCH_ORIGINAL` and `MONK_LAUNCH_WRAPPER` names cannot be
source bindings. Sourceable entry and deferred calls reject caller-provided
markers before effects, since private launcher metadata must not leak into
user external environments.
Owned transport records original open/closed descriptor state before GHC can
reuse descriptor numbers and preserves that state across child startup.
Admitted ordered redirections can open, duplicate or close user descriptors;
extra inherited descriptors are not admitted as owned merely because their
numbers exist. Direct native output uses a bounded byte writer to preserve write errors and
actual SIGPIPE even when stdout starts closed or its reader closes early. This
requires native support for output-only programs; silent scalar/control paths
can remain free of primitive helpers inside the native standalone entry.
Native read on a nonstandard descriptor requires an explicit
owned open/duplication. User descriptors and runtime control transport have
separate owners.

External commands are operating-system-executable dependencies: binaries or
scripts with an executable interpreter directive. Bash's ENOEXEC fallback,
which parses an otherwise uninterpreted runtime text file as Bash source, is
outside the dynamic-source envelope. The runtime fails explicitly on ENOEXEC
and never supplies an implicit Bash interpreter. This does not assume commands
remain present or executable forever: missing-file and permission failures
caused during execution still require Bash-compatible status and diagnostics.

External commands have the same executable behavior and consume ordinary
arguments, exported scalars and user streams. They must not inspect private
helpers, implementation processes, transport descriptors or shell identity and
depth state. In particular, direct or indirect `SHLVL` observation is excluded,
including through `printenv`, `env` or an external program. The
[concrete SHLVL counterexample](../../.superpowers/sdd/2026-09-22-portable-exact/inspection-boundary-review.md)
is an unsupported observation, not evidence of matching output. Ordinary
exported environment preservation, explicit `$!`/wait semantics and user
filesystem/descriptor effects remain obligations.

No unmodeled signal handler, event callback or asynchronous mutation may
intervene. Literal translated EXIT/ERR handlers and owned background jobs are
modeled effects. Combining directory operations with EXIT/ERR traps currently
rejects because shared stdio error state across directory signal handling is
not represented. External ambient callbacks and job-control builtins remain
excluded. The contract does not promise equivalence under exhaustion of the
resources required by generated helpers.

Requirements enumerate commands, typed Fish/platform capabilities, and native
runtime ABI, profile and operation sets. Generated support uses Fish plus the
Rust `monk-runtime`; Python is only a development/evidence tool. ABI 2 accepts
fixed operations over NUL-terminated byte frames. Zero frames and one empty
frame are distinct; NUL cannot occur inside a shell value. Child stdout/stderr
and original stdin are separate from framed control input. Captures remove
trailing newlines and Bash-disallowed NUL bytes, with the required diagnostic.

Combined/stdout output resolves `monk-runtime` on PATH, or the `--runtime FILE`
provider, once at each owning entry. It validates ABI/profile/native-target/operations before
body effects; unavailable or incompatible support reports `monk.runtime` and
returns 125. Installed providers must remain immutable and compatible during
execution. Managed output captures and validates provider bytes while planning,
then publishes and selects that exact executable at mode 0700. Children and
exported functions retain the selected generation across later publications.

The runtime uses raw bytes, explicit processes and owned POSIX descriptors. A
Rust pre-main constructor records stream presence and inherited signal policy
before standard-library startup. Parent-prepared POSIX-spawn actions or a bounded
native fork/exec path preserve ignored INT/QUIT, cwd identity and descriptor
ownership. Post-fork code calls only async-signal-safe libc operations using
precomputed pointers. It neither allocates nor unwinds. The Rust runtime has no
GHC RTS and ignores ambient `GHCRTS`.
Ordinary child script/state capsules use private mode-0600
files in a mode-0700 directory and do not require procfs or descriptor-path
reopening. Streaming process substitution uses real pipe endpoints and a
separate checked `/dev/fd/N` pipe-alias capability; it does not require an
independent seek offset or buffer producer data into files.

Bundling captures the executable for one validated native target, not its
dynamic libraries. Deployments must provide its compatible loader/libraries.
See the [native runtime specification](native-runtime.md).

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
The current caller contract does not admit standalone session ownership, dense
arrays, traps or temporary command-assignment scopes.
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
Repeated literal dependencies normalize each occurrence against its current
context while reusing immutable parse input. Child source calls are admitted;
function sources require absolute immutable targets and compatible call-time
facts. Source-local declarations and inherited caller argv mutation still reject.
Source lookup is fixed by discovery cwd/PATH/sourcepath and
immutable input snapshots, independently of publication destinations.

## Observable result and publication

The comparison covers stdout/stderr bytes, status and control flow, argument
boundaries, external filesystem effects, and declared caller-state changes.
Evaluation and child execution ordering are included when observable. The
supervised pipeline envelope owns builtin writer/SIGPIPE lifetime. Background
jobs and proved wait operands use actual owned PIDs. Streaming process endpoints
are admitted only where their pathname cannot escape through ordinary scalar
results. Literal EXIT/ERR handlers have deferred owned bodies; other signals and
unproved callback control transfers remain exclusions.

Publication has a separate failure contract: all generation members are staged
and flushed before one atomic entry replacement. Retries reflush an existing
verified generation before replacing the entry. Prior generations are retained,
so a reader pinned to an old loader remains consistent. A post-rename failure
reports the observed entry and possible durability uncertainty. This protocol
does not roll back effects of executing a translated program.
