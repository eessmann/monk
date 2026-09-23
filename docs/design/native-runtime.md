# Native runtime ABI 2

Generated programs use native Fish where equivalence has been established and
`monk-runtime` for bounded byte and integer operations. The executable is built
entirely with Cargo from `runtime/`, using edition 2024 and pinned
nightly 2026-09-23. `rustix` 1.1.5 handles owned descriptors, byte I/O, filesystem,
pipes, Unix sockets, descriptor transfer and polling. `nix` 0.31.3 handles suitable
process/signal operations; maintained `libc` bindings cover the small private
native boundary. Clap 4.6.7 derives the CLI argument envelope using `OsString`;
protocol headers remain ordered and command arguments retain raw byte spelling,
including literal `--` and flag-like values. The crate is not published as a library; a narrow CLI entry
point and checked capability facade leave raw descriptor adoption and signal
authority private. No project C source or Haskell execution runtime remains.
The Haskell compiler, typed Fish DSL, semantic admission and publication APIs
are unchanged. The private compiler-support library retains the pure integer
specification, digest, ABI metadata and provider checks.

## Standalone entry

Run standalone generated Fish through the native entry:

```sh
monk-runtime --abi 2 launch ./translated.fish [args...]
```

The launcher records original standard-descriptor presence before Fish startup
and restores missing streams around the compiled body. Fish itself opens
missing standard streams as read/write `/dev/null`; no first command inside
Fish can recover the earlier state. Consequently raw `fish translated.fish`
is not the full standalone stream-fidelity entry. The launcher executes already
generated Fish, never Bash source, and does not itself select a session owner.
`MONK_LAUNCH_ORIGINAL` and `MONK_LAUNCH_WRAPPER` are reserved private markers;
they preserve source-entry identity and are removed from user external command
environments. Sourceable entry and deferred function calls reject either marker
if present in caller state, before source effects; they do not silently remove
caller-provided environment values. Sourceable output retains its declared
caller boundary.

## Selection and transport

`monk-runtime --describe` reports three lines:

```text
monk-runtime 2 bash53-i64
<space-separated capabilities>
target <native-target>
```

The shell-semantic profile is independent of the native target. Declared targets
are `x86_64-linux`, `aarch64-linux` and `aarch64-darwin`. Translated entries resolve
their provider once and verify ABI, profile, required capabilities and target.
`--runtime FILE` selects a provider; otherwise lookup uses PATH. NUL-delimited
resolution preserves embedded/trailing newlines. A provider must remain
immutable and compatible during execution. Missing or incompatible support
returns 125 before body effects with a `monk.runtime` diagnostic. ABI 1 providers
do not satisfy ABI 2 requirements; old immutable generations are retained.

Operations use `--abi 2 OPERATION`. For framed operations, each input value
ends in NUL; an empty stream
contains zero values and one NUL represents one empty value. Shell data cannot
contain NUL, but may contain invalid UTF-8. No operation evaluates supplied Bash,
Python, or Haskell expression strings. Protocol/arity failures return 125;
semantic statuses are operation-specific. Large values travel through pipes,
not process argument vectors. Metadata input never replaces the script's stdin.

| Family | Structured input and result |
| --- | --- |
| `launch FILE [ARGS...]` | Argv-only canonical standalone entry. Capture original standard streams before Fish startup and execute the compiled Fish file with preserved argv and source-entry identity. |
| `write-builtin` | NUL-framed source origin, positive line, builtin name and byte argv. Echo/printf use their finite existing byte implementations; echo-bytes accepts one precomputed payload. Raw source stdout, source-located errno/status1, initially closed descriptors and actual SIGPIPE are preserved. |
| `raise-signal 13` | Fixed argv-only operation with no stdin consumption; restore default unblocked SIGPIPE and terminate the owning evaluator by that signal. No arbitrary signal dispatch. |
| `exec-site` | Source origin/line, executable and argv; success replaces the primitive process, failure preserves source-located errno/status semantics. No implicit ENOEXEC source interpretation. |
| `integer` | An opcode and numeral operands, or a bounded postfix `batch` program of typed pure primitives. Result is three ASCII lines: `ok`, signed decimal, `-`; or `error`, `-`, first error identifier. |
| `split` | IFS and one byte value; output is zero or more framed fields. |
| `argv` | Prefix, suffix, empty-argv policy and positional values; output preserves argument cardinality and attachments. |
| `bytes-platform` | Two NUL-framed ASCII hexadecimal byte vectors, Darwin then Linux; select the native target's ANSI-literal result and emit one NUL-framed value, including an empty value. |
| `echo` | Arguments; output is Bash echo bytes, including option/escape handling and possible NUL output. |
| `pattern` / `pattern-parts` / `glob` | Typed match/trim/literal replacement and quote-activity/value fragments, including byte bracket classes. Results are status or framed byte values. No arbitrary regex or extglob interpreter. |
| `expansion` | Ordered quoted, literal-pattern and split fragments; IFS splitting, empty-field ownership and pathname expansion remain distinct. |
| `descriptor-state` | No input; exit status is the original standard-descriptor bitmask. |
| `child-run` / `child-capture` | Warning origin, descriptor mask, owned generated Fish script, SHLVL and snapshot/argv frames. Original stdin arrives separately on owned fd3. |
| `session-*` | Fixed owner/client requests for executable and compiled-region launches, jobs/wait, descriptor scopes, native read, streaming endpoints and bounded builtin writers. Private replies are separate from user streams. |
| `pipe-paths` | Real pipe-alias roundtrip and EOF preflight for endpoint-path consumers. |
| `directory-*` | Explicit bounded checks, physical cwd, stack formatting or conversion of actual parent Fish cd diagnostics. These operations do not change the parent's cwd. |

The integer specification uses unbounded Haskell intermediates with explicit
signed-64 wrapping after each primitive, truncation toward zero, bounded shift
counts, modular exponentiation and defined division edge cases. Constant
folding uses this retained specification; Rust implements equivalent bounded
integer operations with differential tests against that specification and Bash. Batching excludes writes, lazy control and
failing division/remainder/power boundaries; errors remain in their execution
regions and retain source origins. Independent Bash comparisons validate the
shared specification rather than merely comparing its two consumers.

## Child ownership

Child stdout/stderr remain separate from control transport. Capture drains raw
bytes, reports captured NUL, removes NULs and strips only trailing line feeds.
Mode-0600 script/state files live inside an owned mode-0700 capsule; each consumer
opens an independent offset. Parent ownership keeps the capsule alive until the
child finishes, then removes it. This transport carries compiled Fish and
framed state, never arbitrary Bash source or process-substitution producer data.

Rust pre-main constructors capture original stream presence and inherited
ignored INT/QUIT. Internally reserved standard descriptors remain valid for Rust;
child execution restores actual absent streams. A failed executable replacement
restores both descriptor reservations and signal state before reporting errors
or attempting another PATH candidate. `-Zon-broken-pipe=inherit` and explicit
operation policy preserve actual signal termination.

Prepared launches own argv, environment, cwd and descriptor mappings. Consuming
launch and completion transitions, borrowed descriptors and endpoint leases
make ownership explicit. Fixed private pattern types bound scalar invariants;
they never cross FFI or wire boundaries. Ordinary launches use POSIX spawn;
ignored INT/QUIT uses a bounded fork/exec leaf over precomputed data. Only audited
async-signal-safe libc calls occur after fork: no allocation, formatting,
unwinding or general destructors. POSIX spawn returns positive errno values,
which the native RAII wrappers check directly. Executable lookup never invokes
an implicit shell fallback. Raw wait statuses retain Linux real-time signals.

One session owner mutates signal/spawn state, reaps children and caches status.
Signal handlers only notify. A bounded signal wake thread owns no descriptors
and never reaps; it interrupts the owner thread even when an event arrived just
before a blocking syscall. Shutdown joins it before restoring handlers. Source
read state survives transient interruptions. Normal owner completion leaves
background children alive; explicit failure cleanup terminates owned foreground
work. Guardian leases survive launcher-to-owner transfer and clean capsules on
owner death, with an absolute 60-second authentication deadline.

Private descriptors do not escape into external commands. Linux can use
close_range with a bounded fallback; Darwin inventories descriptors independently
of the current soft limit. Both use native cwd capabilities after rename/unlink.
All temporaries use explicit 0600 file / 0700 directory modes.


## Supervised owner and streaming endpoints

A materialized `NativeSession` requirement selects supervised execution. The
session owner holds user descriptor scopes, child PIDs and cached statuses.
Requests carry fixed operations and byte frames. A search-only cwd descriptor
travels separately from user streams, so rename/unlink does not turn process
cwd identity into a required surviving pathname. Source opens use openat and
child launch uses fchdir actions. The evaluator continues to run
generated Fish. Ordered file opens happen once under that owner. Native read
returns status plus an assignment flag and fields, so a bad descriptor leaves
bindings intact while EOF can assign a partial/empty result. Dense arrays retain
each element's empty/newline/non-UTF8 bytes in vector frames.

Native builtin writers share the owner's descriptors and signal lifetime.
Finite pipelines supervise their stages and preserve SIGPIPE termination,
including callback/cleanup ordering. Background launches establish actual owned
PIDs; wait validates operands, tracks repeated status queries and distinguishes
ordinary jobs from asynchronous process substitutions.

Process substitution allocates a real pipe and an inherited descriptor endpoint.
The separate `pipe-paths` capability probes `/dev/fd/N` alias and EOF behavior.
Only pipe aliasing is needed; no independent-offset reopen is assumed. The owner
leases the consuming endpoint into the next owned invocation or descriptor
scope, excludes it from the producer, and closes its own unused references.
Normal completion does not synchronously wait for every substitution producer.
No FIFO pathname race or temporary producer-data file is used. The frontend
rejects endpoint storage/printing and unproved escaping consumers.

Private capsule, socket and reply names are implementation state guarded before
source effects. They are not user descriptor bindings or observable scalar
results. The [execution profile](execution-profile.md) defines the external
inspection boundary and its concrete SHLVL exclusion.

## Managed images and statistics

`--managed -o ENTRY` captures the selected provider bytes while planning,
validates the captured image, binds its generation-relative reference, and
materializes and admits the final output again. The opaque bundle exposes binary
image bytes, digest, required operations, ABI/profile, target and mode through
inspection functions. Arbitrary images cannot be forged by record updates.

Generation identity covers artifact role, path, mode and bytes. Runtime members
are staged at 0700, checked for compatibility and flushed before the entry
loader is atomically replaced. Reuse compares existing bytes and modes as data;
it never executes a possibly modified destination member. Existing loader,
recovery, publisher serialization and retained-generation rules still apply.
Children and exported functions keep the selected generation's absolute runtime
path, so later publication does not retarget previously installed functions.

Bundling includes one executable for the validated native target. The verification
report records its dynamic library dependencies; this is not a universally
portable static binary. Static materialization statistics count emitted helper
definitions, helper references, native operation call sites and Fish bytes,
including structural embedded child bodies. Measured launches, binary bytes,
bundle bytes and elapsed times are separate evidence.


Portable target declarations do not substitute for execution evidence. The
[Rust migration report](rust-runtime-verification.md) records the current
implementation and remaining execution gates. The historical
[portable runtime report](../../.superpowers/sdd/2026-09-22-portable-exact/portable-runtime-report.md)
records canonical local Darwin tests, the witnessed pre-exec GHC crash and its
POSIX-spawn repair, byte-pattern oracle checks and remaining Linux verification.
The roadmap owns final compiler, shell, package and frozen-corpus results.


## Direct output and exact failures

DirectExecution does not imply no primitive calls. Ordinary printf/echo output
uses one captured-provider writer helper per generated program; it reads framed
metadata and arguments, writes raw bytes without temporary data files, and
preserves source diagnostics. The helper owns one result slot and propagates
status141 through the fixed raise-signal operation so the evaluator terminates
by actual SIGPIPE. Empty output remains successful even with closed stdout.

This is a necessary exception to the original helper-free greeting target.
With initially closed stdout, direct Fish printf returned0 silently where Bash
returned1 with a write-error diagnostic; with an already closed pipe reader,
Fish returned1 where Bash terminated by SIGPIPE. Those admitted I/O states were
not removed from the profile. Silent scalar/control bodies can remain free
of primitive helpers; standalone stream fidelity still uses the native entry. The approved historical plan is retained unchanged.
