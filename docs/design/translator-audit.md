# Translator semantic audit

Refreshed 2026-09-23 for the indexed compiler and typed Rust runtime. This document describes the
conditional admission envelope; test results and platform evidence belong in
the [typed architecture verification](typed-architecture-verification.md) and
the earlier [Rust verification report](rust-runtime-verification.md).

## Contract and evidence

Exact means observable equivalence under the selected execution contract:
output bytes, status/control flow, argument boundaries, filesystem effects and
declared caller-state changes. Ordering is included when observable. The
initial profile is noninteractive Bash 5.3 with signed 64-bit arithmetic and
Fish 4.6, UTF-8 source and C locale. ABI 2 declares x86_64 Linux, aarch64
Linux and aarch64 Darwin native targets. Current local runtime evidence is
Apple Silicon Darwin; declaration of a Linux target is not a Linux test result.
Current local endpoints are
Bash 5.3.9 and Fish 4.6.0; other Bash profiles need separate evidence. The
[versioned execution profile](execution-profile.md) records startup options,
runtime requirements, checks and caller obligations.

A constructor's presence in the implementation is not evidence of exactness.
An opt-in permits only its named approximation. Rejected input produces no
executable artifact in either normal or strict translation. Caller promises
are obligations, not runtime proofs about arbitrary functions or handlers.

The runtime replacement preserves this semantic envelope. Ownership/type tests
prevent invalid resource reuse but do not establish Bash equivalence by
themselves: byte, process, signal, publication and packaged execution evidence
remain independent requirements. See [Rust migration verification](rust-runtime-verification.md).

The indexed cleanup preserves the reviewed admission classifications. It closes
construction holes in pipeline grammar, redirects, flow evidence, control scopes,
request arity and publication lifetime. Those type tests supplement execution
comparisons. Fish control-word function targets now receive a located rejection
before materialization; accepting such a name never established executable Fish
semantics. See the [public API migration](typed-architecture-migration.md).

## Semantic regression responsibilities

The fixtures below preserve concrete failure modes that the translator must
handle or reject. A passing aggregate test count does not replace these
behavioral obligations.

| Fixture | Defect | Required closure |
| --- | --- | --- |
| `untaken-option` | An unexecuted branch changed translator option state. | Runtime option update only on the executed path. |
| `uncalled-option-function` | An uncalled function body changed later behavior. | Defer body effects to invocation. |
| `ifs-set` | Field splitting lost Bash boundaries. | Explicit zero/one/many field plan with IFS semantics. |
| `quoted-argv-adjacent` | Quoted positional arguments collapsed or duplicated fields. | Preserve prefix/suffix attachment and empty argv cases. |
| `case-unreached-effect` | A pattern's expansion ran before its arm was reached. | Pattern evaluation belongs to the lazy matching region. |
| `case-fallthrough` | Case terminator semantics were discarded. | Represent stop, unconditional next body, and retest explicitly. |
| `arithmetic-integral-intermediate` | Floating evaluation changed integer intermediate results. | Operator-tree signed 64-bit primitives with per-operation truncation. |
| `dynamic-local` | Lexical local classification lost Bash's dynamic binding. | Compatible invocation context and verified body-local storage. |
| `dynamic-command` | A provably constant command head disappeared. | Resolve definite command identity before lowering. |
| `array-mixed` | Mixed dense/sparse array initialization silently lost elements. | Reject until sparse storage is implemented. |
| `eval-bash-syntax` | Bash program text was interpreted as Fish. | Reject arbitrary eval without executable output. |

The source programs in `test/fixtures/semantic/` and their test cases retain
these counterexamples independently of old comparison reports.

## Current admission responsibilities

| Area | Exact envelope | Exclusions requiring diagnostics |
| --- | --- | --- |
| Words | ANSI byte literals; nested comma braces; lazy default/alternate modifiers; quote-aware shortest/longest trim with bracket classes and proved active fragments; bounded literal replacement; quoted scalars/empties and one quoted argv or array splice; composed IFS splitting and byte pathname expansion with no-match fallback. | Multiple argv/array products; unknown active trim patterns; extglob; pattern replacement and other unimplemented modifiers. |
| Arithmetic | Owned signed-64 operator trees, ordered updates and lazy errors over proved numeric storage; arithmetic commands, substitutions and for headers; context-specific failure spelling. | Runtime expression strings, unsupported numeric spellings, array lvalues and lost numeric facts. |
| Runtime options/control | Executed errexit/pipefail, tested-context suppression, lazy case arms and immediate loop targets. An unobserved underscore loop binder is a discard. | Other options, unsupported startup states, explicit underscore inspection and nonlocal loop targets. |
| Functions/dispatch | Source-located exec failure handling for OS-executable dependencies; definite finite definitions, declared imports, constant command heads, explicit builtin/command lookup, body-local storage, child-local unconditional definitions and invocation-time redirects. | Implicit ENOEXEC Bash-source fallback; recursion, ambiguous/redefined dependencies, conditional or active-function-nested definitions, unproved local lifetimes, namerefs and unknown dispatch. |
| Binding state | Scalar presence/export/fallback ownership; ordered declarations and scalar append; temporary exported bindings for ordinary commands; IFS-only temporary read bindings. | Prefix writes to their own temporary destination set, prefixed function/source/eval/declaration calls, unrepresented caller attributes and readonly enforcement without its named opt-in. |
| Dense arrays | Standalone owned construction, append, statically contiguous writes, element/count reads and quoted splices; runtime-dense read arrays support append and index-zero updates without losing unknown length. | Sparse/associative arrays, unknown-index writes, array export/declarations/arithmetic lvalues, preexisting ambient array-name bindings and sourceable arrays. |
| Sources/eval | Immutable acyclic source input reused per occurrence with lexical runtime diagnostic origins independent of canonical identity; child sources and absolute immutable function sources; literal or flow-proved eval parsed during translation with effects in order. | Computed sources, cycles, stale call-time dependencies, source-local declarations and inherited caller argv mutation; arbitrary eval, nested diagnostic forms not yet mapped and unbounded expansion. |
| Sourceable entry | Incoming/final status, owned return/argv frames, declared scalar updates/imports and persistent installed functions. | Unknown ambient effects; undeclared effects/dispatch; new session operations, arrays, traps, temporary command prefixes and sourceable option/IFS mutation. |
| Children/jobs | Command capture, subshells, finite closures, snapshots, general supervised pipelines, owned background PIDs and wait, including native builtin SIGPIPE lifetime. | Job-control syntax, unproved wait operands, inherited jobs and implementation-process inspection. |
| Process substitution | Streaming input/output endpoints consumed by admitted byte consumers or ordered owned redirects; asynchronous lifetime and explicit endpoint capability. | Storing or printing endpoint paths, arbitrary escaping consumers, concatenated endpoints and unproved synthetic diagnostics. |
| Redirections/read | Ordered file read/write/append/read-write, duplication/closure, scoped user descriptors, heredocs/here strings; byte read with literal -r/-d/-n/-u/-a, REPLY/scalars/dense arrays. | Unowned inherited extra descriptors, descriptor numbers above 255, effectful ordinary paths/input, unproved redirected assignment expansion order, read -N/-t/-p/-s and computed flags. |
| Traps | Literal EXIT/ERR bodies with live scalar reads, pure builtin/control effects, replacement/reset and explicit exit; child reset and default function ERR suppression. | Other signals, unknown source text, external/function handler lookup, return/break/continue, directory-operation combinations and handler operations with unproved lifetime or synthetic diagnostics. |
| Publication | Verified/flushed immutable generations, target-checked native images and one atomic loader replacement; retained old generations and observed-entry recovery. | Ownership collisions, unsafe destinations, unsupported symlink layouts and cross-target executable reuse. |

These are the implemented conditional admission envelopes. Consult
[the roadmap](translator-todo.md) for final verification and remaining gaps,
[the full constructor policy](shellcheck-syntax-inventory.md) for syntax, and
[the architecture](architecture.md) for ownership. Do not interpret this table
as an unconditional promise about arbitrary Bash syntax or caller state.

## Stable directory contract

Standalone directory behavior requires `--directory-contract stable`.
Sourceable directory behavior requires explicit version 2 cwd/PWD/OLDPWD/stack
permissions; version 1 remains restrictive. The stable obligation includes
empty CDPATH and logical ancestry that external commands cannot rename or
invalidate. Direct PWD mutation, unknown directory paths, implicit HOME `cd`,
physical `cd` options and stack rotations remain rejected.

Directory tests cover both entry modes, missing/file errors, failed push/pop
state, export preservation, spaces, symlink logical and physical paths,
`cd -`, isolated child/substitution stacks, preexisting caller stacks and
relative-source cwd edges. Caller permissions and exported functions remain
separate obligations.

## Lowering and optimization invariants

`Unit.PlannedCommonCoverage` exercises standalone and sourceable entry,
arithmetic-for failures and loop ownership, invalid-byte/NUL ANSI quotes, brace
effect duplication, quoted lazy operands, scalar append ordering, positional
alternates, fixed-arity tests and literal-source argv ownership. These tests
exercise the admitted cases; they do not remove the exclusions above.

Materialization uses the retained Haskell integer specification for successful
constant folding and batches only total pure arithmetic islands for the Rust
runtime. Failing arithmetic,
lazy alternatives and writes stay in distinct evaluation regions. Helper
closure follows structural command identities. Child snapshots are pruned
only when the closed child has no transitive external environment consumer;
export/fallback projections remain intact when external commands can observe
them. Effects justify removing unused option/IFS/substitution state and
substitution bookkeeping from ordinary assignments. These optimizations do
not relax source admission or replace the final suite and benchmark evidence
recorded in the roadmap.

The lexical directory envelope limits each UTF-8 path component to 255
bytes and the operand to 4095 bytes. These are lexical admission limits, not
filesystem existence checks. Longer operands reject because Fish can emit its
ENAMETOOLONG diagnostic outside the builtin stderr stream that the parent
operation captures. Control-byte and non-ASCII operands within the envelope
use Bash ANSI-C diagnostic quoting.

The resolved logical directory path is additionally limited to 1023 bytes on
Darwin and 4095 bytes on Linux.
A pure lexical runtime check enforces that obligation before parent cd, using
the actual PWD and operand; a violation returns/exits with status 125 before
the attempted directory operation. This guard performs no filesystem target
precheck.

## Native materialization and failure flow

Generated support uses native Fish and the versioned Rust runtime described
in [the runtime specification](native-runtime.md). No Monk-generated Python support remains.
Helpers consume framed bytes and typed bounded operations. Native images are
opaque captured products; generation identity includes role, path, mode and
bytes, and existing generation members are verified as data before reuse.

Arithmetic flow retains separate successful and possible failing outcomes.
A later assignment in a sequence is not definite when an earlier operation can
fail. Loop continuation facts belong to their actual depth; break exits also
participate in post-loop numeric admission. Constant folding cannot turn these
partial updates into unconditional facts. Literal sources inherit caller argv
when operand expansion produces zero fields; a quoted empty field still owns
a one-argument frame. Transitive writes through inherited/unknown frames reject.

## Process and inspection boundaries

The current implementation has no general Bash interpreter fallback. Finite
source/eval text and literal callbacks are parsed by ShellCheck during
translation; the runtime receives fixed operations and generated Fish regions.
Capabilities describe required behavior, while the native target describes
which executable can provide it. The final compiler/runtime/package matrix and
frozen fixture comparison remain separate evidence in the roadmap.

Implementation shell-depth inspection is outside the execution profile, even
when an external program hides the read. The profile preserves the concrete
[SHLVL counterexample](execution-profile.md#shell-identity-inspection) and explains
why an environment offset cannot establish general equivalence. This exclusion
does not waive ordinary exported environment preservation, user descriptor
effects, explicit `$!`/wait behavior or process cleanup.

External exec failures are not waived by the dependency contract: removing a
command or changing its execute permission during a script must retain the
reference status and source diagnostic. Direct execution uses a bounded
replace-self exec primitive, while supervised dispatch uses the same error
policy under its process owner. OS binaries and shebang scripts are the external
executable envelope; Bash's implicit ENOEXEC interpretation of arbitrary runtime
text remains excluded by the no-dynamic-Bash-fallback boundary.

Fish printf can silently return 0 with initially closed stdout where Bash
returns 1 and a source write diagnostic, and return 1 for a readerless pipe
where Bash terminates by SIGPIPE. The bounded native write-builtin boundary
corrects this admitted behavior; it is an explicit
exception to the historical helper-free greeting optimization requirement.
The stream contract is unchanged. DirectExecution may use this one writer
helper without introducing a session owner or a runtime source interpreter.

Canonical standalone execution now uses `monk-runtime --abi 2 launch FILE
[ARGS...]` to record missing stdio before Fish replaces it with read/write
/dev/null. This is an explicit selected entry mechanism, not a restriction
removing the closed-stream counterexample. Raw Fish invocation cannot recover
pre-startup descriptor state. Silent bodies may remain primitive-free while
using the same entry; DirectExecution does not imply semantic supervision.
