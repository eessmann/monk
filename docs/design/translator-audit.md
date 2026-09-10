# Translator semantic audit

Refreshed 2026-09-10 for the native runtime and broader coverage. This document replaces the
older blanket exactness claims based on the 339-test suite.

## Contract and evidence

Exact means observable equivalence under the selected execution contract:
output bytes, status/control flow, argument boundaries, filesystem effects and
declared caller-state changes. Ordering is included when observable. The
initial profile is noninteractive Bash 5.3 with signed 64-bit arithmetic and
Fish 4.6, UTF-8 source and C locale on 64-bit Linux. Current local endpoints are
Bash 5.3.9 and Fish 4.6.0; other Bash profiles need separate evidence. The
[versioned execution profile](execution-profile.md) records startup options,
runtime requirements, checks and caller obligations.

A constructor's presence in the implementation is not evidence of exactness.
An opt-in permits only its named approximation. Rejected input produces no
executable artifact in either normal or strict translation. Caller promises
are obligations, not runtime proofs about arbitrary functions or handlers.

## Reproduced main defects

The [2026-09-09 Babelfish bake-off](../babelfish-comparison.md) adds standalone
execution evidence: all 38 admitted translations among 95 selected fixtures
match Bash stdout/stderr bytes and status; 57 reject. This bounded comparison
does not test filesystem or caller-state equivalence and changes no admission
classification.

All eleven programs below were accepted by main in strict mode without a
translation diagnostic. Their new regression group originally failed eleven
of fourteen tests; three useful controls passed.

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

The fixture directory and test sources are the durable counterexamples. The
current implementation pass/fail counts belong in the roadmap and final-tree
evidence; the old main result is a historical baseline.

## Current admission responsibilities

| Area | Exact envelope | Exclusions requiring diagnostics |
| --- | --- | --- |
| Words | ANSI byte literals, pre-expansion nested comma braces, lazy default/alternate modifiers, bounded trim/literal replacement; quoted scalars and empties; one quoted argv splice with adjacent scalar prefix/suffix; IFS splitting over data proved free of later glob characters; literal star/question-mark pathname patterns including Bash no-match behavior. | Multiple argv products, mixed splitting regions, unknown later globbing, bracket/extglob patterns and unimplemented parameter modifiers. |
| Arithmetic | Arithmetic commands, dollar parentheses/brackets and arithmetic-for headers; owned operator trees over proved numeric storage; signed-64 wrapping, intermediate truncation, ordered updates and lazy errors; context-specific error spelling retained from owned source. | Runtime expression strings, unsupported numeric spellings, array lvalues and loss of numeric facts through control joins. |
| Runtime options/control | Executed errexit/pipefail transitions, short-circuit suppression through calls, branches and loops; lazy case patterns with `;;`, `;&`, `;;&`; immediate owned loop control. | Other options, unsupported startup states, loop targets across source/function frames and undischargeable exception boundaries. |
| Functions/dispatch | Finite definite definition identities, declared imports and constant dynamic heads; explicit builtin/command lookup; compatible binding context; body-local scalar storage and invocation-time redirects. | Recursion, ambiguous/redefined dependencies, conditional/nested definitions or locals, local deletion, namerefs and unknown dispatch. |
| Binding state | One runtime owner for scalar presence, export state and inherited environment of unset locals; declaration operands expand before storage changes; scalar append evaluates RHS before reading and replacing the current binding. | Arrays, readonly enforcement without its one named opt-in, unrepresented caller attributes and reserved target bindings. |
| Sources | Immutable acyclic literal dependencies; continuation-based discovery with cwd/PATH/sourcepath; repeated occurrence execution under one compatible file entry context. | Computed sources, cycles, incompatible repeated contexts, deferred source calls in functions/children and inherited caller argv mutation; literal-source shifts require a provably nonempty effective argument frame. |
| Sourceable entry | Owned incoming/final status, return and explicit argv frame; declared visible/global scalar updates, imports and persistent installed functions with owned helpers. | Unknown relevant handlers, undeclared effects/dispatch, sourceable option or IFS mutation, unsupported attributes and nonlocal exits. |
| Child/shared effects | Command substitutions, subshells and bounded pipelines use owned snapshots, function closures, descriptor transport and status; byte/NUL capture behavior is explicit. | Background jobs, process substitution, traps, unproved transitive effects and nondraining builtin-writer signal lifetime. |
| Redirections | Ordered standard descriptor duplication/closure where effects are admitted, and `/dev/null`; shared compound state and deferred function redirects. | General file opens, computed targets, extra descriptors and unowned failure or scope lifetime. |
| Publication | Verified and flushed immutable generations before atomic loader replacement; pinned members, retained old generations, retry durability and observed-entry recovery under publisher serialization. | Ownership collisions, unsupported symlink layouts and unsafe destinations. |

These are the implemented conditional admission envelopes. Consult
[the roadmap](translator-todo.md) for final verification and remaining gaps,
[the full constructor policy](shellcheck-syntax-inventory.md) for syntax, and
[the architecture](architecture.md) for ownership. Do not interpret this table
as an unconditional promise about arbitrary Bash syntax or caller state.

## Stable directory slice (2026-09-10)

Standalone directory behavior requires `--directory-contract stable`.
Sourceable directory behavior requires explicit version 2 cwd/PWD/OLDPWD/stack
permissions; version 1 remains restrictive. The stable obligation includes
empty CDPATH and logical ancestry that external commands cannot rename or
invalidate. Direct PWD mutation, unknown directory paths, implicit HOME `cd`,
physical `cd` options and stack rotations remain rejected.

Local evidence adds the three existing fixtures `cd-tmp`, `pwd-cd` and
`pushd-popd`: all were rejected by the frozen baseline and now match Bash
stdout/stderr/status. A focused 142-case directory run passed with integrations
enabled and one test thread. It covers both entry modes, actual missing/file
errors, failed push/pop state, export preservation, spaces, symlink logical and
physical pwd, `cd -` after a successful edge, isolated child/substitution stacks,
a preexisting caller stack, and relative-source success/failure cwd edges.
The run also covers the independent permission matrix, exported functions after
entry return, and executed inline/separate source argv/return boundaries. The imported-cwd/deep-path cases and updated import/source fixture also pass
in the final 725-test compiler/runtime matrix; see the separate verification
record for release evidence. The detailed ledger is
`.superpowers/sdd/2026-09-10-native-runtime-coverage/directory-report.md`.

## Native common-syntax verification slice (2026-09-10)

The seven historical targets `pyramid-left`, `pyramid-right`,
`syntax-dollar-single-quote`, `syntax-dollar-bracket-arithmetic`,
`semver-normalize`, `neofetch-mini`, and `syntax-brace-expansion` now compare
exactly with Bash in stdout bytes, stderr bytes and exit status. They were all
explicit baseline rejections. They remain members of the original fixture
inventory; new interaction tests do not alter its denominator.

`Unit.PlannedCommonCoverage` exercises both standalone and sourceable entry,
including arithmetic-for header failures and nested continue ownership,
invalid-byte/NUL ANSI quotes, brace effect duplication, quoted lazy operands,
scalar append ordering, positional alternates, fixed-arity tests, and literal
source argv ownership. Unsupported arrays, computed operands and inherited
source argv writes retain explicit rejections.

Materialization shares native signed-integer operations for successful constant
folding and batches only total pure arithmetic islands. Failing arithmetic,
lazy alternatives and writes stay in distinct evaluation regions. Helper
closure follows structural command identities. Child snapshots are pruned
only when the closed child has no transitive external environment consumer;
export/fallback projections remain intact when external commands can observe
them. Effects justify removing unused option/IFS/substitution state and
substitution bookkeeping from ordinary assignments. These optimizations do
not relax source admission or replace the final suite and benchmark evidence
recorded in the roadmap.

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

## Native materialization and failure flow

Generated support uses native Fish and the versioned Haskell runtime described
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
