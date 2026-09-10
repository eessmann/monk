# ShellCheck syntax policy

Input dependency: ShellCheck 0.11.0. Refreshed 2026-09-10.

This is the complete constructor inventory, replacing the earlier six-row
sampling. An **exact envelope** is a conditional policy, not a claim that every
shape of that constructor works. The normalizer must reject a form outside its
specified context before producing executable output. The roadmap records
which envelopes have completed materialization and differential evidence.

`tokenKind` in the authoritative normalizer matches all constructors without a
wildcard so a dependency extension fails the warning-clean build until reviewed.
There is no independent allowlist that authorizes a later legacy AST traversal.

| Constructor | Policy | Behavioral requirement / reason |
| --- | --- | --- |
| `TA_Assignment` | Exact envelope | Closed signed-64 operator tree and numeric operands; bounded primitive helper, ordered mutations and lazy errors. Expression strings and unsupported numeric/storage forms reject. |
| `TA_Binary` | Exact envelope | Closed signed-64 operator tree and numeric operands; bounded primitive helper, ordered mutations and lazy errors. Expression strings and unsupported numeric/storage forms reject. |
| `TA_Expansion` | Exact envelope | Closed signed-64 operator tree and numeric operands; bounded primitive helper, ordered mutations and lazy errors. Expression strings and unsupported numeric/storage forms reject. |
| `TA_Parenthesis` | Exact envelope | Closed signed-64 operator tree and numeric operands; bounded primitive helper, ordered mutations and lazy errors. Expression strings and unsupported numeric/storage forms reject. |
| `TA_Sequence` | Exact envelope | Closed signed-64 operator tree and numeric operands; bounded primitive helper, ordered mutations and lazy errors. Expression strings and unsupported numeric/storage forms reject. |
| `TA_Trinary` | Exact envelope | Closed signed-64 operator tree and numeric operands; bounded primitive helper, ordered mutations and lazy errors. Expression strings and unsupported numeric/storage forms reject. |
| `TA_Unary` | Exact envelope | Closed signed-64 operator tree and numeric operands; bounded primitive helper, ordered mutations and lazy errors. Expression strings and unsupported numeric/storage forms reject. |
| `TA_Variable` | Exact envelope | Closed signed-64 operator tree and numeric operands; bounded primitive helper, ordered mutations and lazy errors. Expression strings and unsupported numeric/storage forms reject. |
| `TC_And` | Exact envelope | Owned double-bracket short-circuit conditions preserve right-operand expansion and effects in the taken branch. |
| `TC_Binary` | Exact envelope | Quote-aware =/==/!= patterns, or integer comparisons over proved numeral data. Expression strings, unknown numeric operands, regex and other operators reject. |
| `TC_Empty` | Rejected | An empty condition is not an admitted predicate. |
| `TC_Group` | Exact envelope | Double-bracket grouping retains its owned lazy condition subtree. |
| `TC_Nullary` | Exact envelope | One scalar condition tests nonempty bytes; no field splitting or pathname expansion. |
| `TC_Or` | Exact envelope | Owned double-bracket short-circuit conditions preserve right-operand expansion and effects in the taken branch. |
| `TC_Unary` | Exact envelope | Admitted scalar -n/-z and file -e/-f/-d/-r/-w/-x predicates only; other unary operators reject. |
| `T_AND_IF` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_AndIf` | Exact envelope | Short-circuit control and runtime status/options stay in the executed region; unsupported nonlocal outcomes reject. |
| `T_Annotation` | Exact envelope | Owned structural wrapper; redirects and multistage pipelines require their own admitted materialization, never transparent deletion. |
| `T_Arithmetic` | Exact envelope | Closed signed-64 operator tree and numeric operands; bounded primitive helper, ordered mutations and lazy errors. Expression strings and unsupported numeric/storage forms reject. |
| `T_Array` | Rejected | Outside the initial implemented storage, control or isolation envelope; no comment/false or silent-success replacement. |
| `T_Assignment` | Exact envelope | Replacing scalar assignment with an admitted storage binding. Prefix lifetime, attributes, array/append forms require separate storage evidence. |
| `T_Backgrounded` | Rejected | Outside the initial implemented storage, control or isolation envelope; no comment/false or silent-success replacement. |
| `T_Backticked` | Rejected | Legacy backtick preprocessing has no owned scalar normalization; use an admitted $() child expression. |
| `T_Bang` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_Banged` | Exact envelope | Short-circuit control and runtime status/options stay in the executed region; unsupported nonlocal outcomes reject. |
| `T_BatsTest` | Rejected | Bats is outside the Bash execution profile. |
| `T_BraceExpansion` | Exact envelope | Nested comma products distribute before expansion; each result owns fresh scalar/effect occurrences. Numeric ranges and unsupported products remain excluded. |
| `T_BraceGroup` | Exact envelope | Owned structural wrapper; redirects and multistage pipelines require their own admitted materialization, never transparent deletion. |
| `T_CLOBBER` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_Case` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_CaseExpression` | Exact envelope | Patterns evaluate lazily; ;; stops, ;& executes the next body, ;;& retests subsequent patterns. |
| `T_CoProc` | Rejected | Outside the initial implemented storage, control or isolation envelope; no comment/false or silent-success replacement. |
| `T_CoProcBody` | Rejected | Outside the initial implemented storage, control or isolation envelope; no comment/false or silent-success replacement. |
| `T_Condition` | Exact envelope | Only condition operators with admitted operand cardinality/comparison semantics; remaining operators reject. |
| `T_DGREAT` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_DLESS` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_DLESSDASH` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_DSEMI` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_Do` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_DollarArithmetic` | Exact envelope | Closed signed-64 operator tree and numeric operands; bounded primitive helper, ordered mutations and lazy errors. Expression strings and unsupported numeric/storage forms reject. |
| `T_DollarBraceCommandExpansion` | Rejected | Outside the initial implemented storage, control or isolation envelope; no comment/false or silent-success replacement. |
| `T_DollarBraced` | Exact envelope | Named scalar, positive positional, status/count and quoted argv; admitted lazy scalar default/assign-default operands preserve unset/empty distinctions. Unimplemented modifiers and operand shapes reject. |
| `T_DollarBracket` | Exact envelope | Signed-64-bit arithmetic with original dollar-bracket source spelling and error evidence. |
| `T_DollarDoubleQuoted` | Rejected | Brace product, extended pattern, or locale-dependent translation needs separate semantics and evidence. |
| `T_DollarExpansion` | Exact envelope | Owned child execution/capture with trailing-newline removal; reject effects that cannot be isolated. |
| `T_DollarSingleQuoted` | Exact envelope | C-locale ANSI byte decoding, invalid UTF8 byte preservation and NUL termination; unsupported control operands reject. |
| `T_Done` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_DoubleQuoted` | Exact envelope | Preserve quoted scalar versus field-list context, including empty words and embedded positional arguments. |
| `T_EOF` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_Elif` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_Else` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_Esac` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_Extglob` | Rejected | Brace product, extended pattern, or locale-dependent translation needs separate semantics and evidence. |
| `T_FdRedirect` | Exact envelope | Ordered standard descriptor duplication/close and the declared /dev/null envelope only; arbitrary file opens, computed targets and nonstandard descriptors reject. |
| `T_Fi` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_For` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_ForArithmetic` | Exact envelope | Owned arithmetic headers, empty predicate, loop result and matching continue increment; array/runtime-expression operands and unsupported loop effects reject. |
| `T_ForIn` | Exact envelope | Owned binder storage and caller-write admission; finite iteration facts and loop result status. Immediate break/continue target the owned loop; depth operands and unsupported nonlocal outcomes reject. |
| `T_Function` | Exact envelope | Finite definite definitions and compatible call contexts; direct-body locals; invocation-time redirects. Recursion and incompatible contexts reject. |
| `T_GREATAND` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_Glob` | Exact envelope | Literal star/question pathname fragments, quoted literal fragments, C-byte sorting, default dotfile exclusion and literal no-match fallback. Brackets, extglob and dynamic fragments reject. |
| `T_Greater` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_HereDoc` | Rejected | Outside the initial implemented storage, control or isolation envelope; no comment/false or silent-success replacement. |
| `T_HereString` | Rejected | Input byte, buffering and descriptor lifetime semantics are not materialized. There is no here-string approximation identifier. |
| `T_If` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_IfExpression` | Exact envelope | Short-circuit control and runtime status/options stay in the executed region; unsupported nonlocal outcomes reject. |
| `T_In` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_Include` | Rejected at parser boundary | ShellCheck include expansion is disabled. Monk owns literal source occurrences, dependency identity and normalization continuations. |
| `T_IndexedElement` | Rejected | Outside the initial implemented storage, control or isolation envelope; no comment/false or silent-success replacement. |
| `T_IoDuplicate` | Exact envelope | Duplicate or close standard descriptors in source order. Duplicating a closed source, builtin writes through closed/read-only stdout, and incompatible function descriptor contexts reject. |
| `T_IoFile` | Exact envelope | The bounded /dev/null envelope only. Arbitrary file opens need owned errors and descriptor lifetime; preflight and reopen cannot establish exactness. |
| `T_LESSAND` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_LESSGREAT` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_Lbrace` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_Less` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_Literal` | Exact envelope | Preserve quoted scalar versus field-list context, including empty words and embedded positional arguments. |
| `T_Lparen` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_NEWLINE` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_NormalWord` | Exact envelope | Preserve quoted scalar versus field-list context, including empty words and embedded positional arguments. |
| `T_OR_IF` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_OrIf` | Exact envelope | Short-circuit control and runtime status/options stay in the executed region; unsupported nonlocal outcomes reject. |
| `T_ParamSubSpecialChar` | Exact envelope | Consumed only as a parameter grammar fragment by its owning braced expansion; never independently executable. |
| `T_Pipe` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_Pipeline` | Exact envelope | Owned child stages; builtin writers require proved unconditional draining downstream stages. Unknown signal lifetime, background execution and unsupported descriptors reject. |
| `T_ProcSub` | Rejected | Asynchronous producer/consumer timing, descriptor lifetime and effects remain undischargeable; old synchronous temporary-file replacement is not exact. |
| `T_Rbrace` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_Redirecting` | Exact envelope | Owned structural wrapper; redirects and multistage pipelines require their own admitted materialization, never transparent deletion. |
| `T_Rparen` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_Script` | Exact envelope | Owned structural wrapper; redirects and multistage pipelines require their own admitted materialization, never transparent deletion. |
| `T_Select` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_SelectIn` | Rejected | Outside the initial implemented storage, control or isolation envelope; no comment/false or silent-success replacement. |
| `T_Semi` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_SimpleCommand` | Exact envelope | Resolve command identity before builtin interception; validate operands. Unknown dynamic dispatch and eval reject. |
| `T_SingleQuoted` | Exact envelope | Preserve quoted scalar versus field-list context, including empty words and embedded positional arguments. |
| `T_SourceCommand` | Rejected at parser boundary | ShellCheck include expansion is disabled. Monk owns literal source occurrences, dependency identity and normalization continuations. |
| `T_Subshell` | Exact envelope | Owned child snapshot, function closure, status and byte transport; unsupported descriptors/imports/introspection reject. No shared-scope approximation exists. |
| `T_Then` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_UnparsedIndex` | Rejected | Outside the initial implemented storage, control or isolation envelope; no comment/false or silent-success replacement. |
| `T_Until` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_UntilExpression` | Exact envelope | Short-circuit control and runtime status/options stay in the executed region; unsupported nonlocal outcomes reject. |
| `T_While` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_WhileExpression` | Exact envelope | Short-circuit control and runtime status/options stay in the executed region; unsupported nonlocal outcomes reject. |

## Context admission

| Context | Required distinction |
| --- | --- |
| Command head | Definite local/import identity before builtin lookup; provably constant dynamic heads only; no arbitrary eval. |
| Command argument | Zero/one/many fields, quotation and empty arguments. IFS splitting requires proved absence of later wildcard expansion; literal pathname globs have a separate plan. Mixed split regions and multiple argv products reject. |
| Scalar assignment | Ordinary assignments are sequential; local/export declaration operands all expand before binding installation. Logical export state and environment fallback belong to the runtime binding owner. |
| Redirect target | Only the admitted standard descriptor and /dev/null envelope; ordered invocation effects. Arbitrary file opens and computed filenames reject until errors and lifetime have an owner. |
| Case subject/pattern | Scalar subject, separately owned lazy pattern evaluation and all three terminators. |
| Arithmetic command/expansion | Same operator tree but different status, error and continuation rules; retain source spelling for observable errors. |
| Condition/and-or/negation | Actual short-circuit region and errexit suppression boundary, including transitive calls. |
| Function definition/invocation | Definition is an effect; body and redirects execute at invocation under a compatible finite binding context. |
| Source body | Owned return/argv/status boundary and caller-flow effects, joining every possible return edge; acyclic literal dependency snapshot with occurrence identity. Source inside functions/children and escaping source-loop control reject. |
| Substitution/pipeline/job | Distinguish shared from child storage, status, timing and callback effects; retain exact cases only with isolation evidence. |
| Sourceable entry | Explicit caller obligations, runtime-checkable scalar/lookup facts, declared updates and relevant handler assumptions. |

Named opt-ins change only the named semantic approximation. They never suppress
unrelated diagnostics or authorize unknown dispatch, sources, storage or
callbacks. Rejection tests must identify a stable diagnostic with source
location; each useful exact envelope also requires positive execution tests.

The only approximation identifier is `readonly-unchecked`, produced by admitted
standalone `readonly name=value` declarations with exactly one assignment operand
and a located warning. Multiple declaration operands reject rather than changing
their expansion order. It is a
command policy under `T_SimpleCommand`, not a new parser constructor. Selecting
it does not admit declaration inspection, sourceable readonly attributes,
arrays, eval or any unrelated excluded form.

## Directory builtin operand envelope (2026-09-10)

Within `T_SimpleCommand`, stable-contract directory calls lower to a dedicated
semantic operation. The supported forms are `cd DIR`, `cd -- DIR`, `cd -L DIR`,
`cd -L -- DIR`, a proved `cd -`, `pwd`, `pwd -L`, `pwd -P`, `pushd DIR` and
no-argument `popd`. Directory operands must resolve at normalization to one
ordinary path. Leading `..` is supported; an interior `name/..` is rejected.
An explicit literal assignment can prove a later variable operand. An earlier
successful directory edge can prove the ordinary previous directory for
`cd -`; an unconditional possibly failing call cannot invent that fact.

An empty CDPATH, valid ordinary exported global PWD and stable logical ancestry
are contract requirements. Implicit HOME `cd`, `cd -P/-e/-@`, dynamic paths,
rotations and direct PWD mutation remain outside the envelope. A relative
source after an uncertain directory transition rejects. On a proved success
or failure edge it uses that edge's execution cwd, never the source file's
containing directory. Relative sources in a directory-changing loop need an
invariant absolute execution cwd.

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
