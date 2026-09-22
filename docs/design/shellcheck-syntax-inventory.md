# ShellCheck syntax policy

Input dependency: ShellCheck 0.11.0. Refreshed 2026-09-22.

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
| `T_Array` | Exact envelope | Standalone dense replacement/append with one proved field per initializer; typed length and element identity. Sparse/associative/indexed initializer elements, unknown cardinality and sourceable array storage reject. |
| `T_Assignment` | Exact envelope | Sequential scalar replacement/append; dense array construction and statically contiguous literal-index writes; temporary command bindings with owned export/scope lifetime. Temporary RHS writes into prefix destinations and unproved array shapes reject. |
| `T_Backgrounded` | Exact envelope | Standalone owned child launch with isolated snapshot, actual PID, zero launch status and wait lifetime. Inherited jobs, job-control syntax and sourceable session operations reject. |
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
| `T_DollarBraced` | Exact envelope | Named/positional scalars, status/count, last owned job PID, array element/count and one quoted argv/array splice; lazy defaults/alternates; quote-aware proved trim and literal replacement. Unknown active trim, sparse/unknown array shapes and unimplemented modifiers reject. |
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
| `T_FdRedirect` | Exact envelope | Ordered owned duplication/close/file/input redirects through bounded user descriptors. Extra descriptor numbers need standalone ownership; unowned inherited sources, numbers above 255 and unproved expansion/lifetime contexts reject. |
| `T_Fi` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_For` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_ForArithmetic` | Exact envelope | Owned arithmetic headers, empty predicate, loop result and matching continue increment; array/runtime-expression operands and unsupported loop effects reject. |
| `T_ForIn` | Exact envelope | Owned binder storage and finite iteration facts; immediate break/continue. An unobserved underscore binder is a discard; explicit underscore use, array binders and unsupported nonlocal outcomes reject. |
| `T_Function` | Exact envelope | Finite definite definitions, including unconditional child-local definitions; compatible call contexts, direct-body locals and invocation-time redirects. Recursion, conditional definitions, definitions nested in active functions and incompatible contexts reject. |
| `T_GREATAND` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_Glob` | Exact envelope | Active pathname star/question/bracket fragments with quoted literal boundaries, C-byte sorting, default dotfile exclusion and literal no-match fallback. Case/condition pattern admission remains separately bounded; extglob rejects. |
| `T_Greater` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_HereDoc` | Exact envelope | Owned input descriptor with quoted literal content or admitted scalar expansion. Dashed form strips source-leading tabs before expansion. Effectful input expressions and sourceable session ownership reject. |
| `T_HereString` | Exact envelope | One scalar input value followed by a newline, with owned descriptor lifetime. Effectful input expressions and sourceable session ownership reject. |
| `T_If` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_IfExpression` | Exact envelope | Short-circuit control and runtime status/options stay in the executed region; unsupported nonlocal outcomes reject. |
| `T_In` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_Include` | Rejected at parser boundary | ShellCheck include expansion is disabled. Monk owns literal source occurrences, dependency identity and normalization continuations. |
| `T_IndexedElement` | Rejected | Outside the initial implemented storage, control or isolation envelope; no comment/false or silent-success replacement. |
| `T_IoDuplicate` | Exact envelope | Source-ordered duplication or close of owned descriptors. Duplicating an unowned/closed source and incompatible function descriptor contexts reject; native builtin writers own closed/read-only output errors. |
| `T_IoFile` | Exact envelope | Owned read/write/append/read-write opens, legacy null-device form and streaming process endpoints. Ordinary filenames require one proved scalar field and no unmodeled expansion effects; opens and failures occur once under their descriptor owner. |
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
| `T_Pipeline` | Exact envelope | Owned child stages and snapshots; direct draining forms or standalone supervised general pipelines with builtin-writer SIGPIPE ownership. Sourceable stages still require their narrower direct proof. |
| `T_ProcSub` | Exact envelope | Standalone real streaming pipe endpoint as an entire word for admitted byte consumers or owned redirection. Stored/printed/concatenated endpoints, arbitrary escaping consumers and unproved synthetic diagnostics reject. |
| `T_Rbrace` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_Redirecting` | Exact envelope | Owned structural wrapper; redirects and multistage pipelines require their own admitted materialization, never transparent deletion. |
| `T_Rparen` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_Script` | Exact envelope | Owned structural wrapper; redirects and multistage pipelines require their own admitted materialization, never transparent deletion. |
| `T_Select` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_SelectIn` | Rejected | Outside the initial implemented storage, control or isolation envelope; no comment/false or silent-success replacement. |
| `T_Semi` | Rejected as semantic node | Lexical punctuation/keyword is consumed only by its owning grammar constructor; it cannot independently become an executable statement, word or condition. |
| `T_SimpleCommand` | Exact envelope | Resolve definite command identity and validate each builtin operand policy. Read/wait/trap and finite eval have dedicated typed plans; ordinary command-prefix scopes are explicit. Unknown dispatch, implicit ENOEXEC source interpretation and arbitrary source evaluation reject. |
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
| Command argument | Zero/one/many fields, quotation and forced empty arguments. Composed fragments preserve splitting versus pathname activity. One quoted argv/array splice may attach scalar prefix/suffix; multiple splices reject. Process endpoint words have restricted consumers. |
| Scalar assignment | Sequential ordinary assignments and declaration-wide operand expansion before installation; runtime presence/export/fallback ownership. Dense array storage and temporary command binding lifetime are separate typed operations. |
| Redirect target | One proved ordinary filename field or a whole process endpoint; ordered owner opens/duplication and failure scope. Effectful ordinary paths and unproved redirected assignment expansions reject. |
| Case subject/pattern | Scalar subject, separately owned lazy pattern evaluation and all three terminators. |
| Arithmetic command/expansion | Same operator tree but different status, error and continuation rules; retain source spelling for observable errors. |
| Condition/and-or/negation | Actual short-circuit region and errexit suppression boundary, including transitive calls. |
| Function definition/invocation | Definition is an effect; body and redirects execute at invocation under a compatible finite binding context. |
| Source body | Owned return/argv/status and flow joins; acyclic immutable input per source occurrence. Child sources and absolute immutable function sources are admitted with compatible call-time facts; source-local declarations, inherited argv writes and escaping loop control reject. |
| Substitution/pipeline/job | Shared versus child storage, stream bytes, actual PID/status, callback inheritance and asynchronous lifetime. The native owner supervises general pipelines, jobs and restricted streaming endpoints. |
| Sourceable entry | Explicit scalar/import/directory obligations and runtime shape/lookup checks. New standalone session operations, arrays, traps and temporary command scopes are not inferred from that caller contract. |

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
sparse arrays, arbitrary eval or any unrelated excluded form.

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


## Standalone builtin context policies (2026-09-22)

`read` admits literal combined/separate -r, -d, -n, -u and -a options, REPLY,
scalar destinations or an owned dense array. Delimiter/count/descriptor
operands must meet their literal bounded forms. Timeout, prompt, silent and
exact-count flags reject. An IFS-only temporary prefix is admitted unless read
also targets IFS. A nonstandard input descriptor must already be owned by the
current redirected region.

`wait` consumes no operands or proved decimal PID words, with runtime validation
and precise nonchild/invalid-operand behavior. `$!` is one scalar, initially
empty, and tracks actual owned asynchronous launches. Job specifications and
unproved dynamic options reject.

`trap` admits literal EXIT/0 or ERR registration/reset. The handler is parsed
and normalized at translation time with scalar constants and numeric/array
length facts cleared. Pure scalar/builtin/control effects, replacement/reset
and explicit exit are admitted. External/function dispatch, nonlocal handler
control transfers and unproved diagnostic/lifetime operations reject.

`eval` admits literal or flow-proved pure scalar source text, concatenated with
Bash's argument separator and parsed with ShellCheck before materialization.
Effects enter the surrounding finite flow in source order. Arbitrary text,
recursive/unbounded evaluation and nested diagnostic forms without an exact
source mapping reject; generated output never invokes Bash eval.

These forms retain the stricter sourceable boundary. See the
[semantic audit](translator-audit.md) for storage and inspection exclusions;
constructor presence alone does not authorize a context.


Directory operations combined with EXIT/ERR registration reject through
`directory-trap-signal`: their shared stdio error state across signal handling
has no complete materialization yet. This is a composition exclusion, not a
claim that either separately admitted operation is absent.
