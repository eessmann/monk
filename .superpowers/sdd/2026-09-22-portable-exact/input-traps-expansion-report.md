# Input, deferred callbacks, and composed words

## Implemented frontend envelope

Standalone read admits literal -r/-d/-n/-u/-a, combined flag spellings, REPLY/scalar destinations and owned dense array destinations. Read-array results carry runtime-dense shape -2; element reads, counts, quoted splices, append and index-zero writes preserve that shape. Timeout, prompt, silent and exact-count modes remain excluded. Runtime owns byte acquisition, IFS assignment, EOF and bad-FD diagnostics.

Ordered file opens support read/write/append/read-write, nonstandard descriptors bounded to 0..255, here strings with one appended newline, and quoted/unquoted heredoc literal/parameter expansion. Dashed heredocs strip source-leading tabs before expansion. Effectful paths/input expansions and effectful assignment/declaration operands under fallible opens remain rejected until expansion order is represented more broadly. File-open failure joins pre-body and post-body facts; function definitions under a fallible redirect are not treated as definite. Sourceable new session operations remain rejected; existing standard descriptor forms stay available.

Literal EXIT/ERR handlers are parsed with ShellCheck during translation and normalized with registration-time constants, numeric proofs and array lengths invalidated. Pure builtin/scalar/control bodies, replacement/reset, and explicit exit are admitted. External/function lookup, return/break/continue, source/descriptor operations and operations requiring unmapped synthetic diagnostics remain outside this initial callback envelope. Possible ERR writes invalidate later constant/numeric/array-length facts, including registrations inside finite functions. Root owns callback execution, status preservation, inheritance/reset and scope behavior.

Composed word fallback emits typed quoted/literal/split expansion fragments. Existing one-field, safe split and simple literal glob fast paths remain. Empty quotes next to a disappearing unquoted expansion explicitly retain one field. Multiple positional/array splices in one word remain rejected. Runtime owns splitting plus bracket/star/question pathname expansion.

Temporary command assignments use PrefixedInvoke for literal external or ordinary pure builtin targets. Command words normalize first, then assignment RHS left-to-right. Temporary-name facts restore afterward while other RHS effects persist. RHS writes intersecting temporary destination names, arrays, and function/source/eval/declaration invocation contexts remain rejected. IFS-only prefixes on read are also admitted, except read destinations named IFS. Root owns lexical temporary exported bindings and expansion timing around redirections.

## Tests and verification

Added Unit.PlannedInput (including composed words and temporary bindings) and Unit.PlannedTraps. Existing CLI baseline demonstrated read/heredoc/file/prefix/mixed-split/trap admission failures before the rebuilt implementation. Canonical build-all passes with GHC 9.14.1 Wall/Werror.

Focused canonical Bash 5.3.9/Fish 4.6: **98/98 pass** across arrays, input/files, composed words, temporary bindings and callbacks. This also closes the overflowing-array-index negative and multi-array/argv snapshot gaps from the array report. Logs: `/tmp/monk-canonical-input-build.log`, `/tmp/monk-canonical-input-focused.log`. Four later boundary cases (failing file-open expansion timing, read-write descriptor offset, and expansion-introduced heredoc tabs) await the next rebuild.

Broad current-binary run is still in the new recursive admitted-composition property. Twelve stale exclusion expectations identified earlier in that run are now updated: read/temp-env/here-string generated properties become mandatory parity tests; unit negatives now exercise remaining explicit restrictions. No actual byte/status mismatch was observed in the 98-case focused run. Runtime C process-spawn stabilization is owned by the runtime agent and requires final rebuilt verification.

No commits. Root owns final semantic audit/todo and all materialization; runtime agent owns descriptor/read/session operations and build agent owns the composed byte expansion implementation.

## Process endpoints and child definitions

Entire process-substitution words may reach cat/diff/cmp/tee/wc or an owned file-descriptor redirect. Scalar storage, concatenation, printable builtin operands and arbitrary executable consumers remain rejected because they could expose the private endpoint pathname. Inputs and outputs remain streaming; runtime owns asynchronous child lifetimes and descriptor handoff. Process substitutions remain excluded from synthetic eval/callback diagnostic contexts. Six process-output fixtures now list Darwin as well as Linux; this enables local Darwin execution and does not claim Linux execution evidence.

Unconditional top-level definitions in an isolated child are admitted. The child closure combines inherited entry definitions with newly introduced child definitions; ordered DefineFunction nodes still replace definitions at their source location. Explicit tests cover calls before/after child redefinition and preservation of the parent definition. Conditional definitions and definitions nested in an active function remain excluded. Unobserved underscore loop binders become explicit discard markers; any explicit underscore read/write is still rejected.

## Integration findings after runtime process-launch repair

Canonical build-all `/tmp/monk-canonical-process-build.log` passed. Focused run `/tmp/monk-canonical-process-focused.log`: 142/144 passed, with two new oracle findings sent to root materialization: prefix arithmetic executes before a failing file open, and EXIT retains an active redirected compound's descriptors. The prior anticipated expected values were corrected to the actual canonical Bash behavior. Root subsequently fixed both materialization paths; final rebuilt verification pending.

Broad run `/tmp/monk-canonical-process-broad.log`: 896/903 passed in 54 seconds. All 100 recursive generated composition cases passed, including formerly crashing nested input/background construction. No generator restriction was added. Remaining failures were the two findings above plus stale golden/admission expectations, now updated. Eleven newly admitted input/source/trap fixtures passed byte/status comparisons. Explicit minimized nested crash regression also passed. Pipeline golden shrank from approximately 104 KB to 31 KB after root removed duplicate unused child-launch definitions; this remains structural rendering.

Additional pending-final-run regressions cover prefix RHS/path expansion order, process-output temporary-IFS loops, discarded loop binders, read-array append/count/index-zero tail retention, and rejection of a later index write whose runtime shape remains unknown.

## Final feature verification

`/tmp/monk-canonical-final-build.log`: canonical GHC 9.14.1 build-all passed after the prefix/EXIT/discard fixes. `/tmp/monk-canonical-final-broad.log`: **911/912 passed** in 52.65 seconds. The sole failure was a metadata-loader expectation still asserting Linux-only process fixtures; it is corrected to `[linux, darwin]` and awaits its small rebuild. All semantic tests passed, including seven process-substitution fixtures executed on Darwin, all 100 recursive composition cases, both prefix filename ordering witnesses, redirected EXIT/ERR behavior, runtime-dense array shape regressions, and process-output compound temporary-IFS loops. This run used canonical custom reference Bash 5.3.9 and Fish 4.6 from `/tmp/monk-canonical-environment`.

HLint on owned Normalize/Parser/Pretty/ArithmeticPlan/Pattern modules and Input/Traps/Isolation tests: no hints. Git diff whitespace check passed. Linux execution remains unavailable locally, not implied by expanded platform metadata.


## Parameter trim and documentation handoff

Task7 adds `ParameterPatternTransform` normalization for #/##/%/%%. ShellCheck
raw pattern fragments retain active backslash syntax; quoted fragments become
literal pattern pieces independently of outer expansion quotes. Active dynamic
fragments require current literal/numeric proof excluding parentheses, a
conservative no-extglob bound. Arbitrary quoted variable values are safe literal
fragments. Brackets and C byte classes use the native pattern-parts capability.
Literal replacement keeps its previous narrower operation.

The old CLI rejected the quoted-star regression before implementation.
Canonical build-all `/tmp/monk-canonical-trim-build.log` passed; focused
`/tmp/monk-canonical-trim-focused.log` passed **36/36**, including 17 new trim
regressions and the corrected portable platform metadata expectation. Thus the
prior sole metadata failure is closed; the controller/build agent owns the
final whole-tree run after subsequent directory changes. Normalize and trim
tests remain HLint-clean.

Documentation updated and handed back to root: architecture, semantic audit,
execution profile, native runtime, syntax constructor/context inventory and
migration guide. Relative links resolve and git diff whitespace checks pass.
Documents distinguish supported native target declarations from local execution
evidence, reference the concrete unsupported SHLVL observation, preserve dated
historical counts, and leave final comparison numbers to the controller.


## Review-driven runtime closure and reference change

The user subsequently required all dependencies/test programs to come from Nix
packages. Prior references above to the custom Bash build are historical local
evidence, not the final canonical runtime declaration. The pinned Nixpkgs
Bash 5.3p9 package is now authoritative; its iconv configuration differs from the
previous custom build for high Unicode escape handling. Runtime owner and
controller are rerunning affected expectations and the final matrix.

Independent runtime review found three admitted counterexamples: read on a
write-only/directory descriptor returned 125 silently instead of status 1 and an
IO diagnostic; failed external exec lost errno-specific status/diagnostics;
removing an otherwise valid cwd caused the session's pathname lookup to fail.
Raw probes distinguished direct versus supervised exec failures. Runtime owner
owns read/error formatting and Session integration; this agent added the
search-only cwd descriptor/openat/fchdir transport and replace-self exec APIs,
plus C source closure restoration immediately before exec. C syntax and
GHC 9.14 typechecks pass warning-clean; no Cabal build was run concurrently.

Unit.PlannedInput now contains durable read-error, direct/supervised failed exec,
removed-cwd, external-function capability and pipeline-PID/stream regressions.
They await the integrated runtime build. The ENOEXEC interpreterless-text
boundary is explicit and does not waive missing/permission failures. Directory
operations combined with EXIT/ERR callbacks retain a precise rejection. Docs
were updated to match these implementation and admission boundaries.


Final signal review also identified asynchronous INT/QUIT inheritance. Spawn now
exports an explicit asynchronous mode; a constructor records initial ignored
INT/QUIT before RTS startup. The ordinary foreground path keeps POSIX spawn.
Asynchronous/inherited-ignore launches use parent-prepared C-only fork/exec,
blocked signals during setup, and a CLOEXEC errno acknowledgement pipe. No
Haskell or allocating child action runs after fork. Replace-self exec preserves
initial ignore and restores RTS descriptors, dispositions and mask on failure.
Direct C probes pass foreground defaults, asynchronous ignore, inherited ignore
and failed-exec restoration; GHC 9.14 warning-clean typecheck passes. Integrated
runtime and Haskell differential checks remain pending at this checkpoint.


The first complete pinned-Nixpkgs run built successfully and passed 944/950 tests
(`/tmp/monk-nixpkgs-main-build2.log`, `/tmp/monk-nixpkgs-broad.log`). All newly
added read-IO, removed-cwd, direct/supervised exec-failure, external-function and
pipeline PID/stream regressions passed. Remaining failures were two structural
statistics expectations, two target-dependent ANSI expectations, one directory
closed-stderr diagnostic, and one stale pipeline golden. Corrections remain
pending a fresh integrated run; no full-suite green is claimed here.

The ANSI fix retains Darwin/Linux byte variants in a typed PlatformBytes scalar
and selects at runtime, avoiding host-dependent output. New tests cover retained
empty fields and NUL truncation. Structural newline rendering removes empty
quote fragments around escaped newlines while preserving an actual empty word;
a focused renderer test accompanies this final readability adjustment.


## Final corrected pinned-Nixpkgs verification

`/tmp/monk-nixpkgs-main-build3.log`: full GHC 9.14.1 build-all passed, including
PlatformBytes, original diagnostic-stderr inheritance, structural statistics,
and renderer corrections. `/tmp/monk-nixpkgs-broad2.log`: **all 955 tests passed
in 61.60 seconds**, including 100 recursively generated admitted compositions,
Bash/Fish semantic integration, read IO errors, removed cwd identity, mutable
executable failure, external function inheritance, actual background pipeline
PID identity, platform-dependent ANSI bytes, empty fields, NUL truncation and
all complete-file goldens. The actual environment was captured from fresh
devenv at `/tmp/monk-nixpkgs-environment`: Bash
`/nix/store/s0psayl7zvkvwdcqc8fy1sbv8rlf1yq8-bash-5.3p9/bin/bash` (5.3.9), Fish
`/nix/store/2rb8r6s2ic5wryq75aa0k3vhav51mxj6-fish-4.6.0/bin/fish` (4.6.0),
TMPDIR `/private/tmp`. No manually built third-party test program was used.

All six admitted full-file goldens were regenerated through the exact rebuilt
public API with relative fixture source origins and manually reviewed. They
remove redundant empty quote chunks around literal newlines; pipeline also
records the source-located external-site stage. Scalar/array boundary guards,
empty arguments and command control structure remain intact. Excluded fixtures
retain their precise rejection expectations. Owned Normalize/Pretty/Spawn/FD
bridge and test modules passed HLint without hints; C bridges passed
`clang -Wall -Wextra -Werror -fsyntax-only`; git whitespace checks passed.

Source, tests and docs were frozen before the build agent's final receipt.
Final whole-tree provenance, packaging, compiler compatibility and moving-Fish
gates belong to that independent run. Local Darwin success does not establish
Linux runtime execution; declared portable target support remains distinguished
from platform evidence.
