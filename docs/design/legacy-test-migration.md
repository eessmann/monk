# Legacy translation test migration

The former `Unit.Translation` and `Unit.TranslatorMonad` suites mixed renderer snapshots, best-effort fallbacks, and semantic claims. The replacement treats each source as either exact or rejected before lowering. Exact claims compare Bash and generated Fish exit status, stdout, and stderr. Rejections require a stable `monk.semantic.*` code, `PhaseTranslate`, `DiagnosticError`, `Unsafe`, and a source range; receiving `Right` is a failure because it would expose executable output.

The compact unit tables keep representative source counterexamples executable. The reviewed 90-row `test/fixtures/admission.tsv` retains the broader corpus policy. Arithmetic, child isolation, sourceable execution, and primitive byte behavior remain in their dedicated differential groups. Renderer wrapper and helper-registration assertions were removed because those private shapes are not semantic contracts.

Several former strict-mode subshell tests changed classification: the owned child plan now admits supported subshells exactly. Default mode no longer converts unsupported constructs into warnings or runnable approximations.

The tables retain the 156 former case names. A family-level replacement test is
not evidence that every preserved legacy snippet is admitted. The decisions
below distinguish the actual snippet policy from the narrower positive family;
`admission.tsv` and the executable focused groups remain acceptance evidence.

## `Unit.Translation` mapping

| Former test | Preserved source | Decision | Replacement evidence |
| --- | --- | --- | --- |
| Process substitution output redirect lowers to status-preserving temp-file block | <code>printf hi &gt; &gt;(wc -c &gt; out)</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Multiple process substitution output redirects lower without command-substitution helper | <code>printf hi &gt; &gt;(cat &gt; one)\nprintf bye &gt; &gt;(cat &gt; two)</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Command substitution preserves command redirections | <code>echo $(printf hi &gt; /tmp/monk-count)</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Process substitution output preserves consumer redirections | <code>printf hi &gt; &gt;(wc -c &gt; /tmp/monk-count)</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Echo -e lowers to printf %b | <code>echo -e \&quot;hi\\nthere\&quot;</code> | exact | `Unit.PlannedPrimitives` plus `admission.tsv` echo fixtures |
| Echo -n stays echo with -n | <code>echo -n hi</code> | exact | `Unit.PlannedPrimitives` plus `admission.tsv` echo fixtures |
| Echo -E stays echo without escapes | <code>echo -E \&quot;hi\\nthere\&quot;</code> | exact | `Unit.PlannedPrimitives` plus `admission.tsv` echo fixtures |
| Echo without options stays echo | <code>echo hello</code> | exact | `Unit.PlannedPrimitives` plus `admission.tsv` echo fixtures |
| Echo -- preserves literal arguments | <code>echo -- -n</code> | exact | `Unit.PlannedPrimitives` plus `admission.tsv` echo fixtures |
| Process substitution input uses psub | <code>cat &lt;(echo 123)</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Unsupported extglob uses bash shim | <code>echo !(foo&#124;bar)</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Shift uses argv slice | <code>shift</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Declare export maps to set --global --export | <code>declare -x FOO=bar</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Default expansion uses set -q for unset | <code>echo ${JAVA_HOME-}</code> | reject | This unquoted default lacks pathname-expansion proof (`monk.semantic.pathname-expansion`). Quoted default-value positives are covered separately by `Unit.Translation`. |
| Alternate expansion uses test -n | <code>echo ${NIX_PATH:+:$NIX_PATH}</code> | reject | The preserved alternate modifier/operand is outside the parameter envelope (`monk.semantic.parameter`); default-value tests do not establish alternate-modifier support. |
| Assigning expansion hoists side effects | <code>echo ${HOME:=/tmp}</code> | reject | This unquoted default lacks pathname-expansion proof (`monk.semantic.pathname-expansion`). Quoted assignment-default positives have separate coverage. |
| Error expansion hoists exit | <code>echo ${MISSING:?nope}</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Redirection expansion hoists side effects | <code>echo hi &gt; ${OUT:=/tmp/out}</code> | reject | Shared-scope file opens require owned errors and descriptor lifetime; no preflight/reopen approximation. |
| Status redirection expansion hoists side effects | <code>if true &gt; ${OUT:=/tmp/out}; then echo ok; fi</code> | reject | Same file-redirection ownership exclusion; exact descriptor ordering has focused positive coverage. |
| Heredoc expansion hoists side effects | <code>cat &lt;&lt;EOF<br>${VAL:=ok}<br>EOF</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Length expansion for argv uses count | <code>echo ${#@}</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Length expansion for arrays uses count | <code>echo ${#arr[@]}</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Unset functions and variables translate to functions -e / set -e | <code>unset -f foo -v bar</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection |
| Unset variable without flags maps to set -e | <code>unset ASPELL_CONF</code> | exact | `Unit.Translation` unset/default differential |
| Hash in word is preserved | <code>a=nixpkgs\nnix run $a#hello</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Pushd and popd pass through | <code>pushd /tmp</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Nested command substitution translates inner | <code>echo $(echo $(echo hi))</code> | reject | The unquoted substitution needs pathname-expansion proof. `Unit.PlannedIsolation` covers admitted quoted child expressions, not this legacy snippet. |
| Command substitution preserves status conjunctions | <code>echo $(false &#124;&#124; true &amp;&amp; false)</code> | reject | The unquoted substitution needs pathname-expansion proof. Quoted status/control cases are covered in `Unit.PlannedIsolation`. |
| Command substitution status fallback is not silent success | <code>echo \&quot;$(case x in x) false ;; esac &#124;&#124; echo fallback)\&quot;</code> | exact | `Unit.PlannedIsolation` differential group |
| Command substitution status redirection hoists target expansion | <code>echo \&quot;$(true &gt; ${OUT:=/tmp/out} &amp;&amp; printf ok)\&quot;</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection |
| Command substitution redirections share operator parsing | <code>echo \&quot;$(printf hi &amp;&gt; out; printf bye 3&gt;&amp;-; cat &lt;&gt; rw)\&quot;</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Unsupported pipeline status stage is not silent success | <code>false &#124; wc -c</code> | architecture rejection | `Unit.Translation` hand-built AST fail-closed assertion |
| Errexit guard is command-substitution aware | <code>set -e<br>digitCount() { local num=$1 count=0; while ((num != 0)); do ((++count)); ((num = num / 10)); done; echo &quot;$count&quot;; }<br>echo $(digitCount 12)</code> | exact | `Unit.PlannedIsolation` differential group |
| Strict mode fails on unsupported coproc | <code>coproc echo hi</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Strict mode fails on subshell | <code>(echo hi)</code> | exact | `Unit.PlannedIsolation` differential group; legacy rejection expectation retired |
| Strict mode fails on status-context subshell | <code>if (echo hi); then echo ok; fi</code> | exact | `Unit.PlannedIsolation` differential group; legacy rejection expectation retired |
| Strict mode fails on command-substitution subshell | <code>echo $( (echo hi) )</code> | exact | `Unit.PlannedIsolation` differential group; legacy rejection expectation retired |
| Command-substitution subshell keeps its body in non-strict mode | <code>echo $( (echo hi) )</code> | exact | `Unit.PlannedIsolation` differential group; legacy rejection expectation retired |
| Array index assignment is 1-based | <code>arr[0]=foo</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Array index expansion is 1-based | <code>echo ${arr[0]}</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Substring expansion uses string sub | <code>echo ${var:1:2}</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection |
| Pattern removal uses string replace | <code>echo ${var#foo}</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Pattern replacement // uses -a | <code>echo ${var//foo/bar}</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Case modification uses string upper/lower | <code>echo ${var^^}</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Length expansion uses string length | <code>echo ${#var}</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Arithmetic command sets status from math | <code>((1 + 2))</code> | exact | `Unit.PlannedArithmetic` differential group |
| Arithmetic postfix increment hoists temp | <code>echo $((i++))</code> | exact | `Unit.PlannedArithmetic` differential group |
| Arithmetic prefix increment updates variable | <code>echo $((++i))</code> | exact | `Unit.PlannedArithmetic` differential group |
| Arithmetic assignment in expression hoists set | <code>echo $((x = y + 1))</code> | exact | `Unit.PlannedArithmetic` differential group |
| Arithmetic short-circuit lowers to conditional evaluation | <code>echo $((a++ &amp;&amp; b++))</code> | exact | `Unit.PlannedArithmetic` differential group |
| Arithmetic ternary lowers to conditional evaluation | <code>echo $((a ? b++ : c++))</code> | exact | `Unit.PlannedArithmetic` differential group |
| Arithmetic for loop lowers to begin/while and increment | <code>for ((i=0; i&lt;2; i++)); do echo $i; done</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| For loop avoids fish readonly underscore variable | <code>for _ in 1; do true; done</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| For loop rewrites underscore body references with scoped binding | <code>for _ in a; do echo \&quot;$_\&quot;; done</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Until loop negates condition | <code>until true; do echo 1; done</code> | exact | `Unit.Translation` until differential |
| Until loop negates compound condition | <code>until false &amp;&amp; true; do echo ok; done</code> | exact | `Unit.Translation` until differential |
| Time prefix is preserved in pipelines | <code>time sleep 1</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Pipeline to source stays piped | <code>echo 123 &#124; source</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Double bracket pattern match uses string match -q | <code>if [[ $x == foo* ]]; then echo ok; fi</code> | exact envelope | The literal active `foo*` pattern is admitted. `Unit.PlannedWordContexts` covers quote-aware pattern behavior; unknown active pattern values remain excluded. |
| Double bracket regex uses string match -qr | <code>if [[ $x =~ ^foo ]]; then echo ok; fi</code> | reject | `Unit.TranslatorMonad` located semantic rejection or condition envelope |
| Double bracket negation uses not | <code>if [[ ! $x == foo ]]; then echo ok; fi</code> | reject | This double-bracket negation shape rejects with `monk.semantic.condition`. Shell-list negation and admitted equality cases are separate families. |
| Double bracket with &amp;&amp; and &#124;&#124; uses conjunctions | <code>if [[ $x == foo &amp;&amp; $y != bar &#124;&#124; $z == baz ]]; then echo ok; fi</code> | reject | `Unit.TranslatorMonad` located semantic rejection or condition envelope |
| Double bracket parentheses preserve nested conjunctions | <code>if [[ ( $x == foo &#124;&#124; $y == bar ) &amp;&amp; ! $z == baz ]]; then echo ok; fi</code> | reject | `Unit.TranslatorMonad` located semantic rejection or condition envelope |
| Simplifier elides trivial begin wrapper in else branch | <code>if [[ $x == foo ]]; then echo ok; else true; fi</code> | exact | `Unit.PlannedIsolation` or positive-control differential; renderer shape assertion removed |
| Simplifier keeps multi-statement pipeline stages wrapped | <code>echo hi &#124; FOO=bar BAR=baz cat</code> | reject | `Unit.TranslatorMonad` command-prefix/background/redirect admission boundary |
| Simplifier keeps conjunction stages wrapped when preludes remain | <code>FOO=bar BAR=baz true &amp;&amp; echo ok</code> | reject | `Unit.TranslatorMonad` command-prefix/background/redirect admission boundary |
| Simplifier does not elide background wrappers around instrumented jobs | <code>FOO=bar BAR=baz true &amp;</code> | reject | `Unit.TranslatorMonad` command-prefix/background/redirect admission boundary |
| Simplifier preserves redirected brace groups | <code>{ echo hi; } &gt; out</code> | reject | `Unit.TranslatorMonad` command-prefix/background/redirect admission boundary |
| Simplifier flattens nested scope-neutral prelude begins | <code>{ { X=1; }; echo hi; }</code> | exact | `Unit.PlannedIsolation` or positive-control differential; renderer shape assertion removed |
| Simplifier flattens nested trivial begin wrappers | <code>{ { true; }; }</code> | exact | `Unit.PlannedIsolation` or positive-control differential; renderer shape assertion removed |
| Simplifier elides pipeline-local wrapper only for single safe stage | <code>echo hi &#124; { { cat; }; }</code> | exact | `Unit.PlannedIsolation` or positive-control differential; renderer shape assertion removed |
| Simplifier preserves scope-changing prelude begin wrappers | <code>{ { FOO=bar true; }; echo hi; }</code> | reject | `Unit.TranslatorMonad` command-prefix/background/redirect admission boundary |
| Env prefix uses local export block | <code>FOO=bar echo hi</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Export command uses set --global --export | <code>export FOO=bar</code> | exact | `Unit.Translation` child-environment differential |
| Local command uses set --local | <code>local FOO=bar</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Select loop uses read prompt and items list | <code>select x in a b; do echo $x; break; done</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Case patterns preserve globs | <code>case $x in foo* ) echo ok ;; esac</code> | exact | `Unit.Translation` case differential and `admission.tsv` case fixtures |
| Case patterns with expansion keep glob meta | <code>case $x in ${Y}* ) echo ok ;; esac</code> | reject | Unknown active `Y` rejects with `monk.semantic.pattern-envelope`. Literal or finitely proven active patterns have separate positive coverage. |
| Case pattern expansion hoists side effects | <code>case $x in ${Y:=1}) echo ok ;; esac</code> | reject | The unknown existing value of `Y` leaves an unproved active pattern (`monk.semantic.pattern-envelope`). Proven unset/numeric/literal default-pattern cases are covered separately. |
| Case switch expansion hoists side effects | <code>case ${X:=1} in 1) echo ok ;; esac</code> | exact | `Unit.Translation` case differential and `admission.tsv` case fixtures |
| Read flags translate to fish equivalents | <code>read -d : first second</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Read helpers are registered once | <code>read -d : a b\nread -d : c d</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Background jobs use Monk tracking runtime | <code>false &amp;\nbg=$!\nwait \&quot;$bg\&quot;</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Background runtime is registered once | <code>false &amp;\nwait \&quot;$!\&quot;\ntrue &amp;\nwait \&quot;$!\&quot;</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Pipefail helper is registered once | <code>set -o pipefail\nfalse &#124; true\ntrue &#124; false</code> | exact | `Unit.PlannedIsolation` and `admission.tsv` pipefail differentials |
| Exact read delimiter array helper emits no semantic warnings | <code>read -d &#x27;&#x27; -ra fields</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Multi-variable delimiter reads use exact helper without warnings | <code>read -d : one two three</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Mixed delimiter flag clusters use exact helper | <code>read -rsd: -n 3 field</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| No-var null delimiter assigns REPLY exactly | <code>read -d &#x27;&#x27;</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Delimiter values normalize to the first character | <code>read -d &#x27;::&#x27; field</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Source passes args | <code>source /tmp/script.sh a b</code> | exact | `Unit.PlannedSourceable` differential group |
| Trap translates to fish trap syntax | <code>trap &#x27;echo bye&#x27; EXIT</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Trap uses distinct per-signal body variables | <code>trap &#x27;echo first&#x27; EXIT INT\ntrap &#x27;echo second&#x27; EXIT</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Trap clear removes Monk-generated handlers | <code>trap - EXIT INT</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Trap normalizes SIG-prefixed signals | <code>trap &#x27;echo hi&#x27; SIGINT</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |

## `Unit.TranslatorMonad` mapping

| Former test | Preserved source | Decision | Replacement evidence |
| --- | --- | --- | --- |
| Unsupported warning is ranged and not duplicated | <code>coproc echo hi</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Strict mode raises error for unsupported | <code>coproc echo hi</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection |
| Warnings accumulate in order | <code>coproc echo hi\ncoproc echo bye</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Background tracking warning is emitted once across repeated wait translation | <code>sleep 1 &amp;\nwait\nwait</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Function scope is restored after translating a function body | <code>f() { local inside=1; }\nlocal outside=1</code> | reject | `Unit.TranslatorMonad` local-context rejection after the function body |
| Warnings keep their own source ranges across statements | <code>coproc echo hi\ntrap</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Nested translation restores outer ranges for later warnings | <code>f() { coproc echo hi; }\ntrap</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Arithmetic short-circuit no longer warns on side effects | <code>echo $((a++ &amp;&amp; b++))</code> | exact | `Unit.PlannedArithmetic` differential group |
| Arithmetic short-circuit (&#124;&#124;) no longer warns on side effects | <code>echo $((a++ &#124;&#124; b++))</code> | exact | `Unit.PlannedArithmetic` differential group |
| Arithmetic ternary no longer warns on side effects | <code>echo $((a ? b++ : c++))</code> | exact | `Unit.PlannedArithmetic` differential group |
| Read -r is treated as no-op | <code>read -r name</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Read array uses exact newline helper without IFS warning | <code>read -a arr</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Set -euo pipefail enables errexit/pipefail and warns about nounset | <code>set -euo pipefail</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| set -- clears argv without warning | <code>set --</code> | exact | `Unit.Translation` argv differential |
| set -- assigns argv exactly | <code>set -- a b</code> | exact | `Unit.Translation` argv differential |
| set options before -- still assign argv | <code>set -e -- a b</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| ambiguous set arguments warn and stay raw | <code>set a b</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| shopt is warning-only and lowered to true | <code>shopt -s nullglob</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| redirected shopt is warning-only and lowered to redirected true | <code>shopt -s nullglob &gt; out</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| non-literal source is warning-driven and preserved | <code>source \&quot;$child\&quot;</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| missing source argument is warning-driven and preserved | <code>.</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| strict mode fails on source issues | <code>source \&quot;$child\&quot;</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| argument-position output process substitution warns for manual review | <code>echo &gt;(cat)</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| strict mode fails on argument-position output process substitution | <code>echo &gt;(cat)</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| unsupported output process substitution consumer warns instead of silently dropping body | <code>printf hi &gt; &gt;(case x in x) cat ;; esac)</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| strict mode fails on unsupported output process substitution consumer | <code>printf hi &gt; &gt;(case x in x) cat ;; esac)</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| stderr output process substitution stays on warning-driven generic path | <code>printf hi 2&gt; &gt;(wc -c &gt; err.count)</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| case expression preserves status in a conjunction | <code>case x in x) false ;; esac &amp;&amp; echo bad</code> | exact | `Unit.PlannedIsolation` differential group |
| case expression preserves status in an if condition | <code>if case x in x) false ;; esac; then echo bad; else echo ok; fi</code> | exact | `Unit.PlannedIsolation` differential group |
| banged pipefail pipeline remains supported in status context | <code>set -o pipefail\nif ! false &#124; true; then echo ok; else echo bad; fi</code> | exact | `Unit.PlannedIsolation` differential group |
| strict mode accepts supported compound status expressions | <code>case x in x) false ;; esac &amp;&amp; echo bad</code> | exact | `Unit.PlannedIsolation` differential group |
| read -d lowers to exact helper without semantic warning | <code>read -d : first second</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| read without variables assigns REPLY exactly | <code>read</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| read delimiter without variables assigns REPLY exactly | <code>read -d :</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| multi-variable newline read uses exact helper without IFS warning | <code>read first second</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| read -d &#x27;&#x27; array path lowers without delimiter or IFS warnings | <code>read -d &#x27;&#x27; -ra items</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| numeric fd read lowers to exact helper without semantic warnings | <code>read -u 3 -r tail</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| read -s lowers without warning | <code>read -s secret</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| clustered delimiter read flags use exact helper | <code>read -rsd: -n 3 secret</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| clustered read flags keep nchars and array options | <code>read -n3 -a items</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| read -d &#x27;&#x27; with no vars assigns REPLY exactly | <code>read -d &#x27;&#x27;</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| dynamic set -o warns for manual review | <code>set -o $mode</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Warnings carry stable codes and severities | <code>readonly FOO=bar\ntrap</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| Command-substitution subshell warns with a stable code | <code>echo $( (echo hi) )</code> | exact | `Unit.PlannedIsolation` differential group; legacy rejection expectation retired |
| trap with no arguments warns | <code>trap</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| trap options warn and stay raw | <code>trap &#x27;echo hi&#x27; -p</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| trap pseudo-signals warn and avoid invalid fish handlers | <code>trap &#x27;echo hi&#x27; ERR</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| numeric trap signal stays numeric to avoid platform mapping | <code>trap &#x27;echo int&#x27; 2</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| uncatchable trap signals warn instead of registering handlers | <code>trap &#x27;echo nope&#x27; KILL STOP</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| non-numeric read fd stays warning-driven | <code>read -u fd value</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| unsupported clustered read flags stay raw and warning-driven | <code>read -rz value</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| shift negative count warns with comment | <code>shift -1</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| shift with multiple arguments warns with comment | <code>shift 1 2</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| readonly warns about missing enforcement | <code>readonly FOO=bar</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| declare invalid argument warns | <code>declare 1x</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| local outside function warns | <code>local FOO=bar</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection |
| local invalid argument warns | <code>local 1x</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| export invalid argument warns | <code>export 1x</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| unset invalid flag warns but keeps valid arguments | <code>unset -z foo</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| for arithmetic unsupported init warns | <code>for ((i+=1; i&lt;2; i++)); do echo $i; done</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |
| for arithmetic unsupported increment warns | <code>for ((i=0; i&lt;2; i*=2)); do echo $i; done</code> | reject | `Unit.TranslatorMonad` stable located semantic rejection or `admission.tsv` reject fixture |

## Evidence boundaries

- `test/Unit/Translation.hs` keeps eight representative exact differentials and the synthetic raw-AST fail-closed guard.
- `test/Unit/TranslatorMonad.hs` keeps the diagnostic boundary: policy-mode independence, first-failure ordering, stable category codes, located failures, and absence of legacy warning state on exact output.
- `test/fixtures/admission.tsv` is the reviewed corpus authority for the broader 39 exact and 51 rejected fixtures.
- `Unit.PlannedArithmetic`, `Unit.PlannedIsolation`, `Unit.PlannedPrimitives`, and `Unit.PlannedSourceable` own their respective semantic mechanisms. This migration does not duplicate their large runtime matrices.

## Other suite migrations

| Former expectation | Replacement and reason |
| --- | --- |
| `Property.Translation` array index/assignment renderer offsets | Generated, located admission failures until array storage is implemented. |
| Substring and case-modifier renderer spellings | Generated explicit parameter-operator exclusions. |
| `[[ ... ]]` helper name and match/not command counts | Differential quoted/active-pattern cases; explicit compound-condition and regex exclusions. No discarded unsupported samples. |
| Arithmetic emits Fish `math` | Generated signed integer command/status differential; operator-tree arithmetic suite owns precision, wrapping and errors. |
| `Property.OutputEquivalence` mixed unclassified generators | Five exact generated families plus four explicit exclusions (arrays, read, temporary environment, here-string). Admission is tested even without runtime prerequisites. |
| Single-command pipefail renderer wrapper shape | Exact toggle/output/status differential in `Unit.Pipefail`. |
| `Property.Pretty` literal quote spelling | Generated literal DSL values must parse and round-trip through Fish; structural renderer properties remain. |
| API04 legacy helper existence and strictness fallback | Opaque products, named approximation behavior, located requirement producers and capability consumers, parser-only source evidence, and forged source metadata rejection. |
| Refactor legacy translator filename conventions | The old semantic walkers must be absent. Live materializers cannot import raw ShellCheck tokens or renderer internals; public/private boundaries remain checked. |
| Golden and integration fixtures all translated best-effort | Every fixture has a reviewed exact/rejected policy. Exact goldens compare rendering and exact integrations execute both shells; rejected fixtures must produce no executable product. Historical Fish files for exclusions remain examples only. |
| Silent real-world prerequisite skips | Explicit skip reasons. These tests are labeled manual Fish baselines, not compiler acceptance. |
| Medium/large benchmarks always translate | Retain the old inputs as rejection benchmarks; add useful medium/large exact-core workloads with mandatory admission. |

These migrations do not turn arbitrary current failures into acceptable output.
`admission.tsv` records decisions from reviewed source semantics. Newly found
counterexamples remain mandatory regressions, and the exact positive core must
pass before completion.
