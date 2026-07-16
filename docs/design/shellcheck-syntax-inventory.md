# ShellCheck Syntax Inventory

Last refreshed: 2026-07-16

ShellCheck's AST is Monk's Bash input boundary. This inventory records the
deliberate outcome for syntax nodes that previously had unclear or accidental
coverage. “Supported” means the construct has a stable typed lowering and
focused evidence; “out of scope” means Bash mode intentionally rejects or
fails closed rather than attempting another language's syntax.

| ShellCheck form | Bash surface | Monk policy | Evidence |
| --- | --- | --- | --- |
| `T_BraceExpansion` | `{alpha,beta}`, `pre{1,2}post` | Supported. Expand brace products left-to-right into structural glob expressions so multiple braces retain Bash ordering; variables inside alternatives remain typed expressions. | `syntax-brace-expansion` Bash/Fish integration fixture. |
| `T_DollarSingleQuoted` | `$'line\nvalue'` | Supported. Preserve ShellCheck's decoded string as a typed literal and let the Fish renderer choose safe quoting. | `syntax-dollar-single-quote` Bash/Fish integration fixture. |
| `T_DollarBracket` | `$[1 + 2 * 3]` | Supported for compatibility with deprecated Bash syntax. Normalize through the same typed arithmetic plan as `$((...))`; Monk never re-emits the deprecated form. | `syntax-dollar-bracket-arithmetic` Bash/Fish integration fixture. |
| `T_Annotation` source/include wrappers | ShellCheck wrappers around sourced or annotated input | Defensive support. ShellCheck source expansion is disabled with `psCheckSourced = False` because Monk's typed `SourceGraph` owns discovery. Any annotation wrapper that still reaches statement, condition, or status translation is unwrapped without exposing the wrapper in diagnostics. | Parser configuration, recursive source-graph tests, status-context wrapper handling, and shared-runtime bundle execution coverage. |
| `T_Banged` | standalone `! command` and negated pipelines | Supported. Lower through the structural Fish `not` command in both statement and status contexts. | `syntax-standalone-negation` integration fixture plus 0.4 API and pipefail tests. |
| `T_BatsTest` and other Bats-only nodes | `@test ...` | Out of Bash-mode scope. The parser is pinned to Bash, not Bats. If a Bats-only node is supplied through another parser configuration, normal translation uses the generic unsupported fail-closed path and strict translation rejects it. | Constructed-AST pipeline regression plus stable `Bats test` construct naming in unsupported diagnostics. |

## Maintenance Rule

When ShellCheck adds a node or an existing fallback becomes semantic, add it to
this inventory together with a focused unit or Bash/Fish differential fixture.
User-facing diagnostics must use stable Monk construct names and codes; they
must not include `Show` output for ShellCheck tokens.
