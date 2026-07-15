# README Architecture Expansion Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Expand Monk's README so users and new contributors can understand the translation pipeline, safeguards, repository structure, and behavioral verification strategy without losing the current quick-start material.

**Architecture:** This is a documentation-only change to `README.md`. Preserve the existing product description, supported-feature summary, limitations, CLI examples, public API, and development commands; add concise architectural explanations and route detailed claims to the existing design documents.

**Tech Stack:** Markdown, Haskell/Cabal project structure, ShellCheck Bash AST, Monk's typed fish DSL and pretty-printer.

## Global Constraints

- Do not change source behavior, public APIs, build configuration, or fixtures.
- Keep `docs/design/translator-audit.md` as the fidelity source of truth.
- Keep `docs/design/architecture.md` as the detailed subsystem-boundary reference.
- Describe generated output as migration code that still requires human review.
- Preserve every currently documented CLI option and development command.

---

### Task 1: Expand and verify the project README

**Files:**
- Modify: `README.md`
- Reference: `docs/superpowers/specs/2026-07-16-readme-architecture-design.md`
- Reference: `docs/design/architecture.md`
- Reference: `docs/design/translator-audit.md`

**Interfaces:**
- Consumes: the current CLI flags, public module names, Cabal targets, and translator boundaries documented by the referenced files.
- Produces: a single coherent project landing page; no software interface changes.

- [x] **Step 1: Add the execution-pipeline explanation**

After `## What To Expect`, retain the existing expectations and review-area list, then add this high-level section before `## Quick Start`:

````markdown
## How It Works

Monk works like a small compiler:

```text
Bash source
  -> ShellCheck parser and Bash AST
  -> Monk translator
  -> typed fish DSL
  -> raw renderer AST
  -> pretty-printed fish source
```

1. `Language.Bash.Parser` asks ShellCheck to parse Bash and retain source positions and parse diagnostics.
2. `Language.Fish.Translator` recursively translates ShellCheck tokens. Focused modules handle control flow, commands, variables, arithmetic, redirections, parameter expansion, process substitution, and other semantic areas.
3. The translator produces a typed `Language.Fish.DSL.Script`. Its types keep blocks and pipelines non-empty, distinguish expression types, and restrict pipeline stages to status-returning commands.
4. `Language.Fish.DSL.Lower` explicitly lowers the typed script into the raw backend AST consumed by `Language.Fish.Pretty`.
5. The pretty-printer renders the final fish source while the translation result retains structured diagnostics for the caller.

The translator also tracks context such as function scope, local variables, command substitution, `errexit`, and `pipefail`. When fish has no direct equivalent for required Bash behavior, Monk can emit a generated helper preamble for supported cases such as background-job tracking, exact `read` behavior, process substitution, and `pipefail` handling.
````

- [x] **Step 2: Add diagnostics and recursive-source explanations**

Immediately after `## How It Works`, add:

```markdown
## Diagnostics And Strict Mode

Warnings are structured values with a code, severity, optional detail, and source range. The CLI prints them to stderr and summarizes translation confidence; high-risk warnings are called out for review.

Default mode keeps translating when a best-effort result is available. `--strict` instead turns unsupported constructs into translation failures. This makes normal mode useful for migrations and strict mode useful when approximate output is unacceptable.

## Recursive Sources

With `--recursive`, Monk discovers literal `source` and `.` references and builds a graph of the scripts it can resolve. `--sources inline` combines translated files into one output, while `--sources separate` emits individual `.fish` files and rewrites source paths to their translated targets.

Dynamic source expressions cannot be resolved statically and remain warning-driven manual-review cases.
```

- [x] **Step 3: Add the repository map and testing strategy**

Between `## Development` and the existing bake-off command, add this contributor-oriented material, moving the bake-off subsection below it without changing the command or prerequisites:

```markdown
### Repository Map

- `app/`: the `monk` CLI entry point
- `src/Monk/`: public translation, diagnostics, and source-graph APIs
- `src/Language/Bash/`: the ShellCheck parser boundary
- `src/Language/Fish/DSL*`: the typed fish construction API and explicit lowering layer
- `src/Language/Fish/Translator/`: translation orchestration and semantic subsystems
- `src/Language/Fish/Pretty/`: the raw fish AST renderer
- `test/`: unit, property, golden, integration, and real-world tests
- `scripts/Bakeoff/`: the Monk-versus-Babelfish comparison harness
- `docs/design/`: architecture, fidelity evidence, and active translator design notes

### Testing Strategy

The test suite checks both generated structure and runtime behavior:

- unit tests cover focused translator, DSL, renderer, diagnostics, source, and harness behavior
- property tests exercise rendering and translation invariants
- golden tests compare generated fish text with checked-in expected output
- integration and real-world tests run Bash and translated fish, then compare exit status, stdout, stderr, and environment changes
- the bake-off runner compares Monk with Babelfish and can benchmark both translators

Run `cabal test` for the normal suite. Set `MONK_INTEGRATION=1` to enable tests that require Bash and fish execution.
```

- [x] **Step 4: Re-read the complete README and remove duplication**

Keep one copy of each command, flag list, capability list, limitation list, public module list, bake-off prerequisite list, and documentation link. Ensure the final heading order is:

```text
Monk
What It Does
What To Expect
How It Works
Diagnostics And Strict Mode
Recursive Sources
Quick Start
Library Surface
Development
  Repository Map
  Testing Strategy
Docs
```

- [x] **Step 5: Verify all relative Markdown links resolve**

Run:

```bash
ruby -e 'text = File.read("README.md"); links = text.scan(/\[[^\]]+\]\(([^)]+)\)/).flatten.reject { |x| x.match?(%r{\A(?:https?|mailto):}) || x.start_with?("#") }; missing = links.reject { |x| File.exist?(x.split("#", 2).first) }; abort("missing links: #{missing.join(", ")}") unless missing.empty?; puts "README relative links: OK"'
```

Expected:

```text
README relative links: OK
```

- [x] **Step 6: Verify documented modules and directories exist**

Run:

```bash
test -f src/Language/Bash/Parser.hs \
  && test -f src/Language/Fish/Translator.hs \
  && test -f src/Language/Fish/DSL.hs \
  && test -f src/Language/Fish/DSL/Lower.hs \
  && test -f src/Language/Fish/Pretty.hs \
  && test -d scripts/Bakeoff \
  && printf 'README architecture references: OK\n'
```

Expected:

```text
README architecture references: OK
```

- [x] **Step 7: Check Markdown content and the final diff**

Run:

```bash
rg -n '^## |^### |ShellCheck|typed fish DSL|MONK_INTEGRATION|Babelfish' README.md
git diff --check
git diff -- README.md
```

Expected: the new headings and architectural terms appear once where intended, `git diff --check` emits no errors, and the diff preserves all existing user-facing commands and flags.

- [x] **Step 8: Commit the documentation update**

```bash
git add README.md docs/superpowers/plans/2026-07-16-readme-architecture.md
git commit -m "Expand README architecture guide"
```

Expected: one documentation commit containing the README expansion and its implementation plan.
