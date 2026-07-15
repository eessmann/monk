# README Architecture Expansion Design

## Goal

Update `README.md` so a new user can understand both what Monk does and how a Bash script moves through the system, without turning the project landing page into a complete implementation reference.

## Audience

The primary audience is prospective users and new contributors. Readers should be able to run Monk quickly, understand why its output requires review, and identify the relevant subsystem when they want to explore the code.

## Approach

Expand the existing README in place. Preserve its introduction, capability summary, limitations, quick start, CLI flags, public library surface, development commands, and documentation links.

Add concise material covering:

- the end-to-end pipeline from Bash source through ShellCheck, translation, the typed fish DSL, raw backend lowering, and pretty-printing;
- translator state, source-positioned diagnostics, strict mode, and generated runtime helpers;
- recursive literal `source` discovery and inline versus separate output;
- a repository map connecting directories to their responsibilities; and
- the layered testing strategy, especially behavioral Bash-versus-fish integration tests.

Detailed fidelity claims remain in `docs/design/translator-audit.md`, and detailed subsystem boundaries remain in `docs/design/architecture.md`. The README will link to those documents rather than duplicate them.

## Structure

The revised README will follow this reader journey:

1. Project purpose and conservative translation philosophy.
2. Supported behavior and known review areas.
3. Quick start and CLI controls.
4. High-level execution pipeline.
5. Key semantic safeguards: typed construction, diagnostics, strict mode, helpers, and recursive sources.
6. Public library API.
7. Repository layout and contributor workflow.
8. Testing strategy, bake-off tooling, and detailed documentation links.

## Verification

After editing:

- inspect Markdown headings, lists, and fenced examples;
- verify every relative documentation link resolves;
- compare architectural statements with the current modules and Cabal targets;
- run a whitespace/error check on the diff; and
- review the final diff for accidental removal or duplication of existing guidance.

No source behavior, public API, build configuration, or test fixture will change.
