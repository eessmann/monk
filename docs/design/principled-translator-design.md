# Principled translator redesign and gap closure

Accepted by the author on 2026-09-09. This document is the implementation
contract; the older gap-closure branches are prototypes, not merge candidates.

## Correctness contract

- Rebuild from main in working vertical slices. Preserve the structural Fish DSL
  and private renderer. Reuse prototype counterexamples and useful construction
  invariants, not overlapping walkers or detached receipt bookkeeping.
- Strict translation accepts only behavior justified under an explicit execution
  contract. Normal translation permits documented approximations only through
  named opt-ins. Unsupported inputs produce diagnostics without executable output.
- Support standalone and sourceable output. Sourceable exactness is conditional
  on declared imports/exports, variable attributes, function lookup, options,
  execution context, and ambient effects. Runtime checks and caller obligations
  must be distinguished; checking a declaration does not prove arbitrary code.
- Start with a versioned Bash 5.3 signed-64-bit profile, initially evidenced by
  Bash 5.3.9 and Fish 4.6.0. Record startup options, locale, encoding, and platforms.
  Moving Fish versions are compatibility evidence, not a promise about future
  releases. Other Bash profiles require dedicated evidence.
- Bounded Fish/Python helpers are allowed. No arbitrary Bash interpreter, Python
  eval, runtime expression-string interpretation, or whole-shell emulation.
- Observable equivalence covers stdout/stderr bytes, status/control flow, argv,
  filesystem effects, declared caller-state changes, and observable ordering.

## Public interfaces

Replace Boolean-only configuration with policy, target profile, entry mode and
caller contract, with CLI equivalents and named approximation selection. API/CLI
breaks are authorized with migration guidance. TranslationResult, SourceGraph and
OutputBundle become opaque products with inspection accessors. General Fish DSL
construction remains available without claiming translation certification.

Add PhaseOutput, remove the unused runtime phase, and replace free-text Fish
feature requirements with capabilities having real producers, consumers and tests.

## Authoritative pipeline

ShellCheck syntax -> private semantic plan -> admitted materialization plan ->
structural Fish DSL -> rendering/publication.

Plans preserve source occurrences, quoting/cardinality, evaluation regions,
storage decisions, control outcomes, and shared/child execution boundaries.
Generated helpers, wrappers, redirects, argv setup, and cleanup are planned before
admission. Semantic changes invalidate/rebuild admission. Type indices protect
actual grammar, cardinality, control-scope and ownership constraints; detached
receipts and equality searches are not final-artifact membership proofs.

## Stages

1. Evidence/admission: preserve eleven fresh strict-clean counterexamples; audit
   parser constructors and contexts; classify exact, named approximation, rejected;
   retain a mandatory positive translation suite.
2. Words/evaluation: quoted scalars, empty fields, embedded $@, IFS splitting,
   admitted globbing, lazy effects, all three case terminators.
3. Arithmetic/runtime: operator tree, integer intermediate operations, overflow,
   short circuit and contextual errors; separate lexical context, static flow facts
   and runtime options so only executed paths change option state.
4. Functions/dispatch: resolve identity before builtin interception; definite local
   calls/imports/constant dynamic heads; ordered redefinition; deferred redirects.
   Initially reject recursion and incompatible call binding contexts.
5. Sourceable/source: owned body for return/status/argv/exports/caller locals;
   explicit cwd/PATH/sourcepath resolution and immutable dependency identities;
   execute every repeated occurrence; acyclic graph and one compatible entry
   context per source file; discovery remains separate from relocation.
6. Isolation/residual semantics: apply the same model to substitutions, pipelines,
   background jobs, process substitution, traps and remaining nodes; preserve
   exact supported cases and reject undischarged isolation/callback obligations.
   Remove old semantic walkers as their ownership moves.
7. Publication/readiness: managed immutable generations, consistent documentation,
   compatible quality-tool bootstrap, packaging and full local/CI evidence.
   Publication/tagging remains a separate action.

## Explicit initial limits

Flow domains use finite definition identities, effect flags and Unknown. Changing
environment facts widen to Unknown. Arbitrary eval, computed source, cycles,
unknown dispatch and caller effects reject. Source/function context sensitivity
does not grow unbounded call strings or recursive specializations. SCCs are
deferred; if justified later, use a finite monotone worklist, never claim program
termination from analyzer convergence.

Function-local support initially uses the verified nonshadowing/function-body
local envelope. Conditional locals, local deletion, namerefs and sparse arrays
reject until storage semantics exist. Internal source calls explicitly forward
inherited argv; external sourceable entrypoints accept explicit argv. Caller argv
mutation and unsupported nonlocal exits reject. Relevant unknown handlers prevent
admission, including transitive effects; suppressing handlers is not equivalence.

## Atomic publication

Stage immutable generations on the destination filesystem, pin all runtime/child
references to the generation, flush files/directories, and atomically replace one
entry loader. Preserve entry status, arguments, source scope and final status.
Serialize publishers per destination; reject ownership and symlink conflicts.
Retain old generations without automatic GC. Recovery after ambiguous replacement
inspects the published entry. Readers see an old or new complete generation.
This does not roll back effects of executing a translated script.

## Acceptance

Every stage needs RED/GREEN differential or explicit-rejection evidence, positive
end-to-end examples, accounted-for expectation changes, and documentation updates.
No unexplained failure backlog or blanket-rejection completion claims.

Required interactions: all eleven counterexamples; zero/one/many word fields;
empty quoted substitutions; option changes through branches/calls; redefinitions;
deferred redirects; source return/status/argv/repeated execution; caller locals;
namespace collisions; callbacks; shared/child effects. Add bounded compositional
generators with shrinking and a distinct zero-diagnostic-mismatch category.
Compile negatives need successful positive controls and real abstraction checks.
Publication tests inject staging/flush/replacement/recovery failures and exercise
concurrent readers.

Final gates: GHC 9.12.2 and 9.14.1, Cabal 3.16.1, pinned Fish/runtime evidence,
moving Fish signal, integrations, six Linux selectors, parity manifests, HLint,
Ormolu, Haddock, cabal check, unpacked-sdist build/install smoke. Skips remain gaps.
Completion needs a working exact words/arithmetic/control/functions/literal-source
core in both entry modes, truthful residual classification, atomic publication and
reproducible final-tree evidence. Rejection coverage alone is insufficient.
