# Principled translator implementation plan

**Goal:** Implement the author-approved design as usable, independently reviewed
vertical slices, starting from main 1a2c382.

**Spec:** ../../design/principled-translator-design.md

**Global constraints:** Haskell/GHC2024/Relude, GHC 9.12.2 and 9.14.1, Cabal 3.16.1,
Fish 4.6.0 and Bash 5.3.9 initial runtime evidence. No /tmp worktrees. No wholesale
prototype import, general shell interpreter, detached certificates, or unaccounted
test failures. Preserve the user's main-checkout AGENTS.md change.

## Task 1: Preserve counterexamples and positive evidence

Add `test/Unit/SemanticAdmission.hs` and persistent Bash fixtures under
`test/fixtures/semantic/`. Register tests in Spec.hs and monk.cabal. Use current
strictConfig and the real shell harness; exact cases must translate AND match
stdout/stderr/status, rejected cases must fail with diagnostics. Reproduce the
eleven saved cases: untaken option branch, uncalled option function, multi-IFS,
embedded quoted argv, lazy case pattern, case fallthrough, sparse/mixed array,
integer intermediate division, dynamic caller local, constant dynamic command,
and eval. Sparse arrays and eval are rejection cases; remaining nine require
successful exact behavior. Include simple commands/control as positive controls.
Record the expected RED failures before any production fix.

## Task 2: Introduce contracts and semantic admission

Implement explicit policy/profile/entry/caller types, stable admission diagnostics,
and a private semantic plan owning actual source syntax/identities and generated
behavior. Make exactness decisions before lowering. Reject unknown parser forms,
unsupported storage/eval and unproven execution effects. Keep successful core paths.
Use one authoritative normalization/analysis pass; no new framework of receipts.
Add API/CLI/negative-construction tests and document intentional compatibility
changes. Do not reclassify failed exact-core tests as unsupported.

## Task 3: Close word and evaluation semantics

Preserve quote fragments and field splicing until splitting/globbing decisions;
implement embedded argv and empty fields, multi-character IFS membership, lazy
case patterns/fallthrough, nested evaluation effects, and explicit supported
expansion contexts. Add boundaries and generated-composition differential tests.

## Task 4: Close integer arithmetic and option execution

Implement parsed bounded integer operation helpers with typed outcomes, explicit
64-bit semantics and no expression eval. Runtime option changes follow executed
control paths; merge analysis conservatively and preserve function deferral.
Verify integer division chains, signs, boundaries, failures and option interactions.

## Task 5: Close function/source dispatch and sourceable ABI

Implement finite definite bindings and imported-call contracts, dynamic heads with
proved constants, deferred function redirects, and bounded dynamic local support.
Reject recursion/incompatible binding contexts. Add owned source-body return and
status/argv boundaries, occurrence-based literal source discovery, dependency
snapshot resolution and explicit sourceable imports/exports. Verify actual
standalone and sourceable programs, repeated sources and caller-state behavior.

## Task 6: Complete residual policy and opaque public products

Audit every remaining parser constructor/context, child/shared execution,
pipelines/substitutions/background/traps. Retain supported exact cases, gate named
approximations, reject unsupported behavior before output. Make results, graphs,
bundles opaque with accessors and construction controls. Remove superseded walkers
and make docs/inventory agree with observed support.

## Task 7: Implement generation publication and final readiness

Add pure publication planning and a separate effectful writer with managed
immutable generations, pinned dependencies, fsync, destination lock and atomic
entry replacement. Preserve sourceable ABI across the loader. Inject failures and
test concurrent readers/recovery. Fix CI quality-tool bootstrap and packaging;
run all spec gates and independent whole-change review. Record unavailable remote
evidence honestly; do not publish or tag.
