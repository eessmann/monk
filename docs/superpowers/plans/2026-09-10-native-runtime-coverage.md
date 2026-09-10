# Broader Bash coverage and a smaller, faster Fish runtime

Approved implementation specification: the user's 2026-09-10 plan of the same
name. This extends the existing uncommitted principled translator; it does not
replace or reset that work. No worktrees in `/tmp`, commits, or publication.

## Global constraints

Keep one syntax → semantic plan → complete materialization → admission → Fish
DSL → publication pipeline. Preserve occurrences, evaluation regions, storage,
control, and child/shared effects. Exactness is conditional on the Bash 5.3
signed-64-bit, C-locale, Linux profile, initially Bash 5.3.9/Fish 4.6.0. Helpers
execute bounded structured operations, never arbitrary expression strings.

## Task 1: Native runtime and child transport

Replace all six Python operation families with a private Haskell runtime core
and `monk-runtime`: signed integer primitives, IFS splitting, quoted argv,
echo bytes, patterns/globs, and child launch/capture. Use raw byte frames,
explicit operation/ABI/status, preserve original stdin and child descriptor,
signal, environment, NUL and newline behavior. Keep runtime free of ShellCheck.
Share pure integer operations with compile-time optimization. Add independent
Bash comparisons and malformed transport tests before implementation.

## Task 2: Runtime ownership and publication

Add typed native requirements, `--runtime FILE`, and `--managed`. Bind runtime
before admission. Installed entry resolves and checks the helper before body
effects (failure status 125); provider remains immutable during execution.
Managed plans capture provider bytes and include role/mode/path/bytes in the
generation identity. Stage executable mode 0700; validate and flush before
entry replacement. Add opaque artifact accessors, retain publication recovery,
and pin children/exported functions to their generation. Preserve old functions
after new publication and recognize their private runtime capture as owned.

## Task 3: Materialization optimization

Intern typed helpers by owning boundary; fold successful signed-64-bit
constants; batch pure arithmetic without crossing writes/lazy/effect regions;
use proved native Fish fast paths; remove dead temporaries/state; prune child
snapshots and helper closure. Keep failures and occurrence identity. Expose
authoritative static statistics separately from measured process launches.

## Task 4: Common syntax coverage

Implement arithmetic for/$[], byte ANSI quotes, nested comma braces with
effect duplication before expansion, lazy defaults/alternates, bounded trim
and literal replacement, ordinary scalar +=, owned literal shift, fixed-arity
tests and lazy/grouped [[ conditions ]]. For shifts through literal sources,
require provably nonempty effective source arguments; otherwise reject
transitive argv writes. Preserve exclusions for arrays, runtime evaluation,
recursion, computed sources, unsupported parameter/test/callback forms.
Mandatory existing fixture gains: pyramid-left, pyramid-right,
syntax-dollar-single-quote, syntax-dollar-bracket-arithmetic, semver-normalize,
neofetch-mini, syntax-brace-expansion. Both entry modes require fresh positives.

## Task 5: Stable directory contract

Standalone `--directory-contract stable`; sourceable v2 contracts declare
cwd/PWD/OLDPWD/stack permissions separately. V1 retains previous restrictions.
Require empty CDPATH, proved paths, stable logical ancestry including external
effects; reject interior name/.. cancellation and unsupported options/paths.
Support cd DIR/--/-L, proved cd -, pwd/-L/-P, pushd DIR, no-argument popd.
Use parent Fish cd plus actual diagnostic conversion, not filesystem prechecks.
Preserve OLDPWD export state, stack failure behavior and Bash output. Reject
direct PWD mutation. Track success/failure cwd and only resolve sources under
known execution cwd. Preserve child directory isolation. Mandatory additional
existing fixtures: cd-tmp, pwd-cd, pushd-popd.

## Task 6: Verification and evidence

Freeze source, binaries, 95-fixture inventory and baseline measurements first.
Targets: >=45/95 default and >=48/95 stable-directory, no admitted mismatches,
all current exact cases retained. All requested interaction regressions from
the approved specification remain mandatory. New fixtures do not change the
historic denominator. Simple literal output has no native helper/Python;
large-exact Fish <=25% baseline bytes; arithmetic group >=2x median speedup;
original common-16 aggregate median regression <=10%. Three warmups, twenty
serial alternating baseline/candidate samples; first-run and total native
bundle bytes reported separately. Bakeoff workers carry full config/runtime.

Finish both GHCs, integrations, pinned/moving Fish, publication and compile-fail
positive controls, Linux selectors/parity, HLint/Ormolu/Haddock/cabal check,
unpacked source install of both binaries, execution without Python. Update
architecture/audit/syntax/roadmap/migration/comparison evidence per slice. Keep
all unrun remote checks and skips explicit. Publishing/tagging remain separate.
