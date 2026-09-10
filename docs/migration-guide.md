# Migrating to the principled translator

The redesign deliberately changes the pre-1.0 API and CLI. The active
implementation and verification status is in
[the roadmap](design/translator-todo.md).

Run output under the [versioned execution profile](design/execution-profile.md).
It specifies startup options, locale, command requirements and caller obligations
separately from the checks performed by generated code.

## Policy and entry mode

The Boolean configuration field is replaced by explicit `translationPolicy`,
`targetProfile`, `entryMode`, and `callerContract` fields. Use `strictConfig` for
exact-only standalone translation. `defaultConfig` also permits no
approximations unless they are selected by name.

```haskell
cfg = defaultConfig
  { translationPolicy = Migration (Set.singleton ReadonlyUnchecked) }
```

The CLI accepts `--target-profile bash-5.3-fish-4.6` and
`--entry standalone|sourceable`. `--strict` cannot be combined with
`--allow-approximation NAME`. The initial approximation identifier is
`readonly-unchecked`; selecting it never authorizes unrelated unsupported behavior. A selected approximation
still needs a materialized implementation and its own diagnostic.

Unsupported input now returns diagnostics without executable output. The old
normal-mode comment/false or silent-success replacements are retired from the
public translation path. Do not treat an absent output file as a translated
script, or an approximation warning as exactness evidence.

`translateParseResult` remains an advanced syntax-only entry. Because a raw
ShellCheck parse result does not own the original source bytes, it rejects
operations needing exact error spelling. Use `translateBashScript` or
`translateBashFile` for ordinary translation; use source-graph translation for
literal dependencies. Tests that deliberately construct parser nodes can still
exercise syntax admission through the limited entry.

## Sourceable caller contract

`--entry sourceable` requires `--caller-contract FILE`. A contract is rejected
in standalone mode so its obligations cannot silently go unused. The JSON
format is versioned and rejects unknown fields:

```json
{
  "version": 1,
  "ambientEffects": "none",
  "variables": {
    "result": { "access": "read-write", "scope": "visible", "exported": false }
  },
  "functions": {
    "visit": { "target": "host_visit", "reads": ["result"], "writes": ["result"] }
  }
}
```

`access` is `read`, `write` or `read-write`; `scope` is `visible` or `global`.
`exported` describes the initial binding attribute (default false), not a desired
attribute synthesized by assignment. An absent global output may initially be
created only unexported; changing export status requires an executed Bash
`export`. Visible writable bindings must already exist in the caller frame.
`exportedFunctions` lists the function names the source is allowed to leave
installed, and those functions must remain callable after the source returns.
Function effects must refer to appropriately accessible declared variables.
A declared imported function must have equivalent output/status behavior and
only its declared state effects. Imported functions must not inspect private
runtime bindings or reenter the translated artifact. `ambientEffects: none` promises that no
relevant callbacks or ambient effects intervene; omitting it means unknown,
which does not authorize sourceable execution. Runtime shape/lookup checks
cannot prove these behavioral obligations. Reserved runtime and Fish names
are rejected.

Programmatically constructed contracts undergo the same semantic validation as
JSON contracts before any imported facts enter normalization. Nonempty caller
contracts in standalone mode reject through the library API as well as the CLI.

Sourceable entry arguments are explicit. Internal literal Bash `source` calls
without operands inherit argv and must forward it explicitly. Return exits an
owned source-body frame; it must not escape the caller function. Caller argv
mutation and unsupported nonlocal exits are initially excluded.

## Results and output

`TranslationResult` is opaque. Use `translationScript`,
`translationDiagnostics`, `translationRuntimeRequirements`, and
`renderTranslation` to inspect it. They are ordinary functions, so record
updates cannot forge a new result. General `Monk.AST` / `Language.Fish.DSL`
construction remains supported, but arbitrary scripts cannot become certified
translation products. Source graph and output bundle consumers use
inspection accessors and authoritative planners on the same principle.

Diagnostics have parse, translate, source and output phases. `PhaseRuntime`
is removed. `RequiresFishFeature` takes a typed capability instead of free text.
Consumers should pattern-match the capability or call `fishFeatureName`.
`RequiresPlatformCapability Linux64DescriptorFilesystem` has a child producer,
a profile admission check and a runtime descriptor preflight; use
`platformCapabilityName` for display.

Separate bundles migrate from direct root/child writes to managed immutable
generations and one atomic entry loader. Rendered inspection output is not a
publication protocol: use the bundle publisher. Prior generations are retained;
automatic garbage collection is outside this change. Recovery distinguishes
an unpublished attempt from a replacement whose final durability is uncertain.

Source lookup uses the declared execution cwd, PATH and Bash sourcepath state.
It does not fall back to the directory containing the Bash source file.
Output relocation never changes discovery semantics. Computed sources and
cycles require manual restructuring.

## Rechecking scripts

Keep exact positive examples alongside exclusions. Test stdout, stderr, exit
status, argument boundaries, filesystem changes and declared caller updates
against the selected Bash profile. Arrays, runtime expression-string arithmetic,
recursion, unknown dynamic dispatch and arbitrary eval need explicit redesign
rather than disabling diagnostics.

Both `planCombinedOutputBundle` and `planSeparateOutputBundle` return their
planning result in `IO`; resolving a relative output destination does not write
files. `publishOutputBundle` returns an opaque receipt or structured failure.
Inspect `outputFailureKind`, `outputFailureDiagnostic` and
`outputFailureObservedEntry`; do not assume that a post-rename failure rolled
back the entry. Separate rendered inspection files include the entry loader
and pinned generation members. There is no helper extraction pass after
admission and no unversioned child path to overwrite in place.

General file redirection is now an explicit exclusion: an access check followed
by native Fish reopening would introduce races and different failure behavior.
The admitted standard descriptor and `/dev/null` forms preserve ordering and
function invocation timing. Literal source dependencies can repeat under one
compatible entry context; a second call after incompatible binding/definition
changes rejects rather than reusing stale analysis.

The bake-off now executes standalone output and records `ShellRunExec`; legacy
fixture `.mode` sidecars no longer select sourcing for that runner. Use the
dedicated caller-contract suite for sourceable comparisons. Run
`scripts/compare-bakeoff-bash.py` after the runner to compare raw stdout/stderr
and status against Bash. The refreshed `--compatible` selector denotes the
dated shared matching subset, rather than mere translation success.

## Opt into stable directory operations

Standalone output can enable the bounded directory envelope with:

```bash
monk script.bash --strict --directory-contract stable --runtime /absolute/path/monk-runtime
```

The obligation is empty CDPATH, an ordinary exported scalar global PWD naming
the actual cwd, and logical cwd ancestry that remains valid throughout
execution, including external commands. Renaming a cwd ancestor is outside
this contract. Supported forms include proved `cd DIR` with `--`/`-L`, proved
`cd -`, `pwd` with `-L`/`-P`, `pushd DIR`, and no-argument `popd`. Interior
`name/..` paths, unknown paths, implicit HOME cd, physical cd and rotations
reject. The runtime attempts the actual parent Fish cd and converts its actual
C-locale diagnostic to the Bash source origin.

For sourceable output, keep the standalone CLI option absent and use version 2:

```json
{
  "version": 2,
  "ambientEffects": "none",
  "directory": {
    "contract": "stable",
    "cwd": "read-write",
    "PWD": "read-write",
    "OLDPWD": "read-write",
    "stack": "read-write"
  }
}
```

Each directory permission independently accepts `none` (the default), `read`,
`write`, or `read-write`. These permissions do not authorize arbitrary PWD
assignment. The stack bridge is an ordinary unexported global Fish `dirstack`
list of nonempty ordinary absolute logical paths. OLDPWD keeps its existing
export attribute; after explicit unset, successful cd creates it unexported.
Failure preserves the old value, cwd and stack. Standalone initialization also
models Bash's export-marked but unset initial OLDPWD.

A version 2 imported function may declare a `directory` object containing the
same four access keys; no `contract` key is needed within that effect object.
Undeclared directory effects mean preservation, and declared effects cannot
exceed the caller's permissions. Version 1 contracts continue to reject
directory operations. Unknown relevant ambient effects authorize neither
version. Relative sources require a known execution cwd on their actual
control edge: `cd /known/path && source ./dependency.bash` may establish that
fact, while `cd /known/path; source ./dependency.bash` cannot assume cd succeeds.

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

## Install or bundle the native runtime

Install both executables with `cabal install exe:monk exe:monk-runtime`.
Generated support no longer uses Python. Combined/stdout output needs a
compatible installed runtime when its typed requirements include native
operations. `--runtime FILE` overrides PATH lookup; the provider must remain
immutable and compatible during execution. Entries check ABI/profile/operations
before body effects and return 125 with `monk.runtime` on incompatibility.

```bash
monk script.bash --strict --runtime /absolute/path/monk-runtime -o script.fish
monk script.bash --strict --managed --runtime /absolute/path/monk-runtime -o entry.fish
```

`--managed` also works for a single input file. It captures provider bytes in
an immutable generation at mode 0700; the provider is not needed on PATH when
that bundle executes. The executable still needs its platform loader/libraries.
Existing exported functions keep their previous runtime generation after a new
entry is published. Prior generations must remain available.

Library callers select `translationRuntime` (`RuntimeOnPath` or `RuntimePath`)
and use `planManagedOutputBundle`. `RuntimeGeneration` is reserved for the
publisher's rematerialization. The unused `bundleRuntimeFile` slot is replaced
by `bundleRuntimeArtifacts`; inspect each artifact with `runtimeArtifactTarget`,
`runtimeArtifactMode`, `runtimeArtifactImage`, and the `nativeImage*` functions.
`renderOutputBundle` remains Fish text inspection; use `publishOutputBundle` to
publish binary members and executable modes correctly. Runtime image and
artifact constructors remain private.

`RequiresNativeRuntime` carries ABI, target profile and typed operation sets;
use `nativeOperationName` for display. `translationStatistics`,
`sourceGraphStatistics`, and `generatedStatistics` expose structural counts.
Static native-call sites are not a measurement of launched processes.
