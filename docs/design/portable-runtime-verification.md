# Portable runtime verification

Local verification uses locked Nix packages: GHC 9.14.1 (default), GHC 9.12.2
(compatibility), Cabal 3.16.1, upstream `bashNonInteractive` 5.3p9 (reporting
5.3.9), Fish 4.6.0 and the separately locked moving Fish 4.9.3. The private Bash
build was removed at the user's request; its earlier observations are historical.
The [reference receipt](../evidence/portable-exact-reference-2026-09-22.json)
records actual binary hashes and Unicode behavior. Version strings alone do not
establish reference equivalence. Dependencies and test programs come from Nix.

## Verified local outcomes

The [durable final ledger](../evidence/portable-exact-final-verification-2026-09-22.json)
preserves command outcomes, source fingerprints, log and binary hashes, package
inspection and execution evidence. Full local receipts and raw observations are
under `.superpowers/sdd/2026-09-22-portable-exact/`.

| Gate | Local outcome |
| --- | --- |
| GHC 9.14.1 main suite | 980 tests pass; Haskell publication 38 and pure runtime suite pass |
| GHC 9.12.2 compatibility | Build all, main 980, publication 38 and pure runtime suite pass |
| Moving Fish 4.9.3 | Full 978 before the harness correction, plus both new framing regressions; unchanged product bytes |
| Native semantics | Protocol, byte, printf, expansion, patterns, read, descriptors, sessions, processes, exec, signals, launcher and writer checks pass |
| Callback diagnostics | 33 exact byte comparisons pass |
| Managed publication | Captured provider survives installed-provider removal; loader and generation entry pass all eight stdio masks |
| Python publication / API | 10 publication cases and 12 public API probes pass |
| Quality / collector regressions | HLint, Ormolu, Cabal checks and 27 evidence regressions pass |
| Haddock / source distribution | Documentation generated; unpacked archive builds all components, installs both executables and passes Unicode-path combined/managed parity |
| Darwin package | Final signed bytes inspected, copied outside the store and executed with a clean environment |

The main-suite total uses Tasty's normal result accounting: the oversized manual
neofetch baseline is explicitly skipped, and is not an exact execution match.
Linux execution was explicitly deferred by the user. No remote hosts, builders or
source transfers were used for these final checks; configured CI jobs are not
claimed to have run.

One complete canonical command passed main 980 and all native checks, then failed
an obsolete publication expectation that literal output needed no runtime helper.
That failed receipt remains failed. A focused stable follow-up passed the corrected
10 publication tests, API probes, quality and collector tests. No unchanged broad
suite was repeated just to replace that receipt. The exact writer requirement and
native launcher's purpose are now tested directly; a silent literal assignment
provides a separate no-body-helper control.

## Provenance and final source archive

Receipt schema 2 records inputs before and after each command, including production
sources, Haskell and native tests, fixtures, collector scripts, documentation and
packaging. Changes invalidate its successful-stable flag. Documentation closure
is recorded separately; earlier full-input receipts remain snapshots.

The preserved pre-optimization comparison build fingerprint is
`ebad27c97225ecf2306d50dcc671ddbd966b8e8e659c0322f32af258a85c48a1`.
After the test-harness correction, the conservative production-scanner fingerprint is
`6a7a5cbc423fc1e9b20b64f89de58933598ce78c35dad405bc57f6f1cc17b9d5`.
Their only differing scanned source is `src/Monk/Internal/Shell.hs`, registered
only in the test harness. Its environment snapshot changed from line splitting
to NUL framing after an inherited multiline `DEVENV_CMDLINE` forged a map entry.
Two regressions and full 980 runs under both compilers validate the fix. The
compiler and runtime executable hashes are exactly unchanged, explicitly bridging
the earlier comparison and moving-Fish evidence. The scanner was not weakened.

A subsequent measured optimization changes only runtime leaf termination after
complete output or launcher cleanup. It avoids ordinary RTS teardown for describe,
direct writers and the launcher; session and asynchronous primitive paths retain
their existing lifecycle. The final production fingerprint is
`15f89a7328f7ed21582ef002376ea1f3158902e1cfb2d13b82c4c566d4452c63`,
and the canonical runtime hash is
`069d8c65809e22c0ed6ed91ea806c87d501dbd7ffcbf9db140a7f8442bd6353a`.
Compiler bytes remain unchanged. A fresh canonical 980 run, all native suites,
publication 10, optimized GHC 9.12 runtime build/focused checks, moving-Fish focused
checks and copied package checks pass. New partial-reader SIGPIPE and describe
flush controls supplement the existing stdio, cleanup and surviving-background
checks. An initial compatibility runner omitted the runtime PATH; its failed
provider lookup is preserved, and the corrected focused receipt passes.

The final source archive is `artifacts/source-final/monk-0.4.0.tar.gz`. Its
repackaging/content receipt and checksum are stored beside the archive, outside
its own contents. Unchanged components reuse the successful unpacked build/install evidence.
The changed runtime source was separately compiled with both GHC versions and
inside the clean Nix package, then executed in the recorded native checks.
Final archive content verification includes that source, new tests and documents;
it does not claim a second full unpacked build of the final archive.

## Independent correctness comparison

The [frozen manifest](../evidence/portable-exact-cohorts-2026-09-22.json) retains
all original 95 source hashes and the denominator. Each provider is compared
independently with Bash. Original Monk (`2bc0e72`) and current baseline
(`c2bd371`) were built locally from unchanged git archives. The current baseline's
native runtime rejects Darwin; those cases are unavailable, never counted as
matches or translation rejections.

| Historic95 provider/lane | Match | Mismatch | Rejection | Unsupported native platform |
| --- | ---: | ---: | ---: | ---: |
| Original Monk | 71 | 24 | 0 | 0 |
| Current baseline, default | 23 | 0 | 50 | 22 |
| Candidate, default | 74 | 0 | 21 | 0 |
| Babelfish 1.2.1 | 26 | 32 | 37 | 0 |
| Current baseline, stable directory | 23 | 0 | 47 | 25 |
| Candidate, stable directory | 77 | 0 | 18 | 0 |

The final [default](../evidence/portable-exact-comparison-optimized-default-2026-09-22.json)
and [stable-directory](../evidence/portable-exact-comparison-optimized-stable-2026-09-22.json)
receipts preserve captured executable hashes. All 23 locally executable baseline
positives remain matches. The default lane separately matches filesystem bytes
and modes (1), sourceable caller state (1), handshaked process behavior (2), and
four strengthened cases. Visible timing is explicitly rejected. These nine
additional cases do not enlarge historic95. Stable-directory results are reported
for historic95 separately; that option is incompatible with the sourceable caller
contract, so its extra caller rejection does not replace the default caller result.

The strengthened controls observe populated `read -a` with nonempty IFS, EOF
without a newline, NUL plus invalid UTF-8 input, dense-array append and quoted
expansion, and deterministic visible `time` stderr. Discarded values or timing
cannot silently pass. Standalone candidate execution is
`monk-runtime --abi 2 launch SCRIPT ARGS`, preserving stdio before Fish startup.
Sourceable caller observations retain their Fish wrapper. Historical providers
retain their original entry contract.

## Performance and unmet acceptance requirements

The frozen common16, original arithmetic3 and targeted-native4 cohorts use three
warmups and twenty serial samples with alternating provider order. Every sample
checks raw stdout, stderr and status against Bash. Missing inputs, translation
rejections and unsupported native execution leave the full cohort unverified;
fast surviving subsets cannot pass it. No compiler or verification jobs ran
concurrently with the final timing pass. The pre-optimization and optimized performance receipts record
actual subset timings separately from complete-cohort acceptance.

The preserved [pre-optimization timing receipt](../evidence/portable-exact-performance-final-2026-09-22.json)
measured 14/16 common cases; two require the baseline's unavailable Darwin runtime.
For that surviving subset, the median sum is 154.864 ms for the baseline and
1013.566 ms for the candidate: **6.545 times the baseline time**, a substantial
observed regression. This subset is not the full common16 acceptance result.
No arithmetic3 case was measurable. Targeted-native4 has two unsupported baseline
platform cases and two baseline translation rejections, so none was measurable.
The complete performance gates remain unverified, and the measured regression
must not be described as a pass.

After the bounded leaf-termination correction, the
[optimized timing receipt](../evidence/portable-exact-performance-optimized-2026-09-22.json)
measures the same common14 subset at 171.476 ms baseline and 621.447 ms candidate:
**3.624 times the baseline time**. Candidate summed median time is 38.7% lower
than in the earlier run, but the observed regression remains substantial. The
complete-cohort and tracing gaps are unchanged. **Performance is not accepted.**
Both runs and their exact providers are preserved; no post hoc subset replaces
the frozen acceptance cohorts.

The original arithmetic3 source inputs are missing; their historical hashes are
retained, without reconstructed replacements. Native baseline execution is
unavailable on Darwin. Process tracing did not establish launch reductions.
Historical Linux timings are not fresh evidence for this candidate.

The helper-free simple greeting/conditional criterion remains **unmet**. Exact
output error and SIGPIPE semantics require the native writer, and the user chose
a native launcher to preserve initially closed stdio. There is no approximate
backend or silently narrowed output contract. Linux native execution, minimum-OS
execution and complete performance/launch-reduction gates also remain unverified.

## Runtime package

The Darwin package uses static Haskell and third-party dependencies, with only
Apple `libSystem`, `libiconv` and `libffi` dynamically referenced. Its final
signed runtime is 27,433,632 bytes, SHA256
`76e2ba5b9e2ee3dbc4af5a6a03f3542f0d678c38ea9d0c0fd8c9598a9ffc5ae0`,
ABI 2, profile `bash53-i64`. Mach-O declares minimum macOS 14.0; execution was
verified on this arm64 macOS 27 host, not on the minimum version. The optimized Nix
derivation is:
`/nix/store/f2m9znv6cf4vkz2qacl843mlfg8wm4xd-monk-runtime-aarch64-darwin.drv`.
Only three Monk derivations were built; dependency/compiler builds were not needed.

The portable archive is `artifacts/monk-runtime-aarch64-darwin-optimized.tar.gz`
(5,864,158 bytes, SHA256
`49571f0db325aab4dc79168d38816b5d7183e7b76f8835803f6099b59b1a61ad`).
It contains the executable, package inspection and copied execution manifests.
The extracted local artifact is `artifacts/runtime-optimized/bin/monk-runtime` and
its execution manifest is `artifacts/runtime-optimized-execution.json`.

The original package, archive and manifests remain preserved under their prior
`runtime-final` paths; their hashes remain in the ledger as pre-teardown evidence.

Linux release derivations require fully static musl ELF files without interpreter
or dynamic dependencies. Their declared kernel 5.4 floor is conservative and
unverified by execution. See [environment commands](../../nix/README.md) for
native devenv/Fish usage and release inspection commands. Earlier
[snapshot evidence](../evidence/portable-exact-local-snapshots-2026-09-22.json)
remains historical and does not replace the final records.
