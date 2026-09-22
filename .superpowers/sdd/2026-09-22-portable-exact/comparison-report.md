# Local comparative evidence — preliminary

The first fresh Darwin run preserves the 95 historical inputs and their SHA256 hashes. Each of the four translators was independently compared against the pinned custom Bash 5.3.9 under LC_ALL=C and Fish 4.6.0. Generated files, diagnostics, raw stdout/stderr and status are retained per fixture in `comparison-preliminary`; copied executable providers are immutable and excluded from Git because they are approximately 250 MiB.

| Frozen95 outcome | Original 2bc0e72 | Current c2bd371 | Candidate snapshot | Babelfish 1.2.1 |
|---|---:|---:|---:|---:|
| Match | 71 | 23 | 54 | 26 |
| Admitted mismatch | 24 | 0 | 0 | 32 |
| Translation rejected | 0 | 50 | 41 | 37 |
| Native platform unavailable | 0 | 22 | 0 | 0 |

The unchanged c2bd371 native provider explicitly rejects Darwin with `this runtime profile requires 64-bit Linux`. Its 22 native-dependent frozen95 cases therefore provide no semantic comparison on this host. No generated baseline was rewritten or ported. This is not evidence that those 22 cases fail on Linux.

Four additional cases remain outside the denominator. The candidate sourceable function/caller-state case passed. File bytes/append/modes and the two process cases (FIFO-handshake background job, 1 MiB raw-byte process substitution) were rejected by this preliminary binary, which predates the new redirect/process-substitution materializer. The current baseline's caller case reaches the same unavailable native guard and records source status125; the initial raw report labels its final wrapper result a mismatch, but it is a platform gap. The next run's classifier correctly accounts for sourceable wrappers returning a later status.

The process handshake uses ready/go FIFOs and records readiness/release events; it does not infer ordering from sleeps. The original background translation timed out and is unavailable rather than a match. Filesystem observations include regular-file raw bytes, SHA256, permission mode, directory type, symlink target and FIFO type. Caller observations test an exported function's local variable isolation and preserved caller bindings/status. These focused cases supplement, rather than replace, the larger runtime ownership regression suites.

## Reproduction and provenance

- `scripts/portable-comparison.py freeze --output NEW_DIRECTORY` copies reproducible input/auxiliary files and rejects any changed historical input hash.
- `scripts/portable-comparison.py measure --help` lists required independently built translators/providers, exact Bash/Fish, stage and source-fingerprint inputs. Every run creates a fresh directory; it never overwrites an earlier run.
- Original and current source snapshots came from local `git archive 2bc0e72` and `git archive c2bd371`, built unchanged under the canonical GHC9.14.1 shell in `/tmp/monk-portable-comparison-snapshots`. Their build log is `/tmp/monk-comparison-build.log`.
- The candidate came from the canonical `dist-ghc9141` build after arrays/jobs integration. It predates pending overflow-array-index and later descriptor work. `preliminary-source-inputs.json` is a working-tree hash receipt at capture, not proof that every in-progress source edit was in that binary. Candidate and runtime binary hashes are recorded independently in the raw report.
- `scripts/reference-runtime-profile.py` records executable SHA256, complete version line and raw Unicode escape probes. Nixpkgs Bash with the same 5.3.9 version but no iconv is not interchangeable with the canonical custom Bash.
- Harness checks passed: three focused regression tests for raw bytes, modes/link targets, and timeout classification. Package-verifier checks remain separate.

## Outstanding evidence

A final-tree run is still required after all implementation and builds stabilize. This preliminary count is not final acceptance. Fresh timing and process-launch improvements remain unverified: the historical native baseline cannot execute on Darwin, and the user explicitly deferred Linux execution and kept all source local. Historical Linux performance reports remain historical. No remote run, CI result or package execution on Linux is claimed.
