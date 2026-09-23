# Rust runtime performance evidence — 2026-09-23

The final Rust release was faster in all five measured workloads, with identical stdout bytes, stderr bytes, and exit status in every accepted sample. Its executable is 1,290,640 bytes versus 27,848,096 bytes for the frozen Haskell executable: 4.635% of the baseline size. These are local representative measurements, not a replacement for the historical aggregate performance gate.

| Workload | Haskell median (ms) | Rust median (ms) | Rust / Haskell medians | Paired ratio p10–p90 |
|---|---:|---:|---:|---:|
| Startup `--describe` | 13.651 | 7.264 | 0.532 | 0.456–0.572 |
| Arithmetic loop, 32 iterations | 3957.126 | 582.517 | 0.147 | 0.143–0.149 |
| Byte echo/printf, eight iterations | 679.641 | 133.409 | 0.196 | 0.190–0.201 |
| Supervised read/pipeline | 696.287 | 149.613 | 0.215 | 0.212–0.223 |
| Process substitution | 597.114 | 130.474 | 0.219 | 0.211–0.225 |

Evidence: `docs/evidence/rust-runtime-performance-2026-09-23.json` contains every raw sample, min/p10/median/p90/max distributions, paired ratios, command arguments, input/generated hashes, provider hashes, stream hashes, and host details. Lower ratios mean less elapsed time.

## Frozen inputs

- Haskell runtime: `artifacts/rust-runtime-baseline/monk-runtime`, SHA-256 `4e5a2527dae2133a7915fdbfacf8b4b2b95f38be13b59fe60a98a6850083fdb2`.
- Rust release: `artifacts/rust-runtime-clap/monk-runtime`, SHA-256 `5174a6e287204bf63c71e14b7d6d97ef0017ac467a2e3dfe7cacf3b8529a27a0`.
- Same translator for both providers: `artifacts/rust-runtime-baseline/monk`, SHA-256 `b95ef206052e1bfc7a8cdd8d5a240c539600c97a20eeb09e2b75b33fc3c01f8a`.
- Bash 5.3.9: `/nix/store/s0psayl7zvkvwdcqc8fy1sbv8rlf1yq8-bash-5.3p9/bin/bash`, SHA-256 `c76a7b7482203eb6d4108271500848f121364b2898e7febaefbd458714c8f56e`.
- Fish 4.6.0: `/nix/store/2rb8r6s2ic5wryq75aa0k3vhav51mxj6-fish-4.6.0/bin/fish`, SHA-256 `241c9c36726193c252a8e3b58c1233edf2b192c3b4ae07b71077e04e52c33640`.

Runtime and translator copies under `artifacts/rust-runtime-performance-clap/providers` were checked before and after copying, then rechecked before measurement. Each source was compiled in strict mode with an explicit absolute provider path. Untimed preparation compared both runtime providers and, for generated scripts, Bash. The byte-output fixture includes a non-UTF-8 byte. All fixtures are embedded in the collector.

## Method and reproduction

Collector: `scripts/MeasureRustRuntime.hs`, compiled with GHC 9.14.1 and `-O2 -threaded -Wall -Werror -package aeson`; Ormolu and HLint pass. The final candidate includes the private native boundary, generated opcode constants, and Clap 4.6.7 with only `std` and `derive`. It uses Rust nightly 2026-09-23, Cargo release, and `-Zon-broken-pipe=inherit`. Host: Apple M1 Pro, ten logical CPUs, 16 GiB RAM, macOS 27.0 build 26A428, Darwin 27.0.0 arm64.

Three untimed warmups per provider preceded 20 paired samples per workload. Provider order alternated each pair, and all invocations ran serially. Monotonic nanoseconds cover process creation, completion, and raw ByteString stream capture, without polling sleeps. Every warmup and measured invocation had to match the validated baseline bytes and status. Environment is explicitly constructed with pinned provider/Bash/Fish paths, isolated HOME, `LC_ALL=C`, `LANG=C`, `TZ=UTC`, and canonical short `TMPDIR=/private/tmp`.

```sh
ghc -O2 -threaded -Wall -Werror -package aeson -outputdir /private/tmp/monk-runtime-performance-build scripts/MeasureRustRuntime.hs -o /private/tmp/measure-rust-runtime
/private/tmp/measure-rust-runtime prepare artifacts/rust-runtime-baseline/monk-runtime artifacts/rust-runtime-clap/monk-runtime artifacts/rust-runtime-baseline/monk /nix/store/s0psayl7zvkvwdcqc8fy1sbv8rlf1yq8-bash-5.3p9/bin/bash /nix/store/2rb8r6s2ic5wryq75aa0k3vhav51mxj6-fish-4.6.0/bin/fish FRESH_WORKSPACE docs/evidence/rust-runtime-performance-2026-09-23.json
# Stop competing builds and tests before measuring:
/private/tmp/measure-rust-runtime measure FRESH_WORKSPACE
```

The reported final Clap run started only after the coordinator and build agent confirmed a quiet window and finished at 2026-09-23 12:43:49 UTC. Builds resumed only after completion. All 200 measured invocations (five workloads, two providers, 20 samples) passed byte/status equality.

Earlier measurements for SHA `9b5cb364…` are preserved under `artifacts/rust-runtime-performance/prior-snapshot-measurements.json` and `prior-snapshot-report.md`; they are not included in this report. A partial attempt during that earlier measurement phase overlapped packaging and was discarded without writing a report. The intermediate `rust-runtime-performance-complete` workspace contains untimed preparation only and was never measured. The durable JSON now identifies only the final Clap SHA `5174a6e2…`.

Limits: one host, warmed wall times including launch/capture overhead, and a small representative workload set. No Linux execution, cold-cache measurement, allocation profiling, process tracing, or whole-system performance claims. Binary file size excludes separately installed shared libraries. Raw distributions are observations, not confidence intervals.
