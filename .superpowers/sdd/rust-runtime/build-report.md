# Rust runtime build and distribution report

Workspace: `/Users/erich/.codex/worktrees/monk-rust-runtime/monk`. Durable machine-readable receipt: `docs/evidence/rust-runtime-build-2026-09-23.json`.

## Build inputs and cutover

`devenv.lock` pins rust-overlay `fb058ecf6d14837ea152a3d5225ce7f88ee5cde1`; `rust-toolchain.toml` selects the 2026-09-23 nightly through `languages.rust.toolchainFile`. The inspected manifest has cargo, rustfmt, Clippy, Miri, rust-src, and Rust std for the three supported targets. The pinned shell reported rustc `1.100.0-nightly (6bb1652a0 2026-09-22)` and Miri `0.1.0 (6bb1652a02 2026-09-22)`. `Cargo.lock` contains clap 4.6.7, libc 0.2.189, nix 0.31.3, rustix 1.1.5, and tempfile 3.27.0; `.cargo/config.toml` applies `-Zon-broken-pipe=inherit`.

`monk.cabal` remains a Simple Cabal package for the translator and pure `monk-compiler-support`. Devenv orchestrates Cargo and Cabal separately. The superseded Haskell execution runtime and C sources were removed after conformance. `protocol/abi2.tsv` now generates the shared ABI/capability/90-opcode tables for Haskell and Rust; its drift check passed. The generated Haskell support/tooling rebuilt warning-free under `-fdevelopment`, and the compiler-support and 63 tooling tests passed. `monk-quality` passed with no HLint hints or Cabal metadata warnings. Haddock passed on an earlier Haskell snapshot with nonfatal missing-doc/link warnings; it was not rerun after the opcode generation change.

## Final Darwin package and integration

The clap-based Nix package built at `/nix/store/x6jz2wx103yvv1d5p90hjvn8w5yc6xc1-monk-rust-runtime-aarch64-darwin`. Its copied Rust executable is 1,285,584 bytes, SHA-256 `0d197973592bacbaa20c3291416c25c48375ef091f0d18dfac08f6b42ebc639c`. `otool` lists only Apple's libSystem and iconv, `codesign --verify` passed, and the package inspector accepted ABI 2, arm64, and a macOS 14.0 deployment target. The minimum OS was not executed.

All 15 named runtime ABI suites plus child transport passed against that exact packaged executable. The 16 receipts are under `artifacts/rust-runtime-build/package-darwin-clap/checks/`; `artifacts/rust-runtime-build/package-darwin-clap/native-execution.json` binds them to the binary hash and records `execution_verified: true` and `check_receipts_verified: true`. The local expansion suite reported two filesystem-rejected invalid-byte pathnames as platform gaps.

The final `devenv shell -- monk-integration` passed: 980 translator cases, 38 publication cases, the compiler-support test, 63 tooling cases, all 15 runtime suites, digest, child transport, and public boundaries. Its complete log is `artifacts/rust-runtime-build/integration-clap.log` (SHA-256 `a78def4baef245f32d91a7e8e70aa341ce11a09251790368d0b6ae9142ff2a24`). The root release/differential/Miri and performance evidence is separately recorded in `docs/evidence/rust-runtime-verification-2026-09-23.json` and `docs/evidence/rust-runtime-performance-2026-09-23.json`.

## Source archive and limits

`devenv shell -- monk-sdist` passed on the final clap snapshot. The tarball is `artifacts/sdist/unpacked-jHB8xbtv/monk-0.4.0.tar.gz`, 1,033,835 bytes, SHA-256 `0f9784c8c9ac57be00786724cde33a537b6e39baaf5590b620002e7dc8ca6c8f`. It contains the clap 4.6.7 lock, private CLI source and CLI contract tests, ABI generator/tables, type tests, and final performance JSON. Its unpacked tree passed ABI drift checking, locked Cargo tests and release build, Cabal build/install, and strict combined and managed Bash/Fish Unicode smoke checks. The verification log is `artifacts/sdist/unpacked-jHB8xbtv/verification.log` (SHA-256 `4ae97ab40d7192bb74fd0fda3b69a52e5b55c352822252e62ae008c49a0c33f9`). Later build receipt and prose annotations are necessarily outside this archive snapshot; the package Nix path likewise names its own earlier source snapshot.

The x86_64 and aarch64 Linux targets have Nix evaluation and all-target Cargo check evidence only. No linked Linux release or native Linux execution is claimed. CI has not run in this local worktree.
