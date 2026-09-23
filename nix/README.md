# Reproducible development and native runtime packages

Use standalone devenv 2.3.1 or newer. `devenv.yaml` declares
`github:input-output-hk/haskell.nix`, `github:NixOS/nixpkgs/nixos-unstable`,
and `github:oxalica/rust-overlay`.
Exact revisions belong in `devenv.lock`, including the independently locked tool
and reference package sets. haskell.nix's nested `nixpkgs` input follows the root
`nixpkgs` input. `follows: nixpkgs` names that input; the branch belongs in its URL,
not in a `follows: nixpkgs/nixos-unstable` path.
`cabal.project` supplies the shared Hackage index-state and development flags.
The default compiler is GHC 9.14.1; compatibility uses GHC 9.12.2.
The explicit haskell.nix imports for development and the release verifier inherit
devenv's `allow_unfree` policy. Without that propagation, newer nixpkgs rejects
the GHC toolchain bootstrap's unknown license metadata even when the project
already enables the policy in `devenv.yaml`.
The native runtime uses Rust 2024. `rust-toolchain.toml` pins the 2026-09-23
nightly with rustfmt, Clippy, Miri, and standard libraries for both Linux musl
targets and aarch64 Darwin. `languages.rust.toolchainFile` selects it through
devenv's locked rust-overlay. Cargo dependencies are fixed in `Cargo.lock`.

Configure the upstream IOG binary cache before the first shell evaluation.
For a single command, without changing machine configuration:

```sh
devenv --nix-option extra-substituters https://cache.iog.io \
  --nix-option extra-trusted-public-keys hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= \
  shell
```

The same two cache options apply to every command below unless the machine
already trusts that cache. First evaluation resolves the Cabal project; it may
need cached haskell.nix planning tools as well as the compiler. Inspect a dry run
before accepting a compiler source build.

```sh
devenv shell -- monk-build
devenv shell -- monk-test
devenv shell -- monk-integration
devenv shell -- monk-quality
devenv shell -- monk-benchmark
devenv shell -- monk-docs
devenv shell -- monk-sdist
devenv shell -- monk-rust-build
devenv shell -- monk-rust-test
devenv shell -- monk-rust-quality
devenv -O monk.compiler:string ghc9122 shell -- monk-build
devenv -O monk.fishChannel:string moving shell -- monk-integration
```

Each script also has a `monk:<name>` devenv task. The pinned reference consists
of the upstream nixpkgs `bashNonInteractive` 5.3p9 package (Bash 5.3.9 at runtime) and
Fish 4.6.0. The moving lane uses Fish from `nixpkgs-tools`, still fixed by the
lock until an intentional `devenv update nixpkgs-tools` revision change.
Record the exact binaries and locale-sensitive Bash build behavior with
`cabal run monk-tool -- evidence profile`. In particular, iconv configuration
can change invalid Unicode escape behavior even at the same Bash version.
Use targeted updates such as `devenv update haskell-nix`, `devenv update nixpkgs`,
or `devenv update nixpkgs-tools`. A bare `devenv update` also advances
`nixpkgs-reference`; update that input only when deliberately changing the
reference pair and its version assertions in `nix/reference-runtimes.nix`.
The reference input intentionally retains its validated revision when updating
the compiler and tooling inputs; advancing it is a separate semantic-baseline
change.
Update `rust-overlay` separately when changing the Rust toolchain; verify the
new manifest has every configured component and target before changing the
toolchain file. `bash scripts/generate-abi-metadata.sh --check` verifies that
the Haskell and Rust ABI constants match `protocol/abi2.tsv`.
Check IOG cache availability after changing the compiler package set: following
the root nixpkgs does not guarantee it matches haskell.nix's tested revision.
CI reads its standalone devenv bootstrap revision from the `nixpkgs-tools` lock
node so the workflow does not duplicate a commit pin.

Native Fish integration remains the standalone devenv hook. Existing users
of `devenv hook fish | source` can approve this checkout with `devenv allow`;
`devenv --shell fish shell` also selects Fish explicitly. This project does not
provide a second flake development shell.

## Runtime release outputs

Build on each matching native host:

```sh
devenv build outputs.runtime.x86_64-linux
devenv build outputs.runtime.aarch64-linux
devenv build outputs.runtime.aarch64-darwin
```

`outputs.runtime` builds the Cargo runtime from the locked workspace.
Linux packages use a musl package set and static executable linking.
Darwin packages must pass the Apple-system-only dynamic
library check. Every output contains `bin/monk-runtime` and
`share/monk/package-evidence.json`. The build fails if the artifact has the
wrong architecture, a Linux interpreter/DT_NEEDED entry, or a non-Apple Darwin
dynamic dependency. `cabal run monk-tool -- runtime inspect --binary FILE --target TARGET` can repeat inspection
on a copied artifact. Inspection deliberately records `execution_verified:
false`: successful linking is not evidence of native execution outside Nix.
Run the native protocol/transport tests on each copied artifact before making
release claims. Fish and explicit source commands remain deployment dependencies.

## Devenv MCP

The executable and its argument must be separate fields:

```toml
[mcp_servers.devenv]
command = "devenv"
args = ["mcp"]
```

Run it in this checkout. `command = "devenv mcp"` asks the host to find an
executable containing a space and fails. The stdio handshake and tools/list
were verified with `devenv mcp`; it exposes `search_options`,
`search_packages`, and process inspection/control. Global Codex settings are
not changed by this project.

Upstream references:
[devenv inputs](https://devenv.sh/inputs/),
[haskell.nix cache setup](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started.html),
[haskell.nix static cross compilation](https://input-output-hk.github.io/haskell.nix/tutorials/cross-compilation.html).

The checked-in CI uses the same devenv compiler/Fish selectors and native package
outputs. Local workflow lint is distinct from successful remote CI execution.

Release manifests hash the final signed executable and record ABI 2, the
`bash53-i64` profile, architecture, byte size and allowed dynamic dependencies.
Darwin minimum macOS is read from the Mach-O deployment target; running on a
newer host does not verify that minimum. Linux declares a conservative kernel
5.4 release floor for the pinned musl/GHC and POSIX operations, but execution
on that floor remains unverified. Process substitution probes descriptor paths.

Darwin test temporary directories use `/private/tmp`, since publication rejects
symlink ancestors and `/tmp` is a system symlink. The standalone devenv shell
exports this physical path through `enterShell`, including native Fish entry.
