{ pkgs, lib, inputs, config, ... }:
let
  # Share the lock-resolved root nixpkgs with haskell.nix's overlay.
  hp = import inputs.nixpkgs {
    system = pkgs.stdenv.hostPlatform.system;
    inherit (inputs.haskell-nix) config;
    overlays = [ inputs.haskell-nix.overlay ];
  };
  tools = import inputs.nixpkgs-tools { system = pkgs.stdenv.hostPlatform.system; };
  referencePkgs = import inputs.nixpkgs-reference { system = pkgs.stdenv.hostPlatform.system; };
  reference = import ./nix/reference-runtimes.nix { inherit referencePkgs; };
  project = import ./nix/project.nix {
    pkgs = hp;
    src = import ./nix/source.nix { inherit lib; };
    compiler-nix-name = config.monk.compiler;
  };
  haskellShell = project.shellFor { withHoogle = false; exactDeps = true; };
in {
  options.monk.fishChannel = lib.mkOption {
    type = lib.types.enum [ "pinned" "moving" ];
    default = "pinned";
    description = "Reference Fish 4.6.0 or the moving version locked by nixpkgs-tools.";
  };
  options.monk.compiler = lib.mkOption {
    type = lib.types.enum [ "ghc9141" "ghc9122" ];
    default = "ghc9141";
    description = "The pinned haskell.nix compiler; ghc9122 is the compatibility lane.";
  };
  config = {
    inputsFrom = [ haskellShell ];
    languages.rust = {
      enable = true;
      toolchainFile = ./rust-toolchain.toml;
    };
    # inputsFrom merges packages, not the shell derivation's environment.
    env = {
      inherit (haskellShell) CABAL_CONFIG NIX_GHC NIX_GHCPKG NIX_GHC_LIBDIR NIX_GHC_DOCDIR;
    };
    packages = (with tools; [ cabal-install hlint ormolu git babelfish coreutils m4 cabal2nix nil ])
      ++ [ hp.gmp (lib.hiPrio reference.bash)
        (lib.hiPrio (if config.monk.fishChannel == "pinned" then reference.fish else tools.fish)) ];
    env.LC_ALL = "C";
    env.LANG = "C";
    env.MONK_REFERENCE_BASH = "${reference.bash}/bin/bash";
    # Publication rejects symlink ancestors; Darwin /tmp points at /private/tmp.
    enterShell = lib.mkAfter ''
      export TMPDIR=${if pkgs.stdenv.isDarwin then "/private/tmp" else "/tmp"}
      export PATH="$PWD/target/debug:$PATH"
    '';
    scripts.monk-build.exec = ''
      set -euo pipefail
      cargo build --locked --package monk-runtime
      cabal build all
    '';
    scripts.monk-rust-build.exec = "cargo build --locked --package monk-runtime";
    scripts.monk-rust-test.exec = "cargo test --locked --package monk-runtime";
    scripts.monk-rust-quality.exec = ''
      set -euo pipefail
      cargo fmt --all --check
      cargo clippy --locked --workspace --all-targets -- -D warnings
    '';
    scripts.monk-test.exec = ''
      set -euo pipefail
      cargo test --locked --package monk-runtime
      cargo build --locked --package monk-runtime
      cabal build all
      cabal test all --test-show-details=direct
    '';
    scripts.monk-integration.exec = ''
      set -euo pipefail
      cargo test --locked --package monk-runtime
      cargo build --locked --package monk-runtime
      cabal build all
      runtime="$PWD/target/debug/monk-runtime"
      monk="$(cabal list-bin exe:monk)"
      tool="$(cabal list-bin exe:monk-tool)"
      export PATH="$PWD/target/debug:$PATH"
      MONK_INTEGRATION=1 cabal test all --test-show-details=direct
      "$tool" runtime check --all --runtime "$runtime" --monk "$monk"
      "$tool" runtime check --suite digest --runtime "$(cabal list-bin test:compiler-support-test)"
      "$tool" boundaries check --report artifacts/public-boundaries.json
    '';
    scripts.monk-quality.exec = ''
      set -euo pipefail
      bash scripts/generate-abi-metadata.sh --check
      sources=()
      while IFS= read -r -d "" source; do
        if test -f "$source"; then sources+=("$source"); fi
      done < <(git ls-files --cached --others --exclude-standard -z -- '*.hs')
      hlint "''${sources[@]}"
      ormolu --mode check "''${sources[@]}"
      cabal check
      bash scripts/check-tooling-language.sh
    '';
    scripts.monk-benchmark.exec = "cabal bench monk-benchmark";
    scripts.monk-docs.exec = "cabal haddock all";
    scripts.monk-sdist.exec = "bash scripts/check-source-distribution.sh artifacts/sdist";
    tasks = {
      "monk:build".exec = config.scripts.monk-build.exec;
      "monk:rust-build".exec = config.scripts.monk-rust-build.exec;
      "monk:rust-test".exec = config.scripts.monk-rust-test.exec;
      "monk:rust-quality".exec = config.scripts.monk-rust-quality.exec;
      "monk:test".exec = config.scripts.monk-test.exec;
      "monk:integration".exec = config.scripts.monk-integration.exec;
      "monk:quality".exec = config.scripts.monk-quality.exec;
      "monk:benchmark".exec = config.scripts.monk-benchmark.exec;
      "monk:docs".exec = config.scripts.monk-docs.exec;
      "monk:sdist".exec = config.scripts.monk-sdist.exec;
    };
    outputs.reference = reference;
    outputs.testPrograms = { coreutils = tools.coreutils; };
    outputs.tooling = project.hsPkgs.monk.components.exes.monk-tool;
    outputs.translator = project.hsPkgs.monk.components.exes.monk;
    outputs.compiler = hp.haskell-nix.compiler.${config.monk.compiler};
    outputs.runtime = lib.genAttrs [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ]
      (system: import ./nix/release.nix {
        inherit inputs system;
        buildSystem = pkgs.stdenv.buildPlatform.system;
        src = import ./nix/source.nix { inherit lib; };
        compiler-nix-name = config.monk.compiler;
      });
    scripts.monk-package.exec = "devenv build outputs.runtime.${pkgs.stdenv.hostPlatform.system}";
    tasks."monk:package".exec = config.scripts.monk-package.exec;
  };
}
