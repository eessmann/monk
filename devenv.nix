{ pkgs, lib, inputs, config, ... }:
let
  # Match haskell.nix's tested nixpkgs and overlay to use the IOG cache.
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
    # inputsFrom merges packages, not the shell derivation's environment.
    env = {
      inherit (haskellShell) CABAL_CONFIG NIX_GHC NIX_GHCPKG NIX_GHC_LIBDIR NIX_GHC_DOCDIR;
    };
    packages = (with tools; [ cabal-install hlint ormolu python3 git babelfish coreutils m4 cabal2nix ])
      ++ [ hp.gmp (lib.hiPrio reference.bash)
        (lib.hiPrio (if config.monk.fishChannel == "pinned" then reference.fish else tools.fish)) ];
    env.LC_ALL = "C";
    env.LANG = "C";
    # Publication rejects symlink ancestors; Darwin /tmp points at /private/tmp.
    enterShell = lib.mkAfter ''
      export TMPDIR=${if pkgs.stdenv.isDarwin then "/private/tmp" else "/tmp"}
    '';
    scripts.monk-build.exec = "cabal build all";
    scripts.monk-test.exec = ''
      set -euo pipefail
      cabal test all --test-show-details=direct
      python3 scripts/test_runtime_package.py
      python3 scripts/test_portable_comparison.py
      python3 scripts/test_portable_performance.py
      python3 scripts/test_verification_evidence.py
    '';
    scripts.monk-integration.exec = ''
      set -euo pipefail
      cabal build all
      export PATH="$(dirname "$(cabal list-bin exe:monk-runtime)"):$PATH"
      MONK_INTEGRATION=1 cabal test all --test-show-details=direct
      python3 runtime-test/protocol.py "$(cabal list-bin exe:monk-runtime)"
      python3 runtime-test/portable.py "$(cabal list-bin exe:monk-runtime)"
      python3 runtime-test/printf.py "$(cabal list-bin exe:monk-runtime)"
      python3 runtime-test/expansion.py "$(cabal list-bin exe:monk-runtime)"
      python3 runtime-test/session.py "$(cabal list-bin exe:monk-runtime)"
      for suite in descriptors read process-substitution pattern-parts exec signals; do
        python3 "runtime-test/$suite.py" "$(cabal list-bin exe:monk-runtime)"
      done
      python3 runtime-test/digest.py "$(cabal list-bin test:runtime-test)"
      python3 runtime-test/callback-diagnostics.py "$(cabal list-bin exe:monk-runtime)" "$(cabal list-bin exe:monk)"
      python3 runtime-test/native-launcher.py "$(cabal list-bin exe:monk-runtime)" "$(cabal list-bin exe:monk)"
      python3 runtime-test/direct-output.py "$(cabal list-bin exe:monk-runtime)" "$(cabal list-bin exe:monk)"
      python3 runtime-test/directory-signals.py "$(cabal list-bin exe:monk)" "$(cabal list-bin exe:monk-runtime)"
      bash test/native/child-transport.sh "$(cabal list-bin exe:monk-runtime)"
      MONK_NATIVE_TEST_BINARY="$(cabal list-bin exe:monk)" \
        MONK_NATIVE_RUNTIME="$(cabal list-bin exe:monk-runtime)" \
        python3 scripts/test_native_publication.py
      cabal exec -- python3 scripts/check-public-boundaries.py --report artifacts/public-boundaries.json
      python3 scripts/test_runtime_package.py
      python3 scripts/test_portable_comparison.py
      python3 scripts/test_portable_performance.py
      python3 scripts/test_verification_evidence.py
    '';
    scripts.monk-quality.exec = ''
      set -euo pipefail
      hlint .
      git ls-files --cached --others --exclude-standard -z -- '*.hs' | xargs -0 ormolu --mode check
      cabal check
    '';
    scripts.monk-benchmark.exec = "cabal bench monk-benchmark";
    scripts.monk-docs.exec = "cabal haddock all";
    scripts.monk-sdist.exec = "bash scripts/check-source-distribution.sh artifacts/sdist";
    tasks = {
      "monk:build".exec = config.scripts.monk-build.exec;
      "monk:test".exec = config.scripts.monk-test.exec;
      "monk:integration".exec = config.scripts.monk-integration.exec;
      "monk:quality".exec = config.scripts.monk-quality.exec;
      "monk:benchmark".exec = config.scripts.monk-benchmark.exec;
      "monk:docs".exec = config.scripts.monk-docs.exec;
      "monk:sdist".exec = config.scripts.monk-sdist.exec;
    };
    outputs.reference = reference;
    outputs.testPrograms = { python = tools.python3; coreutils = tools.coreutils; };
    outputs.compiler = hp.haskell-nix.compiler.${config.monk.compiler};
    outputs.runtime = lib.genAttrs [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ]
      (system: import ./nix/release.nix {
        inherit inputs system;
        src = import ./nix/source.nix { inherit lib; };
        compiler-nix-name = config.monk.compiler;
      });
    scripts.monk-package.exec = "devenv build outputs.runtime.${pkgs.stdenv.hostPlatform.system}";
    tasks."monk:package".exec = config.scripts.monk-package.exec;
  };
}
