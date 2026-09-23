# Native Rust release package for the three declared ABI 2 targets.
{ inputs, system, buildSystem, src, compiler-nix-name ? "ghc9141" }:
let
  lib = inputs.nixpkgs.lib;
  linux = lib.hasSuffix "-linux" system;
  architecture = lib.removeSuffix "-linux" system;
  overlay = inputs.rust-overlay.overlays.default;
  pkgs = import inputs.nixpkgs ({
    inherit system;
    overlays = [ overlay ];
  } // lib.optionalAttrs linux {
    crossSystem = { config = "${architecture}-unknown-linux-musl"; };
  });
  buildPkgs = import inputs.nixpkgs {
    system = buildSystem;
    overlays = [ overlay ];
  };
  toolchain = buildPkgs.rust-bin.fromRustupToolchainFile ../rust-toolchain.toml;
  rustPlatform = pkgs.makeRustPlatform { cargo = toolchain; rustc = toolchain; };
  runtime = rustPlatform.buildRustPackage {
    pname = "monk-runtime";
    version = "0.4.0";
    inherit src;
    cargoLock.lockFile = ../Cargo.lock;
    cargoBuildFlags = [ "--package" "monk-runtime" ];
    # Runtime behavior is checked with Bash/Fish and the copied artifact in CI.
    doCheck = false;
    RUSTFLAGS = "-Zon-broken-pipe=inherit";
  };
  hostPkgs = import inputs.nixpkgs {
    system = buildSystem;
    inherit (inputs.haskell-nix) config;
    overlays = [ inputs.haskell-nix.overlay ];
  };
  hostProject = import ./project.nix {
    pkgs = hostPkgs;
    inherit src compiler-nix-name;
  };
  verifier = hostProject.hsPkgs.monk.components.exes.monk-tool;
in pkgs.buildPackages.runCommand "monk-rust-runtime-${system}" {
  nativeBuildInputs = [ verifier pkgs.buildPackages.stdenv.cc.bintools ]
    ++ lib.optionals linux [ pkgs.buildPackages.binutils ]
    ++ lib.optionals (!linux) [ pkgs.buildPackages.darwin.sigtool ];
  passthru = { inherit runtime; target = system; };
} ''
  mkdir -p "$out/bin" "$out/share/monk"
  cp ${runtime}/bin/monk-runtime "$out/bin/monk-runtime"
  ${lib.optionalString (!linux) ''
    # Nix's Darwin stdenv resolves -liconv to its store copy. The release
    # binary must use the compatible macOS system library when copied out.
    for dependency in $(otool -L "$out/bin/monk-runtime" | awk 'NR > 1 { print $1 }'); do
      case "$dependency" in
        /nix/store/*/lib/libiconv.2.dylib)
          install_name_tool -change "$dependency" /usr/lib/libiconv.2.dylib "$out/bin/monk-runtime"
          ;;
      esac
    done
    codesign --force --sign - "$out/bin/monk-runtime"
  ''}
  monk-tool runtime inspect --binary "$out/bin/monk-runtime" \
    --target ${system} --report "$out/share/monk/package-evidence.json" >/dev/null
''
