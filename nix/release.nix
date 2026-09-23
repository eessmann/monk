# Evaluated by devenv build outputs.runtime.<target>. The runtime may be
# cross-compiled, but the inspector always runs on the machine doing the build.
{ inputs, system, buildSystem, src, compiler-nix-name ? "ghc9141" }:
let
  lib = inputs.nixpkgs.lib;
  linux = lib.hasSuffix "-linux" system;
  architecture = lib.removeSuffix "-linux" system;
  pkgs = import inputs.nixpkgs ({
    inherit system;
    inherit (inputs.haskell-nix) config;
    overlays = [ inputs.haskell-nix.overlay ];
  } // lib.optionalAttrs linux {
    crossSystem = { config = "${architecture}-unknown-linux-musl"; };
  });
  hostPkgs = import inputs.nixpkgs {
    system = buildSystem;
    inherit (inputs.haskell-nix) config;
    overlays = [ inputs.haskell-nix.overlay ];
  };
  project = import ./project.nix {
    inherit pkgs src compiler-nix-name;
    release = true;
  };
  hostProject = import ./project.nix {
    pkgs = hostPkgs;
    inherit src compiler-nix-name;
  };
  runtime = project.hsPkgs.monk.components.exes.monk-runtime;
  verifier = hostProject.hsPkgs.monk.components.exes.monk-tool;
in pkgs.buildPackages.runCommand "monk-runtime-${system}" {
  nativeBuildInputs = [ verifier pkgs.buildPackages.stdenv.cc.bintools ]
    ++ lib.optionals linux [ pkgs.buildPackages.binutils ]
    ++ lib.optionals (!linux) [ pkgs.buildPackages.darwin.sigtool ];
  passthru = { inherit runtime; target = system; };
} ''
  mkdir -p "$out/bin" "$out/share/monk"
  cp ${runtime}/bin/monk-runtime "$out/bin/monk-runtime"
  ${lib.optionalString (!linux) ''
    # These Nix libraries are Apple's ABI-compatible system libraries. Keep
    # system dependencies system-relative; every other dylib remains an error.
    install_name_tool \
      -change ${pkgs.libffi}/lib/libffi.7.dylib /usr/lib/libffi.dylib \
      -change ${pkgs.libiconv}/lib/libiconv.2.dylib /usr/lib/libiconv.2.dylib \
      "$out/bin/monk-runtime"
    codesign --force --sign - "$out/bin/monk-runtime"
  ''}
  monk-tool runtime inspect --binary "$out/bin/monk-runtime" \
    --target ${system} --report "$out/share/monk/package-evidence.json" >/dev/null
''
