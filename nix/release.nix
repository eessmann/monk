# Evaluated by devenv build outputs.runtime.<target>. Build on the target host.
{ inputs, system, src, compiler-nix-name ? "ghc9141" }:
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
  project = import ./project.nix {
    inherit pkgs src compiler-nix-name;
    release = true;
  };
  runtime = project.hsPkgs.monk.components.exes.monk-runtime;
in pkgs.buildPackages.runCommand "monk-runtime-${system}" {
  nativeBuildInputs = [ pkgs.buildPackages.python3 pkgs.buildPackages.stdenv.cc.bintools ]
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
  python3 ${../scripts/verify-runtime-package.py} "$out/bin/monk-runtime" \
    --target ${system} --report "$out/share/monk/package-evidence.json"
''
