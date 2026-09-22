# Both the development shell and release packages consume cabal.project.
{ pkgs, src, compiler-nix-name ? "ghc9141", release ? false }:
pkgs.haskell-nix.cabalProject' {
  inherit src compiler-nix-name;
  modules = [
    ({ lib, ... }: {
      packages.monk.flags.development = true;
      packages.monk.components.exes.monk-runtime = lib.mkIf release {
        enableShared = false;
        enableStatic = true;
        enableExecutableDynamic = false;
        configureFlags = lib.optionals pkgs.stdenv.hostPlatform.isMusl [
          "--ghc-option=-optl=-static"
          "--ghc-option=-optl=-pthread"
          "--ghc-option=-optl=-L${pkgs.gmp.override { withStatic = true; }}/lib"
          "--ghc-option=-optl=-L${pkgs.libffi.overrideAttrs (_: { dontDisableStatic = true; })}/lib"
        ] ++ lib.optionals pkgs.stdenv.hostPlatform.isDarwin [
          # Load GMP's archive before GHC's implicit -lgmp, then discard the
          # now-unused dylib. Apple libffi/libiconv are normalized at packaging.
          "--ghc-option=-optl=-Wl,-force_load,${pkgs.gmp.override { withStatic = true; }}/lib/libgmp.a"
          "--ghc-option=-optl=-Wl,-dead_strip_dylibs"
        ];
      };
    })
  ];
}
