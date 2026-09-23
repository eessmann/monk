# Haskell compiler and host tooling project; the native runtime is built by Cargo.
{ pkgs, src, compiler-nix-name ? "ghc9141" }:
pkgs.haskell-nix.cabalProject' {
  inherit src compiler-nix-name;
  modules = [
    ({ ... }: {
      packages.monk.flags.development = true;
    })
  ];
}
