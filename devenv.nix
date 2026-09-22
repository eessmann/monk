{ pkgs, lib, config, inputs, ... }:

{
  packages = [
    pkgs.m4
    pkgs.gmp
    pkgs.fish
    pkgs.bash
    pkgs.babelfish
    pkgs.cabal2nix    
  ];

  languages.haskell = {
    enable = true;
    
    # Explicitly set the GHC version to 9.12
    package = pkgs.haskell.packages.ghc912.ghc;
    
    # Optional: Automatically configures stack/cabal to use this compiler
    cabal.enable = true; 
  };
  
}
