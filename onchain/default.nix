{ system ? builtins.currentSystem
, pkgs ? import ../nix { inherit system; }
}:

pkgs.haskell-nix.cabalProject' {
    src = pkgs.lib.cleanSource ./.;
    compiler-nix-name = "ghc96";
    inputMap = {
      "https://input-output-hk.github.io/cardano-haskell-packages" = pkgs.CHaP;
    };
    modules = [
      {
        packages = {
          onchain-poc = {
            doHaddock = false;
            flags.defer-plugin-errors = false;
            ghcOptions = [ "-Werror" ];
          };
          trustless-sidechain-prelude = {
            doHaddock = false;
            ghcOptions = [ "-Werror" ];
          };
          trustless-sidechain = {
            doHaddock = false;
            ghcOptions = [ "-Werror" ];
          };
        };
      }
    ];
  }
