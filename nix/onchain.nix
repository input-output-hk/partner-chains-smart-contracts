{ repoRoot
, inputs
, pkgs
, lib
, system
,
}:
let
  cabalProject' = pkgs.haskell-nix.cabalProject' ({ config
                                                  , pkgs
                                                  , ...
                                                  }:
    let
      # Only a limited subset of components can be cross-compiled on windows.
      # When `isCross` is `true`, it means that we are cross-compiling the project.
      isCross = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;
    in
    {
      name = "trustless-sidechain";

      src = lib.cleanSource ../onchain;

      compiler-nix-name = "ghc96";

      flake.variants.profiled.modules = [
        {
          enableProfiling = true;
          enableLibraryProfiling = true;
        }
      ];

      shell.withHoogle = false;

      inputMap = {
        "https://input-output-hk.github.io/cardano-haskell-packages" =
          inputs.CHaP;
      };

      modules = [
        {
          packages = {
            trustless-sidechain.doHaddock = false;
            trustless-sidechain-prelude.doHaddock = false;
            onchain-poc.doHaddock = false;
            onchain-poc.flags.defer-plugin-errors = false;
            onchain-poc.preCheck = ''
              export CARDANO_CLI=${inputs.cardano-node.legacyPackages.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}
              export CARDANO_NODE=${inputs.cardano-node.legacyPackages.cardano-node}/bin/cardano-node${pkgs.stdenv.hostPlatform.extensions.executable}
            '';
            trustless-sidechain.ghcOptions = [ "-Werror" ];
            onchain-poc.ghcOptions = [ "-Werror" ];
            trustless-sidechain-prelude.ghcOptions = [ "-Werror" ];
          };
        }
      ];
    });

  cabalProject = cabalProject';

  onchain = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    shellArgs = repoRoot.nix.shell;
  };
in
onchain
# haskell

