{ repoRoot, inputs, pkgs, lib, system }:

cabalProject:

let
  cardano-cli = inputs.cardano-node.legacyPackages.cardano-cli;
  cardano-node = inputs.cardano-node.legacyPackages.cardano-node;

in
{
  name = "trustless-sidechain";
  packages = [
    cardano-cli
    cardano-node
    pkgs.ghcid
    pkgs.curl
    pkgs.haskellPackages.hoogle
  ];

  env = {
    CARDANO_CLI = "${cardano-cli}/bin/cardano-cli";
    CARDANO_NODE = "${cardano-node}/bin/cardano-node";
  };


  preCommit = {
    fourmolu.enable = false;
    shellcheck.enable = false;
    cabal-fmt.enable = true;
    nixpkgs-fmt.enable = true;
  };
}
