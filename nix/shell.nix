{
  repoRoot,
  inputs,
  pkgs,
  lib,
  system,
}: cabalProject: let
  cardano-cli = inputs.cardano-node.packages.${system}.cardano-cli;
  cardano-node = inputs.cardano-node.packages.${system}.cardano-node;
  cardano-testnet = inputs.cardano-node.packages.${system}.cardano-testnet;
in {
  name = "trustless-sidechain";
  welcomeMessage = "Welcome to the Trustless Sidechain shell";
  packages = [
    cardano-cli
    cardano-node
    cardano-testnet
    pkgs.ghcid
    pkgs.curl
    pkgs.nixci
    pkgs.haskellPackages.hoogle
    pkgs.watchexec
  ];
  env = {
    CARDANO_CLI = "${cardano-cli}/bin/cardano-cli";
    CARDANO_NODE = "${cardano-node}/bin/cardano-node";
  };

  preCommit = {
    fourmolu.enable = false;
    shellcheck.enable = false;
    cabal-fmt.enable = false;
    optipng.enable = false;
    nixpkgs-fmt.enable = false;
  };
}
