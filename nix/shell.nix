{
  repoRoot,
  inputs,
  pkgs,
  lib,
  system,
}: cabalProject: let
  cardano-cli = inputs.cardano-node.legacyPackages.cardano-cli;
  cardano-node = inputs.cardano-node.legacyPackages.cardano-node;
in {
  name = "trustless-sidechain";
  welcomeMessage = "Welcome to the Trustless Sidechain shell";
  packages = [
    cardano-cli
    cardano-node
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

  /*
    *
  **
  TODO
  All precommits checks are off for now to narrow the scope of code-change.
  Future PRs will address the formatting strategy.
  For now, we use the pre-existing formatting enforcement strategy.
  **
  */
  preCommit = {
    fourmolu.enable = false;
    shellcheck.enable = false;
    cabal-fmt.enable = false;
    optipng.enable = false;
    nixpkgs-fmt.enable = false;
  };
}
