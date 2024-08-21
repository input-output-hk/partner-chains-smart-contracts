{ repoRoot
, inputs
, pkgs
, lib
, system
,
}: cabalProject:
let
  cardano-cli = inputs.self._packages.cardano-cli;
  cardano-node = inputs.self._packages.cardano-node;
  cardano-testnet = inputs.self._packages.cardano-testnet;
in
{
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
    pkgs.spago
    pkgs.purescript
  ];
  env = {
    CARDANO_CLI = "${cardano-cli}/bin/cardano-cli";
    CARDANO_NODE = "${cardano-node}/bin/cardano-node";
  };


  preCommit = {
    fourmolu.enable = true;
    shellcheck.enable = true;
    cabal-fmt.enable = true;
    optipng.enable = true;
    nixpkgs-fmt.enable = true;
    purs-tidy.enable = true;
  };
}
