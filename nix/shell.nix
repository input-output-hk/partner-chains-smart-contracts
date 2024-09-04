{ repoRoot
, inputs
, pkgs
, lib
, system
,
}: cabalProject:
let
  cardano-cli = inputs.self.packages.cardano-cli;
  cardano-node = inputs.self.packages.cardano-node;
  cardano-testnet = inputs.self.packages.cardano-testnet;
in
{
  name = "partner-chains-smart-contracts";
  welcomeMessage = "Partner Chains Smart Contracts shell";
  packages = [
    cardano-cli
    cardano-node
    cardano-testnet
    pkgs.ghcid
    pkgs.curl
    pkgs.nixci
    pkgs.haskellPackages.hoogle
    pkgs.watchexec
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
