{ inputs
, pkgs
, offchain
, ...
}:
let
  flake-compat = import inputs.flake-compat;
  cardanoPackages = (flake-compat { src = inputs.cardano-node; }).defaultNix.packages.${pkgs.system};
in
rec {
  inherit (cardanoPackages) cardano-node cardano-cli cardano-testnet;

  default = pc-contracts-cli;
  pc-contracts-cli = offchain.bundled;
  pc-contracts-release-bundle = pkgs.runCommand "bundled-cli" { buildInputs = [ pkgs.zip ]; } ''
    cp -r ${pc-contracts-cli}/* ./
    mkdir -p $out
    zip -r $out/release.zip ./package.json ./node_modules README.md
    zip -j $out/release.zip ./dist/pc-contracts-cli
  '';
  kupo = pkgs.callPackage ./kupo.nix { };
  ogmios = pkgs.callPackage ./ogmios.nix { };
  spago2nix = pkgs.callPackage ./spago2nix.nix { };
}
