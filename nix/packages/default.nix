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

  pc-contracts-cli = offchain.bundled;
  pc-contracts-release-bundle = pkgs.runCommand "bundled-cli" { buildInputs = [ pkgs.zip ]; } ''
    cp -r ${pc-contracts-cli}/* ./
    mkdir -p $out
    zip -r $out/release.zip  ./{package.json,node_modules,dist,README.md}
  '';
  kupo = pkgs.callPackage ./kupo.nix { };
  ogmios = pkgs.callPackage ./ogmios.nix { };
  spago2nix = pkgs.callPackage ./spago2nix.nix { };
}
