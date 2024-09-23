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

  pc-contracts-cli = pkgs.writeShellApplication {
    name = "pc-contracts-cli";
    runtimeInputs = [ pkgs.nodejs-18_x ];
    text = ''
      ${pkgs.nodejs-18_x}/bin/node --enable-source-maps -e 'import("${offchain.compiled}/output/Main/index.js").then(m => m.main())' pc-contracts-cli "$@"
    '';
  };
  pc-contracts-cli-bundled = offchain.bundled;
  pc-contracts-release-bundle = pkgs.runCommand "bundled-cli" { buildInputs = [ pkgs.zip ]; } ''
    cp -R ${pc-contracts-cli-bundled}/node_modules .
    chmod -R u+rw ./node_modules
    cp ${pc-contracts-cli-bundled}/pc-contracts-cli ./pc-contracts-cli
    mkdir -p $out
    zip -r $out/release.zip  ./node_modules ./pc-contracts-cli
  '';
  kupo = pkgs.callPackage ./kupo.nix { };
  ogmios = pkgs.callPackage ./ogmios.nix { };
  spago2nix = pkgs.callPackage ./spago2nix.nix { };
}
