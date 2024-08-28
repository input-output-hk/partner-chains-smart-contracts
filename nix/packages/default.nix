{ inputs
, repoRoot
, pkgs
, system
, ...
}:
let
  flake-compat = import inputs.flake-compat;
  cardanoPackages = (flake-compat { src = inputs.cardano-node; }).defaultNix.packages.${system};
  project = repoRoot.nix.offchain;
in
rec {
  inherit (cardanoPackages) cardano-node cardano-cli cardano-testnet;

  sidechain-main-cli = pkgs.writeShellApplication {
    name = "sidechain-main-cli";
    runtimeInputs = [ pkgs.nodejs-18_x ];
    text = ''
      ${pkgs.nodejs-18_x}/bin/node --enable-source-maps -e 'import("${project.compiled}/output/Main/index.js").then(m => m.main())' sidechain-main-cli "$@"
    '';
  };
  sidechain-main-cli-bundled = project.bundled;
  sidechain-release-bundle = pkgs.runCommand "bundled-cli" { buildInputs = [ pkgs.zip ]; } ''
    cp -R ${sidechain-main-cli-bundled}/node_modules .
    chmod -R u+rw ./node_modules
    cp ${sidechain-main-cli-bundled}/sidechain-main-cli ./sidechain-cli
    mkdir -p $out
    zip -r $out/release.zip  ./node_modules ./sidechain-cli
  '';
  kupo = pkgs.callPackage ./kupo.nix { };
  ogmios = pkgs.callPackage ./ogmios.nix { };
  spago2nix = pkgs.callPackage ./spago2nix.nix { };
}
