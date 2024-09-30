inputs @ { self
, pkgs
, ...
}:
let
  onchain = (import ./onchain.nix { inherit inputs pkgs; });
  offchain = import ./offchain.nix { inherit inputs self pkgs; };
in
pkgs.lib.recursiveUpdate onchain {
  inherit onchain;
  apps = rec {
    default = pc-contracts-cli;
    pc-contracts-cli = {
      type = "app";
      program = "${self.packages.pc-contracts-cli}/bin/pc-contracts-cli";
    };
  };
  devShells = import ./shells.nix { inherit inputs self pkgs; };
  packages = import ./packages { inherit inputs pkgs; inherit offchain; };

  # as 'check' isn't a default flake output, this prevents these from being built by the CI
  check = import ./checks.nix { inherit inputs self pkgs; inherit offchain; };

}
