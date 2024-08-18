{ system ? builtins.currentSystem
, pkgs ? import ./nix { inherit system; }
}:

{
  offchain = pkgs.callPackage ./offchain {};
  onchain = pkgs.callPackage ./onchain {};
}
