{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            fd
            ghc
            libsodium
            blst
            secp256k1
            cabal-install
            pkg-config
            zlib
            dhall
            esbuild
            gnumake
            nodejs
            purescript
            purescript-psa
          ];
        };
      });
}
