{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/d1c3fea7ecbed758168787fe4e4a3157e52bc808.tar.gz") { }
}:
let
  spagoPkgs = import ../spago-packages.nix { inherit pkgs; };
in
pkgs.stdenv.mkDerivation rec {
  pname = "ctl-scripts";
  version = "0.1.0";

  src = ./..;

  # based on https://discourse.purescript.org/t/spago2nix-any-complete-example-to-look-at/2532/2
  buildInputs = [
    pkgs.spago
    pkgs.purescript
    pkgs.nodejs
    spagoPkgs.installSpagoStyle
    spagoPkgs.buildSpagoStyle
    spagoPkgs.buildFromNixStore
  ];

  unpackPhase = ''
    cp $src/{packages,spago}.dhall .
    cp -r $src/src .
    cp -r $src/package.json $src/package-lock.json .
    chmod +rw ./*.json
    install-spago-style
  '';

  buildPhase = ''
    export HOME=$(pwd)
    npm install --prefix $(pwd)

    build-spago-style ./src/*.purs ./src/*/*.purs
    spago bundle-app --no-build --no-install --global-cache skip
    mv index.js main.js
  '';

  installPhase = ''
    mkdir -p $out
    tar cvf $out/ctl-scripts-${version}.tar main.js node_modules
  '';
}
