{ pkgs ? import
    (fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/d1c3fea7ecbed758168787fe4e4a3157e52bc808.tar.gz";
      sha256 = "0ykm15a690v8lcqf2j899za3j6hak1rm3xixdxsx33nz7n3swsyy";
    })
    { }
, easy-ps ? import
    (fetchTarball {
      url = "https://github.com/justinwoo/easy-purescript-nix/archive/ddd2ded8d37ab5d3013f353ca3b6ee05eb23d5c0.tar.gz";
      sha256 = "1i7zqda52npklj3d7pq80zw5rfjjzdqpl5bdrsp6vchg5frgj6ky";
    })
    { inherit pkgs; }
, purs ? easy-ps.purs-0_14_5
, nodejs ? pkgs.nodejs-14_x
, spagoPkgs ? import ../ctl/spago-packages.nix { inherit pkgs; }
}:
pkgs.stdenv.mkDerivation rec {
  pname = "ctl-bundle-cli";
  version = "0.1.0";

  src = ../ctl;

  # based on https://discourse.purescript.org/t/spago2nix-any-complete-example-to-look-at/2532/2
  buildInputs = [
    purs
    nodejs
    easy-ps.spago
    spagoPkgs.installSpagoStyle
    spagoPkgs.buildSpagoStyle
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
