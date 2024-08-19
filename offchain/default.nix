{ system ? builtins.currentSystem
, pkgs ? import ../nix { inherit system; }
, spagoPkgs ? import ./spago-packages.nix { inherit pkgs; }
}:

let

  inherit (pkgs) npmlockToNix lib nodejs-18_x stdenv;
  inherit (lib) fileset;

  cliScript = pkgs.writeScript "sidechain-mai-cli" ''
    #!/usr/bin/env bash
    SCRIPT_DIR="$(cd "$(dirname "$(readlink -f "$0")")" && pwd)"
    export NODE_PATH="$SCRIPT_DIR/node_modules"

    if ! command -v node &>/dev/null; then
      echo "Error: nodejs binary not found in path! Exiting."
      exit 1
    fi

    command -v node &>/dev/null || echo "Error: node binary not found in path!"
    node "$SCRIPT_DIR/.index.mjs" "$@"
  '';

  nodeModules = npmlockToNix.v2.node_modules {
    src = fileset.toSource {
      root = ./.;
      fileset = fileset.union ./package.json ./package-lock.json;
    };
    nodejs = nodejs-18_x;
    sourceOverrides = {
      buildRequirePatchShebangs = true;
      node-pre-gyp = npmlockToNix.v2.packageRequirePatchShebangs;
    };
    ESBUILD_BINARY_PATH = "${pkgs.esbuild}/bin/esbuild";
  };

  cli = stdenv.mkDerivation {
    name = "cli";

    src = fileset.toSource {
      root = ./.;
      fileset = (fileset.unions [
        ./package.json
        ./package-lock.json
        ./entry.js
        ./esbuild.js
        ./src
        ./test
      ]);
    };

    dontPatchShebangs = true;

    buildInputs = with spagoPkgs; [
      installSpagoStyle
      buildSpagoStyle
    ];

    nativeBuildInputs = with pkgs; [
      purescript-psa
      purs
      spago
      nodejs-18_x
    ];

    unpackPhase = ''
      cp $src/entry.js .
      cp $src/package.json .
      cp $src/package-lock.json .
      cp -r $src/src .
      cp -r $src/test .
      cp $src/esbuild.js .
      ln -sfn ${nodeModules}/node_modules node_modules
      install-spago-style
    '';

    buildPhase = ''
      psa --censor-lib \
          --stash \
          --strict \
          --is-lib=.spago ".spago/*/*/src/**/*.purs" \
          --censor-codes=UserDefinedWarning "./src/**/*.purs" "./test/**/*.purs"

      node ./esbuild.js
    '';

    installPhase = ''
      mkdir -p $out
      cp dist/index.js $out/.index.mjs
      cp -R node_modules $out
      cp ${cliScript} $out/sidechain-main-cli
      chmod +x $out/sidechain-main-cli
    '';

  };

in
cli
