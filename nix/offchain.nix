{ inputs
, self
, pkgs
, ...
}:
let
  inherit (pkgs) lib nodejs-18_x stdenv;
  inherit (lib) fileset;

  src = fileset.toSource {
    root = ../offchain;
    fileset = (fileset.unions [
      ../offchain/package.json
      ../offchain/package-lock.json
      ../offchain/esbuild
      ../offchain/README.md
      ../offchain/src
      ../offchain/test
      ../offchain/config.example.json
    ]);
  };

  npmlockToNix = pkgs.callPackage inputs.npmlock2nix { };

  nodeModules = npmlockToNix.v2.node_modules {
    src = fileset.toSource {
      root = ../offchain;
      fileset = fileset.union ../offchain/package.json ../offchain/package-lock.json;
    };
    nodejs = nodejs-18_x;
    sourceOverrides = {
      buildRequirePatchShebangs = true;
      node-pre-gyp = npmlockToNix.v2.packageRequirePatchShebangs;
    };
    ESBUILD_BINARY_PATH = "${pkgs.esbuild}/bin/esbuild";
  };

  # Purescript project output
  compiled = stdenv.mkDerivation {
    inherit src;
    name = "pc-contracts-cli";

    dontPatchShebangs = true;

    buildInputs = with (import ../offchain/spago-packages.nix { inherit pkgs; }); [
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
      cp -r $src/* .
      chmod u+rw ./package.json package-lock.json
      cp -R ${nodeModules}/node_modules node_modules
      chmod -R u+rw ./node_modules
      chmod -R +w src
      install-spago-style
    '';
    buildPhase = ''
      psa --censor-lib \
          --stash \
          --strict \
          --is-lib=.spago ".spago/*/*/src/**/*.purs" \
          --censor-codes=UserDefinedWarning "./src/**/*.purs" "./test/**/*.purs" \
          -gsourcemaps,js
    '';
    installPhase = ''
      mkdir -p $out
      cp -r output .spago $out/
      cp package.json $out/output/
      ln -s ${nodeModules}/node_modules $out/node_modules
    '';
  };

  # Contains node_modules and cli only
  bundled = stdenv.mkDerivation {
    inherit src;
    name = "pc-contracts-cli-bundled";
    unpackPhase = ''
      cp -r $src/* .
      chmod u+rw ./package.json package-lock.json
      cp -R ${nodeModules}/node_modules node_modules
      cp -R ${compiled}/{output,.spago} .
      chmod -R u+rw ./node_modules
    '';
    nativeBuildInputs = [
      pkgs.nodejs-18_x
      pkgs.spago
    ];
    buildInputs = [
      pkgs.fd
    ];
    buildPhase = ''
      mkdir -p dist/
      echo 'import("../output/Main/index.js").then(m => m.main());' > ./dist/entrypoint.js
      node ./esbuild/bundle.js ./dist/entrypoint.js dist/pc-contracts-cli --loglevel=verbose
    '';
    installPhase = ''
      mkdir -p $out/dist
      cp dist/pc-contracts-cli $out/dist
      cp package.json README.md $out/
      chmod +x $out/dist/pc-contracts-cli
      npm prune --offline --cache node_modules --omit=dev --ignore-scripts
      cp -R node_modules $out
    '';
  };
in
{
  inherit compiled bundled src nodeModules;

}
