{ repoRoot
, inputs
, pkgs
, lib
, system
,
}:
let
  inherit (pkgs) lib nodejs-18_x stdenv;
  inherit (lib) fileset;

  src = fileset.toSource {
    root = ../offchain;
    fileset = (fileset.unions [
      ../offchain/package.json
      ../offchain/package-lock.json
      ../offchain/entry.js
      ../offchain/esbuild.js
      ../offchain/set_version.sh
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
      inputs.nixpkgs.legacyPackages.spago
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
    # Add a patchPhase for patching in the CLI version from a script
    patchPhase =
      let
        rev =
          if builtins.hasAttr "rev" inputs.self
          then inputs.self.rev
          else inputs.self.dirtyRev;
      in
      ''
        substituteInPlace set_version.sh \
          --replace 'jq' '${pkgs.jq}/bin/jq'
        sed -i 's/gitHash=".*"/gitHash="${rev}"/' set_version.sh
        ${pkgs.bash}/bin/bash set_version.sh
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
    ];
    buildPhase = ''
      node ./esbuild.js --loglevel=verbose
    '';
    installPhase =
      let
        cliScript = pkgs.writeScript "pc-contracts-cli" ''
          #!/usr/bin/env bash

          SCRIPT_DIR="$(cd "$(dirname "$(readlink -f "$0")")" && pwd)"
          CLI_TMP="$SCRIPT_DIR/.index.mjs"

          cat << 'EOFCLI' > "$CLI_TMP"
          TEMPSCRIPT
          EOFCLI

          export NODE_PATH="$SCRIPT_DIR/node_modules"

          node "$CLI_TMP" "$@"
        '';
      in
      ''
        mkdir -p $out
        contents=$(cat dist/index.js)
        cp ${cliScript} $out/pc-contracts-cli

        substituteInPlace $out/pc-contracts-cli \
          --replace 'TEMPSCRIPT' "$contents"

        npm prune --offline --cache node_modules --omit=dev --ignore-scripts
        cp -R node_modules $out
      '';
  };
in
{
  inherit compiled bundled src nodeModules;

}
