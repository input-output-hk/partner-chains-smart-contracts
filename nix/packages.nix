{ repoRoot
, inputs
, pkgs
, lib
, system
, ...
}:
let
  project = repoRoot.nix.offchain;
in
rec {
  sidechain-main-cli = pkgs.writeShellApplication {
    name = "sidechain-main-cli";
    runtimeInputs = [ project.nodejs ];
    text = ''
      ${project.nodejs}/bin/node --enable-source-maps -e 'import("${project.compiled}/output/Main/index.js").then(m => m.main())' sidechain-main-cli "$@"
    '';
  };

  sidechain-main-cli-bundle-esbuild = project.bundlePursProjectEsbuild {
    main = "Main";
    builtProject = project.compiled;
    browserRuntime = false;
  };

  sidechain-release-bundle =
    let
      jsContents =
        builtins.readFile "${sidechain-main-cli-bundle-esbuild}/index.js";
      wrappedNodeScript = pkgs.writeScript "sidechain-main-cli" ''
        #!/usr/bin/env bash

        SCRIPT_DIR="$(cd "$(dirname "$(readlink -f "$0")")" && pwd)"
        CLI_TMP="$SCRIPT_DIR/.index.mjs"

        cat << 'EOFCLI' > "$CLI_TMP"
        ${jsContents}
        EOFCLI

        export NODE_PATH="$SCRIPT_DIR/node_modules"

        node "$CLI_TMP" "$@"
      '';
      prunedNodeModules = project.mkNodeModules { withDevDeps = false; };
    in
    pkgs.runCommand "bundled-cli" { buildInputs = [ pkgs.zip ]; } ''
      cp -R ${prunedNodeModules}/lib/node_modules .
      chmod -R u+rw ./node_modules
      cp ${wrappedNodeScript} ./sidechain-cli
      mkdir -p $out
      zip -r $out/release.zip  ./node_modules ./sidechain-cli
    '';
}
