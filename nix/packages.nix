{
  repoRoot,
  inputs,
  pkgs,
  lib,
  system,
  ...
}: let
  project = repoRoot.nix.offchain;
  bundled =
    pkgs.runCommand "sidechain-bundled" {}
    ''
      mkdir -p $out
      cp -R ${project.compiled}/* $out
      chmod -R u+rw $out/output
      ln -s ${project.nodeModules}/lib/node_modules $out/node_modules
    '';
in rec {
  sidechain-main-cli = pkgs.writeShellApplication {
      name = "sidechain-main-cli";
      runtimeInputs = [project.nodejs];
      # Node's `process.argv` always contains the executable name as the
      # first argument, hence passing `sidechain-main-cli "$@"` rather than just
      # `"$@"`
      text = ''
        ${project.nodejs}/bin/node --enable-source-maps -e 'import("${bundled}/output/Main/index.js").then(m => m.main())' sidechain-main-cli "$@"
      '';
    };

  sidechain-main-cli-bundle = let
    name = "trustless-sidechain-cli-bundle";
    version = "5.0.0";
    src = ../offchain;
    project = repoRoot.nix.lib.patchedProject {
      inherit src pkgs;
      projectName = name;
      withRuntime = false;
    };
  in
    pkgs.stdenv.mkDerivation rec {
      inherit name src version;
      buildInputs = [
        #project.purs # this (commonjs ffi) instead of pkgs.purescript (esmodules ffi)
      ];
      runtimeInputs = [project.nodejs];
      unpackPhase = ''
        ln -s ${project.compiled}/* .
        ln -s ${project.nodeModules}/lib/node_modules node_modules
      '';
      buildPhase = ''
        purs bundle "output/*/*.js" -m Main --main Main -o main.js
      '';
      installPhase = ''
        mkdir -p $out
        tar chf $out/${name}-${version}.tar main.js node_modules
      '';
    };

  sidechain-main-cli-bundle-esbuild = project.bundlePursProjectEsbuild {
    main = "Main";
    builtProject = project.compiled;
    browserRuntime = false;
  };

  sidechain-release-bundle = let
    project = repoRoot.nix.offchain;
    jsContents = builtins.readFile "${sidechain-main-cli-bundle-esbuild}/index.js";
    wrappedNodeScript =  pkgs.writeScript "sidechain-main-cli"
    ''
    #!/usr/bin/env bash
    CLI_TMP="./.index.mjs"
    cat << 'EOFCLI' > "$CLI_TMP"
    ${jsContents}
    EOFCLI

    NODE_PATH=$PWD/node_modules node  $CLI_TMP $@
  '';
  in
    pkgs.runCommand "bundled-cli" 
    {
      buildInputs = [ pkgs.zip ];
    }
    ''
    cp -R ${project.nodeModules}/lib/node_modules .
    chmod -R u+rw ./node_modules
    cp ${wrappedNodeScript} ./sidechain-cli 
    mkdir -p $out
    zip -r $out/release.zip  ./node_modules ./sidechain-cli
    '';
#    ''
#    mkdir -p $out
#    cp -R ${project.nodeModules}/lib/node_modules $out/node_modules
#    cp ${wrappedNodeScript} $out/sidechain-cli
#    '';

  sidechain-main-cli-image = inputs.n2c.packages.nix2container.buildImage {
    name = "sidechain-main-cli-docker";
    tag = "${inputs.self.shortRev or inputs.self.dirtyShortRev}";
    config = {
      Cmd = ["sidechain-main-cli"];
    };
    copyToRoot = pkgs.buildEnv {
      name = "root";
      paths = [pkgs.bashInteractive pkgs.coreutils sidechain-main-cli];
      pathsToLink = ["/bin"];
    };
  };
}
