{
  repoRoot,
  inputs,
  pkgs,
  lib,
  system,
  ...
}: let
  pkgs-ctl = inputs.self.pkgs-ctl;
in {
  formatCheck =
    pkgs-ctl.runCommand "format-check"
    {
      nativeBuildInputs =
        inputs.self.devShells.hs.nativeBuildInputs
        ++ inputs.self.devShells.ps.nativeBuildInputs
        ++ inputs.self.devShells.ps.buildInputs
        ++ inputs.self.devShells.default.nativeBuildInputs;
    } ''

      pushd ${inputs.self}
      export LC_CTYPE=C.UTF-8
      export LC_ALL=C.UTF-8
      export LANG=C.UTF-8
      export IN_NIX_SHELL='pure'

      make nixpkgsfmt_check
      popd

      pushd ${inputs.self}/onchain/
      #make format_check cabalfmt_check lint
      popd

      pushd ${inputs.self}/offchain
      #make check-format
      popd

      mkdir $out
    '';
  upToDatePlutusScriptCheck = let
    hsProject = repoRoot.nix.project.flake;
  in
    pkgs-ctl.runCommand "up-to-date-plutus-scripts-check"
    {
      nativeBuildInputs =
        inputs.self.devShells.hs.nativeBuildInputs
        ++ inputs.self.devShells.ps.nativeBuildInputs
        ++ inputs.self.devShells.ps.buildInputs;
    } ''
      export LC_CTYPE=C.UTF-8
      export LC_ALL=C.UTF-8
      export LANG=C.UTF-8
      export IN_NIX_SHELL='pure'

      # Acquire temporary files..
      TMP=$(mktemp)

      # Setup temporary files cleanup
      function cleanup() {
        rm -rf $TMP
      }
      trap cleanup EXIT

      pushd ${inputs.self}/onchain > /dev/null
      ${hsProject.packages."trustless-sidechain-serialise"}/bin/trustless-sidechain-serialise \
        --purescript-plutus-scripts="$TMP"
      popd > /dev/null

      pushd ${inputs.self}/offchain > /dev/null

      # Compare the generated file and the file provided in the repo.
      cmp $TMP src/TrustlessSidechain/RawScripts.purs || {
        exitCode=$? ;
        echo "Plutus scripts out of date." ;
        echo 'See `offchain/src/TrustlessSidechain/RawScripts.purs` for instructions to resolve this' ;
        exit $exitCode ;
      }

      popd > /dev/null

      touch $out
    '';
  trustless-sidechain-ctl = let
    # This is almost a direct copy of the function from the CTL library
    # Unfortunately the original did not accept a modified project
    runPursTest = {
      project,
      pkgs ? pkgs-ctl,
      testMain ? "Test.Main",
      name ? "trustless-sidechain-ctl-check",
      nodeModules ? project.nodeModules,
      env ? {},
      buildInputs ? [],
      ...
    }:
      pkgs-ctl.runCommand "${name}"
      (
        {
          buildInputs = [project.compiled nodeModules] ++ buildInputs;
          NODE_PATH = "${nodeModules}/lib/node_modules";
        }
        // env
      )
      ''
        cd ${project.compiled}
        ${pkgs-ctl.nodejs}/bin/node --enable-source-maps -e 'require("./output/${testMain}").main()'
        touch $out
      '';
    runPlutipTest = args:
      runPursTest (
        args
        // {
          buildInputs = with pkgs-ctl;
            [
              ogmios
              plutip-server
              kupo
            ]
            ++ (args.buildInputs or []);
        }
      );
  in
    runPlutipTest {
      project = repoRoot.nix.lib.mkPurescriptProject;
      testMain = "Test.Main";
    };
}