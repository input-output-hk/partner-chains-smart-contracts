{
  repoRoot,
  inputs,
  pkgs,
  lib,
  system,
  ...
}: {
  mkPurescriptProject = let
    project = pkgs.purescriptProject {
      projectName = "sidechain-main-cli";
      src = builtins.path {
        path = ../offchain;
        name = "sidechain-main-cli-src";
        # TODO: Add more filters
        filter = path: ftype: !(pkgs.lib.hasSuffix ".md" path);
      };

      packageJson = ../offchain/package.json;
      packageLock = ../offchain/package-lock.json;
      spagoPackages = ../offchain/spago-packages.nix;
      withRuntime = true;
      shell.packages = with pkgs; [
        # Shell Utils
        bashInteractive
        git
        jq

        # Lint / Format
        fd
        dhall

        # CTL Runtime
        docker
      ];
    };
    overrideBuilder = project.compiled.overrideAttrs (old: {
      # Add a patchPhase for patching in the CLI version from a script
      patchPhase = let
        rev =
          if builtins.hasAttr "rev" inputs.self
          then inputs.self.rev
          else inputs.self.dirtyRev;
      in ''
        chmod -R +w src
        substituteInPlace src/set_version.sh \
          --replace 'jq' '${pkgs.jq}/bin/jq'
        sed -i 's/gitHash=".*"/gitHash="${rev}"/' src/set_version.sh
        pushd src
        ${pkgs.bash}/bin/bash set_version.sh
        popd
      '';
    });
    projectPatched =
      project
      // {
        compiled = overrideBuilder;
      };
  in
    projectPatched;
}
