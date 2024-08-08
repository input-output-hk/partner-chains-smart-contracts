{ repoRoot
, inputs
, pkgs
, lib
, system
,
}:
let
  offchain = repoRoot.nix.lib.patchedProject {
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
in
offchain
