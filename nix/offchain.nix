{ repoRoot
, inputs
, pkgs
, lib
, system
,
}:
let
  offchain =
    let
      isDarwin = pkgs.lib.hasSuffix "darwin" system;
    in
    repoRoot.nix.lib.patchedProject {
      projectName = "sidechain-main-cli";
      src = builtins.path {
        path = ../offchain;
        name = "sidechain-main-cli-src";
        # TODO: Add more filters
        filter = path: ftype: !(pkgs.lib.hasSuffix ".md" path);
      };

      packageJson =
        if isDarwin
        then ../offchain/package-macos-aarch64.json
        else ../offchain/package.json;
      packageLock =
        if isDarwin
        then ../offchain/package-lock-macos-aarch64.json
        else ../offchain/package-lock.json;
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
