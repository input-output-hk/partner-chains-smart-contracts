{ repoRoot, inputs, pkgs, lib, system, ... }:

let
  project = repoRoot.nix.project;
in
[
  (
    project.flake
  )
  rec {
    pkgs-ctl = import inputs.nixpkgs-ctl {
      inherit system;
      overlays = [
        (import "${inputs.iohk-nix}/overlays/crypto")
        inputs.haskell-nix.overlay
        inputs.cardano-transaction-lib.overlays.runtime
        inputs.cardano-transaction-lib.overlays.purescript
        inputs.cardano-transaction-lib.overlays.spago
      ];
    };
    apps = rec {
      default = sidechain-main-cli;
      sidechain-main-cli = {
        type = "app";
        program = "${inputs.self.packages.sidechain-main-cli}/bin/sidechain-main-cli";
      };
    };
    devShells = rec {
      default = pkgs.mkShell {
        inputsFrom = [ps hs];
        nativeBuildInputs = with pkgs; [
          # Shell utils
          bashInteractive
          git
          cabal-install

          # Lint / Format
          fd
          hlint
          haskellPackages.apply-refact
          haskellPackages.cabal-fmt
          haskellPackages.fourmolu
          #nixpkgs-fmt
          alejandra
          graphviz
        ];
        shellHook = ''
          ${ps.shellHook}
        '';
      };
      profiled = project.variants.profiled.devShell;
      hs = inputs.self.devShell;
      ps = let
        shell = repoRoot.nix.lib.mkPurescriptProject.devShell;
      in
        pkgs.mkShell {
          inputsFrom = [shell];
          packages = [pkgs-ctl.nodejs];
          shellHook = ''
            if [ ! -e "offchain/src/TrustlessSidechain/CLIVersion.purs" ]; then
              pushd offchain
              make version
              popd
            fi
          '';
        };
    };
    packages = repoRoot.nix.packages;
    checks = repoRoot.nix.checks;

    # This is used for nix build .#check.<system> because nix flake check
    # does not work with haskell.nix import-from-derivtion.
    check = pkgs.runCommand "combined-check"
      {
        nativeBuildInputs =
          builtins.attrValues repoRoot.nix.checks
          ++ builtins.attrValues inputs.self.packages
          ++ inputs.self.devShells.hs.nativeBuildInputs
          ++ inputs.self.devShells.ps.nativeBuildInputs
          ++ inputs.self.devShells.ps.buildInputs;
      } "touch $out";

  }
]
