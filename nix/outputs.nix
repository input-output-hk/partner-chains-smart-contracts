{ repoRoot
, inputs
, pkgs
, lib
, system
, ...
}:
let
  onchain = repoRoot.nix.onchain;
in
[
  (onchain.flake)
  {
    apps = rec {
      default = sidechain-main-cli;
      sidechain-main-cli = {
        type = "app";
        program = "${inputs.self.packages.sidechain-main-cli}/bin/sidechain-main-cli";
      };
    };
    devShells = rec {
      default = pkgs.mkShell {
        inputsFrom = [ ps hs ];
        nativeBuildInputs = [
          # These packages are all required for running checks present
          # in the makefiles
          pkgs.hlint
          pkgs.nixpkgs-fmt
          pkgs.haskellPackages.cabal-fmt
          pkgs.haskellPackages.fourmolu
          pkgs.nodePackages.purs-tidy
          pkgs.nodePackages.eslint
        ];
        shellHook = ''
          ${ps.shellHook}
        '';
      };
      profiled = onchain.variants.profiled.devShell;
      hs = inputs.self.devShell;
      ps = pkgs.mkShell {
        packages = with pkgs; [
          # Shell Utils
          bashInteractive
          git
          jq

          # Lint / Format
          fd
          dhall

          # CTL Runtime
          docker

          nodejs-18_x
          nodePackages.node2nix

          # Purescript
          purescript-psa
          pscid
          purs
          purs-tidy
          # no aarch64-darwin from purescript-overlay's spago
          inputs.nixpkgs.legacyPackages.spago

          inputs.self.packages.spago2nix

          inputs.self.packages.kupo
          inputs.self.packages.ogmios

        ];
        shellHook = ''
          PROJ_ROOT=$(git rev-parse --show-toplevel)
          if [ ! -e "$PROJ_ROOT/offchain/src/TrustlessSidechain/CLIVersion.purs" ]; then
            pushd $PROJ_ROOT/offchain
            make version
            popd
          fi
        '';
      };
    };
    packages = repoRoot.nix.packages.default;

    _checks = repoRoot.nix.checks;

    # This is used for nix build .#check.<system> because nix flake check
    # does not work with haskell.nix import-from-derivtion.
    check =
      pkgs.runCommand "combined-check"
        {
          nativeBuildInputs =
            builtins.attrValues inputs.self._checks.${system}
            ++ builtins.attrValues inputs.self.packages.${system}
            ++ inputs.self.devShells.${system}.hs.nativeBuildInputs
            ++ inputs.self.devShells.${system}.ps.nativeBuildInputs
            ++ inputs.self.devShells.${system}.ps.buildInputs;
        } "touch $out";
  }
]
