{
  repoRoot,
  inputs,
  pkgs,
  lib,
  system,
  ...
}: let
  onchain = repoRoot.nix.onchain;
in [
  (
    onchain.flake
  )
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
        inputsFrom = [ps hs];
        nativeBuildInputs = [
          # Shell utils
          pkgs.bashInteractive
          pkgs.git
          pkgs.cabal-install

          # Lint / Format
          pkgs.fd
          pkgs.hlint
          #pkgs.haskellPackages.apply-refact
          #pkgs.haskellPackages.cabal-fmt
          #pkgs.haskellPackages.fourmolu
          #nixpkgs-fmt
          pkgs.alejandra
          pkgs.graphviz
        ];
        shellHook = ''
          ${ps.shellHook}
        '';
      };
      profiled = onchain.variants.profiled.devShell;
      hs = inputs.self.devShell;
      ps = let
        shell = repoRoot.nix.offchain.devShell;
      in
        pkgs.mkShell {
          inputsFrom = [shell];
          packages = [pkgs.nodejs pkgs.git];
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
    packages = repoRoot.nix.packages;
    checks = repoRoot.nix.checks;

    # This is used for nix build .#check.<system> because nix flake check
    # does not work with haskell.nix import-from-derivtion.
    check =
      pkgs.runCommand "combined-check"
      {
        nativeBuildInputs =
          builtins.attrValues inputs.self.checks.${system}
          ++ builtins.attrValues inputs.self.packages.${system}
          ++ inputs.self.devShells.${system}.hs.nativeBuildInputs
          ++ inputs.self.devShells.${system}.ps.nativeBuildInputs
          ++ inputs.self.devShells.${system}.ps.buildInputs;
      } "touch $out";
  }
]
