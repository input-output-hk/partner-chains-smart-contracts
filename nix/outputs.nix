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
      ps =
        pkgs.mkShell {
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
            easy-ps.spago
            easy-ps.psa
            easy-ps.spago2nix
            easy-ps.pscid
            easy-ps.purs
            easy-ps.purs-tidy

            inputs.self._packages.ogmios
            inputs.self._packages.kupo

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
    packages = repoRoot.nix.packages;
    _packages =
      let
        flake-compat = import inputs.flake-compat;
        cardanoPackages = (flake-compat { src = inputs.cardano-node; }).defaultNix.packages.${system};
      in
      {
        inherit (cardanoPackages) cardano-node cardano-cli cardano-testnet;
        ogmios = inputs.cardano-nix.packages.${system}."ogmios-6.5.0";
        kupo = inputs.cardano-nix.packages.${system}."kupo-2.9.0";
        # This package doesn't work in the check output for some esoteric reason
        sidechain-main-cli-image = inputs.n2c.packages.nix2container.buildImage {
          name = "sidechain-main-cli-docker";
          tag = "${inputs.self.shortRev or inputs.self.dirtyShortRev}";
          config = { Cmd = [ "sidechain-main-cli" ]; };
          copyToRoot = pkgs.buildEnv {
            name = "root";
            paths = [ pkgs.bashInteractive pkgs.coreutils inputs.self.packages.sidechain-main-cli ];
            pathsToLink = [ "/bin" ];
          };
        };
      };
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
