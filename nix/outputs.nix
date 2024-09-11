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
      default = pc-contracts-cli;
      pc-contracts-cli = {
        type = "app";
        program = "${inputs.self.packages.pc-contracts-cli}/bin/pc-contracts-cli";
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

          # no aarch64-darwin for spago 0.21 (stable) from purescript-overlay's spago
          (if pkgs.stdenv.isDarwin
          then
            inputs.purescript-overlay.packages.x86_64-darwin.spago-0_21_0
          else
            spago
          )

          inputs.self.packages.spago2nix

          inputs.self.packages.kupo
          inputs.self.packages.ogmios

        ];
      };
      nodeDeps = pkgs.mkShell {
        inputsFrom = [ ps hs ];
        packages = with pkgs; [
          tmux
        ];
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
        shellHook =
          let tmuxConf = pkgs.writeText "tmux.conf" "set-option -g default-shell ${pkgs.fish}/bin/fish"; in
          ''
            mkdir -p cardano-preview-workdir/kupo/
            tmux -f ${tmuxConf} new-session \; \
                send-keys './start-deps.sh cardano-node' C-m \; \
                split-window -v \; \
                send-keys './start-deps.sh kupo' C-m \; \
                split-window -v \; \
                send-keys './start-deps.sh ogmios' C-m \; \
                select-layout even-vertical \;
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
