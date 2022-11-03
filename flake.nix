{
  description = "trustless-sidechain";

  inputs = {
    cardano-transaction-lib.url = "github:Plutonomicon/cardano-transaction-lib/85208c5c705d2947e50e32c5eb9f22c3b0b72401";
    nixpkgs.follows = "cardano-transaction-lib/nixpkgs";
    haskell-nix.follows = "cardano-transaction-lib/haskell-nix";
    iohk-nix.follows = "cardano-transaction-lib/iohk-nix";

    cardano-transaction-lib.inputs = {
      plutip.follows = "plutip";
    };
    bot-plutus-interface = {
      url = github:hyphenrf/bot-plutus-interface/cardano-base-patch;
      inputs.cardano-base.url = github:input-output-hk/cardano-base/c16a1ebf60a27051303ec4ea76495311e3d2c4b1;
      inputs.cardano-wallet.url = github:input-output-hk/cardano-wallet/9d34b2633ace6aa32c1556d33c8c2df63dbc8f5b;
    };
    plutip = {
      url = github:mlabs-haskell/plutip/ee7df5bfc86751e6199b118c13135a36cf3679b9;
      inputs = {
        bot-plutus-interface.follows = "bot-plutus-interface";
        haskell-nix.follows = "bot-plutus-interface/haskell-nix";
        iohk-nix.follows = "bot-plutus-interface/iohk-nix";
        nixpkgs.follows = "bot-plutus-interface/nixpkgs";
      };
    };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    CHaP.follows = "cardano-transaction-lib/CHaP";
  };

  outputs = { self, nixpkgs, haskell-nix, CHaP, cardano-transaction-lib, plutip, ... }@inputs:
    let
      runtimeConfig = {
        network = {
          name = "vasil-dev";
          magic = 9;
        };
      };

      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [
            haskell-nix.overlay
            (import "${inputs.iohk-nix}/overlays/crypto")
            cardano-transaction-lib.overlays.runtime
            cardano-transaction-lib.overlays.purescript
          ];
          inherit (haskell-nix) config;
        };

      hsProjectFor = system:
        let
          pkgs = nixpkgsFor system;
          project = pkgs.haskell-nix.cabalProject {
            src = ./.;
            inputMap = {
              "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
            };
            compiler-nix-name = "ghc8107";
            modules = plutip.haskellModules;
            shell = {
              withHoogle = true;
              exactDeps = true;
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
                nixpkgs-fmt
              ];
              additional = ps: [
                ps.cardano-crypto-class
                ps.plutus-tx-plugin
                ps.plutus-script-utils
                ps.plutus-ledger
                ps.playground-common
              ];
              shellHook = ''
                [ -z "$(git config core.hooksPath)" -a -d hooks ] && {
                     git config core.hooksPath hooks
                }
              '';
              tools.haskell-language-server = { };
            };
          };
        in
        project;

      psProjectFor = system:
        let
          projectName = "trustless-sidechain-ctl";
          pkgs = nixpkgsFor system;
          src = builtins.path {
            path = ./ctl;
            name = "${projectName}-src";
            # TODO: Add more filters
            filter = path: ftype: !(pkgs.lib.hasSuffix ".md" path);
          };
        in
        pkgs.purescriptProject {
          inherit src pkgs projectName;
          packageJson = "${src}/package.json";
          packageLock = "${src}/package-lock.json";
          spagoPackages = "${src}/spago-packages.nix";
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

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
        in
        pkgs.runCommand "format-check"
          {
            nativeBuildInputs = self.devShells.${system}.hs.nativeBuildInputs
              ++ self.devShells.${system}.ps.nativeBuildInputs
              ++ self.devShells.${system}.ps.buildInputs;
          } ''
          cd ${self}
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          export IN_NIX_SHELL='pure'
          make format_check cabalfmt_check nixpkgsfmt_check lint
          cd ${self}/ctl
          make check-format
          mkdir $out
        '';

      # CTL's `runPursTest` won't pass command-line arugments to the `node`
      # invocation, so we can essentially recreate `runPursTest` here with and
      # pass the arguments
      ctlMainFor = system:
        let
          pkgs = nixpkgsFor system;
          project = psProjectFor system;
        in
        pkgs.writeShellApplication {
          name = "ctl-main";
          runtimeInputs = [ pkgs.nodejs-14_x ];
          # Node's `process.argv` always contains the executable name as the
          # first argument, hence passing `ctl-main "$@"` rather than just
          # `"$@"`
          text = ''
            export NODE_PATH="${project.nodeModules}/lib/node_modules"
            node -e 'require("${project.compiled}/output/Main").main()' ctl-main "$@"
          '';
        };
    in
    {
      project = perSystem hsProjectFor;

      flake = perSystem (system: (hsProjectFor system).flake { });

      packages = perSystem (system: self.flake.${system}.packages // {
        ctl-runtime = (nixpkgsFor system).buildCtlRuntime runtimeConfig;
        ctl-main = ctlMainFor system;
        ctl-bundle-web = (psProjectFor system).bundlePursProject {
          main = "Main";
          entrypoint = "index.js"; # must be same as listed in webpack config
          webpackConfig = "webpack.config.js";
          bundledModuleName = "output.js";
        };
      });

      apps = perSystem (system: self.flake.${system}.apps // {
        ctl-runtime = (nixpkgsFor system).launchCtlRuntime runtimeConfig;
        ctl-main = {
          type = "app";
          program = "${ctlMainFor system}/bin/ctl-main";
        };
      });

      # This is used for nix build .#check.<system> because nix flake check
      # does not work with haskell.nix import-from-derivtion.
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check"
          {
            nativeBuildInputs = builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.flake.${system}.packages
              ++ self.devShells.${system}.hs.nativeBuildInputs
              ++ self.devShells.${system}.ps.nativeBuildInputs
              ++ self.devShells.${system}.ps.buildInputs;
          } "touch $out");

      checks = perSystem (system: self.flake.${system}.checks // {
        formatCheck = formatCheckFor system;
        trustless-sidechain-ctl = (psProjectFor system).runPlutipTest {
          testMain = "Test.Main";
        };
      });

      devShells = perSystem (system: rec {
        ps = (psProjectFor system).devShell;
        hs = self.flake.${system}.devShell;
        default = (nixpkgsFor system).mkShell {
          inputsFrom = [ ps hs ];
          shellHook = ''
            ${hs.shellHook}
            ${ps.shellHook}
          '';
        };
      });
    };
}
