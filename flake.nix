{
  description = "trustless-sidechain";

  inputs = rec {
    nixpkgs.follows = "cardano-transaction-lib/nixpkgs";
    haskell-nix.follows = "cardano-transaction-lib/haskell-nix";
    iohk-nix.follows = "cardano-transaction-lib/iohk-nix";
    CHaP.follows = "cardano-transaction-lib/CHaP";
    plutip.follows = "cardano-transaction-lib/plutip";

    cardano-transaction-lib.url = "github:Plutonomicon/cardano-transaction-lib/e5ea971efe2c1816fd448b2244b7421ab435c66d";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, CHaP, cardano-transaction-lib, plutip, ... }@inputs:
    let
      vasilDevRuntimeConfig = {
        network = {
          name = "vasil-dev";
          magic = 9;
        };
      };

      previewRuntimeConfig = {
        # Conveniently, by default the ctl runtime configuration uses the
        # preview network. See here:
        # https://github.com/Plutonomicon/cardano-transaction-lib/blob/87233da45b7c433c243c539cb4d05258e551e9a1/nix/runtime.nix
        network = {
          name = "preview";
          magic = 2;
        };

        # Need use a more recent node version -- iirc. there was a hard fork
        # somewhat recently?
        node = {
          # the version of the node to use, corresponds to the image version tag,
          # i.e. `"inputoutput/cardano-node:${tag}"`
          tag = "1.35.4";
        };

        datumCache = {
          # The `firstBlock` is essentially what ogmios-datum-cache starts
          # "syncing" from, and the default doesn't exist apparently... so
          # we give it a block which actually exists.
          blockFetcher = {
            firstBlock = {
              slot = 3212169;
              id = "199a5953f54a216532b396b112c7b8c561710e93a978383173dddadda3b9bc17";
            };
          };
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
          runtimeInputs = [ project.nodejs ];
          # Node's `process.argv` always contains the executable name as the
          # first argument, hence passing `ctl-main "$@"` rather than just
          # `"$@"`
          text = ''
            export NODE_PATH="${project.nodeModules}/lib/node_modules"
            node -e 'require("${project.compiled}/output/Main").main()' ctl-main "$@"
          '';
        };

      ctlBundleCliFor = system:
        let
          name = "trustless-sidechain-cli";
          version = "0.1.0";
          src = ./ctl;
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              cardano-transaction-lib.overlays.purescript
            ];
          };
          project = pkgs.purescriptProject {
            inherit src pkgs;
            projectName = name;
            withRuntime = false;
          };
        in
        pkgs.stdenv.mkDerivation rec {
          inherit name src version;
          buildInputs = [
            project.purs # this (commonjs ffi) instead of pkgs.purescript (esmodules ffi)
          ];
          runtimeInputs = [ project.nodejs ];
          unpackPhase = ''
            ln -s ${project.compiled}/* .
            ln -s ${project.nodeModules} node_modules
          '';
          buildPhase = ''
            purs bundle "output/*/*.js" -m Main --main Main -o main.js
          '';
          installPhase = ''
            mkdir -p $out
            tar chf $out/${name}-${version}.tar main.js node_modules
          '';
        };
    in
    {
      project = perSystem hsProjectFor;

      flake = perSystem (system: (hsProjectFor system).flake { });

      packages = perSystem
        (system: self.flake.${system}.packages // {
          ctl-runtime-preview = (nixpkgsFor system).launchCtlRuntime previewRuntimeConfig;
          ctl-runtime = (nixpkgsFor system).buildCtlRuntime vasilDevRuntimeConfig;
          ctl-main = ctlMainFor system;
          ctl-bundle-web = (psProjectFor system).bundlePursProject {
            main = "Main";
            entrypoint = "index.js"; # must be same as listed in webpack config
            webpackConfig = "webpack.config.js";
            bundledModuleName = "output.js";
          };
          ctl-bundle-cli = ctlBundleCliFor system;
        });

      apps = perSystem (system: self.flake.${system}.apps // {
        ctl-runtime = (nixpkgsFor system).launchCtlRuntime vasilDevRuntimeConfig;
        ctl-runtime-preview = (nixpkgsFor system).launchCtlRuntime previewRuntimeConfig;
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
      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
