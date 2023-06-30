{
  description = "trustless-sidechain";

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  inputs = {
    cardano-transaction-lib.url = "github:Plutonomicon/cardano-transaction-lib?rev=9c8d5dd1a2b5ad4ac2bc4b74473fec3a11b24a8e";

    plutip.follows = "cardano-transaction-lib/plutip";
    haskell-nix.follows = "cardano-transaction-lib/plutip/haskell-nix";
    nixpkgs.follows = "cardano-transaction-lib/nixpkgs";
    iohk-nix.follows = "cardano-transaction-lib/plutip/iohk-nix";
    CHaP.follows = "cardano-transaction-lib/plutip/CHaP";

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
        in
        pkgs.haskell-nix.cabalProject {
          src = ./onchain;
          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
          };
          compiler-nix-name = "ghc8107";
          modules = plutip.haskellModules;
          shell = {
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
              graphviz
            ];
            additional = ps: with ps; [
              cardano-crypto-class
              plutus-tx-plugin
              plutus-script-utils
              plutus-ledger
              playground-common
            ];
            shellHook = ''
              [ -z "$(git config core.hooksPath)" -a -d hooks ] && {
                   git config core.hooksPath hooks
              }
            '';
            tools.haskell-language-server = { };
          };
        };

      psProjectFor = system:
        let
          projectName = "trustless-sidechain-ctl";
          pkgs = nixpkgsFor system;
          src = builtins.path {
            path = ./offchain;
            name = "${projectName}-src";
            # TODO: Add more filters
            filter = path: ftype: !(pkgs.lib.hasSuffix ".md" path);
          };
        in
        pkgs.purescriptProject {
          inherit src pkgs projectName;
          packageJson = ./offchain/package.json;
          packageLock = ./offchain/package-lock.json;
          spagoPackages = ./offchain/spago-packages.nix;
          withRuntime = true;
          shell.withChromium = false;
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

          pushd ${self}
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          export IN_NIX_SHELL='pure'

          make nixpkgsfmt_check
          popd

          pushd ${self}/onchain/
          make format_check cabalfmt_check lint
          popd

          pushd ${self}/offchain
          make check-format
          popd

          mkdir $out
        '';

      upToDatePlutusScriptCheckFor = system:
        let
          pkgs = nixpkgsFor system;
          hsProject = (hsProjectFor system).flake';
        in
        pkgs.runCommand "up-to-date-plutus-scripts-check"
          {
            nativeBuildInputs = self.devShells.${system}.hs.nativeBuildInputs
              ++ self.devShells.${system}.ps.nativeBuildInputs
              ++ self.devShells.${system}.ps.buildInputs;
          } ''
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          export IN_NIX_SHELL='pure'

          # Acquire temporary files..
          TMP=$(mktemp)

          # Setup temporary files cleanup
          function cleanup() {
            rm -rf $TMP
          }
          trap cleanup EXIT

          pushd ${self}/onchain
          ${hsProject.packages."trustless-sidechain:exe:trustless-sidechain-serialise"}/bin/trustless-sidechain-serialise \
            --purescript-plutus-scripts="$TMP"
          popd

          pushd ${self}/offchain
          diff $TMP src/TrustlessSidechain/RawScripts.purs
          exitCode=$?
          if [ "$exitCode" != "0" ]; then
            echo "Plutus scripts out of date."
            exit $exitCode
          fi
          popd

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
          name = "sidechain-main-cli";
          runtimeInputs = [ project.nodejs ];
          # Node's `process.argv` always contains the executable name as the
          # first argument, hence passing `sidechain-main-cli "$@"` rather than just
          # `"$@"`
          text = ''
            export NODE_PATH="${project.nodeModules}/lib/node_modules"
            node -e 'require("${project.compiled}/output/Main").main()' sidechain-main-cli "$@"
          '';
        };

      ctlBundleCliFor = system:
        let
          name = "trustless-sidechain-cli";
          version = "0.1.0";
          src = ./offchain;
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
            shell.withChromium = false;
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
            ln -s ${project.nodeModules}/lib/node_modules node_modules
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
          sidechain-main-cli = ctlMainFor system;
          # TODO: Fix web bundling
          # ctl-bundle-web = (psProjectFor system).bundlePursProject {
          #   main = "Main";
          #   entrypoint = "index.js"; # must be same as listed in webpack config
          #   webpackConfig = "webpack.config.js";
          #   bundledModuleName = "output.js";
          # };
          ctl-bundle-cli = ctlBundleCliFor system;
        });

      apps = perSystem (system: self.flake.${system}.apps // {
        ctl-runtime = (nixpkgsFor system).launchCtlRuntime vasilDevRuntimeConfig;
        ctl-runtime-preview = (nixpkgsFor system).launchCtlRuntime previewRuntimeConfig;
        sidechain-main-cli = {
          type = "app";
          program = "${ctlMainFor system}/bin/sidechain-main-cli";
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
        upToDatePlutusScriptCheck = upToDatePlutusScriptCheckFor system;
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
