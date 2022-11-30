{
  description = "trustless-sidechain";

  inputs = rec {
    nixpkgs.follows = "cardano-transaction-lib/nixpkgs";
    haskell-nix.follows = "cardano-transaction-lib/haskell-nix";
    iohk-nix.follows = "cardano-transaction-lib/iohk-nix";
    CHaP.follows = "cardano-transaction-lib/CHaP";

    plutip.url = github:jaredponn/plutip/697dfd248b9c80098d0a0d4d0bad986902c93fbc;

    cardano-transaction-lib = {
      url = "github:Plutonomicon/cardano-transaction-lib/87233da45b7c433c243c539cb4d05258e551e9a1";
      inputs = {
        plutip = plutip;
        ogmios-datum-cache.url = github:mlabs-haskell/ogmios-datum-cache/880a69a03fbfd06a4990ba8873f06907d4cd16a7;
      };
    };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    # TODO: spago bundle-app is not working in a derivation with spago 0.20.9 (https://github.com/purescript/spago/issues/888)
    easy-ps = {
      url = "github:justinwoo/easy-purescript-nix/ddd2ded8d37ab5d3013f353ca3b6ee05eb23d5c0";
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

      previewRuntimeConfig = pkgs: _final: rec {
        # See the [ctl runtime
        # documentation](https://github.com/Plutonomicon/cardano-transaction-lib/blob/1ec5a7a82e2a119364a3577022b6ff3c7e84a612/doc/runtime.md)
        # Note that we include the current systems packages as an argument so we
        # can spin up our own version of ogmios-datum-cache which is called with
        # a CLI argument that syncs ogmios-datum-cache up from the origin.
        name = "preview";
        magic = 2;

        ogmios.port = 1337;

        ctlServer = { enable = true; port = 8081; };
        node = {
          port = 3001;
          # the version of the node to use, corresponds to the image version tag,
          # i.e. `"inputoutput/cardano-node:${tag}"`
          tag = "1.35.4";
        };

        postgres = {
          port = 5432;
          user = "ctxlib";
          password = "ctxlib";
          db = "ctxlib";
        };

        datumCache = {
          port = 9999;
          controlApiToken = "user:password";
          # some easy login info here, this is used for someone to fix the
          # control API -- see the ogmios-datum-cache documentation
        };

        extraServices = {
          # Spin up our own service of ogmios-datum-cache that allows us to
          # sync up with the node from the origin
          ogmios-datum-cache =
            {
              service = {
                useHostStore = true;
                ports = [ ("${toString datumCache.port}:${toString datumCache.port}") ];
                restart = "on-failure";
                depends_on = [ "postgres-${name}" "ogmios" ];
                command = [
                  "${pkgs.bash}/bin/sh"
                  "-c"
                  ''
                    ${pkgs.ogmios-datum-cache}/bin/ogmios-datum-cache \
                      --log-level warn \
                      --use-latest \
                      --server-api "${toString datumCache.controlApiToken}" \
                      --server-port ${toString datumCache.port} \
                      --ogmios-address ogmios \
                      --ogmios-port ${toString ogmios.port} \
                      --db-port "${toString postgres.port}" \
                      --db-host "postgres-${name}" \
                      --db-user "${postgres.user}" \
                      --db-name "${postgres.db}" \
                      --db-password "${postgres.password}" \
                      --from-origin
                  ''
                ];
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
          runtimeInputs = [ pkgs.nodejs-14_x ];
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
          src = ./ctl;
          pkgs = nixpkgsFor system;
          spagoPkgs = import "${src}/spago-packages.nix" { inherit pkgs; };
          project = psProjectFor system;
        in
        pkgs.stdenv.mkDerivation rec {
          inherit src;

          pname = "ctl-bundle-cli";
          version = "0.1.0";
          buildInputs = [
            pkgs.spago
            pkgs.purescript
            pkgs.nodejs
            spagoPkgs.installSpagoStyle
            spagoPkgs.buildSpagoStyle
            spagoPkgs.buildFromNixStore
          ];
          unpackPhase = ''
            cp $src/{packages,spago}.dhall .
            cp -r ${project.compiled} .
            mkdir node_modules
            cp -r ${project.nodeModules} ./node_modules/
          '';
          buildPhase = ''
            spago bundle-app --no-build --no-install --global-cache skip
            mv index.js main.js
          '';

          installPhase = ''
            mkdir -p $out
            tar cvf $out/ctl-scripts-${version}.tar main.js node_modules
          '';
        };
      # { } ''
      # mkdir $out
      # cp -r ${project.compiled} .
      # cp -r ${project.nodeModules} .
      # spago bundle-app --no-build --no-install --global-cache skip
      # mv index.js main.js
      # tar cvf $out/ctl-scripts-${version}.tar main.js ${project.nodeModules}
      # '';
    in
    {
      project = perSystem hsProjectFor;

      flake = perSystem (system: (hsProjectFor system).flake { });

      packages = perSystem
        (system: self.flake.${system}.packages // {
          ctl-runtime-preview = (nixpkgsFor system).launchCtlRuntime (previewRuntimeConfig (nixpkgsFor system));
          ctl-runtime = (nixpkgsFor system).buildCtlRuntime vasilDevRuntimeConfig;
          ctl-main = ctlMainFor system;
          ctl-bundle-web = (psProjectFor system).bundlePursProject {
            main = "Main";
            entrypoint = "index.js"; # must be same as listed in webpack config
            webpackConfig = "webpack.config.js";
            bundledModuleName = "output.js";
          };
          # ctl-bundle-cli = ctlBundleCliFor system;
          ctl-bundle-cli = import ./nix/ctl-bundle-cli.nix rec {
            pkgs = nixpkgsFor system;
            easy-ps = import inputs.easy-ps { inherit pkgs; };
            purs = easy-ps.purs-0_14_5;
            nodejs = pkgs.nodejs-14_x;
          };
        });

      apps = perSystem (system: self.flake.${system}.apps // {
        ctl-runtime = (nixpkgsFor system).launchCtlRuntime vasilDevRuntimeConfig;
        ctl-runtime-preview = (nixpkgsFor system).launchCtlRuntime (previewRuntimeConfig (nixpkgsFor system));
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
