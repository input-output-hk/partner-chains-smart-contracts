{
  description = "trustless-sidechain";

  inputs = {
    plutip.url = "github:mlabs-haskell/plutip?rev=88e5318e66e69145648d5ebeab9d411fa82f6945";

    nixpkgs.follows = "cardano-transaction-lib/nixpkgs"; # < TODO: verify this is what we want to follow
    haskell-nix.follows = "cardano-transaction-lib/haskell-nix"; # <
    iohk-nix.follows = "cardano-transaction-lib/haskell-nix"; # <
    cardano-transaction-lib = {
      type = "github";
      owner = "Plutonomicon";
      repo = "cardano-transaction-lib";
      rev = "acb68d4a238bfd56e1c4c2c0a1cfda42887817ea";
      inputs.cardano-configurations = {
        url = "path:./cardano-configurations";
        flake = false;
      };
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, plutip, cardano-transaction-lib, ... }@inputs:
    let
      runtimeConfig = {
        network = {
          name = "takao";
          magic = 9;
        };
      };

      supportedSystems = with nixpkgs.lib.systems.supported;
        tier1 ++ tier2 ++ tier3;
      # TODO: Do we really claim to support all of them?
      # Including for example MIPS and PPC?
      # Also lib.systems.supported is deprecated (and removed in 22.05)

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [
            haskell-nix.overlay
            (import "${plutip.inputs.iohk-nix}/overlays/crypto")
            cardano-transaction-lib.overlays.runtime
            cardano-transaction-lib.overlays.purescript
          ];
          inherit (haskell-nix) config;
        };

      # TODO: Do we actually need this?? Can't we just use the overlayed pkgs?
      nixpkgsFor' = system:
        import nixpkgs {
          inherit system;
          inherit (haskell-nix) config;
        };

      hsProjectFor = system:
        let
          pkgs' = nixpkgsFor' system;
          project = (nixpkgsFor system).haskell-nix.cabalProject {
            src = ./.;
            compiler-nix-name = "ghc8107";
            inherit (plutip) cabalProjectLocal;
            extraSources = plutip.extraSources ++ [{
              src = plutip;
              subdirs = [ "." ];
            }];
            modules = plutip.haskellModules ++ [{
              packages = {
                trustless-sidechain.components.tests.trustless-sidechain-test.build-tools =
                  [
                    project.hsPkgs.cardano-cli.components.exes.cardano-cli
                    project.hsPkgs.cardano-node.components.exes.cardano-node
                  ];
              };
            }];
            shell = {
              withHoogle = true;
              exactDeps = true;
              nativeBuildInputs = with pkgs'; [
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

                # Cardano Runtime
                project.hsPkgs.cardano-cli.components.exes.cardano-cli
                project.hsPkgs.cardano-node.components.exes.cardano-node
              ];
              additional = ps: [ ps.plutip ];
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
            nativeBuildInputs = [
              self.devShells.${system}.hs.nativeBuildInputs
              self.devShells.${system}.ps.buildInputs
            ];
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
      # TODO: ^ thouch or mkdir -p are less error-prone.
      #       currently this fails because $out/bin/format-check doesn't exist.

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
          sources = [ "src" ];
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

      # TODO: What's this used for?
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check"
          {
            nativeBuildInputs = builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.flake.${system}.packages
              ++ [
              self.devShells.${system}.hs.nativeBuildInputs
              self.devShells.${system}.ps.buildInputs
            ];
          } "touch $out");

      checks = perSystem (system: self.flake.${system}.checks // {
        formatCheck = formatCheckFor system;
        trustless-sidechain-ctl = (psProjectFor system).runPursTest {
          sources = [ "src" "test" ];
          testMain = "Test.Main";
        };
      });

      # TODO: make this a combined shell.
      #       using // makes the rhs devShell override the lhs one.
      devShells = perSystem (system: {
        ps = (psProjectFor system).devShell;
        hs = self.flake.${system}.devShell;
      });

      devShell = perSystem (system: self.devShells.${system}.hs);
      # END TODO

      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
