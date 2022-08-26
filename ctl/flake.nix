{
  description = "ctl-test";
  inputs = {
    nixpkgs.follows = "cardano-transaction-lib/nixpkgs";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    cardano-transaction-lib = {
      type = "github";
      owner = "Plutonomicon";
      repo = "cardano-transaction-lib";
      rev = "acb68d4a238bfd56e1c4c2c0a1cfda42887817ea";
      inputs.cardano-configurations = {
        type = "github";
        owner = "input-output-hk";
        repo = "cardano-configurations";
        flake = false;
      };
    };
  };
  outputs = { self, nixpkgs, cardano-transaction-lib, ... }@inputs:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      perSystem = nixpkgs.lib.genAttrs defaultSystems;
      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          cardano-transaction-lib.overlays.runtime
          cardano-transaction-lib.overlays.purescript
        ];
      };
      runtimeConfig = {
        network = {
          name = "vasil-dev";
          magic = 9;
        };
      };
      psProjectFor = system:
        let
          projectName = "ctl-test";
          pkgs = nixpkgsFor system;
          src = builtins.path {
            path = self;
            name = "${projectName}-src";
            filter = path: ftype:
              !(pkgs.lib.hasSuffix ".md" path) # filter out certain files, e.g. markdown
              && !(ftype == "directory" && builtins.elem # or entire directories
                (baseNameOf path) [ "doc" ]
              );
          };
        in
        pkgs.purescriptProject {
          inherit pkgs src projectName;
          packageJson = ./package.json;
          packageLock = ./package-lock.json;
          spagoPackages = ./spago-packages.nix;
          withRuntime = true;
          shell.packages = with pkgs; [
            bashInteractive
            fd
            docker
            dhall
            nixpkgs-fmt
          ];
        };
      ctlMainFor = system:
        let
          pkgs = nixpkgsFor system;
          project = psProjectFor system;
        in
        pkgs.writeShellApplication {
          name = "ctl-main";
          runtimeInputs = [ pkgs.nodejs-14_x ];
          text = ''
            export NODE_PATH="${project.nodeModules}/lib/node_modules"
            node -e 'require("${project.compiled}/output/Main").main()' "$@"
          '';
        };
    in
    {
      packages = perSystem (system: {
        default = self.packages.${system}.ctl-bundle-web;
        ctl-bundle-web = (psProjectFor system).bundlePursProject {
          sources = [ "src" ];
          main = "Main";
          entrypoint = "index.js"; # must be same as listed in webpack config
          webpackConfig = "webpack.config.js";
          bundledModuleName = "output.js";
        };
        ctl-runtime = (nixpkgsFor system).buildCtlRuntime runtimeConfig;
        ctl-main = ctlMainFor system;
      });
      apps = perSystem (system: {
        ctl-runtime = (nixpkgsFor system).launchCtlRuntime runtimeConfig;
        ctl-main = {
          type = "app";
          program = "${ctlMainFor system}/bin/ctl-main";
        };
      });
      devShell = perSystem (system: (psProjectFor system).devShell);
      checks = perSystem (system:
        let pkgs = nixpkgsFor system; in
        {
          ctl-test = (psProjectFor system).runPursTest {
            sources = [ "src" "test" ];
            testMain = "Test.Main";
          };
          formatting-check = pkgs.runCommand "formatting-check"
            { nativeBuildInputs = [ pkgs.easy-ps.purs-tidy pkgs.fd ]; }
            ''cd ${self} && purs-tidy check $(fd -epurs) && touch $out'';
        });
    };
}
