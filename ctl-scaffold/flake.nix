{
  description = "ctl-scaffold";
  inputs = {
    nixpkgs.follows  = "cardano-transaction-lib/nixpkgs";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    cardano-transaction-lib = {
      type = "github";
      owner = "Plutonomicon";
      repo = "cardano-transaction-lib";
      rev = "27b38d402a9a9b2ef90129c195c9730c0091da35";
    };
  };
  outputs = { self, nixpkgs, cardano-transaction-lib, ... }@inputs: let
    defaultSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
    perSystem = nixpkgs.lib.genAttrs defaultSystems;
    nixpkgsFor = system: import nixpkgs {
      inherit system;
      overlays = [ cardano-transaction-lib.overlay ];
    };
    psProjectFor = system: let
      projectName = "ctl-scaffold";
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
      in pkgs.purescriptProject {
        inherit pkgs src projectName;
        packageJson = ./package.json;
        packageLock = ./package-lock.json;
        shell.packages = with pkgs; [
          bashInteractive
          fd
          docker
        ];
      };
    in {
    packages = perSystem (system: {
      default = self.packages.${system}.ctl-scaffold-bundle-web;
      ctl-scaffold-bundle-web = (psProjectFor system).bundlePursProject {
        sources = [ "src" ];
        main = "Main";
        entrypoint = "index.js"; # must be same as listed in webpack config
        webpackConfig = "webpack.config.js";
        bundledModuleName = "output.js";
      };
      ctl-scaffold-runtime = (nixpkgsFor system).buildCtlRuntime { };
    });
    apps = perSystem (system: { ctl-scaffold-runtime = (nixpkgsFor system).launchCtlRuntime { }; });
    devShell = perSystem (system: (psProjectFor system).devShell
      );
    checks = perSystem (system: let pkgs = nixpkgsFor system; in {
      ctl-scaffold = (psProjectFor system).runPursTest {
        sources = [ "src" "test" ];
        testMain = "Test.Main";
        };
      formatting-check = pkgs.runCommand "formatting-check"
        { nativeBuildInputs = [ pkgs.easy-ps.purs-tidy pkgs.fd ]; }
        ''cd ${self} && purs-tidy check $(fd -epurs) && touch $out'';
    });
  };
}
