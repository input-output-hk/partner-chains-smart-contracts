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
      rev = "9d6d73a4285439f2ed8ec46fe4b2a1974fb89b0c";
    };
  };
  outputs = { self, nixpkgs, cardano-transaction-lib, ... }@inputs: let
    defaultSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
    perSystem = nixpkgs.lib.genAttrs defaultSystems;
    nixpkgsFor = system: import nixpkgs {
      inherit system;
      overlays = [ cardano-transaction-lib.overlay.${system} ];
    };
    psProjectFor = system: let
      projectName = "ctl-scaffold";
      pkgs = nixpkgsFor system;
      src = ./.;
      in pkgs.purescriptProject {
        inherit pkgs src projectName;
      };
    in {
    packages = perSystem (system: {
      default = self.packages.${system}.ctl-scaffold-bundle-web;
      ctl-scaffold-bundle-web = (psProjectFor system).bundlePursProject {
        sources = [ "src" ];
        main = "Main";
      };
      ctl-scaffold-runtime = (nixpkgsFor system).buildCtlRuntime { };
    });
    apps = perSystem (system: { ctl-scaffold-runtime = (nixpkgsFor system).launchCtlRuntime { }; });
    devShell = perSystem (system: (psProjectFor system).devShell);
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
