{
  description = "trustless-sidechain";

  inputs = {
    plutip.url = "github:mlabs-haskell/plutip?rev=da2b4b42a86ba370493402b15dc01f35ed7cbda7";

    nixpkgs.follows = "plutip/nixpkgs";
    haskell-nix.follows = "plutip/haskell-nix";
    iohk-nix.follows = "plutip/haskell-nix";
    cardano-node.url = "github:input-output-hk/cardano-node?rev=6471c31f8b61798df57a9f3345548703295cac9e";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, plutip, ... }@inputs:
    let
      supportedSystems = with nixpkgs.lib.systems.supported;
        tier1 ++ tier2 ++ tier3;

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [
            haskell-nix.overlay
            (import "${plutip.inputs.iohk-nix}/overlays/crypto")
          ];
          inherit (haskell-nix) config;
        };
      nixpkgsFor' = system:
        import nixpkgs {
          inherit system;
          inherit (haskell-nix) config;
        };

      deferPluginErrors = true;

      ghcVersion = "ghc8107";

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
        in
        (nixpkgsFor system).haskell-nix.cabalProject {
          src = ./.;
          compiler-nix-name = ghcVersion;
          inherit (plutip) cabalProjectLocal;
          extraSources = plutip.extraSources ++ [{
            src = "${plutip}";
            subdirs = [ "." ];
          }];
          modules = plutip.haskellModules ++ [{
            packages = {
              trustless-sidechain.components.tests.trustless-sidechain-test.build-tools =
                [
                  inputs.cardano-node.packages.${system}.cardano-node
                  inputs.cardano-node.packages.${system}.cardano-cli
                ];
            };
          }];
          shell = {
            withHoogle = false;
            exactDeps = true;
            nativeBuildInputs = with pkgs'; [
              git
              haskellPackages.apply-refact
              fd
              cabal-install
              hlint
              haskellPackages.cabal-fmt
              haskellPackages.fourmolu
              nixpkgs-fmt
              inputs.cardano-node.packages.${system}.cardano-node
              inputs.cardano-node.packages.${system}.cardano-cli
            ];
            tools.haskell-language-server = { };
            additional = ps: [ ps.plutip ];
          };
        };
      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
        in
        pkgs.runCommand "format-check"
          { nativeBuildInputs = [ self.devShell.${system}.nativeBuildInputs ]; } ''
          cd ${self}
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          export IN_NIX_SHELL='pure'
          make format_check cabalfmt_check nixpkgsfmt_check lint
          mkdir $out
        '';
    in
    {
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      packages = perSystem (system: self.flake.${system}.packages);

      apps = perSystem (system: self.flake.${system}.apps);

      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check"
          {
            nativeBuildInputs = builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.flake.${system}.packages
              ++ [ self.flake.${system}.devShell.inputDerivation ];
          } "touch $out");
      checks = perSystem (system: self.flake.${system}.checks // {
        formatCheck = formatCheckFor system;
      });

      devShell = perSystem (system: self.flake.${system}.devShell);

      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
