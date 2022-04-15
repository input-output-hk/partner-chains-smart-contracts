{
  description = "trustless-sidechain";

  inputs = {
    plutip = {
      url = "github:mlabs-haskell/plutip?rev=dd1656081fdd438890a15ab3e66b4713edcac1e4";
    };

    nixpkgs.follows = "plutip/nixpkgs";
    haskell-nix.follows = "plutip/haskell-nix";
    iohk-nix.follows = "plutip/haskell-nix";
    cardano-node.follows = "plutip/bot-plutus-interface/cardano-node";
    plutip.inputs.bot-plutus-interface.url = "github:mlabs-haskell/bot-plutus-interface?rev=5e504bdb31aad0bd63f481c5fa43cbe9322c84b5";
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
            withHoogle = true;
            exactDeps = true;
            nativeBuildInputs = with pkgs'; [
              git
              haskellPackages.apply-refact
              fd
              cabal-install
              hlint
              haskellPackages.cabal-fmt
              nixpkgs-fmt
              inputs.cardano-node.packages.${system}.cardano-node
              inputs.cardano-node.packages.${system}.cardano-cli
            ];
            tools.haskell-language-server = { };
            additional = ps: [ ps.plutip ];
          };
        };
    in
    {
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      packages = perSystem (system: self.flake.${system}.packages);

      apps = perSystem (system: self.flake.${system}.apps);

      checks = perSystem (system: self.flake.${system}.checks);

      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check"
          {
            nativeBuildInputs = builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.flake.${system}.packages
              ++ [ self.flake.${system}.devShell.inputDerivation ];
          } "touch $out");

      devShell = perSystem (system: self.flake.${system}.devShell);
    };
}
