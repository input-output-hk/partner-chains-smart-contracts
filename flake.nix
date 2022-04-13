{
  description = "trustless-sidechain";

  inputs = {
    plutip.url = "github:mlabs-haskell/plutip";

    nixpkgs.follows = "plutip/nixpkgs";
    haskell-nix.follows = "plutip/haskell-nix";
    iohk-nix.follows = "plutip/haskell-nix";
  };

  outputs = { self, nixpkgs, haskell-nix, plutip, ... }:
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
        in (nixpkgsFor system).haskell-nix.cabalProject {
          src = ./.;
          compiler-nix-name = ghcVersion;
          inherit (plutip) cabalProjectLocal;
          extraSources = plutip.extraSources ++ [{
            src = "${plutip}";
            subdirs = [ "." ];
          }];
          modules = plutip.haskellModules;
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
            ];
            tools.haskell-language-server = { };
            additional = ps: [ ps.plutip ];
          };
        };
    in {
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      packages = perSystem (system: self.flake.${system}.packages);

      apps = perSystem (system: self.flake.${system}.apps);

      checks = perSystem (system: self.flake.${system}.checks);

      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check" {
          nativeBuildInputs = builtins.attrValues self.checks.${system}
            ++ builtins.attrValues self.flake.${system}.packages
            ++ [ self.flake.${system}.devShell.inputDerivation ];
        } "touch $out");

      devShell = perSystem (system: self.flake.${system}.devShell);

      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
