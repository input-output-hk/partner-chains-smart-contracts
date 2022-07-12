{
  description = "trustless-sidechain";

  inputs = {
    plutip.url = "github:mlabs-haskell/plutip?rev=88e5318e66e69145648d5ebeab9d411fa82f6945";

    nixpkgs.follows = "plutip/nixpkgs";
    haskell-nix.follows = "plutip/haskell-nix";
    iohk-nix.follows = "plutip/haskell-nix";
    cardano-node.url = "github:input-output-hk/cardano-node/1.35.0";
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

      ghcVersion = "ghc8107";

      projectFor = system:
        let
          pkgs' = nixpkgsFor' system;
          project = (nixpkgsFor system).haskell-nix.cabalProject {
            src = ./.;
            compiler-nix-name = ghcVersion;
            inherit (plutip) cabalProjectLocal;
            extraSources = plutip.extraSources ++ [{
              src = plutip;
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
                bashInteractive
                coreutils-full
                direnv
                lesspipe
                git
                haskellPackages.apply-refact
                fd
                jq
                unixtools.xxd
                cabal-install
                hlint
                haskellPackages.cabal-fmt
                haskellPackages.fourmolu
                nixpkgs-fmt
                python3
                inputs.cardano-node.packages.${system}.cardano-node
                inputs.cardano-node.packages.${system}.cardano-cli
              ];
              tools.haskell-language-server = { };
              additional = ps: [ ps.plutip ];
            };
          };
        in
        project;
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
