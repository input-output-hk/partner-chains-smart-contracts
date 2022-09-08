{
  description = "trustless-sidechain";

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = "true";
  };
  
  inputs = {
    # FIXME: https://github.com/NixOS/nix/issues/6986
    bot-plutus-interface.url = "github:mlabs-haskell/bot-plutus-interface?rev=1f18513d9a6326bf70a5cd68e74a40b5aa104861";
    bot-plutus-interface.inputs.haskell-nix.follows = "haskell-nix";
    bot-plutus-interface.inputs.nixpkgs.follows = "haskell-nix/nixpkgs";
    plutip.url = "github:mlabs-haskell/plutip?rev=88e5318e66e69145648d5ebeab9d411fa82f6945";
    plutip.inputs.bot-plutus-interface.follows = "bot-plutus-interface";
    plutip.inputs.haskell-nix.follows = "bot-plutus-interface/haskell-nix";
    plutip.inputs.iohk-nix.follows = "bot-plutus-interface/iohk-nix";
    plutip.inputs.nixpkgs.follows = "bot-plutus-interface/nixpkgs";

    nixpkgs.follows = "bot-plutus-interface/nixpkgs";
    haskell-nix.url = "github:input-output-hk/haskell.nix?ref=extra-sources";
    iohk-nix.follows = "plutip/haskell-nix";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, plutip, ... }@inputs:
    let
      supportedSystems = nixpkgs.lib.systems.flakeExposed;

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
                    project.hsPkgs.cardano-cli.components.exes.cardano-cli
                    project.hsPkgs.cardano-node.components.exes.cardano-node
                  ];
              };
            }];
            shell = {
              withHoogle = true;
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
