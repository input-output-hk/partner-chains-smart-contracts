{
  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.sc.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "cache.sc.iog.io:b4YIcBabCEVKrLQgGW8Fylz4W8IvvfzRc+hy0idqrWU="
    ];
    accept-flake-config = true;
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
    cardano-node.url = "github:input-output-hk/cardano-node/d7abccd4e90c38ff5cd4d6a7839689d888332056";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , purescript-overlay
    , cardano-node
    , pre-commit-hooks
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      overlays = [
        purescript-overlay.overlays.default
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
      };
      kupo = pkgs.callPackage ./nix/packages/kupo.nix { };
      ogmios = pkgs.callPackage ./nix/packages/ogmios.nix { };
    in

    rec {
      checks = {
        pre-commit-check = pre-commit-hooks.lib."${system}".run {
          src = ./.;
          hooks = {
            fourmolu.enable = true;
            shellcheck.enable = true;
            cabal-fmt.enable = true;
            nixpkgs-fmt.enable = true;
            purs-tidy.enable = true;
            end-of-file-fixer =
              {
                enable = true;
                excludes = [ ".*\\.golden" ];
              };
            trim-trailing-whitespace =
              {
                enable = true;
                excludes = [ ".*\\.golden" ];
              };
          };
          tools = {
            cabal-fmt = pkgs.haskellPackages.cabal-fmt.bin;
            fourmolu = pkgs.haskellPackages.fourmolu;
            shellcheck = pkgs.shellcheck;
            nixpkgs-fmt = pkgs.nixpkgs-fmt;
            purs-tidy = pkgs.purs-tidy;
          };
        };
      };

      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [
          #
          # build tools
          #
          blst
          cabal-install
          dhall
          esbuild
          fd
          ghc
          gnumake
          haskell-language-server
          haskellPackages.cabal-fmt
          haskellPackages.fourmolu
          hlint
          libsodium
          nixpkgs-fmt
          nodejs
          pkg-config
          purescript
          purescript-psa
          purs-tidy
          eslint
          secp256k1
          spago-bin.spago-0_21_0
          zlib
          zip

          #
          # runtime dependencie
          #
          cardano-node.packages."${system}".cardano-cli
          cardano-node.packages."${system}".cardano-node
          cardano-node.packages."${system}".cardano-testnet
          ogmios
          kupo

          #
          # Rust dependencies for raw-scripts crate
          #
          cargo
          clippy
          rust-analyzer
          rustfmt
          cargo-edit
        ];
        shellHook = ''
          ${checks.pre-commit-check.shellHook}
        '';
      };
    });
}
