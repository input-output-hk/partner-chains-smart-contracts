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
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , pre-commit-hooks
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      overlays = [
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
      };
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
          };
        };
      };

      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          #
          # build tools
          #
          awscli2
          bashInteractive
          blst
          cabal-install
          changie
          fd
          ghc
          git
          gnumake
          haskell-language-server
          haskellPackages.cabal-fmt
          haskellPackages.fourmolu
          hlint
          libsodium
          nixpkgs-fmt
          pkg-config
          secp256k1
          zlib
          zip

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
          # LD_LIBRARY_PATH is required for cabal to find libraries
          export LD_LIBRARY_PATH="${pkgs.blst}/lib:${pkgs.libsodium}/lib:${pkgs.secp256k1}/lib"

          ${checks.pre-commit-check.shellHook}
        '';
      };
    });
}
