{
  description = "partner-chains-smart-contracts";

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.sc.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "cache.sc.iog.io:b4YIcBabCEVKrLQgGW8Fylz4W8IvvfzRc+hy0idqrWU="
    ];
    allow-import-from-derivation = true;
    accept-flake-config = true;
  };

  inputs = {
    nixpkgs.follows = "haskell-nix/nixpkgs";
    pkgs.follows = "nixpkgs";
    nosys.url = "github:input-output-hk/nosys/overlays";

    blank.url = "github:input-output-hk/empty-flake";

    flake-compat = {
      url = "github:input-output-hk/flake-compat/fixes";
      flake = false;
    };
    pre-commit-hooks = {
      url = "github:cachix/git-hooks.nix";
    };
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    iohk-nix.inputs.nixpkgs.follows = "nixpkgs";

    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";

    npmlock2nix.url = "github:nix-community/npmlock2nix";
    npmlock2nix.flake = false;

    spago2nix = {
      url = "github:justinwoo/spago2nix";
      flake = false;
    };

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
      inputs.hydra.follows = "blank";
    };
    devx = {
      url = "github:input-output-hk/devx/e0d22b5e52a90be3ee23c1056e8334b737bd88e2";
      inputs.haskellNix.follows = "haskell-nix";
      inputs.iohk-nix.follows = "iohk-nix";
    };
    # Used to provide the cardano-node and cardano-cli executables.
    cardano-node = {
      url = "github:input-output-hk/cardano-node/9.2.1";
      flake = false;
    };
  };
  outputs = inputs @ { nosys, pkgs, self, ... }:
    let outputs = import ./nix/outputs.nix;
    in nosys
      (inputs // {
        overlays = [
          inputs.haskell-nix.overlay
          inputs.iohk-nix.overlays.crypto
          inputs.iohk-nix.overlays.haskell-nix-extra
          inputs.purescript-overlay.overlays.default
          (
            final: prev: {
              # In order to actually apply the changes provided by iohk-nix
              # we need to modify haskell.nix overwriting the attribute set
              # with the altered crypto libraries
              haskell-nix = prev.haskell-nix // {
                extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings or { } // {
                  "libblst" = [ "libblst" ];
                  "libsodium" = [ "libsodium-vrf" ];
                };
              };
            }
          )
          (
            final: prev: {
              # no aarch64-darwin for spago 0.21 (stable) from purescript-overlay's spago
              spago =
                if prev.stdenv.isDarwin
                then
                  inputs.purescript-overlay.packages.x86_64-darwin.spago-0_21_0
                else
                  prev.spago;
            }
          )
        ];
      })
      outputs;
}
