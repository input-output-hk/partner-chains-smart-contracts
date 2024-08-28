{
  description = "trustless-sidechain";

  inputs = {
    iogx = {
      url = "github:input-output-hk/iogx";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.hackage.follows = "hackage";
      inputs.CHaP.follows = "CHaP";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.iohk-nix.follows = "iohk-nix";
      inputs.nix2container.follows = "blank";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };
    blank.url = "github:input-output-hk/empty-flake";

    flake-compat = {
      url = "github:input-output-hk/flake-compat/fixes";
      flake = false;
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

    nixpkgs.follows = "haskell-nix/nixpkgs";

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

    # Used to provide the cardano-node and cardano-cli executables.
    cardano-node = {
      url = "github:input-output-hk/cardano-node/9.1.0";
      flake = false;
    };

    mithril = {
      url = "github:input-output-hk/mithril";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs:
    inputs.iogx.lib.mkFlake {
      inherit inputs;
      repoRoot = ./.;
      systems = [ "x86_64-linux" "aarch64-darwin" ];
      outputs = import ./nix/outputs.nix;
      nixpkgsArgs = {
        overlays = [
          inputs.iohk-nix.overlays.crypto
          inputs.haskell-nix.overlay
          inputs.purescript-overlay.overlays.default
        ];
      };
    };
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
}
