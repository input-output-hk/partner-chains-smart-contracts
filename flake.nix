{
  description = "trustless-sidechain";

  inputs = {

    purifix.url = "github:purifix/purifix";

    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
    };

    iogx = {
      url = "github:input-output-hk/iogx";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.hackage.follows = "hackage";
      inputs.CHaP.follows = "CHaP";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    n2c = {
      url = "github:nlewo/nix2container";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    blank.url = "github:input-output-hk/empty-flake";
    cardano-nix = {
      url = "github:tgunnoe/cardano.nix/add-darwin";
      inputs."cardano-node-8.7.3".follows = "blank";
      inputs."cardano-node-8.1.1".follows = "blank";
      inputs.cardano-db-sync.follows = "blank";
      inputs.blockfrost.follows = "blank";
      inputs.nixpkgs.follows = "nixpkgs";
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
    };

    cardano-node.url = "github:input-output-hk/cardano-node/9.1.0";

  };
  outputs = inputs:
    inputs.iogx.lib.mkFlake {
      inherit inputs;
      repoRoot = ./.;
      systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
      outputs = import ./nix/outputs.nix;
      nixpkgsArgs = {
        overlays = [
          inputs.iohk-nix.overlays.crypto
          inputs.haskell-nix.overlay
          inputs.purescript-overlay.overlays.default
          inputs.purifix.overlay
        ];
      };
    };
  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://public-plutonomicon.cachix.org"
      "https://cache.sc.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc="
      "cache.sc.iog.io:b4YIcBabCEVKrLQgGW8Fylz4W8IvvfzRc+hy0idqrWU="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = true;
    accept-flake-config = true;
  };
}
