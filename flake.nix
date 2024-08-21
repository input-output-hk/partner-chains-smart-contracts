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
      inputs.nix2container.follows = "n2c";
      inputs.easy-purescript-nix.follows = "cardano-transaction-lib/easy-purescript-nix";
      inputs.nixpkgs-stable.follows = "nixpkgs";
      inputs.pre-commit-hooks-nix.follows = "cardano-nix/pre-commit-hooks-nix";
    };
    n2c = {
      url = "github:nlewo/nix2container";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    blank.url = "github:input-output-hk/empty-flake";

    flake-compat = {
      url = "github:input-output-hk/flake-compat/fixes";
      flake = false;
    };
    cardano-nix = {
      url = "github:tgunnoe/cardano.nix/add-darwin";
      inputs."cardano-node-8.7.3".follows = "blank";
      inputs."cardano-node-8.1.1".follows = "blank";
      inputs.cardano-db-sync.follows = "blank";
      inputs.blockfrost.follows = "blank";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cardano-transaction-lib = {
      url = "github:Plutonomicon/cardano-transaction-lib/3c134eabb573c5b7b9eed3a064be194c8273d1c3";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-arion.follows = "blank";
      inputs.cardano-node.follows = "blank";
      inputs.db-sync.follows = "blank";
      inputs.cardano-nix.follows = "cardano-nix";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.hackage-nix.follows = "hackage";
      inputs.hercules-ci-effects.follows = "blank";
      inputs.blockfrost.follows = "blank";
      inputs.CHaP.follows = "CHaP";
    };
    iohk-nix.follows = "cardano-transaction-lib/iohk-nix";

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
          inputs.cardano-transaction-lib.overlays.purescript
          inputs.cardano-transaction-lib.overlays.spago
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
