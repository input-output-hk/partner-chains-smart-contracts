{ inputs
, self
, pkgs
, ...
}:
let
  cardano-cli = self.packages.cardano-cli;
  cardano-node = self.packages.cardano-node;
  cardano-testnet = self.packages.cardano-testnet;
in
{
  default = pkgs.mkShell {
    inherit (self.check.pre-commit-check) shellHook;
    buildInputs = self.check.pre-commit-check.enabledPackages;
    env = {
      CARDANO_CLI = "${cardano-cli}/bin/cardano-cli";
      CARDANO_NODE = "${cardano-node}/bin/cardano-node";
    };
    nativeBuildInputs = [
      # These packages are all required for running checks present
      # in the makefiles
      pkgs.nixpkgs-fmt
      pkgs.nodePackages.purs-tidy
      pkgs.nodePackages.eslint
    ];
    inputsFrom = [
      #self.devShells.default
      inputs.devx.devShells.ghc96-iog
    ];
    packages = with pkgs; [
      # local stack
      cardano-cli
      cardano-node
      cardano-testnet
      self.packages.kupo
      self.packages.ogmios

      watchexec
      moreutils

      # Shell Utils
      bashInteractive
      git
      jq
      curl

      # Lint / Format
      fd
      dhall

      docker

      nodejs-18_x

      # Purescript
      purescript-psa
      pscid
      purs
      purs-tidy
      spago
      self.packages.spago2nix
    ];
  };
}
