{ inputs
, self
, pkgs
, ...
}:
let
  cardano-cli = self.packages.cardano-cli;
  cardano-node = self.packages.cardano-node;
  cardano-testnet = self.packages.cardano-testnet;
  haskellPackages = pkgs.haskell.packages.ghc96.ghcWithPackages (ps: with ps; [
    cabal-install
    hlint
    hoogle
    cabal-fmt
    fourmolu
    haskell-language-server
  ]);
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
      haskellPackages
      pkgs.nixpkgs-fmt
      pkgs.nodePackages.purs-tidy
      pkgs.nodePackages.eslint
    ];
    inputsFrom = [
      #self.devShells.default
    ];
    packages = with pkgs; [
      # local stack
      cardano-cli
      cardano-node
      cardano-testnet
      self.packages.kupo
      self.packages.ogmios

      haskellPackages

      watchexec

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
      self.packages.spago2nix

      # no aarch64-darwin for spago 0.21 (stable) from purescript-overlay's spago
      (if stdenv.isDarwin
      then
        inputs.purescript-overlay.packages.x86_64-darwin.spago-0_21_0
      else
        spago
      )



    ];
  };
}
