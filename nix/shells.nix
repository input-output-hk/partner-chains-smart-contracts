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
      # cabal-install installed separately from haskellPackages, see:
      # https://github.com/NixOS/nixpkgs/issues/321569#issuecomment-2212382173
      cabal-install

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
      spago
      self.packages.spago2nix
    ];
  };
}
