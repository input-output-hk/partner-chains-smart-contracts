{ system ? builtins.currentSystem
, pkgs ? import ./nix { inherit system; }
}:

let
  project = import ./. { inherit system pkgs; };
in
  project.onchain.shellFor {
  withHoogle = false;
  buildInputs = with pkgs; [
    # general inputs
    gnumake
    jq
    fd

    # onchain inputs
    haskellPackages.cabal-fmt
    haskellPackages.cabal-install
    haskellPackages.fourmolu
    haskellPackages.ghcid
    haskellPackages.haskell-language-server
    haskellPackages.hlint
    haskellPackages.hoogle

    # offchain inputs
    cardano-cli
    cardano-node
    cardano-testnet
    esbuild
    fd
    nodejs-18_x
    ogmios
    purescript-language-server
    purescript-psa
    purs
    purs-tidy
    spago
    spago2nix
  ];
}
