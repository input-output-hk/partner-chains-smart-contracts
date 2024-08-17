{ system ? builtins.currentSystem
, pkgs ? import ../nix { inherit system; }
, onchainProject ? import ./. { }
}:

onchainProject.shellFor {
  withHoogle = false;
  buildInputs = with pkgs; [
    gnumake
    haskellPackages.cabal-install
    haskellPackages.cabal-fmt
    haskellPackages.fourmolu
    haskellPackages.ghcid
    haskellPackages.haskell-language-server
    haskellPackages.hlint
    haskellPackages.hoogle
  ];
}
