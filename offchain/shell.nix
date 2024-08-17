{ system ? builtins.currentSystem
, pkgs ? import ../nix { inherit system; }
, hsShell ? import ../onchain/shell.nix { inherit system; }
}:

pkgs.mkShell {
  inputsFrom = [ hsShell ];
  buildInputs = with pkgs; [
    cardano-cli
    cardano-node
    cardano-testnet
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
