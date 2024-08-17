{ system ? builtins.currentSystem
, pkgs ? import ../nix { inherit system; }
, hsShell ? import ../onchain/shell.nix { inherit system; }
}:

pkgs.mkShell {
  inputsFrom = [ hsShell ];
  buildInputs = with pkgs; [
    cardano-node
    cardano-cli
    cardano-testnet
    fd
    ogmios
    purs
    purs-tidy
    purescript-language-server
    purescript-psa
    spago
    nodejs-18_x
  ];
}
