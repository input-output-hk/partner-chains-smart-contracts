{ system ? builtins.currentSystem
, pkgs ? import ../nix { inherit system; }
, hsShell ? import ../onchain/shell.nix { inherit system; }
}:

pkgs.mkShell {
  inputsFrom = [ hsShell ];
  buildInputs = with pkgs; [
    esbuild
    cardano-cli
    cardano-node
    cardano-testnet
    fd
    # NOTE: the purescript-overlay does not provide
    # a spago binary for darwin-aarch64 so we are using
    # the one provided via nixpkgs
    spago
    nodejs-18_x
    ogmios
    purescript-language-server
    purescript-psa
    purs
    purs-tidy
    spago2nix
  ];
}
