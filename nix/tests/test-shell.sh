#!/usr/bin/env nix-shell
#!nix-shell --pure -i bash ../../shell.nix

set -e

if ! [ $IN_NIX_SHELL = "pure" ]; then
  echo "This script should be executed in a pure shell"
  echo "Exiting!"
  exit 1
fi

echo ""
echo "Testing mandatory tools that should be available .."
echo ""

ghc --version
cabal --version
make --version
fourmolu --version
cardano-cli --version
cardano-node --version
cardano-testnet version
purs --version
spago --version
command -v spago2nix
esbuild --version
node --version

echo ""
echo "Success!"
echo ""

