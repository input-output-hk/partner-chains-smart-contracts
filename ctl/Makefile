SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

BROWSER_RUNTIME ?= 1

ps-sources := $(shell fd -epurs)
plutus-scripts := $(wildcard Scripts/*.plutus)

ps-entrypoint := Main
ps-test-entrypoint := Test.Main
SRC = ${ps-test-entrypoint} ${ps-entrypoint}

${SRC}:
output.js: src/Main.purs ${plutus-scripts}
	spago bundle-module -m ${ps-entrypoint} --to output.js

dev: output.js
	@BROWSER_RUNTIME=${BROWSER_RUNTIME} webpack-dev-server --progress

build: output.js
	@BROWSER_RUNTIME=${BROWSER_RUNTIME} webpack --mode=production

# Note. need a plutip-server running (won't put `plutip-server &` here because it tends to flood the terminal)
test: ${plutus-scripts} ${SRC}
	@spago run -m ${ps-test-entrypoint}

check-format:
	@purs-tidy check ${ps-sources}

format:
	@purs-tidy format-in-place ${ps-sources}

# Check that the script is newer than the haskell source file of the same name
# This errors rather than depending on a haskell binary in ../
# Invariant: Plutus scripts must correspond in name to Haskell files.
#            If this cannot be maintained, we can rely on other means to detect
#            freshness (oldest script to newest haskell file).
Scripts/%.plutus: ../src/TrustlessSidechain/OnChain/%.hs
	@echo Please update your plutus scripts by running 'cabal run trustless-sidechain-serialise'
	@echo on the project root.
	@exit 1
