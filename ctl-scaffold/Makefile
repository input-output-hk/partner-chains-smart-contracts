SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

BROWSER_RUNTIME ?= 1
ps-sources := $$(fd -epurs)
ps-entrypoint := Main

output.js: src/${ps-entrypoint}.purs
	spago bundle-module -m ${ps-entrypoint} --to output.js

run-dev: output.js scriptsAreFresh
	@BROWSER_RUNTIME=${BROWSER_RUNTIME} webpack-dev-server --progress

run-build: output.js scriptsAreFresh
	@BROWSER_RUNTIME=${BROWSER_RUNTIME} webpack --mode=production

check-format:
	@purs-tidy check ${ps-sources}

format:
	@purs-tidy format-in-place ${ps-sources}

# Check that the oldest script is newer than the newest haskell source file
# Make isn't quite designed for that
# This errors rather than depending on a haskell binary in ../
scriptsAreFresh:
	@HSDir=../src/TrustlessSidechain/OnChain/ && ScriptDir=Scripts/ \
	&& NewestHSScript=$$HSDir$$(ls -t $$HSDir | head -1) \
        && OldestPSScript=$$ScriptDir$$(ls -rt $$ScriptDir/ | head -1) \
        && echo Checking that Scripts/ are fresh enough \
	&& echo Newest hs file: $$NewestHSScript should be newer than oldest serialised script: $$OldestPSScript ! \
	&& if [ $$NewestHSScript -ot $$OldestPSScript ] then; else echo If this fails, Please reserialise the scripts from the haskell side \
