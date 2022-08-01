SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

BROWSER_RUNTIME ?= 1
ps-sources := $$(fd -epurs)
ps-entrypoint := Main

output.js: src/${ps-entrypoint}.purs
	spago bundle-module -m ${ps-entrypoint} --to output.js

run-dev: output.js
	@BROWSER_RUNTIME=${BROWSER_RUNTIME} webpack-dev-server --progress

run-build: output.js
	@BROWSER_RUNTIME=${BROWSER_RUNTIME} webpack --mode=production

check-format:
	@purs-tidy check ${ps-sources}

format:
	@purs-tidy format-in-place ${ps-sources}
