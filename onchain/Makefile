.PHONY: all cabal-update onchain update-scripts check-raw-scripts

trustless-dir := ../raw-scripts
trustless-traced-dir := ../raw-scripts-traced

serialise        := cabal run trustless-sidechain-serialise --
serialise-traced := cabal run trustless-sidechain-serialise-traced --

raw-scripts := $(trustless-dir)/src/lib.rs
raw-scripts-trace := $(trustless-traced-dir)/src/lib.rs

all: cabal-update onchain check-raw-scripts

cabal-update:
	cabal update

onchain:
	cabal build
	cabal test all --test-show-details=direct

update-scripts:
	@echo "Generating $(raw-scripts)..."
	$(serialise) --rust-plutus-scripts=$$PWD/$(raw-scripts)
	@echo "Updated $(raw-scripts)."
	@echo "Generating $(raw-scripts-trace)..."
	$(serialise-traced) --rust-plutus-scripts=$$PWD/$(raw-scripts-trace)
	@echo "Updated $(raw-scripts-trace)."

check-raw-scripts:
	@$(MAKE) update-scripts
	@if ! git diff --quiet -- $(raw-scripts); then \
		echo "\nError: $(raw-scripts) is not up to date\n"; \
		exit 1; \
	fi
	@if ! git diff --quiet -- $(raw-scripts-trace); then \
		echo "\nError: $(raw-scripts-trace) is not up to date\n"; \
		exit 1; \
	fi
