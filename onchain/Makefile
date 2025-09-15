.PHONY: all cabal-update onchain update-scripts check-raw-scripts coverage-gen coverage

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

update-perf:
	cabal test script-perf --test-options "--accept"

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

coverage-gen:
	cabal test trustless-sidechain-test --enable-coverage
	cabal test roundtrip --enable-coverage
	cabal test script-specs --enable-coverage
	@rm -rf combined.tix coverage
	@find dist-newstyle -name '*.tix' | xargs hpc sum --union --output=combined.tix

coverage: coverage-gen
	@mkdir -p coverage
	hpc markup $(foreach dir,$(shell find dist-newstyle -type d -path '*/hpc/vanilla/mix'),--hpcdir=$(dir)) --destdir=coverage combined.tix
	@echo "Coverage report generated in coverage/"
