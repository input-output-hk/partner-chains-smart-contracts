.PHONY: requires_nix_shell all cabal-update onchain update-scripts check-raw-scripts coverage-gen coverage format

# Target to use as dependency to fail if not inside nix-shell.
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || { \
	echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"; \
	echo "    run 'nix develop' first"; \
	false; \
	}

raw-scripts-dir := ../raw-scripts
raw-scripts-traced-dir := ../raw-scripts-traced

serialise        := cabal run partner-chains-smart-contracts-serialise --
serialise-traced := cabal run partner-chains-smart-contracts-serialise-traced --

raw-scripts := $(raw-scripts-dir)/src/lib.rs
raw-scripts-trace := $(raw-scripts-traced-dir)/src/lib.rs

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
	cabal test partner-chains-smart-contracts-test --enable-coverage
	cabal test roundtrip --enable-coverage
	cabal test script-specs --enable-coverage
	@rm -rf combined.tix coverage
	@find dist-newstyle -name '*.tix' | xargs hpc sum --union --output=combined.tix

coverage: coverage-gen
	@mkdir -p coverage
	hpc markup $(foreach dir,$(shell find dist-newstyle -type d -path '*/hpc/vanilla/mix'),--hpcdir=$(dir)) --destdir=coverage combined.tix
	@echo "Coverage report generated in coverage/"

format: requires_nix_shell
	pre-commit run --all-files
