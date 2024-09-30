.PHONY: format_check format cabalfmt cabalfmt_check lint

CABAL_SOURCES := $(shell fd -ecabal)

cabalfmt: requires_nix_shell
	cabal-fmt --inplace $(CABAL_SOURCES)

cabalfmt_check: requires_nix_shell
	cabal-fmt --check $(CABAL_SOURCES)

# Add folder locations to the list to be reformatted.
format:
	fourmolu $(FOURMOLU_EXTENSIONS) --mode inplace --check-idempotence \
		$(shell fd -ehs -elhs)

format_check:
	fourmolu $(FOURMOLU_EXTENSIONS) --mode check --check-idempotence \
		$(shell fd -ehs -elhs)

lint: requires_nix_shell
	hlint --no-summary $(shell fd -ehs -elhs)

# Include the makefile in the root directory so we can use rules like
# `requires_nix_shell`
include ../Makefile
