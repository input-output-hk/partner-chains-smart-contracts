.PHONY: nix-cabal-repl format_check format cabalfmt cabalfmt_check hoogle lint

CABAL_SOURCES := $(shell fd -ecabal)

# Starts a hoogle Server.
hoogle:
	@ nix develop -c hoogle server --local --port 8008

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

# Starts a ghci repl inside the nix environment.
nix-cabal-repl:
	@ nix develop -c cabal new-repl

# Include the makefile in the root directory so we can use rules like
# `requires_nix_shell`
include ../Makefile
