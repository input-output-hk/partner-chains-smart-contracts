.PHONY: build-nix hoogle nix-build-library nix-build-executables \
        nix-build-test requires_nix_shell ci-build-run

# Generate TOC for README.md
# It has to be manually inserted into the README.md for now.
generate-readme-contents:
	nix shell nixpkgs#nodePackages.npm --command "npx markdown-toc ./README.md --no-firsth1"

# Attempt the CI locally
# TODO

# Build the library with nix.
nix-build-library:
	@ nix build .#trustless-sidechain:lib:trustless-sidechain

current-system := $(shell nix eval --impure --expr builtins.currentSystem)

# Build the executables with nix (also builds the test suite).
nix-build-executables:
	@ nix build .#check.${current-system}

# Build the tests with nix.
nix-build-test:
	@ nix build .#trustless-sidechain:test:trustless-sidechain-tests

# Target to use as dependency to fail if not inside nix-shell.
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || { \
	echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"; \
	echo "    run 'nix develop' first"; \
	false; \
	}

NIX_SOURCES := $(shell fd -enix)

nixpkgsfmt: requires_nix_shell
	nixpkgs-fmt $(NIX_SOURCES)

nixpkgsfmt_check: requires_nix_shell
	nixpkgs-fmt --check $(NIX_SOURCES)

lock: requires_nix_shell
	nix flake lock

lock_check: requires_nix_shell
	nix flake lock --no-update-lock-file

lint: requires_nix_shell
	hlint --no-summary $(shell fd -ehs -elhs)
