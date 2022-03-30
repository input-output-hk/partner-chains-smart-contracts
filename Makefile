# The plutus-pab commands, contracts and hoogle environment
# are made availible by the nix shell defined in shell.nix.
# In most cases you should execute Make after entering nix-shell.

.PHONY: hoogle pab_servers_all pab_servers_all pab_db clean_db \
	build test accept_pirs watch ghci readme_contents \
	format lint requires_nix_shell shell hls_shell code

usage:
	@echo "usage: make <command> [OPTIONS]"
	@echo
	@echo "Available options:"
	@echo "  WALLET  -- Specify wallet for command (1 or 2)"
	@echo "  FLAGS   -- Additional options passed to --ghc-options"
	@echo "  NIXOS   -- Add stack flags --no-nix and --system-ghc to work around stack issues on NixOS"
	@echo
	@echo "Available commands:"
	@echo "  hoogle              -- Start local hoogle"
	@echo "  pab_servers_all     -- Start the pab servers"
	@echo "  pab_servers_client  -- Start the pab web client"
	@echo "  pab_db              -- Generate pab db"
	@echo "  clean_db            -- Delete pab db"
	@echo "  build               -- Run cabal v2-build"
	@echo "  watch               -- Track files: trustless-sidechain.cabal, src/* and run 'make build' on change"
	@echo "  test                -- Run cabal v2-test"
	@echo "  accept_pirs         -- Accept new PIR changes"
	@echo "  ghci                -- Run cabal v2-repl trustless-sidechain"
	@echo "  format              -- Apply source code formatting with fourmolu"
	@echo "  format_check        -- Check source code formatting without making changes"
	@echo "  nixfmt              -- Apply nix formatting with nixfmt"
	@echo "  nixfmt_check        -- Check nix files for format errors"
	@echo "  lint                -- Check the sources with hlint"
	@echo "  ci                  -- Execute CI action"
	@echo "  shell							 -- Start a Nix shell"
	@echo "  hls_shell					 -- Start a Nix shell with HLS ready to launch an editor"
	@echo "  code							   -- Start Visual Studio Code in a Nix environment"
	@echo "  readme_contents     -- Add table of contents to README"
	@echo "  update_plutus       -- Update plutus version with niv"
	@echo "  clear_build         -- Deletes the build files for this specific project"
	@echo "  diagrams            -- Build SVG diagrams from graphviz dot diagrams"
	@echo "  diagram_pngs        -- Build PNG images from graphviz dot diagrams"
	@echo "  clean_diagrams      -- Delete results from diagrams, diagram_pngs"

PAB1 := plutus-pab --config=${PAB_CONFIG_PATH}/pab_env1.yaml
PAB2 := plutus-pab --config=${PAB_CONFIG_PATH}/pab_env2.yaml
WALLET ?= 1
ifeq ($(WALLET), 1)
	PAB := $(PAB1)
	PAB_DB_PATH := ${PAB_DB1_PATH}
else
	PAB := $(PAB2)
	PAB_DB_PATH := ${PAB_DB2_PATH}
endif

# Need to use --no-nix and --system-ghc from inside nix-shell
# on NixOS since stack doesn't support nixos, and ghc8103 isnt in any nixpkgs version
# maybe we could support this via stack's nix integration using a separate stack_shell.nix
ifdef NIXOS
STACK_FLAGS = --no-nix --system-ghc
endif

hoogle: requires_nix_shell
	hoogle server --local --port=8070 > /dev/null &

pab_servers_all: | requires_nix_shell pab_db install_contracts
	@echo "Starting plutus servers."
	$(PAB) all-servers

pab_servers_client: | requires_nix_shell pab_db install_contracts
	@echo "Starting plutus client servers"
	$(PAB) client-services

pab_db: requires_nix_shell
	@echo "Generating pab db at ${PAB_DB_PATH}"
	$(PAB) migrate ${PAB_DB_PATH}

clean_db: requires_nix_shell
	@echo "Cleaning db file at ${PAB_DB_PATH}"
	rm $(PAB_DB_PATH)

ifdef FLAGS
GHC_FLAGS = --ghc-options "$(FLAGS)"
endif

build: requires_nix_shell
	cabal v2-build $(GHC_FLAGS)

watch: requires_nix_shell
	while sleep 1; do find src trustless-sidechain.cabal | entr -cd make build; done

test: requires_nix_shell
	cabal v2-test

accept_pirs: requires_nix_shell
	stack build --test $(STACK_FLAGS) $(GHC_FLAGS) --ta '-p MarketAction --accept'

ghci: requires_nix_shell
	cabal v2-repl $(GHC_FLAGS) trustless-sidechain


# Source dirs to run fourmolu on
FORMAT_SOURCES := $(shell git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.hs' )

# Extensions we need to tell fourmolu about
FORMAT_EXTENSIONS := -o -XTemplateHaskell -o -XTypeApplications -o -XImportQualifiedPost -o -XPatternSynonyms -o -fplugin=RecordDotPreprocessor

# Run fourmolu formatter
format: requires_nix_shell
	fourmolu --mode inplace --check-idempotence $(FORMAT_EXTENSIONS) $(FORMAT_SOURCES)

# Check formatting (without making changes)
format_check: requires_nix_shell
	fourmolu --mode check --check-idempotence $(FORMAT_EXTENSIONS) $(FORMAT_SOURCES)

# Execute CI
ci: 
	nix-build ./nix/ci.nix

NIX_SHELL = nix develop
HLS_SHELL = $(NIX_SHELL) -c nix-shell -p bashInteractive haskell-language-server

shell:
	$(NIX_SHELL)

hls_shell:
	$(HLS_SHELL)

code:
	$(HLS_SHELL) --run "code ."

# Nix files to format
NIX_SOURCES := $(shell git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.nix' )

nixfmt: requires_nix_shell
	nixfmt $(NIX_SOURCES)

nixfmt_check: requires_nix_shell
	nixfmt --check $(NIX_SOURCES)

# Check with hlint, currently I couldn't get --refactor to work
lint: requires_nix_shell
	hlint $(FORMAT_SOURCES)

readme_contents:
	echo "this command is not nix-ified, you may receive an error from npx"
	npx markdown-toc ./README.md --no-firsth1

# Target to use as dependency to fail if not inside nix-shell
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "    run 'nix develop' first" && false)


PLUTUS_BRANCH = $(shell jq '.plutus.branch' ./nix/sources.json )
PLUTUS_REPO = $(shell jq '.plutus.owner + "/" + .plutus.repo' ./nix/sources.json )
PLUTUS_REV = $(shell jq '.plutus.rev' ./nix/sources.json )
PLUTUS_SHA256 = $(shell jq '.plutus.sha256' ./nix/sources.json )

update_plutus:
	@echo "Updating plutus version to latest commit at $(PLUTUS_REPO) $(PLUTUS_BRANCH)"
	niv update plutus
	@echo "Latest commit: $(PLUTUS_REV)"
	@echo "Sha256: $(PLUTUS_SHA256)"
	@echo "Make sure to update the plutus rev in stack.yaml with:"
	@echo "    commit: $(PLUTUS_REV)"
	@echo "This may require further resolution of dependency versions."

################################################################################
# Utils

build_path = dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/trustless-sidechain-0.1
clear_build:
	@[ ! -e $(build_path) ] || rm -rf $(build_path)

################################################################################
# Docs

DIAGRAMS := docs/eutxo-design
DOT_INPUTS := $(wildcard $(DIAGRAMS)/*.dot )
DOT_SVGS := $(patsubst %.dot, %.svg, $(DOT_INPUTS))
DOT_PNGS := $(patsubst %.dot, %.png, $(DOT_INPUTS))


diagram_pngs: $(DOT_PNGS)
diagrams: $(DOT_SVGS)

clean_diagrams:
	rm $(DOT_SVGS)
	rm $(DOT_PNGS)

# This doesn't work for now, some issue with resvg not loading fonts
%.png: %.svg
	convert $< $@

%.svg: %.dot
	dot -Tsvg $< -o $@

