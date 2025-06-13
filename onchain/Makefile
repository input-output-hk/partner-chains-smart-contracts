all: onchain check-raw-scripts

onchain-dir := ../onchain/
trustless-sidechain-serialise=cabal run trustless-sidechain-serialise --
# Haskell source dependencies include shared Prelude libraries and the
# serialization binary
haskell-scripts := $(shell find $(onchain-dir)/src -type f -name "*.hs") \
	                 $(shell find $(onchain-dir)/prelude -type f -name "*.hs") \
                   $(shell find $(onchain-dir)/app/serialise -type f -name "*.hs")
# Output files with serialized scripts
raw-scripts-rs := ../raw-scripts/src/lib.rs
raw-scripts-rs-trace := ../raw-scripts-traced/src/lib.rs

onchain:
	cabal clean
	cabal configure --flags="-Trace"
	cabal update
	cabal build
	cabal test all --test-show-details=direct

update-scripts: ${raw-scripts-rs}

update-scripts-trace: ${raw-scripts-rs-trace}
${raw-scripts-rs}: ${haskell-scripts}
	@echo 'Generating `$@`...'
	${trustless-sidechain-serialise} --rust-plutus-scripts=${CURDIR}/${raw-scripts-rs}
	@echo 'Updated `$@`.'

${raw-scripts-rs-trace}: ${haskell-scripts}
	@echo 'Generating `$@`...'
	${trustless-sidechain-serialise-trace} --rust-plutus-scripts=${CURDIR}/${raw-scripts-rs-trace}
	@echo 'Updated `$@`.'

check-raw-scripts:
	@make -B update-scripts
	@if ! git diff --quiet -- ../raw-scripts/src/lib.rs ; then \
	  echo \
	  echo "Error: raw-scripts/src/lib.rs is not up to date"; \
	  echo \
	  exit 1; \
	fi

	cabal clean
	cabal update
	cabal configure --flags="Trace"
	cabal build
	@make -B update-scripts-trace
	@if ! git diff --quiet -- ../raw-scripts/src/lib-trace.rs ; then \
	  echo \
	  echo "Error: raw-scripts-traced/src/lib.rs is not up to date"; \
	  echo \
	  exit 1; \
	fi
