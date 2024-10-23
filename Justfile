# Set the shell to bash with the desired flags
set shell := ["bash", "-eu", "-o", "pipefail", "-c"]

shebang := '/usr/bin/env bash'

# Define variables at the top level without indentation
current_system := `nix eval --impure --expr 'builtins.currentSystem'`

# Define Nix sources, including all .nix files (excluding 'spago*' directories)
nix_sources := `fd --no-ignore-parent -enix --exclude='spago*' | tr '\n' ' '`

# Define Haskell-specific Fourmolu extensions as a single quoted string
FOURMOLU_EXTENSIONS := "-o -XBangPatterns -o -XTypeApplications -o -XTemplateHaskell \
-o -XImportQualifiedPost -o -XPatternSynonyms -o -fplugin=RecordDotPreprocessor"

# Variables for PureScript project
ps_sources := `fd --no-ignore-parent -epurs | tr '\n' ' '`
js_sources := `fd --no-ignore-parent -ejs -ecjs | tr '\n' ' '`
dhall_sources := `fd --no-ignore-parent -edhall | tr '\n' ' '`
purs_args := "--censor-lib"

# Serialization of Plutus on-chain scripts to PureScript
onchain_dir := "./onchain"
offchain_dir := "./offchain"
trustless_sidechain_serialise := "cabal run trustless-sidechain-serialise --"

# Output files with serialized scripts
raw_scripts_purs := offchain_dir + "/src/TrustlessSidechain/RawScripts.purs"

# Bundler setup
ps_entrypoint := "Main"
ps_test_entrypoint := "Test.Main"
ps_entrypoint_function := "main"
browser_runtime := ""  # Use "1" for true and "" for false
serve_port := "4008"

# ------------------------------------------------------------------------------
# Default Target
# ------------------------------------------------------------------------------

# The `all` recipe serves as the default target.
# It builds both the Haskell and PureScript projects.
all: build

# Builds nix versions of Project
nix-builds: nix-build-library nix-build-executables nix-build-test
# ------------------------------------------------------------------------------
# Build Targets
# ------------------------------------------------------------------------------

# Build the Haskell library with Nix.
nix-build-library:
  @echo "Building the trustless-sidechain library..."
  nix build .#trustless-sidechain:lib:trustless-sidechain

# Build the Haskell executables with Nix (also builds the test suite).
nix-build-executables:
  @echo "Building the executables for system: {{current_system}}..."
  nix build -L .#check.{{current_system}}.partner-chains-smart-contracts

# Build the Haskell tests with Nix.
nix-build-test:
  @echo "Building the trustless-sidechain test suite..."
  nix build -L .#trustless-sidechain:test:trustless-sidechain-test

# Ensure that npm dependencies are installed
npm-install:
  @if [ ! -d "{{offchain_dir}}/node_modules" ]; then \
      echo "'node_modules' not found in '{{offchain_dir}}'. Running 'npm install'..."; \
      cd {{offchain_dir}} && npm install; \
    else \
      echo "'node_modules' already exists in '{{offchain_dir}}'."; \
    fi

# Build the PureScript project
build: generate_raw_scripts_purs
  @echo "Building the PureScript project..."
  pushd {{offchain_dir}} && spago build --purs-args {{purs_args}}

# Test the PureScript project
test: generate_raw_scripts_purs
  @echo "Running PureScript tests..."
  pushd {{offchain_dir}} && spago test --purs-args={{purs_args}} -m {{ps_test_entrypoint}}

# Update scripts (depends on raw_scripts_purs)
update-scripts: generate_raw_scripts_purs

# Generate raw_scripts_purs
generate_raw_scripts_purs:
  @echo "Generating '{{raw_scripts_purs}}' with the following command..."
  # Collect Haskell scripts dynamically within the recipe
  haskell_scripts="$$(find {{onchain_dir}}/src -type f -name '*.hs' ; \
                       find {{onchain_dir}}/prelude -type f -name '*.hs' ; \
                       find {{onchain_dir}}/app/serialise -type f -name '*.hs')"
  @echo "Found Haskell scripts:"
  @echo "$$haskell_scripts"
  cd {{onchain_dir}} && {{trustless_sidechain_serialise}} \
    --purescript-plutus-scripts=../{{raw_scripts_purs}}
  @echo "Updated '{{raw_scripts_purs}}'."

# Create bundle entrypoint
create-bundle-entrypoint:
  @echo "Creating bundle entrypoint..."
  mkdir -p {{offchain_dir}}/dist/
  echo 'import("../output/Main/index.js").then(m => m.{{ps_entrypoint_function}}());' > {{offchain_dir}}/dist/entrypoint.js

# Delete bundle entrypoint
delete-bundle-entrypoint:
  @echo "Deleting bundle entrypoint..."
  rm -f {{offchain_dir}}/dist/entrypoint.js

# Bundle using esbuild
esbuild-bundle: build npm-install create-bundle-entrypoint
  @echo "Bundling using esbuild..."
  mkdir -p {{offchain_dir}}/dist/
  BROWSER_RUNTIME={{browser_runtime}} pushd {{offchain_dir}} && \
  node esbuild/bundle.js ./dist/entrypoint.js dist/pc-contracts-cli && \
  chmod +x ./dist/pc-contracts-cli
  just delete-bundle-entrypoint

# Generate spago2nix
spago2nix:
  @echo "Generating spago2nix..."
  pushd {{offchain_dir}} && spago2nix generate && nixpkgs-fmt spago-packages.nix

# Clean build artifacts
clean:
  #!{{shebang}}
  echo "Cleaning build artifacts..."
  cd ./offchain
  rm -r .psc-ide-port || true
  rm -rf .psci_modules || true
  rm -rf .spago || true
  rm -rf generated-docs || true
  rm -rf .spago2nix || true
  rm -rf node_modules || true
  rm -rf output || true
  rm -rf dist || true

# ------------------------------------------------------------------------------
# Environment Requirement
# ------------------------------------------------------------------------------

# Ensure that certain recipes are run inside a Nix shell.
requires_nix_shell:
  @if [ -z "$IN_NIX_SHELL" ]; then \
      echo "This target must be run from inside a Nix shell."; \
      echo "    Run 'nix develop' first."; \
      exit 1; \
  fi

# ------------------------------------------------------------------------------
# Onchain Directory Targets
# ------------------------------------------------------------------------------

# Hoogle server for Haskell documentation
hoogle:
  @echo "Starting Hoogle server..."
  nix develop -c hoogle server --local --port 8008

# Format .cabal files in the onchain directory
cabalfmt: requires_nix_shell
  @echo "Formatting .cabal files in onchain directory..."
  cd {{onchain_dir}} && fd -ecabal -0 | xargs -0 cabal-fmt --inplace

# Check formatting of .cabal files in the onchain directory
cabalfmt_check: requires_nix_shell
  @echo "Checking format of .cabal files in onchain directory..."
  cd {{onchain_dir}} && fd -ecabal -0 | xargs -0 cabal-fmt --check

# Format Haskell source files in the onchain directory
format-onchain: requires_nix_shell
  @echo "Formatting Haskell source files in onchain directory..."
  cd {{onchain_dir}} && fd -ehs -elhs -0 \
    | xargs -0 fourmolu {{FOURMOLU_EXTENSIONS}} --mode inplace --check-idempotence

# Check formatting of Haskell source files in the onchain directory
format-check-onchain: requires_nix_shell
  @echo "Checking format of Haskell source files in onchain directory..."
  cd {{onchain_dir}} && fd -ehs -elhs -0 \
    | xargs -0 fourmolu {{FOURMOLU_EXTENSIONS}} --mode check --check-idempotence

# Lint Haskell source files in the onchain directory
lint-onchain: requires_nix_shell
  @echo "Running hlint on Haskell source files in onchain directory..."
  cd {{onchain_dir}} && fd -ehs -elhs -0 | xargs -0 hlint --no-summary

# Start a ghci REPL inside the nix environment for the onchain project
nix-cabal-repl:
  @echo "Starting ghci REPL inside nix environment..."
  cd {{onchain_dir}} && nix develop -c cabal new-repl trustless-sidechain

# ------------------------------------------------------------------------------
# Formatting Targets
# ------------------------------------------------------------------------------

# Format both onchain and offchain projects
format: format-onchain format-offchain nixpkgsfmt

# Format all Haskell (.hs) files in the repository.
format-hs: requires_nix_shell
  @echo "Formatting all .hs files in the repository..."
  git ls-files -z \
  | grep -Ez '^.*\.hs$$' \
  | while IFS= read -r -d '' FILE; do \
      test -f "$$FILE" && printf "%s\0" "$$FILE"; \
    done \
  | xargs -0 -r fourmolu {{FOURMOLU_EXTENSIONS}} --mode inplace --check-idempotence

# ------------------------------------------------------------------------------
# Nixpkgs Formatting Targets
# ------------------------------------------------------------------------------

# Format all Nix source files using nixpkgs-fmt.
nixpkgsfmt: requires_nix_shell
  @echo "Formatting all Nix source files..."
  nixpkgs-fmt {{ nix_sources }}

# Check formatting of all Nix source files without modifying them.
nixpkgsfmt_check: requires_nix_shell
  @echo "Checking format of all Nix source files..."
  nixpkgs-fmt --check {{ nix_sources }}

# ------------------------------------------------------------------------------
# Nix Flake Locking Targets
# ------------------------------------------------------------------------------

# Lock Nix flake dependencies to ensure reproducible builds.
lock: requires_nix_shell
  @echo "Locking Nix flake dependencies..."
  nix flake lock

# Check if the Nix flake lock file is up-to-date without updating it.
lock_check: requires_nix_shell
  @echo "Checking if Nix flake lock file is up-to-date..."
  nix flake lock --no-update-lock-file

# ------------------------------------------------------------------------------
# Additional Formatting Checks
# ------------------------------------------------------------------------------

# Check for whitespace errors in staged changes.
check-format-whitespace: requires_nix_shell
  @echo "Checking for whitespace errors in staged changes..."
  git diff --check --cached HEAD --

# Check formatting for PureScript, JavaScript, and Dhall files
check-format-offchain: requires_nix_shell npm-install
  @echo "Checking formatting for PureScript, JavaScript, and Dhall files..."
  purs-tidy check {{ps_sources}}
  eslint -c {{offchain_dir}}/eslint.config.mjs {{js_sources}} && dhall lint --check {{dhall_sources}}

# Format PureScript, JavaScript, and Dhall files
format-offchain: requires_nix_shell npm-install
  @echo "Formatting PureScript, JavaScript, and Dhall files..."
  purs-tidy format-in-place {{ps_sources}}
  eslint -c {{offchain_dir}}/eslint.config.mjs --fix {{js_sources}} && dhall lint {{dhall_sources}}

# ------------------------------------------------------------------------------
# Other Targets
# ------------------------------------------------------------------------------

# Publish nodejs package to npmjs.com
publish: build
 cd {{offchain_dir}} && npm publish