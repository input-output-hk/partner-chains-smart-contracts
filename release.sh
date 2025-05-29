#!/usr/bin/env bash

# official semver regex from https://regex101.com/r/Ly7O1x/3/
semver_pattern="^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(-((0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(\.(0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(\+([0-9a-zA-Z-]+(\.[0-9a-zA-Z-]+)*))?$"

# shellcheck disable=SC2125
cabalfile=onchain/*.cabal
hsfiles=$(find onchain/src -type f -name "*.hs")

if [[ -z $IN_NIX_SHELL ]]; then
	echo "The release script must be run from inside a nix shell"
	echo "    run 'nix develop' first"
    exit 1
fi

if [[ ($# -eq 0) || $1 == "--help" || $1 == "-h" ]]; then
    echo "Usage: $0 <semver>"
    exit 1
fi

if [[ "$1" =~ $semver_pattern ]]; then
    next_version=$1
else
    echo "ERROR: provided version ($1) is not valid semver"
fi

# if we didn't set next_version above we exit
if [ -z "${next_version+x}" ]; then exit 1; fi

# we check if release tag already exists
if git show-ref --tags "v$next_version" --quiet; then
    echo "ERROR: tag already exists for version!"
    exit 1
fi

echo "Making release changes for new release $next_version"

# shellcheck disable=SC2086
changie batch $next_version
changie merge
# shellcheck disable=SC2086
sed -i -r "s/^version:(\s*)\S+$/version:\1$next_version/" $cabalfile
# shellcheck disable=SC2086
sed -i -r "s/@since (U|u)nreleased/@since v$next_version/" $hsfiles
# shellcheck disable=SC2086
cargo set-version --manifest-path raw-scripts/Cargo.toml $next_version
