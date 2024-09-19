#!/usr/bin/env bash

# offical semver regex from https://regex101.com/r/Ly7O1x/3/
semver_pattern="^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(-((0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(\.(0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(\+([0-9a-zA-Z-]+(\.[0-9a-zA-Z-]+)*))?$"
packagejson=offchain/package.json
changelog=CHANGELOG.md
current_version=$(cat $packagejson | jq -r ".version")
cabalfile="onchain/*.cabal"
hsfiles=$(find onchain/src -type f -name "*.hs")

if [[ -z $IN_NIX_SHELL ]]; then
	echo "The release script must be run from inside a nix shell"
	echo "    run 'nix develop' first"
    exit 1
fi

if [[ ($# -eq 0) || $1 == "--help" || $1 == "-h" ]]; then
    echo "Usage: $0 [next-major|next-minor|next-patch|<semver>]"
    exit 1
fi

# check if there are any modified files (based on https://stackoverflow.com/a/3879077)
git update-index --refresh &> /dev/null
git diff-index --quiet HEAD --
if [[ ($? -eq 1) ]]; then
    echo "Release script can only run on a clean repo. Please stash your changes."
    exit 1
fi

if [[ $(git rev-parse --abbrev-ref HEAD) != "master" ]]; then
    echo "WARNING: you are trying to cut a release from a branch other than master!"
    read -r -n1 -p "Do you wish to continue? [y/n]" yesno
    printf "\n"
    if [[ "$yesno" =~ ^[^Yy]$ ]]; then
        echo "Aborting..."
        exit 1
    fi
fi

case $1 in
    next-major | next-minor | next-patch)
        if [[ "$current_version" =~ $semver_pattern ]]; then
            case $1 in
                next-major) next_version="$((BASH_REMATCH[1]+1)).0.0" ;;
                next-minor) next_version="${BASH_REMATCH[1]}.$((BASH_REMATCH[2]+1)).0" ;;
                next-patch) next_version="${BASH_REMATCH[1]}.${BASH_REMATCH[2]}.$((BASH_REMATCH[3]+1))" ;;
            esac
        else
            echo "FATAL: version number in $packagejson is not valid semver!"
        fi
    ;;
    *)
        if [[ "$1" =~ $semver_pattern ]]; then
            next_version=$1
        else
            echo "ERROR: provided version ($1) is not valid semver"
        fi
    ;;
esac

# if we didn't set next_version above we exit
if [ -z "${next_version+x}" ]; then exit 1; fi

# we check if release tag already exists
if git show-ref --tags "v$next_version" --quiet; then
    echo "ERROR: tag already exists for version!"
    exit 1
fi

echo "Making release changes for new release $next_version (last version: $current_version)"

cat $packagejson | jq ".version=\"$next_version\"" | sponge $packagejson
sed -i "s/# Unreleased/# Unreleased\n\n# v$next_version/" $changelog
sed -i -r "s/^version:(\s*)\S+$/version:\1$next_version/" "$cabalfile"
# shellcheck disable=SC2086
sed -i -r "s/@since (U|u)nreleased/@since v$next_version/" $hsfiles

git checkout -b "release-v$next_version"
git add .
git commit -m "chore: bump version to v$next_version"
