#!/usr/bin/env bash

# offical semver regex from https://regex101.com/r/Ly7O1x/3/
semver_pattern="^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(-((0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(\.(0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(\+([0-9a-zA-Z-]+(\.[0-9a-zA-Z-]+)*))?$"
packagejson=offchain/package.json
changelog=CHANGELOG.md
current_version=$(cat $packagejson | jq -r ".version")

if [[ ($# -eq 0) || $1 == "--help" || $1 == "-h" ]]; then
    echo "Usage: $0 [next-major|next-minor|next-patch|<semver>]"
    exit 1
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

if [ -z "${next_version+x}" ]; then exit 1; fi

echo "Making release changes for new release $next_version (last version: $current_version)"

cat $packagejson | jq ".version=\"$next_version\"" | sponge $packagejson
sed -i "s/# Unreleased/# Unreleased\n\n# v$next_version/" $changelog
