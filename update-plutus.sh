#!/usr/bin/env bash

# Check args are correct
if [[ $# -ne 1 ]]; then
    echo "Incorrect usage: update-plutus.sh {ref}" >&2
    exit 2
fi

# Download the new cabal.project file from plutus
PLUTUS_CABAL=$(curl -s https://raw.githubusercontent.com/input-output-hk/plutus/$1/cabal.project)

# Ensure download worked
if [[ $PLUTUS_CABAL == "404: Not Found" ]]; then
    echo "No cabal.project file could be found for ref: \"$1\"" >&2
    exit 2
fi

# Gets new index state
INDEX=$(echo "$PLUTUS_CABAL" | head -2)

# Gets old cabal file
OLD_CABAL=$(<cabal.project)

# Gets the old packages definition
OWN_PACKAGES_START=$(echo "$OLD_CABAL" | grep -n -m 1 "packages:" | cut -d: -f1)
OWN_PACKAGES=$(echo "$OLD_CABAL" | tail -n +"$OWN_PACKAGES_START" | sed -n '1,/^[[:space:]]*$/ p')

# Makes the source repo package def for plutus, without the subdirs
PLUTUS_SOURCE=$'\nsource-repository-package\n  type: git\n  location: https://github.com/input-output-hk/plutus.git\n  tag: '$1$'\n  subdir:'

# Finds where the "packages" definition in new cabal starts
PACKAGES_START=$(echo "$PLUTUS_CABAL" | grep -n -m 1 "packages:" | cut -d: -f1)

# Gets the list of packages from the new cabal.project, along with the rest of the file
PACKAGES=$(echo "$PLUTUS_CABAL" | tail -n +"$PACKAGES_START")
PACKAGES=${PACKAGES:9}

# Combines all the strings together to create new project.cabal
NEW_CABAL="${INDEX}\n\n${OWN_PACKAGES}\n${PLUTUS_SOURCE}${PACKAGES}"

# Write cabal.project to file
echo -e "$NEW_CABAL" > './cabal.project'
echo "Successfully updated cabal.project, updating sources..."

# Updates sources.json using Kirills script (which now also updates owner)
sh ./update-sha256map.sh

echo "Plutus successfully updated to $1, good luck with the API changes :)"
