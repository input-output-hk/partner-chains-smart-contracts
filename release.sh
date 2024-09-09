#!/usr/bin/env bash

version=$1
packagejson=offchain/package.json
changelog=CHANGELOG.md

cat $packagejson | jq ".version=\"$version\"" | sponge $packagejson
sed -i "s/# Unreleased/# Unreleased\n\n# v$version/" $changelog
