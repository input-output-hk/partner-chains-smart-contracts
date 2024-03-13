#!/usr/bin/env bash

set -e

semVer="$(jq .version package.json)"
gitHash="$(nix flake metadata | grep 'Revision:' | awk '{print $2}')"

template="src/TrustlessSidechain/.CLIVersion.purs.template"
file="src/TrustlessSidechain/CLIVersion.purs"

cp $template $file

sed -i "s/gitHash =.*/gitHash = \"$gitHash\"/" $file
sed -i "s/semVer =.*/semVer = $semVer/" $file
