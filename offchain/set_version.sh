#!/nix/store/0h73sj1n8hzc6fs36cjvsvcvz3av7n47-bash-interactive-5.1-p16/bin/bash

set -e

semVer="$(jq .version package.json)"
gitHash="\"$(git rev-parse HEAD)\""

template="src/TrustlessSidechain/.CLIVersion.purs.template"
file="src/TrustlessSidechain/CLIVersion.purs"

cp $template $file

sed -i "s/gitHash =.*/gitHash = $gitHash/" $file
sed -i "s/semVer =.*/semVer = $semVer/" $file
