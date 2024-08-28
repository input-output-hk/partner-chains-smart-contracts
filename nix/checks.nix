{ repoRoot
, inputs
, pkgs
, lib
, system
, ...
}: {
  upToDatePlutusScriptCheck =
    let
      hsProject = repoRoot.nix.onchain.flake;
    in
    pkgs.runCommand "up-to-date-plutus-scripts-check"
      {
        nativeBuildInputs =
          inputs.self.devShells.hs.nativeBuildInputs
          ++ inputs.self.devShells.ps.nativeBuildInputs
          ++ inputs.self.devShells.ps.buildInputs;
      } ''
      export LC_CTYPE=C.UTF-8
      export LC_ALL=C.UTF-8
      export LANG=C.UTF-8
      export IN_NIX_SHELL='pure'

      # Acquire temporary files..
      TMP=$(mktemp)

      # Setup temporary files cleanup
      function cleanup() {
      rm -rf $TMP
      }
      trap cleanup EXIT

      pushd ${inputs.self}/onchain > /dev/null
      ${
        hsProject.packages."trustless-sidechain-serialise"
      }/bin/trustless-sidechain-serialise \
      --purescript-plutus-scripts="$TMP"
      popd > /dev/null

      pushd ${inputs.self}/offchain > /dev/null

      # Compare the generated file and the file provided in the repo.
      cmp $TMP src/TrustlessSidechain/RawScripts.purs || {
      exitCode=$? ;
      echo "Plutus scripts out of date." ;
      echo 'See `offchain/src/TrustlessSidechain/RawScripts.purs` for instructions to resolve this' ;
      exit $exitCode ;
      }

      popd > /dev/null

      touch $out
    '';
  partner-chains-smart-contracts =
    let
      project = repoRoot.nix.offchain;
    in
    pkgs.runCommand "pc-contracts-check"
      {
        src = project.src;
        nativeBuildInputs = with inputs.self.packages; [
          ogmios
          kupo
          cardano-testnet
          cardano-node
          cardano-cli
        ];
      } ''

        cp -r ${project.compiled}/* .

        # Does the following step really make sense? comes from the CTL...
        cp -r ${../offchain}/* .
        export LC_ALL=C.UTF-8

        ${pkgs.nodejs-18_x}/bin/node --enable-source-maps -e 'import("./output/Test.Main/index.js").then(m => m.main())'

        touch $out
      '';
}
