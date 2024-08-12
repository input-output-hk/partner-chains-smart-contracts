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
  trustless-sidechain-ctl =
    let
      project = repoRoot.nix.offchain;
      ogmios = inputs.cardano-nix.packages.${system}."ogmios-6.5.0";
      kupo = inputs.cardano-nix.packages.${system}."kupo-2.9.0";
    in
    project.runPursTest {
      testMain = "Test.Main";
      builtProject = project.compiled;
      buildInputs = [
        ogmios
        kupo
        inputs.self._packages.cardano-testnet
        inputs.self._packages.cardano-node
        inputs.self._packages.cardano-cli
        #pkgs.psmisc # breaks tests on macos
      ];
    };
}
