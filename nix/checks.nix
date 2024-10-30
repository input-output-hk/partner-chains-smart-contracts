{ inputs
, self
, pkgs
, offchain
, ...
}: {
  pre-commit-check = inputs.pre-commit-hooks.lib.run {
    src = ./.;
    hooks = {
      fourmolu.enable = true;
      shellcheck.enable = true;
      cabal-fmt.enable = true;
      # existed in iogx but not here, do we need it?
      #optipng.enable = true;
      nixpkgs-fmt.enable = true;
      purs-tidy.enable = true;
    };
  };

  upToDatePlutusScriptCheck = pkgs.runCommand "up-to-date-plutus-scripts-check"
    {
      nativeBuildInputs =
        self.devShells.${pkgs.system}.default.nativeBuildInputs
        ++ self.devShells.${pkgs.system}.default.buildInputs;
    } ''
    export LC_CTYPE=C.UTF-8
    export LC_ALL=C.UTF-8
    export LANG=C.UTF-8
    export IN_NIX_SHELL='pure'

    # Acquire temporary files..
    TMP_PURS=$(mktemp)
    TMP_RS=$(mktemp)

    # Setup temporary files cleanup
    function cleanup() {
    rm -rf $TMP_PURS
    }
    trap cleanup EXIT

    pushd ${self}/onchain > /dev/null
    ${
      self.packages."trustless-sidechain:exe:trustless-sidechain-serialise"
    }/bin/trustless-sidechain-serialise \
    --purescript-plutus-scripts="$TMP_PURS"
    ${
      self.packages."trustless-sidechain:exe:trustless-sidechain-serialise"
    }/bin/trustless-sidechain-serialise \
    --rust-plutus-scripts="$TMP_RS"
    popd > /dev/null

    pushd ${self}/offchain > /dev/null

    # Compare the generated file and the file provided in the repo.
    cmp $TMP_PURS src/TrustlessSidechain/RawScripts.purs || {
    exitCode=$? ;
    echo "Plutus scripts out of date." ;
    echo 'See `offchain/src/TrustlessSidechain/RawScripts.purs` for instructions to resolve this' ;
    exit $exitCode ;
    }

    popd > /dev/null

    pushd ${self}/raw-scripts > /dev/null

    # Compare the generated file and the file provided in the repo.
    cmp $TMP_RS src/lib.rs || {
    exitCode=$? ;
    echo "Plutus scripts out of date." ;
    echo 'See `raw-scripts/src/lib.rs` for instructions to resolve this' ;
    exit $exitCode ;
    }

    popd > /dev/null

    touch $out
  '';
  partner-chains-smart-contracts =
    pkgs.runCommand "pc-contracts-check"
      {
        src = offchain.src;
        nativeBuildInputs = with self.packages; [
          ogmios

          kupo
          cardano-testnet
          cardano-node
          cardano-cli
        ];
      } ''

        cp -r ${offchain.compiled}/* .


        # Does the following step really make sense? comes from the CTL...
        cp -r ${../offchain}/* .
        export LC_ALL=C.UTF-8

        ${pkgs.nodejs-18_x}/bin/node --enable-source-maps -e 'import("./output/Test.Main/index.js").then(m => m.main())'

        touch $out
      '';
}
