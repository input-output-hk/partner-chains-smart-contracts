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
