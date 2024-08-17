{ system ? builtins.currentSystem
, pkgs ? import ../nix { inherit system; }
, spagoPkgs ? import ./spago-packages.nix { inherit pkgs; }
}:

let

  inherit (pkgs) npmLockToNix lib nodejs-18_x stdenv;


  nodeModules = npmLockToNix.v2.node_modules {
    src = lib.cleanSource ./.;
    nodejs = nodejs-18_x;
    sourceOverrides = {
      buildRequirePatchShebangs = true;
      node-pre-gyp = npmLockToNix.v2.packageRequirePatchShebangs;
    };
  };

  cli = stdenv.mkDerivation {
    name = "cli";
    src = lib.cleanSource ./.;

    buildInputs = with spagoPkgs; [
      installSpagoStyle
      buildSpagoStyle
    ];

    nativeBuildInputs = with pkgs; [
      purescript-psa
      purs
      spago
      nodejs-18_x
    ];

    unpackPhase = ''
      cp -r $src/src .
      cp -r $src/test .
      ln -sfn ${nodeModules}/node_modules node_modules
      install-spago-style
    '';

    buildPhase = ''
      build-spago-style "./src/**/*.purs" "./test/**/*.purs"
    '';

    installPhase = ''
      mkdir -p $out
      mv output $out
    '';

  };

in
cli
