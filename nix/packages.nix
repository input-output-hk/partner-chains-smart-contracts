{
  repoRoot,
  inputs,
  pkgs,
  lib,
  system,
  ...
}: rec {
  sidechain-main-cli = let
    project = repoRoot.nix.lib.mkPurescriptProject;

    bundled = pkgs.runCommand "sidechain-bundled" {}
    ''
      mkdir -p $out
      cp -R ${project.compiled}/* $out
      chmod -R u+rw $out/output
      ln -s ${project.nodeModules}/lib/node_modules $out/output/node_modules
    '';
  in
    pkgs.writeShellApplication {
      name = "sidechain-main-cli";
      text = ''
      ${project.nodejs}/bin/node -e 'import("${bundled}/output/Main/index.js").then(m => m.main())' sidechain-main-cli "$@"
      '';
    };

  sidechain-main-cli-image = inputs.n2c.packages.nix2container.buildImage {
    name = "sidechain-main-cli-docker";
    tag = "${inputs.self.shortRev or inputs.self.dirtyShortRev}";
    config = {
      Cmd = ["sidechain-main-cli"];
    };
    copyToRoot = pkgs.buildEnv {
      name = "root";
      paths = [pkgs.bashInteractive pkgs.coreutils sidechain-main-cli];
      pathsToLink = ["/bin"];
    };
  };
}
