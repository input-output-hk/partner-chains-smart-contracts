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
  in
    pkgs.writeShellApplication {
      name = "sidechain-main-cli";
      runtimeInputs = [project.nodejs];
      # Node's `process.argv` always contains the executable name as the
      # first argument, hence passing `sidechain-main-cli "$@"` rather than just
      # `"$@"`
      text = ''
        export NODE_PATH="${project.nodeModules}/lib/node_modules"
        node --enable-source-maps -e 'require("${project.compiled}/output/Main").main()' sidechain-main-cli "$@"
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
