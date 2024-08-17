{ pkgs }:
let
  ogmios = pkgs.fetchzip {
    url = "https://github.com/CardanoSolutions/ogmios/releases/download/v6.5.0/ogmios-v6.5.0-x86_64-linux.zip";
    hash = "sha256-C7vwUefYXCXhnfIUt/Kmj3/f4cd3IogAZxaBtDftUOU=";
    stripRoot = false;
    version = "6.5.0";
    name = "ogmios-6.5.0";
    postFetch = "chmod +x $out/bin/ogmios";
  };
in
ogmios
