{ fetchzip, stdenv }:
let
  ogmiosLinux = fetchzip {
    url = "https://github.com/CardanoSolutions/ogmios/releases/download/v6.8.0/ogmios-v6.8.0-x86_64-linux.zip";
    hash = "sha256-PM3tB6YdFsXRxGptDuxOvLke0m/08ySy4oV1WfIu//g=";
    stripRoot = false;
    version = "6.8.0";
    name = "ogmios-6.8.0";
    postFetch = "chmod +x $out/bin/ogmios";
  };

  ogmiosDarwin = fetchzip {
    url = "https://github.com/CardanoSolutions/ogmios/releases/download/v6.8.0/ogmios-v6.8.0-aarch64-macos.zip";
    hash = "sha256-YcSUft/aH9o2F0o1CFcmrvSnSYs0RE1fPvFW6ihWVWM=";
    stripRoot = false;
    version = "6.8.0";
    name = "ogmios-6.8.0";
    postFetch = "chmod +x $out/bin/ogmios";
  };

in
if stdenv.isLinux then ogmiosLinux else ogmiosDarwin
