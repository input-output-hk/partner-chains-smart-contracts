{ fetchzip, stdenv }:

let

  kupoLinux = fetchzip {
    url = "https://github.com/CardanoSolutions/kupo/releases/download/v2.9/kupo-v2.9.0-x86_64-linux.zip";
    hash = "sha256:sEfaFPph1qBuPrxQzFeTKU/9i9w0KF/v7GpxxmorPWQ=";
    stripRoot = false;
    version = "2.9.0";
    name = "kupo-2.9.0";
    postFetch = "chmod +x $out/bin/kupo";
  };

  kupoDarwin = fetchzip {
    url = "https://github.com/CardanoSolutions/kupo/releases/download/v2.9/kupo-v2.9.0-aarch64-macos.zip";
    hash = "sha256:1d18mpvmjiafy56pjljf46r1nh7ma44k29jzwk3bpr22ra9dvi0x";
    stripRoot = false;
    version = "2.9.0";
    name = "kupo-2.9.0";
    postFetch = "chmod +x $out/bin/kupo";
  };

in
if stdenv.isDarwin then kupoDarwin else kupoLinux
