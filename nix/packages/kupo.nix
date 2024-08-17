{ pkgs
}:

let
  kupo = pkgs.fetchzip {
    url = "https://github.com/CardanoSolutions/kupo/releases/download/v2.9/kupo-v2.9.0-x86_64-linux.zip";
    hash = "sha256:sEfaFPph1qBuPrxQzFeTKU/9i9w0KF/v7GpxxmorPWQ=";
    stripRoot = false;
    version = "2.9.0";
    name = "kupo-2.9.0";
    postFetch = "chmod +x $out/bin/kupo";
  };
in
kupo
