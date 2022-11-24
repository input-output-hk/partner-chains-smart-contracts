# Bundling

CTL can be built and bundled using Nix by running:

```shell
nix-build src/ctl-bundle-cli.nix
```

This will produce a package `ctl-scripts-<version>.tar` with the compiled `main.js` script that can be run using Node
and all necessary dependencies in `node_modules` directory. 
