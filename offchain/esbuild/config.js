import { wasmLoader } from "esbuild-plugin-wasm";
import { polyfillNode } from "esbuild-plugin-polyfill-node";
import { replace } from "esbuild-plugin-replace";
import pkg from "../package.json" with { type: "json" };

const isBrowser = !!process.env.BROWSER_RUNTIME;

export const buildOptions = ({ entryPoint, outfile }) => {
  const config = {
    entryPoints: [entryPoint],
    outfile: outfile,
    plugins: [
      wasmLoader({
        mode: "deferred"
      }),
      replace({
        '__semVer': pkg.version,
      }),
    ],
    bundle: true,
    platform: isBrowser ? "browser" : "node",
    format: "esm",
    treeShaking: true,
    logLevel: "error",
    minify: true,
    banner: !isBrowser
      ? {
          js: "#!/usr/bin/env node\n",
        }
      : {},
  };

  // https://esbuild.github.io/api/#packages
  if (!isBrowser) {
    // Keep dependencies outside of the bundle for nodejs
    config.packages = "external";
  } else {
    // Provide browser polyfills for NodeJS modules
    config.plugins.push(
      polyfillNode({
        polyfills: {
          crypto: true,
          fs: true,
          os: true
        }
      })
    );
  }

  return config;
};
