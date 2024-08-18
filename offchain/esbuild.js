import * as esbuild from "esbuild"
import { wasmLoader } from "esbuild-plugin-wasm";
import { polyfillNode } from "esbuild-plugin-polyfill-node";

esbuild.build({
  entryPoints: [ './entry.js' ],
  outfile: 'dist/index.js',
  bundle: true,
  platform: 'node',
  format: 'esm',
  treeShaking: true,
  logLevel: 'error',
  minify: true,
  packages: 'external',
})
