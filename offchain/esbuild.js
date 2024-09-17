import * as esbuild from "esbuild"

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
