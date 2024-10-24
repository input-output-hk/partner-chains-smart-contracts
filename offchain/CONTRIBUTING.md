# Prerequisites
Please make sure you
- don't have `node_modules` inside the ctl directory,
- have `spago-packages.nix` inside the ctl directory,

before running `nix develop`.

The workflow in this module relies entirely on `nix` and `spago`. The only viable use for `npm` is
when you want to update js dependencies or run npm scripts in your editor. The npm scripts simply
call to `make`. The workflow for updating/adding dependencies is outlined below and in more detail
in [the ctl docs](https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/doc).

# General Rules of Thumb
* The Makefile is the central source of truth for how to deal with the environment.
* Never invoke `npm install` without `--package-lock-only`.

# Build/Test/Run
* `make format` for formatting
* `make test` for running tests
* `make build` will output bundled example module to dist
* Note `BROWSER_RUNTIME` env variable should be set to 1 to build project properly for browser (for
node environments leave unset or set 0)

# Adding PS/JS dependencies

## Purescript
Run `make spago2nix` from within a nix shell.

We rely on `spago2nix`, because spago alone is deficient inside a nix-based sandboxing. `spago2nix`
creates an autogenerated and required nix file: `spago-packages.nix` which gets imported and its
entries get converted to nix derivations. `spago` alone uses `spago.dhall` and `packages.dhall` (as
well as `package.json` which nix also uses).

This disperancy between `spago-packages.nix` and the dhall files, however, means that it is possible
for our declared Purescript dependencies to drift from the autogenerated nix file and subsequently
the Purescript derivations.

If you add a Purescript dependency, make sure to run `make spago2nix` **from within the nix shell**
to update the nix file.

Do not edit `spago-packages.nix` by hand, or the build will likely break.

## JS
Run `npm install` with the `--package-lock-only` argument.

If you add a dependency to `package.json`, make sure to update the lockfile with
`npm i --package-lock-only` **before** entering a new dev shell, otherwise the shellHook will fail.

You'll need to remove the any existing `node_modules` if you have the directory for some reason
(`npm` will try to write to `node_modules`, but will fail because the modules are symlinked to the
nix store).

After updating your `package-lock.json` with the previous `npm` invocation, remember to reload your
nix shell.

# Upgrading CTL
It's an involved process. If not followed correctly, the environment will likely break. Please
[read their docs](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/ctl-as-dependency.md#upgrading-ctl).

# Notes about CTL
* CTL is unable to compile scripts as needed. All scripts must be serialised and explicitly passed
to the CTL side. The format used is `TextEnvelope`. See:
  * [Contract.TextEnvelope](https://plutonomicon.github.io/cardano-transaction-lib/Contract.TextEnvelope.html)
  * [serialise/Main.hs](../app/serialise/Main.hs)

* When serialising scripts, make sure they take their args separately as `BuiltinData`. See:
  * [The docs](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutus-comparison.md#applying-arguments-to-parameterized-scripts)
  * [serialisableReserveAuthPolicy](../onchain/src/TrustlessSidechain/Reserve.hs)
