# Off-chain Operations

This document describes usage of the partner chains CLI.  This command line tool
provides a set of commands (also known as "endpoints"), that are used to manage
the Cardano components of a partner chain by submitting transactions to the
Cardano network.

**NOTE:** terms _partner chain_ and _sidechain_ are used interchangeably in this
document.

## 1. Development

If you want to develop for this submodule, please consult the notes in
[CONTRIBUTING.md](./CONTRIBUTING.md) before setting up your environment.

## 2. Environment setup

In order to execute off-chain commands with the CLI, you need to setup the
runtime dependencies:

- cardano-node
- ogmios
- kupo

### 2.1. Configuring hosted runtime dependencies

In case you are running the runtime dependencies (ogmios and kupo) on a hosted
environment, or anything else than the default settings, you can either configure
it via CLI arguments, or set these in the configuration file (see Section 3.1).

The arguments for ogmios and kupo are using the following scheme:

```
  --ogmios-host localhost  Address host of ogmios (default: "localhost")
  --ogmios-path some/path  Address path of ogmios
  --ogmios-port 1234       Port of ogmios (default: 1337u)
  --ogmios-secure          Whether ogmios is using an HTTPS connection
```

So in case you want to use a remote ogmios service on `https://1.2.3.4:5678`,
you want to use the following arguments:
```
nix run .#pc-contracts-cli -- [...] --ogmios-host 1.2.3.4 --ogmios-port 5678 --ogmios-secure
```
where `[...]` represents a command and command-specific flags and options.

See [3.1. Using a configuration file](#3.1.-using-a-configuration-file) on how
to use a configuration file instead of command-line options.

## 3. Running the CLI

You can run the CLI tool either through Nix or by directly running the compiled
executables (these take the form of JavaScript files).

When using `nix`, commands and options passed to the CLI must be preceeded by
`--`.  For example, to display the list of all available commands one needs to
execute:

```
nix run .#pc-contracts-cli -- --help
```

**TODO**: instructions below on bundling and running JavaScript do not work and
need to be updated/rewritten.

**Bundle to a JavaScript file and run using node:**
```shell
nix build .#pc-contracts-release-bundle
```

This will produce an archive `release.zip` with the
compiled `.index.mjs` with a wrapped bash script that can be run using Node and all necessary
dependencies in `node_modules` directory.
```
./pc-contracts-cli --help
```

### 3.1. Using a configuration file

When running the CLI one needs to pass a single command (see section 3.3 below)
followed by options.  A set of options related to defining the partner chain
parameters is used by all commands.  Instead of having to pass these options on
the command line with every call, it is easier to put them in a configuration
file `$CWD/config.json` in the following format:

```json
  "sidechainParameters": {
    "chainId": 123,
    "genesisUtxo": "3824c3a7c4437cc6ca4f893cd1519ae1dbe77862304e14d910ddc1f32de69b60#1",
    "threshold": {
      "numerator": 2,
      "denominator": 3
    },
    "governanceAuthority": "4f2d6145e1700ad11dc074cad9f4194cc53b0dbab6bd25dfea6c501a"
  },
  "runtimeConfig": {
    "network": "testnet",
    "ogmios": {
      "host": "localhost",
      "port": 1337,
      "secure": false,
      "path": null
    },
    "kupo": {
      "host": "localhost",
      "port": 1442,
      "secure": false,
      "path": null
    }
  },
  "paymentSigningKeyFile": "/absolute/path/to/payment.skey",
  "stakeSigningKeyFile": "/optional/path/to/stake.skey"
```

This allows to shorten a CLI call from:
```
nix run .#pc-contracts-cli -- deregister \
  --sidechain-id 123 \
  --genesis-committee-hash-utxo 3824c3a7c4437cc6ca4f893cd1519ae1dbe77862304e14d910ddc1f32de69b60#1 \
  --threshold-numerator 2 \
  --threshold-denominator 3 \
  --governance-authority 4f2d6145e1700ad11dc074cad9f4194cc53b0dbab6bd25dfea6c501c \
  --network testnet \
  --ogmios-host localhost \
  --ogmios-port 1442 \
  --ogmios-secure \
  --kupo-host localhost \
  --kupo-port 1337 \
  --kupo-secure \
  --payment-signing-key-file payment.skey \
  --stake-signing-key-file stake.skey \
  --spo-public-key aabbcc \
  --ada-based-staking
```

to:

```
nix run .#pc-contracts-cli -- deregister \
  --spo-public-key aabbcc
```

**We henceforth assume that these common options are located in the
configuration file and omit them from any further examples.  Thus, all provided
options are specific to a particular command being demonstrated.**

You can find a sample configuration file in `config.example.json`.

When using the CLI arguments and the configuration file together, the **CLI
arguments override** values in the configuration file. You can also set any of
the above values to null, if you don't want to set a value for that property.
Any configuration entry where no configuration is defined will fallback to its
default value.

### 3.2. Using the CLI commands

Notes:

- `genesis-committee-hash-utxo` is pinned to the partner chain parameters, so we
  have to add an arbitrary UTxO here.

- If not using a config file, prior to running the contracts it may be desirable
  to have available your signing key in the environment. Example:
  ```bash
  export SIGNING_KEY=/Users/gergo/Dev/cardano/testnets/addresses/server.skey
  ```

  and then pass it on the command line as `--payment-signing-key-file
  $SIGNING_KEY`.

Available commands:
```
  init                     Initialise sidechain
  init-tokens-mint         Mint all sidechain initialisation tokens
  init-reserve-management  Initialise native token reserve management system
  addresses                Get the script addresses for a given sidechain
  register                 Register a committee candidate
  deregister               Deregister a committee member
  reserve-create           Create a new token reserve
  reserve-handover         Empty and remove an existing reserve
  reserve-deposit          Deposit assets to existing reserve
  reserve-release-funds    Release currently available funds from an existing
                           reserve
  insert-version-2         Initialize version 2 of a protocol
  update-version           Update an existing protocol version
  invalidate-version       Invalidate a protocol version
  list-versioned-scripts   Get scripts (validators and minting policies) that
                           are currently being versioned
  insert-d-parameter       Insert new D parameter
  update-d-parameter       Update a D parameter
  update-permissioned-candidates
                           Update a Permissioned Candidates list
  init-token-status        List the number of each init token the wallet still
                           holds
  cli-version              Display semantic version of the CLI and its git hash
```

#### 3.2.1.1 Initialising the sidechain in full

The `init` command initializes all components of a sidechain.  It first burns
the genesis UTxO and mints required initialization tokens, and then uses these
initialization tokens to initialize the following functionalities of a
sidechain:

  * candidate permission tokens (optional - see below)
  * native token reserve management system

To initialise the sidechain, we run the following command:
```
nix run .#pc-contracts-cli -- init \
  --committee-pub-key aabbcc \
  --committee-pub-key ccbbaa \
  --sidechain-epoch 0 \
  --version 1
```

The `init` command is idempotent, i.e. in case of failure it can safely be
re-run to finish an interrupted sidechain initialization.

#### 3.2.1.2 Initialising the sidechain in parts

As an alternative to using the `init` command, the user might want to perform
each initialization step individually.

##### Mint initialization tokens

A mandatory first step is to mint the so called "initialization tokens" (or
"init tokens" for short).  These tokens are required to subsequently initialize
desired components of a partner chain.  The command mints all tokens, regardless
of whether there's an intention to use all of them.

Spends Genesis Utxo

Mints:
* `"Committee oracle InitToken"`
* `"Version oracle InitToken"` (multiple tokens)

```
nix run .#pc-contracts-cli -- init-tokens-mint --version 1
```

#### 3.2.2. List currently versioned scripts

```
nix run .#pc-contracts-cli -- list-versioned-scripts \
  --version 1
```

Returns the list of currently versioned scripts.

More specifically, it returns all scripts that were inserted in the version
oracle. This includes an initial set of scripts that were added by the `init`
command, and the scripts that were added when inserting a new protocol version
using the `insert-version`.

#### 3.2.3. Get script addresses of a sidechain

Script addresses depend on the sidechain parameters, so we get different
addresses for different parameters. To get the script addresses for a given
sidechain, you can use the following command:

```
nix run .#pc-contracts-cli -- addresses \
  --version 1
```

#### 3.2.4. Register committee candidate

```
nix run .#pc-contracts-cli -- register \
  --spo-public-key 67663ee94098ceca0dacbf7f947946bfdc4de1848d76da5249b1c3a18a41a57a \
  --sidechain-public-key 02599181389043ba0b83e53d3d665c2dfaa187453a24a4538723766f8f0509c55d \
  --spo-signature cf5fc5b10dff794ac0f5908c38d28a1d8e8430f17c2036cf14f4b28c990b6794f754ca809d69ecd52e4c4d542f90c43b017ff7f23cf46efc4d8f6b07a3895403 \
  --sidechain-signature 9da47b68b68cbca5cbaf7a0bd2a2bfedfe8c58e90ab8a709b8aed4c1644065885823203526b331284d15b238d11a60abb5c0cb3a8f2ef1102fbae736d98146bd \
  --registration-utxo a03ebf281ed96549f74d0e724841fcf928194c44f6ff9a8056d1829598042c62#1
```

#### 3.2.5. Deregister committee candidate

```
nix run .#pc-contracts-cli -- deregister \
  --spo-public-key aabbcc
```

#### 3.2.6 Create a new token reserve

```
nix run .#pc-contracts-cli -- reserve-create \
   --total-accrued-function-script-hash SCRIPT-HASH \
   --reserve-posixtime-t0 POSIXTIME \
   --reserve-asset-script-hash ASSET-SCRIPT-HASH \
   --reserve-asset-name RESERVE_ASSET_NAME \
   --reserve-initial-deposit-amount RESERVE-DEPOSIT-AMOUNT
```

Instead of `--reserve-asset-script-hash` and `--reserve-asset-name` one might
specify `--reserve-ada-asset` flag to indicate that Ada is to be used as the
reserve asset.

Optionally one might also pass `--reserve-initial-incentive-amount
RESERVE-INCENTIVE-AMOUNT` option to set the incentive, i.e. the amount of tokens
awarded for a reserve release.

#### 3.2.7 Empty and remove an existing reserve

```
nix run .#pc-contracts-cli -- reserve-handover
```

Perform the reserve handover.

#### 3.2.8 Deposit assets to existing reserve

```
nix run .#pc-contracts-cli -- reserve-deposit \
  --deposit-reserve-asset ASSET-SCRIPT-HASH \
  --reserve-asset-name RESERVE_ASSET_NAME \
  --reserve-initial-deposit-amount RESERVE-DEPOSIT-AMOUNT
```

Instead of `--deposit-reserve-asset` and `--reserve-asset-name` one might
specify `--reserve-ada-asset` flag to indicate that Ada is being used as the
reserve asset.

#### 3.2.9 Release currently available funds from an existing reserve

```
nix run .#pc-contracts-cli -- reserve-release-funds \
  --total-accrued-till-now INT \
  --reserve-transaction-input RESERVE-TRANSACTION-INPUT
```

#### 3.2.10 Insert new protocol version

This command is only for testing purposes and shouldn't be used.

```
nix run .#pc-contracts-cli --insert-version-2
```

#### 3.2.11 Update existing protocol version

```
nix run .#pc-contracts-cli -- update-version \
  --old-version 1 \
  --new-version 2
```

#### 3.2.12 Invalidate protocol version

```
nix run .#pc-contracts-cli -- invalidate-version \
  --version 1
```

#### 3.2.13 Insert a D parameter value

```
nix run .#pc-contracts-cli -- insert-d-parameter \
  --d-parameter-permissioned-candidates-count N \
  --d-parameter-registered-candidates-count M
```

where N and M are integers.  Note that this should be only done once and then
`update-d-parameter` value should be used (see below).  However, there is no
safeguard against inserting multiple D parameter values.

#### 3.2.14 Update a D parameter value

```
nix run .#pc-contracts-cli -- update-d-parameter \
  --d-parameter-permissioned-candidates-count N \
  --d-parameter-registered-candidates-count M
```

where N and M are integers.  If more than one D parameter value was inserted
this will remove all inserted values first and then replace them with a single
new value.

#### 3.2.15 Insert a list of permissioned candidates

```
nix run .#pc-contracts-cli -- update-permissioned-candidates \
  --add-candidate "SIDECHAIN_KEY_1:AURA_KEY_1:GRANDPA_KEY_1" \
  --add-candidate "SIDECHAIN_KEY_2:AURA_KEY_2:GRANDPA_KEY_2" \
  --add-candidate "SIDECHAIN_KEY_3:AURA_KEY_3:GRANDPA_KEY_3"
```

Insert a new list of permissioned candidates.  Each candidate is listed
separately using the `--permissioned-candidate-keys` flag followed by a string
of 3 keys separated from each other by a single colon.  This command should
only be used once to initialize the list.  All subsequent updates should be done
using the `update-permissioned-candidates` command below, though there is no
safeguard against calling `insert-permissioned-candidates` multiple times.

#### 3.2.16 Update a list of permissioned candidates

```
nix run .#pc-contracts-cli -- update-permissioned-candidates \
  --add-candidate "SIDECHAIN_KEY_1:AURA_KEY_1:GRANDPA_KEY_1" \
  --add-candidate "SIDECHAIN_KEY_2:AURA_KEY_2:GRANDPA_KEY_2" \
  --remove-candidate "SIDECHAIN_KEY_3:AURA_KEY_3:GRANDPA_KEY_3"
```

You can add and remove candidates in a single transaction.  Each candidate is
listed separately using the `--add-candidate` or `--remove-candidate` flag
followed by a string of four keys separated from each other by a single colon.

#### 3.2.17 Remove all permissioned candidates

```
nix run .#pc-contracts-cli -- update-permissioned-candidates \
  --remove-all-candidates
```

Remove all currently registered permissioned candidates. You can also remove all
candidates and add new ones in a single transaction. Just provide
`--add-candidate` as described above.
