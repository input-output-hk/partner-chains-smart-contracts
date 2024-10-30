# Off-chain Operations

This document describes usage of the partner chains CLI. This command line tool
provides a set of commands, that are used to manage the Cardano components of a
partner chain by submitting transactions to the Cardano network.

**NOTE:** terms _partner chain_ and _sidechain_ are used interchangeably in this
document.

- [Off-chain Operations](#off-chain-operations)
  - [Development](#development)
  - [Environment setup](#environment-setup)
    - [Configuring hosted runtime dependencies](#configuring-hosted-runtime-dependencies)
  - [Running the CLI](#running-the-cli)
    - [Using a configuration file](#using-a-configuration-file)
    - [Using the CLI commands](#using-the-cli-commands)
  - [Governance](#governance)
    - [1. Initialization](#1-initialization)
    - [2. D parameters](#2-d-parameters)
      - [Insert a D parameter value](#insert-a-d-parameter-value)
      - [Update a D parameter value](#update-a-d-parameter-value)
    - [3. Committee management](#3-committee-management)
      - [Register committee candidate](#register-committee-candidate)
      - [Deregister committee candidate](#deregister-committee-candidate)
      - [Update list of permissioned candidates](#update-list-of-permissioned-candidates)
      - [Remove all permissioned candidates](#remove-all-permissioned-candidates)
    - [4. Native Tokens](#4-native-tokens)
      - [Create a new token reserve](#create-a-new-token-reserve)
      - [Empty and remove an existing reserve](#empty-and-remove-an-existing-reserve)
      - [Deposit assets to existing reserve](#deposit-assets-to-existing-reserve)
      - [Release currently available funds from an existing reserve](#release-currently-available-funds-from-an-existing-reserve)
    - [5. Queries](#5-queries)
      - [List currently versioned scripts](#list-currently-versioned-scripts)
      - [Get script addresses of a sidechain](#get-script-addresses-of-a-sidechain)
    - [6. Versioning](#6-versioning)
      - [Update existing protocol version](#update-existing-protocol-version)
      - [Invalidate protocol version](#invalidate-protocol-version)

## Development

If you want to develop for this submodule, please consult the notes in
[CONTRIBUTING.md](./CONTRIBUTING.md) before setting up your environment.

## Environment setup

In order to execute off-chain commands with the CLI, you need to setup the
runtime dependencies:

- cardano-node
- ogmios
- kupo

### Configuring hosted runtime dependencies

In case you are running the runtime dependencies (ogmios and kupo) on a hosted
environment, or anything else than the default settings, you can either
configure it via CLI arguments, or set these in the
[configuration file](#using-a-configuration-file).

The arguments for ogmios and kupo are using the following scheme:

```
  --ogmios-host localhost  # Address host of ogmios (default: "localhost")
  --ogmios-path some/path  # Address path of ogmios
  --ogmios-port 1234       # Port of ogmios (default: 1337u)
  --ogmios-secure          # Whether ogmios is using an HTTPS connection
```

So in case you want to use a remote ogmios service on `https://1.2.3.4:5678`,
you want to use the following arguments:

```
nix run .#pc-contracts-cli -- [...] --ogmios-host 1.2.3.4 --ogmios-port 5678 --ogmios-secure
```

where `[...]` represents a command and command-specific flags and options.

See [Using a configuration file](#using-a-configuration-file) on how
to use a configuration file instead of command-line options.

## Running the CLI

You can run the CLI tool either by downloading a release or through Nix.

Download [the latest release](https://github.com/input-output-hk/partner-chains-smart-contracts/releases/latest) from GitHub.

When using `nix`, commands and options passed to the CLI must be preceeded by
`--`. For example, to display the list of all available commands one needs to
execute:

```
nix run .#pc-contracts-cli -- --help
```

### Using a configuration file

When running the CLI one needs to pass a single command followed by options. A
set of options related to defining the partner chain parameters is used by all
commands. Instead of having to pass these options on the command line with every
call, it is easier to put them in a configuration file `$CWD/config.json` in the
following format:

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
configuration file and omit them from any further examples. Thus, all provided
options are specific to a particular command being demonstrated.**

You can find a sample configuration file in `config.example.json`.

When using the CLI arguments and the configuration file together, the **CLI
arguments override** values in the configuration file. You can also set any of
the above values to null, if you don't want to set a value for that property.
Any configuration entry where no configuration is defined will fallback to its
default value.

### Using the CLI commands

Notes:

- `genesis-committee-hash-utxo` is pinned to the partner chain parameters, so we
  have to add an arbitrary UTxO here.

- If not using a config file, prior to running the contracts it may be desirable
  to have available your signing key in the environment. Example:

  ```
  export SIGNING_KEY=/Users/gergo/Dev/cardano/testnets/addresses/server.skey
  ```

  and then pass it on the command line as `--payment-signing-key-file
$SIGNING_KEY`.

## Governance

### 1. Initialization

| Command                   | Description                                       |
| ------------------------- | ------------------------------------------------- |
| `init-governance`         | Initialise governance |
| `init-reserve-management` | Initialise native token reserve management system |

A mandatory first step is to initialize governance using
the `init-governance` command.

Executing `init-governance` spends the Genesis Utxo.

```
nix run .#pc-contracts-cli -- init-governance --governance-authority "4f2d6145e1700ad11dc074cad9f4194cc53b0dbab6bd25dfea6c501a"
```

To be able to use native tokens related scripts must be inserted into versioning
system first using the `init-reserve-management` command.

- [Governance smart contract code](../onchain/src/TrustlessSidechain/Governance/MultiSig.hs)
- [Governance offchain code](./src/TrustlessSidechain/Governance.purs)

### 2. D parameters

| Command              | Description            |
| -------------------- | ---------------------- |
| `insert-d-parameter` | Insert new D parameter |
| `update-d-parameter` | Update a D parameter   |

#### Insert a D parameter value

```
nix run .#pc-contracts-cli -- insert-d-parameter \
  --d-parameter-permissioned-candidates-count N \
  --d-parameter-registered-candidates-count M
```

Note that this should be only done once and then `update-d-parameter` value
should be used (see below). However, there is no safeguard against inserting
multiple D parameter values.

#### Update a D parameter value

```
nix run .#pc-contracts-cli -- update-d-parameter \
  --d-parameter-permissioned-candidates-count N \
  --d-parameter-registered-candidates-count M
```

If more than one D parameter value was inserted this will remove all inserted
values first and then replace them with a single new value.

- [D-param smart contract code](../onchain/src/TrustlessSidechain/DParameter.hs)
- [D-param off-chain code](./src/TrustlessSidechain/DParameter.purs)

### 3. Committee management

| Command                          | Description                           |
| -------------------------------- | ------------------------------------- |
| `register`                       | Register a committee candidate        |
| `deregister`                     | Deregister a committee member         |
| `update-permissioned-candidates` | Update a Permissioned Candidates list |

A blockchain validator is a network node that helps process and validate
transaction blocks on the platform according to the protocol consensus
mechanism. A permissioned candidate is a block-producing validator that has been
whitelisted by the chain builder so it can validate partner chain blocks and
secure the network.

A registered block producer is a Cardano stake pool operator (SPO) that desires
to process and validate transaction blocks on the partner chain. A registered
block producer will use the partner chains toolkit (as well as other tools) to
contribute to the validity of the partner chain ledger and, in turn, secure it.

#### Register committee candidate

```
nix run .#pc-contracts-cli -- register \
  --spo-public-key 67663ee94098ceca0dacbf7f947946bfdc4de1848d76da5249b1c3a18a41a57a \
  --sidechain-public-key 02599181389043ba0b83e53d3d665c2dfaa187453a24a4538723766f8f0509c55d \
  --spo-signature cf5fc5b10dff794ac0f5908c38d28a1d8e8430f17c2036cf14f4b28c990b6794f754ca809d69ecd52e4c4d542f90c43b017ff7f23cf46efc4d8f6b07a3895403 \
  --sidechain-signature 9da47b68b68cbca5cbaf7a0bd2a2bfedfe8c58e90ab8a709b8aed4c1644065885823203526b331284d15b238d11a60abb5c0cb3a8f2ef1102fbae736d98146bd \
  --registration-utxo a03ebf281ed96549f74d0e724841fcf928194c44f6ff9a8056d1829598042c62#1
```

#### Deregister committee candidate

```
nix run .#pc-contracts-cli -- deregister \
  --spo-public-key aabbcc
```

#### Update list of permissioned candidates

```
nix run .#pc-contracts-cli -- update-permissioned-candidates \
  --add-candidate "SIDECHAIN_KEY_1:AURA_KEY_1:GRANDPA_KEY_1" \
  --add-candidate "SIDECHAIN_KEY_2:AURA_KEY_2:GRANDPA_KEY_2" \
  --remove-candidate "SIDECHAIN_KEY_3:AURA_KEY_3:GRANDPA_KEY_3"
```

You can add and remove candidates in a single transaction. Each candidate is
listed separately using the `--add-candidate` or `--remove-candidate` flag
followed by a string of three keys separated from each other by a single colon.

#### Remove all permissioned candidates

```
nix run .#pc-contracts-cli -- update-permissioned-candidates \
  --remove-all-candidates
```

Remove all currently registered permissioned candidates. You can also remove all
candidates and add new ones in a single transaction. Just provide
`--add-candidate` as described above.

- [Registration/deregistration smart contract code](../onchain/src/TrustlessSidechain/CommitteeCandidateValidator.hs)
- [Registration/deregistration off-chain code](./src/TrustlessSidechain/CommitteeCandidateValidator.purs)
- [List update smart contract code](../onchain/src/TrustlessSidechain/PermissionedCandidates.hs)
- [List update off-chain code](./src/TrustlessSidechain/PermissionedCandidates.purs)

### 4. Native Tokens

| Command                 | Description                                                |
| ----------------------- | ---------------------------------------------------------- |
| `reserve-create`        | Create a new token reserve                                 |
| `reserve-handover`      | Empty and remove an existing reserve                       |
| `reserve-deposit`       | Deposit assets to existing reserve                         |
| `reserve-release-funds` | Release currently available funds from an existing reserve |

#### Create a new token reserve

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

#### Empty and remove an existing reserve

```
nix run .#pc-contracts-cli -- reserve-handover
```

Perform the reserve handover.

#### Deposit assets to existing reserve

```
nix run .#pc-contracts-cli -- reserve-deposit \
  --deposit-reserve-asset ASSET-SCRIPT-HASH \
  --reserve-asset-name RESERVE_ASSET_NAME \
  --reserve-initial-deposit-amount RESERVE-DEPOSIT-AMOUNT
```

Instead of `--deposit-reserve-asset` and `--reserve-asset-name` one might
specify `--reserve-ada-asset` flag to indicate that Ada is being used as the
reserve asset.

#### Release currently available funds from an existing reserve

```
nix run .#pc-contracts-cli -- reserve-release-funds \
  --total-accrued-till-now INT \
  --reserve-transaction-input RESERVE-TRANSACTION-INPUT
```

`total-accrued-till-now` is a non-negative integer Computed integer for v(t)
`reserve-transaction-input` is a CBOR-encoded transaction id and the transaction
index separated by a `#`. It must contain a policy script to transfer illiquid
circulation.

Example: `a03ebf281ed96549f74d0e724841fcf928194c44f6ff9a8056d1829598042c62#0`

- [Reserve smart contract code](../onchain/src/TrustlessSidechain/Reserve.hs)
- [Reserve off-chain code](./src/TrustlessSidechain/NativeTokenManagement/Reserve.purs)
- [Illiquid circulation supply smart contract code](../onchain/src/TrustlessSidechain/IlliquidCirculationSupply.hs)
- [Illiquid circulation supply off-chain code](./src/TrustlessSidechain/NativeTokenManagement/IlliquidCirculationSupply.purs)

### 5. Queries

| Command                  | Description                                                                      |
| ------------------------ | -------------------------------------------------------------------------------- |
| `list-versioned-scripts` | Get scripts (validators and minting policies) that are currently being versioned |
| `addresses`              | Get the script addresses for a given sidechain                                   |
| `cli-version`            | Display semantic version of the CLI and its git hash                             |

#### List currently versioned scripts

```
nix run .#pc-contracts-cli -- list-versioned-scripts
```

Returns the list of currently versioned scripts.

More specifically, it returns all scripts that were inserted in the version
oracle. This includes an initial set of scripts that were added by init
commands, and the scripts that were added when inserting a new protocol version
using the `insert-version`.

#### Get script addresses of a sidechain

Script addresses depend on the sidechain parameters, so we get different
addresses for different parameters. To get the script addresses for a given
sidechain, you can use the following command:

```
nix run .#pc-contracts-cli -- addresses
```

### 6. Versioning

| Command              | Description                         |
| -------------------- | ----------------------------------- |
| `update-version`     | Update an existing protocol version |
| `invalidate-version` | Invalidate a protocol version       |

#### Update existing protocol version

```
nix run .#pc-contracts-cli -- update-version
```

#### Invalidate protocol version

```
nix run .#pc-contracts-cli -- invalidate-version
```

- [Version oracle smart contract code](../onchain/src/TrustlessSidechain/)
- [Version oracle off-chain code](./src/TrustlessSidechain/NativeTokenManagement/Reserve.purs)
- [Script cache smart contract code](../onchain/src/TrustlessSidechain/ScriptCache.hs)
- [Script cache off-chain code](./src/TrustlessSidechain/ScriptCache.purs)
