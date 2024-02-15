# Trustless Sidechain CLI

## 1. Development

If you want to develop for this submodule, please before setting up an environment
consult the notes on [CONTRIBUTING.md](./CONTRIBUTING.md) first.

## 2. Environment setup

In order to execute off-chain commands with the CLI, you need to setup the
runtime dependencies:

- cardano-node
- ogmios
- kupo

Luckily, we have a dockerised setup, that spins up all these easily with a
preset Preview or Preprod. Just run:

```
nix run .#ctl-runtime-preview
```

To change the testnet you're using, you have to change the network name in the `flake.nix`

```
      runtimeConfig = {
        network = {
          name = "NETWORK_NAME";
          magic = NETWORK_MAGIC;
        };
      };
```

You can also run these components directly without using Docker, more about these
can be found [here](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/runtime.md).

Or by using a nix derivation, that brings required runtime dependencies into
scope:
https://github.com/szg251/cardano-dev-shell

### 2.1. Configuring hosted runtime dependencies

In case you are running the runtime dependencies (ogmios and kupo) on a hosted
environment, or anything else than the default settings, you can either configure
it via CLI arguments, or set these in the configuration.

The arguments for each service are using the following scheme:

```
  --ogmios-host localhost  Address host of ogmios (default: "localhost")
  --ogmios-path some/path  Address path of ogmios
  --ogmios-port 1234       Port of ogmios (default: 1337u)
  --ogmios-secure          Whether ogmios is using an HTTPS connection
```

So in case you want to use a remote ogmios service on `https://1.2.3.4:5678`,
you want to use the following arguments:

```
nix run .#sidechain-main-cli -- burn-v1 --amount 100 --recipient aabb --ogmios-host 1.2.3.4 --ogmios-port 5678 --ogmios-secure
```

For more information about the arguments, please refer to
`nix run .#sidechain-main-cli -- burn-v1 --help`

To use a configuration file instead, see
[3.3. Configuring hosted runtime dependencies](#3.3.-configuring-hosted-runtime-dependencies)

## 3. Running the CLI

You can call the contract endpoints with the following CLI command
(you need to add `--` before the arguments):

**Running with nix:**

```
nix run .#sidechain-main-cli -- --help
```

**Bundle to a JavaScript file and run using node:**

```shell
nix build .#ctl-bundle-cli
```

This will produce a package `trustless-sidechain-cli-<version>.tar` with the
compiled `main.js` script that can be run using Node and all necessary
dependencies in `node_modules` directory.

```
node main.js --help
```

### 3.1. Using a configuration file

When running the CLI one needs to pass a single command (see section 3.3 below)
followed by options.  A set of options related to defining the sidechain
parameters is used by all commands.  Instead of having to pass these options on
the command line with every call, it is easier to put them in a configuration
file `$CWD/config.json` in the following format:

```json
{
  "sidechainParameters": {
    "chainId": 123,
    "genesisUtxo": "3824c3a7c4437cc6ca4f893cd1519ae1dbe77862304e14d910ddc1f32de69b60#1",
    "threshold": { "numerator": 2, "denominator": 3 },
    "atmsKind": "plain-ecdsa-secp256k1"
    "governanceAuthority": "4f2d6145e1700ad11dc074cad9f4194cc53b0dbab6bd25dfea6c501c"
  },
  "runtimeConfig": null,
  "paymentSigningKeyFile": "/absolute/path/to/payment.skey",
  "stakeSigningKeyFile": null
}
```

This allows to shorten a CLI call from:

```
nix run .#sidechain-main-cli -- burn-v1 \
  --sidechain-id 123 \
  --genesis-committee-hash-utxo 3824c3a7c4437cc6ca4f893cd1519ae1dbe77862304e14d910ddc1f32de69b60#1 \
  --threshold-numerator 2 \
  --threshold-denominator 3 \
  --atms-kind plain-ecdsa-secp256k1 \
  --governance-authority 4f2d6145e1700ad11dc074cad9f4194cc53b0dbab6bd25dfea6c501c \
  --payment-signing-key-file payment.skey \
  --stake-signing-key-file stake.skey \
  --amount 5 \
  --recipient aabbcc
```

to:

```
nix run .#sidechain-main-cli -- burn-v1 \
  --amount 5 \
  --recipient aabbcc
```

**We henceforth assume that these common options are located in the
configuration file and omit them from any further examples.**

You can find a sample configuration file in `config.example.json`.

When using the CLI argument and the configuration file together, the **CLI
arguments override** these configuration values. You can also set any of the
above values to null, if you don't want to set a default value for that
property.

### 3.2. Configuring hosted runtime dependencies

You can set the network configuration of the runtime dependecies in the
configuration file using the following format:

_$CWD/config.json_

```json
{
  "sidechainParameters": null,
  "runtimeConfig": {
    "network": "testnet",
    "ogmios": {
      "host": "1.2.3.4.5",
      "port": 1337,
      "secure": true,
      "path": null
    },
    "ogmiosDatumCache": {
      "host": "2.3.4.5.6",
      "port": 9999,
      "secure": false,
      "path": null
    },
    "kupo": null
  }
}
```

Any service where no configuration is defined will fallback to its default value
(localhost).

### 3.3. Using the CLI commands

Notes:

- `genesis-committee-hash-utxo` is pinned to the sidechain parameters, so we
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
  addresses                Get the script addresses for a given sidechain
  claim-v1                 Claim a FUEL tokens from a proof
  burn-v1                  Burn a certain amount of FUEL tokens
  claim-v2                 Claim FUEL tokens from thin air
  burn-v2                  Burn a certain amount of FUEL tokens
  register                 Register a committee candidate
  candidate-permission-token
                           Mint candidate permission tokens
  deregister               Deregister a committee member
  committee-hash           Update the committee hash
  save-root                Saving a new merkle root
  committee-handover       An alias for saving the merkle root, followed by
                           updating the committee hash
  save-checkpoint          Saving a new checkpoint
  insert-version           Initialize a new protocol version
  update-version           Update an existing protocol version
  invalidate-version       Invalidate a protocol version
  utils                    Utility functions for cryptographic primitives and
                           messages.
  insert-d-parameter       Insert new D parameter
  update-d-parameter       Update a D parameter
  update-permissioned-candidates
                           Update a Permissioned Candidates list
  collect-garbage          Burn unneccessary NFTs
```

#### 3.3.1. Initialising the sidechain

Before we can start claiming tokens, we must initialise the sidechain by:
initialising the sidechain's tokens, and setting our initial committee. Only
after these steps will we be able to obtain the validator addresses.

To initialise the sidechain, we can run the following command which will spend
the genesis committee hash UTxO.

```
nix run .#sidechain-main-cli -- init \
  --committee-pub-key aabbcc \
  --committee-pub-key ccbbaa \
  --sidechain-epoch 0 \
  --version 1
```

To use permissioned registrations, i.e. such registrations that require a
distinguished token, pass the optional `--candidate-permission-token-amount`
parameter followed by an integer to denote how many permission tokens should be
minted:

```
nix run .#sidechain-main-cli -- init \
  --committee-pub-key aabbcc \
  --committee-pub-key ccbbaa \
  --sidechain-epoch 0 \
  --version 1
  --candidate-permission-token-amount 42
```

#### 3.3.2. Get script addresses of a sidechain

Script addresses depend on the sidechain parameters, so we get different
addresses for different parameters. To get the script addresses for a given
sidechain, you can use the following command:

```
nix run .#sidechain-main-cli -- addresses \
  --version 1
```

An optional `--use-candidate-permission-token` flag can be used to also display
policy of the candidate permission tokens.

#### 3.3.3. Claim FUEL tokens

```
nix run .#sidechain-main-cli -- claim-v1 \
  --combined-proof aabb \
  --distributed-set-utxo "a21f14faf41ffdecf311598f2a858f565b5eba0a9c8d6238988485a3ed64cf1f#0"
    # ^ Optional flag to avoid a linear scan through the UTxO set
```

Note: at the moment there also exists `claim-v2` command, which works with
scripts in version 2 - see below for how versions are managed.  `claim-v2`
exists for demonstration purposes only and does not require any proofs, allowing
to claim FUEL out of thin air:

```
nix run .#sidechain-main-cli -- claim-v2 \
  --amount 13
```

#### 3.3.4. Burn user owned FUEL tokens

```
nix run .#sidechain-main-cli -- burn-v1 \
  --amount 5 \
  --recipient aabbcc
```

Note: at the moment there also exists `burn-v2` command, which works with
scripts in version 2 - see below for how versions are managed.  `burn-v2` exists
for demonstration purposes only and uses same arguments as `burn-v1`.

```
nix run .#sidechain-main-cli -- burn-v2 \
  --amount 5 \
  --recipient aabbcc
```

#### 3.3.5. Register committee candidate

In order to generate the signatures, you can use the signature generator tool:

```
cabal run trustless-sidechain-gen-signatures -- register \
  --spo-signing-key c460e6d00d5e4e11240469046a988cecd136f10e12e6115ddc46566e01d199f4 \
  --sidechain-signing-key c460e6d00d5e4e11240469046a988cecd136f10e12e6115ddc46566e01d199f4 \
  --registration-utxo a03ebf281ed96549f74d0e724841fcf928194c44f6ff9a8056d1829598042c62#1
```
And use it's output for the registration:

```
nix run .#sidechain-main-cli -- register \
  --spo-public-key 67663ee94098ceca0dacbf7f947946bfdc4de1848d76da5249b1c3a18a41a57a \
  --sidechain-public-key 02599181389043ba0b83e53d3d665c2dfaa187453a24a4538723766f8f0509c55d \
  --spo-signature cf5fc5b10dff794ac0f5908c38d28a1d8e8430f17c2036cf14f4b28c990b6794f754ca809d69ecd52e4c4d542f90c43b017ff7f23cf46efc4d8f6b07a3895403 \
  --sidechain-signature 9da47b68b68cbca5cbaf7a0bd2a2bfedfe8c58e90ab8a709b8aed4c1644065885823203526b331284d15b238d11a60abb5c0cb3a8f2ef1102fbae736d98146bd \
  --registration-utxo a03ebf281ed96549f74d0e724841fcf928194c44f6ff9a8056d1829598042c62#1
```

Optionally, assuming that we are using a permissioned candidates system, one
can include the candidate permission token when registering as follows.
```
nix run .#sidechain-main-cli -- register \
  --spo-public-key e734ea6c2b6257de72355e472aa05a4c487e6b463c029ed306df2f01b5636b58 \
  --sidechain-public-key 0281158622b7d2eb738b885e1cca50218fb36ab4dc39014b83286b8ed95c78789d \
  --spo-signature de9a8ac3db51bab648a97b56bdbe6757d189633dac91b129156607cf6f3db51217ec1b3c327ab781c6e2de3c4338e3a989449e119daed60a3530aaf268cd3709 \
  --sidechain-signature 5fe405ba531216cf5bfe65f2826d618c5d4a84df7016fcc4f4a6a68323ecb5f56799e08aa8dba6bc087b9131c5b76483ededa250da0ddbf2d24e00991b627e6e \
  --registration-utxo "fff1c0f7f2834cb30a2136c7aadeb37a4680b30e3ae6ea088edc4e1ece939026#3" \
  --use-candidate-permission-token
```

#### 3.3.6. Deregister committee candidate

```
nix run .#sidechain-main-cli -- deregister \
  --spo-public-key aabbcc
```

#### 3.3.7. Committee hash update

```
nix run .#sidechain-main-cli -- committee-hash \
  --committee-pub-key-and-signature aabbcc01:aaaaaa \
  --committee-pub-key-and-signature aabbcc02 \
  --committee-pub-key-and-signature aabbcc03:bbbbbb \
  --committee-pub-key-and-signature aabbcc04:cccccc \
  --new-committee-pub-key ddeeff01 \
  --new-committee-pub-key ddeeff02 \
  --new-committee-pub-key ddeeff03 \
  --new-committee-pub-key ddeeff04 \
  --sidechain-epoch 6 \
  --new-committee-validator-cbor-encoded-address aadd \
  --previous-merkle-root abcdef
```

where we note that `--new-committee-validator-cbor-encoded-address` can be found
from the JSON key `cborEncodedAddresses` from the output of the `addresses`
subcommand.

#### 3.3.8. Save merkle root

```
nix run .#sidechain-main-cli -- save-root \
  --merkle-root abababab \
  --committee-pub-key-and-signature aabbcc01:aaaaaa \
  --committee-pub-key-and-signature aabbcc02 \
  --committee-pub-key-and-signature aabbcc03:bbbbbb \
  --committee-pub-key-and-signature aabbcc04:cccccc \
  --previous-merkle-root abcdef
```

#### 3.3.9 Committee handover

```
nix run .#sidechain-main-cli -- committee-handover \
  --merkle-root abababab \
  --sidechain-epoch 6 \
  --previous-merkle-root abcdef \
  --new-committee-pub-key ddeeff01 \
  --new-committee-pub-key ddeeff02 \
  --new-committee-pub-key ddeeff03 \
  --committee-pub-key-and-new-committee-signature aabbcc01:aaaaaa \
  --committee-pub-key-and-new-committee-signature aabbcc02 \
  --committee-pub-key-and-new-committee-signature aabbcc03:bbbbbb \
  --committee-pub-key-and-new-merkle-root-signature aabbcc01:aaaaaa \
  --committee-pub-key-and-new-merkle-root-signature aabbcc02 \
  --committee-pub-key-and-new-merkle-root-signature aabbcc03:bbbbbb
```

#### 3.3.10 Candidiate permission token

```
nix run .#sidechain-main-cli -- candidate-permission-token \
  --candidate-permission-token-amount 10
```

#### 3.3.11 Save checkpoint

```
nix run .#sidechain-main-cli -- save-checkpoint \
  --new-checkpoint-block-hash d8063cc6e907f497360ca50238af5c2e2a95a8869a2ce74ab3e75fe6c9dcabd0d8063cc6e907f497360ca50238af5c2e2a95a8869a2ce74ab3e75fe6c9dcabd0 \
  --new-checkpoint-block-number 42 \
  --sidechain-epoch 5
```

#### 3.3.12 Insert new protocol version

```
nix run .#sidechain-main-cli -- insert-version \
  --version 2
```

#### 3.3.13 Update existing protocol version

```
nix run .#sidechain-main-cli -- update-version \
  --old-version 1 \
  --new-version 2
```

#### 3.3.14 Invalidate protocol version

```
nix run .#sidechain-main-cli -- invalidate-version \
  --version 1
```

#### 3.3.15 Insert a D parameter value

```
nix run .#sidechain-main-cli -- insert-d-parameter \
  --d-parameter-permissioned-candidates-count N \
  --d-parameter-registered-candidates-count M
```

where N and M are integers.  Note that this should be only done once and then
`update-d-parameter` value should be used (see below).  However, there is no
safeguard against inserting multiple D parameter values.

#### 3.3.16 Update a D parameter value

```
nix run .#sidechain-main-cli -- update-d-parameter \
  --d-parameter-permissioned-candidates-count N \
  --d-parameter-registered-candidates-count M
```

where N and M are integers.  If more than one D parameter value was inserted
this will remove all inserted values first and then replace them with a single
new value.

#### 3.3.17 Insert a list of permissioned candidates

```
nix run .#sidechain-main-cli -- update-permissioned-candidates \
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

#### 3.3.18 Update a list of permissioned candidates

```
nix run .#sidechain-main-cli -- update-permissioned-candidates \
  --add-candidate "SIDECHAIN_KEY_1:AURA_KEY_1:GRANDPA_KEY_1" \
  --add-candidate "SIDECHAIN_KEY_2:AURA_KEY_2:GRANDPA_KEY_2" \
  --remove-candidate "SIDECHAIN_KEY_3:AURA_KEY_3:GRANDPA_KEY_3"
```

You can add and remove candidates in a single transaction.  Each candidate is
listed separately using the `--add-candidate` or `--remove-candidate` flag
followed by a string of four keys separated from each other by a single colon.

#### 3.3.19 Remove all permissioned candidates

```
nix run .#sidechain-main-cli -- update-permissioned-candidates \
  --remove-all-candidates
```

Remove all currently registered permissioned candidates. You can also remove all
candidates and add new ones in a single transaction. Just provide
`--add-candidate` as described above.

#### 3.3.20 Garbage collect redundant tokens

```
nix run .#sidechain-main-cli -- collect-garbage
```

Burn all waste tokens found on a user's PubKey address. These tokens include
ATMS Tokens and FUEL Mint and Burn tokens. These tokens don't play any role in
the system after they are minted. Their only purpose is to be minted alongside
some other tokens, as a proof that some kind of check has passed. After that
there is no other way to use them.

#### 3.3.21 Utils

TODO: who wrote this command?
