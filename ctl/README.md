# Trustless Sidechain CTL

## 1. Development

If you want to develop for this submodule, please before setting up an environment consult the notes on [CONTRIBUTING.md](CONTRIBUTING.md) first.

## 2. Environment setup

In order to run CTL you need to setup the runtime dependencies:

- cardano-node
- ogmios
- ogmios-datum-cache
- ctl-server

Luckily, we have a dockerised setup, that spins up all these easily with a preset test network. Just run:

```
nix run .#ctl-runtime
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

You can also run these components directly without using Docker, more about these can be found [here](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/runtime.md).

### 2.1. Configuring hosted runtime dependencies

In case you are running the runtime dependencies (ogmios, ogmiosDatumCache and ctlServer) on a hosted environment, or anything else than the default settings, you can either configure it via CLI arguments, or set these in the configuration.

The arguments for each service are using the following scheme:

```
  --ogmios-host localhost  Address host of ogmios (default: "localhost")
  --ogmios-path some/path  Address path of ogmios
  --ogmios-port 1234       Port of ogmios (default: 1337u)
  --ogmios-secure          Whether ogmios is using an HTTPS connection
```

So in case you want to use a remote ogmios service on `https://1.2.3.4:5678`, you want to use the following arguments:

```
nix run .#ctl-main -- mint --amount 100 --ogmios-host 1.2.3.4 --ogmios-port 5678 --ogmios-secure
```

For more information about the arguments, please refer to `nix run .#ctl-main -- mint --help`

To use a configuration file instead, see [3.3. Configuring hosted runtime dependencies](#3.3.-configuring-hosted-runtime-dependencies)

## 3. Running the CLI

You can call the contract endpoints with the following CLI command (you need to add `--` before the arguments):

**Running with nix:**

```
nix run .#ctl-main -- --help
```

**Bundle to a JavaScript file and run using node:**

```shell
nix-build src/ctl-bundle-cli.nix
```

This will produce a package `ctl-scripts-<version>.tar` with the compiled `main.js` script that can be run using Node
and all necessary dependencies in `node_modules` directory.

```
node main.js --help
```

### 3.1. Using the CLI commands

Below are some examples for running the Passive Bridge endpoints.
Notes:

- `genesis-committee-hash-utxo` is not used in the Passive Bridge, but it is pinned to the sidechain parameters, so we have to add an arbitrary utxo here. It can be the same as the mint utxo

- `genesis-mint-utxo` is not a required argument. If omitted from the sidechain parameters, we can mint multiple times

- prior to running the contracts - it may be desirable to have available your signing key in the environment. Example:

```bash
export SIGNING_KEY=/Users/gergo/Dev/cardano/testnets/addresses/server.skey
```

Available commands:

```
  init                     Initialise sidechain
  addresses                Get the script addresses for a given sidechain
  mint                     Mint a certain amount of FUEL tokens
  burn                     Burn a certain amount of FUEL tokens
  register                 Register a committee candidate
  deregister               Deregister a committee member
```

#### 3.1.1. Initialising the sidechain (Active Bridge only)

Before we can start claiming tokens, we must set our initial committee, and initialise the sidechain. Only after this step will we be able to obtain the validator addresses (in the future, there will be a way to obtain the sidechain parameters and validator addressses before setting the first committee).

At this step, the genesis committee hash utxo will be spent.

```
nix run .#ctl-main -- init \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --threshold 2/3 \
  --sidechain-genesis-hash 112233 \
  --committee-pub-key aabbcc \
  --committee-pub-key ccbbaa \
  --sidechain-epoch 0
```

#### 3.1.2. Get script addresses of a sidechain

Script addresses depend on the sidechain parameters, so we get different addresses for different parameters. To get the script addresses for a given sidechain, you can use the following command:

```
nix run .#ctl-main -- addresses \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --genesis-mint-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --threshold 2/3 \
  --sidechain-genesis-hash 112233
```

#### 3.1.3. Mint FUEL tokens

```
nix run .#ctl-main -- mint \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --genesis-mint-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233 \
  --threshold 2/3 \
  --amount 5
```

#### 3.1.4. Burn user owned FUEL tokens

```
nix run .#ctl-main -- burn \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --genesis-mint-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233 \
  --threshold 2/3 \
  --amount 5 \
  --recipient aabbcc
```

#### 3.1.5. Register committee candidate

In order to generate the signatures, you can use the signature generator tool:

```
cabal run trustless-sidechain-gen-signatures -- \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --genesis-mint-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233 \
  --threshold 2/3 \
  --spo-signing-key fa832cc7ad4a0990f36db287df51c62a64c12287e161c07fbc8a4bde0b587c0a \
  --sidechain-signing-key fa832cc7ad4a0990f36db287df51c62a64c12287e161c07fbc8a4bde0b587c0a \
  --registration-utxo 7eddcb7807899d5078ebc25c59d372b484add88604db461e6ef077fd0379733d#0
```

And use it's output for the registration:

```
nix run .#ctl-main -- register \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --genesis-mint-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233 \
  --threshold 2/3 \
  --spo-public-key f71ff66b6b8da0702444183b5ce5de09f6754457a6a71b3354b81ced8dcd7e30 \
  --sidechain-public-key 03eef26d3cf978e0fc2d786c443b1284b27b265a7c82eeeec68c24cd3fd0bb6428 \
  --spo-signature 980db1db31457189326e948c7f292b16278ab91bd45f5fd6ee9ad637bf993f26936c17ee126e510c52d0a3381b52acb36a2a89d4fe55a587cf3478678114dd0f \
  --sidechain-signature bd00090a8a26b3aad534ae2e75ce4ab5b284ffad0751b793d447a3980f770f217929543c21bc7d2567c6ee0c23b983e3983f22d8eb41dfb901a6c31ae3d5b41d \
  --registration-utxo 7eddcb7807899d5078ebc25c59d372b484add88604db461e6ef077fd0379733d#0
```

#### 3.1.6. Deregister committee candidate

```
nix run .#ctl-main -- deregister \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --genesis-mint-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233 \
  --threshold 2/3 \
  --spo-public-key aabbcc
```

#### 3.1.5. Committee hash update

```
nix run .#ctl-main -- committee-hash \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --genesis-mint-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233 \
  --threshold 2/3 \
  --committee-pub-key-and-signature aabbcc01:aaaaaa \
  --committee-pub-key-and-signature aabbcc02 \
  --committee-pub-key-and-signature aabbcc03:bbbbbb \
  --committee-pub-key-and-signature aabbcc04:cccccc \
  --new-committee-pub-key ddeeff01 \
  --new-committee-pub-key ddeeff02 \
  --new-committee-pub-key ddeeff03 \
  --new-committee-pub-key ddeeff04 \
  --sidechain-epoch 6 \
  --previous-merkle-root abcdef
```

#### 3.1.6. Save merkle root

```
nix run .#ctl-main -- save-root \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --genesis-mint-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-genesis-hash 112233 \
  --threshold 2/3 \
  --sidechain-id 1 \
  --merkle-root abababab \
  --committee-pub-key-and-signature aabbcc01:aaaaaa \
  --committee-pub-key-and-signature aabbcc02 \
  --committee-pub-key-and-signature aabbcc03:bbbbbb \
  --committee-pub-key-and-signature aabbcc04:cccccc \
  --previous-merkle-root abcdef
```

#### 3.1.7 Committee handover

```
nix run .#ctl-main -- committee-handover \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --genesis-mint-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233 \
  --threshold 2/3 \
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

### 3.2. Using a configuration file

You can also provide a configuration in `$CWD/config.json` in the following format instead of repeating them in all commands.

```json
{
  "sidechainParameters": {
    "chainId": 123,
    "genesisHash": "11223344aabbcc",
    "genesisMint": "3824c3a7c4437cc6ca4f893cd1519ae1dbe77862304e14d910ddc1f32de69b60#0",
    "genesisUtxo": "3824c3a7c4437cc6ca4f893cd1519ae1dbe77862304e14d910ddc1f32de69b60#1",
    "threshold": { "numerator": 2, "denominator": 3 }
  },
  "runtimeConfig": null,
  "paymentSigningKeyFile": "/absolute/path/to/payment.skey",
  "stakeSigningKeyFile": null
}
```

and now you can call the CLI without these values:

```
nix run .#ctl-main -- mint --amount 5
```

You can find a sample configuration file in `ctl/config.example.json`.

When using the CLI argument and the configuration file together, the **CLI arguments override** these configuration values. You can also set any of the above values to null, if you don't want to set a default value for that property.

### 3.3. Configuring hosted runtime dependencies

You can set the network configuration of the runtime dependecies in the configuration file using the following format:

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
    "ctlServer": null
  },
  "signingKeyFile": null
}
```

Any service where no configuration is defined will fallback to its default value (localhost).
