# Trustless Sidechain CTL

## Development

If you want to develop for this submodule, please before setting up an environment consult the notes on [CONTRIBUTING.md](CONTRIBUTING.md) first.

## Environment setup

In order to run CTL you need to setup the runtime dependencies:
* cardano-node
* ogmios
* ogmios-datum-cache
* ctl-server

Luckily, we have a dockerised setup, that spins up all these easily with a preset test network. Just run:
```
nix run .#ctl-runtime
```

To change the testnet you're using, you have to change the network name in the `flake.nix`, and make sure you have the updated configuration files in `ctl/cardano-configurations/network/NETWORK_NAME`
```
      runtimeConfig = {
        network = {
          name = "NETWORK_NAME";
          magic = NETWORK_MAGIC;
        };
      };
```

You can also run these components directly without using Docker, more about these can be found [here](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/runtime.md).

## Running the CLI

You can call the contract endpoints with the following CLI command (you need to add `--` before the arguments):
```
nix run .#ctl-main -- --help
```

Alternatively, you can run `make main.js` inside a `nix develop` shell to bundle CTL into an executable javascript file.
Then, you can use it the following way (without the `--`):
```
./main.js --help
```

### Using the CLI commands

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
  addresses                Get the script addresses for a given sidechain
  mint                     Mint a certain amount of FUEL tokens
  burn                     Burn a certain amount of FUEL tokens
  register                 Register a committee candidate
  deregister               Deregister a committee member
```

#### Get script addresses of a sidechain

Script addresses depend on the sidechain parameters, so we get different addresses for different parameters. To get the script addresses for a given sidechain, you can use the following command:

```
nix run .#ctl-main -- addresses \
  --signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --genesis-mint-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233
```

#### Mint FUEL tokens

```
nix run .#ctl-main -- mint \
  --signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --genesis-mint-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233 \
  --amount 5
```

#### Burn user owned FUEL tokens

```
nix run .#ctl-main -- burn \
  --signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --genesis-mint-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233 \
  --amount 5 \
  --recipient aabbcc
```

#### Register committee candidate

In order to generate the signatures, you can use the signature generator tool:

```
cabal run trustless-sidechain-gen-signatures -- \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --genesis-mint-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233 \
  --spo-signing-key fa832cc7ad4a0990f36db287df51c62a64c12287e161c07fbc8a4bde0b587c0a \
  --sidechain-signing-key fa832cc7ad4a0990f36db287df51c62a64c12287e161c07fbc8a4bde0b587c0a \
  --registration-utxo 7eddcb7807899d5078ebc25c59d372b484add88604db461e6ef077fd0379733d#0
```

And use it's output for the registration:

```
nix run .#ctl-main -- register \
  --signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --genesis-mint-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233 \
  --spo-public-key f71ff66b6b8da0702444183b5ce5de09f6754457a6a71b3354b81ced8dcd7e30 \
  --sidechain-public-key 03eef26d3cf978e0fc2d786c443b1284b27b265a7c82eeeec68c24cd3fd0bb6428 \
  --spo-signature 980db1db31457189326e948c7f292b16278ab91bd45f5fd6ee9ad637bf993f26936c17ee126e510c52d0a3381b52acb36a2a89d4fe55a587cf3478678114dd0f \
  --sidechain-signature bd00090a8a26b3aad534ae2e75ce4ab5b284ffad0751b793d447a3980f770f217929543c21bc7d2567c6ee0c23b983e3983f22d8eb41dfb901a6c31ae3d5b41d \
  --registration-utxo 7eddcb7807899d5078ebc25c59d372b484add88604db461e6ef077fd0379733d#0
```

#### Deregister committee candidate

```
nix run .#ctl-main -- deregister \
  --signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --genesis-mint-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233 \
  --spo-public-key aabbcc
```

### Using a configuration file

You can also provide a configuration file named `config.json` in the following format instead of repeating them in all commands.

```
{
  "sidechainParameters": {
    "chainId": 123,
    "genesisHash": "11223344aabbcc",
    "genesisMint": "3824c3a7c4437cc6ca4f893cd1519ae1dbe77862304e14d910ddc1f32de69b60#0",
    "genesisUtxo": "3824c3a7c4437cc6ca4f893cd1519ae1dbe77862304e14d910ddc1f32de69b60#1"
  },
  "signingKeyFile": "/absolute/path/to/signing-key.skey",
  "stakeSigningKeyFile": null
}
```

and now you can call the CLI without these values:

```
nix run .#ctl-main -- mint --amount 5
```

When using the CLI argument and the configuration file together, the **CLI arguments override** these configuration values. You can also set any of the above values to null, if you don't want to set a default value for that property.
