# Trustless Sidechain CTL
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

You can also run these components indidually, more about these can be found [here](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/runtime.md).

## Running the CLI

You can call the contract endpoints with the following CLI command (you need to add `--` before the arguments):
```
nix run .#ctl-main -- -help
```

Alternatively, you can run `make main.js` inside a `nix develop` shell to bundle CTL into an executable javascript file.
Then, you can use it the following way (without the `--`):
```
./main.js --help
```

### Examples

Below are some examples for running the Passive Bridge endpoints.
Notes:
- `genesis-committee-hash-utxo` is not used in the Passive Bridge, but it is pinned to the sidechain parameters, so we have to add an arbitrary utxo here. It can be the same as the mint utxo
- `genesis-mint-utxo` is not a required argument. If omitted from the sidechain parameters, we can mint multiple times

#### Register committee candidate

```
nix run .#ctl-main -- register \
  --signing-key-file /Users/gergo/Dev/cardano/testnets/addresses/server.skey \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --genesis-mint-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --chain-id 1 \
  --genesis-hash 112233 \
  --spo-public-key aabbcc \
  --sidechain-public-key aabbcc \
  --spo-signature aabbcc \
  --sidechain-signature aabbcc \
  --registration-utxo 7eddcb7807899d5078ebc25c59d372b484add88604db461e6ef077fd0379733d#0
```

#### Deregister committee candidate

```
nix run .#ctl-main -- deregister \
  --signing-key-file /Users/gergo/Dev/cardano/testnets/addresses/server.skey \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --genesis-mint-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --chain-id 1 \
  --genesis-hash 112233 \
  --spo-public-key aabbcc
```

#### Mint FUEL tokens

```
nix run .#ctl-main -- mint \
  --signing-key-file /Users/gergo/Dev/cardano/testnets/addresses/server.skey \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --genesis-mint-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --chain-id 1 \
  --genesis-hash 112233 \
  --amount 5
```


#### Burn user owned FUEL tokens

```
nix run .#ctl-main -- burn \
  --signing-key-file /Users/gergo/Dev/cardano/testnets/addresses/server.skey \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --genesis-mint-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --chain-id 1 \
  --genesis-hash 112233 \
  --amount 5 \
  --recipient aabbcc
```
