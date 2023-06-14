# Trustless Sidechain CTL

## 1. Development

If you want to develop for this submodule, please consult the notes on [CONTRIBUTING.md](CONTRIBUTING.md) before setting up an environment.  

The term 'FUEL' is used widely in this repository. It refers to a test token used as an example of a sidechain token. It has no real-world value.  

## 2. Environment setup

To run CTL you need to set up the runtime dependencies:

- cardano-node
- ogmios
- kupo

Luckily, there is a dockerised setup, that spins up all these easily with a preset test network. Just run:

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

In case you are running the runtime dependencies (Ogmios and Kupo) on a hosted environment, or anything other than the default settings, you can either configure it via CLI arguments or set these in the configuration.

The arguments for each service use the following scheme:

```
  --ogmios-host localhost  Address host of ogmios (default: "localhost")
  --ogmios-path some/path  Address path of ogmios
  --ogmios-port 1234       Port of ogmios (default: 1337u)
  --ogmios-secure          Whether ogmios is using an HTTPS connection
```

So if you want to use a remote ogmios service on `https://1.2.3.4:5678`, use the following arguments:

```
nix run .#sidechain-main-cli -- burn --amount 100 --recipient aabb --ogmios-host 1.2.3.4 --ogmios-port 5678 --ogmios-secure
```

For more information about the arguments, please refer to `nix run .#sidechain-main-cli -- burn --help`

To use a configuration file instead, see [3.3. Configuring hosted runtime dependencies](#3.3.-configuring-hosted-runtime-dependencies)

## 3. Running the CLI

You can call the contract endpoints with the following CLI command (you need to add `--` before the arguments):

**Running with nix:**

```
nix run .#sidechain-main-cli -- --help
```

**Bundle to a JavaScript file and run using node:**

```shell
nix build .#ctl-bundle-cli
```

This will produce a package `trustless-sidechain-cli-<version>.tar` with the compiled `main.js` script that can be run using Node
and all necessary dependencies in `node_modules` directory.

```
node main.js --help
```

### 3.1. Using the CLI commands

Notes:

- `genesis-committee-hash-utxo` is pinned to the sidechain parameters, so it is necessary to add an arbitrary UTXO here.

- before running the contracts, it may be desirable to have available your signing key in the environment. Example:

```bash
export SIGNING_KEY=/Users/gergo/Dev/cardano/testnets/addresses/server.skey
```

Available commands:

```
  init-tokens-mint            Pre-mint tokens without actually initialising
                              sidechain
  init                        Initialize sidechain
  addresses                   Get the script addresses for a given sidechain
  claim                       Claim a certain amount of FUEL tokens
  burn                        Burn a certain amount of FUEL tokens
  committee-hash              Update the committee hash
  register                    Register a committee candidate
  deregister                  Deregister a committee member
  candidate-permission-token  Mint candidate permission tokens
  save-checkpoint             Save a new checkpoint block
```

#### 3.1.1. Initialising the sidechain

Before claiming tokens, you must initialise the sidechain by: initialising the sidechain's tokens, and setting your initial committee. Only after these steps will you be able to obtain the validator addresses.

To initialise the sidechain, you can run the following command which will spend the genesis committee hash UTXO.

```
nix run .#sidechain-main-cli -- init \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --threshold 2/3 \
  --sidechain-genesis-hash 112233 \
  --committee-pub-key aabbcc \
  --committee-pub-key ccbbaa \
  --sidechain-epoch 0
```

Normally, the protocol has to wait until the committee has reached the desired number of members.
To defend against accidentally consuming the `genesis-committee-hash-utxo`, it's possible, to split
up the initialisation step into two separate commands:

1. mint tokens identifying tokens used later in the protocol (spending the `genesis-committee-hash-utxo`)
2. set up the first committee (only the owner of the above minted tokens can do this)

To mint the initial tokens, use the following command:

```
nix run .#sidechain-main-cli -- init-tokens-mint \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --threshold 2/3 \
  --sidechain-genesis-hash 112233
```

Alternatively, one may optionally mint candidate permission tokens in the same transaction as
the `init-tokens-mint` transaction. The following command achieves this.
```
nix run .#sidechain-main-cli -- init-tokens-mint \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo  "9fda32facf5f250f74cc3a77762fb9efec093b0afe5221a6d8bca2a217428668#1" \
  --sidechain-id 1 \
  --threshold 2/3 \
  --sidechain-genesis-hash 112233 \
  --candidate-permission-token-amount 1 \
  --candidate-permission-token-name ""\
  --candidate-permission-token-utxo "9fda32facf5f250f74cc3a77762fb9efec093b0afe5221a6d8bca2a217428668#1"
```

And when ready, continue with setting up the first committee with the `init` command with the
`use-init-tokens` flag:

```
nix run .#sidechain-main-cli -- init \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --threshold 2/3 \
  --sidechain-genesis-hash 112233 \
  --committee-pub-key aabbcc \
  --committee-pub-key ccbbaa \
  --sidechain-epoch 0 \
  --use-init-tokens
```

#### 3.1.2. Get script addresses of a sidechain

Script addresses depend on the sidechain parameters, so you get different addresses for different parameters. To get the script addresses for a given sidechain, you can use the following command:

```
nix run .#sidechain-main-cli -- addresses \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --threshold 2/3 \
  --sidechain-genesis-hash 112233
```

#### 3.1.3. Claim FUEL tokens

```
nix run .#sidechain-main-cli -- claim \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233 \
  --threshold 2/3 \
  --combined-proof aabb \
  --distributed-set-utxo "a21f14faf41ffdecf311598f2a858f565b5eba0a9c8d6238988485a3ed64cf1f#0"
    # ^ Optional flag to avoid a linear scan through the UTxO set
```

#### 3.1.4. Burn user-owned FUEL tokens

```
nix run .#sidechain-main-cli -- burn \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233 \
  --threshold 2/3 \
  --amount 5 \
  --recipient aabbcc
```

#### 3.1.5. Register committee candidate

To generate the signatures, you can use the signature generator tool:

```
cabal run trustless-sidechain-gen-signatures -- \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233 \
  --threshold 2/3 \
  --spo-signing-key fa832cc7ad4a0990f36db287df51c62a64c12287e161c07fbc8a4bde0b587c0a \
  --sidechain-signing-key fa832cc7ad4a0990f36db287df51c62a64c12287e161c07fbc8a4bde0b587c0a \
  --registration-utxo 7eddcb7807899d5078ebc25c59d372b484add88604db461e6ef077fd0379733d#0
```

And use its output for the registration:

```
nix run .#sidechain-main-cli -- register \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233 \
  --threshold 2/3 \
  --spo-public-key f71ff66b6b8da0702444183b5ce5de09f6754457a6a71b3354b81ced8dcd7e30 \
  --sidechain-public-key 03eef26d3cf978e0fc2d786c443b1284b27b265a7c82eeeec68c24cd3fd0bb6428 \
  --spo-signature 980db1db31457189326e948c7f292b16278ab91bd45f5fd6ee9ad637bf993f26936c17ee126e510c52d0a3381b52acb36a2a89d4fe55a587cf3478678114dd0f \
  --sidechain-signature bd00090a8a26b3aad534ae2e75ce4ab5b284ffad0751b793d447a3980f770f217929543c21bc7d2567c6ee0c23b983e3983f22d8eb41dfb901a6c31ae3d5b41d \
  --registration-utxo 7eddcb7807899d5078ebc25c59d372b484add88604db461e6ef077fd0379733d#0
```

Optionally, assuming that we are using a permissioned candidates system, one
can include the candidate permission token when registering as follows.
```
nix run .#sidechain-main-cli -- register \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233 \
  --threshold 2/3 \
  --spo-public-key e734ea6c2b6257de72355e472aa05a4c487e6b463c029ed306df2f01b5636b58 \
  --sidechain-public-key 0281158622b7d2eb738b885e1cca50218fb36ab4dc39014b83286b8ed95c78789d \
  --spo-signature de9a8ac3db51bab648a97b56bdbe6757d189633dac91b129156607cf6f3db51217ec1b3c327ab781c6e2de3c4338e3a989449e119daed60a3530aaf268cd3709 \
  --sidechain-signature 5fe405ba531216cf5bfe65f2826d618c5d4a84df7016fcc4f4a6a68323ecb5f56799e08aa8dba6bc087b9131c5b76483ededa250da0ddbf2d24e00991b627e6e \
  --registration-utxo "fff1c0f7f2834cb30a2136c7aadeb37a4680b30e3ae6ea088edc4e1ece939026#3" \
  --candidate-permission-token-name "aabb" \
  --candidate-permission-token-utxo "9ce50ad2562c18295e8a76ade473eeb17fc32f978961ab5adf78704d70b23834#1"
```
where we note the flags `--candidate-permission-token-utxo` and
`--candidate-permission-token-name` must be generated by the
`candidate-permission-token` command.

#### 3.1.6. Deregister committee candidate

```
nix run .#sidechain-main-cli -- deregister \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233 \
  --threshold 2/3 \
  --spo-public-key aabbcc
```

#### 3.1.5. Committee hash update

```
nix run .#sidechain-main-cli -- committee-hash \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
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

#### 3.1.6. Save Merkle root

```
nix run .#sidechain-main-cli -- save-root \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
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
nix run .#sidechain-main-cli -- committee-handover \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
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

#### 3.1.8 Candidiate permission token

```
nix run .#sidechain-main-cli -- candidate-permission-token \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233 \
  --threshold 2/3 \
  --candidate-permission-token-amount 10 \
  --candidate-permission-token-name "aabb" \
  --candidate-permission-token-utxo "9ce50ad2562c18295e8a76ade473eeb17fc32f978961ab5adf78704d70b23834#1"
```

#### 3.1.8 Save checkpoint

```
nix run .#sidechain-main-cli -- save-checkpoint \
  --payment-signing-key-file $SIGNING_KEY \
  --genesis-committee-hash-utxo df24e6edc13440da24f074442a858f565b5eba0a9c8d6238988485a3ed64cf1f#0 \
  --sidechain-id 1 \
  --sidechain-genesis-hash 112233 \
  --threshold 2/3 \
  --new-checkpoint-block-hash d8063cc6e907f497360ca50238af5c2e2a95a8869a2ce74ab3e75fe6c9dcabd0d8063cc6e907f497360ca50238af5c2e2a95a8869a2ce74ab3e75fe6c9dcabd0 \
  --new-checkpoint-block-number 42 \
  --sidechain-epoch 5
```
### 3.2. Using a configuration file

You can also provide a configuration in `$CWD/config.json` in the following format instead of repeating them in all commands.

```json
{
  "sidechainParameters": {
    "chainId": 123,
    "genesisHash": "11223344aabbcc",
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
nix run .#sidechain-main-cli -- burn --amount 5 --recipient aabb
```

You can find a sample configuration file in `ctl/config.example.json`.

When using the CLI argument and the configuration file together, the **CLI arguments override** these configuration values. You can also set any of the above values to null if you don't want to set a default value for that property.

### 3.3. Configuring hosted runtime dependencies

You can set the network configuration of the runtime dependencies in the configuration file using the following format:

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
  },
  "signingKeyFile": null
}
```

Any service where no configuration is defined will fall back to its default value (localhost).
