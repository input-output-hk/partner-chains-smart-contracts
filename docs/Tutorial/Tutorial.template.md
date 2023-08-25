# Introduction
This document is a tutorial for how to use `sidechain-main-cli` to interact
with the Cardano blockchain on the preview testnet.

# Prerequisites
We discuss prerequisites for running the system.

Many of the following programs are available in the `nix develop`
shell of this project.
Thus, to follow along, please execute `nix develop` in this project.

## Programs
We discuss some features of the common programming tools we will use.

### Common shell utilities `seq`, shell `for` loops, `printf`, `tee`
`seq` will be used to generate a list of numbers.
Consider the following execution of `seq`.
```bash
seq 1 1 4
```
Note that it starts counting at 1, increments by 1, and ends at 4.

We will often use `for` loops in the shell.
To briefly review, if we wanted to accumulate a list of space separated words
with a unique counter as a suffix, we could execute the following command

```bash
FOR_TEST=
for i in $(seq 1 1 4)
do FOR_TEST="$FOR_TEST yorkie$i"
done
echo "$FOR_TEST"
```
Note that we:

1. Initialize the shell variable `FOR_TEST` to the empty string.

2. In the `for` loop we set `FOR_TEST` to itself, a space, the word
   `yorkie`, then the integer `i` where `i` counts from 1 through 4 as the
   result of `seq`'s output from command substitution.

`printf` is used for printing things to stdout.
In particular, the first argument is a format string which controls how the
rest of the arguments are printed.
Make note of the following examples.

```bash
printf " --flag %s " "value"
printf " --flag %s:%s " "first_value" "second_value"
```

`tee` allows one to read from stdin, and output stdin to both a file and
stdout.
In this tutorial, we will find it instructive to pipe outputs to stdout in
addition to saving the outputs to a file.
In practise, it's probably not necessary to pipe outputs to stdout, and one can
redirect outputs exclusively to a file (or not at all).
The following command creates a small JSON object of information about dog
    breeds using `tee` to write to the file `test.json` while showing the
    JSON object on stdout.

```bash
echo '{ "pomeranian" : { "colour": "variety" },  "maltese" : { "colour": "white" } }' \
    | tee test.json
```

### `jq`
We will use `jq` for working with JSON objects.

Suppose we have the JSON object in `test.json` defined previously.
```bash
cat test.json
```
Note we use `cat` to print the contents of `test.json` to stdout.

If we'd like to simply pretty print `test.json`, we may execute the following
command.
```bash
cat test.json | jq
```

Note that we use `cat` to pipe `test.json` to stdout, which via the pipe is fed
to `jq`.

If we'd like to get the `"colour"` of `"pomeranian"` in `test.json`, we could
execute the following.
```bash
jq -r .pomeranian.colour test.json
```
Note that we use the flag `-r` to tell `jq` to print the "raw" outputs instead
    of putting JSON quotes around strings.
Moreover, the argument `'.pomeranian.colour'` is the important argument which
    allows us to extract the value of the pomeranian's colour.

If we'd like to get any key of `test.json` (either `pomeranian` or `maltese`),
    we could execute the following.
```bash
jq -r 'keys_unsorted[0]' test.json
```

Note that `keys_unsorted` grabs us the keys of the JSON object (unsorted) as an
    array, and `[0]` tells `jq` to access the 0th element of the array.

## Cardano key pairs
We will assume that the reader has their own wallet address key pairs.

In particular, we will assume that we have the following shell variables set.

- `ADDR`: human readable bech32 Cardano wallet payment address.
- `SIGNING_KEY`: file path to the corresponding secret key of the public
  verification key of `ADDR`.

For example, I have these set as follows.
```bash
ADDR=$(cat ./payment.addr)
SIGNING_KEY=./payment.skey
echo $ADDR
echo $SIGNING_KEY
```

See [here](https://developers.cardano.org/docs/operate-a-stake-pool/cardano-key-pairs/)
    for details of the different Cardano key pairs,
    and see [here](https://github.com/input-output-hk/cardano-node-wiki/blob/main/docs/reference/cardano-node-cli-reference.md)
    for the Cardano node CLI reference for generating such key pairs.

## Setting up the runtime environment
In order to run the system, we need the following runtime dependencies.

- `cardano-node`
- `ogmios` which provides a WebSocket interface with `cardano-node`
- `kupo` which provides a method to query onchain information.

We have provided a convenient way to launch the runtime dependencies
    for the preview test net with `docker`.
Consider running the following command.

```bash
nix run .#ctl-runtime-preview >ctl-runtime.log 2>&1 &
```

Note that we redirect stdout to the file `ctl-runtime.log`,
    and duplicate stdout (which now redireects to the file `ctl-runtime.log`)
        to stderr i.e., all outputs are redirected to `ctl-runtime.log`.
One can view the logs with a command like `less ctl-runtime.log`.

Since the docker images run "persistent" processes that must be online
    in later steps in the tutorial, we run it in the background (as indicated with `&`)
    to ensure that we can still do things in this terminal window.
Alternatively, one could run `nix run .#ctl-runtime-preview` in a separate
    terminal window.

This may take some time to load, so it's good to wait a few (say 5) seconds for
everything to load up.
```bash
sleep 5
```

We can then see the docker images that we have created with the following
command.
```bash
docker ps
```
Make note of which docker image is the cardano node.
At the time of writing this, this is `ctl-runtime_cardano-node_1`.

Using `docker`, we can run commands in the docker image
    `ctl-runtime_cardano-node_1`.
For example, if we want to query all the UTxOs we may spend on the preview
    testnet, we may execute the following command.

```bash
docker exec \
    -e CARDANO_NODE_SOCKET_PATH="/ipc/node.socket" ctl-runtime_cardano-node_1 \
    cardano-cli query utxo --testnet-magic 2 --address "$ADDR" \
        | head -n 4
```

Note that `-e CARDANO_NODE_SOCKET_PATH="/ipc/node.socket"` sets an environment variable
    for the command `cardano-cli` in the docker image `ctl-runtime_cardano-node_1`.
For details about the arguments to `cardano-cli`, please see
[here](https://github.com/input-output-hk/cardano-node-wiki/blob/main/docs/reference/cardano-node-cli-reference.md).
Also, note we use `head -n 4` to show only the first 4 lines as the output can get
large.

From a programmers point of view, while the table output is readable, it isn't
    very nice to work with.
Fortunately, we may surprisingly make `cardano-cli` dump out JSON objects of
    the UTxOs we may spend by passing the flag `--out-file /dev/stdout` for
    which the outputted JSON object has string keys `TX_HASH#TX_IX` and values
    JSON objects.
We are only concerned about the keys of this JSON object i.e., a UTxO's
    `TX_HASH_TX_IX` information.
So, we can pick any UTxO we may spend with the following command.

```bash
docker exec \
    -e CARDANO_NODE_SOCKET_PATH="/ipc/node.socket" ctl-runtime_cardano-node_1 \
    cardano-cli query utxo --testnet-magic 2 --address "$ADDR" --out-file /dev/stdout \
    | jq -r 'keys_unsorted[0]'
```

# Overview
We will demonstrate the following workflow.

_Workflow._

1. Generate a sidechain committee.

2. Initialize the sidechain.

3. Save a Merkle root of transactions from the sidechain.

4. Claim one's FUEL from the Merkle root of transactions from the sidechain.

5. Update the committee to a new committee.

6. Save another Merkle root of transactions from the sidechain.

We will demonstrate this workflow with both

- ECDSA SECP256k1 key pairs; and

- Schnorr SECP256k1 key pairs

in two distinct sidechains.
We will call the first sidechain the ECDSA SECP256k1 sidechain;
    and we will call the second sidechain the Schnorr SECP256k1 sidechain.

Often, the commands for the Schnorr SECP256k1 sidechain will be essentially identical
    to the commands for the ECDSA SECP256k1 sidechain, so we will only focus on the differences
    when describing the Schnorr SECP256k1 sidechain.

In practise, we probably don't want to initialize 2 sidechains with distinct
    signing mechanisms so one should only follow the steps for either of the
    sidechains.

## Workflow.

### 1. Generate a sidechain committee.
We need to generate an initial sidechain committee which
    from the perspective of the mainchain ceritifies whether a sequence of
    interchain transactions has occurred on the sidechain.

#### ECDSA SECP256k1 sidechain commands
The following command generates an ECDSA SECP256k1 key pair.

```bash
nix run .#sidechain-main-cli -- utils key-gen ecdsa-secp256k1 \
    | jq
```

We want to generate a sidechain committee of 4 members.
We will put each sidechain committee member in files:
- `EcdsaSecp256k1ScCommitteeMember1.json`,
- `EcdsaSecp256k1ScCommitteeMember2.json`,
- `EcdsaSecp256k1ScCommitteeMember3.json`, and
- `EcdsaSecp256k1ScCommitteeMember4.json`.

For convenience, we will first put the list of files in an environment variable
    `ECDSA_SECP256K1_SC_COMMITTEE` with the following commands.

```bash
ECDSA_SECP256K1_SC_COMMITTEE=
for i in $(seq 1 1 4)
do ECDSA_SECP256K1_SC_COMMITTEE="$ECDSA_SECP256K1_SC_COMMITTEE EcdsaSecp256k1ScCommitteeMember$i.json"
done
echo $ECDSA_SECP256K1_SC_COMMITTEE
```

Then, we can generate the sidechain committee with the following command.

```bash
for SC_MEMBER in $ECDSA_SECP256K1_SC_COMMITTEE
do nix run .#sidechain-main-cli -- utils key-gen ecdsa-secp256k1 \
    | tee "$SC_MEMBER" \
    | jq
done
```
#### Schnorr SECP256k1 sidechain commands
Alternatively, one can use the following command to generate a Schnorr
SECP256k1 key pair.

```bash
nix run .#sidechain-main-cli -- utils key-gen schnorr-secp256k1 \
    | jq
```

Then, similarly to last time, we can create a sidechain of 4 committee members
with Schnorr public / private key pairs.

```bash
# SCHNORR_SECP256K1_SC_COMMITTEE is the shell variable for the files
#   - SchnorrSecp256k1ScCommitteeMember1.json
#   - SchnorrSecp256k1ScCommitteeMember2.json
#   - SchnorrSecp256k1ScCommitteeMember3.json
#   - SchnorrSecp256k1ScCommitteeMember4.json
SCHNORR_SECP256K1_SC_COMMITTEE=
for i in $(seq 1 1 4)
do SCHNORR_SECP256K1_SC_COMMITTEE="$SCHNORR_SECP256K1_SC_COMMITTEE SchnorrSecp256k1ScCommitteeMember$i.json"
done
echo $SCHNORR_SECP256K1_SC_COMMITTEE

# Generate the key pairs
for SC_MEMBER in $SCHNORR_SECP256K1_SC_COMMITTEE
do nix run .#sidechain-main-cli -- utils key-gen schnorr-secp256k1 \
    | tee "$SC_MEMBER" \
    | jq
done
```

### 2. Initialize the sidechain
Initializing the sidechain amounts to:

1. Finding a distinguished UTxO to spend to uniquely identify the committee
   onchain.

2. With the previous UTxO, we may initialize the sidechain with the committee
   we just generated.

#### ECDSA SECP256k1 commands
We show how to find a distinguished UTxO we may spend.
Since we will use this frequently later, we will set this to the shell variable
    `ECDSA_SECP256K1_GENESIS_UTXO`.
Consider the following command.
```bash
ECDSA_SECP256K1_GENESIS_UTXO=$(docker exec \
        -e CARDANO_NODE_SOCKET_PATH="/ipc/node.socket" ctl-runtime_cardano-node_1 \
        cardano-cli query utxo --testnet-magic 2 --address "$ADDR" --out-file /dev/stdout \
        | jq -r 'keys_unsorted[0]')
echo $ECDSA_SECP256K1_GENESIS_UTXO
```

Note the format.

```
TX_HASH#TX_IX
```

Now, we will initialize the sidechain with our committee we generated in the
    previous step.
Note that we will need to pass the `rawHexPublicKey`s of each of our committee
    members as a flag to `sidechain-main-cli` with the flag `--committee-pub-key`.
The following command initializes the sidechain with the committee from the previous step.

```bash
nix run .#sidechain-main-cli -- init \
    --payment-signing-key-file $SIGNING_KEY \
    --genesis-committee-hash-utxo $ECDSA_SECP256K1_GENESIS_UTXO \
    --sidechain-id 69 \
    --sidechain-genesis-hash 112233 \
    --threshold-numerator 1 \
    --threshold-denominator 2 \
    --atms-kind plain-ecdsa-secp256k1 \
    $(for SC_MEMBER in $ECDSA_SECP256K1_SC_COMMITTEE
        do printf " --committee-pub-key %s " "$(jq -r .rawHexPublicKey "$SC_MEMBER")"
        done) \
    --sidechain-epoch 0 \
    | jq
```

#### Schnorr SECP256k1 sidechain commands
Initializing the Schnorr SECP256k1 sidechain is essentially identical to
    initializing the ECDSA SECP256k1 sidechain
    with the only difference being we must have the flag
    `--atms-kind plain-schnorr-secp256k1` instead `--atms-kind plain-ecdsa-secp256k1`.
We also create similar shell variables / files for the Schnorr SECP256k1
    sidechain as we did with the ECDSA SECP256k1 sidechain with changes in the
    naming.

```bash
# Finding the distinguished UTxO
SCHNORR_SECP256K1_GENESIS_UTXO=$(docker exec \
        -e CARDANO_NODE_SOCKET_PATH="/ipc/node.socket" ctl-runtime_cardano-node_1 \
        cardano-cli query utxo --testnet-magic 2 --address "$ADDR" --out-file /dev/stdout \
        | jq -r 'keys_unsorted[0]')
echo $SCHNORR_SECP256K1_GENESIS_UTXO

# Initializing the sidechain
nix run .#sidechain-main-cli -- init \
    --payment-signing-key-file $SIGNING_KEY \
    --genesis-committee-hash-utxo $SCHNORR_SECP256K1_GENESIS_UTXO \
    --sidechain-id 69 \
    --sidechain-genesis-hash 112233 \
    --threshold-numerator 1 \
    --threshold-denominator 2 \
    --atms-kind plain-schnorr-secp256k1 \
    $(for SC_MEMBER in $SCHNORR_SECP256K1_SC_COMMITTEE
        do printf " --committee-pub-key %s " "$(jq -r .rawHexPublicKey "$SC_MEMBER")"
        done) \
    --sidechain-epoch 0 \
    | jq
```

### 3. Save a Merkle root of transactions from the sidechain
Saving a Merkle root of transactions from the sidechain to the mainchain
amounts to:

1. Creating a Merkle root of transactions from the sidechain.

2. Creating the Merkle root insertion message of the Merkle root

3. Signing the Merkle root insertion message.

4. Submitting the signed Merkle root (and the signatures) to the blockchain.

To create a Merkle root of transactions from the sidechain, we need to first
create a sequence of Merkle tree entries that summarize transactions from the
sidechain.
We will create two Merkle tree entries `MerkleTreeEntry1.json` and
`MerkleTreeEntry2.json`.

`MerkleTreeEntry1.json` will be created s.t.

- It mints 69 FUEL tokens.

- The recipient is `$ADDR`.

The following command creates this `MerkleTreeEntry1.json`.

```bash
nix run .#sidechain-main-cli -- utils encode cbor-merkle-tree-entry \
    --index 1 \
    --amount 69 \
    --recipient $ADDR \
        | tee "MerkleTreeEntry1.json" \
        | jq
```

Note that the only requirement of the `--index` flag is that it needs to be
unique amongst Merkle tree entries in the same Merkle tree.
Since this is the first Merkle tree entry, we chose this to be `1` but this
could have been an arbitrary integer.

Also, there is an optional flag, `--previous-merkle-root`, which is the
previous Merkle root (if it exists). Since this is the first Merkle root we are
submitting to the blockchain, we know that no such Merkle root exists so we do
not include the flag in this case.
But, _all_ of the following instances of saving a Merkle root to the mainchain
    _must_ include the `--previous-merkle-root` flag (this is not checked by
    the system as the `sidechain-main-cli` is missing the architecture to
    efficiently check this) as the previous Merkle root to guarantee uniqueness
    of Merkle tree entries amongst different Merkle trees.

Now, we create the Merkle tree entry `MerkleTreeEntry2.json` which satisfies the following

- It mints 420 FUEL tokens.

- The recipient is `$ADDR`.

The following command creates `MerkleTreeEntry2.json`.
```bash
nix run .#sidechain-main-cli -- utils encode cbor-merkle-tree-entry \
    --index 2 \
    --amount 420 \
    --recipient $ADDR \
        | tee "MerkleTreeEntry2.json" \
        | jq
```

With `MerkleTreeEntry1.json` and `MerkleTreeEntry2.json`, we create the Merkle
    root.
The following command creates a Merkle root from `MerkleTreeEntry1.json` and
`MerkleTreeEntry2.json`, and saves it to the file `MerkleTree.json`.
```bash
nix run .#sidechain-main-cli -- utils encode cbor-merkle-tree \
    --cbor-merkle-tree-entry $(jq -r .cborHexMerkleTreeEntry MerkleTreeEntry1.json) \
    --cbor-merkle-tree-entry $(jq -r .cborHexMerkleTreeEntry MerkleTreeEntry2.json) \
        | tee "MerkleTree.json" \
        | jq
```

The next step will be to create a Merkle root insertion message from the Merkle
root.
This is where the steps for the ECDSA SECP256k1 sidechain and the Schnorr
SECP256k1 sidechain diverge.

#### ECDSA SECP256k1 sidechain commands
The following command creates the Merkle root insertion message to the file
    `EcdsaMerkleRootInsertionMessage.json` for the ECDSA SECP256k1 sidechain.
```bash
nix run .#sidechain-main-cli -- utils encode cbor-merkle-root-insertion-message \
    --sidechain-id 69 \
    --sidechain-genesis-hash 112233 \
    --threshold-numerator 1 \
    --threshold-denominator 2 \
    --genesis-committee-hash-utxo $ECDSA_SECP256K1_GENESIS_UTXO \
    --merkle-root $(jq -r .rawHexMerkleRoot MerkleTree.json) \
        | tee EcdsaMerkleRootInsertionMessage.json \
        | jq
```

It's important to note that if there did exist a previous Merkle root, we
    _must_ include the flag `--previous-merkle-root` with the previous Merkle root.

Then, to sign the Merkle root insertion message with a ECDSA SECP256k1 private key, say
`EcdsaSecp256k1ScCommitteeMember1.json`, we execute the following command.
```bash
nix run .#sidechain-main-cli -- utils sign ecdsa-secp256k1 \
    --private-key $(jq -r .rawHexPrivateKey EcdsaSecp256k1ScCommitteeMember1.json) \
    --message $(jq -r .cborHexMerkleRootInsertionMessage EcdsaMerkleRootInsertionMessage.json) \
        | jq
```
A similar command (by replacing `EcdsaSecp256k1ScCommitteeMember1.json` with any committee member)
    can be used to sign `EcdsaMerkleRootInsertionMessage.json`.

Finally, we show how to submit the Merkle root to the blockchain.
Note that the flags
    `--committee-pub-key-and-signature PUBLIC_KEY[:[SIGNATURE]]`
    altogether must provide the entire current committee,
    and note that the signature is optional if the committee member has not
    signed the message.
In particular, since the sidechain has `--threshold-numerator 1` and
    `--threshold-denominator 2`, this means that strictly more than 1/2 of the
    sidechain committee member's must provide a signature.
The following command submits the Merkle root to the blockchain
    with the required signatures where the entire committee signs the message.
```bash
nix run .#sidechain-main-cli -- save-root \
    --payment-signing-key-file $SIGNING_KEY \
    --genesis-committee-hash-utxo $ECDSA_SECP256K1_GENESIS_UTXO \
    --sidechain-id 69 \
    --sidechain-genesis-hash 112233 \
    --threshold-numerator 1 \
    --threshold-denominator 2 \
    --atms-kind plain-ecdsa-secp256k1 \
    $( for SC_MEMBER in $ECDSA_SECP256K1_SC_COMMITTEE
        do printf " --committee-pub-key-and-signature %s:%s " \
                "$(jq -r .rawHexPublicKey $SC_MEMBER)" \
                "$(nix run .#sidechain-main-cli -- utils sign ecdsa-secp256k1 \
                    --private-key $(jq -r .rawHexPrivateKey $SC_MEMBER) \
                    --message $(jq -r .cborHexMerkleRootInsertionMessage EcdsaMerkleRootInsertionMessage.json) \
                        | jq -r .rawHexSignature
                    )"
        done
    ) \
    --merkle-root $(jq -r .rawHexMerkleRoot MerkleTree.json) \
        | jq
```

#### Schnorr SECP256k1 sidechain commands
Similarly to ECDSA SECP256k1 sidechain, we need to create a Merkle root
    insertion message.
The following command creates the Merkle root insertion message and redirects it
    to the file `SchnorrMerkleRootInsertionMessage.json` for the Schnorr
    SECP256k1 sidechain where we note that we use the
    `SCHNORR_SECP256K1_GENESIS_UTXO`.
```bash
nix run .#sidechain-main-cli -- utils encode cbor-merkle-root-insertion-message \
    --sidechain-id 69 \
    --sidechain-genesis-hash 112233 \
    --threshold-numerator 1 \
    --threshold-denominator 2 \
    --genesis-committee-hash-utxo $SCHNORR_SECP256K1_GENESIS_UTXO \
    --merkle-root $(jq -r .rawHexMerkleRoot MerkleTree.json) \
        | tee SchnorrMerkleRootInsertionMessage.json \
        | jq
```

Then, we can create a Schnorr signature with, say
`SchnorrSecp256k1ScCommitteeMember1.json`, as follows.
```bash
nix run .#sidechain-main-cli -- utils sign schnorr-secp256k1 \
    --private-key $(jq -r .rawHexPrivateKey SchnorrSecp256k1ScCommitteeMember1.json) \
    --message $(jq -r .cborHexMerkleRootInsertionMessage SchnorrMerkleRootInsertionMessage.json) \
        | jq
```

And finally, we can submit the transaction to the blockchain as follows.
```bash
nix run .#sidechain-main-cli -- save-root \
    --payment-signing-key-file $SIGNING_KEY \
    --genesis-committee-hash-utxo $SCHNORR_SECP256K1_GENESIS_UTXO \
    --sidechain-id 69 \
    --sidechain-genesis-hash 112233 \
    --threshold-numerator 1 \
    --threshold-denominator 2 \
    --atms-kind plain-schnorr-secp256k1 \
    $( for SC_MEMBER in $SCHNORR_SECP256K1_SC_COMMITTEE
        do printf " --committee-pub-key-and-signature %s:%s " \
                "$(jq -r .rawHexPublicKey $SC_MEMBER)" \
                "$(nix run .#sidechain-main-cli -- utils sign schnorr-secp256k1 \
                    --private-key $(jq -r .rawHexPrivateKey $SC_MEMBER) \
                    --message $(jq -r .cborHexMerkleRootInsertionMessage SchnorrMerkleRootInsertionMessage.json) \
                        | jq -r .rawHexSignature
                    )"
        done
    ) \
    --merkle-root $(jq -r .rawHexMerkleRoot MerkleTree.json) \
        | jq
```

### 4. Claim one's FUEL from the Merkle root of transactions from the sidechain.
We show how to claim our FUEL from the Merkle root of transactions we just
submitted to the sidechain.
In particular, we will claim the FUEL tokens from `MerkleTreeEntry1.json`.

To do this, we first need to create a combined Merkle proof from from the
    Merkle tree, `MerkleTree.json`, and the Merkle tree entry `MerkleTreeEntry1.json`.
The following creates the combined Merkle proof and redirects it to the file
    `CombinedMerkleProof1.json`.

```bash
nix run .#sidechain-main-cli -- utils encode cbor-combined-merkle-proof \
    --cbor-merkle-tree-entry $(jq -r .cborHexMerkleTreeEntry MerkleTreeEntry1.json) \
    --cbor-merkle-tree $(jq -r .cborHexMerkleTree MerkleTree.json) \
        | tee CombinedMerkleProof1.json \
        | jq
```

Then, we can submit a transaction to the blockchain that uses the combined Merkle proof
    `CombinedMerkleProof1.json` to claim our FUEL.

#### ECDSA SECP256k1 sidechain commands
The following command submits the transaction to save the Merkle root to the
ECDSA SECP256k1 sidechain.
```bash
nix run .#sidechain-main-cli -- claim \
    --payment-signing-key-file $SIGNING_KEY \
    --genesis-committee-hash-utxo $ECDSA_SECP256K1_GENESIS_UTXO \
    --sidechain-id 69 \
    --sidechain-genesis-hash 112233 \
    --threshold-numerator 1 \
    --threshold-denominator 2 \
    --atms-kind plain-ecdsa-secp256k1 \
    --combined-proof "$(jq -r .cborHexCombinedMerkleProof CombinedMerkleProof1.json)" \
        | jq
```

#### Schnorr SECP256k1 sidechain commands
The following command submits the transaction to save the Merkle root to the
Schnorr SECP256k1 sidechain.

```bash
nix run .#sidechain-main-cli -- claim \
    --payment-signing-key-file $SIGNING_KEY \
    --genesis-committee-hash-utxo $SCHNORR_SECP256K1_GENESIS_UTXO \
    --sidechain-id 69 \
    --sidechain-genesis-hash 112233 \
    --threshold-numerator 1 \
    --threshold-denominator 2 \
    --atms-kind plain-schnorr-secp256k1 \
    --combined-proof "$(jq -r .cborHexCombinedMerkleProof CombinedMerkleProof1.json)" \
        | jq
```

#### ECDSA SECP256k1 sidechain commands for verifying we have our claimed FUEL

For testing, it's good to verify that we have our claimed FUEL.
To do this, we need to identify the currency symbol of FUEL.
The following command dumps all the addresses, and currency symbols
    related to the sidechain.

```bash
nix run .#sidechain-main-cli -- addresses \
    --payment-signing-key-file $SIGNING_KEY \
    --genesis-committee-hash-utxo $ECDSA_SECP256K1_GENESIS_UTXO \
    --sidechain-id 69 \
    --sidechain-genesis-hash 112233 \
    --threshold-numerator 1 \
    --threshold-denominator 2 \
    --atms-kind plain-ecdsa-secp256k1 \
        | jq
```

Note that it gives `mintingPolicies` which has `FuelMintingPolicyId` which is
    the currency symbol of FUEL minting policy.

Then, we can query the UTxOs we may spend with `cardano-cli`, and observe that
we may spend a UTxO which has 69 FUEL tokens which corresponds to
`MerkleTreeEntry1.json`.
```bash
docker exec \
    -e CARDANO_NODE_SOCKET_PATH="/ipc/node.socket" ctl-runtime_cardano-node_1 \
    cardano-cli query utxo --testnet-magic 2 --address "$ADDR" \
        | grep \
            "$( nix run .#sidechain-main-cli -- addresses \
                --payment-signing-key-file $SIGNING_KEY \
                --genesis-committee-hash-utxo $ECDSA_SECP256K1_GENESIS_UTXO \
                --sidechain-id 69 \
                --sidechain-genesis-hash 112233 \
                --threshold-numerator 1 \
                --threshold-denominator 2 \
                --atms-kind plain-ecdsa-secp256k1 \
                    | jq -r .mintingPolicies.FuelMintingPolicyId)"
```

#### Schnorr SECP256k1 sidechain commands for verifying we have our claimed FUEL

This is essentially the same as the ECDSA SECP256k1 sidechain except we use the
    alternate `SCHNORR_SECP256K1_GENESIS_UTXO` and the flag
    `--atms-kind plain-schnorr-secp256k1`.

```bash
# Getting the addresses of the Schnorr sidechain
nix run .#sidechain-main-cli -- addresses \
    --payment-signing-key-file $SIGNING_KEY \
    --genesis-committee-hash-utxo $SCHNORR_SECP256K1_GENESIS_UTXO \
    --sidechain-id 69 \
    --sidechain-genesis-hash 112233 \
    --threshold-numerator 1 \
    --threshold-denominator 2 \
    --atms-kind plain-schnorr-secp256k1 \
        | jq

# Verifying that we have the Schnorr sidechain's FUEL tokens.
docker exec \
    -e CARDANO_NODE_SOCKET_PATH="/ipc/node.socket" ctl-runtime_cardano-node_1 \
    cardano-cli query utxo --testnet-magic 2 --address "$ADDR" \
        | grep \
            "$( nix run .#sidechain-main-cli -- addresses \
                --payment-signing-key-file $SIGNING_KEY \
                --genesis-committee-hash-utxo $SCHNORR_SECP256K1_GENESIS_UTXO \
                --sidechain-id 69 \
                --sidechain-genesis-hash 112233 \
                --threshold-numerator 1 \
                --threshold-denominator 2 \
                --atms-kind plain-schnorr-secp256k1 \
                    | jq -r .mintingPolicies.FuelMintingPolicyId)"
```

### 5. Update the committee to a new committee.
We show how to update the committee onchain to a new committee.
This amounts to doing the following.

1. Creating a new committee (identical to before).

2. Creating the update committee message which contains the data to show that
   the old committee wishes to transfer control to the new committee we just
   generated.

3. Signing the update committee message.

4. Submitting the transaction which updates the committee to the blockchain.

#### ECDSA SECP256k1 sidechain commands.

We first create a new committee of committee members
- `NewEcdsaSecp256k1ScCommitteeMember1.json`
- `NewEcdsaSecp256k1ScCommitteeMember2.json`
- `NewEcdsaSecp256k1ScCommitteeMember3.json`
- `NewEcdsaSecp256k1ScCommitteeMember4.json`.

We will also put these new committee members in the shell variable
    `NEW_ECDSA_SECP256K1_SC_COMMITTEE`.
This process is identical to the last time we created the committee `ECDSA_SECP256K1_SC_COMMITTEE`,
    so we will simply give the commands for this.
```bash
# Create the shell variable NEW_ECDSA_SECP256K1_SC_COMMITTEE
NEW_ECDSA_SECP256K1_SC_COMMITTEE=
for i in $(seq 1 1 4)
do NEW_ECDSA_SECP256K1_SC_COMMITTEE="$NEW_ECDSA_SECP256K1_SC_COMMITTEE NewEcdsaSecp256k1ScCommitteeMember$i.json"
done
echo $NEW_ECDSA_SECP256K1_SC_COMMITTEE

# Create the public / private key pairs of the new committee members
for SC_MEMBER in $NEW_ECDSA_SECP256K1_SC_COMMITTEE
do nix run .#sidechain-main-cli -- utils key-gen ecdsa-secp256k1 \
    | tee "$SC_MEMBER" \
    | jq
done
```

The next step is to create the update committee message which contains the data
to show that the old committee `ECDSA_SECP256K1_SC_COMMITTEE` wishes to transfer control to the
new committee  `NEW_ECDSA_SECP256K1_SC_COMMITTEE`.
In particular, the update committee message must contain:

- The aggregated public key of `NEW_ECDSA_SECP256K1_SC_COMMITTEE`.

- The validator address of the UTxO which holds `NEW_ECDSA_SECP256K1_SC_COMMITTEE`.

So, we first create the aggregated public key of `NEW_ECDSA_SECP256K1_SC_COMMITTEE`. We do this
with the following command, and redirect it to the file
`NextAggregateEcdsaSecp256k1PublicKeys.json`.

```bash
nix run .#sidechain-main-cli -- utils encode cbor-plain-aggregate-public-keys \
    $( for SC_MEMBER in $NEW_ECDSA_SECP256K1_SC_COMMITTEE
        do printf " --public-key %s" "$(jq -r .rawHexPublicKey $SC_MEMBER)"
        done) \
    | tee NextAggregateEcdsaSecp256k1PublicKeys.json | jq
```

Then, we need to get the validator address of the next UTxO which will hold
`NEW_ECDSA_SECP256K1_SC_COMMITTEE`.
This is provided to us from the `addresses` subcommand from
`sidechain-main-cli`.
We can get this with the following command.
```bash
CBOR_ENCODED_COMMITTEE_VALIDATOR_ADDR=$(nix run .#sidechain-main-cli -- addresses \
    --payment-signing-key-file $SIGNING_KEY \
    --genesis-committee-hash-utxo $ECDSA_SECP256K1_GENESIS_UTXO \
    --sidechain-id 69 \
    --sidechain-genesis-hash 112233 \
    --threshold-numerator 1 \
    --threshold-denominator 2 \
    --atms-kind plain-ecdsa-secp256k1 \
        | jq -r .cborEncodedAddresses.CommitteeHashValidator)
echo $CBOR_ENCODED_COMMITTEE_VALIDATOR_ADDR
```

Then, we can finally create the update committee message and redirect it
to the file `UpdateEcdsaSecp256k1Message.json` with the following command.

```bash
nix run .#sidechain-main-cli -- utils encode cbor-update-committee-message \
    --genesis-committee-hash-utxo $ECDSA_SECP256K1_GENESIS_UTXO \
    --sidechain-id 69 \
    --sidechain-genesis-hash 112233 \
    --threshold-numerator 1 \
    --threshold-denominator 2 \
    --cbor-aggregated-public-keys $(jq -r .cborHexPlainAggregatedPublicKeys NextAggregateEcdsaSecp256k1PublicKeys.json) \
    --sidechain-epoch 1 \
    --new-committee-validator-cbor-encoded-address $CBOR_ENCODED_COMMITTEE_VALIDATOR_ADDR \
    --previous-merkle-root $(jq -r .rawHexMerkleRoot MerkleTree.json) \
        | tee "UpdateEcdsaSecp256k1Message.json" \
        | jq
```

Some notes:
    - `--sidechain-epoch` must be strictly greater than the last
      `--sidechain-epoch` (which we recall started at 0)

    - `--previous-merkle-root` is the optional flag which must be the previous
      Merkle root (if it exists).

Finally, we can submit the transaction which updates the committee.
Note that similarly to the `save-root` subcommand, we must include
the current committee with its signatures with the flag
`--committee-pub-key-and-signature PUBLIC_KEY[:[SIGNATURE]]`.
We also must include _all_ new committee members with the flag
`--new-committee-pub-key`.

```bash
nix run .#sidechain-main-cli -- committee-hash \
    --payment-signing-key-file $SIGNING_KEY \
    --genesis-committee-hash-utxo $ECDSA_SECP256K1_GENESIS_UTXO \
    --sidechain-id 69 \
    --sidechain-genesis-hash 112233 \
    --threshold-numerator 1 \
    --threshold-denominator 2 \
    --atms-kind plain-ecdsa-secp256k1 \
    $( for SC_MEMBER in $ECDSA_SECP256K1_SC_COMMITTEE
        do printf " --committee-pub-key-and-signature %s:%s " \
                "$(jq -r .rawHexPublicKey $SC_MEMBER)" \
                "$(nix run .#sidechain-main-cli -- utils sign ecdsa-secp256k1 \
                    --private-key $(jq -r .rawHexPrivateKey $SC_MEMBER) \
                    --message $(jq -r .cborHexUpdateCommitteeMessage UpdateEcdsaSecp256k1Message.json) \
                        | jq -r .rawHexSignature
                    )"
        done
    ) \
    $( for SC_MEMBER in $NEW_ECDSA_SECP256K1_SC_COMMITTEE
        do printf " --new-committee-pub-key %s " "$(jq -r .rawHexPublicKey $SC_MEMBER)"
        done
    ) \
    --sidechain-epoch 1 \
    --previous-merkle-root $(jq -r .rawHexMerkleRoot MerkleTree.json) \
    --new-committee-validator-cbor-encoded-address $CBOR_ENCODED_COMMITTEE_VALIDATOR_ADDR \
        | jq
```

#### Schnorr SECP256k1 sidechain commands
Again, this process is essentially identical to the ECDSA SECP256k1 sidechain
except we use generate a new Schnorr committee, and tell the sidechain that we
wish to use Schnorr signatures with the flag `--atms-kind
plain-schnorr-secp256k1`.

We first create a new committee with Schnorr committee members.
```bash
# Create the shell variable NEW_SCHNORR_SECP256K1_SC_COMMITTEE
NEW_SCHNORR_SECP256K1_SC_COMMITTEE=
for i in $(seq 1 1 4)
do NEW_SCHNORR_SECP256K1_SC_COMMITTEE="$NEW_SCHNORR_SECP256K1_SC_COMMITTEE NewSchnorrSecp256k1ScCommitteeMember$i.json"
done
echo $NEW_SCHNORR_SECP256K1_SC_COMMITTEE

# Create the public / private key pairs of the new committee members
for SC_MEMBER in $NEW_SCHNORR_SECP256K1_SC_COMMITTEE
do nix run .#sidechain-main-cli -- utils key-gen schnorr-secp256k1 \
    | tee "$SC_MEMBER" \
    | jq
done
```

Then, we create the message to sign and submit it to the blockchain.
```bash
# Aggregate the Schnorr public keys
nix run .#sidechain-main-cli -- utils encode cbor-plain-aggregate-public-keys \
    $( for SC_MEMBER in $NEW_SCHNORR_SECP256K1_SC_COMMITTEE
        do printf " --public-key %s" "$(jq -r .rawHexPublicKey $SC_MEMBER)"
        done) \
    | tee NextAggregateSchnorrSecp256k1PublicKeys.json | jq

# Grab the validator address
CBOR_ENCODED_COMMITTEE_VALIDATOR_ADDR=$(nix run .#sidechain-main-cli -- addresses \
    --payment-signing-key-file $SIGNING_KEY \
    --genesis-committee-hash-utxo $SCHNORR_SECP256K1_GENESIS_UTXO \
    --sidechain-id 69 \
    --sidechain-genesis-hash 112233 \
    --threshold-numerator 1 \
    --threshold-denominator 2 \
    --atms-kind plain-schnorr-secp256k1 \
        | jq -r .cborEncodedAddresses.CommitteeHashValidator)
echo $CBOR_ENCODED_COMMITTEE_VALIDATOR_ADDR

# Create the update committee message
nix run .#sidechain-main-cli -- utils encode cbor-update-committee-message \
    --genesis-committee-hash-utxo $SCHNORR_SECP256K1_GENESIS_UTXO \
    --sidechain-id 69 \
    --sidechain-genesis-hash 112233 \
    --threshold-numerator 1 \
    --threshold-denominator 2 \
    --cbor-aggregated-public-keys $(jq -r .cborHexPlainAggregatedPublicKeys NextAggregateSchnorrSecp256k1PublicKeys.json) \
    --sidechain-epoch 1 \
    --new-committee-validator-cbor-encoded-address $CBOR_ENCODED_COMMITTEE_VALIDATOR_ADDR \
    --previous-merkle-root $(jq -r .rawHexMerkleRoot MerkleTree.json) \
        | tee "UpdateSchnorrSecp256k1Message.json" \
        | jq

# Finally, submit the transaction to the blockchain
nix run .#sidechain-main-cli -- committee-hash \
    --payment-signing-key-file $SIGNING_KEY \
    --genesis-committee-hash-utxo $SCHNORR_SECP256K1_GENESIS_UTXO \
    --sidechain-id 69 \
    --sidechain-genesis-hash 112233 \
    --threshold-numerator 1 \
    --threshold-denominator 2 \
    --atms-kind plain-schnorr-secp256k1 \
    $( for SC_MEMBER in $SCHNORR_SECP256K1_SC_COMMITTEE
        do printf " --committee-pub-key-and-signature %s:%s " \
                "$(jq -r .rawHexPublicKey $SC_MEMBER)" \
                "$(nix run .#sidechain-main-cli -- utils sign schnorr-secp256k1 \
                    --private-key $(jq -r .rawHexPrivateKey $SC_MEMBER) \
                    --message $(jq -r .cborHexUpdateCommitteeMessage UpdateSchnorrSecp256k1Message.json) \
                        | jq -r .rawHexSignature
                    )"
        done
    ) \
    $( for SC_MEMBER in $NEW_SCHNORR_SECP256K1_SC_COMMITTEE
        do printf " --new-committee-pub-key %s " "$(jq -r .rawHexPublicKey $SC_MEMBER)"
        done
    ) \
    --sidechain-epoch 1 \
    --previous-merkle-root $(jq -r .rawHexMerkleRoot MerkleTree.json) \
    --new-committee-validator-cbor-encoded-address $CBOR_ENCODED_COMMITTEE_VALIDATOR_ADDR \
        | jq
```

### 6. Save another Merkle root of transactions from the sidechain.
We will finally demonstrate how to save another Merkle root of transactions.
It's important to note that this time, since we have previous Merkle root, it's
    necessary to include the `--previous-merkle-root` flag in the Merkle tree
    entries and the Merkle root insertion message.
Moreover, since we updated the committee, we must use
    the new sidechain committee instead of the sidechain committee we
    originally started with.
So, for the ECDSA SECP256k1 sidechain, we must use
    `NEW_ECDSA_SECP256K1_SC_COMMITTEE` instead of `ECDSA_SECP256K1_SC_COMMITTEE`;
    and for the Schnorr SECP256k1 sidechain, we must use
    `NEW_SCHNORR_SECP256K1_SC_COMMITTEE` instead of `SCHNORR_SECP256K1_SC_COMMITTEE`;

Since we've already detailed this process once, we simply just paste the
    commands to do this.

We create the Merkle tree entries `NextMerkleTreeEntry1.json` and
`NextMerkleTreeEntry2.json` as follows (note the `--previous-merkle-root`
flag).
```bash
nix run .#sidechain-main-cli -- utils encode cbor-merkle-tree-entry \
    --index 1 \
    --amount 69420 \
    --recipient $ADDR \
    --previous-merkle-root $(jq -r .rawHexMerkleRoot MerkleTree.json) \
        | tee "NextMerkleTreeEntry1.json" \
        | jq

nix run .#sidechain-main-cli -- utils encode cbor-merkle-tree-entry \
    --index 1 \
    --amount 42069 \
    --recipient $ADDR \
    --previous-merkle-root $(jq -r .rawHexMerkleRoot MerkleTree.json) \
        | tee "NextMerkleTreeEntry2.json" \
        | jq
```

Then, we create the Merkle tree `NextMerkleTree.json` as follows.
```bash
nix run .#sidechain-main-cli -- utils encode cbor-merkle-tree \
    --cbor-merkle-tree-entry $(jq -r .cborHexMerkleTreeEntry NextMerkleTreeEntry1.json) \
    --cbor-merkle-tree-entry $(jq -r .cborHexMerkleTreeEntry NextMerkleTreeEntry2.json) \
        | tee "NextMerkleTree.json" \
        | jq
```

Again, this is where the steps differ for the respective sidechains.

#### ECDSA SECP256k1 sidechain commands
We first create the  Merkle tree insertion message
`NextEcdsaSecp256k1MerkleTreeInsertionMessage.json` as follows (note the
`--previous-merkle-root` flag).
```bash
nix run .#sidechain-main-cli -- utils encode cbor-merkle-root-insertion-message \
    --sidechain-id 69 \
    --sidechain-genesis-hash 112233 \
    --threshold-numerator 1 \
    --threshold-denominator 2 \
    --genesis-committee-hash-utxo $ECDSA_SECP256K1_GENESIS_UTXO \
    --previous-merkle-root $(jq -r .rawHexMerkleRoot MerkleTree.json) \
    --merkle-root $(jq -r .rawHexMerkleRoot NextMerkleTree.json) \
        | tee NextEcdsaSecp256k1MerkleTreeInsertionMessage.json \
        | jq
```

And finally, we can save `NextMerkleTree.json`'s Merkle root to the blockchain.

Note we use `NEW_ECDSA_SECP256K1_SC_COMMITTEE` and
`NextEcdsaSecp256k1MerkleTreeInsertionMessage.json`.
```bash
nix run .#sidechain-main-cli -- save-root \
    --payment-signing-key-file $SIGNING_KEY \
    --genesis-committee-hash-utxo $ECDSA_SECP256K1_GENESIS_UTXO \
    --sidechain-id 69 \
    --sidechain-genesis-hash 112233 \
    --threshold-numerator 1 \
    --threshold-denominator 2 \
    --atms-kind plain-ecdsa-secp256k1 \
    $( for SC_MEMBER in $NEW_ECDSA_SECP256K1_SC_COMMITTEE
        do printf " --committee-pub-key-and-signature %s:%s " \
                "$(jq -r .rawHexPublicKey $SC_MEMBER)" \
                "$(nix run .#sidechain-main-cli -- utils sign ecdsa-secp256k1 \
                    --private-key $(jq -r .rawHexPrivateKey $SC_MEMBER) \
                    --message $(jq -r .cborHexMerkleRootInsertionMessage NextEcdsaSecp256k1MerkleTreeInsertionMessage.json) \
                        | jq -r .rawHexSignature
                    )"
        done
    ) \
    --previous-merkle-root $(jq -r .rawHexMerkleRoot MerkleTree.json) \
    --merkle-root $(jq -r .rawHexMerkleRoot NextMerkleTree.json) \
        | jq
```

#### Schnorr SECP256k1 sidechain commands
Like last time, we first create the message which we sign.
```bash
nix run .#sidechain-main-cli -- utils encode cbor-merkle-root-insertion-message \
    --sidechain-id 69 \
    --sidechain-genesis-hash 112233 \
    --threshold-numerator 1 \
    --threshold-denominator 2 \
    --genesis-committee-hash-utxo $SCHNORR_SECP256K1_GENESIS_UTXO \
    --previous-merkle-root $(jq -r .rawHexMerkleRoot MerkleTree.json) \
    --merkle-root $(jq -r .rawHexMerkleRoot NextMerkleTree.json) \
        | tee NextSchnorrSecp256k1MerkleTreeInsertionMessage.json \
        | jq
```

Then, we submit the transaction to the blockchain.
```bash
nix run .#sidechain-main-cli -- save-root \
    --payment-signing-key-file $SIGNING_KEY \
    --genesis-committee-hash-utxo $SCHNORR_SECP256K1_GENESIS_UTXO \
    --sidechain-id 69 \
    --sidechain-genesis-hash 112233 \
    --threshold-numerator 1 \
    --threshold-denominator 2 \
    --atms-kind plain-schnorr-secp256k1 \
    $( for SC_MEMBER in $NEW_SCHNORR_SECP256K1_SC_COMMITTEE
        do printf " --committee-pub-key-and-signature %s:%s " \
                "$(jq -r .rawHexPublicKey $SC_MEMBER)" \
                "$(nix run .#sidechain-main-cli -- utils sign schnorr-secp256k1 \
                    --private-key $(jq -r .rawHexPrivateKey $SC_MEMBER) \
                    --message $(jq -r .cborHexMerkleRootInsertionMessage NextSchnorrSecp256k1MerkleTreeInsertionMessage.json) \
                        | jq -r .rawHexSignature
                    )"
        done
    ) \
    --previous-merkle-root $(jq -r .rawHexMerkleRoot MerkleTree.json) \
    --merkle-root $(jq -r .rawHexMerkleRoot NextMerkleTree.json) \
        | jq
```
