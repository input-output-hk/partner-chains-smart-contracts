# 1 Discussion on the internal testing tool `trustless-sidechain-gen-signatures`
This document outlines using the CLI interface of the CTL project with the internal tool, `trustless-sidechain-gen-signatures`, to assist in generating CLI commands / signatures.

We will discuss:
- [Starting the runtime dependencies](#2-starting-the-runtime-dependencies)
- [High level overview of the workflow](#3-High-level-overview-of-the-workflow)
- [More environment setup](#4-More-environment-setup)
- [Initialising the sidechain](#5-Initialising-the-sidechain)
- [Saving a merkle root](#6-Saving-a-merkle-root)
- [Claiming FUEL tokens](#7-Claiming-FUEL-tokens)
- [Updating the committee hash](#8-Updating-the-committee-hash)
- [Registering a committee candidate](#9-Registering-a-committee-candidate)

# 2 Starting the runtime dependencies
In order to run the system, we require the following runtime dependencies.

- `cardano-node`
- `ogmios` which provides a WebSocket interface to interact with the `cardano-node`
- `ogmios-datum-cache` is used to query datums and confirm transactions[^1]
- `postgres` is the database backing `ogmios-datum-cache`
- `kupo` is used to queries utxos, and other on-chain information

[^1]: The CTL documentation doesn't specify this, but a laborious inspection of the code will reveal this fact. In particular, this was the root cause of the CTL project "hanging forever" waiting for transactions to be confirmed -- see issue [#234](https://github.com/mlabs-haskell/trustless-sidechain/issues/234).

We have provided a convenient way to launch the runtime dependencies in `docker` images for you.
In a separate terminal window in the project's root directory, execute the following command to launch the runtime dependences in `docker` images for the preview test net.
```
$ nix run .#ctl-runtime-preview
```

This will run
- `cardano-node` on the preview test net
- `ogmios` on port 1337
- `kupo` on port 1442

An interested reader can find details in the [CTL project documentation](https://github.com/Plutonomicon/cardano-transaction-lib/blob/87233da45b7c433c243c539cb4d05258e551e9a1/doc/runtime.md).

# 3 High level overview of the workflow
We give a high level overview of the workflow. Consider the following state machine[^2].

[^2]: We ignore some technicalities for ensuring "uniqueness" of the sidechain.

![State machine of workflow](./StateMachineWorkflow.svg)

The notation needs some explanation[^3]. The arrows indicate the transition from one state to another.
The event and preconditions causing the transition is shown above the horizontal line labeling the transition, and actions taken and side effects executed when the event occurs are shown below the line.
We assume that there are 4 global variables stored onchain called `onchain-committee` a representation of the committee, `onchain-previous-merkle-root` a representation of the previous merkle root (if it exists), `onchain-epoch` an integer, and `onchain-merkleroots` a collection storing merkle roots supporting an insertion and membership test operation.
Moreover, we assume that there are some public functions `committee-hash-msg` and `save-root-msg` which are used to help generate signatures.

[^3]: This notation is loosely based off of pp.218 of _Computer Networking: A Top-Down Approach (5th Edition)_ by James F. Kurose and Keith W. Ross.

We discuss the state transitions.

- `register` (not included in the state machine) allows one to register as a committee candidate.

- `init` initialises the sidechain. In particular this determines the `initial committee` stored onchain which has authority over what merkle roots get saved in the `save-root` command along with who the succeeding committee will be in the `committee-hash` command. Moreover, this initialises internal data structures used for the `claim` endpoint that will not be further discussed.

- `committee-hash` allows the `current committee` to sign off a `new committee` to replace them.

- `save-root` allows the `current committee` to sign a merkle root of transactions from the sidechain which may be claimed with the `claim` command.

- `claim` allows an individual who is the recipient of a transaction included in a merkle root from `save-root` to claim their tokens.

In the following sections, we will demonstrate how one may go through this workflow on the preview testnet.

# 4 More environment setup
We will assume that we have our own secret and public key on the preview testnet. We will also assume that we are in a `nix develop` shell in the project root i.e., we executed
```
nix develop
```
in the project root.

We will set the environment variable `PUBLIC_KEY` to our public key e.g., I have
```
PUBLIC_KEY="addr_test1vq9m0ma46xzspaq2jwdefuurt2zm2ct9yj495t22578p6xc7kgt8y"
```
Moreover, we will also assume that we have set the environment variable `SIGNING_KEY` to the path of our secret key e.g., I have the following.
```
SIGNING_KEY=./test.skey
```

Also, we will need to interact with the `cardano-node` in the `docker` image which was launched in [#2](#2-Starting-the-runtime-dependencies). So, to identify the `docker` image, run
```
$ docker ps
CONTAINER ID   IMAGE                                     COMMAND                  CREATED        STATUS                          PORTS                                       NAMES
030c58185ed5   cardanosolutions/kupo:v2.2.0              "/bin/kupo --node-co…"   6 weeks ago    Up About a minute (unhealthy)   0.0.0.0:1442->1442/tcp, :::1442->1442/tcp   store_kupo_1
27895329c110   ogmios:h2rh7q74qffjcs8i6n4bpc7v3pr8fmjy   "/nix/store/p7bpdnxq…"   4 months ago   Up About a minute               0.0.0.0:1337->1337/tcp, :::1337->1337/tcp   store_ogmios_1
34e28749f2c2   inputoutput/cardano-node:1.35.4           "entrypoint run --co…"   6 months ago   Up About a minute               0.0.0.0:3001->3001/tcp, :::3001->3001/tcp   store_cardano-node_1
```
and make note of the docker image `store_cardano_node_1`.

# 5 Initialising the sidechain
We describe initialisation of the sidechain. As an overview, we will:
- Generate an initial committee.
- Find a distinguished UTxO to spend (which is used to ensure that this sidechain is unique).
- Initialise the sidechain.

1. Generating the initial committee using the `trustless-sidechain-gen-signatures`. In the `onchain/` directory, run
```
$ cabal run -v0 trustless-sidechain-gen-signatures -- fresh-sidechain-committee --size 10 > COMMITTEE1
```
which will create a JSON file of a fresh committee called `COMMITTEE1`. This
may take some time if this is the first time running
`trustless-sidechain-gen-signatures` as it may need to compile the project.

2. Finding a distinguished UTxO to spend. We can query the `cardano-node` for UTxOs we may spend with the following command

```
$ docker exec \
    -e CARDANO_NODE_SOCKET_PATH="/ipc/node.socket" store_cardano-node_1 \
    cardano-cli query utxo --testnet-magic 2 --address "$PUBLIC_KEY"
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
211a00e3ac8bebb1545f4d6855c5bbe281357ad8e580d72b1385080bc21445be     0        9908260657 lovelace + TxOutDatumNone
443346868f174378916e5426f8a14aeef31a64c2c8818f644cd65c7fe01242af     3        1344720 lovelace + 420 50fdad256d2fdaf0089915e81fbe0ff4fc4633eee3af8dc9a8111035.4655454c + TxOutDatumNone
5ecd30509c5e06bdf7efdef8f8da7b25b55becc6ca5dfde20ad9a577f50b1756     3        1344720 lovelace + 420 c5ae7e4af4502074687c642f9a6dd97e0ec328ec862d4b40cd87eeac.4655454c + TxOutDatumNone
b39b82f56be9f55af4d3b501ac084c2d2c5be3af8665f7a5bba53c63b0740021     3        1344720 lovelace + 69 c5ae7e4af4502074687c642f9a6dd97e0ec328ec862d4b40cd87eeac.4655454c + TxOutDatumNone
f06b5829a9e02f341e3267fe10bc97a5b9df095713102e4276f957c863213dd1     3        1344720 lovelace + 69 c5ae7e4af4502074687c642f9a6dd97e0ec328ec862d4b40cd87eeac.4655454c + TxOutDatumNone
```

I can choose any of these UTxOs to spend. In particular, I will choose the first one.

Then, for convenience, set the environment variable `GENESIS_UTXO` to the UTxO you choose to spend. For example, I will do
```
GENESIS_UTXO="ea092fb8d592f7533021598178182ba9db2ef5f8c233db802ca1728994df1193#2"
```
where we note that we use the notation `TxHash#TxIx`.

3. Initialising the sidechain. We will use `trustless-sidechain-gen-signatures` to help us generate the CLI command in order to initialise the committee.

Execute the following command
```
$ cabal run -v0 trustless-sidechain-gen-signatures -- init \
  `# Sidechain parameters` \
  --payment-signing-key-file "$SIGNING_KEY" \
  --genesis-committee-hash-utxo "$GENESIS_UTXO" \
  --sidechain-id 69 \
  --threshold 2/3 \
  --sidechain-genesis-hash 112233 \
  --atms-kind "plain" \
  `# Init sidechain parameters` \
  --committee "./COMMITTEE1" \
  --sidechain-epoch 0
Please call sidechain-main-cli with the following arguments:
nix run .#sidechain-main-cli -- init \
--payment-signing-key-file ./payment.skey \
--genesis-committee-hash-utxo ea092fb8d592f7533021598178182ba9db2ef5f8c233db802ca1728994df1193#2 \
--sidechain-id 69 \
--sidechain-genesis-hash 112233 \
--threshold-numerator 2 \
--threshold-denominator 3 \
--atms-kind plain \
--committee-pub-key 026d98b269d0369c65200eab0114d230c08134220d8ee47733c4f1de6832bad87f \
--committee-pub-key 024f37ee7a521295eed5540fec4a303fbbceb35423fe62d7fc8d985806f29bd8eb \
--committee-pub-key 03259582ab42143dc5dba149558cfa1629212086ba581c56d17fb9c79f155c2919 \
--committee-pub-key 020d17ed61b1c27214310d9102aa4b49a0231bf02d6621a5ce3d3f778910752c77 \
--committee-pub-key 02899c2442f77ca98b2e82d44fd78a74a92fa51f4157217a981576f8e01d04a2af \
--committee-pub-key 0267f78812ee29b169c6d2b24792b2e5b7f6bec0a64191a49b02354e11e6a713d8 \
--committee-pub-key 03edad4ed70b04948084447c117cccaceaac520b305cbaef16304577a8acb6696a \
--committee-pub-key 020a585f9fd995517abf97776b55de33be8220fa25f4098c3b0efb1e6da5d35781 \
--committee-pub-key 0354fcd0de4fa92d42de31c6fc5f7cfeafb3ba037c55d2f3bced0c8fb8fb4e5406 \
--committee-pub-key 03a451e61a7aece6f073ab5254f3ce480779481792a591dc069523283f147813cc \
--sidechain-epoch 0
```

It will tell us the command to execute with `sidechain-main-cli` to submit the transaction. Let's do what it says and execute that.
```
$ nix run .#sidechain-main-cli -- init \
--payment-signing-key-file ./payment.skey \
--genesis-committee-hash-utxo ea092fb8d592f7533021598178182ba9db2ef5f8c233db802ca1728994df1193#2 \
--sidechain-id 69 \
--sidechain-genesis-hash 112233 \
--threshold-numerator 2 \
--threshold-denominator 3 \
--atms-kind plain \
--committee-pub-key 026d98b269d0369c65200eab0114d230c08134220d8ee47733c4f1de6832bad87f \
--committee-pub-key 024f37ee7a521295eed5540fec4a303fbbceb35423fe62d7fc8d985806f29bd8eb \
--committee-pub-key 03259582ab42143dc5dba149558cfa1629212086ba581c56d17fb9c79f155c2919 \
--committee-pub-key 020d17ed61b1c27214310d9102aa4b49a0231bf02d6621a5ce3d3f778910752c77 \
--committee-pub-key 02899c2442f77ca98b2e82d44fd78a74a92fa51f4157217a981576f8e01d04a2af \
--committee-pub-key 0267f78812ee29b169c6d2b24792b2e5b7f6bec0a64191a49b02354e11e6a713d8 \
--committee-pub-key 03edad4ed70b04948084447c117cccaceaac520b305cbaef16304577a8acb6696a \
--committee-pub-key 020a585f9fd995517abf97776b55de33be8220fa25f4098c3b0efb1e6da5d35781 \
--committee-pub-key 0354fcd0de4fa92d42de31c6fc5f7cfeafb3ba037c55d2f3bced0c8fb8fb4e5406 \
--committee-pub-key 03a451e61a7aece6f073ab5254f3ce480779481792a591dc069523283f147813cc \
--sidechain-epoch 0
[INFO] 2023-06-27T07:57:02.116Z InitSidechain.initSidechain: Submitted initialise sidechain tokens Tx: (TransactionHash (hexToByteArrayUnsafe "e9f9774bb8b8b3529dacd685ccd2ecb50607d7ad7ffaf5a21c34627e0e582264"))
[INFO] 2023-06-27T07:57:31.431Z InitSidechain.initSidechain: Initialise sidechain tokens transaction submitted successfully.
{
  "endpoint": "Init",
  "transactionId": "e9f9774bb8b8b3529dacd685ccd2ecb50607d7ad7ffaf5a21c34627e0e582264",
  "sidechainParams": {
    "chainId": 69,
    "genesisHash": "112233",
    "genesisUtxo": "ea092fb8d592f7533021598178182ba9db2ef5f8c233db802ca1728994df1193#2",
    "thresholdDenominator": 3,
    "thresholdNumerator": 2
  },
  "addresses": {
    "CommitteeCandidateValidator": "addr_test1wph8v2tqv8ltuwaj39m5uft37q4yyyqmpr0gd90kgw8m70s9z385d",
    "MerkleRootTokenValidator": "addr_test1wzv4p2g59xm29xlq94kaqzgs55qxrse6kxu8elx0pzd77kcwczejj",
    "CommitteeHashValidator": "addr_test1wphtswy47yxk8f8r6a4n77dfyf4c86uqcxf6lcyfw4efe7ga42d4q",
    "DSConfValidator": "addr_test1wz5vkh48ys3tuqpxp4uawuf56yja5p8rgqhaet25gjzxu0s58gnlk",
    "DSInsertValidator": "addr_test1wpvhg3a7cjmy7z0te0y9gp6feyln9ywe3eugexvt2zx4lxcdv09xu"
  },
  "cborEncodedAddresses": {
    "CommitteeHashValidator": "d8799fd87a9f581c6eb83895f10d63a4e3d76b3f79a9226b83eb80c193afe08975729cf9ffd87a80ff"
  },
  "mintingPolicies": {
    "FuelMintingPolicyId": "78ac5f25a31100e4e27571b31958adf2c31859ea716e88f760e935c5",
    "MerkleRootTokenMintingPolicyId": "62c987501cd2a7eaf1b3cade54249de4aac65ab617605fdb6566a0b9",
    "CommitteeNftPolicyId": "c8ad80e77df4c4fbc8746ee023fe84371ac973fd80bf5fc0549f59e2",
    "DSKeyPolicy": "2f8cfc021538cf8c4070402d22522db047c0df73deecd3cea03462ae",
    "DSConfPolicy": "b4c45326619a888bbff0c8d88c04bb301a1ac1bdcf7b38b0f41cdc45"
  }
}

```
Hopefully, it will succeed -- meaning that we have initialised the sidechain. Note that it also outputs various addresses / minting policies related to the sidechain in JSON format. We can ignore this for now.

# 6 Saving a merkle root
We will demonstrate how to save a merkle root with the sidechain we have just initialised. As an overview, we will
- Create a merkle tree / merkle root of some transactions
- Save the merkle root.

1. Create a merkle tree / merkle root of some transactions. We will use `trustless-sidechain-gen-signatures` for this. Run the following commands to create a merkle tree.
```
$ cabal run -v0 trustless-sidechain-gen-signatures -- merkle-tree \
    --merkle-tree-entry="{\"index\":1 , \"amount\": 69,  \"recipient\":\"$PUBLIC_KEY\", \"previousMerkleRoot\": null }" \
    --merkle-tree-entry="{\"index\":2 , \"amount\": 420, \"recipient\":\"$PUBLIC_KEY\", \"previousMerkleRoot\": null }"
$ MERKLE_TREE_1_1=$(!!)
$ MERKLE_ROOT_1_1=$(cabal run -v0 trustless-sidechain-gen-signatures -- root-hash --merkle-tree $MERKLE_TREE_1_1)
```
Note we use `!!` to run the last command and store it to an environment variable. This merkle tree includes transactions to

- Pay 69 FUEL to myself, and
- Pay 420 FUEL to myself

We can also save another merkle root as follows.

```
$ cabal run -v0 trustless-sidechain-gen-signatures -- merkle-tree \
    --merkle-tree-entry="{\"index\":1 , \"amount\": 169,  \"recipient\":\"$PUBLIC_KEY\", \"previousMerkleRoot\": \"$MERKLE_ROOT_1_1\" }" \
    --merkle-tree-entry="{\"index\":2 , \"amount\": 420, \"recipient\":\"$PUBLIC_KEY\", \"previousMerkleRoot\": \"$MERKLE_ROOT_1_1\" }"
$ MERKLE_TREE_1_2=$(!!)
$ MERKLE_ROOT_1_2=$(cabal run -v0 trustless-sidechain-gen-signatures -- root-hash --merkle-tree $MERKLE_TREE_1_2)
```
which has transactions to

- Pay 169 FUEL to myself, and
- Pay 1420 FUEL to myself

Note that the `previousMerkleRoot` field was changed from `null` to `MERKLE_ROOT_1_1` -- this is for technical reasons.

2. Save a merkle root. We will use `trustless-sidechain-gen-signatures` to create the command to save the merkle root. Run the following command.
```
$ cabal run -v0 trustless-sidechain-gen-signatures -- save-root \
  `# Sidechain parameters` \
  --payment-signing-key-file "$SIGNING_KEY" \
  --genesis-committee-hash-utxo "$GENESIS_UTXO" \
  --sidechain-id 69 \
  --threshold 2/3 \
  --sidechain-genesis-hash 112233 \
  --atms-kind plain \
  `# Current committee private keys (used for signing)` \
  --current-committee "./COMMITTEE1" \
  `# Parameters for the save-root` \
  --merkle-root $MERKLE_ROOT_1_1
  #  --previous-merkle-root
Please call sidechain-main-cli with the following arguments:
nix run .#sidechain-main-cli -- save-root \
--payment-signing-key-file ./payment.skey \
--genesis-committee-hash-utxo ea092fb8d592f7533021598178182ba9db2ef5f8c233db802ca1728994df1193#2 \
--sidechain-id 69 \
--sidechain-genesis-hash 112233 \
--threshold-numerator 2 \
--threshold-denominator 3 \
--atms-kind plain \
--committee-pub-key-and-signature 026d98b269d0369c65200eab0114d230c08134220d8ee47733c4f1de6832bad87f:efc54805397e4fa110df3650ce192a43b20f2086366ee6ee320645003f322ec870c03b2893bb8e06641e63bbc8dc1e6e13aee225edde10004753db1d468d6bba \
--committee-pub-key-and-signature 024f37ee7a521295eed5540fec4a303fbbceb35423fe62d7fc8d985806f29bd8eb:55b119469215ce5b537c5bee0e69fd6acaf89f8f0dae3088228d0d2950f0955376a60aa1442b8eb03cdfbfd61cce8fdc7da52d9977022053a16d181e8d4aa752 \
--committee-pub-key-and-signature 03259582ab42143dc5dba149558cfa1629212086ba581c56d17fb9c79f155c2919:88bbba3f2c0640f6ad657dba0747f64b7e512ae047b33d0ca3d9299fd5001db338585b842b60a9d231a708e1a9943a64622973fd86208e3daa85270e62d563e6 \
--committee-pub-key-and-signature 020d17ed61b1c27214310d9102aa4b49a0231bf02d6621a5ce3d3f778910752c77:ac7d1ebc4056d8f1b4cb4ec2b7a7f6e7fe538e3909a59dbdfd9e3ce767b355c469bc69fe606634273d00db67eb71b1ce2c8f42cbc78f7c700848a01d8fc07637 \
--committee-pub-key-and-signature 02899c2442f77ca98b2e82d44fd78a74a92fa51f4157217a981576f8e01d04a2af:8b55ed3b8ad73eea92e042b00dc18d80b7e43d77f8e7c20054ada6295b9747c340a49e5066ac457278391e447bc203dd29434bb99d9b956704e4da9844e82a9e \
--committee-pub-key-and-signature 0267f78812ee29b169c6d2b24792b2e5b7f6bec0a64191a49b02354e11e6a713d8:4606c688dcd9c71653decbe9fdf86733a4465733531b563ce8a22c4613b7f8b83beb4550cbad60c685e63e3d783d5a2c82b47d709b181a4322b2b3c73b17b47b \
--committee-pub-key-and-signature 03edad4ed70b04948084447c117cccaceaac520b305cbaef16304577a8acb6696a:c95516d4b58b6b4592df8493405cb56f8a291aab508595505697a751651fc15e004c0139287d1c4b1f146d353dfe65baf815be4c12d021461cf0cee0aa2e4029 \
--committee-pub-key-and-signature 020a585f9fd995517abf97776b55de33be8220fa25f4098c3b0efb1e6da5d35781:0f62bc1824072791d614ff2630a796337de8d5a124192292ee55a4e6d1180e2d20cdb4830f459bbeda973b9ad4f1642a9c4191ef0154fe13e003e6b9a7ed05e6 \
--committee-pub-key-and-signature 0354fcd0de4fa92d42de31c6fc5f7cfeafb3ba037c55d2f3bced0c8fb8fb4e5406:3bde22e994178cb2a6218d309a40dcfccec6b334671758ea6e601f3344333d1d734eb85994f7b21e5f83c3b57f51e491f3329386f6a0440dbfb838529be0da0c \
--committee-pub-key-and-signature 03a451e61a7aece6f073ab5254f3ce480779481792a591dc069523283f147813cc:d46bd145032e031b5d0caf3974623cd9b96246c54efaae70aa8530cd5d3caec26a1f6ce33fd04c3b24326acc5070b9aec42bd9575280318da6d38dc93c8ad659 \
--merkle-root eed04d5f8c29240d92325f887a9d53883dfea50d364ae1633c651533b87a896f
```
It'll tell us how to call `sidechain-main-cli`. Let's do as it says
```
$ nix run .#sidechain-main-cli -- save-root \
--payment-signing-key-file ./payment.skey \
--genesis-committee-hash-utxo ea092fb8d592f7533021598178182ba9db2ef5f8c233db802ca1728994df1193#2 \
--sidechain-id 69 \
--sidechain-genesis-hash 112233 \
--threshold-numerator 2 \
--threshold-denominator 3 \
--atms-kind plain \
--committee-pub-key-and-signature 026d98b269d0369c65200eab0114d230c08134220d8ee47733c4f1de6832bad87f:efc54805397e4fa110df3650ce192a43b20f2086366ee6ee320645003f322ec870c03b2893bb8e06641e63bbc8dc1e6e13aee225edde10004753db1d468d6bba \
--committee-pub-key-and-signature 024f37ee7a521295eed5540fec4a303fbbceb35423fe62d7fc8d985806f29bd8eb:55b119469215ce5b537c5bee0e69fd6acaf89f8f0dae3088228d0d2950f0955376a60aa1442b8eb03cdfbfd61cce8fdc7da52d9977022053a16d181e8d4aa752 \
--committee-pub-key-and-signature 03259582ab42143dc5dba149558cfa1629212086ba581c56d17fb9c79f155c2919:88bbba3f2c0640f6ad657dba0747f64b7e512ae047b33d0ca3d9299fd5001db338585b842b60a9d231a708e1a9943a64622973fd86208e3daa85270e62d563e6 \
--committee-pub-key-and-signature 020d17ed61b1c27214310d9102aa4b49a0231bf02d6621a5ce3d3f778910752c77:ac7d1ebc4056d8f1b4cb4ec2b7a7f6e7fe538e3909a59dbdfd9e3ce767b355c469bc69fe606634273d00db67eb71b1ce2c8f42cbc78f7c700848a01d8fc07637 \
--committee-pub-key-and-signature 02899c2442f77ca98b2e82d44fd78a74a92fa51f4157217a981576f8e01d04a2af:8b55ed3b8ad73eea92e042b00dc18d80b7e43d77f8e7c20054ada6295b9747c340a49e5066ac457278391e447bc203dd29434bb99d9b956704e4da9844e82a9e \
--committee-pub-key-and-signature 0267f78812ee29b169c6d2b24792b2e5b7f6bec0a64191a49b02354e11e6a713d8:4606c688dcd9c71653decbe9fdf86733a4465733531b563ce8a22c4613b7f8b83beb4550cbad60c685e63e3d783d5a2c82b47d709b181a4322b2b3c73b17b47b \
--committee-pub-key-and-signature 03edad4ed70b04948084447c117cccaceaac520b305cbaef16304577a8acb6696a:c95516d4b58b6b4592df8493405cb56f8a291aab508595505697a751651fc15e004c0139287d1c4b1f146d353dfe65baf815be4c12d021461cf0cee0aa2e4029 \
--committee-pub-key-and-signature 020a585f9fd995517abf97776b55de33be8220fa25f4098c3b0efb1e6da5d35781:0f62bc1824072791d614ff2630a796337de8d5a124192292ee55a4e6d1180e2d20cdb4830f459bbeda973b9ad4f1642a9c4191ef0154fe13e003e6b9a7ed05e6 \
--committee-pub-key-and-signature 0354fcd0de4fa92d42de31c6fc5f7cfeafb3ba037c55d2f3bced0c8fb8fb4e5406:3bde22e994178cb2a6218d309a40dcfccec6b334671758ea6e601f3344333d1d734eb85994f7b21e5f83c3b57f51e491f3329386f6a0440dbfb838529be0da0c \
--committee-pub-key-and-signature 03a451e61a7aece6f073ab5254f3ce480779481792a591dc069523283f147813cc:d46bd145032e031b5d0caf3974623cd9b96246c54efaae70aa8530cd5d3caec26a1f6ce33fd04c3b24326acc5070b9aec42bd9575280318da6d38dc93c8ad659 \
--merkle-root eed04d5f8c29240d92325f887a9d53883dfea50d364ae1633c651533b87a896f
[INFO] 2023-06-27T08:02:58.212Z MerkleRoot.saveRoot: Submitted save root Tx: (TransactionHash (hexToByteArrayUnsafe "7750ea6bc21f33a6d483fc5231ec2bfa0053ec9a9d477bd508439f0fef316cbc"))
[INFO] 2023-06-27T08:03:36.688Z MerkleRoot.saveRoot: Save root Tx submitted successfully!
{"endpoint":"SaveRoot","transactionId":"7750ea6bc21f33a6d483fc5231ec2bfa0053ec9a9d477bd508439f0fef316cbc"}
```

Now, let's save the other merkle root.
```
$ cabal run -v0 trustless-sidechain-gen-signatures -- save-root \
  `# Sidechain parameters` \
  --payment-signing-key-file "$SIGNING_KEY" \
  --genesis-committee-hash-utxo "$GENESIS_UTXO" \
  --sidechain-id 69 \
  --threshold 2/3 \
  --sidechain-genesis-hash 112233 \
  --atms-kind plain \
  `# Current committee private keys (used for signing)` \
  --current-committee "./COMMITTEE1" \
  `# Parameters for the save-root` \
  --merkle-root $MERKLE_ROOT_1_2 \
  --previous-merkle-root $MERKLE_ROOT_1_1
Please call sidechain-main-cli with the following arguments:
nix run .#sidechain-main-cli -- save-root \
--payment-signing-key-file ./payment.skey \
--genesis-committee-hash-utxo ea092fb8d592f7533021598178182ba9db2ef5f8c233db802ca1728994df1193#2 \
--sidechain-id 69 \
--sidechain-genesis-hash 112233 \
--threshold-numerator 2 \
--threshold-denominator 3 \
--atms-kind plain \
--committee-pub-key-and-signature 026d98b269d0369c65200eab0114d230c08134220d8ee47733c4f1de6832bad87f:3cbbdadc26251839d54ab37919c7845042525802e542352b69ec3209c77b9d0d5605ed521bf90d5f6ae5a5842c043cd0a3a6bb3ddda0515372cf36be98327203 \
--committee-pub-key-and-signature 024f37ee7a521295eed5540fec4a303fbbceb35423fe62d7fc8d985806f29bd8eb:21be28d4788b64e125c05e133fbb9378124d6908717b919f11b4210d3550d7620a023173975943fe6c7125a69b91cc91147e78fc232ae22c7439e3795cf12558 \
--committee-pub-key-and-signature 03259582ab42143dc5dba149558cfa1629212086ba581c56d17fb9c79f155c2919:ebd3af866dedeb77acbb92459b9d3b90d3c21b3fe8f20247a011b2f5d856afde4cb2ff5914959cbb0c3180d551fdcf7310782ef886fb5754f32812128aee693f \
--committee-pub-key-and-signature 020d17ed61b1c27214310d9102aa4b49a0231bf02d6621a5ce3d3f778910752c77:29cfd762f02047f7093586302ea163f2a381bdf7552f131bc86abce3b6c067bb5c1f855d33c44dc87a3d08a565aa84c4faaaa2256020ebb9b6ab25a53abf4374 \
--committee-pub-key-and-signature 02899c2442f77ca98b2e82d44fd78a74a92fa51f4157217a981576f8e01d04a2af:6428becf2a976dd2b49ad5d2085dc79fafb6df309cfba4037a979b37bd603fcc62effaae428632f6fbae62e3daebc8f44f904061b76517cd629939fb9dc22349 \
--committee-pub-key-and-signature 0267f78812ee29b169c6d2b24792b2e5b7f6bec0a64191a49b02354e11e6a713d8:c351148e711dfa874ec445d86858874139c763064e19fc69862f4451e742ab6a49d684bbba31c3b54a97eab02bf354aa324ffeb80395fb0ad2046208b75fe614 \
--committee-pub-key-and-signature 03edad4ed70b04948084447c117cccaceaac520b305cbaef16304577a8acb6696a:fcadc165a15a3780c61bc168c76d122c4bb544f9e6b42c94a45b89852cbed54f23af9089a891289b3b5a5c80a924e4f46b075e86252f07f2679cdfacd5ce7ff3 \
--committee-pub-key-and-signature 020a585f9fd995517abf97776b55de33be8220fa25f4098c3b0efb1e6da5d35781:7f0351ea9d95a829ce8b2664af9b3232d55cecc81294605ff657b91eb42e7ed26fdaa0b06cc551cd7701dd6b28d49b65586476bc40e80a62397ba038198e1023 \
--committee-pub-key-and-signature 0354fcd0de4fa92d42de31c6fc5f7cfeafb3ba037c55d2f3bced0c8fb8fb4e5406:d010f1eee25344061655fc30720f313c39091d6b1d4578653e383a089cdf9eca24047d00f66c224d112dd1d597d2a2cd54219e4cba47e631186c1b6116e0a71c \
--committee-pub-key-and-signature 03a451e61a7aece6f073ab5254f3ce480779481792a591dc069523283f147813cc:7ec9083de2dd1df6074008db669cad7980fbb8f079f82af696ac11b1bcf94ec53bf5685a4878fa6a2a9b378793506f2911335f7763d0786ebb4e3a8bcaea0f10 \
--merkle-root fa43e2b2d66e4c4db3be723eb5a4e1ba718aca4a375139600b6f53de258e2bb3 \
--previous-merkle-root eed04d5f8c29240d92325f887a9d53883dfea50d364ae1633c651533b87a896f
```
Note that we include the flag `--previous-merkle-root` with `$MERKLE_ROOT_1_1`.

Then, let's do what it says and call `sidechain-main-cli` as given.
```
$ nix run .#sidechain-main-cli -- save-root \
--payment-signing-key-file ./payment.skey \
--genesis-committee-hash-utxo ea092fb8d592f7533021598178182ba9db2ef5f8c233db802ca1728994df1193#2 \
--sidechain-id 69 \
--sidechain-genesis-hash 112233 \
--threshold-numerator 2 \
--threshold-denominator 3 \
--atms-kind plain \
--committee-pub-key-and-signature 026d98b269d0369c65200eab0114d230c08134220d8ee47733c4f1de6832bad87f:3cbbdadc26251839d54ab37919c7845042525802e542352b69ec3209c77b9d0d5605ed521bf90d5f6ae5a5842c043cd0a3a6bb3ddda0515372cf36be98327203 \
--committee-pub-key-and-signature 024f37ee7a521295eed5540fec4a303fbbceb35423fe62d7fc8d985806f29bd8eb:21be28d4788b64e125c05e133fbb9378124d6908717b919f11b4210d3550d7620a023173975943fe6c7125a69b91cc91147e78fc232ae22c7439e3795cf12558 \
--committee-pub-key-and-signature 03259582ab42143dc5dba149558cfa1629212086ba581c56d17fb9c79f155c2919:ebd3af866dedeb77acbb92459b9d3b90d3c21b3fe8f20247a011b2f5d856afde4cb2ff5914959cbb0c3180d551fdcf7310782ef886fb5754f32812128aee693f \
--committee-pub-key-and-signature 020d17ed61b1c27214310d9102aa4b49a0231bf02d6621a5ce3d3f778910752c77:29cfd762f02047f7093586302ea163f2a381bdf7552f131bc86abce3b6c067bb5c1f855d33c44dc87a3d08a565aa84c4faaaa2256020ebb9b6ab25a53abf4374 \
--committee-pub-key-and-signature 02899c2442f77ca98b2e82d44fd78a74a92fa51f4157217a981576f8e01d04a2af:6428becf2a976dd2b49ad5d2085dc79fafb6df309cfba4037a979b37bd603fcc62effaae428632f6fbae62e3daebc8f44f904061b76517cd629939fb9dc22349 \
--committee-pub-key-and-signature 0267f78812ee29b169c6d2b24792b2e5b7f6bec0a64191a49b02354e11e6a713d8:c351148e711dfa874ec445d86858874139c763064e19fc69862f4451e742ab6a49d684bbba31c3b54a97eab02bf354aa324ffeb80395fb0ad2046208b75fe614 \
--committee-pub-key-and-signature 03edad4ed70b04948084447c117cccaceaac520b305cbaef16304577a8acb6696a:fcadc165a15a3780c61bc168c76d122c4bb544f9e6b42c94a45b89852cbed54f23af9089a891289b3b5a5c80a924e4f46b075e86252f07f2679cdfacd5ce7ff3 \
--committee-pub-key-and-signature 020a585f9fd995517abf97776b55de33be8220fa25f4098c3b0efb1e6da5d35781:7f0351ea9d95a829ce8b2664af9b3232d55cecc81294605ff657b91eb42e7ed26fdaa0b06cc551cd7701dd6b28d49b65586476bc40e80a62397ba038198e1023 \
--committee-pub-key-and-signature 0354fcd0de4fa92d42de31c6fc5f7cfeafb3ba037c55d2f3bced0c8fb8fb4e5406:d010f1eee25344061655fc30720f313c39091d6b1d4578653e383a089cdf9eca24047d00f66c224d112dd1d597d2a2cd54219e4cba47e631186c1b6116e0a71c \
--committee-pub-key-and-signature 03a451e61a7aece6f073ab5254f3ce480779481792a591dc069523283f147813cc:7ec9083de2dd1df6074008db669cad7980fbb8f079f82af696ac11b1bcf94ec53bf5685a4878fa6a2a9b378793506f2911335f7763d0786ebb4e3a8bcaea0f10 \
--merkle-root fa43e2b2d66e4c4db3be723eb5a4e1ba718aca4a375139600b6f53de258e2bb3 \
--previous-merkle-root eed04d5f8c29240d92325f887a9d53883dfea50d364ae1633c651533b87a896f
[INFO] 2023-06-27T08:05:53.827Z MerkleRoot.saveRoot: Submitted save root Tx: (TransactionHash (hexToByteArrayUnsafe "d087d4bddc162d36d6ded59f5a8bea3d04680582ad33f0d9b1480af73d8c40b4"))
[INFO] 2023-06-27T08:06:21.363Z MerkleRoot.saveRoot: Save root Tx submitted successfully!
{"endpoint":"SaveRoot","transactionId":"d087d4bddc162d36d6ded59f5a8bea3d04680582ad33f0d9b1480af73d8c40b4"}
```

# 7 Claiming FUEL tokens
We will demonstrate how to claim FUEL tokens. As an overview, here's what we will discuss

- Finding out what the Currency Symbol of FUEL tokens are.
- Claiming FUEL tokens.

1. Finding out what the Currency Symbol of FUEL tokens are. This will help us verify that we have actually received FUEL tokens in our wallet. Conveniently, `sidechain-main-cli` provides a command to gather all addresses related to the sidechain for us as follows.
```
$ nix run .#sidechain-main-cli -- addresses \
--payment-signing-key-file $SIGNING_KEY \
--genesis-committee-hash-utxo $GENESIS_UTXO \
--sidechain-id 69 \
--sidechain-genesis-hash 112233 \
--threshold-numerator 2 \
--threshold-denominator 3 \
--atms-kind plain | jq
{
  "endpoint": "GetAddrs",
  "addresses": {
    "CommitteeCandidateValidator": "addr_test1wph8v2tqv8ltuwaj39m5uft37q4yyyqmpr0gd90kgw8m70s9z385d",
    "MerkleRootTokenValidator": "addr_test1wzv4p2g59xm29xlq94kaqzgs55qxrse6kxu8elx0pzd77kcwczejj",
    "CommitteeHashValidator": "addr_test1wphtswy47yxk8f8r6a4n77dfyf4c86uqcxf6lcyfw4efe7ga42d4q",
    "DSConfValidator": "addr_test1wz5vkh48ys3tuqpxp4uawuf56yja5p8rgqhaet25gjzxu0s58gnlk",
    "DSInsertValidator": "addr_test1wpvhg3a7cjmy7z0te0y9gp6feyln9ywe3eugexvt2zx4lxcdv09xu"
  },
  "cborEncodedAddresses": {
    "CommitteeHashValidator": "d8799fd87a9f581c6eb83895f10d63a4e3d76b3f79a9226b83eb80c193afe08975729cf9ffd87a80ff"
  },
  "mintingPolicies": {
    "FuelMintingPolicyId": "78ac5f25a31100e4e27571b31958adf2c31859ea716e88f760e935c5",
    "MerkleRootTokenMintingPolicyId": "62c987501cd2a7eaf1b3cade54249de4aac65ab617605fdb6566a0b9",
    "CommitteeNftPolicyId": "c8ad80e77df4c4fbc8746ee023fe84371ac973fd80bf5fc0549f59e2",
    "DSKeyPolicy": "2f8cfc021538cf8c4070402d22522db047c0df73deecd3cea03462ae",
    "DSConfPolicy": "b4c45326619a888bbff0c8d88c04bb301a1ac1bdcf7b38b0f41cdc45"
  }
}
```
Note that we pipe the output to `jq` for easier JSON file viewing, but this is optional. In particular, we are interested in
```
    "FuelMintingPolicyId": "78ac5f25a31100e4e27571b31958adf2c31859ea716e88f760e935c5",
```
which identifies the FUEL minting policy.

2. Claiming FUEL tokens. We will try to claim
```
    --merkle-tree-entry="{\"index\":1 , \"amount\": 69,  \"recipient\":\"$PUBLIC_KEY\", \"previousMerkleRoot\": null }" \
```
from `MERKLE_TREE_1_1`. We first need to generate the `CombinedMerkleProof` -- we do this with `trustless-sidechain-gen-signatures` as follows.
```
$ cabal run -v0 trustless-sidechain-gen-signatures -- combined-merkle-proof \
    --merkle-tree $MERKLE_TREE_1_1 \
    --merkle-tree-entry="{\"index\":1 , \"amount\": 69,  \"recipient\":\"$PUBLIC_KEY\", \"previousMerkleRoot\": null }"
$ COMBINED_MERKLE_PROOF_1_1_1=$(!!)
```

Then, we can claim our FUEL as follows. But before doing this, let's peek at our wallet now:
```
$ docker exec \
    -e CARDANO_NODE_SOCKET_PATH="/ipc/node.socket" store_cardano-node_1 \
    cardano-cli query utxo --testnet-magic 2 --address "$PUBLIC_KEY"
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5ecd30509c5e06bdf7efdef8f8da7b25b55becc6ca5dfde20ad9a577f50b1756     3        1344720 lovelace + 420 c5ae7e4af4502074687c642f9a6dd97e0ec328ec862d4b40cd87eeac.4655454c + TxOutDatumNone
66d92213b22174c5fedbb99c5140ac9c836f5b887189b0bd6367483c053607ee     0        9900195759 lovelace + 420 50fdad256d2fdaf0089915e81fbe0ff4fc4633eee3af8dc9a8111035.4655454c + TxOutDatumNone
b39b82f56be9f55af4d3b501ac084c2d2c5be3af8665f7a5bba53c63b0740021     3        1344720 lovelace + 69 c5ae7e4af4502074687c642f9a6dd97e0ec328ec862d4b40cd87eeac.4655454c + TxOutDatumNone
f06b5829a9e02f341e3267fe10bc97a5b9df095713102e4276f957c863213dd1     3        1344720 lovelace + 69 c5ae7e4af4502074687c642f9a6dd97e0ec328ec862d4b40cd87eeac.4655454c + TxOutDatumNone
```
and it's easy to see that we have no FUEL tokens.

Now, we claim our FUEL as follows.
```
$ nix run .#sidechain-main-cli -- claim \
  `# Sidechain parameters` \
  --payment-signing-key-file "$SIGNING_KEY" \
  --genesis-committee-hash-utxo "$GENESIS_UTXO" \
  --sidechain-id 69 \
  --threshold-numerator 2 \
  --threshold-denominator 3 \
  --sidechain-genesis-hash 112233 \
  --atms-kind plain \
  `# claim parameters` \
  --combined-proof $COMBINED_MERKLE_PROOF_1_1_1
[WARN] 2023-06-27T08:12:12.605Z Finding the required distributed set node (this may take a while)...
[INFO] 2023-06-27T08:12:17.484Z FUELMintingPolicy.runFuelMP: Submitted Tx: (TransactionHash (hexToByteArrayUnsafe "defedc3e3e520e2e6b15e76a7df2b7e396cfffcb0ee5561103013e31db929c81"))
[INFO] 2023-06-27T08:12:27.660Z FUELMintingPolicy.runFuelMP: Tx submitted successfully!
{"endpoint":"ClaimAct","transactionId":"defedc3e3e520e2e6b15e76a7df2b7e396cfffcb0ee5561103013e31db929c81"}
```

Then, we can verify that we have received FUEL by checking our wallet as follows.
```
$ docker exec \
    -e CARDANO_NODE_SOCKET_PATH="/ipc/node.socket" store_cardano-node_1 \
    cardano-cli query utxo --testnet-magic 2 --address "$PUBLIC_KEY"
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0989c38f40b6f490fbcb5bdedc0e8c40192b81ae82b3be6f45d05c42e93a3bfa     0        9897669929 lovelace + 420 50fdad256d2fdaf0089915e81fbe0ff4fc4633eee3af8dc9a8111035.4655454c + 420 c5ae7e4af4502074687c642f9a6dd97e0ec328ec862d4b40cd87eeac.4655454c + TxOutDatumNone
0989c38f40b6f490fbcb5bdedc0e8c40192b81ae82b3be6f45d05c42e93a3bfa     3        1344720 lovelace + 69 78ac5f25a31100e4e27571b31958adf2c31859ea716e88f760e935c5.4655454c + TxOutDatumNone
b39b82f56be9f55af4d3b501ac084c2d2c5be3af8665f7a5bba53c63b0740021     3        1344720 lovelace + 69 c5ae7e4af4502074687c642f9a6dd97e0ec328ec862d4b40cd87eeac.4655454c + TxOutDatumNone
f06b5829a9e02f341e3267fe10bc97a5b9df095713102e4276f957c863213dd1     3        1344720 lovelace + 69 c5ae7e4af4502074687c642f9a6dd97e0ec328ec862d4b40cd87eeac.4655454c + TxOutDatumNone
```
Where we observe that the 2nd entry in the table is indeed our FUEL.

# 8 Updating the committee hash
We will demonstrate how to update the committee hash. As an overview, we will discuss
- Generating a new committee
- Updating the current committee

Recall that the current committee (as stored on chain) is in the file `./COMMITTEE1`.

1. Generating a new committee. This is the same procedure as given in [#5](#5-Initialising-the-sidechain). Recall the procedure was as follows.
```
$ cabal run -v0 trustless-sidechain-gen-signatures -- fresh-sidechain-committee --size 10 > COMMITTEE2
```
where we note that we save this new committee to the file `COMMITTEE2`.

2. Updating the committee hash. We will use `trustless-sidechain-gen-signatures` to generate the `sidechain-main-cli` command.
Recall from the `addresses` CLI command, we had
```
  "cborEncodedAddresses": {
    "CommitteeHashValidator": "d8799fd87a9f581c6eb83895f10d63a4e3d76b3f79a9226b83eb80c193afe08975729cf9ffd87a80ff"
  },
```
Then, run the following command where we note that the
`--new-committee-validator-cbor-encoded-address` flag take the aforementioned value.
```
$ cabal run -v0 trustless-sidechain-gen-signatures -- committee-hash \
  `# Sidechain parameters` \
  --payment-signing-key-file "$SIGNING_KEY" \
  --genesis-committee-hash-utxo "$GENESIS_UTXO" \
  --sidechain-id 69 \
  --threshold 2/3 \
  --sidechain-genesis-hash 112233 \
  --atms-kind plain \
  `# committee-hash sidechain parameters` \
  --current-committee "./COMMITTEE1" \
  --new-committee "./COMMITTEE2" \
  --sidechain-epoch 1 \
  --previous-merkle-root "$MERKLE_ROOT_1_2" \
  --new-committee-validator-cbor-encoded-address "d8799fd87a9f581c6eb83895f10d63a4e3d76b3f79a9226b83eb80c193afe08975729cf9ffd87a80ff"
Please call sidechain-main-cli with the following arguments:
nix run .#sidechain-main-cli -- committee-hash \
--payment-signing-key-file ./payment.skey \
--genesis-committee-hash-utxo ea092fb8d592f7533021598178182ba9db2ef5f8c233db802ca1728994df1193#2 \
--sidechain-id 69 \
--sidechain-genesis-hash 112233 \
--threshold-numerator 2 \
--threshold-denominator 3 \
--atms-kind plain \
--committee-pub-key-and-signature 026d98b269d0369c65200eab0114d230c08134220d8ee47733c4f1de6832bad87f:ebec7a67229739f93b4b14a1317911e983da20270d754b43d7786bb8fac5377076c48553a0578bb7dfcb27c5926d238ed745ed89d87bca51a3acf65cc6e1cabf \
--committee-pub-key-and-signature 024f37ee7a521295eed5540fec4a303fbbceb35423fe62d7fc8d985806f29bd8eb:c1d81579478bf06291a9738fb8a09f1bbcc5dd67fbab8d4ecb6efc75d378b5a63b043bbad5494c43243ee30ddb8cb8897f6d679c833b2446eebe29f57990a541 \
--committee-pub-key-and-signature 03259582ab42143dc5dba149558cfa1629212086ba581c56d17fb9c79f155c2919:71a41734a8a04b24826cdea9fa2b5a7b1b59c1fa555eb830f7c16168e03c8f7829839a69427da64d2c99257c76257e8e882353f7ebd9ab047086b24524f7314c \
--committee-pub-key-and-signature 020d17ed61b1c27214310d9102aa4b49a0231bf02d6621a5ce3d3f778910752c77:c118e6cd0108dcc8124601c05a88520d379ebf7bc93421c29df7230a890b5a7378d5b8a8f7db37bd36fe804d0b3f92d8e4b5d8e26952427d9ad410903c3efbc2 \
--committee-pub-key-and-signature 02899c2442f77ca98b2e82d44fd78a74a92fa51f4157217a981576f8e01d04a2af:cb0be0866e23889e23aba1a0f3bb338e13fa44e5fa5164892d8e7346b7a187da2b1feb57e79b8c2edb73bfd74342a179d908b19e49b048d5b32980f43f8e8882 \
--committee-pub-key-and-signature 0267f78812ee29b169c6d2b24792b2e5b7f6bec0a64191a49b02354e11e6a713d8:b130e6b014902a3474f32305e3be72877ae57c610a65562937878b539ea350eb5f9ac0f007d6901790ca72ef135cceac5184e67fa58d8ba12988b9ad0a51b9cb \
--committee-pub-key-and-signature 03edad4ed70b04948084447c117cccaceaac520b305cbaef16304577a8acb6696a:ccaf2269e59997e302b9c37027438b7caf7b5c01fda074797322e80cee0cbc762580aa6a88a50455e12411b1f1b9dafe465aec65b7d9eadaf8ac62a98899f6e8 \
--committee-pub-key-and-signature 020a585f9fd995517abf97776b55de33be8220fa25f4098c3b0efb1e6da5d35781:4e8da03c017e5b4ffb2672442504943bf231053e58f054820086fec8b0a6793c2431a9ae7f86e17f1e959fb76054991f8ab3a67ce283be9f206396d722e9ffc9 \
--committee-pub-key-and-signature 0354fcd0de4fa92d42de31c6fc5f7cfeafb3ba037c55d2f3bced0c8fb8fb4e5406:8aac03014bf28b544a1d5763a9bc8746a3875488fa7709fb174a8dff05891a2e0d9cb3c1ef52bcdbf1c4165e862685c33d295e8abc9c04486e5bdc2de0a440e6 \
--committee-pub-key-and-signature 03a451e61a7aece6f073ab5254f3ce480779481792a591dc069523283f147813cc:65dffc4a4887ed8b456026593ba29293a10591967a569a4f8560c4132a8753b576564f5be0cd60efbe56e34a4cd256e9e640466857eb69a028aead2cc69ddd8b \
--new-committee-pub-key 03a649eb4916dfcc1f949b48173a59c67920d81c87a3c33736f006716e0b8007f5 \
--new-committee-pub-key 026fbdf2c6b5dbc6dec546500d92e2b44aba93bb429cfa20bced88a87f4f28606c \
--new-committee-pub-key 03a6eb4c61a4d24f26634ae7b7f96feaf8b8a20225fa5286e8ac2ff373baa3b90c \
--new-committee-pub-key 03033ae1c5aa103c2418f4c0e6fc34bbbcd6a458872a8fcc7c9cdd92c29aa7889c \
--new-committee-pub-key 0213b11fbd17c83fa594ba0fb7617663ef7290ec214f811d5b9a0377e62e5861de \
--new-committee-pub-key 0204c46e64c753f20b76df593a94f36038803765b6c65b5f6a1990918e6caff3a5 \
--new-committee-pub-key 02469792e5afc4cb4c221ae42dabceca54285197d31bc2a377a177f46591d033c1 \
--new-committee-pub-key 039502f7dd22999bccfbcd9294aeadc6d061662c97622c50b9c91a4855e88c289f \
--new-committee-pub-key 022c1d1be84311c61afa812e83a376da001b7e38d49390405d75f53e948dd84c0a \
--new-committee-pub-key 0242dc14051754023311815fdb91ee8e9a7a58f34e9bcb364bc8d5edeb8f49ce79 \
--sidechain-epoch 1 \
--new-committee-validator-cbor-encoded-address d8799fd87a9f581c6eb83895f10d63a4e3d76b3f79a9226b83eb80c193afe08975729cf9ffd87a80ff \
--previous-merkle-root fa43e2b2d66e4c4db3be723eb5a4e1ba718aca4a375139600b6f53de258e2bb3
```
Note that the `--sidechain-epoch` is `1` which is greater than `0` (the current sidechain epoch stored on chain).

Let's do what it says and execute that command.
```
$ nix run .#sidechain-main-cli -- committee-hash \
--payment-signing-key-file ./payment.skey \
--genesis-committee-hash-utxo ea092fb8d592f7533021598178182ba9db2ef5f8c233db802ca1728994df1193#2 \
--sidechain-id 69 \
--sidechain-genesis-hash 112233 \
--threshold-numerator 2 \
--threshold-denominator 3 \
--atms-kind plain \
--committee-pub-key-and-signature 026d98b269d0369c65200eab0114d230c08134220d8ee47733c4f1de6832bad87f:ebec7a67229739f93b4b14a1317911e983da20270d754b43d7786bb8fac5377076c48553a0578bb7dfcb27c5926d238ed745ed89d87bca51a3acf65cc6e1cabf \
--committee-pub-key-and-signature 024f37ee7a521295eed5540fec4a303fbbceb35423fe62d7fc8d985806f29bd8eb:c1d81579478bf06291a9738fb8a09f1bbcc5dd67fbab8d4ecb6efc75d378b5a63b043bbad5494c43243ee30ddb8cb8897f6d679c833b2446eebe29f57990a541 \
--committee-pub-key-and-signature 03259582ab42143dc5dba149558cfa1629212086ba581c56d17fb9c79f155c2919:71a41734a8a04b24826cdea9fa2b5a7b1b59c1fa555eb830f7c16168e03c8f7829839a69427da64d2c99257c76257e8e882353f7ebd9ab047086b24524f7314c \
--committee-pub-key-and-signature 020d17ed61b1c27214310d9102aa4b49a0231bf02d6621a5ce3d3f778910752c77:c118e6cd0108dcc8124601c05a88520d379ebf7bc93421c29df7230a890b5a7378d5b8a8f7db37bd36fe804d0b3f92d8e4b5d8e26952427d9ad410903c3efbc2 \
--committee-pub-key-and-signature 02899c2442f77ca98b2e82d44fd78a74a92fa51f4157217a981576f8e01d04a2af:cb0be0866e23889e23aba1a0f3bb338e13fa44e5fa5164892d8e7346b7a187da2b1feb57e79b8c2edb73bfd74342a179d908b19e49b048d5b32980f43f8e8882 \
--committee-pub-key-and-signature 0267f78812ee29b169c6d2b24792b2e5b7f6bec0a64191a49b02354e11e6a713d8:b130e6b014902a3474f32305e3be72877ae57c610a65562937878b539ea350eb5f9ac0f007d6901790ca72ef135cceac5184e67fa58d8ba12988b9ad0a51b9cb \
--committee-pub-key-and-signature 03edad4ed70b04948084447c117cccaceaac520b305cbaef16304577a8acb6696a:ccaf2269e59997e302b9c37027438b7caf7b5c01fda074797322e80cee0cbc762580aa6a88a50455e12411b1f1b9dafe465aec65b7d9eadaf8ac62a98899f6e8 \
--committee-pub-key-and-signature 020a585f9fd995517abf97776b55de33be8220fa25f4098c3b0efb1e6da5d35781:4e8da03c017e5b4ffb2672442504943bf231053e58f054820086fec8b0a6793c2431a9ae7f86e17f1e959fb76054991f8ab3a67ce283be9f206396d722e9ffc9 \
--committee-pub-key-and-signature 0354fcd0de4fa92d42de31c6fc5f7cfeafb3ba037c55d2f3bced0c8fb8fb4e5406:8aac03014bf28b544a1d5763a9bc8746a3875488fa7709fb174a8dff05891a2e0d9cb3c1ef52bcdbf1c4165e862685c33d295e8abc9c04486e5bdc2de0a440e6 \
--committee-pub-key-and-signature 03a451e61a7aece6f073ab5254f3ce480779481792a591dc069523283f147813cc:65dffc4a4887ed8b456026593ba29293a10591967a569a4f8560c4132a8753b576564f5be0cd60efbe56e34a4cd256e9e640466857eb69a028aead2cc69ddd8b \
--new-committee-pub-key 03a649eb4916dfcc1f949b48173a59c67920d81c87a3c33736f006716e0b8007f5 \
--new-committee-pub-key 026fbdf2c6b5dbc6dec546500d92e2b44aba93bb429cfa20bced88a87f4f28606c \
--new-committee-pub-key 03a6eb4c61a4d24f26634ae7b7f96feaf8b8a20225fa5286e8ac2ff373baa3b90c \
--new-committee-pub-key 03033ae1c5aa103c2418f4c0e6fc34bbbcd6a458872a8fcc7c9cdd92c29aa7889c \
--new-committee-pub-key 0213b11fbd17c83fa594ba0fb7617663ef7290ec214f811d5b9a0377e62e5861de \
--new-committee-pub-key 0204c46e64c753f20b76df593a94f36038803765b6c65b5f6a1990918e6caff3a5 \
--new-committee-pub-key 02469792e5afc4cb4c221ae42dabceca54285197d31bc2a377a177f46591d033c1 \
--new-committee-pub-key 039502f7dd22999bccfbcd9294aeadc6d061662c97622c50b9c91a4855e88c289f \
--new-committee-pub-key 022c1d1be84311c61afa812e83a376da001b7e38d49390405d75f53e948dd84c0a \
--new-committee-pub-key 0242dc14051754023311815fdb91ee8e9a7a58f34e9bcb364bc8d5edeb8f49ce79 \
--sidechain-epoch 1 \
--new-committee-validator-cbor-encoded-address d8799fd87a9f581c6eb83895f10d63a4e3d76b3f79a9226b83eb80c193afe08975729cf9ffd87a80ff \
--previous-merkle-root fa43e2b2d66e4c4db3be723eb5a4e1ba718aca4a375139600b6f53de258e2bb3
[INFO] 2023-06-27T09:00:22.301Z UpdateCommitteeHash.updateCommitteeHash: Submitted update committee hash transaction: (TransactionHash (hexToByteArrayUnsafe "83feed1b090e5f1a7b394279b47c029ff005fa9c114ec3076fae58dfb40d08b3"))
[INFO] 2023-06-27T09:00:55.658Z UpdateCommitteeHash.updateCommitteeHash: Update committee hash transaction submitted successfully
{"endpoint":"CommitteeHash","transactionId":"83feed1b090e5f1a7b394279b47c029ff005fa9c114ec3076fae58dfb40d08b3"}

```
which indicates that the committee was updated successfully.

# 9 Registering a committee candidate
We describe how one may register a committee candidate. As an overview, we will:
- Find a UTxO that we may spend to register a committee candidate
- Register the committee candidate

Recall this may occur at any time and is not dependent on initialisation happening prior to this contract.

1. Find a UTxO that we may spend to register a committee candidate. Run the following command to query the cardano-node what UTxOs we may spend.
```
$ docker exec \
    -e CARDANO_NODE_SOCKET_PATH="/ipc/node.socket" store_cardano-node_1 \
    cardano-cli query utxo --testnet-magic 2 --address "$PUBLIC_KEY"
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0989c38f40b6f490fbcb5bdedc0e8c40192b81ae82b3be6f45d05c42e93a3bfa     3        1344720 lovelace + 69 e1869d5ce721db9e6b9ae21678ea15bc2630640728f345a46832c9da.4655454c + TxOutDatumNone
9903ad6e0aa7c49a9692eaf53d62b37d673dbf759de4e8513192a8d337294782     0        9897014203 lovelace + 420 50fdad256d2fdaf0089915e81fbe0ff4fc4633eee3af8dc9a8111035.4655454c + 420 c5ae7e4af4502074687c642f9a6dd97e0ec328ec862d4b40cd87eeac.4655454c + TxOutDatumNone
b39b82f56be9f55af4d3b501ac084c2d2c5be3af8665f7a5bba53c63b0740021     3        1344720 lovelace + 69 c5ae7e4af4502074687c642f9a6dd97e0ec328ec862d4b40cd87eeac.4655454c + TxOutDatumNone
f06b5829a9e02f341e3267fe10bc97a5b9df095713102e4276f957c863213dd1     3        1344720 lovelace + 69 c5ae7e4af4502074687c642f9a6dd97e0ec328ec862d4b40cd87eeac.4655454c + TxOutDatumNone
```
In particular, we will consider spending the second transaction.
```
9903ad6e0aa7c49a9692eaf53d62b37d673dbf759de4e8513192a8d337294782#0
```

2. Registering a committee candidate. We will use `trustless-sidechain-gen-signatures` to aid in doing this. Run the following command.
```
$ cabal run -v0 trustless-sidechain-gen-signatures -- register \
  `# Sidechain parameters` \
  --payment-signing-key-file "$SIGNING_KEY" \
  --genesis-committee-hash-utxo "$GENESIS_UTXO" \
  --sidechain-id 69 \
  --threshold 2/3 \
  --sidechain-genesis-hash 112233 \
  --atms-kind plain \
  `# Block producer parameters` \
  --spo-signing-key "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" \
  --sidechain-signing-key "483517da9669cf716a03878bc858fe4d57d9a99ab49c8708753bf449e77de1c5" \
  --registration-utxo "9903ad6e0aa7c49a9692eaf53d62b37d673dbf759de4e8513192a8d337294782#0"
Please call sidechain-main-cli with the following arguments:
nix run .#sidechain-main-cli -- register \
--payment-signing-key-file ./test.skey \
--genesis-committee-hash-utxo ea092fb8d592f7533021598178182ba9db2ef5f8c233db802ca1728994df1193#2 \
--sidechain-id 69 \
--sidechain-genesis-hash 112233 \
--threshold-numerator 2 \
--threshold-denominator 3 \
--atms-kind plain \
--spo-public-key e734ea6c2b6257de72355e472aa05a4c487e6b463c029ed306df2f01b5636b58 \
--sidechain-public-key 0281158622b7d2eb738b885e1cca50218fb36ab4dc39014b83286b8ed95c78789d \
--spo-signature 4d644cebac3a73a8b8596a4bd3d4f191b8987f7892fd602f6f8968b629e7b194f605c8243f3084713a56074441815193ce0320f651fda80639e3806127ef3806 \
--sidechain-signature 1f9721994b4699c5f84d041d6a7ac5e52989568ba5b3203a2d4e6732e0a42c5a4f7f94f1a31b97f450025447ff5d2d3be2550cf3f1c779609b16000771f6b4de \
--registration-utxo 9903ad6e0aa7c49a9692eaf53d62b37d673dbf759de4e8513192a8d337294782#0
```
Some notes:
- `--spo-signing-key` is used to generate a key for Cardano. TODO: perhaps we should allow inputting a key.
- `--sidechain-signing-key` is an arbitrary signing key for someone on the sidechain. This can be generated as in [#5](#5-Initialising-the-sidechain), and looking at the outputted file.
- `--registration-utxo` was the UTxO that we found just above.

Let's do what it says.

```
$ nix run .#sidechain-main-cli -- register \
--payment-signing-key-file ./test.skey \
--genesis-committee-hash-utxo ea092fb8d592f7533021598178182ba9db2ef5f8c233db802ca1728994df1193#2 \
--sidechain-id 69 \
--sidechain-genesis-hash 112233 \
--threshold-numerator 2 \
--threshold-denominator 3 \
--atms-kind plain \
--spo-public-key e734ea6c2b6257de72355e472aa05a4c487e6b463c029ed306df2f01b5636b58 \
--sidechain-public-key 0281158622b7d2eb738b885e1cca50218fb36ab4dc39014b83286b8ed95c78789d \
--spo-signature 4d644cebac3a73a8b8596a4bd3d4f191b8987f7892fd602f6f8968b629e7b194f605c8243f3084713a56074441815193ce0320f651fda80639e3806127ef3806 \
--sidechain-signature 1f9721994b4699c5f84d041d6a7ac5e52989568ba5b3203a2d4e6732e0a42c5a4f7f94f1a31b97f450025447ff5d2d3be2550cf3f1c779609b16000771f6b4de \
--registration-utxo 9903ad6e0aa7c49a9692eaf53d62b37d673dbf759de4e8513192a8d337294782#0
[INFO] 2022-12-06T03:31:18.130Z CommitteeCandidateValidator.register: Submitted committeeCandidate register Tx: (TransactionHash (hexToByteArrayUnsafe "32b66f61a06d7bd6c6e70351fd8d1afea36e04c3ee487414637ef78e590dbf93"))
[INFO] 2022-12-06T03:31:22.169Z CommitteeCandidateValidator.register: Register Tx submitted successfully!
{"endpoint":"CommitteeCandidateReg","transactionId":"32b66f61a06d7bd6c6e70351fd8d1afea36e04c3ee487414637ef78e590dbf93"}
```
which shows it has succeeded.
