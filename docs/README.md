# Trustless sidechain

This specification details the main chain contract of a trustless sidechain system. The work relies on the BLS ATMS signature scheme, but this might not be available in time for Cardano, so we decided to implement the contract in two phases:

- [phase 1](https://github.com/mlabs-haskell/trustless-sidechain/milestone/1): MVP using append only signature scheme
- [phase 1.5](https://github.com/mlabs-haskell/trustless-sidechain/milestone/3): script optimisations and security improvements
- [phase 2](https://github.com/mlabs-haskell/trustless-sidechain/milestone/2): using ATMS signature scheme

Mainchain utilizes the following components to handle interactions with a sidechain:

- `FUELMintingPolicy`: minting policy validating the mint or burn of FUEL tokens on mainchain ([2.](#2.-transfer-fuel-tokens-from-mainchain-to-sidechain), [3.2.](#3.2.-individual-claiming))
- `MPTRootTokenMintingPolicy`: minting policy for storing cross-chain transaction bundles' MPT roots ([3.1.](#3.1.-merkle-root-insertion))
- `CommitteeCandidateValidator`: script address for committee candidates ([4.](#4.-register-committee-candidate), [5.](#5.-deregister-committee-member%2Fcandidate))
- `MPTRootTokenValidator`: script address for storing `MPTRootToken`s ([3.1.](#3.1.-merkle-root-insertion))
- `CommitteeHashValidator`: script address for the committee members' hash ([1.](#1.-initialise-contract), [6.](#6.-update-committee-hash))

All of these policies/validators are parameterised by the sidechain parameters, so we can get unique minting policy and validator script hashes.

```haskell
data SidechainParams = SidechainParams
  { chainId :: Integer
  , genesisHash :: GenesisHash
    -- ^ 'GenesisHash' is a type alias for ByteString
  , genesisMint :: Maybe TxOutRef
    -- ^ 'genesisMint' is an arbitrary 'TxOutRef' used in the Passive Bridge setup, where
    -- FUEL minting can only happen once. This parameter will be removed in the final product.
  , genesisUtxo :: TxOutRef
    -- ^ 'genesisUtxo' is an arbitrary 'TxOutRef' used to identify internal
    -- 'AssetClass's (e.g. see [6.](#6.-update-committee-hash)) of the
    -- sidechain
  }
```

### 1. Initialise contract

For initialisation, we need to set the first committee hash on chain using an NFT (consuming some arbitrary utxo). We use this committee hash to verify signatures for sidechain to mainchain transfers. This is a hash of concatenated public key hashes of the committee members. This hash will be updated when the committee changes, see [6.1](#61-update-committee-hash) for more details.

**Workflow:**

1. Call the initialize sidechain endpoint to generate the `SidechainParams` for a new
   sidechain.
2. Use the given sidechain parameters for the rest of the endpoints to work
   with _this_ particular sidechain.

**Endpoint params:**

```haskell
data InitSidechainParams = InitSidechainParams
  { initChainId :: Integer
  , initGenesisHash :: GenesisHash
    -- ^ 'GenesisHash' is a type alias for ByteString
  , initUtxo :: TxOutRef
    -- ^ 'initUtxo' is used for creating the committee NFT
  , initCommittee :: [PubKey]
    -- ^ 'initCommittee' is the initial committee of the sidechain
  , initMint :: Maybe TxOutRef
    -- ^ 'initMint' is used in the Passive Bridge only, and will be removed in the final product
  }
```

### 2. Transfer FUEL tokens from mainchain to sidechain

**Workflow:**

1. Call the burn endpoint of the contract with BurnParams
2. A transaction will be submitted to mainchain burning the specified amount of FUEL tokens and the corresponding sidechain address in the redeemer
3. The Bridge component observing the mainchain where the given minting policy is handled, verifies the transaction and creates an appropriate sidechain transaction

**Endpoint params:**

```haskell
data BurnParams = BurnParams
  { recipient :: SidechainAddress
    -- ^ 'SidechainAddress' is a type alias for a ByteString (e.g. 0x112233aabbcc)
  , amount :: Integer
  }
```

![MC to SC](MC-SC.svg)
<figcaption align = "center"><i>Mainchain to Sidechain transaction (burning FUEL tokens)</i></figcaption><br />

### 3. Transfer FUEL tokens from sidechain to mainchain

**Workflow:**

1. Sidechain collects unhandled transactions and bundles them at the end of each sidechain epoch
2. Sidechain block producers compute `txs = outgoing_txs.map(tx => blake2b(cbor(MerkleTreeEntry(tx)))` for each transaction (see `MerkleTreeEntry`), and create a Merkle-tree from these. The root of this tree is signed by the committee members with an appended signature
3. Bridge broadcasts Merkle root to chain
4. Txs can be claimed [individually](#32-individual-claiming)


#### 3.1. Merkle root insertion

**Endpoint params for merkle root insertion:**

```haskell
data SaveRootParams = SaveRootParams
  { sidechainParams :: SidechainParams
  , merkleRoot :: ByteString
  , lastMerkleRoot :: Maybe ByteString
  , committeeSignatures :: [(PubKey, Maybe ByteString)] -- Public keys of all committee members with their corresponding signatures
  }
```

Merkle roots are stored on-chain, using `MPTRootToken`s, where the `tokenName` is the Merkle root. These tokens must be at the `MPTRootTokenValidator` script address.

**Redeemer:**

```haskell
data SignedMerkleRoot = SignedMerkleRoot
  { merkleRoot :: ByteString
  , lastMerkleRoot :: Maybe ByteString -- Last Merkle root hash
  , signatures :: [ByteString] -- Current committee signatures ordered as their corresponding keys
  , beneficiary :: ByteString -- Sidechain address
  , committeePubKeys :: [SidechainPubKey] -- Lexicographically sorted public keys of all committee members
  }
```

Minting policy verifies the following:

- signature can be verified with the submitted public keys of committee members, and the concatenated and hashed value of these keys correspond to the one saved on-chain
- list of public keys does not contain duplicates
- if `lastMerkleRoot` is specified, the UTxO with the given roothash is referenced in the transaction as a reference input

Validator script verifies the following:

- UTxOs containing an `MPTRootToken` cannot be unlocked from the script address

![MPTRootToken minting](MPTRoot.svg)
<figcaption align = "center"><i>Merkle root token minting</i></figcaption><br />

The merkle tree has to be constructed in the exact same way as it is done by [following merkle tree implementation](https://github.com/mlabs-haskell/trustless-sidechain/blob/master/src/TrustlessSidechain/MerkleTree.hs). Entries in the tree should be calculated as follow:

```haskell
data MerkleTreeEntry = MerkleTreeEntry
  { index :: Integer -- 32 bit unsigned integer, used to provide uniqueness among transactions within the tree
  , amount :: Integer -- 256 bit unsigned integer that represents amount of tokens being sent out of the bridge
  , recipient :: ByteString -- arbitrary length bytestring that represents decoded bech32 cardano address
  , sidechainEpoch :: Integer -- sidechain epoch for which merkle tree was created
  , lastMerkleRoot :: Maybe ByteString -- lastMerkleRoot is added to make sure that the hashed entry is unique
  }
```

```
entry = blake2b(cbor(MerkleTreeEntry))
```

Signatures for merkle tree should be constructed as follow:

```haskell
data MerkleRootInsertionMessage = MerkleRootInsertionMessage
  { sidechainParams :: SidechainParams
  , sidechainEpoch :: Integer -- sidechain epoch for which we obtain the signature
  , merkleRoot :: ByteString
  , lastMerkleRoot :: Maybe ByteString
  }
```

```
signature = ecdsa.sign(data: blake2b(cbor(MerkleRootInsertionMessage)), key: committeeMemberPrvKey)
```

#### 3.2. Individual claiming

**Endpoint params for claiming:**

```haskell
data MintParams = MintParams
  { amount :: Integer
  , recipient :: ByteString
  , merkleProof :: MerkleProof
  , chainId :: Integer
  , index :: Integer
  , sidechainEpoch :: Integer
  }
```

Minting policy verifies the following:

- `MPTRootToken` with the name of the Merkle root of the transaction (calculated from from the proof) can be found in the `MPTRootTokenValidator` script address
- chainId matches the minting policy chainId
- recipient, amount, index and sidechainEpoch combined with merkleProof match against merkleRootHash
- `claimTransactionHash` of the transaction is NOT included in the distributed set[^1]
- a new entry with the `claimTransactionHash` of the transaction is created in the distributed set
- the transaction is signed by the recipient
- the amount matches the actual tx body contents

where the `claimTransactionHash` is a `blake2(cbor(MerkleTreeEntry))`, uniquely identifying a cross chain transaction by pointing to a Merkle tree and the index of the transaction in the tree

![SC to MC](SC-MC.svg)
<figcaption align = "center"><i>Sidechain to Mainchain transaction (claiming tokens)</i></figcaption><br />

**Minting policy redeemer:**

```haskell
data FUELRedeemer
  = MainToSide ByteString ByteString
  -- ^ Recipient address on the sidechain and the signature of its owner (see 2.)
  | SideToMain MerkleTreeEntry MerkleProof
```

### 4. Register committee candidate

**Workflow:**

1. An SPO registering as a block producer (commitee member) for the sidechain sends BlockProducerRegistration and its signature (where the signed message contains the sidechain parameters, sidechain public key and the input utxo in CBOR format)
2. The Bridge monitoring the committee candidate script address is validating the SPO credentials, chainId, and the consumed inputUtxo

**Datum:**

```haskell
data BlockProducerRegistration = BlockProducerRegistration
  { bprSpoPubKey :: PubKey -- own public key
  , bprInputUtxo :: TxOutRef -- a utxo that must be spent with the transaction
  , bprSidechainPubKey :: ByteString -- public key in the sidechain's desired format
  , bprSpoSignature :: Signature -- Signature of the SPO private key
  , bprSidechainSignature :: ByteString -- Signature of the sidechain private key
  }
```

### 5. Deregister committee member/candidate

**Workflow:**

1. The UTxO with the registration information can be redeemed by the original sender (doesn't have to check the inputUtxo)
2. The Bridge monitoring the committee candidate script address interprets this as a deregister action

### 6. Committee handover

In the current implementation of the sidechain, a [Merkle root insertion (3.1)](#3.1.-merkle-root-insertion) can only occur once per sidechain epoch at the time of the committee handover. We expose an endpoint which can handle this action, however the underlying implementation is detached, so in theory, we could do Merkle root insertion and Committee Hash Update actions independently.

We have to be careful about the order of these actions. If the transaction inserting the merkle root for sidechain epoch 1 gets submitted *after* the committee handover from `committee of epoch 1` to `committee of epoch 2` transaction, the signature would become invalid, since it is signed by the `committee of epoch 1`. To mitigate this issue, we introduce `merkle root chain`, for details see: [6.2](#62-merkle-root-chaining)


#### 6.1 Update committee hash

1. Bridge component triggers the Cardano transaction. This tx does the following:

**Endpoint params:**

```haskell
data UpdateCommitteeHashParams = UpdateCommitteeHashParams
  { -- | The public keys of the new committee.
    newCommitteePubKeys :: [SidechainPubKey]
  , -- | The asset class of the NFT identifying this committee hash
    token :: AssetClass
  , -- | The signature for the new committee hash.
    committeeSignatures :: [(SidechainPubKey, Maybe ByteString)]
  , -- sidechain parameters
     sidechainParams :: SidechainParams
  , -- sidechain epoch for which we obtain the signature
      sidechainEpoch :: Integer
  , -- last merkle root inserted on chain
    lastMerkleRoot :: Maybe ByteString
  }
```

Validator script verifies the following:

- verifies that hash of committeePublicKeys matches the hash saved on chain
- verifies that all the provided signatures are valid
- verifies that size(signatures) > 2/3 \* size(committeePubKeys)
- verifies the NFT of the UTxO holding the old verification key at the script address
- consumes the above mentioned UTxO
- outputs a new UTxO with the updated committee hash containing the NFT to the same script address
- reference to the last Merkle root is referenced in the transaction

**Datum:**

```haskell
data UpdateCommitteeHash = UpdateCommitteeHash
  { committeePubKeysHash :: ByteString -- Hash of all lexicographically sorted public keys of the current committee members
  }
```

```
committeePubKeys = sort([key1, key2, ..., keyN])
committeePubKeysHash = blake2b(concat(committeePubKeys))
keyN - 33 bytes compressed ecdsa public key of a committee member
```

![Public key update](pubkeyupdate.svg)
<figcaption align = "center"><i>Committee handover (updating committee hash)</i></figcaption><br />

**Redeemer:**

```haskell
data UpdateCommitteeRedeemer = UpdateCommitteeRedeemer
  { signatures :: [ByteString]
  , newCommitteePubKeys :: [SidechainPubKey]
  , committeePubKeys :: [SidechainPubKey]
  , sidechainEpoch :: Integer
  , lastMerkleRoot :: Maybe ByteString
  }
```

Signatures are constructed as follow:

```
SidechainPubKey - 33 bytes compressed ecdsa public key
```

```haskell
data UpdateCommitteeMessage = UpdateCommitteeMessage
  { sidechainParams :: SidechainParams
  , sidechainEpoch :: Integer -- sidechain epoch for which we obtain the signature
  , newCommitteePubKeys :: [SidechainPubKey] -- sorted lexicographically
  , lastMerkleRoot :: Maybe ByteString
    -- ^ last Merkle root inserted on chain (Merkle root for the last sidechain epoch)
  }
```

```
signature = ecdsa.sign(data: blake2b(cbor(UpdateCommitteeMessage)), key: committeeMemberPrvKey)
```

#### 6.2. Merkle root chaining


As described in [6. Committee handover](#6.-committee-handover), we have to maintain the correct order of Merkle root insertions and committee hash updates. We introduce a sort of Merkle root chain, where each Merkle root has a reference to its predecessor (if one exists), furthermore all committee hash updates reference the last Merkle root inserted (if one exists).

![Merkle root chaining](MRChain-simple.svg)
<figcaption align = "center"><i>Merkle root chaining (SC ep = sidechain epoch)</i></figcaption><br />

As seen in the graph above, the first Merkle root has no reference, which is completely valid. We do not enforce the existence of the last Merkle root.

In case a sidechain epoch passed without any cross-chain transactions, no Merkle root is inserted, resulting in two committee hash updates referencing the same Merkle root.

![Merkle root chaining - epoch without Merkle root](MRChain-empty.svg)

<figcaption align = "center"><i>Merkle root chaining - epoch without Merkle root (SC ep = sidechain epoch)</i></figcaption><br />

In the future, we want to support multiple Merkle roots per sidechain epoch, so the result could look like the following:

![Merkle root chaining - multipe Merkle roots per epoch](MRChain-multi.svg)
<figcaption align = "center"><i>Merkle root chaining - multiple Merkle roots per epoch (SC ep = sidechain epoch)</i></figcaption><br />

## Appendix

[^1]: Distributed set implementation details are still WIP, but we plan to use something like this: https://github.com/Plutonomicon/plutonomicon/blob/main/stick-breaking-set.md
