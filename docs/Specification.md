# Trustless sidechain - main chain contract specification

This specification details the main chain contract of a trustless sidechain system. The work relies on the BLS ATMS signature scheme, but this might not be available in time for Cardano, so we decided to implement the contract in two phases:

- [phase 1](https://github.com/mlabs-haskell/trustless-sidechain/milestone/1): MVP using append only signature scheme
- [phase 1.5](https://github.com/mlabs-haskell/trustless-sidechain/milestone/3): script optimisations and security improvements
- [phase 2](https://github.com/mlabs-haskell/trustless-sidechain/milestone/2): using ATMS signature scheme

Mainchain utilizes the following components to handle interactions with a sidechain:

- `FUELProxyPolicy`: minting policy validating the mint or burn of FUEL tokens on mainchain, delegating actual burning and minting conditions to versioned policies ([2.](#2-transfer-fuel-tokens-from-mainchain-to-sidechain), [3.2.](#32-individual-claiming))
  - `FUELMintingPolicy.V1`: minting policy based on Merkle trees
  - `FUELBurningPolicy.V1`: burning policy that allows to burn FUEL unconditionally
  - `FUELMintingPolicy.V2` and `FUELBurningPolicy.V2`: policies that allow to mint and burn FUEL unconditionally. They exist for demonstration purposes only and should be removed at some point in the future.
- `MerkleRootTokenMintingPolicy`: minting policy for storing cross-chain transaction bundles' Merkle roots ([3.1.](#31-merkle-root-insertion))
- `CommitteeCandidateValidator`: script address for committee candidates ([4.](#4-register-committee-candidate), [5.](#5-deregister-committee-membercandidate))
- `CandidatePermissionToken`: a minting policy for permissioned committee candidates ([4.1](#41-candidate-permission-token))
- `MerkleRootTokenValidator`: script address for storing `MerkleRootToken`s ([3.1.](#31-merkle-root-insertion))
- `UpdateCommitteeValidator`: script address for the committee members' aggregated public key ([1.](#1-initialise-contract), [6.](#6-committee-handover))
- `CommitteeOraclePolicy`: oneshot token pointing to the UTxO which contains the current committee's aggregated public keys with the sidechain epoch ([6.1](#61-update-committee))
- `DsConfValidator`: validator holding the distributed set configuration ([Distributed Set](./DistributedSet.md))
- `DsConfPolicy`: oneshot token identifying the UTxO holding the distributed set configuration ([Distributed Set](./DistributedSet.md))
- `DsInsertValidator`: validator handling distributed set entry ([Distributed Set](./DistributedSet.md))
- `DsKeyPolicy`: tokens identifying the distributed set entries ([Distributed Set](./DistributedSet.md))
- `CheckpointValidator`: validator handling checkpoints ([7.](#7-checkpointing))
- `CheckpointPolicy`: oneshot token identifying the UTxO holding the checkpoint ([7.](#7-checkpointing))
- `VersionOraclePolicy`: versioning tokens that store script ID and its version ([8.](#8-versioning-system))
- `VersionOracleValidator`: validator that stores versioning tokens and manages updates and invalidation of versions
- A committee certificate verification minting policy to isolate the ATMS scheme functionality ([8.](#8-committee-certificate-minting-policies))

All of these policies/validators are parameterised by the sidechain parameters, so we can get unique minting policy and validator script hashes.

```haskell
data SidechainParams = SidechainParams
  { chainId :: Integer
  , genesisHash :: GenesisHash
    -- ^ 'GenesisHash' is a type alias for ByteString
  , genesisUtxo :: TxOutRef
    -- ^ 'genesisUtxo' is an arbitrary 'TxOutRef' used to identify internal
    -- 'AssetClass's
  , thresholdNumerator :: Integer
    -- ^ 'thresholdNumerator' is the numerator for the ratio of the committee
    -- needed to sign off committee handovers / Merkle roots
  , thresholdDenominator :: Integer
    -- ^ 'thresholdDenominator' is the denominator for the ratio of the
    -- committee needed to sign off committee handovers / Merkle roots
  , governanceAuthority :: GovernanceAuthority
    -- ^ 'governanceAuthority' stores credentials of a governing body allowed to
    -- make updates to versioned scripts.  For now we just use a master public
    -- key, whose owner is allowed to make any decisions about script versions.
  }
```

### 1. Initialise contract

For initialisation, we must mint multiple NFTs to uniquely identify UTxOs which contain (as datum) information for the sidechain.
In particular, we mint an NFT for identifying the current committee's aggregated public key for the sidechain's committee certificate minting policy (see [8.](#8-committee-certificate-minting-policies) and `CommitteeOraclePolicy` in [6.1](#61-update-committee)), and we mint an NFT for maintaining some book keeping to uniquely identify data for the [distributed set](./DistributedSet.md).

An initial committee must be provided which will be used to verify signatures for sidechain to mainchain transfers via the sidechain's committee certificate minting policy (see [8](#8-committee-certificate-minting-policies)).

**Workflow:**

1. Call the initialize sidechain endpoint to generate the `SidechainParams` for a new sidechain.
2. Use the given sidechain parameters for the rest of the endpoints to work with this sidechain.

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
  , initSidechainEpoch :: Integer
    -- ^ 'initSidechainEpoch' is the initial sidechain epoch of the sidechain
  , thresholdNumerator :: Integer
    -- ^ 'thresholdNumerator' is the numerator for the ratio of the committee
    -- needed to sign off committee handovers / Merkle roots
  , thresholdDenominator :: Integer
    -- ^ 'thresholdDenominator' is the denominator for the ratio of the
    -- committee needed to sign off committee handovers / Merkle roots
  , governanceAuthority :: GovernanceAuthority
    -- ^ 'governanceAuthority' stores credentials of a governing body allowed to
    -- make updates to versioned scripts.  For now we just use a master public
    -- key, whose owner is allowed to make any decisions about script versions.
  }
```

### 2. Transfer FUEL tokens from mainchain to sidechain

FUEL tokens on the Cardano network represent locked native assets on the sidechain network. When a certain amount of tokens are locked on a certain sidechain contract, we can mint the equivalent amount on Cardano ([2.](#2-transfer-fuel-tokens-from-mainchain-to-sidechain), [3.2.](#32-individual-claiming)). Conversely burning these tokens on the Cardano network ([3.](#3-transfer-fuel-tokens-from-sidechain-to-mainchain)) will release these tokens and send them to the owner. For more details, refer to the [High level technical specifications](https://docs.google.com/document/d/1UJs4ews1wnKIv4RMyPjFtJcyniyRHi7GmU2JPdUfbQk).

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

![MC to SC](Spec/MC-SC.svg)

<figcaption align = "center"><i>Mainchain to Sidechain transaction (burning FUEL tokens)</i></figcaption><br />

### 3. Transfer FUEL tokens from sidechain to mainchain

**Workflow:**

1. Sidechain collects unhandled transactions and bundles them at the end of each sidechain epoch
2. Sidechain block producers compute `txs = outgoing_txs.map(tx => blake2b(cbor(MerkleTreeEntry(tx)))` for each transaction (see `MerkleTreeEntry`), and create a Merkle-tree from these. The root of this tree is signed by at least `t` (multisig threshold) of the committee members with an appended signature scheme
3. Bridge broadcasts Merkle root to chain
4. Txs can be claimed [individually](#32-individual-claiming)

#### 3.1. Merkle root insertion

**Endpoint params for Merkle root insertion:**

```haskell
data SaveRootParams = SaveRootParams
  { sidechainParams :: SidechainParams
    -- ^ Parameters identifying the Sidechain
  , merkleRoot :: ByteString
  , previousMerkleRoot :: Maybe ByteString
    -- ^ Chaining the Merkle roots to ensure ordering. The first root will have Nothing here.
  , committeeSignatures :: [(ByteString, Maybe ByteString)]
    -- ^ Public keys of all committee members with their corresponding signatures if there's one
  }
```

Merkle roots are stored on-chain, using `MerkleRootToken`s, where the `tokenName` is the Merkle root. These tokens must be at the `MerkleRootTokenValidator` script address.

**Redeemer:**

```haskell
newtype SignedMerkleRootRedeemer = SignedMerkleRootRedeemer
  { previousMerkleRoot :: Maybe ByteString
  }
```

Minting policy verifies the following:

- If `previousMerkleRoot` is specified, the UTxO with the given roothash is referenced in the transaction as a reference input

- The committee certificate minting policy mints with token name `tn` which
  satisfies the following.

  ```
  tn = blake2b_256(cbor(MerkleRootInsertionMessage))
  ```

  where

  ```haskell
  data MerkleRootInsertionMessage = MerkleRootInsertionMessage
    { sidechainParams :: SidechainParams
      -- ^ Parameters identifying the Sidechain
    , merkleRoot :: ByteString
    , previousMerkleRoot :: Maybe ByteString
    }
  ```

- A unique `MerkleRootToken` is minted with token name as `merkleRoot` at a `MerkleRootTokenValidator` script address.

- The sidechain parameters in `msg` match the minting policy's sidechain parameters.

Validator script verifies the following:

- UTxOs containing an `MerkleRootToken` cannot be unlocked from the script address

![MerkleRootToken minting](Spec/MerkleRoot.svg)

<figcaption align = "center"><i>Merkle root token minting</i></figcaption><br />

The Merkle tree has to be constructed in the exact same way as it is done by the following [Merkle tree implementation](https://github.com/mlabs-haskell/trustless-sidechain/blob/master/src/TrustlessSidechain/MerkleTree.hs). Entries in the tree should be calculated as follows:

```haskell
data MerkleTreeEntry = MerkleTreeEntry
  { index :: Integer -- 32 bit unsigned integer, used to provide uniqueness among transactions within the tree
  , amount :: Integer -- 256 bit unsigned integer that represents amount of tokens being sent out of the bridge
  , recipient :: ByteString -- arbitrary length bytestring that represents decoded bech32 cardano address
  , previousMerkleRoot :: Maybe ByteString -- previousMerkleRoot is added to make sure that the hashed entry is unique
  }
```

```
entry = blake2b(cbor(MerkleTreeEntry))
```

#### 3.2. Individual claiming

**Endpoint params for claiming:**

```haskell
data MintParams = MintParams
  { amount :: Integer
  , recipient :: ByteString
  , merkleProof :: MerkleProof
  , index :: Integer
  , previousMerkleRoot:: Maybe ByteString
  , dsUtxo:: Maybe TxOutRef
    -- ^ 'dsUtxo' is used exclusively offchain to potentially avoid a linear
    -- scan through the UTxO set to ensure uniqueness of claiming FUEL.
    -- See [footnote [1] in the distributed set document](./DistributedSet.md)
    -- for details.
  }
```

Minting policy verifies the following:

- `MerkleRootToken` with the name of the Merkle root of the transaction (calculated from from the proof) can be found in the `MerkleRootTokenValidator` script address
- recipient, amount, index and previousMerkleRoot combined with merkleProof match against merkleRootHash
- `claimTransactionHash` of the transaction is NOT included in the distributed set (more details about the distributed set can be found [here](./DistributedSet.md))
- a new entry with the `claimTransactionHash` of the transaction is created in the distributed set
- the transaction is signed by the recipient
- the amount matches the actual tx body contents

where the `claimTransactionHash` is a `blake2(cbor(MerkleTreeEntry))`, uniquely identifying a cross chain transaction by pointing to a Merkle tree and the index of the transaction in the tree

![SC to MC](Spec/SC-MC.svg)

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

1. An SPO registering as a block producer (committee member) for the sidechain sends BlockProducerRegistration and its signature (where the signed message contains the sidechain parameters, sidechain public key and the input utxo in CBOR format)
2. The Bridge monitoring the committee candidate script address (optionally, with a specified token -- see [4.1](#4-1-candidate-permission-token)), is validating the SPO credentials, chainId, and the consumed inputUtxo

**Datum:**

```haskell
data BlockProducerRegistration = BlockProducerRegistration
  { -- | Verification keys required by the stake ownership model
    stakeOwnership :: StakeOwnership
  , -- | public key in the sidechain's desired format
    sidechainPubKey :: LedgerBytes
  , -- | Signature of the sidechain
    sidechainSignature :: Signature
  , -- | A UTxO that must be spent by the transaction
    inputUtxo :: TxOutRef
  , -- | Owner public key hash
    ownPkh :: PubKeyHash
  , -- | Sidechain authority discovery key
    auraKey :: LedgerBytes
  , -- | Sidechain grandpa key
    grandpaKey :: LedgerBytes
  }

data StakeOwnership
  = -- | Ada stake based configuration comprises the SPO public key and signature
    AdaBasedStaking PubKey Signature
  | -- | Token based staking configuration
    TokenBasedStaking
```

#### 4.1 Candidate permission token

**Endpoint params:**

```haskell
data CandidatePermissionMintParams = CandidatePermissionMintParams
  { candidateMintPermissionMint :: CandidatePermissionMint
  , tokenName ∷ TokenName
  , amount ∷ BigInt
  }
```

where

```haskell
data CandidatePermissionMint = CandidatePermissionMint
  { sidechainParams ∷ SidechainParams
  , permissionTokenUtxo ∷ TxOutRef
  }
```

parameterizes the onchain minting policy.

**Workflow:**

1. The Bridge chooses a particular `TxOutRef` and uses this to mint the given
   amount of `CandidatePermissionToken`s, and records the `CurrencySymbol` and
   `TxOutRef`of the `CandidatePermissionToken`.
2. The `CandidatePermissionToken`s are distributed amongst SPOs.
3. As in [4.](#4-register-committee-candidate), the Bridge uses this
   `CandidatePermissionToken` to distinguish which committee candidates have
   permission to register. Committee candidates registering with the
   `CandidatePermissionToken` are considered valid, but otherwise are
   considered invalid.

   If it is desired for the system to be permissionless, the Bridge may ignore
   the `CandidatePermissionToken` requirement and consider all registrations as valid.

Minting policy verifies the following: - The given `permissionTokenUtxo` it is parameterized by is spent.

### 5. Deregister committee member/candidate

**Workflow:**

1. The UTxO with the registration information can be redeemed by the original sender (doesn't have to check the inputUtxo)
2. The Bridge monitoring the committee candidate script address interprets this as a deregister action

### 6. Committee handover

In the current implementation of the sidechain, a [Merkle root insertion (3.1)](#31-merkle-root-insertion) can only occur once per sidechain epoch at the time of the committee handover. We expose an endpoint which can handle this action, however the underlying implementation is detached, so in theory, we could do Merkle root insertion and Committee Hash Update actions independently.

We have to be careful about the order of these actions. If the transaction inserting the Merkle root for sidechain epoch 1 gets submitted _after_ the committee handover from `committee of epoch 1` to `committee of epoch 2` transaction, the signature would become invalid, since it is signed by the `committee of epoch 1`. To mitigate this issue, we introduce `merkle root chain`, for details see: [6.2](#62-merkle-root-chaining)

#### 6.1 Update Committee

1. Bridge component triggers the Cardano transaction. This tx does the following:

**Endpoint params:**

```haskell
data UpdateCommitteeHashParams = UpdateCommitteeHashParams
  { newCommitteePubKeys :: [ByteString]
    -- ^ The public keys of the new committee.
  , committeeSignatures :: [(ByteString, Maybe ByteString)]
    -- ^ Public keys of all committee members with their corresponding signatures if there's one
  , sidechainParams :: SidechainParams
    -- ^ Parameters identifying the Sidechain
  , previousMerkleRoot :: Maybe ByteString
    -- ^ last Merkle root inserted on chain, unless there is no Merkle root inserted yet
  , sidechainEpoch :: Integer
    -- ^ sidechain epoch of the new committee
  }
```

**Datum:**

```haskell
data UpdateCommitteeDatum aggregatePubKeys = UpdateCommitteeDatum
  { committeePubKeysHash :: aggregatePubKeys
    -- ^ aggregated public key of the committee
  , sidechainEpoch :: Integer
    -- ^ sidechain epoch of the committee
  }
```

**Redeemer:**

```haskell
newtype UpdateCommitteeRedeemer = UpdateCommitteeRedeemer
  { previousMerkleRoot :: Maybe ByteString
    -- ^ last Merkle root inserted on chain, unless there is no Merkle root inserted yet
  }
```

Validator script verifies the following:

- Verifies that the committee certificate minting policy mints with token name
  `tn` that satisfies
  ```
  tn = blake2b_256(cbor(msg))
  ```
  for some `msg :: UpdateCommitteeMessage BuiltinData` where
  ```haskell
  data UpdateCommitteeMessage aggregatePubKeys = UpdateCommitteeMessage
    { sidechainParams :: SidechainParams
    , newAggregateCommitteePubKeys :: aggregatePubKeys
    , previousMerkleRoot :: Maybe ByteString
      -- ^ last merkle root inserted on chain, unless there is no Merkle root
      -- inserted yet
    , newSidechainEpoch :: Integer
      -- ^ the sidechain epoch of the new committee
    , newValidatorAddress :: Address
      -- ^ the validator address where the new committee will reside (this allows
      -- one to reuse the `CommitteeOraclePolicy` when one changes the
      -- committee certificate verification minting policy)
    }
  ```
- The transaction corresponds to the signed `msg` in the following sense.

  - The `sidechainParams` in the signed `msg` are the same sidechain
    parameters that the validator is parameterized by

  - The unique script output at address `addr` identified by the NFT
    `CommitteeOraclePolicy` has as datum
    `newUpdateCommitteeHashDatum :: UpdateCommitteeDatum BuiltinData`.

  - `addr` is `newValidatorAddress` in the signed `msg`.

  - `newUpdateCommitteeHashDatum`'s `sidechainEpoch` is `newSidechainEpoch`
    in the signed `msg`, and `sidechainEpoch` is strictly less than
    `newSidechainEpoch` (this prevents replay attacks).

  - `newUpdateCommitteeHashDatum`'s `aggregateCommitteePubKeys` is
    `newAggregateCommitteePubKeys` in the signed `msg`.

  - `previousMerkleRoot` provided in the redeemer is `previousMerkleRoot` in
    the signed `msg`, and this Merkle root is referenced in this transaction.

![Public key update](Spec/pubkeyupdate.svg)

<figcaption align = "center"><i>Committee handover (updating committee hash)</i></figcaption><br />

#### 6.2. Merkle root chaining

As described in [6. Committee handover](#6-committee-handover), we have to maintain the correct order of Merkle root insertions and committee hash updates. We introduce a new Merkle root chain, where each Merkle root has a reference to its predecessor (if one exists), furthermore all committee hash updates reference the last Merkle root inserted (if one exists).

![Merkle root chaining](Spec/MRChain-simple.svg)

<figcaption align = "center"><i>Merkle root chaining (SC ep = sidechain epoch)</i></figcaption><br />

As seen in the graph above, the first Merkle root has no reference, which is completely valid. We do not enforce the existence of the last Merkle root.

In case a sidechain epoch passed without any cross-chain transactions, no Merkle root is inserted, resulting in two committee hash updates referencing the same Merkle root.

![Merkle root chaining - epoch without Merkle root](Spec/MRChain-empty.svg)

<figcaption align = "center"><i>Merkle root chaining - epoch without Merkle root (SC ep = sidechain epoch)</i></figcaption><br />

In the future, we want to support multiple Merkle roots per sidechain epoch, so the result could look like the following:

![Merkle root chaining - multiple Merkle roots per epoch](Spec/MRChain-multi.svg)

<figcaption align = "center"><i>Merkle root chaining - multiple Merkle roots per epoch (SC ep = sidechain epoch)</i></figcaption><br />

### 7. Checkpointing

**Workflow:**

1. During the handover phase the current committee agrees on a "checkpoint" block
2. Signatures are collected
3. Bridge broadcasts checkpoint block with "save-checkpoint" endpoint

**Endpoint params:**

```haskell
data CheckpointEndpointParam = CheckpointEndpointParam
  { sidechainParams ∷ SidechainParams
  , committeeSignatures ∷ [(SidechainPublicKey, Maybe SidechainSignature)]
  , newCheckpointBlockHash ∷ ByteString
  , newCheckpointBlockNumber ∷ Integer
  , sidechainEpoch ∷ Integer
  }
```

Validator script verifies the following:

- the committee certificate verification minting policy mints with token name
  `tn` which satisfies
  ```
  tn = blake2b_256(cbor(CheckpointMessage))
  ```
  where
  ```haskell
  data CheckpointMessage = CheckpointMessage
    { checkpointMsgSidechainParams :: SidechainParams
    , checkpointMsgBlockHash :: BuiltinByteString
    , checkpointMsgBlockNumber :: Integer
    , checkpointMsgSidechainEpoch :: Integer
    }
  ```
- verifies the oneshot token `CheckpointPolicy` of the UTxO holding the old verification key at the script address
- verifies that the new checkpoint number is strictly increasing
- consumes the above mentioned UTxO
- outputs a new UTxO with the updated checkpoint containing the oneshot token `CheckpointPolicy` to the same script address

**Datum**

```haskell
data CheckpointDatum = CheckpointDatum
  { blockHash ∷ ByteString
  , blockNumber ∷ Integer
  }
```

**Redeemer**

```haskell
data CheckpointRedeemer = CheckpointRedeemer
  { newCheckpointBlockHash ∷ ByteString
  , newCheckpointBlockNumber ∷ Integer
  }
```

### 8. Committee Certificate Minting Policies

A committee certificate minting policy is a minting policy which mints only if
its unique token name has been signed by the current committee with a secure ATMS
(adhoc threshold multisignature) scheme.
More precisely, a _committee certificate verification minting policy_ is a
minting policy with the following workflow.

1. The system is initialized by minting an NFT, `CommitteeOraclePolicy`, and
   paying the NFT to some validator script which as datum holds the current
   committee's aggregated public key as some abstract type `aggregatePubKeys`
   (indeed, the datum may store more than just the current committee such as a
   sidechain epoch).

2. The committee certificate verification minting policy is parameterized
   by: the currency symbol of the previous `CommitteeOraclePolicy`, and some
   fraction `n/d` that denotes the ratio of how many committee members are
   needed to sign a message (more details below).

   The committee certificate verification minting policy mints only if:

   - exactly one token with token name, say `tn`, of the committee certificate
     verification minting policy is minted; and

   - `tn` has _enough_ committee members that have verified `tn` from the
     provided multisignature (as a redeemer) of some abstract type
     `multisignature` where the current committee's `aggregatePubKeys` is
     identified by the NFT `CommitteeOraclePolicy` (in a reference input or
     input).

     The number of _enough_ committee members that have verified `tn` is
     defined as any number of committee members that is
     _strictly larger_ than `size of current committee * n / d` (in the real
     number sense) where `n` and `d` are the `thresholdNumerator` and the
     `thresholdDenominator` given in the `SidechainParams`.

   Note that the committee certificate verification token is not needed after
   this transaction and may be paid to some arbitrary burn address.

The mainchain contracts support the following committee certificate minting
policies.
Note that all committee certificate minting policies must define the following.

- A data type for the aggregated public key of the committee members.

- A data type for the multisignature of the committee members.

- The conditions for which the committee certificate minting policy mints when
  it finds the aggregated public key of committee members (provided as a UTxO
  identified by `CommitteeOraclePolicy` failing otherwise), and when given
  given its multisignature as redeemer.

#### 8.1 Plain ECDSA SECP256k1 Committee Certificate Minting Policy

The plain ECDSA SECP256k1 committee certificate minting policy's aggregated
public key is defined as follows.

```haskell
-- | Invariant: 'ATMSPlainAggregatePubKey' is the sorted concatenated hash of
-- sidechain public keys. More precisely,
-- @
-- committeePubKeys = sort([key1, key2, ..., keyN])
-- committeePubKeysHash = blake2b_256(concat(committeePubKeys))
-- keyi - 33 bytes compressed ECDSA public key of a committee member
-- @
newtype ATMSPlainAggregatePubKey = ATMSPlainAggregatePubKey ByteString
```

The multisignature for the ECDSA SECP256k1 committee certificate minting policy
is defined as follows.

```haskell
-- | Invariant: 'atmsPublicKeys' is sorted lexicographically, and must
-- contain all public keys stored onchain.
-- Moreover, 'atmsSignatures' is a subsequence of the list of all signatures
-- (signing the same message) from 'atmsPublicKeys'.
-- Recall that a compressed 33 byte ECDSA SECP256k1 public key is used, and
-- ECDSA SECP256k1 signatures are 64 bytes.
data ATMSPlainMultisignature = ATMSPlainMultisignature
    { atmsPublicKeys :: [ByteString]
    , atmsSignatures :: [ByteString]
    }
```

Then, the plain ECDSA SECP256k1 committee certificate minting policy mints only
if all of the following are satisfied.

- `tn` is the unique token name minted of the committee certificate minting
  policy.

- The concatenated hash of `atmsPublicKeys` matches the aggregated public key
  stored onchain as identified by `CommitteeOraclePolicy`.

- Enough of the `atmsPublicKeys` with its corresponding signature (from
  `atmsSignatures`) show that `tn` is signed with the ECDSA SECP256k1
  signature.
  Note that this can be an efficient linear scan by the invariants of `ATMSPlainAggregatePubKey`

#### 8.2 Plain Schnorr SECP256k1 Committee Certificate Minting Policy

The plain Schnorr SECP256k1 committee certificate minting policy is identical
to the plain ECDSA SECP256k1 committee certificate minting policy except that
it uses the Schnorr SECP256k1 signatures instead.

The plain Schnorr SECP256k1 committee certificate minting policy's aggregated
public key is defined as follows.

```haskell
-- | Invariant: 'ATMSPlainAggregatePubKey' is the sorted concatenated hash of
-- sidechain public keys. More precisely,
-- @
-- committeePubKeys = sort([key1, key2, ..., keyN])
-- committeePubKeysHash = blake2b_256(concat(committeePubKeys))
-- keyi - 32 bytes Schnorr public key of a committee member
-- @
newtype ATMSPlainAggregatePubKey = ATMSPlainAggregatePubKey ByteString
```

The multisignature for the Schnorr SECP256k1 committee certificate minting policy
is defined as follows.

```haskell
-- | Invariant: 'atmsPublicKeys' is sorted lexicographically, and must
-- contain all public keys stored onchain.
-- Moreover, 'atmsSignatures' is a subsequence of the list of all signatures
-- (signing the same message) from 'atmsPublicKeys'.
-- Recall that a 32 byte Schnorr SECP256k1 public key is used, and Schnorr
-- SECP256k1 signatures are 64 bytes.
data ATMSPlainMultisignature = ATMSPlainMultisignature
    { atmsPublicKeys :: [ByteString]
    , atmsSignatures :: [ByteString]
    }
```

Note that `ATMSPlainAggregatePubKey` and `ATMSPlainMultisignature` are the same
types from the ECDSA SECP256k1 committee certificate minting policy with the
exception of changing the invariants of the byte strings.

The plain ECDSA SECP256k1 committee certificate minting policy mints only
if all of the following are satisfied.

- `tn` is the unique token name minted of the committee certificate minting
  policy.

- The concatenated hash of `atmsPublicKeys` matches the aggregated public key
  stored onchain as identified by `CommitteeOraclePolicy`.

- Enough of the `atmsPublicKeys` with its corresponding signature (from
  `atmsSignatures`) show that `tn` is signed with the Schnorr SECP256k1
  signature.
  Note that this can be an efficient linear scan by the invariants of `ATMSPlainAggregatePubKey`

### 9. Versioning system

There is a versioning system in place that:

1. Acts as an oracle of available validators and policies. These are
   enumerated in the off-chain code by the `ScriptId` data type:

   ```
   data ScriptId
     = FUELMintingPolicy
     | MerkleRootTokenPolicy
     | MerkleRootTokenValidator
     | CommitteeCandidateValidator
     | CandidatePermissionPolicy
     | CommitteeNftPolicy
     | CommitteeHashPolicy
     | CommitteeHashValidator
     | DSKeyPolicy
     | DSConfPolicy
     | DSConfValidator
     | DSInsertValidator
     | CheckpointValidator
     | CheckpointPolicy
     | FUELBurningPolicy
     | VersionOraclePolicy -- not versioned
     | VersionOracleValidator -- not versioned
     | FUELProxyPolicy -- not versioned
     | CommitteeCertificateVerificationPolicy
     | CommitteeOraclePolicy -- (previously UpdateCommitteeHashPolicy)
   ```

2. Allows to update existing scripts to newer versions. In particular, it
   allows to update fuel minting and burning policies in such a way that that
   the FUEL currency symbol does not change.

3. Caches scripts so that they can attached to transactions as reference
   outputs. This is strictly necessary not to exceed transaction size limits.

Each version defines a list of validators and policies to be stored in the
versioning system. During activation phase of sidechain initialization
specified version of the scripts is placed in the versioning system. During
sidechain lifetime it is possible to:

1. Insert new script versions by calling insert version endpoint. For
   example, if sidechain was initialized with version 1 of the scripts it is
   then possible to insert version 2 of the scripts and both version will
   exist in parallel. This can be used to allow claiming FUEL using several
   different versions of the scripts.

2. Invalidate existing script versions. Scripts from invalidated version will
   no longer be available. This can be used to disallow claiming FUEL using a
   particular script version.

3. Update existing scripts to new versions. This combines invalidation of an
   existing version and insertion of a new one, possibly re-using existing
   versioning tokens or burning excessive remaining ones.

Note that none of the above three operations is atomic because
insertion/invalidation/update of each scripts happens in a separate transaction
(again, due to transaction size limits). This could potentially lead to
problems if user claims FUEL while an update is performed. Resolving this is
part of future work.

All versioning operations must be approved by the [governance
authority](./Governance.md). The only exception to this rule is minting initial
versioning tokens during sidechain initialization, in which case burning of
genesis UTxO is required.

Every on-chain script that relies on other currency symbols or validator
addresses must be parameterized with versioning system configuration that stores
information which `VersionOracleValidator` address and `VersionOraclePolicy`
currency symbol are to be considered as trusted:

```
data VersionOracleConfig = VersionOracleConfig
  { -- | VersionOracleValidator
    versionOracleAddress :: Address
  , -- | VersionOraclePolicy
    versionOracleCurrencySymbol :: CurrencySymbol
  }
```
