# Trustless sidechain

## Cardano specification

### Initialise sidechain

Mainchain utilizes the following components to handle interactions with a sidechain:

- `FUELMintingPolicy`: minting policy validating the mint or burn of FUEL tokens on mainchain
- `MPTRootTokenMintingPolicy`: minting policy for storing cross-chain transaction bundles' MPT roots
- `CommitteeCandidateValidator`: script address for committee candidates
- `MPTRootTokenValidator`: script address for storing `MPTRootToken`s
- `ATMSVerificationKeyValidator`: script address for the ATMS verification key

All of these policies/validators are parameterised by the sidechain parameters, so we can get unique minting policy and validator script hashes.

```haskell
data SidechainParams = SidechainParams
  { chainId :: BuiltinInteger -- TODO: do we need anything else here?
  }
```

### Transfer FUEL tokens from mainchain to sidechain

**Workflow:**

1. Call the burn endpoint of the contract with BurnParams
2. A transaction will be submitted to mainchain with the burnt amount in the tx body and the sidechain recipient in the redeemer
3. The Bridge component observing the mainchain where the given minting policy is handled, verifies the transaction and creates an appropriate sidechain transaction

**Endpoint params:**

```haskell
data BurnParams = BurnParams
  { recipient :: ByteString
  , amount :: Integer
  }
```

![MC to SC](MC-SC.svg)

### Transfer FUEL tokens from sidechain to mainchain

**Workflow:**

1. Sidechain collects unhandled transactions
2. Sidechain block producers compute `txs = outgoing_txs.map(tx => blake2(tx.recipient, tx.amount)` for each transaction, and create a Merkle-tree from these. The root of this tree is signed with ATMS multisig
3. Bridge broadcasts Merkle root to chain
4. Txs can be claimed individually

**Endpoint params for merkle root insertion:**

Merkle roots are stored on-chain, using `MPTRootToken`s, where the `tokenName` is the Merkle root. These tokens must be at the `MPTRootTokenValidator` script address.

```haskell
data SignedMerkleRoot = SignedMerkleRoot
  { merkleRoot :: ByteString
  , signature :: ByteString
  }
```

Minting policy verifies the following:

- signature can be verified with the ATMS verification key

Validator script verifies the following:

- UTxOs containing an `MPTRootToken` cannot be unlocked from the script address

![MPTRootToken minting](MPTRoot.svg)

**Endpoint params for claiming:**

```haskell
data MintParams = MintParams
  { amount :: Integer
  , recipient :: ByteString
  , merkleProof :: MerkleProof
  , chainId :: Integer
  }
```

Minting policy verifies the following:

- `MPTRootToken` with the name of the Merkle root of the transaction (calculated from from the proof) can be found in the `MPTRootTokenValidator` script address
- chainId matches the minting policy chainId
- recipient and amount matches the actual tx body contents
- the merkleRoot where the transaction is in, and it's position in the list hashed `blake2(merkleRoot, txIdx)` of the transaction is NOT included in the distributed set[^1] (the actual hash might be subject to change)
- a new entry with the value of `blake2(tx.recipient, tx.amount, merkleRoot)` is created in the distributed set

![SC to MC](SC-MC.svg)

**Minting policy redeemer:**

```haskell
data FUELRedeemer
  = MainToSide ByteString -- Recipient address of the sidechain
  | SideToMain MerkleProof
```

ByteString (if the minting amout is positive, we expect this to be a signature, otherwise a recipient address)

### Register committee candidate

**Workflow:**

1. An SPO registering as a block producer (commitee member) for the sidechain sends BlockProducerRegistration and its signature
2. The Bridge monitoring the committee candidate script address is validating the SPO credentials, chainId

**Datum:**

```haskell
data BlockProducerRegistration = BlockProducerRegistration
  { pubKey :: PubKey -- own public key
  , sidechainPubKey :: ByteString -- public key in the sidechain's desired format
  , signature :: Credentials -- TODO: what signature we need exactly
  }
```

### Deregister committee member/candidate

**Workflow:**

1. The UTxO with the registration information can be redeemed by the original sender
2. The Bridge monitoring the committee candidate script address interprets this as a deregister action

### Update ATMS verification key

1. Bridge component triggers the Cardano transaction. This tx does the following:

- verifies the signature on the new ATMS key (must be signed by the old committee)
- verifies the NFT of the UTxO holding the old verification key at the script address
- consumes the above mentioned UTxO
- outputs a new UTxO with the updated ATMS key containing the NFT to the same script address

```haskell
data UpdateVKey = UpdateVKey
  { newVKey :: ByteString,
  , signature :: ByteString
  }
```

![Public key update](pubkeyupdate.svg)

## Appendix

[^1]: Distributed set implementation details are still WIP, but we plan to use something like this: https://github.com/Plutonomicon/plutonomicon/blob/main/stick-breaking-set.md
