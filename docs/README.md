# Trustless sidechain

## Cardano specification

### Initialise sidechain

Mainchain utilizes three components to handle interactions with a sidechain:

- minting policy validating the mint or burn of FUEL tokens on mainchain
- script address for block producer candidates
- script address for the ATMS verification key

All of these policies/validators are parameterised by the sidechain parameters.

```haskell
data SidechainParams = SidechainParams
  { chainId :: BuiltinInteger
  }
```

TODO: might need to add other information

### Transfer FUEL tokens from mainchain to sidechain

**Workflow:**

1. Call the burn endpoint of the contract with BurnParams
2. A transaction will be submitted to mainchain with the burnt amount in the tx body and the sidechain recipient in the redeemer
3. The Bridge component observing the mainchain can verify the transaction by direct observation and create an appropriate sidechain transaction

**Endpoint params:**

```haskell
data BurnParams = BurnParams
  { recipient :: ByteString
  , amount :: Integer
  }
```

![MC to SC](MC-SC.svg)

### Transfer FUEL tokens from sidechain to mainchain

1. Sidechain transaction settled
2. Sidechain block producers serialise and sign the CrossChainTx with ATMS threshold multisignature
3. Bridge component submits the Cardano transaction with the minted amount in the tx body, and the ATMS signature as the redeemer
4. Minting policy verifies the following:

- signature can be verified using the ATMS verification key (as a reference input)
- chainId matches the minting policy chainId
- recipient in the CrossChainTx matches that of the actual tx body
- minted amount in the CrossChainTx matches that of the actual tx body
- inputUtxo is consumed (this is in order to avoid replay attacks)

**Cross chain transaction (serialised as CBOR and signed by ATMS):**

```haskell
data CrossChainTx = CrossChainTx
  { chainId :: Integer
  , amount :: Integer
  , recipient :: ByteString
  , inputUtxo :: TxOutRef
  }
```

**Endpoint params:**

```haskell
data MintParams = MintParams
  { recipient :: Address,
  , amount :: Integer
  , signature :: ByteString
  }
```

![SC to MC](SC-MC.svg)

As in both minting and burning scenarios the expected type of the redeemer is ByteString, we don't use sum type to distinguish them so we can save space. If the minting amout is positive, we expect signature, otherwise a recipient address.

### Register block producer candidate

1. An SPO registering as a block producer for the sidechain sends BlockProducerRegistration and its signature (TODO: how and what to sign)
2. The Bridge monitoring the script address is validating the SPO credentials, chainId
3. UTxOs at this script address can only be unlocked by the PubKey in the datum script sender (or we could make it an `alwaysFail` script)

**Datum:**

```haskell
data BlockProducerRegistration = BlockProducerRegistration
  { pubKey :: PubKey
  }
```

TODO: might need to add other information

### Update ATMS verification key

1. Bridge component triggers the Cardano transaction. This tx does the following:

- verifies the signature on the new ATMS key (must be signed by the old block producers)
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

## Failure scenarios

In case the ATMS verification key is changed while the transaction is being processed, a transaction with the old signature could not be verified. In this scenario, the cross-chain transaction request must be reinitilised with a new signature.
