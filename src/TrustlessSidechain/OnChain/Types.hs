{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TrustlessSidechain.OnChain.Types where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.String (IsString)
import GHC.Generics (Generic)
import Ledger (PaymentPubKeyHash)
import Ledger.Crypto (PubKey, PubKeyHash, Signature)
import Ledger.Typed.Scripts (ValidatorTypes (..))
import Ledger.Value (AssetClass, CurrencySymbol, TokenName)
import Plutus.V2.Ledger.Api (LedgerBytes (LedgerBytes))
import Plutus.V2.Ledger.Tx (TxOutRef)
import PlutusTx (FromData, ToData, UnsafeFromData, makeIsDataIndexed, makeLift)
import PlutusTx.Prelude hiding (Semigroup ((<>)))
import Schema (
  ToSchema,
 )
import TrustlessSidechain.MerkleTree (MerkleProof)
import Prelude qualified

newtype GenesisHash = GenesisHash {getGenesisHash :: BuiltinByteString}
  deriving (IsString, Prelude.Show) via LedgerBytes
  deriving stock (Generic)
  deriving newtype (Prelude.Eq, Prelude.Ord, Eq, Ord, ToData, FromData, UnsafeFromData)
  deriving anyclass (NFData, ToSchema)

makeLift ''GenesisHash

$(deriveJSON defaultOptions ''GenesisHash)

-- | 'SidechainPubKey' is compressed DER Secp256k1 public key.
newtype SidechainPubKey = SidechainPubKey {getSidechainPubKey :: BuiltinByteString}
  deriving (IsString, Prelude.Show) via LedgerBytes
  deriving stock (Generic)
  deriving newtype (Prelude.Eq, Prelude.Ord, Eq, Ord, ToData, FromData, UnsafeFromData)
  deriving anyclass (NFData, ToSchema)

makeLift ''SidechainPubKey

$(deriveJSON defaultOptions ''SidechainPubKey)

-- | Parameters to initialize a sidechain
data InitSidechainParams = InitSidechainParams
  { initChainId :: Integer
  , initGenesisHash :: GenesisHash
  , -- | 'initUtxo ' is a 'TxOutRef' used for creating 'AssetClass's for the
    -- internal function of the side chain (e.g. InitCommitteeHashMint TODO: hyperlink this documentation)
    initUtxo :: TxOutRef
  , -- | 'initCommittee' is the initial committee of the sidechain
    initCommittee :: [PubKey]
  , initMint :: Maybe TxOutRef
  , -- | See 'thresholdNumerator'
    initThresholdNumerator :: Integer
  , -- | See 'thresholdDenominator'
    initThresholdDenominator :: Integer
  }
  deriving stock (Prelude.Show, Generic)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''InitSidechainParams)
PlutusTx.makeLift ''InitSidechainParams
PlutusTx.makeIsDataIndexed ''InitSidechainParams [('InitSidechainParams, 0)]

-- | Parameters uniquely identifying a sidechain
data SidechainParams = SidechainParams
  { chainId :: Integer
  , genesisHash :: GenesisHash
  , -- | Any random UTxO to prevent subsequent minting for the oneshot minting policy.
    -- @Just utxo@ denotes that we will use the oneshot minting policy, and @Nothing@
    -- will use the distributed set implementation.
    genesisMint :: Maybe TxOutRef
  , -- | 'genesisUtxo' is a 'TxOutRef' used to initialize the internal
    -- policies in the side chain (e.g. for the 'UpdateCommitteeHash' endpoint)
    genesisUtxo :: TxOutRef
  , -- | 'thresholdNumerator' is the numerator for the ratio of the committee
    -- needed to sign off committee handovers / merkle roots
    thresholdNumerator :: Integer
  , -- | 'thresholdDenominator' is the denominator for the ratio of the
    -- committee needed to sign off committee handovers / merkle roots
    thresholdDenominator :: Integer
  }
  deriving stock (Prelude.Show, Generic)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''SidechainParams)
PlutusTx.makeLift ''SidechainParams
PlutusTx.makeIsDataIndexed ''SidechainParams [('SidechainParams, 0)]

{- | Parameters uniquely identifying a sidechain, used only in the block producer registration
 TODO: This type has to be removed, when we deprecate Passive Bridge functionality
-}
data SidechainParams' = SidechainParams'
  { chainId :: Integer
  , genesisHash :: GenesisHash
  , -- @Just utxo@ denotes that we will use the oneshot minting policy, and @Nothing@
    -- will use the distributed set implementation.

    -- | 'genesisUtxo' is a 'TxOutRef' used to initialize the internal
    -- policies in the side chain (e.g. for the 'UpdateCommitteeHash' endpoint)
    genesisUtxo :: TxOutRef
  , -- | 'thresholdNumerator' is the numerator for the ratio of the committee
    -- needed to sign off committee handovers / merkle roots
    thresholdNumerator :: Integer
  , -- | 'thresholdDenominator' is the denominator for the ratio of the
    -- committee needed to sign off committee handovers / merkle roots
    thresholdDenominator :: Integer
  }
  deriving stock (Prelude.Show, Generic)

PlutusTx.makeLift ''SidechainParams'
PlutusTx.makeIsDataIndexed ''SidechainParams' [('SidechainParams', 0)]

-- | Convert SidechainParams to the Active Bridge version
convertSCParams :: SidechainParams -> SidechainParams'
convertSCParams (SidechainParams i g _ u numerator denominator) =
  SidechainParams' i g u numerator denominator

-- | Endpoint parameters for committee candidate registration
data RegisterParams = RegisterParams
  { sidechainParams :: SidechainParams
  , spoPubKey :: PubKey
  , sidechainPubKey :: SidechainPubKey
  , spoSig :: Signature
  , sidechainSig :: Signature
  , inputUtxo :: TxOutRef
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''RegisterParams)

-- | Endpoint parameters for committee candidate deregistration
data DeregisterParams = DeregisterParams
  { sidechainParams :: SidechainParams
  , spoPubKey :: PubKey
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''DeregisterParams)

data BurnParams = BurnParams
  { -- | Burnt amount in FUEL (Negative)
    amount :: Integer
  , -- | SideChain address
    recipient :: BuiltinByteString
  , -- | passed for parametrization
    sidechainParams :: SidechainParams
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''BurnParams)

data MintParams = MintParams
  { -- | Minted amount in FUEL (this should be positive)
    amount :: Integer
  , -- | MainChain address
    recipient :: PaymentPubKeyHash
  , -- | the merkle proof to prove to the mainchain that the given unhandled transaction from the sidechain actually happened on the sidechain
    merkleProof :: MerkleProof
  , --  | passed for parametrization.
    -- TODO: in the spec, we don't do this -- we just pass the chainId. Not sure if this is what we want?
    sidechainParams :: SidechainParams
  , -- | See 'TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy.MerkleTreeEntry' for why 'index' is here
    index :: Integer
  , -- | See 'TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy.MerkleTreeEntry' for why 'sidechainEpoch' is here
    sidechainEpoch :: Integer
  , -- | See
    -- 'TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy.MerkleTreeEntry' for
    -- why 'entryHash' is here. This is TODO and will be removed later
    entryHash :: BuiltinByteString
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''MintParams)

{- | Endpoint parameters for committee candidate hash updating

 TODO: it might not be a bad idea to factor out the 'signature' and
 'committeePubKeys' field shared by 'UpdateCommitteeHashParams' and
 'SaveRootParams' in a different data type. I'd imagine there will be lots of
 duplicated code when it comes to verifying that the committee has approved
 of these transactions either way.
-}
data UpdateCommitteeHashParams = UpdateCommitteeHashParams
  { -- | See 'SidechainParams'.
    sidechainParams :: SidechainParams
  , -- | The public keys of the new committee.
    newCommitteePubKeys :: [PubKey]
  , -- | The signature for the new committee hash.
    committeeSignatures :: [BuiltinByteString]
  , -- | Public keys of the current committee members.
    committeePubKeys :: [PubKey]
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''UpdateCommitteeHashParams)

data SaveRootParams = SaveRootParams
  { sidechainParams :: SidechainParams
  , merkleRoot :: BuiltinByteString
  , signatures :: [BuiltinByteString]
  , lastMerkleRoot :: Maybe BuiltinByteString
  , committeePubKeys :: [PubKey] -- Public keys of all committee members
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)
$(deriveJSON defaultOptions ''SaveRootParams)

{- | 'MerkleTreeEntry' (abbr. mte and pl. mtes) is the data which are the elements in the merkle tree
 for the MPTRootToken.
-}
data MerkleTreeEntry = MerkleTreeEntry
  { -- | 32 bit unsigned integer, used to provide uniqueness among transactions within the tree
    mteIndex :: Integer
  , -- | 256 bit unsigned integer that represents amount of tokens being sent out of the bridge
    mteAmount :: Integer
  , -- | arbitrary length bytestring that represents decoded bech32 cardano
    -- address. See [here](https://cips.cardano.org/cips/cip19/) for more details
    -- of bech32
    mteRecipient :: BuiltinByteString
  , -- | the previous merkle root to ensure that the hashed entry is unique
    mtePreviousMerkleRoot :: Maybe BuiltinByteString
  }

makeIsDataIndexed ''MerkleTreeEntry [('MerkleTreeEntry, 0)]

{- | 'MerkleRootInsertionMessage' is a data type for which committee members
 create signatures for
 >  blake2b(cbor(MerkleRootInsertionMessage))
-}
data MerkleRootInsertionMessage = MerkleRootInsertionMessage
  { mrimSidechainParams :: SidechainParams'
  , mrimMerkleRoot :: BuiltinByteString
  , mrimPreviousMerkleRoot :: Maybe BuiltinByteString
  }

makeIsDataIndexed ''MerkleRootInsertionMessage [('MerkleRootInsertionMessage, 0)]

-- | The Redeemer that's to be passed to onchain policy, indicating its mode of usage.
data FUELRedeemer
  = MainToSide BuiltinByteString -- Recipient's sidechain address
  | -- | 'SideToMain' indicates that we wish to mint FUEL on the mainchain.
    -- So, this includes which transaction in the sidechain we are
    -- transferring over to the main chain (hence the 'MerkleTreeEntry'), and
    -- the proof tha this actually happened on the sidechain (hence the
    -- 'MerkleProof')
    SideToMain MerkleTreeEntry MerkleProof

data BlockProducerRegistration = BlockProducerRegistration
  { -- | SPO cold verification key hash
    bprSpoPubKey :: PubKey -- own cold verification key hash
  , -- | public key in the sidechain's desired format
    bprSidechainPubKey :: SidechainPubKey
  , -- | Signature of the SPO
    bprSpoSignature :: Signature
  , -- | Signature of the SPO
    bprSidechainSignature :: Signature
  , -- | A UTxO that must be spent by the transaction
    bprInputUtxo :: TxOutRef
  , -- | Owner public key hash
    bprOwnPkh :: PubKeyHash
  }
  deriving stock (Prelude.Show)

PlutusTx.makeIsDataIndexed ''BlockProducerRegistration [('BlockProducerRegistration, 0)]

data BlockProducerRegistrationMsg = BlockProducerRegistrationMsg
  { bprmSidechainParams :: SidechainParams'
  , bprmSidechainPubKey :: SidechainPubKey
  , -- | A UTxO that must be spent by the transaction
    bprmInputUtxo :: TxOutRef
  }
  deriving stock (Prelude.Show)

PlutusTx.makeIsDataIndexed ''BlockProducerRegistrationMsg [('BlockProducerRegistrationMsg, 0)]

data CommitteeCandidateRegistry
instance ValidatorTypes CommitteeCandidateRegistry where
  type RedeemerType CommitteeCandidateRegistry = ()
  type DatumType CommitteeCandidateRegistry = BlockProducerRegistration

-- Recipient address is in FUELRedeemer just for reference on the mainchain,
-- it's actually useful (and verified) on the sidechain, so it needs to be
-- recorded in the mainchain.

PlutusTx.makeIsDataIndexed ''FUELRedeemer [('MainToSide, 0), ('SideToMain, 1)]

instance ValidatorTypes FUELRedeemer where
  type RedeemerType FUELRedeemer = FUELRedeemer

{- | Datum for the committee hash. This /committee hash/ is used to verify
 signatures for sidechain to mainchain transfers. This is a hash of
 concatenated public key hashes of the committee members
-}
data UpdateCommitteeHashDatum = UpdateCommitteeHashDatum
  { committeeHash :: BuiltinByteString
  , sidechainEpoch :: Integer
  }

instance Eq UpdateCommitteeHashDatum where
  {-# INLINEABLE (==) #-}
  UpdateCommitteeHashDatum cmtHsh epoch == UpdateCommitteeHashDatum cmtHsh' epoch' =
    cmtHsh == cmtHsh' && epoch == epoch'

PlutusTx.makeIsDataIndexed ''UpdateCommitteeHashDatum [('UpdateCommitteeHashDatum, 0)]

{- | The Redeemer that is passed to the on-chain validator to update the
 committee
-}
data UpdateCommitteeHashRedeemer = UpdateCommitteeHashRedeemer
  { -- | The current committee's signatures for the @'aggregateKeys' 'newCommitteePubKeys'@
    committeeSignatures :: [BuiltinByteString]
  , -- | 'committeePubKeys' is the current committee public keys
    committeePubKeys :: [SidechainPubKey]
  , -- | 'newCommitteePubKeys' is the hash of the new committee
    newCommitteePubKeys :: [SidechainPubKey]
  , -- | 'previousMerkleRoot' is the previous merkle root (if it exists)
    previousMerkleRoot :: Maybe BuiltinByteString
  }

PlutusTx.makeIsDataIndexed ''UpdateCommitteeHashRedeemer [('UpdateCommitteeHashRedeemer, 0)]

{- | 'UpdatingCommitteeHash' is the type to associate the 'DatumType' and
 'RedeemerType' to the acutal types used at run time.
-}
data UpdatingCommitteeHash

instance ValidatorTypes UpdatingCommitteeHash where
  type DatumType UpdatingCommitteeHash = UpdateCommitteeHashDatum
  type RedeemerType UpdatingCommitteeHash = UpdateCommitteeHashRedeemer

-- | 'UpdateCommitteeHash' is used as the parameter for the validator.
data UpdateCommitteeHash = UpdateCommitteeHash
  { cSidechainParams :: SidechainParams
  , -- | 'cToken' is the 'AssetClass' of the NFT that is used to
    -- identify the transaction.
    cToken :: AssetClass
  , -- | 'cMptRootTokenCurrencySymbol' is the currency symbol of the corresponding mpt
    -- root token. This is needed for verifying that the previous merkle root is verified.
    cMptRootTokenCurrencySymbol :: CurrencySymbol
  }
  deriving stock (Prelude.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''UpdateCommitteeHash
PlutusTx.makeIsDataIndexed ''UpdateCommitteeHash [('UpdateCommitteeHash, 0)]

data UpdateCommitteeHashMessage = UpdateCommitteeHashMessage
  { uchmSidechainParams :: SidechainParams'
  , -- | 'newCommitteePubKeys' is the new committee public keys and _should_
    -- be sorted lexicographically (recall that we can trust the bridge, so it
    -- should do this for us
    uchmNewCommitteePubKeys :: [SidechainPubKey]
  , uchmPreviousMerkleRoot :: Maybe BuiltinByteString
  , uchmSidechainEpoch :: Integer
  }
PlutusTx.makeLift ''UpdateCommitteeHashMessage
PlutusTx.makeIsDataIndexed ''UpdateCommitteeHashMessage [('UpdateCommitteeHashMessage, 0)]

-- | 'GenesisMintCommitteeHash' is used as the parameter for the minting policy
data GenesisMintCommitteeHash = GenesisMintCommitteeHash
  { -- | 'gcToken' is the token name of the NFT to start the committee hash
    gcToken :: TokenName
  , -- | 'TxOutRef' is the output reference to mint the NFT initially.
    gcTxOutRef :: TxOutRef
  }
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''GenesisMintCommitteeHash

-- | 'SignedMerkleRoot' is the redeemer for the MPT root token minting policy
data SignedMerkleRoot = SignedMerkleRoot
  { -- | New merkle root to insert.
    merkleRoot :: BuiltinByteString
  , -- | Previous merkle root (if it exists)
    previousMerkleRoot :: Maybe BuiltinByteString
  , -- | Current committee signatures ordered as their corresponding keys
    signatures :: [BuiltinByteString]
  , -- | Lexicographically sorted public keys of all committee members
    committeePubKeys :: [SidechainPubKey]
  }

PlutusTx.makeIsDataIndexed ''SignedMerkleRoot [('SignedMerkleRoot, 0)]

instance ValidatorTypes SignedMerkleRoot where
  type RedeemerType SignedMerkleRoot = SignedMerkleRoot

{- | 'CombinedMerkleProof' is a product type to include both the
 'MerkleTreeEntry' and the 'MerkleProof'.

 This exists as for testing in #249.
-}
data CombinedMerkleProof = CombinedMerkleProof
  { cmpTransaction :: MerkleTreeEntry
  , cmpMerkleProof :: MerkleProof
  }

PlutusTx.makeIsDataIndexed ''CombinedMerkleProof [('CombinedMerkleProof, 0)]
