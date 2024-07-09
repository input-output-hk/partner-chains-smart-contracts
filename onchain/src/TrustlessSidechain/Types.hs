{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.Types (
  ATMSPlainAggregatePubKey (..),
  ATMSPlainMultisignature (..),
  ATMSRedeemer (..),
  BlockProducerRegistration (..),
  BlockProducerRegistrationMsg (..),
  CheckpointDatum (..),
  CheckpointMessage (..),
  CheckpointParameter (..),
  CombinedMerkleProof (..),
  CommitteeCertificateMint (..),
  DParameterValidatorDatum (..),
  EcdsaSecp256k1PubKey (..),
  FUELMintingRedeemer (..),
  GovernanceAuthority (GovernanceAuthority),
  InitTokenAssetClass (..),
  InitTokenRedeemer (..),
  MerkleRootInsertionMessage (..),
  MerkleTreeEntry (..),
  PermissionedCandidateKeys (..),
  PermissionedCandidatesPolicyRedeemer (..),
  PermissionedCandidatesValidatorDatum (..),
  PermissionedCandidatesValidatorRedeemer (..),
  PubKey (..),
  SidechainParams (..),
  Signature (..),
  SignedMerkleRootRedeemer (..),
  StakeOwnership (..),
  UpdateCommitteeDatum (..),
  UpdateCommitteeHashMessage (..),
  UpdateCommitteeHashRedeemer (..),
  ImmutableReserveSettings (..),
  MutableReserveSettings (..),
  ReserveStats (..),
  ReserveDatum (..),
  ReserveRedeemer (..),
  IlliquidCirculationSupplyRedeemer (..),
) where

import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (AssetClass)
import Plutus.V2.Ledger.Api (
  BuiltinData (BuiltinData),
  CurrencySymbol,
  LedgerBytes (LedgerBytes),
  POSIXTime,
  TokenName,
  TxOutRef,
  ValidatorHash,
 )
import PlutusTx (makeIsDataIndexed)
import PlutusTx qualified
import TrustlessSidechain.HaskellPrelude qualified as TSPrelude
import TrustlessSidechain.MerkleTree (MerkleProof)
import TrustlessSidechain.PlutusPrelude

-- Note [Roundtrip tests]
-- ~~~~~~~~~~~~~~~~~~~~~~
--
-- Whenever new definitions in this module are added, i.e. either a new data
-- type or a new data constructor is created, corresponding roundtrip tests need
-- to be updated.  These tests reside in Test.TrustlessSidechain.Types module in
-- `tests/` directory and test correctness of serialization and deserialization
-- using `ToData` and `FromData` instances.  Importantly, same tests are
-- performed in the substrate-node to ensure interoperability with
-- trustless-sidechain.

-- * Sidechain Parametrization and general data

newtype GovernanceAuthority = GovernanceAuthority PubKeyHash
  deriving newtype (TSPrelude.Eq, TSPrelude.Ord, TSPrelude.Show, ToData, FromData, UnsafeFromData)

PlutusTx.makeLift ''GovernanceAuthority

-- | Parameters uniquely identifying a sidechain
--
-- = Note
--
-- The 'Data' serializations for this type /cannot/ change.
data SidechainParams = SidechainParams
  { chainId :: Integer
  , -- | 'genesisUtxo' is a 'TxOutRef' used to initialize the internal
    -- policies in the side chain (e.g. for the 'UpdateCommitteeHash' endpoint)
    genesisUtxo :: TxOutRef
  , -- | 'thresholdNumerator' is the numerator for the ratio of the committee
    -- needed to sign off committee handovers / merkle roots
    thresholdNumerator :: Integer
  , -- | 'thresholdDenominator' is the denominator for the ratio of the
    -- committee needed to sign off committee handovers / merkle roots
    thresholdDenominator :: Integer
  , -- | 'governanceAuthority' stores credentials of a governing body allowed to
    -- make updates to versioned scripts.  For now we just use a master public
    -- key, whose owner is allowed to make any decisions about script versions.
    --
    -- @since v5.0.0
    governanceAuthority :: GovernanceAuthority
  }
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

PlutusTx.makeLift ''SidechainParams
PlutusTx.makeIsDataIndexed ''SidechainParams [('SidechainParams, 0)]

-- | @since v4.0.0
makeHasField ''SidechainParams

-- | Compressed DER SECP256k1 public key.
-- = Important note
--
-- The 'Data' serializations for this type /cannot/ change.
newtype EcdsaSecp256k1PubKey = EcdsaSecp256k1PubKey
  { -- | @since v4.0.0
    getEcdsaSecp256k1PubKey :: LedgerBytes
  }
  deriving stock (TSPrelude.Eq, TSPrelude.Ord)
  deriving newtype
    ( Eq
    , Ord
    , ToData
    , FromData
    , UnsafeFromData
    )
  deriving
    ( -- | @since v4.0.0
      IsString
    , TSPrelude.Show
    )
    via LedgerBytes

-- | @since v5.0.0
makeHasField ''EcdsaSecp256k1PubKey

-- | Ed25519 public key
-- = Important note
--
-- The 'Data' serializations for this type /cannot/ change.
newtype PubKey = PubKey
  -- TODO: rename to Ed25519PubKEy
  { -- | @since v4.0.0
    getPubKey :: LedgerBytes
  }
  deriving stock (TSPrelude.Eq, TSPrelude.Ord)
  deriving newtype
    ( Eq
    , Ord
    , ToData
    , FromData
    , UnsafeFromData
    )
  deriving
    ( -- | @since v4.0.0
      IsString
    , TSPrelude.Show
    )
    via LedgerBytes

-- | Ed25519 signature
-- = Important note
--
-- The 'Data' serializations for this type /cannot/ change.
newtype Signature = Signature
  -- TODO: rename to Ed25519Signature
  { -- | @since v4.0.0
    getSignature :: LedgerBytes
  }
  deriving stock (TSPrelude.Eq, TSPrelude.Ord)
  deriving newtype
    ( Eq
    , Ord
    , ToData
    , FromData
    , UnsafeFromData
    )
  deriving
    ( -- | @since v4.0.0
      IsString
    , TSPrelude.Show
    )
    via LedgerBytes

-- * Committee Candidate Validator data

{- Sum type distinguishing different Stake ownership models
 Ada based staking requires the SPO public key and the signature on
 the @BlockProducerRegistrationMsg@, while a native token based staking model
 only requires the own Cardano payment public key hash

-}
data StakeOwnership
  = -- | Ada stake based configuration comprises the SPO public key and signature
    AdaBasedStaking PubKey Signature
  | -- | Token based staking configuration
    TokenBasedStaking
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

PlutusTx.makeIsDataIndexed
  ''StakeOwnership
  [ ('AdaBasedStaking, 0)
  , ('TokenBasedStaking, 1)
  ]

{-
 The 'Data' serializations for this type /cannot/ change.
-}
data BlockProducerRegistration = BlockProducerRegistration
  { -- | Verification keys required by the stake ownership model
    -- | @since v4.0.0
    stakeOwnership :: StakeOwnership
  , -- | public key in the sidechain's desired format
    sidechainPubKey :: LedgerBytes
  , -- | Signature of the sidechain
    -- | @since v4.0.0
    sidechainSignature :: Signature
  , -- | A UTxO that must be spent by th@ext:haskell.haskelltransaction
    -- | @since v4.0.0
    inputUtxo :: TxOutRef
  , -- | Owner public key hash
    -- | @since v4.0.0
    ownPkh :: PubKeyHash
  , -- | Sidechain authority discovery key
    -- | @since v5.0.0
    auraKey :: LedgerBytes
  , -- | Sidechain grandpa key
    -- | @since v5.0.0
    grandpaKey :: LedgerBytes
  }
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

PlutusTx.makeIsDataIndexed ''BlockProducerRegistration [('BlockProducerRegistration, 0)]

-- | @since v4.0.0
makeHasField ''BlockProducerRegistration

-- | = Important note
--
-- The 'Data' serializations for this type /cannot/ change.
data BlockProducerRegistrationMsg = BlockProducerRegistrationMsg
  { sidechainParams :: SidechainParams
  , sidechainPubKey :: LedgerBytes
  , -- | A UTxO that must be spent by the transaction
    -- | @since v4.0.0
    inputUtxo :: TxOutRef
  }
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

PlutusTx.makeIsDataIndexed ''BlockProducerRegistrationMsg [('BlockProducerRegistrationMsg, 0)]

-- | @since v4.0.0
makeHasField ''BlockProducerRegistrationMsg

-- * Merkle Root Token data

-- | 'MerkleTreeEntry' (abbr. mte and pl. mtes) is the data which are the elements in the merkle tree
-- for the MerkleRootToken.
--
-- = Important note
--
-- The 'Data' serializations for this type /cannot/ change.
data MerkleTreeEntry = MerkleTreeEntry
  { -- | 32 bit unsigned integer, used to provide uniqueness among transactions within the tree
    -- | @since v4.0.0
    index :: Integer
  , -- | 256 bit unsigned integer that represents amount of tokens being sent out of the bridge
    -- | @since v4.0.0
    amount :: Integer
  , -- | arbitrary length bytestring that represents decoded bech32 cardano
    -- | address. See [here](https://cips.cardano.org/cips/cip19/) for more details
    -- | of bech32
    -- | @since v4.0.0
    recipient :: LedgerBytes
  , -- | the previous merkle root to ensure that the hashed entry is unique
    -- | @since v4.0.0
    previousMerkleRoot :: Maybe LedgerBytes
  }
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

PlutusTx.makeIsDataIndexed ''MerkleTreeEntry [('MerkleTreeEntry, 0)]

-- | @since v4.0.0
makeHasField ''MerkleTreeEntry

-- | 'MerkleRootInsertionMessage' is a data type for which committee members
-- create signatures for
-- >  blake2b(cbor(MerkleRootInsertionMessage))
--
-- = Important note
--
-- The 'Data' serializations for this type /cannot/ change.
data MerkleRootInsertionMessage = MerkleRootInsertionMessage
  { -- | @since v4.0.0
    sidechainParams :: SidechainParams
  , -- | @since v4.0.0
    merkleRoot :: LedgerBytes
  , -- | @since v4.0.0
    previousMerkleRoot :: Maybe LedgerBytes
  }
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

PlutusTx.makeIsDataIndexed ''MerkleRootInsertionMessage [('MerkleRootInsertionMessage, 0)]

-- | @since v4.0.0
makeHasField ''MerkleRootInsertionMessage

-- | 'SignedMerkleRootRedeemer' is the redeemer for the signed merkle root
-- minting policy.
--
-- @since v4.0.0
newtype SignedMerkleRootRedeemer = SignedMerkleRootRedeemer
  { previousMerkleRoot :: Maybe LedgerBytes
  }
  deriving newtype
    ( -- | @since v4.0.0
      ToData
    , -- | @since v4.0.0
      FromData
    , -- | @since v4.0.0
      UnsafeFromData
    )
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

-- | @since v5.0.0
makeHasField ''SignedMerkleRootRedeemer

-- | 'CombinedMerkleProof' is a product type to include both the
-- 'MerkleTreeEntry' and the 'MerkleProof'.
--
-- This exists as for testing in #249.
--
-- = Important note
--
-- The 'Data' serializations of this type /cannot/ change.
data CombinedMerkleProof = CombinedMerkleProof
  { -- | @since v4.0.0
    transaction :: MerkleTreeEntry
  , -- | @since v4.0.0
    merkleProof :: MerkleProof
  }
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

PlutusTx.makeIsDataIndexed ''CombinedMerkleProof [('CombinedMerkleProof, 0)]

-- | @since v4.0.0
makeHasField ''CombinedMerkleProof

-- * FUEL Minting Policy data

-- | 'FUELMintingRedeemer' indicates that we wish to mint FUEL on the mainchain.
-- So, this includes which transaction in the sidechain we are transferring over
-- to the main chain (hence the 'MerkleTreeEntry'), and the proof that this
-- actually happened on the sidechain (hence the 'MerkleProof')
--
-- @since v5.0.0
data FUELMintingRedeemer
  = FUELMintingRedeemer MerkleTreeEntry MerkleProof
  | FUELBurningRedeemer
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

-- | @since v5.0.0
PlutusTx.makeIsDataIndexed
  ''FUELMintingRedeemer
  [ ('FUELMintingRedeemer, 0)
  , ('FUELBurningRedeemer, 1)
  ]

-- * Update Committee Hash data

-- | Datum for the committee. This is used to verify
-- signatures for sidechain to mainchain transfers.
--
-- The actual representation of the committee's public key depends on the ATMS
-- implementation.
--
-- @since v4.0.0
data UpdateCommitteeDatum aggregatePubKeys = UpdateCommitteeDatum
  { aggregateCommitteePubKeys :: aggregatePubKeys
  , sidechainEpoch :: Integer
  }
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

-- | @since v4.0.0
instance ToData aggregatePubKeys => ToData (UpdateCommitteeDatum aggregatePubKeys) where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (UpdateCommitteeDatum {..}) =
    productToData2 aggregateCommitteePubKeys sidechainEpoch

-- | @since v4.0.0
instance FromData aggregatePubKeys => FromData (UpdateCommitteeDatum aggregatePubKeys) where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 UpdateCommitteeDatum

-- | @since v4.0.0
instance UnsafeFromData aggregatePubKeys => UnsafeFromData (UpdateCommitteeDatum aggregatePubKeys) where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 UpdateCommitteeDatum

-- | @since v4.0.0
makeHasField ''UpdateCommitteeDatum

-- | @since v4.0.0
newtype ATMSPlainAggregatePubKey = ATMSPlainAggregatePubKey LedgerBytes
  deriving newtype
    ( -- | @since v4.0.0
      FromData
    , -- | @since v4.0.0
      ToData
    , -- | @since v4.0.0
      UnsafeFromData
    , -- | @since v4.0.0
      Eq
    , -- | @since v4.0.0
      Ord
    , -- | @since v4.0.0
      IsString
    )
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

-- | = Important note
--
-- The 'Data' serializations for this type /cannot/ be changed.
--
-- @since v4.0.0
data UpdateCommitteeHashMessage aggregatePubKeys = UpdateCommitteeHashMessage
  { sidechainParams :: SidechainParams
  , -- | 'newCommitteePubKeys' is the new aggregate committee public keys
    -- | @since v4.0.0
    newAggregateCommitteePubKeys :: aggregatePubKeys
  , previousMerkleRoot :: Maybe LedgerBytes
  , sidechainEpoch :: Integer
  , -- | @since v5.0.0
    validatorHash :: ValidatorHash
  }
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

-- | @since v4.0.0
instance ToData aggregatePubKeys => ToData (UpdateCommitteeHashMessage aggregatePubKeys) where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (UpdateCommitteeHashMessage {..}) =
    productToData5 sidechainParams newAggregateCommitteePubKeys previousMerkleRoot sidechainEpoch validatorHash

-- | @since v4.0.0
instance FromData aggregatePubKeys => FromData (UpdateCommitteeHashMessage aggregatePubKeys) where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData5 UpdateCommitteeHashMessage

-- | @since v4.0.0
instance UnsafeFromData aggregatePubKeys => UnsafeFromData (UpdateCommitteeHashMessage aggregatePubKeys) where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData5 UpdateCommitteeHashMessage

-- | @since v4.0.0
makeHasField ''UpdateCommitteeHashMessage

-- | @since v4.0.0
newtype UpdateCommitteeHashRedeemer = UpdateCommitteeHashRedeemer
  { previousMerkleRoot :: Maybe LedgerBytes
  }
  deriving newtype
    ( -- | @since v4.0.0
      ToData
    , -- | @since v4.0.0
      FromData
    , -- | @since v4.0.0
      UnsafeFromData
    )
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

-- | @since v5.0.0
makeHasField ''UpdateCommitteeHashRedeemer

-- | Datum for a checkpoint
data CheckpointDatum = CheckpointDatum
  { -- | @since v4.0.0
    blockHash :: LedgerBytes
  , -- | @since v4.0.0
    blockNumber :: Integer
  }
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

-- | @since v4.0.0
instance ToData CheckpointDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (CheckpointDatum {..}) =
    productToData2 blockHash blockNumber

-- | @since v4.0.0
instance FromData CheckpointDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 CheckpointDatum

-- | @since v4.0.0
instance UnsafeFromData CheckpointDatum where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 CheckpointDatum

-- | @since v4.0.0
makeHasField ''CheckpointDatum

-- | 'CommitteeCertificateMint' is the type to parameterize committee
-- certificate verification minting policies.
-- See SIP05 in @docs/SIPs/@ for details.
--
-- @since v5.0.0
data CommitteeCertificateMint = CommitteeCertificateMint
  { thresholdNumerator :: Integer
  , thresholdDenominator :: Integer
  }
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

-- | @since v4.0.0
instance ToData CommitteeCertificateMint where
  toBuiltinData (CommitteeCertificateMint {..}) =
    productToData2 thresholdNumerator thresholdDenominator

-- | @since v4.0.0
instance FromData CommitteeCertificateMint where
  fromBuiltinData = productFromData2 CommitteeCertificateMint

-- | @since v4.0.0
instance UnsafeFromData CommitteeCertificateMint where
  unsafeFromBuiltinData = productUnsafeFromData2 CommitteeCertificateMint

-- | @since v4.0.0
makeHasField ''CommitteeCertificateMint

-- | 'ATMSPlainMultisignature' corresponds to SIP05 in @docs/SIPs/@.
-- This is used as redeemer for the
-- "TrustlessSidechain.CommitteePlainATMSPolicy".
--
-- @since v4.0.0
data ATMSPlainMultisignature = ATMSPlainMultisignature
  { plainPublicKeys :: [LedgerBytes]
  , plainSignatures :: [LedgerBytes]
  }
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

PlutusTx.makeIsDataIndexed ''ATMSPlainMultisignature [('ATMSPlainMultisignature, 0)]

-- | @since v5.0.0
makeHasField ''ATMSPlainMultisignature

-- | 'ATMSReddemer' allows for either minting or burning
-- @since v5.0.0
data ATMSRedeemer
  = -- | @since v5.0.0
    ATMSMint ATMSPlainMultisignature
  | -- | @since v5.0.0
    ATMSBurn
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

PlutusTx.makeIsDataIndexed ''ATMSRedeemer [('ATMSMint, 0), ('ATMSBurn, 1)]

-- | 'Checkpoint' is used as the parameter for the validator.
--
-- @since v4.0.0
data CheckpointParameter = CheckpointParameter
  { -- | @since v4.0.0
    sidechainParams :: SidechainParams
  , -- | The 'AssetClass' of the NFT that is used to
    -- identify the transaction.
    --
    -- @since v4.0.0
    assetClass :: AssetClass
  }
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

-- | @since v4.0.0
instance ToData CheckpointParameter where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (CheckpointParameter {..}) =
    productToData2
      sidechainParams
      assetClass

-- | @since v4.0.0
makeHasField ''CheckpointParameter

-- | @since v4.0.0
instance FromData CheckpointParameter where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 CheckpointParameter

-- | @since v4.0.0
instance UnsafeFromData CheckpointParameter where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 CheckpointParameter

-- | = Important note
--
-- The 'Data' serializations of this type /cannot/ be changed.
data CheckpointMessage = CheckpointMessage
  { -- | @since v4.0.0
    sidechainParams :: SidechainParams
  , -- | @since v4.0.0
    blockHash :: LedgerBytes
  , -- | @since v4.0.0
    blockNumber :: Integer
  , -- | @since v4.0.0
    sidechainEpoch :: Integer
  }
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

PlutusTx.makeIsDataIndexed ''CheckpointMessage [('CheckpointMessage, 0)]

-- | @since v4.0.0
makeHasField ''CheckpointMessage

-- | 'DParameterValidatorDatum' stores the ratio of permissioned candidates.  This
--ratio is represented as a pair of integers - permissionedCandidatesCount and
--registeredCandidatesCount.
--
-- @since v5.0.0
data DParameterValidatorDatum = DParameterValidatorDatum
  { -- | @since v5.0.0
    permissionedCandidatesCount :: Integer
  , -- | @since v5.0.0
    registeredCandidatesCount :: Integer
  }
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

-- | @since v5.0.0
makeHasField ''DParameterValidatorDatum

-- | @since v5.0.0
instance ToData DParameterValidatorDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData
    ( DParameterValidatorDatum
        permissionedCandidatesCount
        registeredCandidatesCount
      ) =
      productToData2 permissionedCandidatesCount registeredCandidatesCount

-- | @since v5.0.0
instance FromData DParameterValidatorDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 DParameterValidatorDatum

-- | @since v5.0.0
instance UnsafeFromData DParameterValidatorDatum where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 DParameterValidatorDatum

-- | 'PermissionedCandidatesPolicyRedeemer' signals whether transaction is supposed to mint or
--burn PermissionedCandidates tokens
--
-- @since v5.0.0
data PermissionedCandidatesPolicyRedeemer
  = -- | @since v5.0.0
    PermissionedCandidatesMint
  | -- | @since v5.0.0
    PermissionedCandidatesBurn
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

-- | @since v5.0.0
instance ToData PermissionedCandidatesPolicyRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData PermissionedCandidatesMint = BuiltinData $ PlutusTx.I 0
  toBuiltinData PermissionedCandidatesBurn = BuiltinData $ PlutusTx.I 1

-- | @since v5.0.0
instance FromData PermissionedCandidatesPolicyRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = do
    integerValue <- fromBuiltinData x
    case integerValue :: Integer of
      0 -> Just PermissionedCandidatesMint
      1 -> Just PermissionedCandidatesBurn
      _ -> Nothing

-- | @since v5.0.0
instance UnsafeFromData PermissionedCandidatesPolicyRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData x =
    let integerValue = unsafeFromBuiltinData x
     in case integerValue :: Integer of
          0 -> PermissionedCandidatesMint
          1 -> PermissionedCandidatesBurn
          _ -> error ()

-- | 'PermissionedCandidateKeys' stores the keys of some permissioned candiate.
--
-- @since v5.0.0
data PermissionedCandidateKeys = PermissionedCandidateKeys
  { -- | @since v5.0.0
    sidechainKey :: LedgerBytes
  , -- | @since v5.0.0
    auraKey :: LedgerBytes
  , -- | @since v5.0.0
    grandpaKey :: LedgerBytes
  }
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

-- | @since v5.0.0
instance ToData PermissionedCandidateKeys where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (PermissionedCandidateKeys s a g) =
    productToData3 s a g

-- | @since v5.0.0
instance FromData PermissionedCandidateKeys where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData3 PermissionedCandidateKeys

-- | @since v5.0.0
instance UnsafeFromData PermissionedCandidateKeys where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData3 PermissionedCandidateKeys

-- | @since v5.0.0
makeHasField ''PermissionedCandidateKeys

-- | 'PermissionedCandidatesValidatorDatum' stores a list of permissioned
--   candidates' keys.
--
-- @since v5.0.0
newtype PermissionedCandidatesValidatorDatum = PermissionedCandidatesValidatorDatum
  { candidates :: [PermissionedCandidateKeys]
  }
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )
  deriving newtype (ToData, FromData, UnsafeFromData)

-- | @since v5.0.0
makeHasField ''PermissionedCandidatesValidatorDatum

-- | 'PermissionedCandidatesValidatorRedeemer' signals whether transaction is
-- supposed to update the list of permissioned candidates or remove the list
-- altogether.
--
-- @since v5.0.0
data PermissionedCandidatesValidatorRedeemer
  = -- | @since v5.0.0
    UpdatePermissionedCandidates
  | -- | @since v5.0.0
    RemovePermissionedCandidates
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

-- | @since v5.0.0
instance ToData PermissionedCandidatesValidatorRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData UpdatePermissionedCandidates = BuiltinData $ PlutusTx.I 0
  toBuiltinData RemovePermissionedCandidates = BuiltinData $ PlutusTx.I 1

-- | @since v5.0.0
instance FromData PermissionedCandidatesValidatorRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = do
    integerValue <- fromBuiltinData x
    case integerValue :: Integer of
      0 -> Just UpdatePermissionedCandidates
      1 -> Just RemovePermissionedCandidates
      _ -> Nothing

-- | @since v5.0.0
instance UnsafeFromData PermissionedCandidatesValidatorRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData x =
    let integerValue = unsafeFromBuiltinData x
     in case integerValue :: Integer of
          0 -> UpdatePermissionedCandidates
          1 -> RemovePermissionedCandidates
          _ -> error ()

-- | 'InitTokenRedeemer' signals whether the init tokens should be minted
-- (possible only in transaction that initializes the sidechain) or burned.
data InitTokenRedeemer
  = -- | @since v6.0.0
    MintInitToken
  | -- | @since v6.0.0
    BurnInitToken
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

-- | @since v6.0.0
instance ToData InitTokenRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData MintInitToken = BuiltinData $ PlutusTx.I 0
  toBuiltinData BurnInitToken = BuiltinData $ PlutusTx.I 1

-- | @since v6.0.0
instance FromData InitTokenRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = do
    integerValue <- fromBuiltinData x
    case integerValue :: Integer of
      0 -> Just MintInitToken
      1 -> Just BurnInitToken
      _ -> Nothing

-- | @since v6.0.0
instance UnsafeFromData InitTokenRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData x =
    let integerValue = unsafeFromBuiltinData x
     in case integerValue :: Integer of
          0 -> MintInitToken
          1 -> BurnInitToken
          _ -> error ()

-- | 'InitTokenAssetClass' stores a currency symbol and a token name for an init
-- token.  This data type is used to parameterize minting policies that require
-- burning of an init token in order to mint their corresponding token.
data InitTokenAssetClass = InitTokenAssetClass
  { initTokenCurrencySymbol :: CurrencySymbol
  , initTokenName :: TokenName
  }
  deriving stock
    ( TSPrelude.Show
    , TSPrelude.Eq
    )

PlutusTx.makeLift ''InitTokenAssetClass
makeHasField ''InitTokenAssetClass

-- | @since v6.0.0
instance ToData InitTokenAssetClass where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (InitTokenAssetClass {..}) =
    productToData2 initTokenCurrencySymbol initTokenName

-- | @since v6.0.0
instance FromData InitTokenAssetClass where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 InitTokenAssetClass

-- | @since v6.0.0
instance UnsafeFromData InitTokenAssetClass where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 InitTokenAssetClass

data ImmutableReserveSettings = ImmutableReserveSettings
  { -- | `t0` is a POSIX time of a reserve UTxO initialization
    t0 :: POSIXTime
  , -- | `tokenKind` is an asset class of tokens that a reserve
    -- UTxO is allowed to store
    tokenKind :: AssetClass
  }
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

instance ToData ImmutableReserveSettings where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (ImmutableReserveSettings s a) =
    productToData2 s a

instance FromData ImmutableReserveSettings where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 ImmutableReserveSettings

instance UnsafeFromData ImmutableReserveSettings where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 ImmutableReserveSettings

makeHasField ''ImmutableReserveSettings

data MutableReserveSettings = MutableReserveSettings
  { -- | `vFunctionTotalAccrued` is a currency symbol of a minting policy
    -- that dictates the upper bound on the number of `tokenKind` tokens that
    -- can be transferred from a reserve utxo to an illiquid circulation supply
    -- from `t0` till now
    vFunctionTotalAccrued :: CurrencySymbol
  , -- | The amount of `tokenKind` the user is allowed to claim when releasing
    -- money from the reserve
    incentiveAmount :: Integer
  }
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

instance ToData MutableReserveSettings where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (MutableReserveSettings vt i) =
    productToData2 vt i

instance FromData MutableReserveSettings where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 MutableReserveSettings

instance UnsafeFromData MutableReserveSettings where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 MutableReserveSettings

makeHasField ''MutableReserveSettings

newtype ReserveStats = ReserveStats
  { -- | `tokenTotalAmountTransferred` is the total number
    -- of tokens that already have been transferred from a reserve utxo
    -- to an illiquid circulation supply
    tokenTotalAmountTransferred :: Integer
  }
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )
  deriving newtype (ToData, FromData, UnsafeFromData, Eq)

makeHasField ''ReserveStats

data ReserveDatum = ReserveDatum
  { immutableSettings :: ImmutableReserveSettings
  , mutableSettings :: MutableReserveSettings
  , stats :: ReserveStats
  }
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

instance ToData ReserveDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (ReserveDatum s a g) =
    productToData3 s a g

instance FromData ReserveDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData3 ReserveDatum

instance UnsafeFromData ReserveDatum where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData3 ReserveDatum

makeHasField ''ReserveDatum

data ReserveRedeemer
  = DepositToReserve
  | TransferToIlliquidCirculationSupply
  | UpdateReserve
  | Handover
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

instance ToData ReserveRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData DepositToReserve = BuiltinData $ PlutusTx.I 0
  toBuiltinData TransferToIlliquidCirculationSupply = BuiltinData $ PlutusTx.I 1
  toBuiltinData UpdateReserve = BuiltinData $ PlutusTx.I 2
  toBuiltinData Handover = BuiltinData $ PlutusTx.I 3

instance FromData ReserveRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = do
    integerValue <- fromBuiltinData x
    case integerValue :: Integer of
      0 -> Just DepositToReserve
      1 -> Just TransferToIlliquidCirculationSupply
      2 -> Just UpdateReserve
      3 -> Just Handover
      _ -> Nothing

instance UnsafeFromData ReserveRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData x =
    let integerValue = unsafeFromBuiltinData x
     in case integerValue :: Integer of
          0 -> DepositToReserve
          1 -> TransferToIlliquidCirculationSupply
          2 -> UpdateReserve
          3 -> Handover
          _ -> error ()

data IlliquidCirculationSupplyRedeemer
  = DepositMoreToSupply
  | WithdrawFromSupply
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

instance ToData IlliquidCirculationSupplyRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData DepositMoreToSupply = BuiltinData $ PlutusTx.I 0
  toBuiltinData WithdrawFromSupply = BuiltinData $ PlutusTx.I 1

instance FromData IlliquidCirculationSupplyRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = do
    integerValue <- fromBuiltinData x
    case integerValue :: Integer of
      0 -> Just DepositMoreToSupply
      1 -> Just WithdrawFromSupply
      _ -> Nothing

instance UnsafeFromData IlliquidCirculationSupplyRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData x =
    let integerValue = unsafeFromBuiltinData x
     in case integerValue :: Integer of
          0 -> DepositMoreToSupply
          1 -> WithdrawFromSupply
          _ -> error ()
