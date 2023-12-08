{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.Types where

import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (AssetClass, CurrencySymbol)
import Plutus.V2.Ledger.Api (BuiltinData (BuiltinData), LedgerBytes (LedgerBytes), TxOutRef, ValidatorHash)
import PlutusTx (makeIsDataIndexed)
import PlutusTx qualified
import TrustlessSidechain.Governance qualified as Governance
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

{- | Parameters uniquely identifying a sidechain

 = Note

 The 'Data' serializations for this type /cannot/ change.
-}
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
    -- @since Unreleased
    governanceAuthority :: Governance.GovernanceAuthority
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
instance HasField "chainId" SidechainParams Integer where
  {-# INLINE get #-}
  get = chainId
  {-# INLINE modify #-}
  modify f sp = sp {chainId = f (chainId sp)}

-- | @since v4.0.0
instance HasField "genesisUtxo" SidechainParams TxOutRef where
  {-# INLINE get #-}
  get = genesisUtxo
  {-# INLINE modify #-}
  modify f sp = sp {genesisUtxo = f (genesisUtxo sp)}

-- | @since v4.0.0
instance HasField "thresholdNumerator" SidechainParams Integer where
  {-# INLINE get #-}
  get = thresholdNumerator
  {-# INLINE modify #-}
  modify f sp = sp {thresholdNumerator = f (thresholdNumerator sp)}

-- | @since v4.0.0
instance HasField "thresholdDenominator" SidechainParams Integer where
  {-# INLINE get #-}
  get = thresholdDenominator
  {-# INLINE modify #-}
  modify f sp = sp {thresholdDenominator = f (thresholdDenominator sp)}

-- | @since Unreleased
instance HasField "governanceAuthority" SidechainParams Governance.GovernanceAuthority where
  {-# INLINE get #-}
  get = governanceAuthority
  {-# INLINE modify #-}
  modify f sp = sp {governanceAuthority = f (governanceAuthority sp)}

{- | Compressed DER SECP256k1 public key.
 = Important note

 The 'Data' serializations for this type /cannot/ change.
-}
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

{- | Ed25519 public key
 = Important note

 The 'Data' serializations for this type /cannot/ change.
-}
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

{- | Ed25519 signature
 = Important note

 The 'Data' serializations for this type /cannot/ change.
-}
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

{- | 'CandidatePermissionMint' is used to parameterize the minting policy in
 'TrustlessSidechain.CommitteeCandidateMintingPolicy'.
-}
data CandidatePermissionMint = CandidatePermissionMint
  { -- | @since v4.0.0
    sidechainParams :: SidechainParams
  , -- | @since v4.0.0
    utxo :: TxOutRef
  }
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

-- | @since v4.0.0
instance ToData CandidatePermissionMint where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (CandidatePermissionMint {..}) =
    productToData2 sidechainParams utxo

-- | @since v4.0.0
instance FromData CandidatePermissionMint where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 CandidatePermissionMint

-- | @since v4.0.0
instance UnsafeFromData CandidatePermissionMint where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 CandidatePermissionMint

-- | @since v4.0.0
instance HasField "sidechainParams" CandidatePermissionMint SidechainParams where
  {-# INLINE get #-}
  get (CandidatePermissionMint sp _) = sp
  {-# INLINE modify #-}
  modify f (CandidatePermissionMint sp u) = CandidatePermissionMint (f sp) u

-- | @since v4.0.0
instance HasField "utxo" CandidatePermissionMint TxOutRef where
  {-# INLINE get #-}
  get (CandidatePermissionMint _ u) = u
  {-# INLINE modify #-}
  modify f (CandidatePermissionMint sp u) = CandidatePermissionMint sp (f u)

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
  , -- | A UTxO that must be spent by the transaction
    -- | @since v4.0.0
    inputUtxo :: TxOutRef
  , -- | Owner public key hash
    -- | @since v4.0.0
    ownPkh :: PubKeyHash
  , -- | Sidechain authority discovery key
    -- | @since Unreleased
    auraKey :: LedgerBytes
  , -- | Sidechain grandpa key
    -- | @since Unreleased
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
instance HasField "spoPubKey" BlockProducerRegistration StakeOwnership where
  {-# INLINE get #-}
  get (BlockProducerRegistration x _ _ _ _ _ _) = x
  {-# INLINE modify #-}
  modify f (BlockProducerRegistration so scPK scS u pkh ak gk) =
    BlockProducerRegistration (f so) scPK scS u pkh ak gk

-- | @since v4.0.0
instance HasField "ecdsaSecp256k1PubKey" BlockProducerRegistration LedgerBytes where
  {-# INLINE get #-}
  get (BlockProducerRegistration _ x _ _ _ _ _) = x
  {-# INLINE modify #-}
  modify f (BlockProducerRegistration so scPK scS u pkh ak gk) =
    BlockProducerRegistration so (f scPK) scS u pkh ak gk

-- | @since v4.0.0
instance HasField "sidechainSignature" BlockProducerRegistration Signature where
  {-# INLINE get #-}
  get (BlockProducerRegistration _ _ x _ _ _ _) = x
  {-# INLINE modify #-}
  modify f (BlockProducerRegistration so scPK scS u pkh ak gk) =
    BlockProducerRegistration so scPK (f scS) u pkh ak gk

-- | @since v4.0.0
instance HasField "inputUtxo" BlockProducerRegistration TxOutRef where
  {-# INLINE get #-}
  get (BlockProducerRegistration _ _ _ x _ _ _) = x
  {-# INLINE modify #-}
  modify f (BlockProducerRegistration so scPK scS u pkh ak gk) =
    BlockProducerRegistration so scPK scS (f u) pkh ak gk

-- | @since v4.0.0
instance HasField "ownPkh" BlockProducerRegistration PubKeyHash where
  {-# INLINE get #-}
  get (BlockProducerRegistration _ _ _ _ x _ _) = x
  {-# INLINE modify #-}
  modify f (BlockProducerRegistration so scPK scS u pkh ak gk) =
    BlockProducerRegistration so scPK scS u (f pkh) ak gk

{- | = Important note

 The 'Data' serializations for this type /cannot/ change.
-}
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
instance HasField "sidechainParams" BlockProducerRegistrationMsg SidechainParams where
  {-# INLINE get #-}
  get (BlockProducerRegistrationMsg x _ _) = x
  {-# INLINE modify #-}
  modify f (BlockProducerRegistrationMsg sp spk u) =
    BlockProducerRegistrationMsg (f sp) spk u

-- | @since v4.0.0
instance HasField "sidechainPubKey" BlockProducerRegistrationMsg LedgerBytes where
  {-# INLINE get #-}
  get (BlockProducerRegistrationMsg _ x _) = x
  {-# INLINE modify #-}
  modify f (BlockProducerRegistrationMsg sp spk u) =
    BlockProducerRegistrationMsg sp (f spk) u

-- | @since v4.0.0
instance HasField "inputUtxo" BlockProducerRegistrationMsg TxOutRef where
  {-# INLINE get #-}
  get (BlockProducerRegistrationMsg _ _ x) = x
  {-# INLINE modify #-}
  modify f (BlockProducerRegistrationMsg sp spk u) =
    BlockProducerRegistrationMsg sp spk (f u)

-- * Merkle Root Token data

{- | 'MerkleTreeEntry' (abbr. mte and pl. mtes) is the data which are the elements in the merkle tree
 for the MerkleRootToken.

 = Important note

 The 'Data' serializations for this type /cannot/ change.
-}
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
instance HasField "index" MerkleTreeEntry Integer where
  {-# INLINE get #-}
  get (MerkleTreeEntry x _ _ _) = x
  {-# INLINE modify #-}
  modify f (MerkleTreeEntry i a r pmr) =
    MerkleTreeEntry (f i) a r pmr

-- | @since v4.0.0
instance HasField "amount" MerkleTreeEntry Integer where
  {-# INLINE get #-}
  get (MerkleTreeEntry _ x _ _) = x
  {-# INLINE modify #-}
  modify f (MerkleTreeEntry i a r pmr) =
    MerkleTreeEntry i (f a) r pmr

-- | @since v4.0.0
instance HasField "recipient" MerkleTreeEntry LedgerBytes where
  {-# INLINE get #-}
  get (MerkleTreeEntry _ _ x _) = x
  {-# INLINE modify #-}
  modify f (MerkleTreeEntry i a r pmr) =
    MerkleTreeEntry i a (f r) pmr

-- | @since v4.0.0
instance HasField "previousMerkleRoot" MerkleTreeEntry (Maybe LedgerBytes) where
  {-# INLINE get #-}
  get (MerkleTreeEntry _ _ _ x) = x
  {-# INLINE modify #-}
  modify f (MerkleTreeEntry i a r pmr) =
    MerkleTreeEntry i a r (f pmr)

{- | 'MerkleRootInsertionMessage' is a data type for which committee members
 create signatures for
 >  blake2b(cbor(MerkleRootInsertionMessage))

 = Important note

 The 'Data' serializations for this type /cannot/ change.
-}
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
instance HasField "sidechainParams" MerkleRootInsertionMessage SidechainParams where
  {-# INLINE get #-}
  get (MerkleRootInsertionMessage x _ _) = x
  {-# INLINE modify #-}
  modify f (MerkleRootInsertionMessage sp mr pmr) =
    MerkleRootInsertionMessage (f sp) mr pmr

-- | @since v4.0.0
instance HasField "merkleRoot" MerkleRootInsertionMessage LedgerBytes where
  {-# INLINE get #-}
  get (MerkleRootInsertionMessage _ x _) = x
  {-# INLINE modify #-}
  modify f (MerkleRootInsertionMessage sp mr pmr) =
    MerkleRootInsertionMessage sp (f mr) pmr

-- | @since v4.0.0
instance HasField "previousMerkleRoot" MerkleRootInsertionMessage (Maybe LedgerBytes) where
  {-# INLINE get #-}
  get (MerkleRootInsertionMessage _ _ x) = x
  {-# INLINE modify #-}
  modify f (MerkleRootInsertionMessage sp mr pmr) =
    MerkleRootInsertionMessage sp mr (f pmr)

{- | 'SignedMerkleRootRedeemer' is the redeemer for the signed merkle root
 minting policy.

 @since v4.0.0
-}
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

instance HasField "previousMerkleRoot" SignedMerkleRootRedeemer (Maybe LedgerBytes) where
  {-# INLINE get #-}
  get (SignedMerkleRootRedeemer x) = x
  {-# INLINE modify #-}
  modify f (SignedMerkleRootRedeemer pmr) =
    SignedMerkleRootRedeemer (f pmr)

{- | 'CombinedMerkleProof' is a product type to include both the
 'MerkleTreeEntry' and the 'MerkleProof'.

 This exists as for testing in #249.

 = Important note

 The 'Data' serializations of this type /cannot/ change.
-}
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
instance HasField "transaction" CombinedMerkleProof MerkleTreeEntry where
  {-# INLINE get #-}
  get (CombinedMerkleProof x _) = x
  {-# INLINE modify #-}
  modify f (CombinedMerkleProof t mp) =
    CombinedMerkleProof (f t) mp

-- | @since v4.0.0
instance HasField "merkleProof" CombinedMerkleProof MerkleProof where
  {-# INLINE get #-}
  get (CombinedMerkleProof _ x) = x
  {-# INLINE modify #-}
  modify f (CombinedMerkleProof t mp) =
    CombinedMerkleProof t (f mp)

-- * FUEL Minting Policy data

{- | 'FUELMintingRedeemer' indicates that we wish to mint FUEL on the mainchain.
 So, this includes which transaction in the sidechain we are transferring over
 to the main chain (hence the 'MerkleTreeEntry'), and the proof that this
 actually happened on the sidechain (hence the 'MerkleProof')

 @since Unreleased
-}
data FUELMintingRedeemer
  = FUELMintingRedeemer MerkleTreeEntry MerkleProof
  | FUELBurningRedeemer

-- | @since Unreleased
PlutusTx.makeIsDataIndexed
  ''FUELMintingRedeemer
  [ ('FUELMintingRedeemer, 0)
  , ('FUELBurningRedeemer, 1)
  ]

-- * Update Committee Hash data

{- | Datum for the committee. This is used to verify
 signatures for sidechain to mainchain transfers.

 The actual representation of the committee's public key depends on the ATMS
 implementation.

 @since v4.0.0
-}
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
instance HasField "aggregateCommitteePubKeys" (UpdateCommitteeDatum aggregatePubKeys) aggregatePubKeys where
  {-# INLINE get #-}
  get (UpdateCommitteeDatum x _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeDatum ch se) =
    UpdateCommitteeDatum (f ch) se

-- | @since v4.0.0
instance HasField "sidechainEpoch" (UpdateCommitteeDatum aggregatePubKeys) Integer where
  {-# INLINE get #-}
  get (UpdateCommitteeDatum _ x) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeDatum ch se) =
    UpdateCommitteeDatum ch (f se)

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

-- | 'UpdateCommitteeHash' is used as the parameter for the validator.
data UpdateCommitteeHash = UpdateCommitteeHash
  { sidechainParams :: SidechainParams
  , -- | 'committeeOracleCurrencySymbol' is the 'CurrencySymbol' of the NFT that is used to
    -- identify the transaction the current committee.
    committeeOracleCurrencySymbol :: CurrencySymbol
  , -- | 'committeeCertificateVerificationCurrencySymbol' is the currency
    -- symbol for the committee certificate verification policy i.e., the
    -- currency symbol whose minted token name indicates that the current
    -- committee has signed the token name.
    committeeCertificateVerificationCurrencySymbol :: CurrencySymbol
  , -- | 'mptRootTokenCurrencySymbol' is the currency symbol of the corresponding merkle
    -- root token. This is needed for verifying that the previous merkle root is verified.
    mptRootTokenCurrencySymbol :: CurrencySymbol
  }
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

-- | @since v4.0.0
instance ToData UpdateCommitteeHash where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (UpdateCommitteeHash {..}) =
    productToData4
      sidechainParams
      committeeOracleCurrencySymbol
      committeeCertificateVerificationCurrencySymbol
      mptRootTokenCurrencySymbol

-- | @since v4.0.0
instance HasField "sidechainParams" UpdateCommitteeHash SidechainParams where
  {-# INLINE get #-}
  get (UpdateCommitteeHash x _ _ _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHash sp cocs ccvcs rtcs) =
    UpdateCommitteeHash (f sp) cocs ccvcs rtcs

-- | @since v4.0.0
instance HasField "committeeOracleCurrencySymbol" UpdateCommitteeHash CurrencySymbol where
  {-# INLINE get #-}
  get (UpdateCommitteeHash _ x _ _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHash sp cocs ccvcs rtcs) =
    UpdateCommitteeHash sp (f cocs) ccvcs rtcs

-- | @since v4.0.0
instance HasField "committeeCertificateVerificationCurrencySymbol" UpdateCommitteeHash CurrencySymbol where
  {-# INLINE get #-}
  get (UpdateCommitteeHash _ _ x _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHash sp cocs ccvcs rtcs) =
    UpdateCommitteeHash sp cocs (f ccvcs) rtcs

-- | @since v4.0.0
instance HasField "mptRootTokenCurrencySymbol" UpdateCommitteeHash CurrencySymbol where
  {-# INLINE get #-}
  get (UpdateCommitteeHash _ _ _ x) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHash sp cocs ccvcs rtcs) =
    UpdateCommitteeHash sp cocs ccvcs (f rtcs)

instance FromData UpdateCommitteeHash where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData4 UpdateCommitteeHash

-- | @since v4.0.0
instance UnsafeFromData UpdateCommitteeHash where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData4 UpdateCommitteeHash

{- | = Important note

 The 'Data' serializations for this type /cannot/ be changed.

 @since v4.0.0
-}
data UpdateCommitteeHashMessage aggregatePubKeys = UpdateCommitteeHashMessage
  { sidechainParams :: SidechainParams
  , -- | 'newCommitteePubKeys' is the new aggregate committee public keys
    -- | @since v4.0.0
    newAggregateCommitteePubKeys :: aggregatePubKeys
  , previousMerkleRoot :: Maybe LedgerBytes
  , sidechainEpoch :: Integer
  , -- | @since Unreleased
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
instance HasField "sidechainParams" (UpdateCommitteeHashMessage aggregatePubKeys) SidechainParams where
  {-# INLINE get #-}
  get (UpdateCommitteeHashMessage x _ _ _ _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashMessage sp nacpks pmr se vh) =
    UpdateCommitteeHashMessage (f sp) nacpks pmr se vh

-- | @since v4.0.0
instance HasField "newAggregateCommitteePubKeys" (UpdateCommitteeHashMessage aggregatePubKeys) aggregatePubKeys where
  {-# INLINE get #-}
  get (UpdateCommitteeHashMessage _ x _ _ _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashMessage sp nacpks pmr se vh) =
    UpdateCommitteeHashMessage sp (f nacpks) pmr se vh

-- | @since v4.0.0
instance HasField "previousMerkleRoot" (UpdateCommitteeHashMessage aggregatePubKeys) (Maybe LedgerBytes) where
  {-# INLINE get #-}
  get (UpdateCommitteeHashMessage _ _ x _ _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashMessage sp nacpks pmr se vh) =
    UpdateCommitteeHashMessage sp nacpks (f pmr) se vh

-- | @since v4.0.0
instance HasField "sidechainEpoch" (UpdateCommitteeHashMessage aggregatePubKeys) Integer where
  {-# INLINE get #-}
  get (UpdateCommitteeHashMessage _ _ _ x _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashMessage sp nacpks pmr se vh) =
    UpdateCommitteeHashMessage sp nacpks pmr (f se) vh

-- | @since Unreleased
instance HasField "validatorHash" (UpdateCommitteeHashMessage aggregatePubKeys) ValidatorHash where
  {-# INLINE get #-}
  get (UpdateCommitteeHashMessage _ _ _ _ x) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashMessage sp nacpks pmr se vh) =
    UpdateCommitteeHashMessage sp nacpks pmr se (f vh)

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

-- | Datum for a checkpoint
data CheckpointDatum = CheckpointDatum
  { -- | @since v4.0.0
    blockHash :: LedgerBytes
  , -- | @since v4.0.0
    blockNumber :: Integer
  }

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
instance HasField "blockHash" CheckpointDatum LedgerBytes where
  {-# INLINE get #-}
  get (CheckpointDatum x _) = x
  {-# INLINE modify #-}
  modify f (CheckpointDatum bh bn) =
    CheckpointDatum (f bh) bn

-- | @since v4.0.0
instance HasField "blockNumber" CheckpointDatum Integer where
  {-# INLINE get #-}
  get (CheckpointDatum _ x) = x
  {-# INLINE modify #-}
  modify f (CheckpointDatum bh bn) =
    CheckpointDatum bh (f bn)

{- | 'CommitteeCertificateMint' is the type to parameterize committee
 certificate verification minting policies.
 See SIP05 in @docs/SIPs/@ for details.

 @since Unreleased
-}
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
instance HasField "thresholdNumerator" CommitteeCertificateMint Integer where
  {-# INLINE get #-}
  get (CommitteeCertificateMint x _) = x
  {-# INLINE modify #-}
  modify f (CommitteeCertificateMint tn td) =
    CommitteeCertificateMint (f tn) td

-- | @since v4.0.0
instance HasField "thresholdDenominator" CommitteeCertificateMint Integer where
  {-# INLINE get #-}
  get (CommitteeCertificateMint _ x) = x
  {-# INLINE modify #-}
  modify f (CommitteeCertificateMint tn td) =
    CommitteeCertificateMint tn (f td)

{- | 'ATMSPlainMultisignature' corresponds to SIP05 in @docs/SIPs/@.
 This is used as redeemer for the
 "TrustlessSidechain.CommitteePlainATMSPolicy".

 @since v4.0.0
-}
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

{- | 'ATMSReddemer' allows for either minting or burning
 @since Unreleased
-}
data ATMSRedeemer
  = -- | @since Unreleased
    ATMSMint ATMSPlainMultisignature
  | -- | @since Unreleased
    ATMSBurn

PlutusTx.makeIsDataIndexed ''ATMSRedeemer [('ATMSMint, 0), ('ATMSBurn, 1)]

{- | The Redeemer that is passed to the on-chain validator to update the
 checkpoint
-}
data CheckpointRedeemer = CheckpointRedeemer
  { newCheckpointBlockHash :: LedgerBytes
  , newCheckpointBlockNumber :: Integer
  }

-- | @since v4.0.0
instance ToData CheckpointRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (CheckpointRedeemer {..}) =
    productToData2
      newCheckpointBlockHash
      newCheckpointBlockNumber

-- | @since v4.0.0
instance FromData CheckpointRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 CheckpointRedeemer

-- | @since v4.0.0
instance UnsafeFromData CheckpointRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 CheckpointRedeemer

-- | @since v4.0.0
instance HasField "newCheckpointBlockHash" CheckpointRedeemer LedgerBytes where
  {-# INLINE get #-}
  get (CheckpointRedeemer x _) = x
  {-# INLINE modify #-}
  modify f (CheckpointRedeemer ncbh ncbn) =
    CheckpointRedeemer (f ncbh) ncbn

-- | @since v4.0.0
instance HasField "newCheckpointBlockNumber" CheckpointRedeemer Integer where
  {-# INLINE get #-}
  get (CheckpointRedeemer _ x) = x
  {-# INLINE modify #-}
  modify f (CheckpointRedeemer ncbh ncbn) =
    CheckpointRedeemer ncbh (f ncbn)

{- | 'Checkpoint' is used as the parameter for the validator.

 @since v4.0.0
-}
data CheckpointParameter = CheckpointParameter
  { -- | @since v4.0.0
    sidechainParams :: SidechainParams
  , -- | The 'AssetClass' of the NFT that is used to
    -- identify the transaction.
    --
    -- @since v4.0.0
    assetClass :: AssetClass
  , -- | The currency symbol which uniquely identifies the
    -- current committee.
    --
    -- @since v4.0.0
    committeeOracleCurrencySymbol :: CurrencySymbol
  , -- | The currency symbol of the committee certificate verification minting
    -- policy.
    --
    -- @since v4.0.0
    committeeCertificateVerificationCurrencySymbol :: CurrencySymbol
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
    productToData4
      sidechainParams
      assetClass
      committeeOracleCurrencySymbol
      committeeCertificateVerificationCurrencySymbol

-- | @since v4.0.0
instance HasField "sidechainParams" CheckpointParameter SidechainParams where
  {-# INLINE get #-}
  get (CheckpointParameter x _ _ _) = x
  {-# INLINE modify #-}
  modify f (CheckpointParameter csp cac ccocs chac) =
    CheckpointParameter (f csp) cac ccocs chac

-- | @since v4.0.0
instance HasField "assetClass" CheckpointParameter AssetClass where
  {-# INLINE get #-}
  get (CheckpointParameter _ x _ _) = x
  {-# INLINE modify #-}
  modify f (CheckpointParameter csp cac ccocs chac) =
    CheckpointParameter csp (f cac) ccocs chac

-- | @since v4.0.0
instance HasField "committeeOracleCurrencySymbol" CheckpointParameter CurrencySymbol where
  {-# INLINE get #-}
  get (CheckpointParameter _ _ x _) = x
  {-# INLINE modify #-}
  modify f (CheckpointParameter csp cac ccocs chac) =
    CheckpointParameter csp cac (f ccocs) chac

-- | @since v4.0.0
instance HasField "committeeCertificateVerificationCurrencySymbol" CheckpointParameter CurrencySymbol where
  {-# INLINE get #-}
  get (CheckpointParameter _ _ _ x) = x
  {-# INLINE modify #-}
  modify f (CheckpointParameter csp cac ccocs chac) =
    CheckpointParameter csp cac ccocs (f chac)

-- | @since v4.0.0
instance FromData CheckpointParameter where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData4 CheckpointParameter

-- | @since v4.0.0
instance UnsafeFromData CheckpointParameter where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData4 CheckpointParameter

{- | = Important note

 The 'Data' serializations of this type /cannot/ be changed.
-}
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

PlutusTx.makeIsDataIndexed ''CheckpointMessage [('CheckpointMessage, 0)]

-- | @since v4.0.0
instance HasField "sidechainParams" CheckpointMessage SidechainParams where
  {-# INLINE get #-}
  get (CheckpointMessage x _ _ _) = x
  {-# INLINE modify #-}
  modify f (CheckpointMessage sp bh bn se) =
    CheckpointMessage (f sp) bh bn se

-- | @since v4.0.0
instance HasField "blockHash" CheckpointMessage LedgerBytes where
  {-# INLINE get #-}
  get (CheckpointMessage _ x _ _) = x
  {-# INLINE modify #-}
  modify f (CheckpointMessage sp bh bn se) =
    CheckpointMessage sp (f bh) bn se

-- | @since v4.0.0
instance HasField "blockNumber" CheckpointMessage Integer where
  {-# INLINE get #-}
  get (CheckpointMessage _ _ x _) = x
  {-# INLINE modify #-}
  modify f (CheckpointMessage sp bh bn se) =
    CheckpointMessage sp bh (f bn) se

-- | @since v4.0.0
instance HasField "sidechainEpoch" CheckpointMessage Integer where
  {-# INLINE get #-}
  get (CheckpointMessage _ _ _ x) = x
  {-# INLINE modify #-}
  modify f (CheckpointMessage sp bh bn se) =
    CheckpointMessage sp bh bn (f se)

{- | 'DParameterPolicyRedeemer' signals whether transaction is supposed to mint or
burn DParameter tokens

@since Unreleased
-}
data DParameterPolicyRedeemer
  = -- | @since Unreleased
    DParameterMint
  | -- | @since Unreleased
    DParameterBurn

-- | @since Unreleased
instance ToData DParameterPolicyRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData DParameterMint = BuiltinData $ PlutusTx.I 0
  toBuiltinData DParameterBurn = BuiltinData $ PlutusTx.I 1

-- | @since Unreleased
instance FromData DParameterPolicyRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = do
    integerValue <- fromBuiltinData x
    case integerValue :: Integer of
      0 -> Just DParameterMint
      1 -> Just DParameterBurn
      _ -> Nothing

-- | @since Unreleased
instance UnsafeFromData DParameterPolicyRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData x =
    let integerValue = unsafeFromBuiltinData x
     in case integerValue :: Integer of
          0 -> DParameterMint
          1 -> DParameterBurn
          _ -> error ()

{- | 'DParameterValidatorDatum' stores the ratio of permissioned candidates.  This
ratio is represented as a pair of integers - permissionedCandidatesCount and
registeredCandidatesCount.

@since Unreleased
-}
data DParameterValidatorDatum = DParameterValidatorDatum
  { -- | @since Unreleased
    permissionedCandidatesCount :: Integer
  , -- | @since Unreleased
    registeredCandidatesCount :: Integer
  }

-- | @since Unreleased
instance HasField "permissionedCandidatesCount" DParameterValidatorDatum Integer where
  {-# INLINE get #-}
  get (DParameterValidatorDatum permissionedCandidatesCount _) =
    permissionedCandidatesCount
  {-# INLINE modify #-}
  modify
    f
    ( DParameterValidatorDatum
        permissionedCandidatesCount
        registeredCandidatesCount
      ) =
      DParameterValidatorDatum
        (f permissionedCandidatesCount)
        registeredCandidatesCount

-- | @since Unreleased
instance HasField "registeredCandidatesCount" DParameterValidatorDatum Integer where
  {-# INLINE get #-}
  get (DParameterValidatorDatum _ registeredCandidatesCount) =
    registeredCandidatesCount
  {-# INLINE modify #-}
  modify f (DParameterValidatorDatum numerator registeredCandidatesCount) =
    DParameterValidatorDatum numerator (f registeredCandidatesCount)

-- | @since Unreleased
instance ToData DParameterValidatorDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData
    ( DParameterValidatorDatum
        permissionedCandidatesCount
        registeredCandidatesCount
      ) =
      productToData2 permissionedCandidatesCount registeredCandidatesCount

-- | @since Unreleased
instance FromData DParameterValidatorDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 DParameterValidatorDatum

-- | @since Unreleased
instance UnsafeFromData DParameterValidatorDatum where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 DParameterValidatorDatum

{- | 'DParameterValidatorRedeemer' signals whether transaction is supposed to
update the d parameter or remove it.

@since Unreleased
-}
data DParameterValidatorRedeemer
  = -- | @since Unreleased
    UpdateDParameter
  | -- | @since Unreleased
    RemoveDParameter

-- | @since Unreleased
instance ToData DParameterValidatorRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData UpdateDParameter = BuiltinData $ PlutusTx.I 0
  toBuiltinData RemoveDParameter = BuiltinData $ PlutusTx.I 1

-- | @since Unreleased
instance FromData DParameterValidatorRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = do
    integerValue <- fromBuiltinData x
    case integerValue :: Integer of
      0 -> Just UpdateDParameter
      1 -> Just RemoveDParameter
      _ -> Nothing

-- | @since Unreleased
instance UnsafeFromData DParameterValidatorRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData x =
    let integerValue = unsafeFromBuiltinData x
     in case integerValue :: Integer of
          0 -> UpdateDParameter
          1 -> RemoveDParameter
          _ -> error ()

{- | 'PermissionedCandidatesPolicyRedeemer' signals whether transaction is supposed to mint or
burn PermissionedCandidates tokens

@since Unreleased
-}
data PermissionedCandidatesPolicyRedeemer
  = -- | @since Unreleased
    PermissionedCandidatesMint
  | -- | @since Unreleased
    PermissionedCandidatesBurn

-- | @since Unreleased
instance ToData PermissionedCandidatesPolicyRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData PermissionedCandidatesMint = BuiltinData $ PlutusTx.I 0
  toBuiltinData PermissionedCandidatesBurn = BuiltinData $ PlutusTx.I 1

-- | @since Unreleased
instance FromData PermissionedCandidatesPolicyRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = do
    integerValue <- fromBuiltinData x
    case integerValue :: Integer of
      0 -> Just PermissionedCandidatesMint
      1 -> Just PermissionedCandidatesBurn
      _ -> Nothing

-- | @since Unreleased
instance UnsafeFromData PermissionedCandidatesPolicyRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData x =
    let integerValue = unsafeFromBuiltinData x
     in case integerValue :: Integer of
          0 -> PermissionedCandidatesMint
          1 -> PermissionedCandidatesBurn
          _ -> error ()

{- | 'PermissionedCandidateKeys' stores the keys of some permissioned candiate.

@since Unreleased
-}
data PermissionedCandidateKeys = PermissionedCandidateKeys
  { -- | @since Unreleased
    mainchainKey :: LedgerBytes
  , -- | @since Unreleased
    sidechainKey :: LedgerBytes
  , -- | @since Unreleased
    auraKey :: LedgerBytes
  , -- | @since Unreleased
    grandpaKey :: LedgerBytes
  }

-- | @since Unreleased
instance ToData PermissionedCandidateKeys where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (PermissionedCandidateKeys m s a g) =
    productToData4 m s a g

-- | @since Unreleased
instance FromData PermissionedCandidateKeys where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData4 PermissionedCandidateKeys

-- | @since Unreleased
instance UnsafeFromData PermissionedCandidateKeys where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData4 PermissionedCandidateKeys

-- | @since Unreleased
instance HasField "mainchainKey" PermissionedCandidateKeys LedgerBytes where
  {-# INLINE get #-}
  get (PermissionedCandidateKeys m _ _ _) = m
  {-# INLINE modify #-}
  modify f (PermissionedCandidateKeys m s a g) =
    PermissionedCandidateKeys (f m) s a g

-- | @since Unreleased
instance HasField "sidechainKey" PermissionedCandidateKeys LedgerBytes where
  {-# INLINE get #-}
  get (PermissionedCandidateKeys _ s _ _) = s
  {-# INLINE modify #-}
  modify f (PermissionedCandidateKeys m s a g) =
    PermissionedCandidateKeys m (f s) a g

-- | @since Unreleased
instance HasField "auraKey" PermissionedCandidateKeys LedgerBytes where
  {-# INLINE get #-}
  get (PermissionedCandidateKeys _ _ a _) = a
  {-# INLINE modify #-}
  modify f (PermissionedCandidateKeys m s a g) =
    PermissionedCandidateKeys m s (f a) g

-- | @since Unreleased
instance HasField "grandpaKey" PermissionedCandidateKeys LedgerBytes where
  {-# INLINE get #-}
  get (PermissionedCandidateKeys _ _ _ g) = g
  {-# INLINE modify #-}
  modify f (PermissionedCandidateKeys m s a g) =
    PermissionedCandidateKeys m s a (f g)

{- | 'PermissionedCandidatesValidatorDatum' stores a list of permissioned
   candidates' keys.

@since Unreleased
-}
newtype PermissionedCandidatesValidatorDatum = PermissionedCandidatesValidatorDatum
  { candidates :: [PermissionedCandidateKeys]
  }
  deriving newtype (ToData, FromData, UnsafeFromData)

-- | @since Unreleased
instance HasField "candidates" PermissionedCandidatesValidatorDatum [PermissionedCandidateKeys] where
  {-# INLINE get #-}
  get (PermissionedCandidatesValidatorDatum candidates) = candidates
  {-# INLINE modify #-}
  modify f (PermissionedCandidatesValidatorDatum candidates) =
    PermissionedCandidatesValidatorDatum (f candidates)

{- | 'PermissionedCandidatesValidatorRedeemer' signals whether transaction is supposed to
update the list of permissioned candidates or remove the list altogether.

@since Unreleased
-}
data PermissionedCandidatesValidatorRedeemer
  = -- | @since Unreleased
    UpdatePermissionedCandidates
  | -- | @since Unreleased
    RemovePermissionedCandidates

-- | @since Unreleased
instance ToData PermissionedCandidatesValidatorRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData UpdatePermissionedCandidates = BuiltinData $ PlutusTx.I 0
  toBuiltinData RemovePermissionedCandidates = BuiltinData $ PlutusTx.I 1

-- | @since Unreleased
instance FromData PermissionedCandidatesValidatorRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = do
    integerValue <- fromBuiltinData x
    case integerValue :: Integer of
      0 -> Just UpdatePermissionedCandidates
      1 -> Just RemovePermissionedCandidates
      _ -> Nothing

-- | @since Unreleased
instance UnsafeFromData PermissionedCandidatesValidatorRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData x =
    let integerValue = unsafeFromBuiltinData x
     in case integerValue :: Integer of
          0 -> UpdatePermissionedCandidates
          1 -> RemovePermissionedCandidates
          _ -> error ()
