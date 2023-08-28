{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.Types where

import Ledger.Crypto (PubKey, PubKeyHash, Signature)
import Ledger.Value (AssetClass, CurrencySymbol)
import Plutus.V2.Ledger.Api (
  Address,
  LedgerBytes (LedgerBytes),
 )
import Plutus.V2.Ledger.Tx (TxOutRef)
import PlutusTx qualified
import TrustlessSidechain.Governance qualified as Governance
import TrustlessSidechain.HaskellPrelude qualified as TSPrelude
import TrustlessSidechain.MerkleTree (MerkleProof)
import TrustlessSidechain.PlutusPrelude

-- * Sidechain Parametrization and general data

newtype GenesisHash = GenesisHash {getGenesisHash :: LedgerBytes}
  deriving stock (TSPrelude.Eq, TSPrelude.Ord)
  deriving newtype
    ( Eq
    , Ord
    )
  deriving (IsString, TSPrelude.Show) via LedgerBytes

-- | @since Unreleased
instance ToData GenesisHash where
  toBuiltinData (GenesisHash x) = toBuiltinData x

-- | @since Unreleased
instance FromData GenesisHash where
  fromBuiltinData x = GenesisHash <$> fromBuiltinData x

-- | @since Unreleased
instance UnsafeFromData GenesisHash where
  unsafeFromBuiltinData = GenesisHash . unsafeFromBuiltinData

PlutusTx.makeLift ''GenesisHash

{- | Parameters uniquely identifying a sidechain

 = Note

 The 'Data' serializations for this type /cannot/ change.
-}
data SidechainParams = SidechainParams
  { chainId :: Integer
  , genesisHash :: GenesisHash
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
    ( -- | @since Unreleased
      TSPrelude.Eq
    , -- | @since Unreleased
      TSPrelude.Show
    )

PlutusTx.makeLift ''SidechainParams
PlutusTx.makeIsDataIndexed ''SidechainParams [('SidechainParams, 0)]

-- | @since Unreleased
instance HasField "chainId" SidechainParams Integer where
  {-# INLINE get #-}
  get = chainId
  {-# INLINE modify #-}
  modify f sp = sp {chainId = f (chainId sp)}

-- | @since Unreleased
instance HasField "genesisHash" SidechainParams GenesisHash where
  {-# INLINE get #-}
  get = genesisHash
  {-# INLINE modify #-}
  modify f sp = sp {genesisHash = f (genesisHash sp)}

-- | @since Unreleased
instance HasField "genesisUtxo" SidechainParams TxOutRef where
  {-# INLINE get #-}
  get = genesisUtxo
  {-# INLINE modify #-}
  modify f sp = sp {genesisUtxo = f (genesisUtxo sp)}

-- | @since Unreleased
instance HasField "thresholdNumerator" SidechainParams Integer where
  {-# INLINE get #-}
  get = thresholdNumerator
  {-# INLINE modify #-}
  modify f sp = sp {thresholdNumerator = f (thresholdNumerator sp)}

-- | @since Unreleased
instance HasField "thresholdDenominator" SidechainParams Integer where
  {-# INLINE get #-}
  get = thresholdDenominator
  {-# INLINE modify #-}
  modify f sp = sp {thresholdDenominator = f (thresholdDenominator sp)}

{- | Compressed DER SECP256k1 public key.
 = Important note

 The 'Data' serializations for this type /cannot/ change.
-}
newtype EcdsaSecp256k1PubKey = EcdsaSecp256k1PubKey
  { -- | @since Unreleased
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
    ( -- | @since Unreleased
      IsString
    , TSPrelude.Show
    )
    via LedgerBytes

-- * Committee Candidate Validator data

{- | 'CandidatePermissionMint' is used to parameterize the minting policy in
 'TrustlessSidechain.CommitteeCandidateMintingPolicy'.
-}
data CandidatePermissionMint = CandidatePermissionMint
  { -- | @since Unreleased
    sidechainParams :: SidechainParams
  , -- | @since Unreleased
    utxo :: TxOutRef
  }
  deriving stock
    ( -- | @since Unreleased
      TSPrelude.Eq
    , -- | @since Unreleased
      TSPrelude.Show
    )

-- | @since Unreleased
instance ToData CandidatePermissionMint where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (CandidatePermissionMint {..}) =
    productToData2 sidechainParams utxo

-- | @since Unreleased
instance FromData CandidatePermissionMint where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 CandidatePermissionMint

-- | @since Unreleased
instance UnsafeFromData CandidatePermissionMint where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 CandidatePermissionMint

-- | @since Unreleased
instance HasField "sidechainParams" CandidatePermissionMint SidechainParams where
  {-# INLINE get #-}
  get (CandidatePermissionMint sp _) = sp
  {-# INLINE modify #-}
  modify f (CandidatePermissionMint sp u) = CandidatePermissionMint (f sp) u

-- | @since Unreleased
instance HasField "utxo" CandidatePermissionMint TxOutRef where
  {-# INLINE get #-}
  get (CandidatePermissionMint _ u) = u
  {-# INLINE modify #-}
  modify f (CandidatePermissionMint sp u) = CandidatePermissionMint sp (f u)

{-
 The 'Data' serializations for this type /cannot/ change.
-}
data BlockProducerRegistration = BlockProducerRegistration
  { -- | SPO cold verification key hash
    -- | @since Unreleased
    spoPubKey :: PubKey -- own cold verification key hash
  , -- | public key in the sidechain's desired format
    sidechainPubKey :: LedgerBytes
  , -- | Signature of the SPO
    -- | @since Unreleased
    spoSignature :: Signature
  , -- | Signature of the sidechain
    -- | @since Unreleased
    sidechainSignature :: Signature
  , -- | A UTxO that must be spent by the transaction
    -- | @since Unreleased
    inputUtxo :: TxOutRef
  , -- | Owner public key hash
    -- | @since Unreleased
    ownPkh :: PubKeyHash
  }
  deriving stock
    ( -- | @since Unreleased
      TSPrelude.Eq
    , -- | @since Unreleased
      TSPrelude.Show
    )

PlutusTx.makeIsDataIndexed ''BlockProducerRegistration [('BlockProducerRegistration, 0)]

-- | @since Unreleased
instance HasField "spoPubKey" BlockProducerRegistration PubKey where
  {-# INLINE get #-}
  get (BlockProducerRegistration x _ _ _ _ _) = x
  {-# INLINE modify #-}
  modify f (BlockProducerRegistration sPK scPK sS scS u pkh) =
    BlockProducerRegistration (f sPK) scPK sS scS u pkh

-- | @since Unreleased
instance HasField "ecdsaSecp256k1PubKey" BlockProducerRegistration LedgerBytes where
  {-# INLINE get #-}
  get (BlockProducerRegistration _ x _ _ _ _) = x
  {-# INLINE modify #-}
  modify f (BlockProducerRegistration sPK scPK sS scS u pkh) =
    BlockProducerRegistration sPK (f scPK) sS scS u pkh

-- | @since Unreleased
instance HasField "spoSignature" BlockProducerRegistration Signature where
  {-# INLINE get #-}
  get (BlockProducerRegistration _ _ x _ _ _) = x
  {-# INLINE modify #-}
  modify f (BlockProducerRegistration sPK scPK sS scS u pkh) =
    BlockProducerRegistration sPK scPK (f sS) scS u pkh

-- | @since Unreleased
instance HasField "sidechainSignature" BlockProducerRegistration Signature where
  {-# INLINE get #-}
  get (BlockProducerRegistration _ _ _ x _ _) = x
  {-# INLINE modify #-}
  modify f (BlockProducerRegistration sPK scPK sS scS u pkh) =
    BlockProducerRegistration sPK scPK sS (f scS) u pkh

-- | @since Unreleased
instance HasField "inputUtxo" BlockProducerRegistration TxOutRef where
  {-# INLINE get #-}
  get (BlockProducerRegistration _ _ _ _ x _) = x
  {-# INLINE modify #-}
  modify f (BlockProducerRegistration sPK scPK sS scS u pkh) =
    BlockProducerRegistration sPK scPK sS scS (f u) pkh

-- | @since Unreleased
instance HasField "ownPkh" BlockProducerRegistration PubKeyHash where
  {-# INLINE get #-}
  get (BlockProducerRegistration _ _ _ _ _ x) = x
  {-# INLINE modify #-}
  modify f (BlockProducerRegistration sPK scPK sS scS u pkh) =
    BlockProducerRegistration sPK scPK sS scS u (f pkh)

{- | = Important note

 The 'Data' serializations for this type /cannot/ change.
-}
data BlockProducerRegistrationMsg = BlockProducerRegistrationMsg
  { sidechainParams :: SidechainParams
  , sidechainPubKey :: LedgerBytes
  , -- | A UTxO that must be spent by the transaction
    -- | @since Unreleased
    inputUtxo :: TxOutRef
  }
  deriving stock
    ( -- | @since Unreleased
      TSPrelude.Eq
    , -- | @since Unreleased
      TSPrelude.Show
    )

PlutusTx.makeIsDataIndexed ''BlockProducerRegistrationMsg [('BlockProducerRegistrationMsg, 0)]

-- | @since Unreleased
instance HasField "sidechainParams" BlockProducerRegistrationMsg SidechainParams where
  {-# INLINE get #-}
  get (BlockProducerRegistrationMsg x _ _) = x
  {-# INLINE modify #-}
  modify f (BlockProducerRegistrationMsg sp spk u) =
    BlockProducerRegistrationMsg (f sp) spk u

-- | @since Unreleased
instance HasField "sidechainPubKey" BlockProducerRegistrationMsg LedgerBytes where
  {-# INLINE get #-}
  get (BlockProducerRegistrationMsg _ x _) = x
  {-# INLINE modify #-}
  modify f (BlockProducerRegistrationMsg sp spk u) =
    BlockProducerRegistrationMsg sp (f spk) u

-- | @since Unreleased
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
    -- | @since Unreleased
    index :: Integer
  , -- | 256 bit unsigned integer that represents amount of tokens being sent out of the bridge
    -- | @since Unreleased
    amount :: Integer
  , -- | arbitrary length bytestring that represents decoded bech32 cardano
    -- | address. See [here](https://cips.cardano.org/cips/cip19/) for more details
    -- | of bech32
    -- | @since Unreleased
    recipient :: LedgerBytes
  , -- | the previous merkle root to ensure that the hashed entry is unique
    -- | @since Unreleased
    previousMerkleRoot :: Maybe LedgerBytes
  }
  deriving stock
    ( -- | @since Unreleased
      TSPrelude.Eq
    , -- | @since Unreleased
      TSPrelude.Show
    )

PlutusTx.makeIsDataIndexed ''MerkleTreeEntry [('MerkleTreeEntry, 0)]

-- | @since Unreleased
instance HasField "index" MerkleTreeEntry Integer where
  {-# INLINE get #-}
  get (MerkleTreeEntry x _ _ _) = x
  {-# INLINE modify #-}
  modify f (MerkleTreeEntry i a r pmr) =
    MerkleTreeEntry (f i) a r pmr

-- | @since Unreleased
instance HasField "amount" MerkleTreeEntry Integer where
  {-# INLINE get #-}
  get (MerkleTreeEntry _ x _ _) = x
  {-# INLINE modify #-}
  modify f (MerkleTreeEntry i a r pmr) =
    MerkleTreeEntry i (f a) r pmr

-- | @since Unreleased
instance HasField "recipient" MerkleTreeEntry LedgerBytes where
  {-# INLINE get #-}
  get (MerkleTreeEntry _ _ x _) = x
  {-# INLINE modify #-}
  modify f (MerkleTreeEntry i a r pmr) =
    MerkleTreeEntry i a (f r) pmr

-- | @since Unreleased
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
  { -- | @since Unreleased
    sidechainParams :: SidechainParams
  , -- | @since Unreleased
    merkleRoot :: LedgerBytes
  , -- | @since Unreleased
    previousMerkleRoot :: Maybe LedgerBytes
  }
  deriving stock
    ( -- | @since Unreleased
      TSPrelude.Eq
    , -- | @since Unreleased
      TSPrelude.Show
    )

PlutusTx.makeIsDataIndexed ''MerkleRootInsertionMessage [('MerkleRootInsertionMessage, 0)]

-- | @since Unreleased
instance HasField "sidechainParams" MerkleRootInsertionMessage SidechainParams where
  {-# INLINE get #-}
  get (MerkleRootInsertionMessage x _ _) = x
  {-# INLINE modify #-}
  modify f (MerkleRootInsertionMessage sp mr pmr) =
    MerkleRootInsertionMessage (f sp) mr pmr

-- | @since Unreleased
instance HasField "merkleRoot" MerkleRootInsertionMessage LedgerBytes where
  {-# INLINE get #-}
  get (MerkleRootInsertionMessage _ x _) = x
  {-# INLINE modify #-}
  modify f (MerkleRootInsertionMessage sp mr pmr) =
    MerkleRootInsertionMessage sp (f mr) pmr

-- | @since Unreleased
instance HasField "previousMerkleRoot" MerkleRootInsertionMessage (Maybe LedgerBytes) where
  {-# INLINE get #-}
  get (MerkleRootInsertionMessage _ _ x) = x
  {-# INLINE modify #-}
  modify f (MerkleRootInsertionMessage sp mr pmr) =
    MerkleRootInsertionMessage sp mr (f pmr)

{- | 'SignedMerkleRootRedeemer' is the redeemer for the signed merkle root
 minting policy.

 @since Unreleased
-}
newtype SignedMerkleRootRedeemer = SignedMerkleRootRedeemer
  { previousMerkleRoot :: Maybe LedgerBytes
  }
  deriving newtype
    ( -- | @since Unreleased
      ToData
    , -- | @since Unreleased
      FromData
    , -- | @since Unreleased
      UnsafeFromData
    )
  deriving stock
    ( -- | @since Unreleased
      TSPrelude.Eq
    , -- | @since Unreleased
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
  { -- | @since Unreleased
    transaction :: MerkleTreeEntry
  , -- | @since Unreleased
    merkleProof :: MerkleProof
  }
  deriving stock
    ( -- | @since Unreleased
      TSPrelude.Eq
    , -- | @since Unreleased
      TSPrelude.Show
    )

PlutusTx.makeIsDataIndexed ''CombinedMerkleProof [('CombinedMerkleProof, 0)]

-- | @since Unreleased
instance HasField "transaction" CombinedMerkleProof MerkleTreeEntry where
  {-# INLINE get #-}
  get (CombinedMerkleProof x _) = x
  {-# INLINE modify #-}
  modify f (CombinedMerkleProof t mp) =
    CombinedMerkleProof (f t) mp

-- | @since Unreleased
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
data FUELMintingRedeemer = FUELMintingRedeemer MerkleTreeEntry MerkleProof

-- | @since Unreleased
PlutusTx.makeIsDataIndexed ''FUELMintingRedeemer [('FUELMintingRedeemer, 0)]

-- * Update Committee Hash data

{- | Datum for the committee. This is used to verify
 signatures for sidechain to mainchain transfers.

 The actual representation of the committee's public key depends on the ATMS
 implementation.

 @since Unreleased
-}
data UpdateCommitteeDatum aggregatePubKeys = UpdateCommitteeDatum
  { aggregateCommitteePubKeys :: aggregatePubKeys
  , sidechainEpoch :: Integer
  }
  deriving stock
    ( -- | @since Unreleased
      TSPrelude.Eq
    , -- | @since Unreleased
      TSPrelude.Show
    )

-- | @since Unreleased
instance ToData aggregatePubKeys => ToData (UpdateCommitteeDatum aggregatePubKeys) where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (UpdateCommitteeDatum {..}) =
    productToData2 aggregateCommitteePubKeys sidechainEpoch

-- | @since Unreleased
instance FromData aggregatePubKeys => FromData (UpdateCommitteeDatum aggregatePubKeys) where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 UpdateCommitteeDatum

-- | @since Unreleased
instance UnsafeFromData aggregatePubKeys => UnsafeFromData (UpdateCommitteeDatum aggregatePubKeys) where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 UpdateCommitteeDatum

-- | @since Unreleased
instance HasField "aggregateCommitteePubKeys" (UpdateCommitteeDatum aggregatePubKeys) aggregatePubKeys where
  {-# INLINE get #-}
  get (UpdateCommitteeDatum x _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeDatum ch se) =
    UpdateCommitteeDatum (f ch) se

-- | @since Unreleased
instance HasField "sidechainEpoch" (UpdateCommitteeDatum aggregatePubKeys) Integer where
  {-# INLINE get #-}
  get (UpdateCommitteeDatum _ x) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeDatum ch se) =
    UpdateCommitteeDatum ch (f se)

-- | @since Unreleased
newtype ATMSPlainAggregatePubKey = ATMSPlainAggregatePubKey LedgerBytes
  deriving newtype
    ( -- | @since Unreleased
      FromData
    , -- | @since Unreleased
      ToData
    , -- | @since Unreleased
      UnsafeFromData
    , -- | @since Unreleased
      Eq
    , -- | @since Unreleased
      Ord
    , -- | @since Unreleased
      IsString
    )
  deriving stock
    ( -- | @since Unreleased
      TSPrelude.Eq
    , -- | @since Unreleased
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
    ( -- | @since Unreleased
      TSPrelude.Eq
    , -- | @since Unreleased
      TSPrelude.Show
    )

-- | @since Unreleased
instance ToData UpdateCommitteeHash where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (UpdateCommitteeHash {..}) =
    productToData4
      sidechainParams
      committeeOracleCurrencySymbol
      committeeCertificateVerificationCurrencySymbol
      mptRootTokenCurrencySymbol

-- | @since Unreleased
instance HasField "sidechainParams" UpdateCommitteeHash SidechainParams where
  {-# INLINE get #-}
  get (UpdateCommitteeHash x _ _ _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHash sp cocs ccvcs rtcs) =
    UpdateCommitteeHash (f sp) cocs ccvcs rtcs

-- | @since Unreleased
instance HasField "committeeOracleCurrencySymbol" UpdateCommitteeHash CurrencySymbol where
  {-# INLINE get #-}
  get (UpdateCommitteeHash _ x _ _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHash sp cocs ccvcs rtcs) =
    UpdateCommitteeHash sp (f cocs) ccvcs rtcs

-- | @since Unreleased
instance HasField "committeeCertificateVerificationCurrencySymbol" UpdateCommitteeHash CurrencySymbol where
  {-# INLINE get #-}
  get (UpdateCommitteeHash _ _ x _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHash sp cocs ccvcs rtcs) =
    UpdateCommitteeHash sp cocs (f ccvcs) rtcs

-- | @since Unreleased
instance HasField "mptRootTokenCurrencySymbol" UpdateCommitteeHash CurrencySymbol where
  {-# INLINE get #-}
  get (UpdateCommitteeHash _ _ _ x) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHash sp cocs ccvcs rtcs) =
    UpdateCommitteeHash sp cocs ccvcs (f rtcs)

instance FromData UpdateCommitteeHash where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData4 UpdateCommitteeHash

-- | @since Unreleased
instance UnsafeFromData UpdateCommitteeHash where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData4 UpdateCommitteeHash

{- | = Important note

 The 'Data' serializations for this type /cannot/ be changed.

 @since Unreleased
-}
data UpdateCommitteeHashMessage aggregatePubKeys = UpdateCommitteeHashMessage
  { sidechainParams :: SidechainParams
  , -- | 'newCommitteePubKeys' is the new aggregate committee public keys
    -- | @since Unreleased
    newAggregateCommitteePubKeys :: aggregatePubKeys
  , previousMerkleRoot :: Maybe LedgerBytes
  , sidechainEpoch :: Integer
  , validatorAddress :: Address
  }
  deriving stock
    ( -- | @since Unreleased
      TSPrelude.Eq
    , -- | @since Unreleased
      TSPrelude.Show
    )

-- | @since Unreleased
instance ToData aggregatePubKeys => ToData (UpdateCommitteeHashMessage aggregatePubKeys) where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (UpdateCommitteeHashMessage {..}) =
    productToData5 sidechainParams newAggregateCommitteePubKeys previousMerkleRoot sidechainEpoch validatorAddress

-- | @since Unreleased
instance FromData aggregatePubKeys => FromData (UpdateCommitteeHashMessage aggregatePubKeys) where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData5 UpdateCommitteeHashMessage

-- | @since Unreleased
instance UnsafeFromData aggregatePubKeys => UnsafeFromData (UpdateCommitteeHashMessage aggregatePubKeys) where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData5 UpdateCommitteeHashMessage

-- | @since Unreleased
instance HasField "sidechainParams" (UpdateCommitteeHashMessage aggregatePubKeys) SidechainParams where
  {-# INLINE get #-}
  get (UpdateCommitteeHashMessage x _ _ _ _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashMessage sp nacpks pmr se va) =
    UpdateCommitteeHashMessage (f sp) nacpks pmr se va

-- | @since Unreleased
instance HasField "newAggregateCommitteePubKeys" (UpdateCommitteeHashMessage aggregatePubKeys) aggregatePubKeys where
  {-# INLINE get #-}
  get (UpdateCommitteeHashMessage _ x _ _ _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashMessage sp nacpks pmr se va) =
    UpdateCommitteeHashMessage sp (f nacpks) pmr se va

-- | @since Unreleased
instance HasField "previousMerkleRoot" (UpdateCommitteeHashMessage aggregatePubKeys) (Maybe LedgerBytes) where
  {-# INLINE get #-}
  get (UpdateCommitteeHashMessage _ _ x _ _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashMessage sp nacpks pmr se va) =
    UpdateCommitteeHashMessage sp nacpks (f pmr) se va

-- | @since Unreleased
instance HasField "sidechainEpoch" (UpdateCommitteeHashMessage aggregatePubKeys) Integer where
  {-# INLINE get #-}
  get (UpdateCommitteeHashMessage _ _ _ x _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashMessage sp nacpks pmr se va) =
    UpdateCommitteeHashMessage sp nacpks pmr (f se) va

-- | @since Unreleased
instance HasField "validatorAddress" (UpdateCommitteeHashMessage aggregatePubKeys) Address where
  {-# INLINE get #-}
  get (UpdateCommitteeHashMessage _ _ _ _ x) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashMessage sp nacpks pmr se va) =
    UpdateCommitteeHashMessage sp nacpks pmr se (f va)

-- | @since Unreleased
newtype UpdateCommitteeHashRedeemer = UpdateCommitteeHashRedeemer
  { previousMerkleRoot :: Maybe LedgerBytes
  }
  deriving newtype
    ( -- | @since Unreleased
      ToData
    , -- | @since Unreleased
      FromData
    , -- | @since Unreleased
      UnsafeFromData
    )
  deriving stock
    ( -- | @since Unreleased
      TSPrelude.Eq
    , -- | @since Unreleased
      TSPrelude.Show
    )

-- | Datum for a checkpoint
data CheckpointDatum = CheckpointDatum
  { -- | @since Unreleased
    blockHash :: LedgerBytes
  , -- | @since Unreleased
    blockNumber :: Integer
  }

-- | @since Unreleased
instance ToData CheckpointDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (CheckpointDatum {..}) =
    productToData2 blockHash blockNumber

-- | @since Unreleased
instance FromData CheckpointDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 CheckpointDatum

-- | @since Unreleased
instance UnsafeFromData CheckpointDatum where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 CheckpointDatum

-- | @since Unreleased
instance HasField "blockHash" CheckpointDatum LedgerBytes where
  {-# INLINE get #-}
  get (CheckpointDatum x _) = x
  {-# INLINE modify #-}
  modify f (CheckpointDatum bh bn) =
    CheckpointDatum (f bh) bn

-- | @since Unreleased
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
    ( -- | @since Unreleased
      TSPrelude.Eq
    , -- | @since Unreleased
      TSPrelude.Show
    )

PlutusTx.makeIsDataIndexed ''CommitteeCertificateMint [('CommitteeCertificateMint, 0)]

{- | 'ATMSPlainMultisignature' corresponds to SIP05 in @docs/SIPs/@.
 This is used as redeemer for the
 "TrustlessSidechain.CommitteePlainATMSPolicy".

 @since Unreleased
-}
data ATMSPlainMultisignature = ATMSPlainMultisignature
  { plainPublicKeys :: [LedgerBytes]
  , plainSignatures :: [LedgerBytes]
  }
  deriving stock
    ( -- | @since Unreleased
      TSPrelude.Eq
    , -- | @since Unreleased
      TSPrelude.Show
    )

PlutusTx.makeIsDataIndexed ''ATMSPlainMultisignature [('ATMSPlainMultisignature, 0)]

{- | The Redeemer that is passed to the on-chain validator to update the
 checkpoint
-}
data CheckpointRedeemer = CheckpointRedeemer
  { newCheckpointBlockHash :: LedgerBytes
  , newCheckpointBlockNumber :: Integer
  }

-- | @since Unreleased
instance ToData CheckpointRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (CheckpointRedeemer {..}) =
    productToData2
      newCheckpointBlockHash
      newCheckpointBlockNumber

-- | @since Unreleased
instance FromData CheckpointRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 CheckpointRedeemer

-- | @since Unreleased
instance UnsafeFromData CheckpointRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 CheckpointRedeemer

-- | @since Unreleased
instance HasField "newCheckpointBlockHash" CheckpointRedeemer LedgerBytes where
  {-# INLINE get #-}
  get (CheckpointRedeemer x _) = x
  {-# INLINE modify #-}
  modify f (CheckpointRedeemer ncbh ncbn) =
    CheckpointRedeemer (f ncbh) ncbn

-- | @since Unreleased
instance HasField "newCheckpointBlockNumber" CheckpointRedeemer Integer where
  {-# INLINE get #-}
  get (CheckpointRedeemer _ x) = x
  {-# INLINE modify #-}
  modify f (CheckpointRedeemer ncbh ncbn) =
    CheckpointRedeemer ncbh (f ncbn)

{- | 'Checkpoint' is used as the parameter for the validator.

 @since Unreleased
-}
data CheckpointParameter = CheckpointParameter
  { -- | @since Unreleased
    sidechainParams :: SidechainParams
  , -- | The 'AssetClass' of the NFT that is used to
    -- identify the transaction.
    --
    -- @since Unreleased
    assetClass :: AssetClass
  , -- | The currency symbol which uniquely identifies the
    -- current committee.
    --
    -- @since Unreleased
    committeeOracleCurrencySymbol :: CurrencySymbol
  , -- | The currency symbol of the committee certificate verification minting
    -- policy.
    --
    -- @since Unreleased
    committeeCertificateVerificationCurrencySymbol :: CurrencySymbol
  }
  deriving stock
    ( -- | @since Unreleased
      TSPrelude.Eq
    , -- | @since Unreleased
      TSPrelude.Show
    )

-- | @since Unreleased
instance ToData CheckpointParameter where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (CheckpointParameter {..}) =
    productToData4
      sidechainParams
      assetClass
      committeeOracleCurrencySymbol
      committeeCertificateVerificationCurrencySymbol

-- | @since Unreleased
instance HasField "sidechainParams" CheckpointParameter SidechainParams where
  {-# INLINE get #-}
  get (CheckpointParameter x _ _ _) = x
  {-# INLINE modify #-}
  modify f (CheckpointParameter csp cac ccocs chac) =
    CheckpointParameter (f csp) cac ccocs chac

-- | @since Unreleased
instance HasField "assetClass" CheckpointParameter AssetClass where
  {-# INLINE get #-}
  get (CheckpointParameter _ x _ _) = x
  {-# INLINE modify #-}
  modify f (CheckpointParameter csp cac ccocs chac) =
    CheckpointParameter csp (f cac) ccocs chac

-- | @since Unreleased
instance HasField "committeeOracleCurrencySymbol" CheckpointParameter CurrencySymbol where
  {-# INLINE get #-}
  get (CheckpointParameter _ _ x _) = x
  {-# INLINE modify #-}
  modify f (CheckpointParameter csp cac ccocs chac) =
    CheckpointParameter csp cac (f ccocs) chac

-- | @since Unreleased
instance HasField "committeeCertificateVerificationCurrencySymbol" CheckpointParameter CurrencySymbol where
  {-# INLINE get #-}
  get (CheckpointParameter _ _ _ x) = x
  {-# INLINE modify #-}
  modify f (CheckpointParameter csp cac ccocs chac) =
    CheckpointParameter csp cac ccocs (f chac)

-- | @since Unreleased
instance FromData CheckpointParameter where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData4 CheckpointParameter

-- | @since Unreleased
instance UnsafeFromData CheckpointParameter where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData4 CheckpointParameter

{- | = Important note

 The 'Data' serializations of this type /cannot/ be changed.
-}
data CheckpointMessage = CheckpointMessage
  { -- | @since Unreleased
    sidechainParams :: SidechainParams
  , -- | @since Unreleased
    blockHash :: LedgerBytes
  , -- | @since Unreleased
    blockNumber :: Integer
  , -- | @since Unreleased
    sidechainEpoch :: Integer
  }

PlutusTx.makeIsDataIndexed ''CheckpointMessage [('CheckpointMessage, 0)]

-- | @since Unreleased
instance HasField "sidechainParams" CheckpointMessage SidechainParams where
  {-# INLINE get #-}
  get (CheckpointMessage x _ _ _) = x
  {-# INLINE modify #-}
  modify f (CheckpointMessage sp bh bn se) =
    CheckpointMessage (f sp) bh bn se

-- | @since Unreleased
instance HasField "blockHash" CheckpointMessage LedgerBytes where
  {-# INLINE get #-}
  get (CheckpointMessage _ x _ _) = x
  {-# INLINE modify #-}
  modify f (CheckpointMessage sp bh bn se) =
    CheckpointMessage sp (f bh) bn se

-- | @since Unreleased
instance HasField "blockNumber" CheckpointMessage Integer where
  {-# INLINE get #-}
  get (CheckpointMessage _ _ x _) = x
  {-# INLINE modify #-}
  modify f (CheckpointMessage sp bh bn se) =
    CheckpointMessage sp bh (f bn) se

-- | @since Unreleased
instance HasField "sidechainEpoch" CheckpointMessage Integer where
  {-# INLINE get #-}
  get (CheckpointMessage _ _ _ x) = x
  {-# INLINE modify #-}
  modify f (CheckpointMessage sp bh bn se) =
    CheckpointMessage sp bh bn (f se)
