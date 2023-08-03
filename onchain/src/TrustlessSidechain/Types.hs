{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TrustlessSidechain.Types where

import Ledger.Crypto (PubKey, PubKeyHash, Signature)
import Ledger.Value (AssetClass, CurrencySymbol)
import Plutus.V2.Ledger.Api (LedgerBytes (LedgerBytes), ValidatorHash)
import Plutus.V2.Ledger.Tx (TxOutRef)
import PlutusTx (makeIsDataIndexed)
import TrustlessSidechain.HaskellPrelude qualified as TSPrelude
import TrustlessSidechain.MerkleTree (MerkleProof)
import TrustlessSidechain.PlutusPrelude

-- * Sidechain Parametrization and general data

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
  }

newtype GenesisHash = GenesisHash {getGenesisHash :: LedgerBytes}
  deriving stock (TSPrelude.Eq, TSPrelude.Ord)
  deriving newtype
    ( Eq
    , Ord
    , ToData
    , FromData
    , UnsafeFromData
    )
  deriving (IsString, TSPrelude.Show) via LedgerBytes

makeIsDataIndexed ''SidechainParams [('SidechainParams, 0)]

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

-- | Endpoint parameters for committee candidate registration
data RegisterParams = RegisterParams
  { sidechainParams :: SidechainParams
  , spoPubKey :: PubKey
  , -- | @since Unreleased
    sidechainPubKey :: EcdsaSecp256k1PubKey
  , spoSig :: Signature
  , sidechainSig :: Signature
  , inputUtxo :: TxOutRef
  }

{- | 'CandidatePermissionMint' is used to parameterize the minting policy in
 'TrustlessSidechain.CommitteeCandidateMintingPolicy'.
-}
data CandidatePermissionMint = CandidatePermissionMint
  { cpmSidechainParams :: SidechainParams
  , cpmUtxo :: TxOutRef
  }

-- | @since Unreleased
instance ToData CandidatePermissionMint where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (CandidatePermissionMint {..}) =
    productToData2 cpmSidechainParams cpmUtxo

-- | @since Unreleased
instance FromData CandidatePermissionMint where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 CandidatePermissionMint

-- | @since Unreleased
instance UnsafeFromData CandidatePermissionMint where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 CandidatePermissionMint

-- | Endpoint parameters for committee candidate deregistration
data DeregisterParams = DeregisterParams
  { sidechainParams :: SidechainParams
  , spoPubKey :: PubKey
  }

{- | = Important note

 The 'Data' serializations for this type /cannot/ change.
-}
data BlockProducerRegistration = BlockProducerRegistration
  { -- | SPO cold verification key hash
    bprSpoPubKey :: PubKey -- own cold verification key hash
  , -- | public key in the sidechain's desired format
    -- | @since Unreleased
    bprEcdsaSecp256k1PubKey :: EcdsaSecp256k1PubKey
  , -- | Signature of the SPO
    bprSpoSignature :: Signature
  , -- | Signature of the SPO
    bprSidechainSignature :: Signature
  , -- | A UTxO that must be spent by the transaction
    bprInputUtxo :: TxOutRef
  , -- | Owner public key hash
    bprOwnPkh :: PubKeyHash
  }

PlutusTx.makeIsDataIndexed ''BlockProducerRegistration [('BlockProducerRegistration, 0)]

{- | = Important note

 The 'Data' serializations for this type /cannot/ change.
-}
data BlockProducerRegistrationMsg = BlockProducerRegistrationMsg
  { bprmSidechainParams :: SidechainParams
  , -- | @since Unreleased
    bprmEcdsaSecp256k1PubKey :: EcdsaSecp256k1PubKey
  , -- | A UTxO that must be spent by the transaction
    bprmInputUtxo :: TxOutRef
  }

PlutusTx.makeIsDataIndexed ''BlockProducerRegistrationMsg [('BlockProducerRegistrationMsg, 0)]

-- * Merkle Root Token data

{- | 'MerkleTreeEntry' (abbr. mte and pl. mtes) is the data which are the elements in the merkle tree
 for the MerkleRootToken.

 = Important note

 The 'Data' serializations for this type /cannot/ change.
-}
data MerkleTreeEntry = MerkleTreeEntry
  { -- | 32 bit unsigned integer, used to provide uniqueness among transactions within the tree
    mteIndex :: Integer
  , -- | 256 bit unsigned integer that represents amount of tokens being sent out of the bridge
    mteAmount :: Integer
  , -- | arbitrary length bytestring that represents decoded bech32 cardano
    -- address. See [here](https://cips.cardano.org/cips/cip19/) for more details
    -- of bech32
    mteRecipient :: LedgerBytes
  , -- | the previous merkle root to ensure that the hashed entry is unique
    mtePreviousMerkleRoot :: Maybe LedgerBytes
  }

PlutusTx.makeIsDataIndexed ''MerkleTreeEntry [('MerkleTreeEntry, 0)]

{- | 'MerkleRootInsertionMessage' is a data type for which committee members
 create signatures for
 >  blake2b(cbor(MerkleRootInsertionMessage))

 = Important note

 The 'Data' serializations for this type /cannot/ change.
-}
data MerkleRootInsertionMessage = MerkleRootInsertionMessage
  { mrimSidechainParams :: SidechainParams
  , mrimMerkleRoot :: LedgerBytes
  , mrimPreviousMerkleRoot :: Maybe LedgerBytes
  }

PlutusTx.makeIsDataIndexed ''MerkleRootInsertionMessage [('MerkleRootInsertionMessage, 0)]

-- | 'SignedMerkleRoot' is the redeemer for the Merkle root token minting policy
data SignedMerkleRoot = SignedMerkleRoot
  { -- | New merkle root to insert.
    merkleRoot :: LedgerBytes
  , -- | Previous merkle root (if it exists)
    previousMerkleRoot :: Maybe LedgerBytes
  , -- | Current committee signatures ordered as their corresponding keys
    signatures :: [LedgerBytes]
  , -- | Lexicographically sorted public keys of all committee members
    committeePubKeys :: [EcdsaSecp256k1PubKey]
  }

-- | @since Unreleased
instance ToData SignedMerkleRoot where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (SignedMerkleRoot {..}) =
    productToData4 merkleRoot previousMerkleRoot signatures committeePubKeys

-- | @since Unreleased
instance FromData SignedMerkleRoot where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData4 SignedMerkleRoot

-- | @since Unreleased
instance UnsafeFromData SignedMerkleRoot where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData4 SignedMerkleRoot

-- | 'SignedMerkleRootMint' is used to parameterize 'mkMintingPolicy'.
data SignedMerkleRootMint = SignedMerkleRootMint
  { -- | 'smrmSidechainParams' includes the 'SidechainParams'
    smrmSidechainParams :: SidechainParams
  , -- | 'smrmUpdateCommitteeHashCurrencySymbol' is the 'CurrencySymbol' which
    -- identifies the utxo for which the 'UpdateCommitteeHashDatum'
    -- resides.
    smrmUpdateCommitteeHashCurrencySymbol :: CurrencySymbol
  , -- | 'smrmValidatorHash' is the validator hash corresponding to
    -- 'TrustlessSidechain.MerkleRootTokenValidator.mkMptRootTokenValidator'
    -- to ensure that this token gets minted to the "right" place.
    smrmValidatorHash :: ValidatorHash
  }

-- | @since Unreleased
instance ToData SignedMerkleRootMint where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (SignedMerkleRootMint {..}) =
    productToData3
      smrmSidechainParams
      smrmUpdateCommitteeHashCurrencySymbol
      smrmValidatorHash

-- | @since Unreleased
instance FromData SignedMerkleRootMint where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData3 SignedMerkleRootMint

-- | @since Unreleased
instance UnsafeFromData SignedMerkleRootMint where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData3 SignedMerkleRootMint

{- | 'CombinedMerkleProof' is a product type to include both the
 'MerkleTreeEntry' and the 'MerkleProof'.

 This exists as for testing in #249.

 = Important note

 The 'Data' serializations of this type /cannot/ change.
-}
data CombinedMerkleProof = CombinedMerkleProof
  { cmpTransaction :: MerkleTreeEntry
  , cmpMerkleProof :: MerkleProof
  }

PlutusTx.makeIsDataIndexed ''CombinedMerkleProof [('CombinedMerkleProof, 0)]

-- * FUEL Minting Policy data

-- | The Redeemer that's to be passed to onchain policy, indicating its mode of usage.
data FUELRedeemer
  = MainToSide LedgerBytes -- Recipient's sidechain address
  | -- | 'SideToMain' indicates that we wish to mint FUEL on the mainchain.
    -- So, this includes which transaction in the sidechain we are
    -- transferring over to the main chain (hence the 'MerkleTreeEntry'), and
    -- the proof tha this actually happened on the sidechain (hence the
    -- 'MerkleProof')
    SideToMain MerkleTreeEntry MerkleProof

-- Recipient address is in FUELRedeemer just for reference on the mainchain,
-- it's actually useful (and verified) on the sidechain, so it needs to be
-- recorded in the mainchain.

PlutusTx.makeIsDataIndexed ''FUELRedeemer [('MainToSide, 0), ('SideToMain, 1)]

{- | 'FUELMint' is the data type to parameterize the minting policy. See
 'mkMintingPolicy' for details of why we need the datum in 'FUELMint'
-}
data FUELMint = FUELMint
  { -- 'fmMptRootTokenValidator' is the hash of the validator script
    -- which /should/ have a token which has the merkle root in the token
    -- name. See 'TrustlessSidechain.MerkleRootTokenValidator' for
    -- details.
    -- > fmMptRootTokenValidator :: ValidatorHash
    -- N.B. We don't need this! We're really only interested in the token,
    -- and indeed; anyone can pay a token to this script so there really
    -- isn't a reason to use this validator script as the "identifier" for
    -- MerkleRootTokens.

    -- | 'fmMptRootTokenCurrencySymbol' is the 'CurrencySymbol' of a token
    -- which contains a merkle root in the 'TokenName'. See
    -- 'TrustlessSidechain.MerkleRootTokenMintingPolicy' for details.
    fmMptRootTokenCurrencySymbol :: CurrencySymbol
  , -- | 'fmSidechainParams' is the sidechain parameters
    fmSidechainParams :: SidechainParams
  , -- | 'fmDsKeyCurrencySymbol' is th currency symbol for the tokens which
    -- hold the key for the distributed set. In particular, this allows the
    -- FUEL minting policy to verify if a string has /just been inserted/ into
    -- the distributed set.
    fmDsKeyCurrencySymbol :: CurrencySymbol
  }

-- | @since Unreleased
instance ToData FUELMint where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (FUELMint {..}) =
    productToData3
      fmMptRootTokenCurrencySymbol
      fmSidechainParams
      fmDsKeyCurrencySymbol

-- | @since Unreleased
instance FromData FUELMint where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData3 FUELMint

-- | @since Unreleased
instance UnsafeFromData FUELMint where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData3 FUELMint

-- * Update Committee Hash data

{- | Datum for the committee hash. This /committee hash/ is used to verify
 signatures for sidechain to mainchain transfers. This is a hash of
 concatenated public key hashes of the committee members
-}
data UpdateCommitteeHashDatum = UpdateCommitteeHashDatum
  { committeeHash :: LedgerBytes
  , sidechainEpoch :: Integer
  }

-- | @since Unreleased
instance ToData UpdateCommitteeHashDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (UpdateCommitteeHashDatum {..}) =
    productToData2 committeeHash sidechainEpoch

-- | @since Unreleased
instance FromData UpdateCommitteeHashDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 UpdateCommitteeHashDatum

-- | @since Unreleased
instance UnsafeFromData UpdateCommitteeHashDatum where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 UpdateCommitteeHashDatum

{- | The Redeemer that is passed to the on-chain validator to update the
 committee
-}
data UpdateCommitteeHashRedeemer = UpdateCommitteeHashRedeemer
  { -- | The current committee's signatures for the @'aggregateKeys' 'newCommitteePubKeys'@
    committeeSignatures :: [LedgerBytes]
  , -- | 'committeePubKeys' is the current committee public keys
    committeePubKeys :: [EcdsaSecp256k1PubKey]
  , -- | 'newCommitteePubKeys' is the hash of the new committee
    newCommitteePubKeys :: [EcdsaSecp256k1PubKey]
  , -- | 'previousMerkleRoot' is the previous merkle root (if it exists)
    previousMerkleRoot :: Maybe LedgerBytes
  }

-- | @since Unreleased
instance ToData UpdateCommitteeHashRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (UpdateCommitteeHashRedeemer {..}) =
    productToData4
      committeeSignatures
      committeePubKeys
      newCommitteePubKeys
      previousMerkleRoot

-- | @since Unreleased
instance FromData UpdateCommitteeHashRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData4 UpdateCommitteeHashRedeemer

-- | @since Unreleased
instance UnsafeFromData UpdateCommitteeHashRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData4 UpdateCommitteeHashRedeemer

-- | 'UpdateCommitteeHash' is used as the parameter for the validator.
data UpdateCommitteeHash = UpdateCommitteeHash
  { cSidechainParams :: SidechainParams
  , -- | 'cToken' is the 'AssetClass' of the NFT that is used to
    -- identify the transaction.
    cToken :: AssetClass
  , -- | 'cMptRootTokenCurrencySymbol' is the currency symbol of the corresponding merkle
    -- root token. This is needed for verifying that the previous merkle root is verified.
    cMptRootTokenCurrencySymbol :: CurrencySymbol
  }

-- | @since Unreleased
instance ToData UpdateCommitteeHash where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (UpdateCommitteeHash {..}) =
    productToData3 cSidechainParams cToken cMptRootTokenCurrencySymbol

-- | @since Unreleased
instance FromData UpdateCommitteeHash where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData3 UpdateCommitteeHash

-- | @since Unreleased
instance UnsafeFromData UpdateCommitteeHash where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData3 UpdateCommitteeHash

{- | = Important note

 The 'Data' serializations for this type /cannot/ be changed.
-}
data UpdateCommitteeHashMessage = UpdateCommitteeHashMessage
  { uchmSidechainParams :: SidechainParams
  , -- | 'newCommitteePubKeys' is the new committee public keys and _should_
    -- be sorted lexicographically (recall that we can trust the bridge, so it
    -- should do this for us
    uchmNewCommitteePubKeys :: [EcdsaSecp256k1PubKey]
  , uchmPreviousMerkleRoot :: Maybe LedgerBytes
  , uchmSidechainEpoch :: Integer
  }

PlutusTx.makeIsDataIndexed ''UpdateCommitteeHashMessage [('UpdateCommitteeHashMessage, 0)]

-- | Datum for a checkpoint
data CheckpointDatum = CheckpointDatum
  { checkpointBlockHash :: LedgerBytes
  , checkpointBlockNumber :: Integer
  }

-- | @since Unreleased
instance ToData CheckpointDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (CheckpointDatum {..}) =
    productToData2 checkpointBlockHash checkpointBlockNumber

-- | @since Unreleased
instance FromData CheckpointDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 CheckpointDatum

-- | @since Unreleased
instance UnsafeFromData CheckpointDatum where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 CheckpointDatum

{- | The Redeemer that is passed to the on-chain validator to update the
 checkpoint
-}
data CheckpointRedeemer = CheckpointRedeemer
  { checkpointCommitteeSignatures :: [LedgerBytes]
  , checkpointCommitteePubKeys :: [EcdsaSecp256k1PubKey]
  , newCheckpointBlockHash :: BuiltinByteString
  , newCheckpointBlockNumber :: Integer
  }

-- | @since Unreleased
instance ToData CheckpointRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (CheckpointRedeemer {..}) =
    productToData4
      checkpointCommitteeSignatures
      checkpointCommitteePubKeys
      newCheckpointBlockHash
      newCheckpointBlockNumber

-- | @since Unreleased
instance FromData CheckpointRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData4 CheckpointRedeemer

-- | @since Unreleased
instance UnsafeFromData CheckpointRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData4 CheckpointRedeemer

-- | 'Checkpoint' is used as the parameter for the validator.
data CheckpointParameter = CheckpointParameter
  { checkpointSidechainParams :: SidechainParams
  , -- | 'checkpointAssetClass' is the 'AssetClass' of the NFT that is used to
    -- identify the transaction.
    checkpointAssetClass :: AssetClass
  , -- | 'committeeHashAssetClass' is the 'AssetClass' of the NFT that is used to
    -- | identify the current committee
    committeeHashAssetClass :: AssetClass
  }

-- | @since Unreleased
instance ToData CheckpointParameter where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (CheckpointParameter {..}) =
    productToData3
      checkpointSidechainParams
      checkpointAssetClass
      committeeHashAssetClass

-- | @since Unreleased
instance FromData CheckpointParameter where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData3 CheckpointParameter

-- | @since Unreleased
instance UnsafeFromData CheckpointParameter where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData3 CheckpointParameter

{- | = Important note

 The 'Data' serializations of this type /cannot/ be changed.
-}
data CheckpointMessage = CheckpointMessage
  { checkpointMsgSidechainParams :: SidechainParams
  , checkpointMsgBlockHash :: LedgerBytes
  , checkpointMsgBlockNumber :: Integer
  , checkpointMsgSidechainEpoch :: Integer
  }

PlutusTx.makeIsDataIndexed ''CheckpointMessage [('CheckpointMessage, 0)]
