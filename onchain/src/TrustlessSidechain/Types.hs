{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TrustlessSidechain.Types where

import Data.String (IsString)
import Ledger.Crypto (PubKey, PubKeyHash, Signature)
import Ledger.Value (AssetClass, CurrencySymbol)
import Plutus.V2.Ledger.Api (ValidatorHash)
import Plutus.V2.Ledger.Tx (TxOutRef)
import PlutusTx (FromData, ToData, UnsafeFromData)
import PlutusTx qualified
import PlutusTx.Prelude
import TrustlessSidechain.MerkleTree (MerkleProof)
import Prelude qualified

-- * Sidechain Parametrization and general data

-- | Parameters uniquely identifying a sidechain
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

newtype GenesisHash = GenesisHash {getGenesisHash :: BuiltinByteString}
  deriving newtype (Prelude.Show, ToData, FromData, UnsafeFromData, IsString)

PlutusTx.makeIsDataIndexed ''SidechainParams [('SidechainParams, 0)]

-- | 'SidechainPubKey' is compressed DER Secp256k1 public key.
newtype SidechainPubKey = SidechainPubKey {getSidechainPubKey :: BuiltinByteString}
  deriving newtype (Prelude.Eq, Prelude.Ord, ToData, FromData, UnsafeFromData)

-- * Committee Candidate Validator data

-- | Endpoint parameters for committee candidate registration
data RegisterParams = RegisterParams
  { sidechainParams :: SidechainParams
  , spoPubKey :: PubKey
  , sidechainPubKey :: SidechainPubKey
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

PlutusTx.makeIsDataIndexed ''CandidatePermissionMint [('CandidatePermissionMint, 0)]

-- | Endpoint parameters for committee candidate deregistration
data DeregisterParams = DeregisterParams
  { sidechainParams :: SidechainParams
  , spoPubKey :: PubKey
  }

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

PlutusTx.makeIsDataIndexed ''BlockProducerRegistration [('BlockProducerRegistration, 0)]

data BlockProducerRegistrationMsg = BlockProducerRegistrationMsg
  { bprmSidechainParams :: SidechainParams
  , bprmSidechainPubKey :: SidechainPubKey
  , -- | A UTxO that must be spent by the transaction
    bprmInputUtxo :: TxOutRef
  }

PlutusTx.makeIsDataIndexed ''BlockProducerRegistrationMsg [('BlockProducerRegistrationMsg, 0)]

-- * Merkle Root Token data

{- | 'MerkleTreeEntry' (abbr. mte and pl. mtes) is the data which are the elements in the merkle tree
 for the MerkleRootToken.
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

PlutusTx.makeIsDataIndexed ''MerkleTreeEntry [('MerkleTreeEntry, 0)]

{- | 'MerkleRootInsertionMessage' is a data type for which committee members
 create signatures for
 >  blake2b(cbor(MerkleRootInsertionMessage))
-}
data MerkleRootInsertionMessage = MerkleRootInsertionMessage
  { mrimSidechainParams :: SidechainParams
  , mrimMerkleRoot :: BuiltinByteString
  , mrimPreviousMerkleRoot :: Maybe BuiltinByteString
  }

PlutusTx.makeIsDataIndexed ''MerkleRootInsertionMessage [('MerkleRootInsertionMessage, 0)]

-- | 'SignedMerkleRoot' is the redeemer for the Merkle root token minting policy
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

PlutusTx.makeIsDataIndexed ''SignedMerkleRootMint [('SignedMerkleRootMint, 0)]

{- | 'CombinedMerkleProof' is a product type to include both the
 'MerkleTreeEntry' and the 'MerkleProof'.

 This exists as for testing in #249.
-}
data CombinedMerkleProof = CombinedMerkleProof
  { cmpTransaction :: MerkleTreeEntry
  , cmpMerkleProof :: MerkleProof
  }

PlutusTx.makeIsDataIndexed ''CombinedMerkleProof [('CombinedMerkleProof, 0)]

-- * FUEL Minting Policy data

-- | The Redeemer that's to be passed to onchain policy, indicating its mode of usage.
data FUELRedeemer
  = MainToSide BuiltinByteString -- Recipient's sidechain address
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

PlutusTx.makeIsDataIndexed ''FUELMint [('FUELMint, 0)]

-- * Update Committee Hash data

{- | Datum for the committee hash. This /committee hash/ is used to verify
 signatures for sidechain to mainchain transfers. This is a hash of
 concatenated public key hashes of the committee members
-}
data UpdateCommitteeHashDatum = UpdateCommitteeHashDatum
  { committeeHash :: BuiltinByteString
  , sidechainEpoch :: Integer
  }

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

PlutusTx.makeIsDataIndexed ''UpdateCommitteeHash [('UpdateCommitteeHash, 0)]

data UpdateCommitteeHashMessage = UpdateCommitteeHashMessage
  { uchmSidechainParams :: SidechainParams
  , -- | 'newCommitteePubKeys' is the new committee public keys and _should_
    -- be sorted lexicographically (recall that we can trust the bridge, so it
    -- should do this for us
    uchmNewCommitteePubKeys :: [SidechainPubKey]
  , uchmPreviousMerkleRoot :: Maybe BuiltinByteString
  , uchmSidechainEpoch :: Integer
  }

PlutusTx.makeIsDataIndexed ''UpdateCommitteeHashMessage [('UpdateCommitteeHashMessage, 0)]

-- | Datum for a checkpoint
data CheckpointDatum = CheckpointDatum
  { checkpointBlockHash :: BuiltinByteString
  , checkpointBlockNumber :: Integer
  }

PlutusTx.makeIsDataIndexed ''CheckpointDatum [('CheckpointDatum, 0)]

{- | The Redeemer that is passed to the on-chain validator to update the
 checkpoint
-}
data CheckpointRedeemer = CheckpointRedeemer
  { checkpointCommitteeSignatures :: [BuiltinByteString]
  , checkpointCommitteePubKeys :: [SidechainPubKey]
  , newCheckpointBlockHash :: BuiltinByteString
  , newCheckpointBlockNumber :: Integer
  }

PlutusTx.makeIsDataIndexed ''CheckpointRedeemer [('CheckpointRedeemer, 0)]

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

PlutusTx.makeIsDataIndexed ''CheckpointParameter [('CheckpointParameter, 0)]

data CheckpointMessage = CheckpointMessage
  { checkpointMsgSidechainParams :: SidechainParams
  , checkpointMsgBlockHash :: BuiltinByteString
  , checkpointMsgBlockNumber :: Integer
  , checkpointMsgSidechainEpoch :: Integer
  }

PlutusTx.makeIsDataIndexed ''CheckpointMessage [('CheckpointMessage, 0)]
