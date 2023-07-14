{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TrustlessSidechain.Types where

import Ledger.Crypto (PubKey, PubKeyHash, Signature)
import Ledger.Value (AssetClass, CurrencySymbol)
import Plutus.V2.Ledger.Api (ValidatorHash)
import Plutus.V2.Ledger.Tx (TxOutRef)
import PlutusTx (FromData, ToData, UnsafeFromData)
import PlutusTx qualified
import TrustlessSidechain.HaskellPrelude qualified as TSPrelude
import TrustlessSidechain.MerkleTree (MerkleProof)
import TrustlessSidechain.PlutusPrelude

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
  deriving newtype
    ( TSPrelude.Show
    , ToData
    , FromData
    , UnsafeFromData
    , IsString
    )

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

-- | 'SidechainPubKey' is compressed DER Secp256k1 public key.
newtype SidechainPubKey = SidechainPubKey
  { getSidechainPubKey :: BuiltinByteString
  }
  deriving newtype
    ( TSPrelude.Eq
    , TSPrelude.Ord
    , ToData
    , FromData
    , UnsafeFromData
    )

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

-- | @since Unreleased
instance HasField "sidechainParams" RegisterParams SidechainParams where
  {-# INLINE get #-}
  get (RegisterParams sp _ _ _ _ _) = sp
  {-# INLINE modify #-}
  modify f (RegisterParams sp spoPK sPK sS scS u) =
    RegisterParams (f sp) spoPK sPK sS scS u

-- | @since Unreleased
instance HasField "spoPubKey" RegisterParams PubKey where
  {-# INLINE get #-}
  get = spoPubKey
  {-# INLINE modify #-}
  modify f rp = rp {spoPubKey = f (spoPubKey rp)}

-- | @since Unreleased
instance HasField "sidechainPubKey" RegisterParams SidechainPubKey where
  {-# INLINE get #-}
  get = sidechainPubKey
  {-# INLINE modify #-}
  modify f rp = rp {sidechainPubKey = f (sidechainPubKey rp)}

-- | @since Unreleased
instance HasField "spoSig" RegisterParams Signature where
  {-# INLINE get #-}
  get = spoSig
  {-# INLINE modify #-}
  modify f rp = rp {spoSig = f (spoSig rp)}

-- | @since Unreleased
instance HasField "sidechainSig" RegisterParams Signature where
  {-# INLINE get #-}
  get = sidechainSig
  {-# INLINE modify #-}
  modify f rp = rp {sidechainSig = f (sidechainSig rp)}

-- | @since Unreleased
instance HasField "inputUtxo" RegisterParams TxOutRef where
  {-# INLINE get #-}
  get = inputUtxo
  {-# INLINE modify #-}
  modify f rp = rp {inputUtxo = f (inputUtxo rp)}

{- | 'CandidatePermissionMint' is used to parameterize the minting policy in
 'TrustlessSidechain.CommitteeCandidateMintingPolicy'.
-}
data CandidatePermissionMint = CandidatePermissionMint
  { -- | @since Unreleased
    sidechainParams :: SidechainParams
  , -- | @since Unreleased
    utxo :: TxOutRef
  }

PlutusTx.makeIsDataIndexed ''CandidatePermissionMint [('CandidatePermissionMint, 0)]

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

-- | Endpoint parameters for committee candidate deregistration
data DeregisterParams = DeregisterParams
  { sidechainParams :: SidechainParams
  , spoPubKey :: PubKey
  }

-- | @since Unreleased
instance HasField "sidechainParams" DeregisterParams SidechainParams where
  {-# INLINE get #-}
  get (DeregisterParams sp _) = sp
  {-# INLINE modify #-}
  modify f (DeregisterParams sp sPK) = DeregisterParams (f sp) sPK

-- | @since Unreleased
instance HasField "spoPubKey" DeregisterParams PubKey where
  {-# INLINE get #-}
  get (DeregisterParams _ x) = x
  {-# INLINE modify #-}
  modify f (DeregisterParams sp sPK) = DeregisterParams sp (f sPK)

data BlockProducerRegistration = BlockProducerRegistration
  { -- | SPO cold verification key hash
    -- | @since Unreleased
    spoPubKey :: PubKey -- own cold verification key hash
  , -- | public key in the sidechain's desired format
    -- | @since Unreleased
    sidechainPubKey :: SidechainPubKey
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

PlutusTx.makeIsDataIndexed ''BlockProducerRegistration [('BlockProducerRegistration, 0)]

-- | @since Unreleased
instance HasField "spoPubKey" BlockProducerRegistration PubKey where
  {-# INLINE get #-}
  get (BlockProducerRegistration x _ _ _ _ _) = x
  {-# INLINE modify #-}
  modify f (BlockProducerRegistration sPK scPK sS scS u pkh) =
    BlockProducerRegistration (f sPK) scPK sS scS u pkh

-- | @since Unreleased
instance HasField "sidechainPubKey" BlockProducerRegistration SidechainPubKey where
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

data BlockProducerRegistrationMsg = BlockProducerRegistrationMsg
  { -- | @since Unreleased
    sidechainParams :: SidechainParams
  , -- | @since Unreleased
    sidechainPubKey :: SidechainPubKey
  , -- | A UTxO that must be spent by the transaction
    -- | @since Unreleased
    inputUtxo :: TxOutRef
  }

PlutusTx.makeIsDataIndexed ''BlockProducerRegistrationMsg [('BlockProducerRegistrationMsg, 0)]

-- | @since Unreleased
instance HasField "sidechainParams" BlockProducerRegistrationMsg SidechainParams where
  {-# INLINE get #-}
  get (BlockProducerRegistrationMsg x _ _) = x
  {-# INLINE modify #-}
  modify f (BlockProducerRegistrationMsg sp spk u) =
    BlockProducerRegistrationMsg (f sp) spk u

-- | @since Unreleased
instance HasField "sidechainPubKey" BlockProducerRegistrationMsg SidechainPubKey where
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
    recipient :: BuiltinByteString
  , -- | the previous merkle root to ensure that the hashed entry is unique
    -- | @since Unreleased
    previousMerkleRoot :: Maybe BuiltinByteString
  }

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
instance HasField "recipient" MerkleTreeEntry BuiltinByteString where
  {-# INLINE get #-}
  get (MerkleTreeEntry _ _ x _) = x
  {-# INLINE modify #-}
  modify f (MerkleTreeEntry i a r pmr) =
    MerkleTreeEntry i a (f r) pmr

-- | @since Unreleased
instance HasField "previousMerkleRoot" MerkleTreeEntry (Maybe BuiltinByteString) where
  {-# INLINE get #-}
  get (MerkleTreeEntry _ _ _ x) = x
  {-# INLINE modify #-}
  modify f (MerkleTreeEntry i a r pmr) =
    MerkleTreeEntry i a r (f pmr)

{- | 'MerkleRootInsertionMessage' is a data type for which committee members
 create signatures for
 >  blake2b(cbor(MerkleRootInsertionMessage))
-}
data MerkleRootInsertionMessage = MerkleRootInsertionMessage
  { -- | @since Unreleased
    sidechainParams :: SidechainParams
  , -- | @since Unreleased
    merkleRoot :: BuiltinByteString
  , -- | @since Unreleased
    previousMerkleRoot :: Maybe BuiltinByteString
  }

PlutusTx.makeIsDataIndexed ''MerkleRootInsertionMessage [('MerkleRootInsertionMessage, 0)]

-- | @since Unreleased
instance HasField "sidechainParams" MerkleRootInsertionMessage SidechainParams where
  {-# INLINE get #-}
  get (MerkleRootInsertionMessage x _ _) = x
  {-# INLINE modify #-}
  modify f (MerkleRootInsertionMessage sp mr pmr) =
    MerkleRootInsertionMessage (f sp) mr pmr

-- | @since Unreleased
instance HasField "merkleRoot" MerkleRootInsertionMessage BuiltinByteString where
  {-# INLINE get #-}
  get (MerkleRootInsertionMessage _ x _) = x
  {-# INLINE modify #-}
  modify f (MerkleRootInsertionMessage sp mr pmr) =
    MerkleRootInsertionMessage sp (f mr) pmr

-- | @since Unreleased
instance HasField "previousMerkleRoot" MerkleRootInsertionMessage (Maybe BuiltinByteString) where
  {-# INLINE get #-}
  get (MerkleRootInsertionMessage _ _ x) = x
  {-# INLINE modify #-}
  modify f (MerkleRootInsertionMessage sp mr pmr) =
    MerkleRootInsertionMessage sp mr (f pmr)

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
  { -- | @since Unreleased
    transaction :: MerkleTreeEntry
  , -- | @since Unreleased
    merkleProof :: MerkleProof
  }

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
    -- | which contains a merkle root in the 'TokenName'. See
    -- | 'TrustlessSidechain.MerkleRootTokenMintingPolicy' for details.
    -- |
    -- | @since Unreleased
    mptRootTokenCurrencySymbol :: CurrencySymbol
  , -- | 'fmSidechainParams' is the sidechain parameters
    -- |
    -- | @since Unreleased
    sidechainParams :: SidechainParams
  , -- | 'fmDsKeyCurrencySymbol' is th currency symbol for the tokens which
    -- | hold the key for the distributed set. In particular, this allows the
    -- | FUEL minting policy to verify if a string has /just been inserted/ into
    -- | the distributed set.
    -- |
    -- | @since Unreleased
    dsKeyCurrencySymbol :: CurrencySymbol
  }

PlutusTx.makeIsDataIndexed ''FUELMint [('FUELMint, 0)]

-- | @since Unreleased
instance HasField "mptRootTokenCurrencySymbol" FUELMint CurrencySymbol where
  {-# INLINE get #-}
  get (FUELMint x _ _) = x
  {-# INLINE modify #-}
  modify f (FUELMint rtcs sp kcs) =
    FUELMint (f rtcs) sp kcs

-- | @since Unreleased
instance HasField "sidechainParams" FUELMint SidechainParams where
  {-# INLINE get #-}
  get (FUELMint _ x _) = x
  {-# INLINE modify #-}
  modify f (FUELMint rtcs sp kcs) =
    FUELMint rtcs (f sp) kcs

-- | @since Unreleased
instance HasField "dsKeyCurrencySymbol" FUELMint CurrencySymbol where
  {-# INLINE get #-}
  get (FUELMint _ _ x) = x
  {-# INLINE modify #-}
  modify f (FUELMint rtcs sp kcs) =
    FUELMint rtcs sp (f kcs)

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

-- | @since Unreleased
instance HasField "committeeHash" UpdateCommitteeHashDatum BuiltinByteString where
  {-# INLINE get #-}
  get (UpdateCommitteeHashDatum x _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashDatum ch se) =
    UpdateCommitteeHashDatum (f ch) se

-- | @since Unreleased
instance HasField "sidechainEpoch" UpdateCommitteeHashDatum Integer where
  {-# INLINE get #-}
  get (UpdateCommitteeHashDatum _ x) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashDatum ch se) =
    UpdateCommitteeHashDatum ch (f se)

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

-- | @since Unreleased
instance HasField "committeeSignatures" UpdateCommitteeHashRedeemer [BuiltinByteString] where
  {-# INLINE get #-}
  get (UpdateCommitteeHashRedeemer x _ _ _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashRedeemer cs cpk ncpk pmr) =
    UpdateCommitteeHashRedeemer (f cs) cpk ncpk pmr

-- | @since Unreleased
instance HasField "committeePubKeys" UpdateCommitteeHashRedeemer [SidechainPubKey] where
  {-# INLINE get #-}
  get (UpdateCommitteeHashRedeemer _ x _ _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashRedeemer cs cpk ncpk pmr) =
    UpdateCommitteeHashRedeemer cs (f cpk) ncpk pmr

-- | @since Unreleased
instance HasField "newCommitteePubKeys" UpdateCommitteeHashRedeemer [SidechainPubKey] where
  {-# INLINE get #-}
  get (UpdateCommitteeHashRedeemer _ _ x _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashRedeemer cs cpk ncpk pmr) =
    UpdateCommitteeHashRedeemer cs cpk (f ncpk) pmr

-- | @since Unreleased
instance HasField "previousMerkleRoot" UpdateCommitteeHashRedeemer (Maybe BuiltinByteString) where
  {-# INLINE get #-}
  get (UpdateCommitteeHashRedeemer _ _ _ x) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashRedeemer cs cpk ncpk pmr) =
    UpdateCommitteeHashRedeemer cs cpk ncpk (f pmr)

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
