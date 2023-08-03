{-# LANGUAGE DeriveAnyClass #-}
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

-- | Endpoint parameters for committee candidate registration
data RegisterParams = RegisterParams
  { sidechainParams :: SidechainParams
  , spoPubKey :: PubKey
  , -- | @since Unreleased
    ecdsaSecp256k1PubKey :: EcdsaSecp256k1PubKey
  , spoSig :: Signature
  , sidechainSig :: Signature
  , inputUtxo :: TxOutRef
  }

-- | @since Unreleased
instance HasField "sidechainParams" RegisterParams SidechainParams where
  {-# INLINE get #-}
  get (RegisterParams x _ _ _ _ _) = x
  {-# INLINE modify #-}
  modify f (RegisterParams sp spoPK sPK sS scS u) =
    RegisterParams (f sp) spoPK sPK sS scS u

-- | @since Unreleased
instance HasField "spoPubKey" RegisterParams PubKey where
  {-# INLINE get #-}
  get (RegisterParams _ x _ _ _ _) = x
  {-# INLINE modify #-}
  modify f (RegisterParams sp spoPK sPK sS scS u) =
    RegisterParams sp (f spoPK) sPK sS scS u

-- | @since Unreleased
instance HasField "ecdsaSecp256k1PubKey" RegisterParams EcdsaSecp256k1PubKey where
  {-# INLINE get #-}
  get (RegisterParams _ _ x _ _ _) = x
  {-# INLINE modify #-}
  modify f (RegisterParams sp spoPK sPK sS scS u) =
    RegisterParams sp spoPK (f sPK) sS scS u

-- | @since Unreleased
instance HasField "spoSig" RegisterParams Signature where
  {-# INLINE get #-}
  get (RegisterParams _ _ _ x _ _) = x
  {-# INLINE modify #-}
  modify f (RegisterParams sp spoPK sPK sS scS u) =
    RegisterParams sp spoPK sPK (f sS) scS u

-- | @since Unreleased
instance HasField "sidechainSig" RegisterParams Signature where
  {-# INLINE get #-}
  get (RegisterParams _ _ _ _ x _) = x
  {-# INLINE modify #-}
  modify f (RegisterParams sp spoPK sPK sS scS u) =
    RegisterParams sp spoPK sPK sS (f scS) u

-- | @since Unreleased
instance HasField "inputUtxo" RegisterParams TxOutRef where
  {-# INLINE get #-}
  get (RegisterParams _ _ _ _ _ x) = x
  {-# INLINE modify #-}
  modify f (RegisterParams sp spoPK sPK sS scS u) =
    RegisterParams sp spoPK sPK sS scS (f u)

{- | 'CandidatePermissionMint' is used to parameterize the minting policy in
 'TrustlessSidechain.CommitteeCandidateMintingPolicy'.
-}
data CandidatePermissionMint = CandidatePermissionMint
  { -- | @since Unreleased
    sidechainParams :: SidechainParams
  , -- | @since Unreleased
    utxo :: TxOutRef
  }

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

{- | = Important note

 The 'Data' serializations for this type /cannot/ change.
-}
data BlockProducerRegistration = BlockProducerRegistration
  { -- | SPO cold verification key hash
    -- | @since Unreleased
    spoPubKey :: PubKey -- own cold verification key hash
  , -- | public key in the sidechain's desired format
    -- | @since Unreleased
    ecdsaSecp256k1PubKey :: EcdsaSecp256k1PubKey
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
instance HasField "ecdsaSecp256k1PubKey" BlockProducerRegistration EcdsaSecp256k1PubKey where
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
  { -- | @since Unreleased
    sidechainParams :: SidechainParams
  , -- | @since Unreleased
    ecdsaSecp256k1PubKey :: EcdsaSecp256k1PubKey
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
instance HasField "ecdsaSecp256k1PubKey" BlockProducerRegistrationMsg EcdsaSecp256k1PubKey where
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

-- | @since Unreleased
instance HasField "merkleRoot" SignedMerkleRoot LedgerBytes where
  {-# INLINE get #-}
  get (SignedMerkleRoot x _ _ _) = x
  {-# INLINE modify #-}
  modify f (SignedMerkleRoot mr pmr sigs cpks) =
    SignedMerkleRoot (f mr) pmr sigs cpks

-- | @since Unreleased
instance HasField "previousMerkleRoot" SignedMerkleRoot (Maybe LedgerBytes) where
  {-# INLINE get #-}
  get (SignedMerkleRoot _ x _ _) = x
  {-# INLINE modify #-}
  modify f (SignedMerkleRoot mr pmr sigs cpks) =
    SignedMerkleRoot mr (f pmr) sigs cpks

-- | @since Unreleased
instance HasField "signatures" SignedMerkleRoot [LedgerBytes] where
  {-# INLINE get #-}
  get (SignedMerkleRoot _ _ x _) = x
  {-# INLINE modify #-}
  modify f (SignedMerkleRoot mr pmr sigs cpks) =
    SignedMerkleRoot mr pmr (f sigs) cpks

-- | @since Unreleased
instance HasField "committeePubKeys" SignedMerkleRoot [EcdsaSecp256k1PubKey] where
  {-# INLINE get #-}
  get (SignedMerkleRoot _ _ _ x) = x
  {-# INLINE modify #-}
  modify f (SignedMerkleRoot mr pmr sigs cpks) =
    SignedMerkleRoot mr pmr sigs (f cpks)

-- | 'SignedMerkleRootMint' is used to parameterize 'mkMintingPolicy'.
data SignedMerkleRootMint = SignedMerkleRootMint
  { -- | @since Unreleased
    sidechainParams :: SidechainParams
  , -- | The 'CurrencySymbol' which
    -- | identifies the utxo for which the 'UpdateCommitteeHashDatum'
    -- | resides.
    -- |
    -- | @since Unreleased
    updateCommitteeHashCurrencySymbol :: CurrencySymbol
  , -- | The validator hash corresponding to
    -- | 'TrustlessSidechain.MerkleRootTokenValidator.mkMptRootTokenValidator'
    -- | to ensure that this token gets minted to the "right" place.
    -- |
    -- | @since Unreleased
    validatorHash :: ValidatorHash
  }

-- | @since Unreleased
instance ToData SignedMerkleRootMint where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (SignedMerkleRootMint {..}) =
    productToData3
      sidechainParams
      updateCommitteeHashCurrencySymbol
      validatorHash

-- | @since Unreleased
instance FromData SignedMerkleRootMint where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData3 SignedMerkleRootMint

-- | @since Unreleased
instance UnsafeFromData SignedMerkleRootMint where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData3 SignedMerkleRootMint

-- | @since Unreleased
instance HasField "sidechainParams" SignedMerkleRootMint SidechainParams where
  {-# INLINE get #-}
  get (SignedMerkleRootMint x _ _) = x
  {-# INLINE modify #-}
  modify f (SignedMerkleRootMint sp uchcs vh) =
    SignedMerkleRootMint (f sp) uchcs vh

-- | @since Unreleased
instance HasField "updateCommitteeHashCurrencySymbol" SignedMerkleRootMint CurrencySymbol where
  {-# INLINE get #-}
  get (SignedMerkleRootMint _ x _) = x
  {-# INLINE modify #-}
  modify f (SignedMerkleRootMint sp uchcs vh) =
    SignedMerkleRootMint sp (f uchcs) vh

-- | @since Unreleased
instance HasField "validatorHash" SignedMerkleRootMint ValidatorHash where
  {-# INLINE get #-}
  get (SignedMerkleRootMint _ _ x) = x
  {-# INLINE modify #-}
  modify f (SignedMerkleRootMint sp uchcs vh) =
    SignedMerkleRootMint sp uchcs (f vh)

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

-- | @since Unreleased
instance ToData FUELMint where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (FUELMint {..}) =
    productToData3
      mptRootTokenCurrencySymbol
      sidechainParams
      dsKeyCurrencySymbol

-- | @since Unreleased
instance FromData FUELMint where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData3 FUELMint

-- | @since Unreleased
instance UnsafeFromData FUELMint where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData3 FUELMint

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

-- | @since Unreleased
instance HasField "committeeHash" UpdateCommitteeHashDatum LedgerBytes where
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

-- | @since Unreleased
instance HasField "committeeSignatures" UpdateCommitteeHashRedeemer [LedgerBytes] where
  {-# INLINE get #-}
  get (UpdateCommitteeHashRedeemer x _ _ _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashRedeemer cs cpk ncpk pmr) =
    UpdateCommitteeHashRedeemer (f cs) cpk ncpk pmr

-- | @since Unreleased
instance HasField "committeePubKeys" UpdateCommitteeHashRedeemer [EcdsaSecp256k1PubKey] where
  {-# INLINE get #-}
  get (UpdateCommitteeHashRedeemer _ x _ _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashRedeemer cs cpk ncpk pmr) =
    UpdateCommitteeHashRedeemer cs (f cpk) ncpk pmr

-- | @since Unreleased
instance HasField "newCommitteePubKeys" UpdateCommitteeHashRedeemer [EcdsaSecp256k1PubKey] where
  {-# INLINE get #-}
  get (UpdateCommitteeHashRedeemer _ _ x _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashRedeemer cs cpk ncpk pmr) =
    UpdateCommitteeHashRedeemer cs cpk (f ncpk) pmr

-- | @since Unreleased
instance HasField "previousMerkleRoot" UpdateCommitteeHashRedeemer (Maybe LedgerBytes) where
  {-# INLINE get #-}
  get (UpdateCommitteeHashRedeemer _ _ _ x) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashRedeemer cs cpk ncpk pmr) =
    UpdateCommitteeHashRedeemer cs cpk ncpk (f pmr)

-- | 'UpdateCommitteeHash' is used as the parameter for the validator.
data UpdateCommitteeHash = UpdateCommitteeHash
  { -- | @since Unreleased
    sidechainParams :: SidechainParams
  , -- | 'token' is the 'AssetClass' of the NFT that is used to
    -- | identify the transaction.
    -- |
    -- | @since Unreleased
    token :: AssetClass
  , -- | 'mptRootTokenCurrencySymbol' is the currency symbol of the corresponding merkle
    -- | root token. This is needed for verifying that the previous merkle root is verified.
    -- |
    -- | @since Unreleased
    mptRootTokenCurrencySymbol :: CurrencySymbol
  }

-- | @since Unreleased
instance ToData UpdateCommitteeHash where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (UpdateCommitteeHash {..}) =
    productToData3 sidechainParams token mptRootTokenCurrencySymbol

-- | @since Unreleased
instance HasField "sidechainParams" UpdateCommitteeHash SidechainParams where
  {-# INLINE get #-}
  get (UpdateCommitteeHash x _ _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHash sp t rtcs) =
    UpdateCommitteeHash (f sp) t rtcs

-- | @since Unreleased
instance HasField "token" UpdateCommitteeHash AssetClass where
  {-# INLINE get #-}
  get (UpdateCommitteeHash _ x _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHash sp t rtcs) =
    UpdateCommitteeHash sp (f t) rtcs

-- | @since Unreleased
instance HasField "mptRootTokenCurrencySymbol" UpdateCommitteeHash CurrencySymbol where
  {-# INLINE get #-}
  get (UpdateCommitteeHash _ _ x) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHash sp t rtcs) =
    UpdateCommitteeHash sp t (f rtcs)

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
  { -- | @since Unreleased
    sidechainParams :: SidechainParams
  , -- | 'newCommitteePubKeys' is the new committee public keys and _should_
    -- | be sorted lexicographically (recall that we can trust the bridge, so it
    -- | should do this for us
    -- |
    -- | @since Unreleased
    newCommitteePubKeys :: [EcdsaSecp256k1PubKey]
  , -- | @since Unreleased
    previousMerkleRoot :: Maybe LedgerBytes
  , -- | @since Unreleased
    sidechainEpoch :: Integer
  }

PlutusTx.makeIsDataIndexed ''UpdateCommitteeHashMessage [('UpdateCommitteeHashMessage, 0)]

-- | @since Unreleased
instance HasField "sidechainParams" UpdateCommitteeHashMessage SidechainParams where
  {-# INLINE get #-}
  get (UpdateCommitteeHashMessage x _ _ _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashMessage sp ncpks pmr se) =
    UpdateCommitteeHashMessage (f sp) ncpks pmr se

-- | @since Unreleased
instance HasField "newCommitteePubKeys" UpdateCommitteeHashMessage [EcdsaSecp256k1PubKey] where
  {-# INLINE get #-}
  get (UpdateCommitteeHashMessage _ x _ _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashMessage sp ncpks pmr se) =
    UpdateCommitteeHashMessage sp (f ncpks) pmr se

-- | @since Unreleased
instance HasField "previousMerkleRoot" UpdateCommitteeHashMessage (Maybe LedgerBytes) where
  {-# INLINE get #-}
  get (UpdateCommitteeHashMessage _ _ x _) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashMessage sp ncpks pmr se) =
    UpdateCommitteeHashMessage sp ncpks (f pmr) se

-- | @since Unreleased
instance HasField "sidechainEpoch" UpdateCommitteeHashMessage Integer where
  {-# INLINE get #-}
  get (UpdateCommitteeHashMessage _ _ _ x) = x
  {-# INLINE modify #-}
  modify f (UpdateCommitteeHashMessage sp ncpks pmr se) =
    UpdateCommitteeHashMessage sp ncpks pmr (f se)

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

{- | The Redeemer that is passed to the on-chain validator to update the
 checkpoint
-}
data CheckpointRedeemer = CheckpointRedeemer
  { checkpointCommitteeSignatures :: [LedgerBytes]
  , checkpointCommitteePubKeys :: [EcdsaSecp256k1PubKey]
  , newCheckpointBlockHash :: LedgerBytes
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

-- | @since Unreleased
instance HasField "checkpointCommitteeSignatures" CheckpointRedeemer [LedgerBytes] where
  {-# INLINE get #-}
  get (CheckpointRedeemer x _ _ _) = x
  {-# INLINE modify #-}
  modify f (CheckpointRedeemer ccs ccpks ncbh ncbn) =
    CheckpointRedeemer (f ccs) ccpks ncbh ncbn

-- | @since Unreleased
instance HasField "checkpointCommitteePubKeys" CheckpointRedeemer [EcdsaSecp256k1PubKey] where
  {-# INLINE get #-}
  get (CheckpointRedeemer _ x _ _) = x
  {-# INLINE modify #-}
  modify f (CheckpointRedeemer ccs ccpks ncbh ncbn) =
    CheckpointRedeemer ccs (f ccpks) ncbh ncbn

-- | @since Unreleased
instance HasField "newCheckpointBlockHash" CheckpointRedeemer LedgerBytes where
  {-# INLINE get #-}
  get (CheckpointRedeemer _ _ x _) = x
  {-# INLINE modify #-}
  modify f (CheckpointRedeemer ccs ccpks ncbh ncbn) =
    CheckpointRedeemer ccs ccpks (f ncbh) ncbn

-- | @since Unreleased
instance HasField "newCheckpointBlockNumber" CheckpointRedeemer Integer where
  {-# INLINE get #-}
  get (CheckpointRedeemer _ _ _ x) = x
  {-# INLINE modify #-}
  modify f (CheckpointRedeemer ccs ccpks ncbh ncbn) =
    CheckpointRedeemer ccs ccpks ncbh (f ncbn)

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
instance HasField "checkpointSidechainParams" CheckpointParameter SidechainParams where
  {-# INLINE get #-}
  get (CheckpointParameter x _ _) = x
  {-# INLINE modify #-}
  modify f (CheckpointParameter csp cac chac) =
    CheckpointParameter (f csp) cac chac

-- | @since Unreleased
instance HasField "checkpointAssetClass" CheckpointParameter AssetClass where
  {-# INLINE get #-}
  get (CheckpointParameter _ x _) = x
  {-# INLINE modify #-}
  modify f (CheckpointParameter csp cac chac) =
    CheckpointParameter csp (f cac) chac

-- | @since Unreleased
instance HasField "committeeHashAssetClass" CheckpointParameter AssetClass where
  {-# INLINE get #-}
  get (CheckpointParameter _ _ x) = x
  {-# INLINE modify #-}
  modify f (CheckpointParameter csp cac chac) =
    CheckpointParameter csp cac (f chac)

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
