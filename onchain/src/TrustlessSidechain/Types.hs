{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TrustlessSidechain.Types where

import Ledger.Crypto (PubKey, PubKeyHash, Signature)
import Ledger.Value (AssetClass, CurrencySymbol)
import Plutus.V2.Ledger.Api (LedgerBytes (LedgerBytes), ValidatorHash)
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
  { getSidechainPubKey :: LedgerBytes
  }
  deriving stock (TSPrelude.Eq, TSPrelude.Ord)
  deriving newtype
    ( Eq
    , Ord
    , ToData
    , FromData
    , UnsafeFromData
    )
  deriving (IsString, TSPrelude.Show) via LedgerBytes

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
    committeePubKeys :: [SidechainPubKey]
  }

PlutusTx.makeIsDataIndexed ''SignedMerkleRoot [('SignedMerkleRoot, 0)]

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
instance HasField "committeePubKeys" SignedMerkleRoot [SidechainPubKey] where
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

PlutusTx.makeIsDataIndexed ''SignedMerkleRootMint [('SignedMerkleRootMint, 0)]

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
  { committeeHash :: LedgerBytes
  , sidechainEpoch :: Integer
  }

PlutusTx.makeIsDataIndexed ''UpdateCommitteeHashDatum [('UpdateCommitteeHashDatum, 0)]

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
    committeePubKeys :: [SidechainPubKey]
  , -- | 'newCommitteePubKeys' is the hash of the new committee
    newCommitteePubKeys :: [SidechainPubKey]
  , -- | 'previousMerkleRoot' is the previous merkle root (if it exists)
    previousMerkleRoot :: Maybe LedgerBytes
  }

PlutusTx.makeIsDataIndexed ''UpdateCommitteeHashRedeemer [('UpdateCommitteeHashRedeemer, 0)]

-- | @since Unreleased
instance HasField "committeeSignatures" UpdateCommitteeHashRedeemer [LedgerBytes] where
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

PlutusTx.makeIsDataIndexed ''UpdateCommitteeHash [('UpdateCommitteeHash, 0)]

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

data UpdateCommitteeHashMessage = UpdateCommitteeHashMessage
  { -- | @since Unreleased
    sidechainParams :: SidechainParams
  , -- | 'newCommitteePubKeys' is the new committee public keys and _should_
    -- | be sorted lexicographically (recall that we can trust the bridge, so it
    -- | should do this for us
    -- |
    -- | @since Unreleased
    newCommitteePubKeys :: [SidechainPubKey]
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
instance HasField "newCommitteePubKeys" UpdateCommitteeHashMessage [SidechainPubKey] where
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

PlutusTx.makeIsDataIndexed ''CheckpointDatum [('CheckpointDatum, 0)]

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
  , checkpointCommitteePubKeys :: [SidechainPubKey]
  , newCheckpointBlockHash :: LedgerBytes
  , newCheckpointBlockNumber :: Integer
  }

PlutusTx.makeIsDataIndexed ''CheckpointRedeemer [('CheckpointRedeemer, 0)]

-- | @since Unreleased
instance HasField "checkpointCommitteeSignatures" CheckpointRedeemer [LedgerBytes] where
  {-# INLINE get #-}
  get (CheckpointRedeemer x _ _ _) = x
  {-# INLINE modify #-}
  modify f (CheckpointRedeemer ccs ccpks ncbh ncbn) =
    CheckpointRedeemer (f ccs) ccpks ncbh ncbn

-- | @since Unreleased
instance HasField "checkpointCommitteePubKeys" CheckpointRedeemer [SidechainPubKey] where
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

PlutusTx.makeIsDataIndexed ''CheckpointParameter [('CheckpointParameter, 0)]

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
