{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TrustlessSidechain.Types where

import Ledger.Crypto (PubKey, PubKeyHash, Signature)
import Ledger.Value (AssetClass, CurrencySymbol)
import Plutus.V2.Ledger.Api (ValidatorHash)
import Plutus.V2.Ledger.Tx (TxOutRef)
import PlutusTx qualified
import PlutusTx.Builtins (matchList)
import PlutusTx.Builtins.Internal qualified as Unsafe
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

-- | @since Unreleased
instance ToData SidechainParams where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (SidechainParams {..}) =
    Unsafe.mkList
      ( Unsafe.mkCons
          (toBuiltinData chainId)
          ( Unsafe.mkCons
              (toBuiltinData genesisHash)
              ( Unsafe.mkCons
                  (toBuiltinData genesisUtxo)
                  ( Unsafe.mkCons
                      (toBuiltinData thresholdNumerator)
                      ( Unsafe.mkCons
                          (toBuiltinData thresholdDenominator)
                          (Unsafe.mkNilData Unsafe.unitval)
                      )
                  )
              )
          )
      )

-- | @since Unreleased
instance FromData SidechainParams where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData dat = Unsafe.chooseData dat Nothing Nothing go Nothing Nothing
    where
      go :: Maybe SidechainParams
      go =
        let ell = Unsafe.unsafeDataAsList dat
         in matchList ell Nothing $ \cid ell' ->
              case fromBuiltinData cid of
                Nothing -> Nothing
                Just cid' -> matchList ell' Nothing $ \gh ell'' ->
                  case fromBuiltinData gh of
                    Nothing -> Nothing
                    Just gh' -> matchList ell'' Nothing $ \gu ell''' ->
                      case fromBuiltinData gu of
                        Nothing -> Nothing
                        Just gu' -> matchList ell''' Nothing $ \tn ell'''' ->
                          case fromBuiltinData tn of
                            Nothing -> Nothing
                            Just tn' -> matchList ell'''' Nothing $ \td ell''''' ->
                              case fromBuiltinData td of
                                Nothing -> Nothing
                                Just td' ->
                                  matchList
                                    ell'''''
                                    (Just (SidechainParams cid' gh' gu' tn' td'))
                                    (\_ _ -> Nothing)

-- | @since Unreleased
instance UnsafeFromData SidechainParams where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData dat =
    let ell = Unsafe.unsafeDataAsList dat
        cid = unsafeFromBuiltinData (Unsafe.head ell)
        ell' = Unsafe.tail ell
        gh = unsafeFromBuiltinData (Unsafe.head ell')
        ell'' = Unsafe.tail ell'
        gu = unsafeFromBuiltinData (Unsafe.head ell'')
        ell''' = Unsafe.tail ell''
        tn = unsafeFromBuiltinData (Unsafe.head ell''')
        ell'''' = Unsafe.tail ell'''
        td = unsafeFromBuiltinData (Unsafe.head ell'''')
     in SidechainParams cid gh gu tn td

{- | 'SidechainPubKey' is compressed DER Secp256k1 public key.

 = Important note

 The 'Data' serializations for this type /cannot/ change.
-}
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

-- | @since Unreleased
instance ToData BlockProducerRegistration where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (BlockProducerRegistration {..}) =
    Unsafe.mkList
      ( Unsafe.mkCons
          (toBuiltinData bprSpoPubKey)
          ( Unsafe.mkCons
              (toBuiltinData bprSidechainPubKey)
              ( Unsafe.mkCons
                  (toBuiltinData bprSpoSignature)
                  ( Unsafe.mkCons
                      (toBuiltinData bprSidechainSignature)
                      ( Unsafe.mkCons
                          (toBuiltinData bprInputUtxo)
                          ( Unsafe.mkCons
                              (toBuiltinData bprOwnPkh)
                              (Unsafe.mkNilData Unsafe.unitval)
                          )
                      )
                  )
              )
          )
      )

-- | @since Unreleased
instance FromData BlockProducerRegistration where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData dat = Unsafe.chooseData dat Nothing Nothing go Nothing Nothing
    where
      go :: Maybe BlockProducerRegistration
      go =
        let ell0 = Unsafe.unsafeDataAsList dat
         in matchList ell0 Nothing $ \spoPK ell1 ->
              case fromBuiltinData spoPK of
                Nothing -> Nothing
                Just spoPK' -> matchList ell1 Nothing $ \scPK ell2 ->
                  case fromBuiltinData scPK of
                    Nothing -> Nothing
                    Just scPK' -> matchList ell2 Nothing $ \spoSig ell3 ->
                      case fromBuiltinData spoSig of
                        Nothing -> Nothing
                        Just spoSig' -> matchList ell3 Nothing $ \scSig ell4 ->
                          case fromBuiltinData scSig of
                            Nothing -> Nothing
                            Just scSig' -> matchList ell4 Nothing $ \inUtxo ell5 ->
                              case fromBuiltinData inUtxo of
                                Nothing -> Nothing
                                Just inUtxo' -> matchList ell5 Nothing $ \ownPK ell6 ->
                                  case fromBuiltinData ownPK of
                                    Nothing -> Nothing
                                    Just ownPK' ->
                                      matchList
                                        ell6
                                        ( Just
                                            ( BlockProducerRegistration
                                                spoPK'
                                                scPK'
                                                spoSig'
                                                scSig'
                                                inUtxo'
                                                ownPK'
                                            )
                                        )
                                        (\_ _ -> Nothing)

-- | @since Unreleased
instance UnsafeFromData BlockProducerRegistration where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData dat =
    let ell0 = Unsafe.unsafeDataAsList dat
        spoPK = unsafeFromBuiltinData (Unsafe.head ell0)
        ell1 = Unsafe.tail ell0
        scPK = unsafeFromBuiltinData (Unsafe.head ell1)
        ell2 = Unsafe.tail ell1
        spoSig = unsafeFromBuiltinData (Unsafe.head ell2)
        ell3 = Unsafe.tail ell2
        scSig = unsafeFromBuiltinData (Unsafe.head ell3)
        ell4 = Unsafe.tail ell4
        inUtxo = unsafeFromBuiltinData (Unsafe.head ell4)
        ell5 = Unsafe.tail ell4
        ownPK = unsafeFromBuiltinData (Unsafe.head ell5)
     in BlockProducerRegistration spoPK scPK spoSig scSig inUtxo ownPK

{- | = Important note

 The 'Data' serializations for this type /cannot/ change.
-}
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
    mteRecipient :: BuiltinByteString
  , -- | the previous merkle root to ensure that the hashed entry is unique
    mtePreviousMerkleRoot :: Maybe BuiltinByteString
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

-- | @since Unreleased
instance ToData SignedMerkleRoot where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (SignedMerkleRoot {..}) =
    Unsafe.mkList
      ( Unsafe.mkCons
          (toBuiltinData merkleRoot)
          ( Unsafe.mkCons
              (toBuiltinData previousMerkleRoot)
              ( Unsafe.mkCons
                  (toBuiltinData signatures)
                  ( Unsafe.mkCons
                      (toBuiltinData committeePubKeys)
                      (Unsafe.mkNilData Unsafe.unitval)
                  )
              )
          )
      )

-- | @since Unreleased
instance FromData SignedMerkleRoot where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData dat = Unsafe.chooseData dat Nothing Nothing go Nothing Nothing
    where
      go :: Maybe SignedMerkleRoot
      go =
        let ell0 = Unsafe.unsafeDataAsList dat
         in matchList ell0 Nothing $ \mRoot ell1 ->
              case fromBuiltinData mRoot of
                Nothing -> Nothing
                Just mRoot' -> matchList ell1 Nothing $ \prevMRoot ell2 ->
                  case fromBuiltinData prevMRoot of
                    Nothing -> Nothing
                    Just prevMRoot' -> matchList ell2 Nothing $ \sigs ell3 ->
                      case fromBuiltinData sigs of
                        Nothing -> Nothing
                        Just sigs' -> matchList ell3 Nothing $ \cpks ell4 ->
                          case fromBuiltinData cpks of
                            Nothing -> Nothing
                            Just cpks' ->
                              matchList
                                ell4
                                ( Just
                                    ( SignedMerkleRoot
                                        mRoot'
                                        prevMRoot'
                                        sigs'
                                        cpks'
                                    )
                                )
                                (\_ _ -> Nothing)

-- | @since Unreleased
instance UnsafeFromData SignedMerkleRoot where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData dat =
    let ell0 = Unsafe.unsafeDataAsList dat
        mRoot = unsafeFromBuiltinData (Unsafe.head ell0)
        ell1 = Unsafe.tail ell0
        prevMRoot = unsafeFromBuiltinData (Unsafe.head ell1)
        ell2 = Unsafe.tail ell1
        sigs = unsafeFromBuiltinData (Unsafe.head ell2)
        ell3 = Unsafe.tail ell2
        cpks = unsafeFromBuiltinData (Unsafe.head ell3)
     in SignedMerkleRoot mRoot prevMRoot sigs cpks

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
  { committeeHash :: BuiltinByteString
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
    committeeSignatures :: [BuiltinByteString]
  , -- | 'committeePubKeys' is the current committee public keys
    committeePubKeys :: [SidechainPubKey]
  , -- | 'newCommitteePubKeys' is the hash of the new committee
    newCommitteePubKeys :: [SidechainPubKey]
  , -- | 'previousMerkleRoot' is the previous merkle root (if it exists)
    previousMerkleRoot :: Maybe BuiltinByteString
  }

-- | @since Unreleased
instance ToData UpdateCommitteeHashRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (UpdateCommitteeHashRedeemer {..}) =
    Unsafe.mkList
      ( Unsafe.mkCons
          (toBuiltinData committeeSignatures)
          ( Unsafe.mkCons
              (toBuiltinData committeePubKeys)
              ( Unsafe.mkCons
                  (toBuiltinData newCommitteePubKeys)
                  ( Unsafe.mkCons
                      (toBuiltinData previousMerkleRoot)
                      (Unsafe.mkNilData Unsafe.unitval)
                  )
              )
          )
      )

-- | @since Unreleased
instance FromData UpdateCommitteeHashRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData dat = Unsafe.chooseData dat Nothing Nothing go Nothing Nothing
    where
      go :: Maybe UpdateCommitteeHashRedeemer
      go =
        let ell0 = Unsafe.unsafeDataAsList dat
         in matchList ell0 Nothing $ \cses ell1 ->
              case fromBuiltinData cses of
                Nothing -> Nothing
                Just cses' -> matchList ell1 Nothing $ \oldPks ell2 ->
                  case fromBuiltinData oldPks of
                    Nothing -> Nothing
                    Just oldPks' -> matchList ell2 Nothing $ \newPks ell3 ->
                      case fromBuiltinData newPks of
                        Nothing -> Nothing
                        Just newPks' -> matchList ell3 Nothing $ \pmr ell4 ->
                          case fromBuiltinData pmr of
                            Nothing -> Nothing
                            Just pmr' ->
                              matchList
                                ell4
                                ( Just
                                    ( UpdateCommitteeHashRedeemer
                                        cses'
                                        oldPks'
                                        newPks'
                                        pmr'
                                    )
                                )
                                (\_ _ -> Nothing)

-- | @since Unreleased
instance UnsafeFromData UpdateCommitteeHashRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData dat =
    let ell0 = Unsafe.unsafeDataAsList dat
        cses = unsafeFromBuiltinData (Unsafe.head ell0)
        ell1 = Unsafe.tail ell0
        oldPks = unsafeFromBuiltinData (Unsafe.head ell1)
        ell2 = Unsafe.tail ell1
        newPks = unsafeFromBuiltinData (Unsafe.head ell2)
        ell3 = Unsafe.tail ell2
        pmr = unsafeFromBuiltinData (Unsafe.head ell3)
     in UpdateCommitteeHashRedeemer cses oldPks newPks pmr

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
  { checkpointCommitteeSignatures :: [BuiltinByteString]
  , checkpointCommitteePubKeys :: [SidechainPubKey]
  , newCheckpointBlockHash :: BuiltinByteString
  , newCheckpointBlockNumber :: Integer
  }

-- | @since Unreleased
instance ToData CheckpointRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (CheckpointRedeemer {..}) =
    Unsafe.mkList
      ( Unsafe.mkCons
          (toBuiltinData checkpointCommitteeSignatures)
          ( Unsafe.mkCons
              (toBuiltinData checkpointCommitteePubKeys)
              ( Unsafe.mkCons
                  (toBuiltinData newCheckpointBlockHash)
                  ( Unsafe.mkCons
                      (toBuiltinData newCheckpointBlockNumber)
                      (Unsafe.mkNilData Unsafe.unitval)
                  )
              )
          )
      )

-- | @since Unreleased
instance FromData CheckpointRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData dat = Unsafe.chooseData dat Nothing Nothing go Nothing Nothing
    where
      go :: Maybe CheckpointRedeemer
      go =
        let ell0 = Unsafe.unsafeDataAsList dat
         in matchList ell0 Nothing $ \cses ell1 ->
              case fromBuiltinData cses of
                Nothing -> Nothing
                Just cses' -> matchList ell1 Nothing $ \pks ell2 ->
                  case fromBuiltinData pks of
                    Nothing -> Nothing
                    Just pks' -> matchList ell2 Nothing $ \bh ell3 ->
                      case fromBuiltinData bh of
                        Nothing -> Nothing
                        Just bh' -> matchList ell3 Nothing $ \bn ell4 ->
                          case fromBuiltinData bn of
                            Nothing -> Nothing
                            Just bn' ->
                              matchList
                                ell4
                                (Just (CheckpointRedeemer cses' pks' bh' bn'))
                                (\_ _ -> Nothing)

-- | @since Unreleased
instance UnsafeFromData CheckpointRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData dat =
    let ell0 = Unsafe.unsafeDataAsList dat
        cses = unsafeFromBuiltinData (Unsafe.head ell0)
        ell1 = Unsafe.tail ell0
        pks = unsafeFromBuiltinData (Unsafe.head ell1)
        ell2 = Unsafe.tail ell1
        bh = unsafeFromBuiltinData (Unsafe.head ell2)
        ell3 = Unsafe.tail ell2
        bn = unsafeFromBuiltinData (Unsafe.head ell3)
     in CheckpointRedeemer cses pks bh bn

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
  , checkpointMsgBlockHash :: BuiltinByteString
  , checkpointMsgBlockNumber :: Integer
  , checkpointMsgSidechainEpoch :: Integer
  }

PlutusTx.makeIsDataIndexed ''CheckpointMessage [('CheckpointMessage, 0)]
