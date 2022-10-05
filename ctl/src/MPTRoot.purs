module MPTRoot where

import Contract.Prelude

import Contract.Address as Address
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData (class ToData, PlutusData(Constr), toData, unitDatum)
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy(..)
  , Validator(..)
  )
import Contract.Scripts as Scripts
import Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV2)
  , textEnvelopeBytes
  )
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript
  , awaitTxConfirmed
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as TxConstraints
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import RawScripts (rawMPTRootTokenMintingPolicy, rawMPTRootTokenValidator)
import SidechainParams (SidechainParams)
import Types (PubKey, Signature)
import Types.Scripts (plutusV2Script)
import UpdateCommitteeHash
  ( InitCommitteeHashMint(InitCommitteeHashMint)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  )
import UpdateCommitteeHash as UpdateCommitteeHash
import Utils.Crypto as Utils.Crypto
import Utils.Utxos as UtilsUtxos

-- | 'SignedMerkleRoot' is the redeemer for the minting policy.
data SignedMerkleRoot = SignedMerkleRoot
  { -- The new merkle root to insert.
    merkleRoot ∷ ByteArray
  , -- Either 'Just' the last merkle root (in the case it exists), or 'Nothing'
    -- if there is no such last merkle root (i.e., in the first transaction).
    lastMerkleRoot ∷ Maybe ByteArray
  , -- Ordered as their corresponding keys (also the same length as
    -- 'committeePubKeys')
    signatures ∷ Array Signature
  , -- Sorted public keys of all committee members
    committeePubKeys ∷ Array PubKey
  }

derive instance Generic SignedMerkleRoot _
instance ToData SignedMerkleRoot where
  toData
    ( SignedMerkleRoot
        { merkleRoot, lastMerkleRoot, signatures, committeePubKeys }
    ) =
    Constr zero
      [ toData merkleRoot
      , toData lastMerkleRoot
      , toData signatures
      , toData committeePubKeys
      ]

-- | 'SignedMerkleRootMint' parameterizes the onchain minting policy.
newtype SignedMerkleRootMint = SignedMerkleRootMint
  { -- | 'sidechainParams' includes the 'SidechainParams'
    sidechainParams ∷ SidechainParams
  , -- | 'updateCommitteeHashCurrencySymbol' is the 'CurrencySymbol' which
    -- (uniquely) identifies the utxo for which the 'UpdateCommitteeHashDatum'
    -- resides.
    updateCommitteeHashCurrencySymbol ∷ CurrencySymbol
  }

derive instance Generic SignedMerkleRootMint _
derive instance Newtype SignedMerkleRootMint _
instance ToData SignedMerkleRootMint where
  toData
    (SignedMerkleRootMint { sidechainParams, updateCommitteeHashCurrencySymbol }) =
    Constr zero
      [ toData sidechainParams
      , toData updateCommitteeHashCurrencySymbol
      ]

-- | 'SaveRootParams' is the offchain parameter for MPTRoot ('saveRoot')
-- endpoint.
newtype SaveRootParams = SaveRootParams
  { sidechainParams ∷ SidechainParams
  , merkleRoot ∷ ByteArray
  , lastMerkleRoot ∷ Maybe ByteArray
  , -- Public keys of all committee members and their corresponding signatures.
    committeeSignatures ∷ Array (PubKey /\ Maybe Signature)
  }

-- | 'mptRootTokenMintingPolicy' gets the minting policy corresponding to
-- 'RawScripts.rawMPTRootTokenMintingPolicy' paramaterized by the given
-- 'SignedMerkleRootMint'.
mptRootTokenMintingPolicy ∷ SignedMerkleRootMint → Contract () MintingPolicy
mptRootTokenMintingPolicy sp = do
  mptRootMP ← (plutusV2Script >>> MintingPolicy) <$> textEnvelopeBytes
    rawMPTRootTokenMintingPolicy
    PlutusScriptV2
  liftedE (Scripts.applyArgs mptRootMP [ toData sp ])

-- | @'findMptRootTokenUtxo' merkleRoot smrm@ locates a utxo which
--
--      1. is sitting at the @'mptRootTokenValidator' smrm.sidechainParams@
--      utxo
--
--      2. contains a token with 'CurrencySymbol' @'mptRootTokenMintingPolicy' smrm@
--      and 'TokenName' as @merkleRoot@.
--
-- Note: in the case that there is more than such utxo, this returns the first
-- such utxo it finds that satisifies the aforementioned properties.
findMptRootTokenUtxo ∷
  TokenName →
  SignedMerkleRootMint →
  Contract ()
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript })
findMptRootTokenUtxo merkleRoot smrm = do
  netId ← Address.getNetworkId
  validator ← mptRootTokenValidator (unwrap smrm).sidechainParams
  let validatorHash = Scripts.validatorHash validator

  validatorAddress ← liftContractM
    "error 'findMptRootTokenUtxo': failed to get validator address"
    (Address.validatorHashEnterpriseAddress netId validatorHash)

  mintingPolicy ← mptRootTokenMintingPolicy smrm
  currencySymbol ←
    liftContractM
      "error 'findMptRootTokenUtxo': failed to get currency symbol for minting policy"
      $ Value.scriptCurrencySymbol mintingPolicy

  UtilsUtxos.findUtxoByValueAt validatorAddress \value →
    -- Note: we just need the existence of the token i.e., there is a nonzero
    -- amount
    Value.valueOf value currencySymbol merkleRoot /= zero

-- | 'mptRootTokenValidator' gets the minting validator corresponding to
-- 'RawScripts.rawMPTRootTokenValidator' paramaterized by 'SidechainParams'.
mptRootTokenValidator ∷ SidechainParams → Contract () Validator
mptRootTokenValidator sp = do
  mptRootVal ← (plutusV2Script >>> Validator) <$> textEnvelopeBytes
    rawMPTRootTokenValidator
    PlutusScriptV2
  liftedE (Scripts.applyArgs mptRootVal [ toData sp ])

-- | 'saveRoot' is the endpoint.
saveRoot ∷ SaveRootParams → Contract () Unit
saveRoot
  ( SaveRootParams
      { sidechainParams, merkleRoot, lastMerkleRoot, committeeSignatures }
  ) = do

  -- Getting the required validators / minting policies...
  ---------------------------------------------------------
  updateCommitteeHashPolicy ← UpdateCommitteeHash.committeeHashPolicy
    $ InitCommitteeHashMint { icTxOutRef: (unwrap sidechainParams).genesisUtxo }
  updateCommitteeHashCurrencySymbol ←
    liftContractM
      "error 'saveRoot': failed to get update committee hash currency symbol"
      $ Value.scriptCurrencySymbol updateCommitteeHashPolicy
  let
    smrm = SignedMerkleRootMint
      { sidechainParams
      , updateCommitteeHashCurrencySymbol
      }
  rootTokenMP ← mptRootTokenMintingPolicy smrm
  rootTokenCS ← liftContractM "Cannot get currency symbol"
    (Value.scriptCurrencySymbol rootTokenMP)
  rootTokenVal ← mptRootTokenValidator sidechainParams
  merkleRootTokenName ← liftContractM
    "error 'saveRoot': invalid merkle root token name"
    (Value.mkTokenName merkleRoot)

  -- Grab the transaction holding the last merkle root
  ---------------------------------------------------------
  -- 'Just' means that we found the corresponding last merkle root.
  -- 'Nothing' means that there was no such last merkle root.
  -- Note: the 'Maybe' does not indicate whether we have found it or not!
  maybeLastMerkleRootUtxo ← case lastMerkleRoot of
    Nothing → pure Nothing
    Just lastMerkleRoot' → do
      lastMerkleRootTokenName ← liftContractM
        "error 'saveRoot': invalid lastMerkleRoot token name"
        (Value.mkTokenName lastMerkleRoot')
      lkup ← findMptRootTokenUtxo lastMerkleRootTokenName smrm
      lkup' ←
        liftContractM "error 'saveRoot': failed to find last merkle root" $ lkup
      pure $ Just lkup'

  -- Grab the utxo with the current committee hash
  ---------------------------------------------------------
  let
    uch = UpdateCommitteeHash
      { uchAssetClass: updateCommitteeHashCurrencySymbol /\
          UpdateCommitteeHash.initCommitteeHashMintTn
      }
  { index: committeeHashTxIn, value: _committeeHashTxOut } ←
    liftedM "error 'saveRoot': failed to find committee hash utxo" $
      UpdateCommitteeHash.findUpdateCommitteeHashUtxo uch

  -- Building the transaction
  ---------------------------------------------------------
  let
    value = Value.singleton rootTokenCS merkleRootTokenName one
    committeePubKeys /\ signatures =
      Utils.Crypto.normalizeCommitteePubKeysAndSignatures committeeSignatures

    redeemer = SignedMerkleRoot
      { merkleRoot, lastMerkleRoot, signatures, committeePubKeys }

    constraints ∷ TxConstraints Void Void
    constraints =
      TxConstraints.mustMintValueWithRedeemer (wrap (toData redeemer)) value
        <> TxConstraints.mustPayToScript (Scripts.validatorHash rootTokenVal)
          unitDatum
          TxConstraints.DatumWitness
          value
        <> TxConstraints.mustReferenceOutput committeeHashTxIn
        <> case maybeLastMerkleRootUtxo of
          Nothing → mempty
          Just { index: oref } → TxConstraints.mustReferenceOutput oref

    lookups ∷ Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy rootTokenMP

  -- Submitting the transaction
  ---------------------------------------------------------
  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedM "Failed to balance/sign tx" (balanceAndSignTx ubTx)
  txId ← submit bsTx
  logInfo' ("Submitted saveRoot Tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' "saveRoot Tx submitted successfully!"
