module MPTRoot where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData (class ToData, PlutusData(Constr), toData, unitDatum)
import Contract.Prim.ByteArray (ByteArray, byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy(..)
  , Validator(..)
  , applyArgs
  , validatorHash
  )
import Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV2)
  , textEnvelopeBytes
  )
import Contract.Transaction (awaitTxConfirmed, balanceAndSignTx, submit)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as TxConstraints
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import Data.BigInt (BigInt)
import RawScripts (rawMPTRootTokenMintingPolicy, rawMPTRootTokenValidator)
import SidechainParams (SidechainParams)
import Types (PubKey, Signature)
import Types.Scripts (plutusV2Script)
import UpdateCommitteeHash
  ( InitCommitteeHashMint(InitCommitteeHashMint)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  )
import UpdateCommitteeHash as UpdateCommitteeHash

-- | 'SignedMerkleRoot' is the redeemer for the minting policy.
data SignedMerkleRoot = SignedMerkleRoot
  { merkleRoot ∷ ByteArray
  , signatures ∷ Array Signature
  , threshold ∷ BigInt -- Natural: the number of committee pubkeys needed to sign off
  , committeePubKeys ∷
      Array PubKey -- Sorted public keys of all committee members
  }

derive instance Generic SignedMerkleRoot _
instance ToData SignedMerkleRoot where
  toData
    (SignedMerkleRoot { merkleRoot, signatures, threshold, committeePubKeys }) =
    Constr zero
      [ toData merkleRoot
      , toData signatures
      , toData threshold
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
  , signatures ∷ Array Signature
  , threshold ∷ BigInt
  , committeePubKeys ∷ Array PubKey -- Public keys of all committee members.
  }

-- | 'mptRootTokenMintingPolicy' gets the minting policy corresponding to
-- 'RawScripts.rawMPTRootTokenMintingPolicy' paramaterized by the given
-- 'SignedMerkleRootMint'.
mptRootTokenMintingPolicy ∷ SignedMerkleRootMint → Contract () MintingPolicy
mptRootTokenMintingPolicy sp = do
  mptRootMP ← (plutusV2Script >>> MintingPolicy) <$> textEnvelopeBytes
    rawMPTRootTokenMintingPolicy
    PlutusScriptV2
  liftedE (applyArgs mptRootMP [ toData sp ])

-- | 'mptRootTokenValidator' gets the minting validator corresponding to
-- 'RawScripts.rawMPTRootTokenValidator' paramaterized by 'SidechainParams'.
mptRootTokenValidator ∷ SidechainParams → Contract () Validator
mptRootTokenValidator sp = do
  mptRootVal ← (plutusV2Script >>> Validator) <$> textEnvelopeBytes
    rawMPTRootTokenValidator
    PlutusScriptV2
  liftedE (applyArgs mptRootVal [ toData sp ])

-- | 'saveRoot' is the endpoint.
saveRoot ∷ SaveRootParams → Contract () Unit
saveRoot
  ( SaveRootParams
      { sidechainParams, merkleRoot, threshold, signatures, committeePubKeys }
  ) = do
  -- Getting the required validators / minting policies...
  ---------------------------------------------------------
  updateCommitteeHashPolicy ← UpdateCommitteeHash.committeeHashPolicy
    $ InitCommitteeHashMint { icTxOutRef: (unwrap sidechainParams).genesisUtxo }
  updateCommitteeHashCurrencySymbol ←
    liftContractM
      "error 'saveRoot': failed to get update committee hash currency symbol"
      $ Value.scriptCurrencySymbol updateCommitteeHashPolicy
  rootTokenMP ← mptRootTokenMintingPolicy $
    SignedMerkleRootMint
      { sidechainParams
      , updateCommitteeHashCurrencySymbol
      }
  rootTokenCS ← liftContractM "Cannot get currency symbol"
    (Value.scriptCurrencySymbol rootTokenMP)
  rootTokenVal ← mptRootTokenValidator sidechainParams
  tn ← liftContractM "Cannot get token name"
    (Value.mkTokenName merkleRoot)

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
    value = Value.singleton rootTokenCS tn one
    redeemer = SignedMerkleRoot
      { merkleRoot, signatures, threshold, committeePubKeys }

    constraints ∷ TxConstraints Void Void
    constraints =
      TxConstraints.mustMintValueWithRedeemer (wrap (toData redeemer)) value
        <> TxConstraints.mustPayToScript (validatorHash rootTokenVal) unitDatum
          TxConstraints.DatumWitness
          value
        <> TxConstraints.mustReferenceOutput committeeHashTxIn

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
