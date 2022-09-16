module MPTRoot where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData (class ToData, PlutusData(Constr), toData, unitDatum)
import Contract.Prim.ByteArray (byteArrayFromAscii)
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
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.BigInt (BigInt, fromInt)
import RawScripts (rawMPTRootTokenMintingPolicy, rawMPTRootTokenValidator)
import SidechainParams (SidechainParams)
import Types.Scripts (plutusV2Script)

data SignedMerkleRoot = SignedMerkleRoot
  { merkleRoot ∷ String
  , signatures ∷ Array String
  , threshold ∷ BigInt -- Natural: the number of committee pubkeys needed to sign off
  , committeePubKeys ∷
      Array PaymentPubKeyHash -- PubKey -- Public keys of all committee members
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

newtype SaveRootParams = SaveRootParams
  { sidechainParams ∷ SidechainParams
  , merkleRoot ∷ String
  , signatures ∷ Array String
  , threshold ∷ BigInt
  , committeePubKeys ∷
      Array PaymentPubKeyHash -- PubKey -- Public keys of all committee members
  }

derive instance Generic SaveRootParams _
derive instance Newtype SaveRootParams _
instance ToData SaveRootParams where
  toData
    ( SaveRootParams
        { sidechainParams, merkleRoot, signatures, threshold, committeePubKeys }
    ) = Constr zero
    [ toData sidechainParams
    , toData merkleRoot
    , toData signatures
    , toData threshold
    , toData committeePubKeys
    ]

getRootTokenMintingPolicy ∷ SidechainParams → Contract () MintingPolicy
getRootTokenMintingPolicy sp = do
  mptRootMP ← (plutusV2Script >>> MintingPolicy) <$> textEnvelopeBytes
    rawMPTRootTokenMintingPolicy
    PlutusScriptV2
  liftedE (applyArgs mptRootMP [ toData sp ])

getRootTokenValidator ∷ SidechainParams → Contract () Validator
getRootTokenValidator sp = do
  mptRootVal ← (plutusV2Script >>> Validator) <$> textEnvelopeBytes
    rawMPTRootTokenValidator
    PlutusScriptV2
  liftedE (applyArgs mptRootVal [ toData sp ])

saveRoot ∷ SaveRootParams → Contract () Unit
saveRoot
  ( SaveRootParams
      { sidechainParams, merkleRoot, threshold, signatures, committeePubKeys }
  ) = do
  rootTokenMP ← getRootTokenMintingPolicy sidechainParams
  rootTokenCS ← liftContractM "Cannot get currency symbol"
    (Value.scriptCurrencySymbol rootTokenMP)
  rootTokenVal ← getRootTokenValidator sidechainParams
  tn ← liftContractM "Cannot get token name"
    (Value.mkTokenName =<< byteArrayFromAscii merkleRoot)
  let
    value = Value.singleton rootTokenCS tn (fromInt 1)
    redeemer = SignedMerkleRoot
      { merkleRoot, signatures, threshold, committeePubKeys }

    constraints ∷ Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustMintValueWithRedeemer (wrap (toData redeemer)) value
        <> Constraints.mustPayToScript (validatorHash rootTokenVal) unitDatum
          value

    lookups ∷ Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy rootTokenMP
  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedM "Failed to balance/sign tx" (balanceAndSignTx ubTx)
  txId ← submit bsTx
  logInfo' ("Submitted saveRoot Tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' "saveRoot Tx submitted successfully!"
