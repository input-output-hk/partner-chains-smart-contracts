module MPTRoot where

import Contract.Address (PaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData (class ToData, PlutusData(Constr), toData, unitDatum)
import Contract.Prelude
  ( class Generic
  , Unit
  , Void
  , bind
  , discard
  , negate
  , one
  , show
  , wrap
  , zero
  , (<$>)
  , (<>)
  , (=<<)
  , (>>>)
  )
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy(..)
  , Validator(..)
  , applyArgs
  , validatorHash
  )
import Contract.TextEnvelope (TextEnvelopeType(..), textEnvelopeBytes)
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

rootTokenMintingPolicy ∷ SidechainParams → Contract () MintingPolicy
rootTokenMintingPolicy sp = do
  mptRootMP ← (plutusV2Script >>> MintingPolicy) <$> textEnvelopeBytes
    rawMPTRootTokenMintingPolicy
    PlutusScriptV2
  liftedE (applyArgs mptRootMP [ toData sp ])

rootTokenValidator ∷ SidechainParams → Contract () Validator
rootTokenValidator sp = do
  mptRootVal ← (plutusV2Script >>> Validator) <$> textEnvelopeBytes
    rawMPTRootTokenValidator
    PlutusScriptV2
  liftedE (applyArgs mptRootVal [ toData sp ])

saveRoot ∷ SaveRootParams → Contract () Unit
saveRoot
  ( SaveRootParams
      { sidechainParams, merkleRoot, threshold, signatures, committeePubKeys }
  ) = do
  rootTokenMP ← rootTokenMintingPolicy sidechainParams
  rootTokenCS ← liftContractM "Cannot get currency symbol"
    (Value.scriptCurrencySymbol rootTokenMP)
  rootTokenVal ← rootTokenValidator sidechainParams
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
  --bsTx ← liftedM "Failed to balance/sign tx" (balanceAndSignTx ubTx)
  bsTx ← balanceAndSignTx ubTx
  txId ← submit bsTx
  logInfo' ("Submitted saveRoot Tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' "saveRoot Tx submitted successfully!"
