module Test.PoCECDSA where

import Contract.Prelude

import Contract.Address (getNetworkId)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  , liftedM
  )
import Contract.PlutusData
  ( class ToData
  , PlutusData(Constr)
  , Redeemer(..)
  , toData
  , unitDatum
  )
import Contract.Prim.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( Validator(..)
  , validatorHash
  , validatorHashEnterpriseAddress
  )
import Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV2)
  , textEnvelopeBytes
  )
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , balanceAndSignTxE
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Set as Set
import RawScripts (rawPoCECDSA)
import Types.Scripts (plutusV2Script)

getValidator ∷ Contract () Validator
getValidator =
  (plutusV2Script >>> Validator) <$> textEnvelopeBytes
    rawPoCECDSA
    PlutusScriptV2

newtype ECDSARed = ECDSARed
  { msg ∷ ByteArray
  , sig ∷ ByteArray
  , pk ∷ ByteArray
  }

derive instance Generic ECDSARed _
derive instance Newtype ECDSARed _
instance ToData ECDSARed where
  toData (ECDSARed { msg, sig, pk }) = Constr zero
    [ toData msg, toData sig, toData pk ]

-- | Prepate the ECDSA test by locking some funds at the validator address
prepTest ∷ Contract () TransactionHash
prepTest = do
  validator ← getValidator
  let
    valHash = validatorHash validator
    val = Value.lovelaceValueOf (BigInt.fromInt 1)

    lookups ∷ Lookups.ScriptLookups Void
    lookups = Lookups.validator validator

    constraints ∷ Constraints.TxConstraints Void Void
    constraints = Constraints.mustPayToScript valHash unitDatum
      Constraints.DatumInline
      val
  ubTx ← liftedE $ Lookups.mkUnbalancedTx lookups constraints
  bsTx ← liftedE $ balanceAndSignTxE ubTx
  txId ← submit bsTx
  logInfo' $ ("Submitted ECDSA test prep tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' "Transaction confirmed."

  pure txId

-- | Attempt to unlock one utxo using an ECDSA signature
testVerification ∷ ECDSARed → Contract () TransactionHash
testVerification ecdsaRed = do
  let red = Redeemer $ toData ecdsaRed

  validator ← getValidator
  let valHash = validatorHash validator

  netId ← getNetworkId
  valAddr ← liftContractM "cannot get validator address"
    (validatorHashEnterpriseAddress netId valHash)

  scriptUtxos ← liftedM "Cannot get script utxos" (utxosAt valAddr)
  txIn ← liftContractM "No UTxOs found at validator address"
    $ Set.findMin
    $ Map.keys scriptUtxos
  let
    lookups ∷ Lookups.ScriptLookups Void
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs scriptUtxos

    constraints ∷ Constraints.TxConstraints Void Void
    constraints = Constraints.mustSpendScriptOutput txIn red
  ubTx ← liftedE $ Lookups.mkUnbalancedTx lookups constraints
  bsTx ← liftedE $ balanceAndSignTxE ubTx
  txId ← submit bsTx
  logInfo' $ ("Submitted ECDSA test verification tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' "Transaction confirmed."

  pure txId

-- | Testing ECDSA verification function on-chain
testScenario ∷ Contract () Unit
testScenario = do
  txId ← prepTest
  awaitTxConfirmed txId
  void $ testVerification $
    -- | TODO: Find the correct format
    ECDSARed
      { msg: hexToByteArrayUnsafe
          "16e0bf1f85594a11e75030981c0b670370b3ad83a43f49ae58a2fd6f6513cde9"
      , sig: hexToByteArrayUnsafe
          "5fb12954b28be6456feb080cfb8467b6f5677f62eb9ad231de7a575f4b6857512754fb5ef7e0e60e270832e7bb0e2f0dc271012fa9c46c02504aa0e798be6295"
      , pk: hexToByteArrayUnsafe
          "0492d7b94bc6a11c335a043ee1ff326b6eacee6230d3685861cd62bce350a172e03d86b820b8e013a21cf96f8e84a59fc96eec8e08b4e049b2bf6f6455f2606895"
      }
