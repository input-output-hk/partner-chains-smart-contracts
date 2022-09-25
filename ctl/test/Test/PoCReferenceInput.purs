-- | Some proof of concept tests for using reference inputs.
module Test.PoCReferenceInput (testScenario1, testScenario2) where

import Contract.Prelude

import Contract.Address (Address)
import Contract.Address as Address
import Contract.Log as Log
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer))
import Contract.PlutusData as PlutusData
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (Validator)
import Contract.Scripts as Scripts
import Contract.TextEnvelope (TextEnvelopeType(PlutusScriptV2))
import Contract.TextEnvelope as TextEnvelope
import Contract.Transaction
  ( Language(PlutusV2)
  , TransactionInput
  , TransactionOutputWithRefScript
  )
import Contract.Transaction as Transaction
import Contract.TxConstraints
  ( DatumPresence(DatumWitness)
  , TxConstraints
  )
import Contract.TxConstraints as TxConstraints
import Contract.Utxos as Utxos
import Contract.Value as Value
import Control.Monad.Error.Class as MonadError
import Data.BigInt as BigInt
import Data.Map as Map
import Effect.Exception as Exception
import RawScripts as RawScripts

-- |  @'headUtxo' addr@ gets the first utxo at the given address, and throws an
-- error if there is NOT exactly one utxo at this address.
headUtxo ∷
  Address → Contract () (Tuple TransactionInput TransactionOutputWithRefScript)
headUtxo addr = do
  utxoMap ← Monad.liftedM "Failed to get utxos at script address" $
    Utxos.utxosAt addr
  let
    err = Monad.throwContractError
      $ "Expected exactly one script address but got:"
      <> show utxoMap

  case Map.findMin utxoMap of
    Just { key, value }
      | length utxoMap == 1 → pure $ key /\ value
      | otherwise → err
    Nothing → err

-- | 'testScenario1' (which should succeed) goes as follows:
--  1.
--      Grabs the validators
--  2.
--      Build / submit the transaction to pay some ada to the
--      'RawScripts.rawPoCToReference' validator which holds the integer 69 as an
--      inline datum, and 'RawScripts.rawPoCReference'
--  3.
--      Build / submit another transaction such that the 'RawScripts.rawPoCReference'
--      references the 'RawScripts.rawPoCToReference' script and verifies that the
--      (witness) datum really is 69
testScenario1 ∷ Contract () Unit
testScenario1 = do
  -- 1.
  toReferenceValidatorBytes ← TextEnvelope.textEnvelopeBytes
    RawScripts.rawPoCToReference
    PlutusScriptV2
  let
    toReferenceValidator =
      wrap $ wrap $ toReferenceValidatorBytes /\ PlutusV2 ∷ Validator
    toReferenceValidatorHash = Scripts.validatorHash toReferenceValidator
    toReferenceValidatorDat = Datum $ PlutusData.toData $ BigInt.fromInt 69
    toReferenceValidatorAddress = Address.scriptHashAddress
      toReferenceValidatorHash

  referenceValidatorBytes ← TextEnvelope.textEnvelopeBytes
    RawScripts.rawPoCReference
    PlutusScriptV2
  let
    referenceValidatorUnapplied =
      wrap $ wrap $ referenceValidatorBytes /\ PlutusV2 ∷ Validator
  referenceValidator ← Monad.liftedE $ Scripts.applyArgs
    referenceValidatorUnapplied
    [ PlutusData.toData toReferenceValidatorAddress ]
  let
    referenceValidatorHash = Scripts.validatorHash referenceValidator
    referenceValidatorDat = Datum $ PlutusData.toData $ unit
    referenceValidatorAddress = Address.scriptHashAddress referenceValidatorHash

  -- 2.
  void do
    let
      constraints ∷ TxConstraints Void Void
      constraints =
        TxConstraints.mustPayToScript
          toReferenceValidatorHash
          toReferenceValidatorDat
          DatumWitness
          (Value.lovelaceValueOf one)
          <> TxConstraints.mustPayToScript
            referenceValidatorHash
            referenceValidatorDat
            DatumWitness
            (Value.lovelaceValueOf one)

      lookups ∷ ScriptLookups Void
      lookups = mempty

    unbalancedTx ← Monad.liftedE $ ScriptLookups.mkUnbalancedTx lookups
      constraints
    balancedTx ← Monad.liftedE $ Transaction.balanceAndSignTxE unbalancedTx
    txId ← Transaction.submit balancedTx
    Log.logInfo' $ "Transaction submitted: " <> show txId
    Transaction.awaitTxConfirmed txId
    Log.logInfo' $ "Transaction confirmed: " <> show txId

  -- 3.
  void do
    toReferenceIn /\ toReferenceOut ← headUtxo toReferenceValidatorAddress
    referenceIn /\ referenceOut ← headUtxo referenceValidatorAddress

    let
      referenceValidatorRedeemer = Redeemer $ PlutusData.toData $ BigInt.fromInt
        69

      constraints ∷ TxConstraints Void Void
      constraints =
        TxConstraints.mustReferenceOutput toReferenceIn
          <> TxConstraints.mustSpendScriptOutput referenceIn
            referenceValidatorRedeemer
          <> TxConstraints.mustIncludeDatum toReferenceValidatorDat

      lookups ∷ ScriptLookups Void
      lookups =
        ScriptLookups.unspentOutputs (Map.singleton referenceIn referenceOut)
          <> ScriptLookups.unspentOutputs
            (Map.singleton toReferenceIn toReferenceOut)
          <> ScriptLookups.validator referenceValidator

    unbalancedTx ← Monad.liftedE $ ScriptLookups.mkUnbalancedTx lookups
      constraints
    balancedTx ← Monad.liftedE $ Transaction.balanceAndSignTxE unbalancedTx
    txId ← Transaction.submit balancedTx
    Log.logInfo' $ "Transaction submitted: " <> show txId
    Transaction.awaitTxConfirmed txId
    Log.logInfo' $ "Transaction confirmed: " <> show txId

  pure unit

-- | 'testScenario2' (which should fail) goes as follows:
--  1.
--      Grabs the validators
--  2.
--      Build / submit the transaction to pay some ada to the
--      'RawScripts.rawPoCToReference' validator which holds the integer 69 as an
--      inline datum, and 'RawScripts.rawPoCReference'
--  3.
--      Build / submit another transaction such that the 'RawScripts.rawPoCReference'
--      CONSUMES the 'RawScripts.rawPoCToReference' script and verifies that the
--      (witness) datum really is 69. This should fail!
testScenario2 ∷ Contract () Unit
testScenario2 = do
  -- START of duplicated code from 'testScenario1'.
  -- 1.
  toReferenceValidatorBytes ← TextEnvelope.textEnvelopeBytes
    RawScripts.rawPoCToReference
    PlutusScriptV2
  let
    toReferenceValidator =
      wrap $ wrap $ toReferenceValidatorBytes /\ PlutusV2 ∷ Validator
    toReferenceValidatorHash = Scripts.validatorHash toReferenceValidator
    toReferenceValidatorDat = Datum $ PlutusData.toData $ BigInt.fromInt 69
    toReferenceValidatorAddress = Address.scriptHashAddress
      toReferenceValidatorHash

  referenceValidatorBytes ← TextEnvelope.textEnvelopeBytes
    RawScripts.rawPoCReference
    PlutusScriptV2
  let
    referenceValidatorUnapplied =
      wrap $ wrap $ referenceValidatorBytes /\ PlutusV2 ∷ Validator
  referenceValidator ← Monad.liftedE $ Scripts.applyArgs
    referenceValidatorUnapplied
    [ PlutusData.toData toReferenceValidatorAddress ]
  let
    referenceValidatorHash = Scripts.validatorHash referenceValidator
    referenceValidatorDat = Datum $ PlutusData.toData $ unit
    referenceValidatorAddress = Address.scriptHashAddress referenceValidatorHash

  -- 2.
  void do
    let
      constraints ∷ TxConstraints Void Void
      constraints =
        TxConstraints.mustPayToScript
          toReferenceValidatorHash
          toReferenceValidatorDat
          DatumWitness
          (Value.lovelaceValueOf one)
          <> TxConstraints.mustPayToScript
            referenceValidatorHash
            referenceValidatorDat
            DatumWitness
            (Value.lovelaceValueOf one)

      lookups ∷ ScriptLookups Void
      lookups = mempty

    unbalancedTx ← Monad.liftedE $ ScriptLookups.mkUnbalancedTx lookups
      constraints
    balancedTx ← Monad.liftedE $ Transaction.balanceAndSignTxE unbalancedTx
    txId ← Transaction.submit balancedTx
    Log.logInfo' $ "Transaction submitted: " <> show txId
    Transaction.awaitTxConfirmed txId
    Log.logInfo' $ "Transaction confirmed: " <> show txId

  -- END of duplicated code from 'testScenario1'.

  -- 3.
  void do
    result ← MonadError.try do
      toReferenceIn /\ toReferenceOut ← headUtxo toReferenceValidatorAddress
      referenceIn /\ referenceOut ← headUtxo referenceValidatorAddress

      let
        toReferenceValidatorRedeemer = Redeemer $ PlutusData.toData unit
        referenceValidatorRedeemer = Redeemer $ PlutusData.toData $ BigInt.fromInt
          69

        constraints ∷ TxConstraints Void Void
        constraints =
          TxConstraints.mustSpendScriptOutput toReferenceIn
            toReferenceValidatorRedeemer
            <> TxConstraints.mustSpendScriptOutput referenceIn
              referenceValidatorRedeemer
            <> TxConstraints.mustIncludeDatum toReferenceValidatorDat

        lookups ∷ ScriptLookups Void
        lookups =
          ScriptLookups.unspentOutputs (Map.singleton referenceIn referenceOut)
            <> ScriptLookups.unspentOutputs
              (Map.singleton toReferenceIn toReferenceOut)
            <> ScriptLookups.validator referenceValidator
            <> ScriptLookups.validator toReferenceValidator

      unbalancedTx ← Monad.liftedE $ ScriptLookups.mkUnbalancedTx lookups
        constraints
      balancedTx ← Monad.liftedE $ Transaction.balanceAndSignTxE unbalancedTx
      txId ← Transaction.submit balancedTx
      Log.logInfo' $ "Transaction submitted: " <> show txId
      Transaction.awaitTxConfirmed txId
      Log.logInfo' $ "Transaction confirmed: " <> show txId

    case result of
      Right _ →
        Monad.throwContractError $ Exception.error
          "Contract should have failed but it didn't."
      Left _err →
        pure unit

  pure unit
