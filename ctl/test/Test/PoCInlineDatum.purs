-- | Some proof of concept tests for using inline datums.
module Test.PoCInlineDatum (testScenario1, testScenario2) where

import Contract.Prelude

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
  )
import Contract.Transaction as Transaction
import Contract.TxConstraints
  ( DatumPresence(DatumInline, DatumWitness)
  , TxConstraints
  )
import Contract.TxConstraints as TxConstraints
import Contract.Utxos as Utxos
import Contract.Value as Value
import Control.Applicative as Applicative
import Data.BigInt as BigInt
import Data.FoldableWithIndex as FoldableWithIndex
import Data.Map as Map
import RawScripts as RawScripts
import Test.Utils as Test.Utils

-- | 'testScenario1' goes as follows:
--  1.
--      - Grabs the validator for 'RawScripts.rawPoCInlineDatum'
--  2.
--      - Build / submit the transaction to pay some ada to the
--      'RawScripts.rawPoCInlineDatum' validator which holds the integer 69 as an
--      inline datum
--  3.
--      - Build / submit another transaction to consume the previous utxo and verify that
--      the inline datum really was 69
testScenario1 ∷ Contract () Unit
testScenario1 = do
  Log.logInfo' "PoCInlineDatum: testScenario1"

  -- 1.
  validatorBytes ← TextEnvelope.textEnvelopeBytes RawScripts.rawPoCInlineDatum
    PlutusScriptV2
  let
    validator = wrap $ wrap $ validatorBytes /\ PlutusV2 ∷ Validator
    validatorHash = Scripts.validatorHash validator
    validatorDat = Datum $ PlutusData.toData $ BigInt.fromInt 69
    validatorAddress = Address.scriptHashAddress validatorHash

  -- 2.
  void do
    let
      constraints ∷ TxConstraints Void Void
      constraints = TxConstraints.mustPayToScript validatorHash validatorDat
        DatumInline
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
    utxoMap ← Monad.liftedM "Failed to get utxos at script address" $
      Utxos.utxosAt validatorAddress

    Applicative.when (length utxoMap /= 1)
      $ Monad.throwContractError
      $ "Expected exactly one PoCInlineDatum script address but got:"
      <> show utxoMap

    FoldableWithIndex.forWithIndex_ utxoMap $ \txIn txOut → void do
      let
        validatorRedeemer = Redeemer $ PlutusData.toData $ BigInt.fromInt 69

        constraints ∷ TxConstraints Void Void
        constraints = TxConstraints.mustSpendScriptOutput txIn validatorRedeemer

        lookups ∷ ScriptLookups Void
        lookups = ScriptLookups.unspentOutputs (Map.singleton txIn txOut)
          <> ScriptLookups.validator validator

      unbalancedTx ← Monad.liftedE $ ScriptLookups.mkUnbalancedTx lookups
        constraints
      balancedTx ← Monad.liftedE $ Transaction.balanceAndSignTxE unbalancedTx
      txId ← Transaction.submit balancedTx
      Log.logInfo' $ "Transaction submitted: " <> show txId
      Transaction.awaitTxConfirmed txId
      Log.logInfo' $ "Transaction confirmed: " <> show txId

  pure unit

-- | 'testScenario2' goes as follows:
--  1.
--      Grabs the validator
--  2.
--      Build / submit the transaction to pay some ada to the
--      'RawScripts.rawPoCInlineDatum' validator which holds the integer 69 as a
--       witness datum
--  3.
--      Build / submit another transaction to consume the previous utxo and verify this
--      transaction should fail because there is no inline datum.
testScenario2 ∷ Contract () Unit
testScenario2 = do
  Log.logInfo' "PoCInlineDatum: testScenario2"

  -- 1.
  validatorBytes ← TextEnvelope.textEnvelopeBytes RawScripts.rawPoCInlineDatum
    PlutusScriptV2
  let
    validator = wrap $ wrap $ validatorBytes /\ PlutusV2 ∷ Validator
    validatorHash = Scripts.validatorHash validator
    validatorDat = Datum $ PlutusData.toData $ BigInt.fromInt 69
    validatorAddress = Address.scriptHashAddress validatorHash

  -- 2.
  void do
    let
      constraints ∷ TxConstraints Void Void
      constraints = TxConstraints.mustPayToScript validatorHash validatorDat
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
  Test.Utils.fails do
    utxoMap ← Monad.liftedM "Failed to get utxos at script address" $
      Utxos.utxosAt validatorAddress

    Applicative.when (length utxoMap /= 1)
      $ Monad.throwContractError
      $ "Expected exactly one PoCInlineDatum script address but got:"
      <> show utxoMap

    FoldableWithIndex.forWithIndex_ utxoMap $ \txIn txOut → void do
      let
        validatorRedeemer = Redeemer $ PlutusData.toData $ BigInt.fromInt 69

        constraints ∷ TxConstraints Void Void
        constraints = TxConstraints.mustSpendScriptOutput txIn validatorRedeemer

        lookups ∷ ScriptLookups Void
        lookups = ScriptLookups.unspentOutputs (Map.singleton txIn txOut)
          <> ScriptLookups.validator validator

      unbalancedTx ← Monad.liftedE $ ScriptLookups.mkUnbalancedTx lookups
        constraints
      balancedTx ← Monad.liftedE $ Transaction.balanceAndSignTxE unbalancedTx
      txId ← Transaction.submit balancedTx
      Log.logInfo' $ "Transaction submitted: " <> show txId
      Transaction.awaitTxConfirmed txId
      Log.logInfo' $ "Transaction confirmed: " <> show txId

  pure unit
