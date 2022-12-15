-- | Some proof of concept tests for using reference scripts.
module Test.PoCReferenceScript (testScenario1, testScenario2) where

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
import Contract.Transaction (Language(PlutusV2), ScriptRef(PlutusScriptRef))
import Contract.Transaction as Transaction
import Contract.TxConstraints
  ( DatumPresence(DatumWitness)
  , InputWithScriptRef(SpendInput)
  , TxConstraints
  )
import Contract.TxConstraints as TxConstraints
import Contract.Value as Value
import Ctl.Internal.Hashing (scriptRefHash)
import Data.Map as Map
import RawScripts as RawScripts
import Test.Utils as Test.Utils

{- | 'testScenario1' runs the following contract (which should succeed):
1. Grabs the validators for
    - 'RawScripts.rawPoCToReferenceScript' which is a script which always succeeds.

    - 'RawScripts.rawPoCReferenceScript' which is a script that succeeds iff
      its redeemer (of type 'ScriptHash') matches the 'ScriptHash' of at least
      one input.

Note that we also create a 'ScriptRef' for 'RawScripts.rawPoCReferenceScript'
which means that later we can create a transaction which uses
'RawScripts.rawPoCReferenceScript' but doesn't include
'RawScripts.rawPoCReferenceScript' in the witness set.
We also compute the hash of the 'ScriptRef' for
'RawScripts.rawPoCReferenceScript'.

2. We pay some ada to two outputs

    Output 1:
         - has validator 'RawScripts.rawPoCToReferenceScript'
         - includes the script 'RawScripts.rawPoCReferenceScript' on chain (the
           script that we will reference later)

    Output 2:
        - has has validator 'RawScripts.rawPoCReferenceScript'

3. We consume Output 1 and Output 2 by building a transaction as follows (this should succed)
    - Spending Output 1
    - Spending Output 2 with redeemer as the script hash of
      'RawScripts.rawPoCReferenceScript' (i.e., itself)
    - Include the validator 'RawScripts.rawPoCToReferenceScript' in the witness
      set
    - Do NOT Include the validator 'RawScripts.rawPoCReferenceScript' as this
      is given from the reference script in Output 1.
-}
testScenario1 ∷ Contract () Unit
testScenario1 = do
  Log.logInfo' "PoCReferenceScript: testScenario1"
  -- 1.
  toReferenceValidatorBytes ← TextEnvelope.textEnvelopeBytes
    RawScripts.rawPoCToReferenceScript
    PlutusScriptV2
  let
    toReferenceValidator =
      wrap $ wrap $ toReferenceValidatorBytes /\ PlutusV2 ∷ Validator
    toReferenceValidatorHash = Scripts.validatorHash toReferenceValidator
    toReferenceValidatorDat = Datum $ PlutusData.toData $ unit
    toReferenceValidatorAddress = Address.scriptHashAddress
      toReferenceValidatorHash

  referenceValidatorBytes ← TextEnvelope.textEnvelopeBytes
    RawScripts.rawPoCReferenceScript
    PlutusScriptV2
  let
    referenceValidator =
      wrap $ wrap $ referenceValidatorBytes /\ PlutusV2 ∷ Validator
    referenceValidatorHash = Scripts.validatorHash referenceValidator
    referenceValidatorDat = Datum $ PlutusData.toData $ unit
    referenceValidatorAddress = Address.scriptHashAddress referenceValidatorHash

    referenceScriptRef = PlutusScriptRef (unwrap referenceValidator) ∷ ScriptRef
    referenceScriptHash = scriptRefHash referenceScriptRef

  -- 2.
  void do
    let
      constraints ∷ TxConstraints Void Void
      constraints =
        TxConstraints.mustPayToScriptWithScriptRef
          toReferenceValidatorHash
          toReferenceValidatorDat
          DatumWitness
          referenceScriptRef
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

    bsTx ← Monad.liftedE $ Transaction.balanceTx unbalancedTx
    signedTx ← Transaction.signTransaction bsTx
    txId ← Transaction.submit signedTx
    Log.logInfo' $ "Transaction submitted: " <> show txId
    Transaction.awaitTxConfirmed txId
    Log.logInfo' $ "Transaction confirmed: " <> show txId

  -- 3.
  void do
    toReferenceIn /\ toReferenceOut ← Test.Utils.getUniqueUtxoAt
      toReferenceValidatorAddress
    referenceIn /\ referenceOut ← Test.Utils.getUniqueUtxoAt
      referenceValidatorAddress

    let
      toReferenceValidatorRedeemer = Redeemer $ PlutusData.toData $ unit
      referenceValidatorRedeemer = Redeemer $ PlutusData.toData $
        referenceScriptHash

      constraints ∷ TxConstraints Void Void
      constraints =
        TxConstraints.mustSpendScriptOutputUsingScriptRef
          referenceIn
          referenceValidatorRedeemer
          (SpendInput (Transaction.mkTxUnspentOut toReferenceIn toReferenceOut))
          <> TxConstraints.mustSpendScriptOutput toReferenceIn
            toReferenceValidatorRedeemer
          <> TxConstraints.mustIncludeDatum toReferenceValidatorDat
          <> TxConstraints.mustIncludeDatum referenceValidatorDat

      lookups ∷ ScriptLookups Void
      lookups =
        ScriptLookups.unspentOutputs (Map.singleton referenceIn referenceOut)
          <> ScriptLookups.unspentOutputs
            (Map.singleton toReferenceIn toReferenceOut)
          <> ScriptLookups.validator toReferenceValidator

    unbalancedTx ← Monad.liftedE $ ScriptLookups.mkUnbalancedTx lookups
      constraints
    bsTx ← Monad.liftedE $ Transaction.balanceTx unbalancedTx
    signedTx ← Transaction.signTransaction bsTx
    txId ← Transaction.submit signedTx
    Log.logInfo' $ "Transaction submitted: " <> show txId
    Transaction.awaitTxConfirmed txId
    Log.logInfo' $ "Transaction confirmed: " <> show txId

  pure unit

{- | 'testScenario2' is the same as 'testScenario1', but changes 2. to not
include the script on chain, and hence 3. should fail.
-}
testScenario2 ∷ Contract () Unit
testScenario2 = do
  Log.logInfo' "PoCReferenceScript: testScenario2"
  -- START of duplicated code from 'testScenario1'
  -- 1.
  toReferenceValidatorBytes ← TextEnvelope.textEnvelopeBytes
    RawScripts.rawPoCToReferenceScript
    PlutusScriptV2
  let
    toReferenceValidator =
      wrap $ wrap $ toReferenceValidatorBytes /\ PlutusV2 ∷ Validator
    toReferenceValidatorHash = Scripts.validatorHash toReferenceValidator
    toReferenceValidatorDat = Datum $ PlutusData.toData $ unit
    toReferenceValidatorAddress = Address.scriptHashAddress
      toReferenceValidatorHash

  referenceValidatorBytes ← TextEnvelope.textEnvelopeBytes
    RawScripts.rawPoCReferenceScript
    PlutusScriptV2
  let
    referenceValidator =
      wrap $ wrap $ referenceValidatorBytes /\ PlutusV2 ∷ Validator
    referenceValidatorHash = Scripts.validatorHash referenceValidator
    referenceValidatorDat = Datum $ PlutusData.toData $ unit
    referenceValidatorAddress = Address.scriptHashAddress referenceValidatorHash

    referenceScriptRef = PlutusScriptRef (unwrap referenceValidator) ∷ ScriptRef
    referenceScriptHash = scriptRefHash referenceScriptRef

  -- END of duplicated code from 'testScenario1'

  -- 2.
  void do
    let
      constraints ∷ TxConstraints Void Void
      constraints =
        -- START: of line that changes in 2.
        TxConstraints.mustPayToScript
          toReferenceValidatorHash
          toReferenceValidatorDat
          DatumWitness
          (Value.lovelaceValueOf one)
          -- END: of line that changes in 2.
          <> TxConstraints.mustPayToScript
            referenceValidatorHash
            referenceValidatorDat
            DatumWitness
            (Value.lovelaceValueOf one)

      lookups ∷ ScriptLookups Void
      lookups = mempty

    unbalancedTx ← Monad.liftedE $ ScriptLookups.mkUnbalancedTx lookups
      constraints
    balancedTx ← Monad.liftedE $ Transaction.balanceTx unbalancedTx
    signedTx ← Transaction.signTransaction balancedTx
    txId ← Transaction.submit signedTx
    Log.logInfo' $ "Transaction submitted: " <> show txId
    Transaction.awaitTxConfirmed txId
    Log.logInfo' $ "Transaction confirmed: " <> show txId

  -- 3.
  Test.Utils.fails do
    toReferenceIn /\ toReferenceOut ← Test.Utils.getUniqueUtxoAt
      toReferenceValidatorAddress
    referenceIn /\ referenceOut ← Test.Utils.getUniqueUtxoAt
      referenceValidatorAddress

    let
      toReferenceValidatorRedeemer = Redeemer $ PlutusData.toData $ unit
      referenceValidatorRedeemer = Redeemer $ PlutusData.toData $
        referenceScriptHash

      constraints ∷ TxConstraints Void Void
      constraints =
        -- START: of line that changes in 3.
        TxConstraints.mustSpendScriptOutput referenceIn
          referenceValidatorRedeemer
          -- START: of line that changes in 3.
          <> TxConstraints.mustSpendScriptOutput toReferenceIn
            toReferenceValidatorRedeemer
          <> TxConstraints.mustIncludeDatum toReferenceValidatorDat
          <> TxConstraints.mustIncludeDatum referenceValidatorDat

      lookups ∷ ScriptLookups Void
      lookups =
        ScriptLookups.unspentOutputs (Map.singleton referenceIn referenceOut)
          <> ScriptLookups.unspentOutputs
            (Map.singleton toReferenceIn toReferenceOut)
          <> ScriptLookups.validator toReferenceValidator

    unbalancedTx ← Monad.liftedE $ ScriptLookups.mkUnbalancedTx lookups
      constraints
    balancedTx ← Monad.liftedE $ Transaction.balanceTx unbalancedTx
    signedTx ← Transaction.signTransaction balancedTx
    txId ← Transaction.submit signedTx
    Log.logInfo' $ "Transaction submitted: " <> show txId
    Transaction.awaitTxConfirmed txId
    Log.logInfo' $ "Transaction confirmed: " <> show txId

  pure unit
