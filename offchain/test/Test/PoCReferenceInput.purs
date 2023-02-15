-- | Some proof of concept tests for using reference inputs.
module Test.PoCReferenceInput (tests, testScenario1, testScenario2) where

import Contract.Prelude

import Contract.Address as Address
import Contract.Log as Log
import Contract.Monad as Monad
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer))
import Contract.PlutusData as PlutusData
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (Validator(Validator))
import Contract.Scripts as Scripts
import Contract.TextEnvelope
  ( decodeTextEnvelope
  , plutusScriptV2FromEnvelope
  )
import Contract.Transaction as Transaction
import Contract.TxConstraints (DatumPresence(DatumWitness), TxConstraints)
import Contract.TxConstraints as TxConstraints
import Contract.Value as Value
import Contract.Wallet as Wallet
import Data.BigInt as BigInt
import Data.Map as Map
import Mote.Monad as Mote.Monad
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils as Test.Utils
import TrustlessSidechain.RawScripts as RawScripts

-- | `tests` aggregates all the PoCReferenceInput together conveniently
tests ∷ PlutipTest
tests = Mote.Monad.group "PoCReferenceInput tests" do
  testScenario1
  testScenario2

-- | `testScenario1` (which should succeed) goes as follows:
-- |
-- | 1. Grabs the validators
-- |
-- | 2. Build / submit the transaction to pay some ada to the
-- |     `RawScripts.rawPoCToReferenceInput` validator which holds the integer 69 as an
-- |     inline datum, and `RawScripts.rawPoCReferenceInput`
-- | 3.
-- |     Build / submit another transaction such that the `RawScripts.rawPoCReferenceInput`
-- |     references the `RawScripts.rawPoCToReferenceInput` script and verifies that the
-- |     (witness) datum really is 69
testScenario1 ∷ PlutipTest
testScenario1 = Mote.Monad.test "PoCReferenceInput: testScenario1"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
  $ \alice → Wallet.withKeyWallet alice do
      -- 1.
      let
        toReferenceScript = decodeTextEnvelope RawScripts.rawPoCToReferenceInput
          >>= plutusScriptV2FromEnvelope

      toReferenceUnapplied ← Monad.liftContractM "Decoding text envelope failed."
        toReferenceScript
      let
        toReferenceValidator = Validator toReferenceUnapplied
        toReferenceValidatorHash = Scripts.validatorHash toReferenceValidator
        toReferenceValidatorDat = Datum $ PlutusData.toData $ BigInt.fromInt 69
        toReferenceValidatorAddress = Address.scriptHashAddress
          toReferenceValidatorHash
          Nothing

      let
        referenceScript = decodeTextEnvelope RawScripts.rawPoCReferenceInput
          >>= plutusScriptV2FromEnvelope

      referenceUnapplied ← Monad.liftContractM "Decoding text envelope failed."
        referenceScript
      referenceApplied ← Monad.liftContractE $ Scripts.applyArgs
        referenceUnapplied
        [ PlutusData.toData toReferenceValidatorAddress ]
      let
        referenceValidator = Validator referenceApplied
        referenceValidatorHash = Scripts.validatorHash referenceValidator
        referenceValidatorDat = Datum $ PlutusData.toData $ unit
        referenceValidatorAddress = Address.scriptHashAddress
          referenceValidatorHash
          Nothing

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
          referenceValidatorRedeemer = Redeemer $ PlutusData.toData $
            BigInt.fromInt
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
        bsTx ← Monad.liftedE $ Transaction.balanceTx unbalancedTx
        signedTx ← Transaction.signTransaction bsTx
        txId ← Transaction.submit signedTx
        Log.logInfo' $ "Transaction submitted: " <> show txId
        Transaction.awaitTxConfirmed txId
        Log.logInfo' $ "Transaction confirmed: " <> show txId

      pure unit

-- | `testScenario2` (which should fail) goes as follows:
-- |
-- |    1. Grabs the validators
-- |
-- |    2. Build / submit the transaction to pay some ada to the
-- |    `RawScripts.rawPoCToReferenceInput` validator which holds the
-- |    integer 69 as an inline datum, and `RawScripts.rawPoCReferenceInput`
-- |
-- |    3. Build / submit another transaction such that the
-- |    `RawScripts.rawPoCReferenceInput` CONSUMES the `RawScripts.rawPoCToReferenceInput`
-- |    script and verifies that the (witness) datum really is 69. This should fail!
testScenario2 ∷ PlutipTest
testScenario2 = Mote.Monad.test "PoCReferenceInput: testScenario2"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
  $ \alice → Wallet.withKeyWallet alice do
      -- START of duplicated code from `testScenario1`.
      -- 1.
      let
        toReferenceScript = decodeTextEnvelope RawScripts.rawPoCToReferenceInput
          >>= plutusScriptV2FromEnvelope

      toReferenceUnapplied ← Monad.liftContractM "Decoding text envelope failed."
        toReferenceScript
      let
        toReferenceValidator = Validator toReferenceUnapplied
        toReferenceValidatorHash = Scripts.validatorHash toReferenceValidator
        toReferenceValidatorDat = Datum $ PlutusData.toData $ BigInt.fromInt 69
        toReferenceValidatorAddress = Address.scriptHashAddress
          toReferenceValidatorHash
          Nothing

      let
        referenceScript = decodeTextEnvelope RawScripts.rawPoCReferenceInput
          >>= plutusScriptV2FromEnvelope

      referenceUnapplied ← Monad.liftContractM "Decoding text envelope failed."
        referenceScript
      referenceApplied ← Monad.liftContractE $ Scripts.applyArgs
        referenceUnapplied
        [ PlutusData.toData toReferenceValidatorAddress ]
      let
        referenceValidator = Validator referenceApplied
      let
        referenceValidatorHash = Scripts.validatorHash referenceValidator
        referenceValidatorDat = Datum $ PlutusData.toData $ unit
        referenceValidatorAddress = Address.scriptHashAddress
          referenceValidatorHash
          Nothing

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
        bsTx ← Monad.liftedE $ Transaction.balanceTx unbalancedTx
        signedTx ← Transaction.signTransaction bsTx
        txId ← Transaction.submit signedTx
        Log.logInfo' $ "Transaction submitted: " <> show txId
        Transaction.awaitTxConfirmed txId
        Log.logInfo' $ "Transaction confirmed: " <> show txId

      -- END of duplicated code from `testScenario1`.

      -- 3.
      Test.Utils.fails do
        toReferenceIn /\ toReferenceOut ← Test.Utils.getUniqueUtxoAt
          toReferenceValidatorAddress
        referenceIn /\ referenceOut ← Test.Utils.getUniqueUtxoAt
          referenceValidatorAddress

        let
          toReferenceValidatorRedeemer = Redeemer $ PlutusData.toData unit
          referenceValidatorRedeemer = Redeemer $ PlutusData.toData $
            BigInt.fromInt
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
        balancedTx ← Monad.liftedE $ Transaction.balanceTx unbalancedTx
        signedTx ← Transaction.signTransaction balancedTx
        txId ← Transaction.submit signedTx
        Log.logInfo' $ "Transaction submitted: " <> show txId
        Transaction.awaitTxConfirmed txId
        Log.logInfo' $ "Transaction confirmed: " <> show txId

      pure unit
