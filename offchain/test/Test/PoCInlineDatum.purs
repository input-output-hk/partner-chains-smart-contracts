-- | Some proof of concept tests for using inline datums.
module Test.PoCInlineDatum (tests, testScenario1, testScenario2) where

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
import Contract.TxConstraints
  ( DatumPresence(DatumInline, DatumWitness)
  , TxConstraints
  )
import Contract.TxConstraints as TxConstraints
import Contract.Utxos as Utxos
import Contract.Value as Value
import Contract.Wallet as Wallet
import Control.Applicative as Applicative
import Data.BigInt as BigInt
import Data.FoldableWithIndex as FoldableWithIndex
import Data.Map as Map
import Mote.Monad as Mote.Monad
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils as Test.Utils
import TrustlessSidechain.RawScripts as RawScripts

-- | `tests` aggregates all the PoCInlineDatums together conveniently
tests ∷ PlutipTest
tests = Mote.Monad.group "PoCInlineDatum tests" do
  testScenario1
  testScenario2

-- | `testScenario1` goes as follows:
-- |
-- |    1. Grabs the validator for `RawScripts.rawPoCInlineDatum`
-- |
-- |    2. Build / submit the transaction to pay some ada to the
-- |     `RawScripts.rawPoCInlineDatum` validator which holds the integer 69 as an
-- |     inline datum
-- |
-- |    3. Build / submit another transaction to consume the previous utxo and verify that
-- |     the inline datum really was 69
testScenario1 ∷ PlutipTest
testScenario1 = Mote.Monad.test "PoCInlineDatum: testScenario1"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
  $ \alice → Wallet.withKeyWallet alice do
      -- 1.
      let
        script = decodeTextEnvelope RawScripts.rawPoCInlineDatum >>=
          plutusScriptV2FromEnvelope

      unapplied ← Monad.liftContractM "Decoding text envelope failed." script
      let
        validator = Validator unapplied
        validatorHash = Scripts.validatorHash validator
        validatorDat = Datum $ PlutusData.toData $ BigInt.fromInt 69
        validatorAddress = Address.scriptHashAddress validatorHash Nothing

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
        bsTx ← Monad.liftedE $ Transaction.balanceTx unbalancedTx
        signedTx ← Transaction.signTransaction bsTx
        txId ← Transaction.submit signedTx
        Log.logInfo' $ "Transaction submitted: " <> show txId
        Transaction.awaitTxConfirmed txId
        Log.logInfo' $ "Transaction confirmed: " <> show txId

      -- 3.
      void do
        utxoMap ← Utxos.utxosAt validatorAddress

        Applicative.when (length utxoMap /= 1)
          $ Monad.throwContractError
          $ "Expected exactly one PoCInlineDatum script address but got:"
          <> show utxoMap

        FoldableWithIndex.forWithIndex_ utxoMap $ \txIn txOut → void do
          let
            validatorRedeemer = Redeemer $ PlutusData.toData $ BigInt.fromInt 69

            constraints ∷ TxConstraints Void Void
            constraints = TxConstraints.mustSpendScriptOutput txIn
              validatorRedeemer

            lookups ∷ ScriptLookups Void
            lookups = ScriptLookups.unspentOutputs (Map.singleton txIn txOut)
              <> ScriptLookups.validator validator

          unbalancedTx ← Monad.liftedE $ ScriptLookups.mkUnbalancedTx lookups
            constraints
          bsTx ← Monad.liftedE $ Transaction.balanceTx unbalancedTx
          signedTx ← Transaction.signTransaction bsTx
          txId ← Transaction.submit signedTx
          Log.logInfo' $ "Transaction submitted: " <> show txId
          Transaction.awaitTxConfirmed txId
          Log.logInfo' $ "Transaction confirmed: " <> show txId

      pure unit

-- | `testScenario2` goes as follows:
-- |
-- | 1. Grabs the validator
-- |
-- | 2. Build / submit the transaction to pay some ada to the
-- |     `RawScripts.rawPoCInlineDatum` validator which holds the integer 69 as a
-- |      witness datum
-- |
-- | 3. Build / submit another transaction to consume the previous utxo and verify this
-- |     transaction should fail because there is no inline datum.
testScenario2 ∷ PlutipTest
testScenario2 = Mote.Monad.test "PoCInlineDatum: testScenario2"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
  $ \alice → Wallet.withKeyWallet alice do
      -- 1.
      let
        script = decodeTextEnvelope RawScripts.rawPoCInlineDatum >>=
          plutusScriptV2FromEnvelope

      unapplied ← Monad.liftContractM "Decoding text envelope failed." script
      let
        validator = Validator unapplied
        validatorHash = Scripts.validatorHash validator
        validatorDat = Datum $ PlutusData.toData $ BigInt.fromInt 69
        validatorAddress = Address.scriptHashAddress validatorHash Nothing

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
        balancedTx ← Monad.liftedE $ Transaction.balanceTx unbalancedTx
        signedTx ← Transaction.signTransaction balancedTx
        txId ← Transaction.submit signedTx
        Log.logInfo' $ "Transaction submitted: " <> show txId
        Transaction.awaitTxConfirmed txId
        Log.logInfo' $ "Transaction confirmed: " <> show txId

      -- 3.
      Test.Utils.fails do
        utxoMap ← Utxos.utxosAt validatorAddress

        Applicative.when (length utxoMap /= 1)
          $ Monad.throwContractError
          $ "Expected exactly one PoCInlineDatum script address but got:"
          <> show utxoMap

        FoldableWithIndex.forWithIndex_ utxoMap $ \txIn txOut → void do
          let
            validatorRedeemer = Redeemer $ PlutusData.toData $ BigInt.fromInt 69

            constraints ∷ TxConstraints Void Void
            constraints = TxConstraints.mustSpendScriptOutput txIn
              validatorRedeemer

            lookups ∷ ScriptLookups Void
            lookups = ScriptLookups.unspentOutputs (Map.singleton txIn txOut)
              <> ScriptLookups.validator validator

          unbalancedTx ← Monad.liftedE $ ScriptLookups.mkUnbalancedTx lookups
            constraints
          balancedTx ← Monad.liftedE $ Transaction.balanceTx unbalancedTx
          signedTx ← Transaction.signTransaction balancedTx
          txId ← Transaction.submit signedTx
          Log.logInfo' $ "Transaction submitted: " <> show txId
          Transaction.awaitTxConfirmed txId
          Log.logInfo' $ "Transaction confirmed: " <> show txId

      pure unit
