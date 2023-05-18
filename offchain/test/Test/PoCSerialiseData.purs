-- | Some proof of concept tests for using the onchain builtin `serialiseData`
-- | function.
module Test.PoCSerialiseData (tests, testScenario1, testScenario2) where

import Contract.Prelude

import Contract.Address as Address
import Contract.Log as Log
import Contract.Monad (liftContractM, liftedE)
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
  ( DatumPresence(DatumWitness)
  , TxConstraints
  )
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

tests ∷ PlutipTest
tests = Mote.Monad.group "PoCSerialiseData tests" do
  testScenario1
  testScenario2

-- | `testScenario1` should succeed. It does the following.
-- |
-- |    1. Grabs the validator for `RawScripts.rawPoCSerialiseData`
-- |
-- |    2. Build / submit the transaction to pay some ada to the
-- |     `RawScripts.rawPoCSerialiseData` validator which holds the cbor serialized
-- |     integer 69 as a datum.
-- |
-- |    3. Spend the transaction created in 2., with redeemer the builtin data of 69.
testScenario1 ∷ PlutipTest
testScenario1 = Mote.Monad.test "PoCSerialiseData: testScenario1"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
  $ \alice → Wallet.withKeyWallet alice do
      -- 1.
      let
        script = decodeTextEnvelope RawScripts.rawPoCSerialiseData >>=
          plutusScriptV2FromEnvelope

      unapplied ← liftContractM "Decoding text envelope failed." script
      let
        validator = Validator unapplied
        validatorHash = Scripts.validatorHash validator
        validatorAddress = Address.scriptHashAddress validatorHash Nothing
      -- Getting this validator's datum is a bit confusing..
      -- First, we have
      --  - The integer 69
      --  - Convert it to Plutus Data
      --  - Serialise it to cbor (this is ByteArray)
      --  - Then we need to convert the ByteArray back into PlutusData (the validator's datum must be PlutusData!)
      let
        validatorDat = Datum $ PlutusData.toData
          $ PlutusData.serializeData
          $ PlutusData.toData
          $ BigInt.fromInt 69

      -- 2.
      void do
        let
          constraints ∷ TxConstraints Void Void
          constraints = TxConstraints.mustPayToScript validatorHash validatorDat
            DatumWitness
            (Value.lovelaceValueOf one)

          lookups ∷ ScriptLookups Void
          lookups = mempty

        unbalancedTx ← liftedE $ ScriptLookups.mkUnbalancedTx lookups
          constraints
        bsTx ← liftedE $ Transaction.balanceTx unbalancedTx
        signedTx ← Transaction.signTransaction bsTx
        txId ← Transaction.submit signedTx
        Log.logInfo' $ "Transaction submitted: " <> show txId
        Transaction.awaitTxConfirmed txId
        Log.logInfo' $ "Transaction confirmed: " <> show txId

      -- 3.
      void do
        (txIn /\ txOut) ← Test.Utils.getUniqueUtxoAt validatorAddress
        let
          validatorRedeemer = Redeemer $ PlutusData.toData $ BigInt.fromInt 69

          constraints ∷ TxConstraints Void Void
          constraints = TxConstraints.mustSpendScriptOutput txIn validatorRedeemer

          lookups ∷ ScriptLookups Void
          lookups = ScriptLookups.unspentOutputs (Map.singleton txIn txOut)
            <> ScriptLookups.validator validator

        unbalancedTx ← liftedE $ ScriptLookups.mkUnbalancedTx lookups
          constraints
        bsTx ← liftedE $ Transaction.balanceTx unbalancedTx
        signedTx ← Transaction.signTransaction bsTx
        txId ← Transaction.submit signedTx
        Log.logInfo' $ "Transaction submitted: " <> show txId
        Transaction.awaitTxConfirmed txId
        Log.logInfo' $ "Transaction confirmed: " <> show txId

      pure unit

-- | `testScenario2` should fail. It is essentially identical to
-- | `testScenario1`, except for step 3. In full, it does the following
-- |
-- |  1. Grabs the validator for `RawScripts.rawPoCSerialiseData`
-- |
-- |  2. Build / submit the transaction to pay some ada to the
-- |      `RawScripts.rawPoCSerialiseData` validator which holds the cbor serialized
-- |      integer 69 as a datum.
-- |
-- |  3. Spend the transaction created in 2., with redeemer the builtin data of
-- |      70 (but this will fail because 70 is very clearly not 69).
-- |
-- | Note: This function is almost entirely duplicated code from 'testScenario1'
testScenario2 ∷ PlutipTest
testScenario2 = Mote.Monad.test "PoCSerialiseData: testScenario2"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
  $ \alice → Wallet.withKeyWallet alice do
      -- 1.
      let
        script = decodeTextEnvelope RawScripts.rawPoCSerialiseData >>=
          plutusScriptV2FromEnvelope

      unapplied ← liftContractM "Decoding text envelope failed." script
      let
        validator = Validator unapplied
        validatorHash = Scripts.validatorHash validator
        validatorAddress = Address.scriptHashAddress validatorHash Nothing
      -- Getting this validator's datum is a bit confusing..
      -- First, we have
      --  - The integer 69
      --  - Convert it to Plutus Data
      --  - Serialise it to cbor (this is ByteArray)
      --  - Then we need to convert the ByteArray back into PlutusData (the validator's datum must be PlutusData!)
      let
        validatorDat = Datum
          $ PlutusData.toData
          $ PlutusData.serializeData
          $ PlutusData.toData
          $ BigInt.fromInt 69

      -- 2.
      void do
        let
          constraints ∷ TxConstraints Void Void
          constraints = TxConstraints.mustPayToScript validatorHash validatorDat
            DatumWitness
            (Value.lovelaceValueOf one)

          lookups ∷ ScriptLookups Void
          lookups = mempty

        unbalancedTx ← liftedE $ ScriptLookups.mkUnbalancedTx lookups
          constraints
        balancedTx ← liftedE $ Transaction.balanceTx unbalancedTx
        signedTx ← Transaction.signTransaction balancedTx
        txId ← Transaction.submit signedTx
        Log.logInfo' $ "Transaction submitted: " <> show txId
        Transaction.awaitTxConfirmed txId
        Log.logInfo' $ "Transaction confirmed: " <> show txId

      -- 3.
      Test.Utils.fails do
        (txIn /\ txOut) ← Test.Utils.getUniqueUtxoAt validatorAddress
        let
          -- The only distinct line from `testScenario1`.
          validatorRedeemer = Redeemer $ PlutusData.toData $ BigInt.fromInt 70

          constraints ∷ TxConstraints Void Void
          constraints = TxConstraints.mustSpendScriptOutput txIn validatorRedeemer

          lookups ∷ ScriptLookups Void
          lookups = ScriptLookups.unspentOutputs (Map.singleton txIn txOut)
            <> ScriptLookups.validator validator

        unbalancedTx ← liftedE $ ScriptLookups.mkUnbalancedTx lookups
          constraints
        balancedTx ← liftedE $ Transaction.balanceTx unbalancedTx
        signedTx ← Transaction.signTransaction balancedTx
        txId ← Transaction.submit signedTx
        Log.logInfo' $ "Transaction submitted: " <> show txId
        Transaction.awaitTxConfirmed txId
        Log.logInfo' $ "Transaction confirmed: " <> show txId

      pure unit
