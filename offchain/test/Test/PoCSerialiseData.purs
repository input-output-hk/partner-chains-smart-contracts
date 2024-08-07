-- | Some proof of concept tests for using the onchain builtin `serialiseData`
-- | function.
module Test.PoCSerialiseData (tests, testScenario1, testScenario2) where

import Contract.Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.PlutusScript as PlutusScript
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData (RedeemerDatum(RedeemerDatum))
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as ScriptLookups
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.TxConstraints (DatumPresence(DatumWitness), TxConstraints)
import Contract.TxConstraints as TxConstraints
import Contract.Value as Value
import Contract.Wallet as Wallet
import Data.Map as Map
import JS.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Run.Except (note) as Run
import Test.TestnetTest (TestnetTest)
import Test.TestnetTest as Test.TestnetTest
import Test.PoCRawScripts as RawScripts
import Test.Utils as Test.Utils
import TrustlessSidechain.Effects.Contract (liftContract)
import TrustlessSidechain.Effects.Log (logInfo') as Effect
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Effects.Transaction
  ( awaitTxConfirmed
  , balanceTx
  , mkUnbalancedTx
  , signTransaction
  , submit
  ) as Effect
import TrustlessSidechain.Effects.Util (mapError)
import TrustlessSidechain.Error
  ( OffchainError(BalanceTxError, BuildTxError, InvalidScript)
  )
import TrustlessSidechain.Utils.Address (toAddress)

tests ∷ TestnetTest
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
testScenario1 ∷ TestnetTest
testScenario1 = Mote.Monad.test "PoCSerialiseData: testScenario1"
  $ Test.TestnetTest.mkTestnetConfigTest
      [ BigNum.fromInt 50_000_000
      , BigNum.fromInt 50_000_000
      , BigNum.fromInt 50_000_000
      ]
  $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
      -- 1.
      let
        script = decodeTextEnvelope RawScripts.rawPoCSerialiseData >>=
          plutusScriptFromEnvelope

      validator ← Run.note (InvalidScript "Decoding text envelope failed.") script
      let
        scriptHash = PlutusScript.hash validator
      validatorAddress ← toAddress scriptHash
      -- Getting this validator's datum is a bit confusing..
      -- First, we have
      --  - The integer 69
      --  - Convert it to Plutus Data
      --  - Serialise it to cbor (this is ByteArray)
      --  - Then we need to convert the ByteArray back into PlutusData (the validator's datum must be PlutusData!)
      let
        validatorDat = PlutusData.Bytes
          $ unwrap
          $ encodeCbor
          $ PlutusData.Integer
          $ BigInt.fromInt 69

      -- 2.
      void do
        let
          constraints ∷ TxConstraints
          constraints = TxConstraints.mustPayToScript scriptHash validatorDat
            DatumWitness
            (Value.lovelaceValueOf BigNum.one)

          lookups ∷ ScriptLookups
          lookups = mempty

        unbalancedTx ← mapError BuildTxError $ Effect.mkUnbalancedTx lookups
          constraints
        balancedTx ← mapError BalanceTxError $ Effect.balanceTx unbalancedTx
        signedTx ← Effect.signTransaction balancedTx
        txId ← Effect.submit signedTx
        Effect.logInfo' $ "Transaction submitted: " <> show txId
        Effect.awaitTxConfirmed txId
        Effect.logInfo' $ "Transaction confirmed: " <> show txId

      -- 3.
      void do
        (txIn /\ txOut) ← liftContract $ Test.Utils.getUniqueUtxoAt
          validatorAddress
        let
          validatorRedeemer = RedeemerDatum $ PlutusData.Integer $ BigInt.fromInt
            69

          constraints ∷ TxConstraints
          constraints = TxConstraints.mustSpendScriptOutput txIn validatorRedeemer

          lookups ∷ ScriptLookups
          lookups = ScriptLookups.unspentOutputs (Map.singleton txIn txOut)
            <> ScriptLookups.validator validator
            <> ScriptLookups.datum validatorDat

        unbalancedTx ← mapError BuildTxError $ Effect.mkUnbalancedTx lookups
          constraints
        balancedTx ← mapError BalanceTxError $ Effect.balanceTx unbalancedTx
        signedTx ← Effect.signTransaction balancedTx
        txId ← Effect.submit signedTx
        Effect.logInfo' $ "Transaction submitted: " <> show txId
        Effect.awaitTxConfirmed txId
        Effect.logInfo' $ "Transaction confirmed: " <> show txId

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
testScenario2 ∷ TestnetTest
testScenario2 = Mote.Monad.test "PoCSerialiseData: testScenario2"
  $ Test.TestnetTest.mkTestnetConfigTest
      [ BigNum.fromInt 10_000_000, BigNum.fromInt 10_000_000 ]
  $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
      -- 1.
      let
        script = decodeTextEnvelope RawScripts.rawPoCSerialiseData >>=
          plutusScriptFromEnvelope

      validator ← Run.note (InvalidScript "Decoding text envelope failed.") script
      let
        scriptHash = PlutusScript.hash validator
      validatorAddress ← toAddress scriptHash
      -- Getting this validator's datum is a bit confusing..
      -- First, we have
      --  - The integer 69
      --  - Convert it to Plutus Data
      --  - Serialise it to cbor (this is ByteArray)
      --  - Then we need to convert the ByteArray back into PlutusData (the validator's datum must be PlutusData!)
      let
        validatorDat = PlutusData.Bytes
          $ unwrap
          $ encodeCbor
          $ PlutusData.Integer
          $ BigInt.fromInt 69

      -- 2.
      void do
        let
          constraints ∷ TxConstraints
          constraints = TxConstraints.mustPayToScript scriptHash validatorDat
            DatumWitness
            (Value.lovelaceValueOf BigNum.one)

          lookups ∷ ScriptLookups
          lookups = mempty

        unbalancedTx ← mapError BuildTxError $ Effect.mkUnbalancedTx lookups
          constraints
        balancedTx ← mapError BalanceTxError $ Effect.balanceTx unbalancedTx
        signedTx ← Effect.signTransaction balancedTx
        txId ← Effect.submit signedTx
        Effect.logInfo' $ "Transaction submitted: " <> show txId
        Effect.awaitTxConfirmed txId
        Effect.logInfo' $ "Transaction confirmed: " <> show txId

      -- 3.
      withUnliftApp (Test.Utils.fails) do
        (txIn /\ txOut) ← liftContract $ Test.Utils.getUniqueUtxoAt
          validatorAddress
        let
          -- The only distinct line from `testScenario1`.
          validatorRedeemer = RedeemerDatum $ PlutusData.Integer $ BigInt.fromInt
            70

          constraints ∷ TxConstraints
          constraints = TxConstraints.mustSpendScriptOutput txIn validatorRedeemer

          lookups ∷ ScriptLookups
          lookups = ScriptLookups.unspentOutputs (Map.singleton txIn txOut)
            <> ScriptLookups.validator validator

        unbalancedTx ← mapError BuildTxError $ Effect.mkUnbalancedTx lookups
          constraints
        balancedTx ← mapError BalanceTxError $ Effect.balanceTx unbalancedTx
        signedTx ← Effect.signTransaction balancedTx
        txId ← Effect.submit signedTx
        Effect.logInfo' $ "Transaction submitted: " <> show txId
        Effect.awaitTxConfirmed txId
        Effect.logInfo' $ "Transaction confirmed: " <> show txId

      pure unit
