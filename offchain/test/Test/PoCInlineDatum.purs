-- | Some proof of concept tests for using inline datums.
module Test.PoCInlineDatum (tests, testScenario1, testScenario2) where

import Contract.Prelude

import Cardano.ToData as ToData
import Cardano.Types.PlutusScript as PlutusScript
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData (RedeemerDatum(RedeemerDatum))
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as ScriptLookups
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.TxConstraints
  ( DatumPresence(DatumInline, DatumWitness)
  , TxConstraints
  )
import Contract.TxConstraints as TxConstraints
import Contract.Value as Value
import Contract.Wallet as Wallet
import Control.Applicative as Applicative
import Data.FoldableWithIndex as FoldableWithIndex
import Data.Map as Map
import JS.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Run.Except (note) as Run
import Run.Except (throw)
import Test.PoCRawScripts as RawScripts
import Test.TestnetTest (TestnetTest)
import Test.TestnetTest as Test.TestnetTest
import Test.Utils as Test.Utils
import TrustlessSidechain.Effects.Log (logInfo') as Effect
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Effects.Transaction
  ( awaitTxConfirmed
  , balanceTx
  , mkUnbalancedTx
  , signTransaction
  , submit
  , utxosAt
  ) as Effect
import TrustlessSidechain.Effects.Util (mapError)
import TrustlessSidechain.Error
  ( OffchainError
      ( BalanceTxError
      , BuildTxError
      , GenericInternalError
      , InvalidScript
      )
  )
import TrustlessSidechain.Utils.Address (toAddress)

-- | `tests` aggregates all the PoCInlineDatums together conveniently
tests ∷ TestnetTest
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
testScenario1 ∷ TestnetTest
testScenario1 = Mote.Monad.test "PoCInlineDatum: testScenario1"
  $ Test.TestnetTest.mkTestnetConfigTest
      [ BigNum.fromInt 10_000_000, BigNum.fromInt 10_000_000 ]
  $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
      -- 1.
      let
        script = decodeTextEnvelope RawScripts.rawPoCInlineDatum >>=
          plutusScriptFromEnvelope

      validator ← Run.note (InvalidScript "Decoding text envelope failed.") script
      let
        validatorHash = PlutusScript.hash validator
        validatorDat = ToData.toData $ BigInt.fromInt 69
      validatorAddress ← toAddress validatorHash

      -- 2.
      void do
        let
          constraints ∷ TxConstraints
          constraints = TxConstraints.mustPayToScript validatorHash validatorDat
            DatumInline
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
        utxoMap ← Effect.utxosAt validatorAddress

        Applicative.when (length utxoMap /= 1)
          $ throw
          $ GenericInternalError
          $ "Expected exactly one PoCInlineDatum script address but got:"
          <> show utxoMap

        FoldableWithIndex.forWithIndex_ utxoMap $ \txIn txOut → void do
          let
            validatorRedeemer = RedeemerDatum $ ToData.toData $ BigInt.fromInt 69

            constraints ∷ TxConstraints
            constraints = TxConstraints.mustSpendScriptOutput txIn
              validatorRedeemer

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
testScenario2 ∷ TestnetTest
testScenario2 = Mote.Monad.test "PoCInlineDatum: testScenario2"
  $ Test.TestnetTest.mkTestnetConfigTest
      [ BigNum.fromInt 10_000_000, BigNum.fromInt 10_000_000 ]
  $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
      -- 1.
      let
        script = decodeTextEnvelope RawScripts.rawPoCInlineDatum >>=
          plutusScriptFromEnvelope

      validator ← Run.note (InvalidScript "Decoding text envelope failed.") script
      let
        validatorHash = PlutusScript.hash validator
        validatorDat = ToData.toData $ BigInt.fromInt 69
      validatorAddress ← toAddress validatorHash

      -- 2.
      void do
        let
          constraints ∷ TxConstraints
          constraints = TxConstraints.mustPayToScript validatorHash validatorDat
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
        utxoMap ← Effect.utxosAt validatorAddress

        Applicative.when (length utxoMap /= 1)
          $ throw
          $ GenericInternalError
          $ "Expected exactly one PoCInlineDatum script address but got:"
          <> show utxoMap

        FoldableWithIndex.forWithIndex_ utxoMap $ \txIn txOut → void do
          let
            validatorRedeemer = RedeemerDatum $ ToData.toData $ BigInt.fromInt 69

            constraints ∷ TxConstraints
            constraints = TxConstraints.mustSpendScriptOutput txIn
              validatorRedeemer

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
