-- | Some proof of concept tests for using reference scripts.
module Test.PoCReferenceScript (tests, testScenario1, testScenario2) where

import Contract.Prelude

import Cardano.ToData as ToData
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData (RedeemerDatum(RedeemerDatum))
import Contract.PlutusData as PlutusData
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as ScriptLookups
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction (ScriptRef(PlutusScriptRef))
import Contract.TxConstraints
  ( DatumPresence(DatumWitness)
  , InputWithScriptRef(SpendInput)
  , TxConstraints
  )
import Contract.TxConstraints as TxConstraints
import Contract.Value as Value
import Contract.Wallet as Wallet
import Data.Map as Map
import Mote.Monad as Mote.Monad
import Run.Except (note) as Run
import Test.PoCRawScripts as RawScripts
import Test.TestnetTest (TestnetTest)
import Test.TestnetTest as Test.TestnetTest
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
tests = Mote.Monad.group "PoCReferenceScript tests" do
  testScenario1
  testScenario2

-- | `testScenario1` runs the following contract (which should succeed):
-- |
-- | 1. Grabs the validators for
-- |     - `RawScripts.rawPoCToReferenceScript` which is a script which always succeeds.
-- |
-- |     - `RawScripts.rawPoCReferenceScript` which is a script that succeeds iff
-- |       its redeemer (of type `ScriptHash`) matches the `ScriptHash` of at least
-- |       one input.
-- |
-- | Note that we also create a `ScriptRef` for `RawScripts.rawPoCReferenceScript`
-- | which means that later we can create a transaction which uses
-- | `RawScripts.rawPoCReferenceScript` but doesn't include
-- | `RawScripts.rawPoCReferenceScript` in the witness set.
-- | We also compute the hash of the `ScriptRef` for
-- | `RawScripts.rawPoCReferenceScript`.
-- |
-- | 2. We pay some ada to two outputs
-- |
-- |     Output 1:
-- |          - has validator `RawScripts.rawPoCToReferenceScript`
-- |          - includes the script `RawScripts.rawPoCReferenceScript` on chain (the
-- |            script that we will reference later)
-- |
-- |     Output 2:
-- |         - has has validator `RawScripts.rawPoCReferenceScript`
-- |
-- | 3. We consume Output 1 and Output 2 by building a transaction as follows (this should succed)
-- |     - Spending Output 1
-- |     - Spending Output 2 with redeemer as the script hash of
-- |       `RawScripts.rawPoCReferenceScript` (i.e., itself)
-- |     - Include the validator `RawScripts.rawPoCToReferenceScript` in the witness
-- |       set
-- |     - Do NOT Include the validator `RawScripts.rawPoCReferenceScript` as this
-- |       is given from the reference script in Output 1.
testScenario1 ∷ TestnetTest
testScenario1 = Mote.Monad.test "PoCReferenceScript: testScenario1"
  $ Test.TestnetTest.mkTestnetConfigTest
      [ BigNum.fromInt 10_000_000, BigNum.fromInt 10_000_000 ]
  $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
      -- 1.
      let
        toReferenceScript = decodeTextEnvelope RawScripts.rawPoCToReferenceScript
          >>=
            plutusScriptFromEnvelope

      toReferenceValidator ← Run.note
        (InvalidScript "Decoding text envelope failed.")
        toReferenceScript
      let
        toReferenceScriptHash = PlutusScript.hash toReferenceValidator
        toReferenceValidatorDat = ToData.toData $ unit
      toReferenceValidatorAddress ← toAddress toReferenceScriptHash

      let
        referenceScript = decodeTextEnvelope RawScripts.rawPoCReferenceScript
          >>= plutusScriptFromEnvelope

      referenceValidator ← Run.note
        (InvalidScript "Decoding text envelope failed.")
        referenceScript

      let
        referenceScriptHash = PlutusScript.hash referenceValidator
        referenceValidatorDat = ToData.toData $ unit

      referenceValidatorAddress ← toAddress referenceScriptHash

      let
        referenceScriptRef =
          PlutusScriptRef (referenceValidator) ∷ ScriptRef

      -- 2.
      void do
        let
          constraints ∷ TxConstraints
          constraints =
            TxConstraints.mustPayToScriptWithScriptRef
              toReferenceScriptHash
              toReferenceValidatorDat
              DatumWitness
              referenceScriptRef
              (Value.lovelaceValueOf BigNum.one)
              <> TxConstraints.mustPayToScript
                referenceScriptHash
                referenceValidatorDat
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
        toReferenceIn /\ toReferenceOut ← liftContract $
          Test.Utils.getUniqueUtxoAt
            toReferenceValidatorAddress
        referenceIn /\ referenceOut ← liftContract $ Test.Utils.getUniqueUtxoAt
          referenceValidatorAddress

        let
          toReferenceValidatorRedeemer = RedeemerDatum $ PlutusData.toData $ unit
          referenceValidatorRedeemer = RedeemerDatum $ PlutusData.toData $
            referenceScriptHash

          constraints ∷ TxConstraints
          constraints =
            TxConstraints.mustSpendScriptOutputUsingScriptRef
              referenceIn
              referenceValidatorRedeemer
              ( SpendInput
                  ( TransactionUnspentOutput
                      { input: toReferenceIn, output: toReferenceOut }
                  )
              )
              <> TxConstraints.mustSpendScriptOutput toReferenceIn
                toReferenceValidatorRedeemer
              <> TxConstraints.mustIncludeDatum toReferenceValidatorDat
              <> TxConstraints.mustIncludeDatum referenceValidatorDat

          lookups ∷ ScriptLookups
          lookups =
            ScriptLookups.unspentOutputs (Map.singleton referenceIn referenceOut)
              <> ScriptLookups.unspentOutputs
                (Map.singleton toReferenceIn toReferenceOut)
              <> ScriptLookups.validator toReferenceValidator
              <> ScriptLookups.datum referenceValidatorDat

        unbalancedTx ← mapError BuildTxError $ Effect.mkUnbalancedTx lookups
          constraints
        balancedTx ← mapError BalanceTxError $ Effect.balanceTx unbalancedTx
        signedTx ← Effect.signTransaction balancedTx
        txId ← Effect.submit signedTx
        Effect.logInfo' $ "Transaction submitted: " <> show txId
        Effect.awaitTxConfirmed txId
        Effect.logInfo' $ "Transaction confirmed: " <> show txId

      pure unit

-- | `testScenario2` is the same as `testScenario1`, but changes 2. to not
-- | include the script on chain, and hence 3. should fail.
testScenario2 ∷ TestnetTest
testScenario2 = Mote.Monad.test "PoCReferenceScript: testScenario2"
  $ Test.TestnetTest.mkTestnetConfigTest
      [ BigNum.fromInt 10_000_000, BigNum.fromInt 10_000_000 ]
  $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
      -- START of duplicated code from `testScenario1`
      -- 1.

      let
        toReferenceScript = decodeTextEnvelope RawScripts.rawPoCToReferenceScript
          >>=
            plutusScriptFromEnvelope

      toReferenceValidator ← Run.note
        (InvalidScript "Decoding text envelope failed.")
        toReferenceScript
      let
        toReferenceScriptHash = PlutusScript.hash toReferenceValidator
        toReferenceValidatorDat = ToData.toData $ unit
      toReferenceValidatorAddress ← toAddress toReferenceScriptHash

      let
        referenceScript = decodeTextEnvelope RawScripts.rawPoCReferenceScript
          >>= plutusScriptFromEnvelope

      referenceValidator ← Run.note
        (InvalidScript "Decoding text envelope failed.")
        referenceScript

      let
        referenceScriptHash = PlutusScript.hash referenceValidator
        referenceValidatorDat = ToData.toData $ unit

      referenceValidatorAddress ← toAddress referenceScriptHash

      -- END of duplicated code from `testScenario1`

      -- 2.
      void do
        let
          constraints ∷ TxConstraints
          constraints =
            -- START: of line that changes in 2.
            TxConstraints.mustPayToScript
              toReferenceScriptHash
              toReferenceValidatorDat
              DatumWitness
              (Value.lovelaceValueOf BigNum.one)
              -- END: of line that changes in 2.
              <> TxConstraints.mustPayToScript
                referenceScriptHash
                referenceValidatorDat
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
        toReferenceIn /\ toReferenceOut ← liftContract $
          Test.Utils.getUniqueUtxoAt
            toReferenceValidatorAddress
        referenceIn /\ referenceOut ← liftContract $ Test.Utils.getUniqueUtxoAt
          referenceValidatorAddress

        let
          toReferenceValidatorRedeemer = RedeemerDatum $ ToData.toData $ unit
          referenceValidatorRedeemer = RedeemerDatum $ ToData.toData $
            referenceScriptHash

          constraints ∷ TxConstraints
          constraints =
            -- START: of line that changes in 3.
            TxConstraints.mustSpendScriptOutput referenceIn
              referenceValidatorRedeemer
              -- START: of line that changes in 3.
              <> TxConstraints.mustSpendScriptOutput toReferenceIn
                toReferenceValidatorRedeemer
              <> TxConstraints.mustIncludeDatum toReferenceValidatorDat
              <> TxConstraints.mustIncludeDatum referenceValidatorDat

          lookups ∷ ScriptLookups
          lookups =
            ScriptLookups.unspentOutputs (Map.singleton referenceIn referenceOut)
              <> ScriptLookups.unspentOutputs
                (Map.singleton toReferenceIn toReferenceOut)
              <> ScriptLookups.validator toReferenceValidator

        unbalancedTx ← mapError BuildTxError $ Effect.mkUnbalancedTx lookups
          constraints
        balancedTx ← mapError BalanceTxError $ Effect.balanceTx unbalancedTx
        signedTx ← Effect.signTransaction balancedTx
        txId ← Effect.submit signedTx
        Effect.logInfo' $ "Transaction submitted: " <> show txId
        Effect.awaitTxConfirmed txId
        Effect.logInfo' $ "Transaction confirmed: " <> show txId

      pure unit
