-- | Some proof of concept tests for using reference inputs.
module Test.PoCReferenceInput (tests, testScenario1, testScenario2) where

import Contract.Prelude

import Cardano.Plutus.ApplyArgs as Scripts
import Cardano.Plutus.Types.Address as PlutusAddress
import Cardano.ToData as ToData
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
import TrustlessSidechain.Effects.Util (lmapThrow, mapError)
import TrustlessSidechain.Error
  ( OffchainError
      ( BalanceTxError
      , BuildTxError
      , InvalidScriptArgs
      , InvalidAddress
      , InvalidScript
      )
  )
import TrustlessSidechain.Utils.Address (toAddress)

-- | `tests` aggregates all the PoCReferenceInput together conveniently
tests ∷ TestnetTest
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
testScenario1 ∷ TestnetTest
testScenario1 = Mote.Monad.test "PoCReferenceInput: testScenario1"
  $ Test.TestnetTest.mkTestnetConfigTest
      [ BigNum.fromInt 10_000_000, BigNum.fromInt 10_000_000 ]
  $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
      -- 1.
      let
        toReferenceScript = decodeTextEnvelope RawScripts.rawPoCToReferenceInput
          >>= plutusScriptFromEnvelope

      toReferenceValidator ← Run.note
        (InvalidScript "Decoding text envelope failed.")
        toReferenceScript
      let
        toReferenceScriptHash = PlutusScript.hash toReferenceValidator
        toReferenceValidatorDat = ToData.toData $ BigInt.fromInt 69
      toReferenceValidatorAddress ← toAddress toReferenceScriptHash
      toReferenceValidatorAddressData ← ToData.toData
        <$>
          ( Run.note
              ( InvalidAddress "Couldn't map address to PlutusData."
                  toReferenceValidatorAddress
              )
              $ PlutusAddress.fromCardano toReferenceValidatorAddress
          )

      let
        referenceScript = decodeTextEnvelope RawScripts.rawPoCReferenceInput
          >>= plutusScriptFromEnvelope

      referenceUnappliedValidator ← Run.note
        (InvalidScript "Decoding text envelope failed.")
        referenceScript
      referenceValidator ← lmapThrow InvalidScriptArgs $ Scripts.applyArgs
        referenceUnappliedValidator
        [ toReferenceValidatorAddressData ]
      let
        referenceScriptHash = PlutusScript.hash referenceValidator
        referenceValidatorDat = ToData.toData $ unit
      referenceValidatorAddress ← toAddress referenceScriptHash

      -- 2.
      void do
        let
          constraints ∷ TxConstraints
          constraints =
            TxConstraints.mustPayToScript
              toReferenceScriptHash
              toReferenceValidatorDat
              DatumWitness
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
          referenceValidatorRedeemer = RedeemerDatum $ ToData.toData $
            BigInt.fromInt
              69

          constraints ∷ TxConstraints
          constraints =
            TxConstraints.mustReferenceOutput toReferenceIn
              <> TxConstraints.mustSpendScriptOutput referenceIn
                referenceValidatorRedeemer
              <> TxConstraints.mustIncludeDatum toReferenceValidatorDat

          lookups ∷ ScriptLookups
          lookups =
            ScriptLookups.unspentOutputs (Map.singleton referenceIn referenceOut)
              <> ScriptLookups.unspentOutputs
                (Map.singleton toReferenceIn toReferenceOut)
              <> ScriptLookups.validator referenceValidator
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
testScenario2 ∷ TestnetTest
testScenario2 = Mote.Monad.test "PoCReferenceInput: testScenario2"
  $ Test.TestnetTest.mkTestnetConfigTest
      [ BigNum.fromInt 10_000_000, BigNum.fromInt 10_000_000 ]
  $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
      -- START of duplicated code from `testScenario1`.
      -- 1.
      let
        toReferenceScript = decodeTextEnvelope RawScripts.rawPoCToReferenceInput
          >>= plutusScriptFromEnvelope

      toReferenceValidator ← Run.note
        (InvalidScript "Decoding text envelope failed.")
        toReferenceScript
      let
        toReferenceScriptHash = PlutusScript.hash toReferenceValidator
        toReferenceValidatorDat = ToData.toData $ BigInt.fromInt 69
      toReferenceValidatorAddress ← toAddress toReferenceScriptHash
      toReferenceValidatorAddressData ← ToData.toData
        <$>
          ( Run.note
              ( InvalidAddress "Couldn't map address to PlutusData."
                  toReferenceValidatorAddress
              )
              $ PlutusAddress.fromCardano toReferenceValidatorAddress
          )
      let
        referenceScript = decodeTextEnvelope RawScripts.rawPoCReferenceInput
          >>= plutusScriptFromEnvelope

      referenceUnappliedValidator ← Run.note
        (InvalidScript "Decoding text envelope failed.")
        referenceScript
      referenceValidator ← lmapThrow InvalidScriptArgs $ Scripts.applyArgs
        referenceUnappliedValidator
        [ toReferenceValidatorAddressData ]
      let
        referenceScriptHash = PlutusScript.hash referenceValidator
        referenceValidatorDat = ToData.toData $ unit
      referenceValidatorAddress ← toAddress referenceScriptHash

      -- 2.
      void do
        let
          constraints ∷ TxConstraints
          constraints =
            TxConstraints.mustPayToScript
              toReferenceScriptHash
              toReferenceValidatorDat
              DatumWitness
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

      -- END of duplicated code from `testScenario1`.

      -- 3.
      withUnliftApp (Test.Utils.fails) do
        toReferenceIn /\ toReferenceOut ← liftContract $
          Test.Utils.getUniqueUtxoAt
            toReferenceValidatorAddress
        referenceIn /\ referenceOut ← liftContract $ Test.Utils.getUniqueUtxoAt
          referenceValidatorAddress

        let
          toReferenceValidatorRedeemer = RedeemerDatum $ ToData.toData unit
          referenceValidatorRedeemer = RedeemerDatum $ ToData.toData $
            BigInt.fromInt
              69

          constraints ∷ TxConstraints
          constraints =
            TxConstraints.mustSpendScriptOutput toReferenceIn
              toReferenceValidatorRedeemer
              <> TxConstraints.mustSpendScriptOutput referenceIn
                referenceValidatorRedeemer
              <> TxConstraints.mustIncludeDatum toReferenceValidatorDat

          lookups ∷ ScriptLookups
          lookups =
            ScriptLookups.unspentOutputs (Map.singleton referenceIn referenceOut)
              <> ScriptLookups.unspentOutputs
                (Map.singleton toReferenceIn toReferenceOut)
              <> ScriptLookups.validator referenceValidator
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
