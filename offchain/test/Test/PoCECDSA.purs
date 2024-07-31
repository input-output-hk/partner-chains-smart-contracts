module Test.PoCECDSA
  ( ECDSARed(..)
  , testScenario
  ) where

import Contract.Prelude hiding (unit)

import Cardano.ToData (class ToData, toData)
import Cardano.ToData as ToData
import Cardano.Types.PlutusData (PlutusData(Constr), unit)
import Cardano.Types.PlutusScript as PlutusScript
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( RedeemerDatum(RedeemerDatum)
  )
import Contract.Prim.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Contract.ScriptLookups as Lookups
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Contract.Wallet as Wallet
import Data.Map as Map
import Data.Set as Set
import Mote.Monad as Mote.Monad
import Prelude (unit) as Prelude
import Run.Except (note) as Run
import Test.PoCRawScripts (rawPoCECDSA)
import Test.TestnetTest (TestnetTest)
import Test.TestnetTest as Test.TestnetTest
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
  ( OffchainError(BalanceTxError, BuildTxError, InvalidScript, NotFoundUtxo)
  )
import TrustlessSidechain.Utils.Address (toAddress)

newtype ECDSARed = ECDSARed
  { msg ∷ ByteArray
  , sig ∷ ByteArray
  , pk ∷ ByteArray
  }

derive instance Generic ECDSARed _

derive instance Newtype ECDSARed _

instance ToData ECDSARed where
  toData (ECDSARed { msg, sig, pk }) = Constr (BigNum.fromInt 0)
    [ toData msg, toData sig, toData pk ]

-- | Testing ECDSA verification function on-chain
testScenario ∷ TestnetTest
testScenario = Mote.Monad.test "PoCECDSA: testScenario"
  $ Test.TestnetTest.mkTestnetConfigTest
      [ BigNum.fromInt 10_000_000, BigNum.fromInt 10_000_000 ]
  $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
      -- Prep test
      let
        script = decodeTextEnvelope rawPoCECDSA >>= plutusScriptFromEnvelope

      validator ← Run.note (InvalidScript "Decoding text envelope failed.") script

      let
        valHash = PlutusScript.hash validator
        val = Value.lovelaceValueOf (BigNum.fromInt 1)

        lookups ∷ Lookups.ScriptLookups
        lookups = Lookups.validator validator

        constraints ∷ Constraints.TxConstraints
        constraints = Constraints.mustPayToScript valHash unit
          Constraints.DatumInline
          val

      unbalancedTx ← mapError BuildTxError $ Effect.mkUnbalancedTx lookups
        constraints
      balancedTx ← mapError BalanceTxError $ Effect.balanceTx unbalancedTx
      signedTx ← Effect.signTransaction balancedTx
      txId ← Effect.submit signedTx
      Effect.logInfo' $ "Transaction submitted: " <> show txId
      Effect.awaitTxConfirmed txId
      Effect.logInfo' $ "Transaction confirmed: " <> show txId
      ---

      -- | TODO: Find the correct format
      let
        red = RedeemerDatum $ ToData.toData $ ECDSARed
          { msg: hexToByteArrayUnsafe
              "16e0bf1f85594a11e75030981c0b670370b3ad83a43f49ae58a2fd6f6513cde9"
          , sig: hexToByteArrayUnsafe
              "5fb12954b28be6456feb080cfb8467b6f5677f62eb9ad231de7a575f4b6857512754fb5ef7e0e60e270832e7bb0e2f0dc271012fa9c46c02504aa0e798be6295"
          , pk: hexToByteArrayUnsafe
              "0392d7b94bc6a11c335a043ee1ff326b6eacee6230d3685861cd62bce350a172e0"
          }

      let
        script' = decodeTextEnvelope rawPoCECDSA >>= plutusScriptFromEnvelope

      validator' ← Run.note (InvalidScript "Decoding text envelope failed.")
        script'

      let valHash' = PlutusScript.hash validator'

      valAddr ← toAddress valHash'

      scriptUtxos ← Effect.utxosAt valAddr
      txIn ← Run.note (NotFoundUtxo "No UTxOs found at validator address")
        $ Set.findMin
        $ Map.keys scriptUtxos
      let
        lookups' ∷ Lookups.ScriptLookups
        lookups' = Lookups.validator validator
          <> Lookups.unspentOutputs scriptUtxos

        constraints' ∷ Constraints.TxConstraints
        constraints' = Constraints.mustSpendScriptOutput txIn red
      unbalancedTx' ← mapError BuildTxError $ Effect.mkUnbalancedTx lookups'
        constraints'
      balancedTx' ← mapError BalanceTxError $ Effect.balanceTx unbalancedTx'
      signedTx' ← Effect.signTransaction balancedTx'
      txId' ← Effect.submit signedTx'
      Effect.logInfo' $ "Transaction submitted: " <> show txId'
      Effect.awaitTxConfirmed txId'
      Effect.logInfo' $ "Transaction confirmed: " <> show txId'

      pure Prelude.unit
