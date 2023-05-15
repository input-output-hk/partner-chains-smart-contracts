module Test.PoCECDSA (testScenario) where

import Contract.Prelude

import Contract.Address (getNetworkId, validatorHashEnterpriseAddress)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  )
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class ToData
  , PlutusData(Constr)
  , Redeemer(Redeemer)
  , toData
  , unitDatum
  )
import Contract.Prim.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( Validator(Validator)
  , validatorHash
  )
import Contract.TextEnvelope
  ( decodeTextEnvelope
  , plutusScriptV2FromEnvelope
  )
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , balanceTx
  , signTransaction
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Contract.Wallet as Wallet
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Set as Set
import Mote.Monad as Mote.Monad
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import TrustlessSidechain.RawScripts (rawPoCECDSA)

getValidator ∷ Contract Validator
getValidator = do
  let
    script = decodeTextEnvelope rawPoCECDSA >>= plutusScriptV2FromEnvelope

  unapplied ← liftContractM "Decoding text envelope failed." script
  pure $ Validator unapplied

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

-- | Prepate the ECDSA test by locking some funds at the validator address
prepTest ∷ Contract TransactionHash
prepTest = do
  validator ← getValidator
  let
    valHash = validatorHash validator
    val = Value.lovelaceValueOf (BigInt.fromInt 1)

    lookups ∷ Lookups.ScriptLookups Void
    lookups = Lookups.validator validator

    constraints ∷ Constraints.TxConstraints Void Void
    constraints = Constraints.mustPayToScript valHash unitDatum
      Constraints.DatumInline
      val
  ubTx ← liftedE $ Lookups.mkUnbalancedTx lookups constraints
  bsTx ← liftedE $ balanceTx ubTx
  signedTx ← signTransaction bsTx
  txId ← submit signedTx
  logInfo' $ ("Submitted ECDSA test prep tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' "Transaction confirmed."

  pure txId

-- | Attempt to unlock one utxo using an ECDSA signature
testVerification ∷ ECDSARed → Contract TransactionHash
testVerification ecdsaRed = do
  let red = Redeemer $ toData ecdsaRed

  validator ← getValidator
  let valHash = validatorHash validator

  netId ← getNetworkId
  valAddr ← liftContractM "cannot get validator address"
    (validatorHashEnterpriseAddress netId valHash)

  scriptUtxos ← utxosAt valAddr
  txIn ← liftContractM "No UTxOs found at validator address"
    $ Set.findMin
    $ Map.keys scriptUtxos
  let
    lookups ∷ Lookups.ScriptLookups Void
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs scriptUtxos

    constraints ∷ Constraints.TxConstraints Void Void
    constraints = Constraints.mustSpendScriptOutput txIn red
  ubTx ← liftedE $ Lookups.mkUnbalancedTx lookups constraints
  bsTx ← liftedE $ balanceTx ubTx
  signedTx ← signTransaction bsTx
  txId ← submit signedTx
  logInfo' $ ("Submitted ECDSA test verification tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' "Transaction confirmed."

  pure txId

-- | Testing ECDSA verification function on-chain
testScenario ∷ PlutipTest
testScenario = Mote.Monad.test "PoCECDSA: testScenario"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
  $ \alice → Wallet.withKeyWallet alice do
      txId ← prepTest
      awaitTxConfirmed txId
      void $ testVerification $
        -- | TODO: Find the correct format
        ECDSARed
          { msg: hexToByteArrayUnsafe
              "16e0bf1f85594a11e75030981c0b670370b3ad83a43f49ae58a2fd6f6513cde9"
          , sig: hexToByteArrayUnsafe
              "5fb12954b28be6456feb080cfb8467b6f5677f62eb9ad231de7a575f4b6857512754fb5ef7e0e60e270832e7bb0e2f0dc271012fa9c46c02504aa0e798be6295"
          , pk: hexToByteArrayUnsafe
              "0392d7b94bc6a11c335a043ee1ff326b6eacee6230d3685861cd62bce350a172e0"
          }
