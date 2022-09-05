module RunFuelMintingPolicy (runFuelMP, FuelParams(..)) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  , liftedM
  , throwContractError
  )
import Contract.PlutusData (class ToData, PlutusData(Constr), toData)
import Contract.Prim.ByteArray (ByteArray, byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(..), applyArgs)
import Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV2)
  , textEnvelopeBytes
  )
import Contract.Transaction (awaitTxConfirmed, balanceAndSignTx, submit)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo)
import Contract.Value as Value
import Data.BigInt as BigInt
import Data.Map as Map
import RawScripts (rawFUELMintingPolicy)
import SidechainParams (SidechainParams)
import Types.Scripts (plutusV2Script)

data FUELRedeemer
  = MainToSide ByteArray -- recipient sidechain (addr , signature)
  | SideToMain

derive instance Generic FUELRedeemer _
instance ToData FUELRedeemer where
  toData (MainToSide s1) = Constr zero [ toData s1 ]
  toData (SideToMain) = Constr one []

-- Applies SidechainParams to the minting policy
fuelMintingPolicy ∷ SidechainParams → Contract () MintingPolicy
fuelMintingPolicy sp = do
  fuelMPUnapplied ← (plutusV2Script >>> MintingPolicy) <$> textEnvelopeBytes
    rawFUELMintingPolicy
    PlutusScriptV2
  liftedE (applyArgs fuelMPUnapplied [ toData sp ])

data FuelParams
  = Mint { amount ∷ Int, recipient ∷ PaymentPubKeyHash }
  | Burn { amount ∷ Int, recipient ∷ ByteArray }

runFuelMP ∷ SidechainParams → FuelParams → Contract () Unit
runFuelMP sp fp = do
  fuelMP ← fuelMintingPolicy sp

  let
    inputTxIn = (unwrap sp).genesisMint

  inputUtxo ← traverse
    ( \txIn → do
        txOut ← liftedM "Cannot find genesis mint UTxO" $ getUtxo txIn
        pure $ Map.singleton txIn txOut
    )
    inputTxIn

  cs ← maybe (throwContractError "Cannot get currency symbol") pure $
    Value.scriptCurrencySymbol
      fuelMP
  logInfo' (show (toData sp))
  logInfo' ("fuelMP currency symbol: " <> show cs)
  tn ← liftContractM "Cannot get token name"
    (Value.mkTokenName =<< byteArrayFromAscii "FUEL")
  let
    mkValue i = Value.singleton cs tn (BigInt.fromInt i)

    constraints ∷ Constraints.TxConstraints Void Void
    constraints = case fp of
      Burn bp →
        let
          redeemer = wrap (toData (MainToSide bp.recipient))
        in
          Constraints.mustMintValueWithRedeemer redeemer (mkValue (-bp.amount))
      Mint mp →
        let
          value = mkValue mp.amount
        in
          Constraints.mustMintValueWithRedeemer (wrap (toData SideToMain)) value
            <> Constraints.mustPayToPubKey mp.recipient value
            <> (maybe mempty Constraints.mustSpendPubKeyOutput inputTxIn)

    lookups ∷ Lookups.ScriptLookups Void
    lookups = (maybe mempty Lookups.unspentOutputs inputUtxo) <>
      Lookups.mintingPolicy fuelMP

  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedM "Failed to balance/sign tx" (balanceAndSignTx ubTx)
  txId ← submit bsTx
  logInfo' ("Submitted fuelMP Tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' "fuelMP Tx submitted successfully!"
