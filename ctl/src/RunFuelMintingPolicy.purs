module RunFuelMintingPolicy (runFuelMP, FuelParams(..)) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , liftContractAffM
  , liftContractM
  , liftedE
  , liftedM
  )
import Contract.PlutusData (class ToData, PlutusData(Constr), toData)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(..), PlutusScript(..), applyArgs)
import Contract.TextEnvelope (TextEnvelopeType(..), textEnvelopeBytes)
import Contract.Transaction (awaitTxConfirmed, balanceAndSignTx, submit)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.BigInt as BigInt
import ScriptsFFI (fUELMintingPolicy)
import SidechainParams (SidechainParams)

data FUELRedeemer
  = MainToSide String String -- recipient sidechain (addr , signature)
  | SideToMain

derive instance Generic FUELRedeemer _
instance ToData FUELRedeemer where
  toData (MainToSide s1 s2) = Constr zero [ toData s1, toData s2 ]
  toData (SideToMain) = Constr one []

-- Applies SidechainParams to the minting policy
fuelMintingPolicy ∷ SidechainParams → Contract () MintingPolicy
fuelMintingPolicy sp = do
  fuelMPUnapplied ← (PlutusScript >>> MintingPolicy) <$> textEnvelopeBytes
    fUELMintingPolicy
    PlutusScriptV1
  liftedE (applyArgs fuelMPUnapplied [ toData sp ])

data FuelParams
  = Mint { amount ∷ Int, recipient ∷ PaymentPubKeyHash }
  | Burn { amount ∷ Int, recipient ∷ String, sidechainSig ∷ String }

-- it's a limitation of plutus server that we cannot use stake addresses so ignore the custom warning
runFuelMP ∷ FuelParams → SidechainParams → Contract () Unit
runFuelMP fp sp = do
  fuelMP ← fuelMintingPolicy sp
  cs ← liftContractAffM "Cannot get currency symbol"
    (Value.scriptCurrencySymbol fuelMP)
  logInfo' ("fuelMP curreny symbol: " <> show cs)
  tn ← liftContractM "Cannot get token name"
    (Value.mkTokenName =<< byteArrayFromAscii "FUEL")
  let
    mkValue i = Value.singleton cs tn (BigInt.fromInt i)

    constraints ∷ Constraints.TxConstraints Void Void
    constraints = case fp of
      Burn bp →
        let
          redeemer = wrap (toData (MainToSide bp.recipient bp.sidechainSig))
        in
          Constraints.mustMintValueWithRedeemer redeemer (mkValue (-bp.amount))
      Mint mp →
        let
          value = mkValue mp.amount
        in
          Constraints.mustMintValueWithRedeemer (wrap (toData SideToMain)) value
            <> Constraints.mustPayToPubKey mp.recipient value

    lookups ∷ Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy fuelMP
  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedM "Failed to balance/sign tx" (balanceAndSignTx ubTx)
  txId ← submit bsTx
  logInfo' ("Submitted fuelMP Tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' "fuelMP Tx submitted successfully!"
