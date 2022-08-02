module Main (contract) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractAffM)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Scripts (MintingPolicy(..), PlutusScript(..), mintingPolicyHash)
import Contract.TextEnvelope (TextEnvelopeType(..), textEnvelopeBytes)
import Contract.Value (scriptCurrencySymbol)

foreign import fuelMintingPolicy ∷ String

asMintingPolicy ∷ ByteArray -> MintingPolicy
asMintingPolicy = PlutusScript >>> MintingPolicy

contract ∷ Contract () Unit
contract = do
  pol <- asMintingPolicy <$> textEnvelopeBytes fuelMintingPolicy PlutusScriptV1
  sym <- liftContractAffM "Currency Symbol Error" $ scriptCurrencySymbol pol
  mph <- liftContractAffM "Couldn't Hash Script" $ mintingPolicyHash pol
  logInfo' $ "Policy: " <> show pol
  logInfo' $ "Policy Symbol: " <> show sym
  logInfo' $ "Policy Hash: " <> show mph
