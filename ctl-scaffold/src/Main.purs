module Main (main) where

import Contract.Prelude

import Contract.Config (testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (launchAff_, liftContractAffM, runContract)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Scripts (MintingPolicy(..), PlutusScript(..))
import Contract.TextEnvelope (TextEnvelopeType(..), textEnvelopeBytes)
import Contract.Value (scriptCurrencySymbol)

foreign import fuelMintingPolicy ∷ String

asMintingPolicy ∷ ByteArray -> MintingPolicy
asMintingPolicy = PlutusScript >>> MintingPolicy

main ∷ Effect Unit
main = launchAff_ $ runContract testnetNamiConfig do
  pol <- asMintingPolicy <$> textEnvelopeBytes fuelMintingPolicy PlutusScriptV1
  sym <- liftContractAffM "Currency Symbol Error" $ scriptCurrencySymbol pol
  logInfo' $ "Policy: " <> show pol
  logInfo' $ "Policy Symbol: " <> show sym
