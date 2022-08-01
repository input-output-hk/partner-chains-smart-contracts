module Main (main) where

import Contract.Prelude

import Contract.Config (ConfigParams, testnetConfig, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (launchAff_, liftContractAffM, runContract)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Scripts (MintingPolicy(..), PlutusScript(..))
import Contract.TextEnvelope (TextEnvelopeType(..), textEnvelopeBytes)
import Contract.Value (scriptCurrencySymbol)

foreign import fuelMintingPolicy ∷ String
foreign import inBrowser ∷ Boolean

asMintingPolicy ∷ ByteArray -> MintingPolicy
asMintingPolicy = PlutusScript >>> MintingPolicy

config ∷ ConfigParams ()
config = if inBrowser then testnetNamiConfig else testnetConfig

main ∷ Effect Unit
main = launchAff_ $ runContract config do
  pol <- asMintingPolicy <$> textEnvelopeBytes fuelMintingPolicy PlutusScriptV1
  sym <- liftContractAffM "Currency Symbol Error" $ scriptCurrencySymbol pol
--v  <- liftContractAffM "Couldn't hash validator" (mintingPolicyHash mp)
  logInfo' $ "Policy: " <> show pol
  logInfo' $ "Policy Symbol: " <> show sym
